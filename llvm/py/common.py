#! /usr/bin/env python3

import libadalang as lal
import shutil

from os import path
from functools import partial
from sys import argv
from utils import Argument, Function, Package, Procedure
from wrapper import generate_body, generate_decl, is_wrapper_needed


# Remove the first prefix in prefixes string starts with. If string does not
# start with any of the given prefix, return it unchanged.
def remove_prefix(string, prefixes):
    for prefix in prefixes:
        if string.lower().startswith(prefix.lower()):
            return string[len(prefix):]
    return string


# Same as above for suffixes
def remove_suffix(string, suffixes):
    for suffix in suffixes:
        if string.lower().endswith(suffix.lower()):
            return string[:-len(suffix)]
    return string


# Return the name under the Ada convention naming scheme, e.g.
# clang_getNullCursor => Get_Null_Cursor (after stripping the clang prefix)
# LLVMAddLICMPass => Add_LICM_Pass (after stripping the LLVM prefix)
def ada_name(name, strips=None, prefix="", suffix=""):

    strips = list(strips or [])

    name = remove_prefix(name, strips)

    if name.startswith("_"):
        name = name[1:]

    if name.isupper():
        return name

    if "_" in name:
        names = name.split('_')
        res = []
        for name in names:
            res.append(ada_name_from_camelcase(name))
        new_name = '_'.join(res)
    else:
        new_name = ada_name_from_camelcase(name)

    res = prefix + new_name + suffix

    # HACK: if the resulting name is an Ada reserved keyword, modify it to
    # not end up with a keyword (occurrence encountered for a parameter name).
    reserved_keywords = {'String': 'Str'}

    if str(res) in reserved_keywords:
        res = reserved_keywords[res]

    return res


def ada_name_from_camelcase(cpp_name_p, prefix="", suffix=""):
    out = ""
    upper_group = ""

    cpp_name = list(cpp_name_p)
    cpp_name[0] = cpp_name[0].upper()
    for c in cpp_name:
        if c.isupper() or c.isdigit():
            upper_group += c
        else:
            if upper_group:
                if upper_group[0:-1]:
                    upper_group = upper_group[0:-1] + "_" + upper_group[-1]
                if out:
                    out += "_"
                out += upper_group
                upper_group = ""
            out += c

    if upper_group:
        if out:
            out += "_"
        out += upper_group

    return out


def sanitize_unit_name_llvm(name):
    name = remove_prefix(name, ['llvm_c'])
    name = remove_suffix(name, ['_h', '_def'])
    sub_name = ada_name(name)
    package_name = 'LLVM.{}'.format(sub_name)
    filename = '{}.ads'.format(package_name.lower().replace('.', '-'))
    return package_name, filename


def sanitize_unit_name_clang(name):
    name = remove_prefix(name, ['clang_c'])
    name = remove_suffix(name, ['_h', '_def'])
    sub_name = ada_name(name)
    package_name = 'Clang.{}'.format(sub_name)
    filename = '{}.ads'.format(package_name.lower().replace('.', '-'))
    return package_name, filename


class FileAlteration(object):
    def __init__(self, line, start_col, end_line, end_col, string):
        self.line = line
        self.start_col = start_col
        self.end_col = end_col
        self.end_line = end_line
        self.string = string

    def apply(self, file_array):
        if self.line == self.end_line:
            file_array[self.line - 1][self.start_col - 1:self.end_col - 1] = \
                self.string.encode('utf-8')
        else:
            strn_splt = map(lambda string: string.encode('utf-8'),
                            self.string.split("\n"))
            # Disregard columns in multi-line mode because it is really simpler
            file_array[self.line - 1:self.end_line] = strn_splt

    def __repr__(self):
        return "<FileAlteration line={0}, start_col={1}, string={2}>".format(
            self.line, self.start_col, self.string
        )


def create_alteration(node, new_name):
    sloc = node.sloc_range
    return FileAlteration(
        sloc.start.line,
        sloc.start.column,
        sloc.end.line,
        sloc.end.column,
        new_name)


def get_ref_type(ref_node):
    return ref_node.attrib['ref'].split("/")[2]


def get_ads_path(filename):
    return path.join("gen/", filename)


def get_out_path(filename):
    return path.join("out/", filename)


def get_adb_path(filename):
    return path.join("out/", filename.replace(".ads", ".adb"))


def apply_alterations(alterations, file_lines):
    for a in sorted(alterations,
                    key=lambda a: a.line * 100000 + a.start_col,
                    reverse=True):
        a.apply(file_lines)


# Case insensitive version of the standard fun startswith
def startswith(n, prefixes):
    if not prefixes:
        return True
    for prefix in prefixes:
        if n.lower().startswith(prefix.lower()):
            return True
    return False


def get_decls(unit, prefixes):
    return unit.root.finditer(lambda n: n.is_a(lal.BasicDecl)
                              and n.p_defining_name
                              and startswith(n.p_defining_name.text, prefixes))


def get_idents(unit):
    return unit.root.finditer(lambda n: n.is_a(lal.Identifier))


def get_enum_literal_decls(unit, prefixes):
    return unit.root.finditer(lambda n: n.is_a(lal.EnumLiteralDecl)
                              and startswith(n.text, prefixes))


def get_params(unit):
    return unit.root.finditer(lambda n: n.is_a(lal.ParamSpec))


def get_subp_decl(unit):
    return unit.root.finditer(lambda n: n.is_a(lal.SubpDecl))


# Return the decl that stores the aspects
def get_aspects(subp_decl):
    aspects = (subp_decl.f_aspects.f_aspect_assocs
               if subp_decl.f_aspects else [])
    subp_spec = subp_decl.f_subp_spec
    # Work around a LAL bug where the function aspects are attached to the
    # declaration of the returned AnonymousType. TODO: Remove it when the LAL
    # bug is fixed. Also remove the function below.
    if (not aspects and subp_spec.f_subp_kind.is_a(lal.SubpKindFunction) and
            subp_spec.f_subp_returns.is_a(lal.AnonymousType)):
        return subp_spec.f_subp_returns.f_type_decl
    return subp_decl


# Get the actual text of a return; work around the bug described above.
# If we rely on f_subp_returns.text, we will also get the aspects text.
# TODO: use fun_spec.f_subp_returns.text instead of this when LAL bug is
# fixed.
def get_return_str(fun_spec):
    if (fun_spec.f_subp_returns.is_a(lal.AnonymousType)):
        return fun_spec.f_subp_returns.f_type_decl.f_type_def.text
    return fun_spec.f_subp_returns.text


def arguments_array(subp_spec):
    args = []
    if subp_spec.f_subp_params:
        params = subp_spec.f_subp_params.f_params
        for param in params:
            arg_name = param.f_ids.text
            arg_type = param.f_type_expr.text
            args.append(Argument(arg_name, arg_type))
    return args


def subp_tuple(subp_decl):
    sloc = subp_decl.sloc_range
    loc = ((sloc.start.line, sloc.start.column),
           (sloc.end.line, sloc.end.column))
    subp_spec = subp_decl.f_subp_spec
    aspect_decl = get_aspects(subp_decl)
    aspects = (aspect_decl.f_aspects.f_aspect_assocs if aspect_decl.f_aspects
               else [])
    if subp_spec.f_subp_kind.is_a(lal.SubpKindProcedure):
        subp_tuple = Procedure(subp_spec.f_subp_name.text,
                               arguments_array(subp_spec),
                               aspects)
    else:
        subp_tuple = Function(
            subp_spec.f_subp_name.text, get_return_str(subp_spec),
            arguments_array(subp_spec), aspects)

    return subp_decl, subp_tuple, loc


def process_names_for(filenames, prefixes_for_filename, sanitize_unit_name):
    # This map will track all alterations on a specific file
    alterations = {}

    # This map will track all identifier modifications (such as changing
    # the name of a package, of a subprocedure, or of a type).
    ident_modif = {}

    for filename in filenames:
        prefixes = prefixes_for_filename(filename)

        get_name = partial(ada_name, strips=prefixes)

        unit = context.get_from_file(filename)

        # Record all alterations for this file
        alterations[filename] = []

        old_package_name = unit.root.p_decl.f_package_name.text
        new_package_name, _ = sanitize_unit_name(old_package_name)

        # The code generator does not generate pragmas, but aspects, so we
        # will only consider those.
        cpp_decls = get_subp_decl(unit)
        for d in cpp_decls:
            aspect_decl = get_aspects(d)
            conv = aspect_decl.p_get_aspect("Convention").value
            ext_name = aspect_decl.p_get_aspect("External_Name").value
            alterations[filename].append(create_alteration(conv, "C"))
            alterations[filename].append(
                create_alteration(
                    ext_name, '"{}"'.format(d.p_defining_name.text))
            )

        # Fix the name of the package
        package_decl = unit.root.p_decl
        ident_modif[package_decl.p_defining_name.text] = new_package_name

        def get_type_name(tn):
            return get_name(remove_suffix(tn, ["Ref"]), suffix="_T")

        # Process file
        for decl in get_decls(unit, prefixes):
            def_name = decl.p_defining_name
            # Anonymous TypeDecl
            if not def_name:
                continue
            str_def_name = decl.p_defining_name.text

            # Ideally, instead of having to keep track of DefiningName
            # modifications, we would call p_find_all_references for all
            # units on the DefiningName being modified.
            # But this crashes so we work around it by tracking DefiningName
            # modifications and propagating them afterwards. It is probably
            # more efficient anyway.

            if decl.is_a(lal.ObjectDecl, lal.SubpDecl):

                # Yet another kludge. The -fdump-ada-spec now prefixes the enum
                # constants with the enum name. We don't want that for LLVM /
                # clang bindings so we will simply remove this prefix.

                if decl.is_a(lal.ObjectDecl):
                    str_def_name = remove_prefix(str_def_name,
                                                 [decl.f_type_expr.text + "_"])

                new_name = get_name(str_def_name)
                if (decl.is_a(lal.ObjectDecl) and
                        decl.f_type_expr.text == "LLVMOpcode"):
                    new_name = "Op_" + new_name
                ident_modif[decl.p_defining_name.text] = new_name

            elif decl.is_a(lal.BaseTypeDecl):

                # Special case for null records. These represent underlying
                # uncomplete C structure not defined in the C headers.
                # We will suffix them with Impl if not already done.
                #
                # The previous GNAT Ada spec generator was skipping these type
                # definitions, but do not anymore, so if we do not suffix them
                # with _Impl, we face name clashing issues.
                #
                # An example in the LLVM bindings is the following:
                #
                #   type ComdatRef is null record;
                #
                #   type Comdat is access all ComdatRef;
                #
                # As the suffix Ref is removed (see what the function
                # get_type_name above does), we would get:
                #
                #   type Comdat_T is null record;
                #
                #   type Comdat_T is access all Comdat_T;
                #
                # Prefixing beforehand the uncomplete type ensures we do not
                # run into these:
                #
                #   type Comdat_Impl_T is null record;
                #   type Comdat_T is access all Comdat_Impl_T

                if (decl.is_a(lal.TypeDecl) and
                    decl.f_type_def.is_a(lal.RecordTypeDef) and
                    decl.f_type_def.f_record_def.is_a(lal.NullRecordDef) and
                        not str_def_name.endswith("Impl")):
                    str_def_name = str_def_name + "Impl"

                new_name = get_type_name(str_def_name)
                ident_modif[decl.p_defining_name.text] = new_name

        for de in get_enum_literal_decls(unit, prefixes):
            alterations[filename].append(
                create_alteration(de.f_name, get_name(de.f_name.text))
            )

        for p in get_params(unit):
            alterations[filename].append(
                create_alteration(p.f_ids, get_name(p.f_ids.text))
            )

    # Propagate DefiningName modifications (types, subprocedures, packages)
    # then apply all alterations, which results in generating the altered file
    # in the `out` directory

    for filename in filenames:

        unit = context.get_from_file(filename)
        old_package_name = unit.root.p_decl.f_package_name.text
        _, new_file_name = sanitize_unit_name(old_package_name)

        for i in get_idents(unit):
            if (i.text in ident_modif):
                alterations[filename].append(
                    create_alteration(i, ident_modif[i.text])
                )

        with open(get_out_path(new_file_name), "w") as out_file:
            file_lines = [bytearray(line, encoding='utf-8')
                          for line
                          in open(filename).read().split("\n")]

            apply_alterations(alterations[filename], file_lines)

            # Remove empty line at the end of the file
            nb_lines = len(file_lines)
            out_file.write("\n".join(map(lambda line: line.decode('utf-8'),
                                         file_lines[:nb_lines - 1])))


def get_with(unit):
    return unit.root.finditer(lambda n: n.is_a(lal.WithClause))


def handle_dependencies(filename):
    unit = context.get_from_file("gen/" + filename)

    for w in get_with(unit):
        for name in w.f_packages:
            n = name.text
            if (n.endswith("_h") and not n.startswith("llvm") and
                    not n.startswith("clang")):
                dep_name = "{0}.ads".format(n)
                shutil.copy(get_ads_path(dep_name), get_out_path(dep_name))
                handle_dependencies(dep_name)


def subprogram_defs(unit):
    return unit.root.finditer(lambda n: n.is_a(lal.SubpDecl))


def generate_wrappers_for_file(file_name):
    unit = context.get_from_file(get_ads_path(file_name))
    package_name = unit.root.p_decl.f_package_name.text

    subp_defs = [subp_tuple(subp_def) for subp_def in subprogram_defs(unit)]

    with open(get_ads_path(file_name)) as in_file:
        file_lines = [bytearray(line, encoding='utf-8')
                      for line
                      in in_file.read().split("\n")]

    alterations = []
    is_adb_needed = False

    for subp_decl, subp_tpl, ((sl, sc), (el, ec)) in subp_defs:
        if is_wrapper_needed(subp_tpl):
            is_adb_needed = True

            alterations.append(
                FileAlteration(
                    sl, sc, el, ec, "\n   ".join(generate_decl(subp_tpl)))
            )

    apply_alterations(alterations, file_lines)

    with open(get_out_path(file_name), "w") as out_file:
        for line in file_lines:
            out_file.write(line.decode('utf-8') + "\n")

    if is_adb_needed:
        package = Package([package_name], [t for _, t, _ in subp_defs])
        with open(get_adb_path(file_name), "w") as out_file:
            for line in generate_body(package):
                out_file.write(line + "\n")


def llvm_prefixes(filename):
    if filename.find("lto") != -1:
        return ["lto"]
    else:
        return ["llvm"]


def clang_prefixes(filename):
    return ["clang", "CX"]


if __name__ == "__main__":
    context = lal.AnalysisContext(
        unit_provider=lal.UnitProvider.auto(argv[2:])
    )
    if argv[1] == "process_names_llvm":
        process_names_for(argv[2:], llvm_prefixes, sanitize_unit_name_llvm)
        for arg in argv[2:]:
            file_name = path.basename(arg)
            handle_dependencies(file_name)
    elif argv[1] == "process_names_clang":
        process_names_for(argv[2:], clang_prefixes, sanitize_unit_name_clang)
        for arg in argv[2:]:
            file_name = path.basename(arg)
            handle_dependencies(file_name)
    elif argv[1] == "generate_wrappers":
        for arg in argv[2:]:
            file_name = path.basename(arg)
            generate_wrappers_for_file(file_name)
    else:
        print("unexpected arguments")
