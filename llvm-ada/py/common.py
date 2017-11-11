#! /usr/bin/python

from lxml import etree
import os
from os import path
import shutil
from sys import argv
import subprocess
from functools import partial
from lxml_subp import *
from wrapper import *
import wrapper


def sed(filename, script):
    """Execute a sed script on a file, in place."""
    subprocess.check_call(['sed', '-e', script, '-i', filename])


def gnat2xml(filename):
    os.chdir("./gen")
    subprocess.check_call(["gnat2xml", "-mxml", filename])
    os.chdir("../")


def remove_prefix(string, prefix):
    return string[len(prefix):] if string.startswith(prefix) else string


def remove_suffix(string, suffix):
    return string[:-len(suffix)] if string.endswith(suffix) else string


def ada_name(name, strip="llvm", prefix="", suffix=""):

    if name.lower().startswith(strip):
        name = name[4:]

    if name.startswith("_"):
        name = name[1:]

    if name.isupper():
        return name

    if "_" in name:
        name = name.capitalize()
        new_name = "".join([name[i].upper() if name[i - 1] == "_" else name[i]
                            for i in range(len(name))])
    else:
        new_name = ada_name_from_camelcase(name)

    return prefix + new_name + suffix


def ada_name_from_camelcase(cpp_name, prefix="", suffix=""):
    out = ""
    upper_group = ""

    for c in cpp_name:
        if c.isupper():
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
        out += "_"
        out += upper_group

    return out


def sanitize_unit_name(name):
    for prefix in ('llvm_c_', 'llvm_'):
        name = remove_prefix(name, prefix)
    for suffix in ('_h', '_def'):
        name = remove_suffix(name, suffix)
    sub_name = ada_name(name)
    package_name = 'LLVM.{}'.format(sub_name)
    filename = '{}.ads'.format(package_name.lower().replace('.', '-'))
    return package_name, filename


def di_name(defining_identifier):
    return defining_identifier.attrib['def_name']


def i_name(defining_identifier):
    return defining_identifier.attrib['ref_name']


class PackageTree(object):

    def xpath(self, xpath):
        return self.lxml_tree.xpath(xpath)

    def __init__(self, filename):
        with open(filename) as f:
            self.lxml_tree = etree.parse(f)

    def package_di(self):
        return self.lxml_tree.xpath(
            "//package_declaration/names_ql/defining_identifier"
        )[0]

    def package_name(self):
        return di_name(self.package_di())


class FileAlteration(object):
    def __init__(self, line, start_col, end_line, end_col, string):
        self.line = line
        self.start_col = start_col
        self.end_col = end_col
        self.end_line = end_line
        self.string = string

    def apply(self, file_array):
        if self.line == self.end_line:
            file_array[self.line - 1][self.start_col - 1:self.end_col] = \
                self.string
        else:
            strn_splt = self.string.split("\n")
            # Disregard columns in multi-line mode because it is really simpler
            file_array[self.line - 1:self.end_line] = strn_splt

    def __repr__(self):
        return "<FileAlteration line={0}, start_col={1}, string={2}>".format(
            self.line, self.start_col, self.string
        )


def create_alteration(node, new_name):
    sloc = node.getchildren()[0]
    return FileAlteration(
        int(sloc.attrib['line']),
        int(sloc.attrib['col']),
        int(sloc.attrib['endline']),
        int(sloc.attrib['endcol']),
        new_name
    )


def get_ref_type(ref_node):
    return ref_node.attrib['ref'].split("/")[2]


def get_xml_path(filename):
    return "{0}.xml".format(path.join("gen/xml", filename))


def get_ads_path(filename):
    return path.join("gen/", filename)


def get_out_path(filename):
    return path.join("out/", filename)


def get_adb_path(filename):
    return path.join("out/", filename.replace(".ads", ".adb"))

simple_subst_nodes = [
    "procedure_declaration",
    "function_declaration",
    "constant_declaration"
]

type_nodes = [
    "ordinary_type_declaration",
    "subtype_declaration"
]


def xp_startswith(attr_name, prefix):
    return "starts-with({0}, '{1}') or starts-with({0}, '{2}')".format(
        attr_name, prefix.lower(), prefix.upper()
    )


def apply_alterations(alterations, file_lines):
    for a in sorted(alterations,
                    key=lambda a: a.line * 100000 + a.start_col,
                    reverse=True):
        a.apply(file_lines)


def process_names_for(filename, prefix):
    get_name = partial(ada_name, strip=prefix)

    enum_lit_xpath = "//defining_enumeration_literal[{0}]".format(
        xp_startswith("@def_name", prefix)
    )
    tree = PackageTree(get_xml_path(filename))
    package_di = tree.package_di()
    old_package_name = di_name(package_di)
    new_package_name, new_file_name = sanitize_unit_name(old_package_name)

    # First, fix symbol mangling.
    sed(
        get_ads_path(filename),
        r's/pragma Import (CPP, \(.*\), ".*");/pragma Import (C, \1, "\1");/g'
    )

    # Fix the name of the package to LLVM.*
    sed(get_ads_path(filename), 's/^package {} is$/package {} is/g'.format(
        old_package_name, new_package_name
    ))

    sed(get_ads_path(filename), 's/^end {};$/end {};/g'.format(
        old_package_name, new_package_name
    ))

    def get_type_name(tn):
        return get_name(remove_suffix(tn, "Ref"), suffix="_T")

    with open(get_out_path(new_file_name), "w") as out_file:
        file_lines = [bytearray(line)
                      for line
                      in open(get_ads_path(filename)).read().split("\n")]
        alterations = []
        # Process file
        xpath = "//defining_identifier[{0}]".format(
            xp_startswith("@def_name", prefix))
        for di in tree.xpath(xpath):
            if di.getparent().getparent().tag in simple_subst_nodes:
                new_name = get_name(di_name(di))
                if "LLVMOpcode" in di.attrib.get("type", ""):
                    new_name = "Op_" + new_name
                alterations.append(
                    create_alteration(di, new_name)
                )
            elif di.getparent().getparent().tag in type_nodes:
                name = di_name(di)
                new_name = get_type_name(name)
                alterations.append(
                    create_alteration(di, new_name)
                )

        xpath = "//identifier[{0}]".format(xp_startswith("@ref_name", prefix))
        for i in tree.xpath(xpath):
            name = i_name(i)

            if get_ref_type(i) in ["function", "procedure"]:
                new_name = get_name(name)
            elif get_ref_type(i) in ["ordinary_type", "subtype"]:
                new_name = get_type_name(name)
            elif get_ref_type(i) == "package":
                new_name, _ = sanitize_unit_name(name)

            alterations.append(
                create_alteration(i, new_name)
            )

        for de in tree.xpath(enum_lit_xpath):
            alterations.append(
                create_alteration(de, get_name(di_name(de)))
            )

        for di in tree.xpath("//parameter_profile_ql//defining_identifier"):
            alterations.append(
                create_alteration(di, get_name(di_name(di)))
            )

        apply_alterations(alterations, file_lines)

        out_file.write("\n".join(map(str, file_lines)))


def handle_dependencies(file_name):
    xml_path = get_xml_path(file_name)
    tree = None
    with open(xml_path) as f:
        tree = etree.parse(f)

    for i in tree.xpath("//with_clause//identifier"):
        n = i_name(i)
        if n.endswith("_h") and not n.startswith("llvm"):
            dep_name = "{0}.ads".format(n)
            gnat2xml(dep_name)
            shutil.copy(get_ads_path(dep_name), get_out_path(dep_name))
            handle_dependencies(dep_name)


def generate_wrappers_for_file(file_name):
    tree = etree.parse(get_xml_path(file_name))
    package_name = tree.getroot().attrib["unit_full_name"].split(".")
    subp_defs = [subp_tuple(subp_def) for subp_def in subprogram_defs(tree)]

    with open(get_ads_path(file_name)) as in_file:
        file_lines = [bytearray(line)
                      for line
                      in in_file.read().split("\n")]

    alterations = []
    is_adb_needed = False

    # if fmt_name(package.name) == 'LLVM.Core':
    #     result += [
    #     ]

    for subp_def, subp_tpl, ((sl, sc), (el, ec)) in subp_defs:
        if is_wrapper_needed(subp_tpl):
            is_adb_needed = True

            import_pragma = subp_def.getnext()
            pragma_arg = import_pragma.findall(
                ".//pragma_argument_association")[1]

            sloc = pragma_arg.find("sloc")

            al, ac = int(sloc.attrib["line"]), int(sloc.attrib["col"])
            ael, aec = int(sloc.attrib["endline"]), int(sloc.attrib["endcol"])

            alterations.append(
                FileAlteration(al, ac, ael, aec, subp_tpl.name + "_C")
            )
            alterations.append(
                FileAlteration(
                    sl, sc, el, ec, "\n   ".join(generate_decl(subp_tpl)))
            )

    apply_alterations(alterations, file_lines)

    with open(get_out_path(file_name), "w") as out_file:
        for l in file_lines:
            out_file.write(l + "\n")

    if is_adb_needed:
        package = Package(package_name, [t for _, t, _ in subp_defs])
        with open(get_adb_path(file_name), "w") as out_file:
            for line in generate_body(package):
                out_file.write(line + "\n")


if __name__ == "__main__":
    if argv[1] == "process_names":
        for arg in argv[2:]:
            file_name = path.basename(arg)
            if arg.find("lto") != -1:
                process_names_for(file_name, "lto")
            else:
                process_names_for(file_name, "llvm")
            handle_dependencies(file_name)
    elif argv[1] == "generate_wrappers":
        for arg in argv[2:]:
            file_name = path.basename(arg)
            generate_wrappers_for_file(file_name)
