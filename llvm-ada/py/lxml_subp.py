from collections import namedtuple
from wrapper import Argument, Function, Procedure, Package


def subprogram_defs(lxml_tree):
    return lxml_tree.xpath("//function_declaration")\
           + lxml_tree.xpath("//procedure_declaration")


def str_type(objdecl):
    idd = objdecl.findall(".//identifier")
    return ".".join((i.attrib["ref_name"] for i in idd))


def arguments_array(subp_def):
    args = []
    for param_spec in subp_def.find("parameter_profile_ql").getchildren():
        arg_name = param_spec.find(
            "names_ql/defining_identifier").attrib["def_name"]
        arg_type = str_type(param_spec.find("object_declaration_view_q"))
        args.append(Argument(arg_name, arg_type))
    return args


def subp_name(subp_def):
    return subp_def.find(".//names_ql/defining_identifier").attrib["def_name"]


def func_return_type(func_def):
    return str_type(func_def.find("result_profile_q"))


def subp_tuple(subp_def):
    sloc = subp_def.find("sloc")
    loc = ((int(sloc.attrib["line"]), int(sloc.attrib["col"])),
           (int(sloc.attrib["endline"]), int(sloc.attrib["endcol"])))
    if subp_def.tag == "procedure_declaration":
        subp_tuple = Procedure(subp_name(subp_def), arguments_array(subp_def))
    elif subp_def.tag == "function_declaration":
        subp_tuple = Function(
            subp_name(subp_def), func_return_type(subp_def),
            arguments_array(subp_def))

    return subp_def, subp_tuple, loc
