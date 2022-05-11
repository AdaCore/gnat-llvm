#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Generate wrappers for C functions that handle strings/booleans/...

Some C APIs (like LLVM's) handle C string (const char *) or other types in such
a way that they always make internal copies of strings passed to them, and
strings they return must not be free'd.

To make the use of such APIs convenient in Ada, thit script generates wrappers
for functions so that the conversions between C and Ada strings/booleans/...
are hidden.
"""

from transformers import (AllTypesTransformer, BooleanCheckTransformer,
                          INDENT, ReturnTypeTransformer, LocalVariable)

from utils import (
    Package, Function, Procedure, Argument,
    include, get_prototype
)


C_STRING = 'Interfaces.C.Strings.chars_ptr'
LLVM_BOOL = 'Bool_T'
LLVM_TYPES_BOOL = 'LLVM.Types.Bool_T'

# Internal representation of chars_ptr in libclang
CLANG_STRING = 'String_T'
CLANG_TYPES_STRING = "Clang.CX_String.String_T"


class C_String_Transformer(AllTypesTransformer):
    """
    Convert all uses of chars_ptr to Ada String.
    """

    def __init__(self):
        super().__init__([C_STRING], 'String')

    def convert_arg(self, arg):
        local_vars = []
        char_array = LocalVariable(
            '{}_Array'.format(arg.name),
            'aliased char_array',
            'To_C ({})'.format(arg.name)
        )
        chars_ptr = LocalVariable(
            '{}_String'.format(arg.name),
            'constant chars_ptr',
            "To_Chars_Ptr ({}'Unchecked_Access)".format(
                char_array.name
            )
        )
        local_vars.extend([char_array, chars_ptr])
        return (local_vars, chars_ptr.name)

    def convert_return(self, ret_variable):
        return [
            'if {} /= Null_Ptr then'.format(ret_variable.name),
            '   return Value ({});'.format(ret_variable.name),
            'else',
            '   return "";',
            'end if;']


class LLVM_Bool_Transformer(AllTypesTransformer):
    """
    Convert all uses of LLVM boolean type to Ada Boolean type.
    """

    def __init__(self):
        super().__init__([LLVM_BOOL, LLVM_TYPES_BOOL], 'Boolean')

    def convert_arg(self, arg):
        bool_t = LocalVariable(
            '{}_Bool'.format(arg.name),
            'constant LLVM.Types.Bool_T',
            "Boolean'Pos ({})".format(arg.name)
        )
        return ([bool_t], bool_t.name)

    def convert_return(self, ret_variable):
        return ["return {} /= 0;".format(ret_variable.name)]


class Clang_String_Transformer(ReturnTypeTransformer):
    """
    Convert returned values of CXString type to Ada String type.
    """

    def __init__(self):
        super().__init__([CLANG_TYPES_STRING], 'String')

    def convert_return(self, ret_variable):
        return [
            'declare'
            '{}Ada_String : String := Clang.CX_String.Get_C_String ({});'.format(
                INDENT, ret_variable.name
            ),
            'begin'
            '{}Clang.CX_String.Dispose_String ({});'.format(
                INDENT, ret_variable.name
            ),
            'return Ada_String;',
            'end;'
        ]


class Clang_Check_Transformer(BooleanCheckTransformer):
    """
    Specific to clang-c headers: as C doesn't have a boolean standard type,
    and the clang-c headers do not define a boolean type like LLVM does, we
    will assume that every function containing Is_ that returns an unsigned
    or an integer actually returns a boolean (encoded in the resulting
    unsigned / integer).
    """

    def __init__(self):
        super().__init__(['unsigned', 'int'], 'Boolean')

    def convert_return(self, ret_variable):
        return ["return {} /= 0;".format(ret_variable.name)]


TRANSFORMERS = [
    C_String_Transformer(),
    LLVM_Bool_Transformer(),
    Clang_String_Transformer(),
    Clang_Check_Transformer()
]


def fmt_name(name):
    """Format a structured name to a string."""
    return '.'.join(name)


def is_wrapper_needed(element):
    """
    Return whether an Ada wrapper is needed for the "element" C subprogram.
    """
    if element.name.startswith("Initialize_Native_"):
        return False

    for tr in TRANSFORMERS:
        if tr.accept(element):
            return True


def transform_fixpoint(subp, tr_callback):
    """
    While the subprogram can be transformed, keep transforming it. Every time we
    transform it, apply further transformations to the transformed subprogram
    instead of the original one.
    """
    accepted = True
    while accepted:
        accepted = False
        for tr in TRANSFORMERS:
            if tr.accept(subp):
                tr_callback(tr, subp)
                subp = tr.wrapper_fn(subp)
                accepted = True


def generate_body(package):
    result = [
        'pragma Style_Checks (Off);',
        'pragma Warnings (Off, "*is already use-visible*");',
        'pragma Warnings (Off, "*redundant with clause in body*");',
        '',
        'with Interfaces.C;         use Interfaces.C;',
        'pragma Unreferenced (Interfaces.C);',
        'with Interfaces.C.Strings; use Interfaces.C.Strings;'
        'pragma Unreferenced (Interfaces.C.Strings);',
        '',
        'package body {} is'.format(fmt_name(package.name)),
        ''
    ]

    for elt in package.elements:
        if not is_wrapper_needed(elt):
            continue

        # In that case, we do not expose the C declaration. The user
        # should use the wrappers.
        include(get_prototype(elt, decl=True), result, indent=True)

        def callback(tr, subp):
            include(tr.wrapper_body(subp), result, indent=True)
            result.append('')

        transform_fixpoint(elt, callback)

    result.append('end {};'.format(fmt_name(package.name)))

    return result


def generate_decl(function):
    exposed_decl = []

    # Expose only the end-wrapper decl (if we wrap the function several times,
    # we will not expose the intermediate wrappers, nor the C underlying
    # definition).

    def callback(tr, subp):
        nonlocal exposed_decl
        exposed_decl = get_prototype(tr.wrapper_fn(subp), decl=True)

    transform_fixpoint(
        function,
        callback
        )

    return exposed_decl


if __name__ == '__main__':

    def test():
        def name(string):
            return string.split('.')
        p = Package(name('LLVM.Core'), [
            Function('Module_Create_With_Name', 'Module', [
                Argument('ModuleID', 'Interfaces.C.Strings.chars_ptr'),
            ], []),
            Function('Get_Data_Layout', 'Interfaces.C.Strings.chars_ptr', [
                Argument('M', 'Module'),
            ], []),
            Procedure('Set_Data_Layout', [
                Argument('M', 'Module'),
                Argument('Triple', 'Interfaces.C.Strings.chars_ptr'),
            ], []),
            Procedure('Set_Some_Flag', [
                Argument('M', 'Module'),
                Argument('Flag', 'Bool_T'),
            ], []),
            Function('Get_Some_Flag', 'Bool_T', [
                Argument('M', 'Module'),
            ], []),
        ])

        print('--  Specification')
        print('with Interfaces.C.Strings;')
        print('')
        print('package {} is'.format(fmt_name(p.name)))
        print('')
        for elt in p.elements:
            for line in generate_decl(elt):
                print('   {}'.format(line))
            print('')
        print('end {};'.format(fmt_name(p.name)))

        print('')
        print('--  Body')
        for line in generate_body(p):
            print(line)

    test()
