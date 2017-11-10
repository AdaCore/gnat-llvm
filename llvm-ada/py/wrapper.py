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

ADA_KEYWORDS = set('''and or xor not select'''.split())

from collections import namedtuple

# Types used to describe a fragment of API to wrap.
Package = namedtuple('Package', 'name elements')
Function = namedtuple('Function', 'name return_type args')
Procedure = namedtuple('Procedure', 'name args')
Argument = namedtuple('Argument', 'name type')

INDENT = ' ' * 3
C_STRING = 'Interfaces.C.Strings.chars_ptr'
LLVM_BOOL = 'Bool_T'
LLVM_CORE_BOOL = 'LLVM.Core.Bool_T'
LLVM_BUILDER = 'Builder_T'

TYPES_TRANSLATION_TABLE = {
    C_STRING: 'String',
    LLVM_BOOL: 'Boolean',
    LLVM_CORE_BOOL: 'Boolean',
    LLVM_BUILDER: 'Base_Builder_T',
}


def translate_type(type):
    """Translate a type, if needed."""
    return TYPES_TRANSLATION_TABLE.get(type, type)


def fmt_name(name):
    """Format a structured name to a string."""
    return '.'.join(name)


def include(substream, stream, indent=False):
    """
    Extend the "stream" list with the "substream" list of strings.

    If "indent", add one level of identation to the appenned lines.
    """
    if indent:
        substream = (INDENT + line for line in substream)
    stream.extend(substream)


def is_wrapper_needed(element):
    """
    Return whether an Ada wrapper is needed for the "element" C subprogram.
    """
    for arg in element.args:
        if arg.type in TYPES_TRANSLATION_TABLE:
            return True

    return (
        isinstance(element, Function) and
        element.return_type in TYPES_TRANSLATION_TABLE
    )


def get_wrapped(element):
    """
    Return the prototype of the Ada binding for the "element" C subprogram to
    wrap.
    """
    return element._replace(name='{}_C'.format(element.name))


def get_wrapper(element):
    """
    Return the prototype of the Ada subprogram that wraps the "element" C
    subprogram.
    """
    wrapper_fn = element._replace(args=[
        arg._replace(type=translate_type(arg.type))
        for arg in element.args
    ])

    if isinstance(wrapper_fn, Function):
        wrapper_fn = wrapper_fn._replace(
            return_type=translate_type(wrapper_fn.return_type)
        )

    new_name = wrapper_fn.name

    # Special case for instruction builder primivites: strip the "Build_"
    # prefix, unless the result is an Ada keyword.
    if new_name.startswith('Build_'):
        n = new_name[6:]
        if n.lower() not in ADA_KEYWORDS:
            new_name = n
    # Also remove any "_Builder" suffix.
    new_name = new_name.replace('_Builder', '')

    wrapper_fn = wrapper_fn._replace(name=new_name)

    return wrapper_fn


def get_prototype(function, decl=False):
    """
    Format the prototype of the "function" subprogram in Ada.

    If "decl", append ";" at the end, so it can be used as a subprogram
    declaration.
    """
    is_fnct = isinstance(function, Function)

    result = ['{} {}'.format(
        'function' if is_fnct else 'procedure', function.name
    )]
    if function.args:
        longest_name = max(len(arg.name) for arg in function.args)
        for i, arg in enumerate(function.args):
            prefix = (' ' * 2 + '(') if i == 0 else INDENT
            suffix = ')' if i + 1 == len(function.args) else ';'
            result.append('{}{} : {}{}'.format(
                prefix, arg.name.ljust(longest_name), arg.type, suffix
            ))
    if is_fnct:
        result.append('{}return {}'.format(INDENT, function.return_type))
    if decl:
        result[-1] += ';'
    return result


def generate_body(package):
    result = [
        'pragma Ada_2005;',
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
        elt_body = []
        is_fnct = isinstance(elt, Function)

        # If there is no type to translate, no need for a wrapper: go next.
        if not is_wrapper_needed(elt):
            continue

        orig_name = elt.name
        wrapped_elt = get_wrapper(elt)

        # Generate the header of the wrapper subprogram (name, args and return
        # type).
        include(get_prototype(get_wrapper(elt)), elt_body)

        # Mapping argument name -> object name to pass the the wrapped
        # subprogram.
        call_args = {
            arg.name: arg.name
            for arg in elt.args
        }

        local_vars = []
        LocalVariable = namedtuple('LocalVariable', 'name type value')

        # Now declare variables, to hold conversion temporaries.
        for arg in elt.args:
            if arg.type == C_STRING:
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
                call_args[arg.name] = chars_ptr.name
            elif arg.type in (LLVM_BOOL, LLVM_CORE_BOOL):
                bool_t = LocalVariable(
                    '{}_Bool'.format(arg.name),
                    'constant Bool_T',
                    "Boolean'Pos ({})".format(arg.name)
                )
                local_vars.append(bool_t)
                call_args[arg.name] = bool_t.name
            elif arg.type == LLVM_BUILDER:
                call_args[arg.name] = arg.name + ".Bld"

        # Emit them.
        elt_body.append('is')
        if local_vars:
            longest_name = max(len(var.name) for var in local_vars)
            for var in local_vars:
                elt_body.append('{}{} : {} := {};'.format(
                    INDENT,
                    var.name.ljust(longest_name), var.type, var.value
                ))

        elt_body.append('begin')

        # Generate the call to the C subprogram.
        call = '{}_C'.format(orig_name)
        if elt.args:
            call = '{} ({})'.format(call, ', '.join(
                call_args[arg.name] for arg in elt.args
            ))

        # Make some type conversion for the result if needed.
        if is_fnct:
            if elt.return_type == C_STRING:
                call = 'Value ({})'.format(call)
            elif elt.return_type in (LLVM_BOOL, LLVM_CORE_BOOL):
                call = "{} /= 0".format(call)
            elif elt.return_type == LLVM_BUILDER:
                call = '(Bld => {})'.format(call)
            stmt = 'return {}'.format(call)
        else:
            stmt = call
        elt_body.append('{}{};'.format(INDENT, stmt))

        # And then, the wrapper is done!
        elt_body.append('end {};'.format(wrapped_elt.name))
        include(elt_body, result, indent=True)
        result.append('')

    result.append('end {};'.format(fmt_name(package.name)))

    return result


def generate_decl(function):
    wrapped_fn = get_wrapped(function)
    ret = (
        get_prototype(get_wrapper(function), decl=True) +
        get_prototype(wrapped_fn, decl=True)
    )
    return ret

if __name__ == '__main__':

    def test():
        def name(string):
            return string.split('.')
        p = Package(name('LLVM.Core'), [
            Function('Module_Create_With_Name', 'Module', [
                Argument('ModuleID', 'Interfaces.C.Strings.chars_ptr'),
            ]),
            Function('Get_Data_Layout', 'Interfaces.C.Strings.chars_ptr', [
                Argument('M', 'Module'),
            ]),
            Procedure('Set_Data_Layout', [
                Argument('M', 'Module'),
                Argument('Triple', 'Interfaces.C.Strings.chars_ptr'),
            ]),
            Procedure('Set_Some_Flag', [
                Argument('M', 'Module'),
                Argument('Flag', 'Bool_T'),
            ]),
            Function('Get_Some_Flag', 'Bool_T', [
                Argument('M', 'Module'),
            ]),
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
