#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from collections import namedtuple

# Types used to describe a fragment of API to wrap.
Package = namedtuple('Package', 'name elements')
Function = namedtuple('Function', 'name return_type args aspects')
Procedure = namedtuple('Procedure', 'name args aspects')
Argument = namedtuple('Argument', 'name type')

INDENT = ' ' * 3


def include(substream, stream, indent=False):
    """
    Extend the "stream" list with the "substream" list of strings.

    If "indent", add one level of identation to the appenned lines.
    """
    if indent:
        substream = (INDENT + line for line in substream)
    stream.extend(substream)


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
        for i, aspect in enumerate(function.aspects):
            prefix = "with " if i == 0 else ' ' * 5
            suffix = "" if i + 1 == len(function.aspects) else ","
            result.append('{}{} => {}{}'.format(
                prefix, aspect.f_id.text, aspect.f_expr.text, suffix
            ))
        result[-1] += ';'
    return result
