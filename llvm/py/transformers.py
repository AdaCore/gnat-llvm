#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Small API to make wrappers around subprograms, bringing transformations to
either the parameters, or the returned value, such as:
 * Converting a parameter value from a type to another
 * Converting the return type from a type to another
"""

from collections import namedtuple

from utils import (Function, include, INDENT, get_prototype)

ADA_KEYWORDS = set('''and or xor not select'''.split())

LocalVariable = namedtuple('LocalVariable', 'name type value')


class Transformer:
    def __init__(self):
        pass

    def accept(self, subp):
        pass

    def transform_name(self, name):
        return name

    def transform_arg(self, arg):
        return arg

    def transform_return_type(self, return_type):
        return return_type

    def wrapper_fn(self, subp):
        wrapper_fn = subp._replace(args=[
            self.transform_arg(arg)
            for arg in subp.args
        ])
        wrapper_fn = wrapper_fn._replace(
            name=self.transform_name(subp.name)
        )
        if isinstance(subp, Function):
            wrapper_fn = wrapper_fn._replace(
                return_type=self.transform_return_type(
                    wrapper_fn.return_type
                ))

        return wrapper_fn

    def wrapper_body(self, subp):
        pass


class StandardTransformer(Transformer):
    """
    Encode general purpose transformations specifically for LLVM/Clang
    """
    def __init__(self):
        super().__init__()

    def transform_name(self, name):
        # Special case for instruction builder primitives: strip the "Build_"
        # prefix, unless the result is an Ada keyword.
        new_name = name
        if name.startswith('Build_'):
            n = new_name[6:]
            if n.lower() not in ADA_KEYWORDS:
                new_name = n
        # Also remove any "_Builder" suffix.
        new_name = new_name.replace('_Builder', '')
        return new_name

    def wrapper_fn(self, subp):
        wrapper_fn = super().wrapper_fn(subp)
        wrapper_fn = wrapper_fn._replace(aspects=[])
        return wrapper_fn


class AllTypesTransformer(StandardTransformer):
    def __init__(self, from_types, to_type):
        """
        Initialize a transformer that transform variables of from_type to
        to_type. The convert_arg callback takes an arg and convert it from any
        of the type in self.from_types to self.to_type, returning a tuple of
        (list[LocalVariable], string). The list of LocalVariable will be
        inserted in the declaration part of the wrapper, and the returned
        string is the new name of the argument that should be passed to the
        wrapped function.

        The convert_return callback takes a LocalVariable that holds the return
        value of the function (if the wrapped subprogram is a function) and
        transforms it. It returns a list[string] which is a list of statements,
        ended with a return.
        """
        self.from_types = from_types
        self.to_type = to_type

    def convert_arg(self, arg):
        raise NotImplementedError

    def convert_return(self, ret_variable):
        raise NotImplementedError

    def accept(self, subp):
        for arg in subp.args:
            if arg.type in self.from_types:
                return True
        if isinstance(subp, Function) and subp.return_type in self.from_types:
            return True

    def transform_arg(self, arg):
        if arg.type in self.from_types:
            return arg._replace(type=self.to_type)
        return arg

    def transform_return_type(self, return_type):
        if return_type in self.from_types:
            return self.to_type
        return return_type

    def wrapper_body(self, subp):

        result = []

        wrapper_fn = self.wrapper_fn(subp)
        include(get_prototype(wrapper_fn), result)

        call_args = {
            arg.name: arg.name
            for arg in subp.args
        }

        is_fnct = isinstance(subp, Function)

        local_vars = []
        wrapper_body = []

        if is_fnct:
            return_variable = LocalVariable(
                'Return_Value',
                subp.return_type,
                None)
            local_vars.append(return_variable)

        for arg in subp.args:
            if arg.type in self.from_types:
                (vars, new_arg_name) = self.convert_arg(arg)
                local_vars.extend(vars)
                call_args[arg.name] = new_arg_name

        wrapper_body.append('is')
        if local_vars:
            longest_name = max(len(var.name) for var in local_vars)
            for var in local_vars:
                wrapper_body.append('{}{} : {}{};'.format(
                    INDENT,
                    var.name.ljust(longest_name),
                    var.type,
                    ' := {}'.format(var.value) if var.value else ''
                ))

        wrapper_body.append('begin')

        # Generate the call to the C subprogram.
        call = subp.name
        if subp.args:
            call = '{} ({})'.format(call, ', '.join(
                call_args[arg.name] for arg in subp.args
            ))

        if is_fnct:
            wrapper_body.append('{}{} := {};'
                                .format(INDENT, return_variable.name, call))
            if subp.return_type in self.from_types:
                include(self.convert_return(return_variable),
                        wrapper_body,
                        indent=True)
            else:
                wrapper_body.append(
                    '{}return {};'.format(INDENT, return_variable.name)
                )
        else:
            wrapper_body.append('{}{};'.format(INDENT, call))

        wrapper_body.append('end {};'.format(wrapper_fn.name))

        include(wrapper_body, result)
        return result


class ReturnTypeTransformer(AllTypesTransformer):
    """
    A transformer that only transforms the return type.
    """

    def __init__(self, from_type, to_type):
        super().__init__(from_type, to_type)

    def transform_arg(self, arg):
        return arg

    def convert_arg(self, arg):
        return ([], arg.name)

    def accept(self, subp):
        if isinstance(subp, Function):
            return subp.return_type in self.from_types
        return False


class BooleanCheckTransformer(ReturnTypeTransformer):
    """
    A transformer to transform C-like boolean check functions to Ada-like
    boolean check functions.
    """

    def __init__(self, from_type, to_type):
        super().__init__(from_type, to_type)

    def accept(self, subp):
        return super().accept(subp) and "Is_" in subp.name
