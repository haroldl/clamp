#!/usr/bin/env python

CLAMP_VERBOSE = False

"""
Python to Common Lisp Compiler
"""

import ast
from dataclasses import dataclass, replace
import sys
from pathlib import Path

PYTHON_KEYWORDS = {
    "and",
    "as",
    "assert",
    "async",
    "await",
    "break",
    "class",
    "continue",
    "def",
    "del",
    "elif",
    "else",
    "except",
    "False",
    "finally",
    "for",
    "from",
    "global",
    "if",
    "import",
    "in",
    "is",
    "lambda",
    "None",
    "nonlocal",
    "not",
    "or",
    "pass",
    "raise",
    "return",
    "True",
    "try",
    "while",
    "with",
    "yield",
}

codegen_handlers = {}


@dataclass
class Context():
    top_level_stmt: bool = True
    block_name: str | None = None
    mutation_context: bool = False
    module_name: str = "__main__"
    package_name: str = "CLAMP"
    source_path: str | None = None
    loop_block_name: str | None = None
    loop_continue_name: str | None = None
    loop_broke_name: str | None = None
    in_async_function: bool = False
    in_generator_function: bool = False
    module_bound_names: frozenset[str] = frozenset()
    future_annotations: bool = False
    current_exception_symbol: str | None = None
    force_global_names: frozenset[str] = frozenset()
    local_names: frozenset[str] = frozenset()

    def child(self):
        return replace(self, top_level_stmt = False)


def codegen(node, context : Context = Context(top_level_stmt=False)):
    """Recursive codegen of Common Lisp from a Python AST"""
    typ = type(node)
    if typ in codegen_handlers:
        return codegen_handlers[typ](node, context)
    else:
        raise Exception(
            f"Do not have support to codegen {str(type(node))} node with value {ast.dump(node)}"
        )


def target_binding_names(target):
    if isinstance(target, ast.Name):
        return [target.id]
    if isinstance(target, (ast.Tuple, ast.List)):
        names = []
        for elt in target.elts:
            for name in target_binding_names(elt):
                if name not in names:
                    names.append(name)
        return names
    return []


def codegen_target_bindings(target, context: Context):
    return " ".join(
        f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
        for name in target_binding_names(target)
    )


def codegen_store_target(target, value_code: str, context: Context):
    child_context = context.child()
    if isinstance(target, ast.Name):
        lhs = codegen(target, child_context)
        if context.top_level_stmt:
            return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(target.id, child_context)} {lhs} {value_code})"
        return f"(common-lisp:setf {lhs} {value_code})"
    if isinstance(target, ast.Subscript):
        return codegen_subscript_store(target, value_code, child_context)
    if isinstance(target, ast.Attribute):
        return codegen_attribute_store(target, value_code, child_context)
    if isinstance(target, (ast.Tuple, ast.List)):
        starred = [index for index, elt in enumerate(target.elts) if isinstance(elt, ast.Starred)]
        unpacked_symbol = f"__clamp_unpack_{id(target)}"
        if starred:
            if len(starred) != 1:
                raise Exception("multiple starred assignment targets")
            starred_index = starred[0]
            after_count = len(target.elts) - starred_index - 1
            length_symbol = f"__clamp_unpack_len_{id(target)}"
            stores = []
            for index, elt in enumerate(target.elts):
                if isinstance(elt, ast.Starred):
                    stores.append(
                        codegen_store_target(
                            elt.value,
                            f"(common-lisp:apply #'|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST "
                            f"(common-lisp:subseq {unpacked_symbol} {starred_index} (common-lisp:- {length_symbol} {after_count})))",
                            context,
                        )
                    )
                elif index < starred_index:
                    stores.append(codegen_store_target(elt, f"(common-lisp:nth {index} {unpacked_symbol})", context))
                else:
                    offset = len(target.elts) - index
                    stores.append(codegen_store_target(elt, f"(common-lisp:nth (common-lisp:- {length_symbol} {offset}) {unpacked_symbol})", context))
            return (
                f"(common-lisp:let* (({unpacked_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-ITERABLE-TO-LIST {value_code})) "
                f"({length_symbol} (common-lisp:length {unpacked_symbol}))) "
                f"(common-lisp:when (common-lisp:< {length_symbol} {len(target.elts) - 1}) "
                f"(common-lisp:error \"not enough values to unpack\")) "
                + " ".join(stores)
                + ")"
            )
        stores = " ".join(
            codegen_store_target(elt, f"(common-lisp:nth {index} {unpacked_symbol})", context)
            for index, elt in enumerate(target.elts)
        )
        return (
            f"(common-lisp:let (({unpacked_symbol} "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-UNPACK-SEQUENCE {value_code} {len(target.elts)}))) "
            f"{stores})"
        )
    raise Exception(f"TODO: unsupported assignment target {type(target)}")



def namedexpr_target_names(node):
    names = []
    for child in ast.walk(node):
        if isinstance(child, ast.NamedExpr):
            if not isinstance(child.target, ast.Name):
                raise Exception("TODO: unsupported named expression target")
            names.append(child.target.id)
    return names


def codegen_namedexpr(node, context: Context):
    if not isinstance(node.target, ast.Name):
        raise Exception("TODO: unsupported named expression target")
    target = map_name(node.target.id)
    value = codegen(node.value, context.child())
    if context.top_level_stmt:
        return (
            f"(common-lisp:let (({target} {value})) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {lisp_string(node.target.id)} {target} {target}) "
            f"{target})"
        )
    return f"(common-lisp:setf {target} {value})"

def codegen_assign(node, context : Context):
    if len(node.targets) != 1:
        raise Exception("TODO: destructuring bind")
    return codegen_store_target(node.targets[0], codegen(node.value, context.child()), context)


def codegen_annassign(node, context: Context):
    if node.value is None:
        return "COMMON-LISP::nil"
    return codegen_store_target(node.target, codegen(node.value, context.child()), context)


def lisp_float(value: float) -> str:
    text = repr(value)
    if text == "inf":
        return "sb-ext:double-float-positive-infinity"
    if text == "-inf":
        return "sb-ext:double-float-negative-infinity"
    if text == "nan":
        return "(sb-kernel:make-double-float #x7ff80000 0)"
    if "e" in text.lower():
        return text.replace("e", "d").replace("E", "d")
    if "." in text:
        return text + "d0"
    return text + ".0d0"


def codegen_constant(node, context: Context):
    if node.value is Ellipsis:
        return "|CLAMP.__CLAMP_INTERNALS__|:*PY-ELLIPSIS*"
    if isinstance(node.value, complex):
        return f"(common-lisp:complex {lisp_float(node.value.real)} {lisp_float(node.value.imag)})"
    return codegen(node.value, context)


def node_contains_yield(node):
    class YieldFinder(ast.NodeVisitor):
        def __init__(self):
            self.found = False
            self._root = node

        def visit_Yield(self, child):
            self.found = True

        def visit_YieldFrom(self, child):
            self.found = True

        def visit_FunctionDef(self, child):
            if child is self._root:
                self.generic_visit(child)

        def visit_AsyncFunctionDef(self, child):
            if child is self._root:
                self.generic_visit(child)

        def visit_Lambda(self, child):
            if child is self._root:
                self.generic_visit(child)

        def visit_ClassDef(self, child):
            if child is self._root:
                self.generic_visit(child)

    finder = YieldFinder()
    finder.visit(node)
    return finder.found


def apply_function_decorators(node, context: Context, target_expr: str) -> str:
    decorated = target_expr
    for decorator in reversed(node.decorator_list):
        decorated = (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE "
            f"{codegen(decorator, context.child())} {decorated})"
        )
    return decorated


def decorated_function_rebind(node, context: Context) -> str:
    if not node.decorator_list:
        return ""
    target = map_name(node.name)
    decorated = apply_function_decorators(node, context, target)
    if context.top_level_stmt:
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL "
            f"{codegen(node.name, context.child())} {target} {decorated})"
        )
    return f"(common-lisp:setf {target} {decorated})"


def codegen_block(stmts, context: Context) -> str:
    """Generate code for a sequence of statements with lexical assignment semantics.

    When encountering an assignment, we emit a (let ((var value)) ...) that wraps
    the remainder of the statements in the same block to create the correct
    lexical scope for the newly introduced binding.
    """
    if not stmts:
        return ""

    first, rest = stmts[0], stmts[1:]

    # Handle assignment using ASSIGN macro to provide correct scoping
    if isinstance(first, ast.Assign):
        if len(first.targets) != 1:
            rhs = codegen(first.value, context.child())
            value_symbol = f"__clamp_assign_value_{id(first)}"
            stores = " ".join(codegen_store_target(target, value_symbol, context) for target in first.targets)
            first_code = f"(common-lisp:let (({value_symbol} {rhs})) {stores})"
            rest_code = codegen_block(rest, context)
            if not context.top_level_stmt and not context.mutation_context:
                names = []
                for target in first.targets:
                    for name in target_binding_names(target):
                        if name not in names:
                            names.append(name)
                if names:
                    bindings = " ".join(
                        f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
                        for name in names
                    )
                    body = first_code + ("\n" + rest_code if rest_code else "")
                    return f"(common-lisp:let ({bindings}) {body})"
            return first_code + ("\n" + rest_code if rest_code else "")
        target = first.targets[0]
        if isinstance(target, ast.Subscript):
            first_code = codegen_subscript_store(target, codegen(first.value, context.child()), context.child())
            rest_code = codegen_block(rest, context)
            return first_code + ("\n" + rest_code if rest_code else "")
        if isinstance(target, ast.Attribute):
            first_code = codegen_attribute_store(target, codegen(first.value, context.child()), context.child())
            rest_code = codegen_block(rest, context)
            return first_code + ("\n" + rest_code if rest_code else "")
        if not isinstance(target, ast.Name):
            rhs = codegen(first.value, context.child())
            first_code = codegen_store_target(target, rhs, context)
            rest_code = codegen_block(rest, context)
            if isinstance(target, (ast.Tuple, ast.List)) and not context.top_level_stmt and not context.mutation_context:
                bindings = codegen_target_bindings(target, context)
                if bindings:
                    body = first_code + ("\n" + rest_code if rest_code else "")
                    return f"(common-lisp:let ({bindings}) {body})"
            return first_code + ("\n" + rest_code if rest_code else "")

        lhs = codegen(target, context.child())
        rhs = codegen(first.value, context.child())
        rest_code = codegen_block(rest, context)
        if context.top_level_stmt:
            # Top-level module assignment: set global and continue
            if rest_code:
                return f"(common-lisp:progn (|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(target.id, context.child())} {lhs} {rhs}) {rest_code})"
            else:
                return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(target.id, context.child())} {lhs} {rhs})"
        elif context.mutation_context:
            assignment_code = f"(common-lisp:setf {lhs} {rhs})"
            return assignment_code + ("\n" + rest_code if rest_code else "")
        else:
            # Lexical binding within function or inner block
            if rest_code:
                return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} {rhs}) {rest_code})"
            else:
                return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} {rhs}))"

    if isinstance(first, ast.AugAssign):
        first_code = codegen_augassign(first, context)
        rest_code = codegen_block(rest, context)
        return first_code + ("\n" + rest_code if rest_code else "")

    if isinstance(first, ast.AnnAssign) and first.value is None and isinstance(first.target, ast.Name):
        if context.top_level_stmt:
            return codegen_block(rest, context)
        if context.mutation_context:
            return codegen_block(rest, context)
        lhs = codegen(first.target, context.child())
        mutation_context = replace(context, mutation_context=True)
        rest_code = codegen_block(rest, mutation_context)
        if rest_code:
            return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*) {rest_code})"
        return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*))"

    if isinstance(first, (ast.For, ast.AsyncFor, ast.AsyncWith)) and not context.top_level_stmt and not context.mutation_context:
        names = []
        if isinstance(first, ast.AsyncWith):
            for item in first.items:
                if item.optional_vars:
                    for name in target_binding_names(item.optional_vars):
                        if name not in names:
                            names.append(name)
        else:
            names = target_binding_names(first.target)
        for name in module_binding_names(rest):
            if name not in names:
                names.append(name)
        if names:
            bindings = " ".join(
                f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
                for name in names
            )
            mutation_context = replace(context, mutation_context=True)
            first_code = codegen(first, mutation_context)
            rest_code = codegen_block(rest, mutation_context)
            body = first_code + ("\n" + rest_code if rest_code else "")
            return f"(common-lisp:let ({bindings}) {body})"

    if isinstance(first, (ast.If, ast.Try)) and not context.top_level_stmt and not context.mutation_context:
        names = module_binding_names([first, *rest])
        for stmt in [first, *rest]:
            for name in namedexpr_target_names(stmt):
                if name not in names:
                    names.append(name)
        if names:
            bindings = " ".join(
                f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
                for name in names
            )
            mutation_context = replace(context, mutation_context=True)
            first_code = codegen(first, mutation_context)
            rest_code = codegen_block(rest, mutation_context)
            body = first_code + ("\n" + rest_code if rest_code else "")
            return f"(common-lisp:let ({bindings}) {body})"

    if isinstance(first, (ast.Import, ast.ImportFrom)) and not context.top_level_stmt and not context.mutation_context:
        return codegen_import_block(first, rest, context)

    # Default: emit first form and then the rest
    first_code = codegen(first, context)
    rest_code = codegen_block(rest, context)
    body = first_code + ("\n" + rest_code if rest_code else "")
    walrus_names = [] if context.top_level_stmt or context.mutation_context else namedexpr_target_names(first)
    if walrus_names:
        bindings = " ".join(
            f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
            for name in dict.fromkeys(walrus_names)
        )
        return f"(common-lisp:let ({bindings}) {body})"
    return body



def codegen_callable_signature_options(args, default_symbols=None, kw_default_symbols=None) -> str:
    default_symbols = default_symbols or []
    kw_default_symbols = kw_default_symbols or []
    positional_names = [arg.arg for arg in [*args.posonlyargs, *args.args]]
    kwonly_names = [arg.arg for arg in args.kwonlyargs]

    def quoted_list(names):
        if not names:
            return "COMMON-LISP:NIL"
        return "'(" + " ".join(lisp_string(name) for name in names) + ")"

    def value_list(values):
        if not values:
            return "COMMON-LISP:NIL"
        return "(common-lisp:list " + " ".join(values) + ")"

    empty = "|CLAMP.__CLAMP_INTERNALS__|::*PY-INSPECT-EMPTY*"
    positional_defaults = [empty for _ in positional_names]
    first_default = len(positional_names) - len(default_symbols)
    for index, symbol in enumerate(default_symbols):
        target = first_default + index
        if 0 <= target < len(positional_defaults):
            positional_defaults[target] = symbol

    kw_defaults = []
    kw_symbol_index = 0
    for default in args.kw_defaults:
        if default is None:
            kw_defaults.append(empty)
        else:
            kw_defaults.append(kw_default_symbols[kw_symbol_index])
            kw_symbol_index += 1

    vararg = lisp_string(args.vararg.arg) if args.vararg else "COMMON-LISP:NIL"
    kwarg = lisp_string(args.kwarg.arg) if args.kwarg else "COMMON-LISP:NIL"
    return (
        f" :SIGNATURE-PARAM-NAMES {quoted_list(positional_names)}"
        f" :SIGNATURE-PARAM-DEFAULTS {value_list(positional_defaults)}"
        f" :SIGNATURE-KWONLY-NAMES {quoted_list(kwonly_names)}"
        f" :SIGNATURE-KWONLY-DEFAULTS {value_list(kw_defaults)}"
        f" :SIGNATURE-VARARG-NAME {vararg}"
        f" :SIGNATURE-KWARG-NAME {kwarg}"
    )


def function_parameter_names(args) -> set[str]:
    names = {arg.arg for arg in [*args.posonlyargs, *args.args, *args.kwonlyargs]}
    if args.vararg:
        names.add(args.vararg.arg)
    if args.kwarg:
        names.add(args.kwarg.arg)
    return names


def codegen_function_local_bindings(node) -> tuple[list[str], str]:
    parameter_names = function_parameter_names(node.args)
    local_names = [
        name for name in module_binding_names(node.body)
        if name not in parameter_names
    ]
    for stmt in node.body:
        for name in namedexpr_target_names(stmt):
            if name not in parameter_names and name not in local_names:
                local_names.append(name)
    if not local_names:
        return [], ""
    bindings = " ".join(
        f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
        for name in local_names
    )
    return local_names, bindings


def codegen_args_with_keyword_support(args, context: Context, default_symbols=None, kw_default_symbols=None, owner_id=None):
    default_symbols = default_symbols or []
    kw_default_symbols = kw_default_symbols or []
    owner_id = owner_id or id(args)
    all_positional_args = [*args.posonlyargs, *args.args]
    posonly_count = len(args.posonlyargs)
    call_args = f"__clamp_call_args_{owner_id}"
    bound_args = f"__clamp_bound_args_{owner_id}"
    param_names = "'(" + " ".join(lisp_string(arg.arg) for arg in all_positional_args) + ")"
    required_count = len(all_positional_args) - len(default_symbols)
    defaults = (
        "(common-lisp:list " + " ".join(default_symbols) + ")"
        if default_symbols else
        "COMMON-LISP::nil"
    )
    kwonly_names = "'(" + " ".join(lisp_string(arg.arg) for arg in args.kwonlyargs) + ")"
    required_kwonly_names = "'(" + " ".join(
        lisp_string(arg.arg)
        for arg, default in zip(args.kwonlyargs, args.kw_defaults)
        if default is None
    ) + ")"
    kw_default_by_arg = []
    kw_symbol_iter = iter(kw_default_symbols)
    for default in args.kw_defaults:
        if default is None:
            kw_default_by_arg.append("|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*")
        else:
            kw_default_by_arg.append(next(kw_symbol_iter))
    kw_defaults = (
        "(common-lisp:list " + " ".join(kw_default_by_arg) + ")"
        if kw_default_by_arg else
        "COMMON-LISP::nil"
    )
    has_vararg = "COMMON-LISP:T" if args.vararg else "COMMON-LISP:NIL"
    has_kwarg = "COMMON-LISP:T" if args.kwarg else "COMMON-LISP:NIL"
    lambda_list = f"common-lisp:&rest {call_args}"
    binding_specs = [
        f"({map_name(arg.arg)} (common-lisp:nth {index} {bound_args}))"
        for index, arg in enumerate(all_positional_args)
    ]
    offset = len(all_positional_args)
    binding_specs.extend(
        f"({map_name(arg.arg)} (common-lisp:nth {offset + index} {bound_args}))"
        for index, arg in enumerate(args.kwonlyargs)
    )
    offset += len(args.kwonlyargs)
    if args.vararg:
        binding_specs.append(f"({map_name(args.vararg.arg)} (common-lisp:nth {offset} {bound_args}))")
        offset += 1
    if args.kwarg:
        binding_specs.append(f"({map_name(args.kwarg.arg)} (common-lisp:nth {offset} {bound_args}))")
    body_prefix = (
        f"(common-lisp:let* (({bound_args} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-BIND-ARGS-EXTENDED {lisp_string(owner_id if isinstance(owner_id, str) else str(owner_id))} "
        f"{param_names} {required_count} {posonly_count} {defaults} {kwonly_names} {required_kwonly_names} {kw_defaults} {has_vararg} {has_kwarg} {call_args})) "
        + " ".join(binding_specs)
        + ") "
    )
    body_suffix = ")"
    return lambda_list, body_prefix, body_suffix



def codegen_lambda(node, context: Context):
    child_context = context.child()
    owner_id = f"<lambda-{id(node)}>"
    default_symbols = [
        f"__clamp_lambda_default_{id(node)}_{index}"
        for index, _ in enumerate(node.args.defaults)
    ]
    kw_default_symbols = [
        f"__clamp_lambda_kw_default_{id(node)}_{index}"
        for index, default in enumerate(node.args.kw_defaults)
        if default is not None
    ]
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(
        node.args, child_context, default_symbols, kw_default_symbols, owner_id
    )
    signature_options = codegen_callable_signature_options(node.args, default_symbols, kw_default_symbols)
    default_binding_specs = [
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(default_symbols, node.args.defaults)
    ]
    default_binding_specs.extend(
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(kw_default_symbols, (d for d in node.args.kw_defaults if d is not None))
    )
    default_bindings = ""
    default_suffix = ""
    if default_binding_specs:
        default_bindings = "(common-lisp:let (" + " ".join(default_binding_specs) + ") "
        default_suffix = ")"
    forced_global_names = frozenset(
        name for name in context.force_global_names
        if name not in function_parameter_names(node.args)
    )
    body_context = replace(
        child_context,
        block_name=f"__clamp_lambda_block_{id(node)}",
        force_global_names=forced_global_names,
        local_names=frozenset(function_parameter_names(node.args)),
    )
    body = codegen(node.body, body_context)
    return (
        default_bindings
        + f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string('<lambda>')}{signature_options} :FN "
        + f"(common-lisp:lambda ({params}) {arg_body_prefix}"
        + f"(common-lisp:block __clamp_lambda_block_{id(node)} {body})"
        + f"{arg_body_suffix}))"
        + default_suffix
    )

def codegen_function(node, context : Context):
    child_context = context.child()

    default_symbols = [
        f"__clamp_default_{id(node)}_{index}"
        for index, _ in enumerate(node.args.defaults)
    ]
    kw_default_symbols = [
        f"__clamp_kw_default_{id(node)}_{index}"
        for index, default in enumerate(node.args.kw_defaults)
        if default is not None
    ]
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(node.args, child_context, default_symbols, kw_default_symbols, node.name)
    signature_options = codegen_callable_signature_options(node.args, default_symbols, kw_default_symbols)

    default_binding_specs = [
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(default_symbols, node.args.defaults)
    ]
    default_binding_specs.extend(
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(kw_default_symbols, (d for d in node.args.kw_defaults if d is not None))
    )
    default_bindings = ""
    default_suffix = ""
    if default_binding_specs:
        default_bindings = "(common-lisp:let (" + " ".join(default_binding_specs) + ") "
        default_suffix = ")"
    function_symbol = map_name(node.name)
    setter = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(node.name, child_context)} {function_symbol} "
        if context.top_level_stmt
        else f"(common-lisp:setf {function_symbol} "
    )
    is_generator = node_contains_yield(node)
    local_names, local_bindings = codegen_function_local_bindings(node)
    forced_global_names = frozenset(
        name for name in context.force_global_names
        if name not in local_names and name not in function_parameter_names(node.args)
    )
    body_context = replace(
        child_context,
        block_name=node.name,
        in_generator_function=is_generator,
        mutation_context=bool(local_names),
        force_global_names=forced_global_names,
        local_names=frozenset(function_parameter_names(node.args) | set(local_names)),
    )
    bod = codegen_block(node.body, body_context)
    body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    if local_bindings:
        body = f"(common-lisp:let ({local_bindings}) {body})"
    if is_generator:
        call_body = (
            f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-GENERATOR {lisp_string(node.name)} "
            f"(common-lisp:lambda () (common-lisp:block {node.name} {body})))"
        )
    else:
        call_body = f"(common-lisp:block {node.name} {body})"
    callable_value = (
        f"{default_bindings}"
        f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string(node.name)}{signature_options} :FN "
        f"(common-lisp:lambda ({params}) {arg_body_prefix}{call_body}{arg_body_suffix}))"
        f"{default_suffix}"
    )
    callable_value = codegen_with_function_annotations(callable_value, node, context)
    definition = setter + callable_value + ")"
    rebind = decorated_function_rebind(node, context)
    if rebind:
        return f"(common-lisp:progn {definition} {rebind})\n"
    return definition + "\n"

def codegen_async_function(node, context: Context):
    child_context = context.child()
    is_async_generator = node_contains_yield(node)

    default_symbols = [
        f"__clamp_default_{id(node)}_{index}"
        for index, _ in enumerate(node.args.defaults)
    ]
    kw_default_symbols = [
        f"__clamp_kw_default_{id(node)}_{index}"
        for index, default in enumerate(node.args.kw_defaults)
        if default is not None
    ]
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(node.args, child_context, default_symbols, kw_default_symbols, node.name)
    signature_options = codegen_callable_signature_options(node.args, default_symbols, kw_default_symbols)

    default_bindings = ""
    default_binding_specs = [
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(default_symbols, node.args.defaults)
    ]
    default_binding_specs.extend(
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(kw_default_symbols, (d for d in node.args.kw_defaults if d is not None))
    )
    if default_binding_specs:
        default_bindings = "(common-lisp:let (" + " ".join(default_binding_specs) + ") "
    function_symbol = map_name(node.name)
    setter = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(node.name, child_context)} {function_symbol} "
        if context.top_level_stmt
        else f"(common-lisp:setf {function_symbol} "
    )
    maker = "MAKE-PY-ASYNC-GENERATOR" if is_async_generator else "MAKE-PY-COROUTINE"
    hed = (
        setter
        + f"{default_bindings}"
        + f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string(node.name)}{signature_options} :COROUTINE-FUNCTION COMMON-LISP:T :ASYNC-GENERATOR-FUNCTION {'COMMON-LISP:T' if is_async_generator else 'COMMON-LISP:NIL'} :FN "
        + f"(common-lisp:lambda ({params}) {arg_body_prefix}"
        + f"(|CLAMP.__CLAMP_INTERNALS__|:{maker} {lisp_string(node.name)} "
        + f"(common-lisp:lambda () (common-lisp:block {node.name} "
    )

    local_names, local_bindings = codegen_function_local_bindings(node)
    forced_global_names = frozenset(
        name for name in context.force_global_names
        if name not in local_names and name not in function_parameter_names(node.args)
    )
    body_context = replace(
        child_context,
        block_name=node.name,
        in_async_function=True,
        mutation_context=bool(local_names),
        force_global_names=forced_global_names,
        local_names=frozenset(function_parameter_names(node.args) | set(local_names)),
    )
    bod = codegen_block(node.body, body_context)
    body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    if local_bindings:
        body = f"(common-lisp:let ({local_bindings}) {body})"

    definition = hed + body + "))))))" + arg_body_suffix + ( ")" if default_bindings else "")
    rebind = decorated_function_rebind(node, context)
    if rebind:
        return f"(common-lisp:progn {definition} {rebind})\n"
    return definition + "\n"

def codegen_function_lambda(node, context: Context, async_function: bool = False, class_global_names: frozenset[str] = frozenset()):
    child_context = context.child()
    default_symbols = [
        f"__clamp_default_{id(node)}_{index}"
        for index, _ in enumerate(node.args.defaults)
    ]
    kw_default_symbols = [
        f"__clamp_kw_default_{id(node)}_{index}"
        for index, default in enumerate(node.args.kw_defaults)
        if default is not None
    ]
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(node.args, child_context, default_symbols, kw_default_symbols, node.name)
    signature_options = codegen_callable_signature_options(node.args, default_symbols, kw_default_symbols)
    default_bindings = ""
    default_suffix = ""
    default_binding_specs = [
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(default_symbols, node.args.defaults)
    ]
    default_binding_specs.extend(
        f"({symbol} {codegen(default, child_context)})"
        for symbol, default in zip(kw_default_symbols, (d for d in node.args.kw_defaults if d is not None))
    )
    if default_binding_specs:
        default_bindings = "(common-lisp:let (" + " ".join(default_binding_specs) + ") "
        default_suffix = ")"
    local_names, local_bindings = codegen_function_local_bindings(node)
    forced_global_names = frozenset(
        name for name in class_global_names
        if name not in local_names and name not in function_parameter_names(node.args)
    )
    if async_function:
        is_async_generator = node_contains_yield(node)
        maker = "MAKE-PY-ASYNC-GENERATOR" if is_async_generator else "MAKE-PY-COROUTINE"
        body_context = replace(
            child_context,
            block_name=node.name,
            in_async_function=True,
            mutation_context=bool(local_names),
            force_global_names=forced_global_names,
            local_names=frozenset(function_parameter_names(node.args) | set(local_names)),
        )
        bod = codegen_block(node.body, body_context)
        body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
        if local_bindings:
            body = f"(common-lisp:let ({local_bindings}) {body})"
        expr = (
            f"{default_bindings}(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string(node.name)}{signature_options} :COROUTINE-FUNCTION COMMON-LISP:T :ASYNC-GENERATOR-FUNCTION {'COMMON-LISP:T' if is_async_generator else 'COMMON-LISP:NIL'} :FN "
            f"(common-lisp:lambda ({params}) {arg_body_prefix}"
            f"(|CLAMP.__CLAMP_INTERNALS__|:{maker} {lisp_string(node.name)} "
            f"(common-lisp:lambda () (common-lisp:block {node.name} {body}))))"
            f"{arg_body_suffix})"
            + (")" if default_bindings else "")
        )
    else:
        is_generator = node_contains_yield(node)
        body_context = replace(
            child_context,
            block_name=node.name,
            in_generator_function=is_generator,
            mutation_context=bool(local_names),
            force_global_names=forced_global_names,
            local_names=frozenset(function_parameter_names(node.args) | set(local_names)),
        )
        bod = codegen_block(node.body, body_context)
        body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
        if local_bindings:
            body = f"(common-lisp:let ({local_bindings}) {body})"
        if is_generator:
            call_body = (
                f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-GENERATOR {lisp_string(node.name)} "
                f"(common-lisp:lambda () (common-lisp:block {node.name} {body})))"
            )
        else:
            call_body = f"(common-lisp:block {node.name} {body})"
        expr = (
            f"{default_bindings}"
            f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string(node.name)}{signature_options} :FN "
            f"(common-lisp:lambda ({params}) {arg_body_prefix}{call_body}{arg_body_suffix}))"
            f"{default_suffix}"
        )
    return codegen_with_function_annotations(expr, node, context)


def codegen_class_annotation_value(annotation, context: Context) -> str:
    if context.future_annotations:
        return lisp_string(ast.unparse(annotation))
    if isinstance(annotation, ast.Constant) and isinstance(annotation.value, str):
        return lisp_string(annotation.value)
    return codegen(annotation, context.child())


def codegen_function_annotation_pairs(node, context: Context) -> list[tuple[str, str]]:
    pairs = []
    for arg in [*node.args.posonlyargs, *node.args.args, *node.args.kwonlyargs]:
        if arg.annotation is not None:
            pairs.append((arg.arg, codegen_class_annotation_value(arg.annotation, context)))
    if node.args.vararg and node.args.vararg.annotation is not None:
        pairs.append((node.args.vararg.arg, codegen_class_annotation_value(node.args.vararg.annotation, context)))
    if node.args.kwarg and node.args.kwarg.annotation is not None:
        pairs.append((node.args.kwarg.arg, codegen_class_annotation_value(node.args.kwarg.annotation, context)))
    if node.returns is not None:
        pairs.append(("return", codegen_class_annotation_value(node.returns, context)))
    return pairs

def codegen_with_function_annotations(expr: str, node, context: Context) -> str:
    pairs = codegen_function_annotation_pairs(node, context.child())
    docstring = ast.get_docstring(node, clean=False)
    if not pairs and docstring is None:
        return expr
    callable_symbol = f"__clamp_callable_{id(node)}"
    forms = []
    if pairs:
        annotation_pairs = " ".join(
            f"(common-lisp:list {lisp_string(name)} {value})"
            for name, value in pairs
        )
        forms.append(
            f"(common-lisp:setf (common-lisp:gethash \"__annotations__\" "
            f"(|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-ATTRS {callable_symbol})) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS {annotation_pairs}))"
        )
    if docstring is not None:
        forms.append(
            f"(common-lisp:setf (common-lisp:gethash \"__doc__\" "
            f"(|CLAMP.__CLAMP_INTERNALS__|::PY-CALLABLE-ATTRS {callable_symbol})) "
            f"{lisp_string(docstring)})"
        )
    return (
        f"(common-lisp:let (({callable_symbol} {expr})) "
        + " ".join(forms)
        + f" {callable_symbol})"
    )


def class_body_binding_names(stmts) -> list[str]:
    names: list[str] = []

    def add(name: str):
        if name not in names:
            names.append(name)

    for stmt in stmts:
        if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            add(stmt.name)
        elif isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                for name in target_binding_names(target):
                    add(name)
        elif isinstance(stmt, ast.AnnAssign):
            for name in target_binding_names(stmt.target):
                add(name)
        elif isinstance(stmt, ast.If):
            for name in class_body_binding_names(stmt.body):
                add(name)
            for name in class_body_binding_names(stmt.orelse):
                add(name)
    return names



def codegen_class_body_statement(stmt, type_symbol: str, child_context: Context, class_local_names: list[str]):
    forms = []
    annotations = []
    if isinstance(stmt, ast.Pass):
        return forms, annotations
    if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
        return forms, annotations
    if isinstance(stmt, ast.Expr):
        forms.append(codegen(stmt, child_context))
    elif isinstance(stmt, ast.ClassDef):
        value_symbol = f"__clamp_class_value_{id(stmt)}"
        target_symbol = map_name(stmt.name)
        forms.append(
            f"(common-lisp:let (({value_symbol} {codegen_class(stmt, child_context)})) "
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.name)}) {value_symbol}) "
            f"(common-lisp:setf {target_symbol} {value_symbol}))"
        )
    elif isinstance(stmt, ast.FunctionDef):
        method_value = apply_function_decorators(
            stmt, child_context, codegen_function_lambda(stmt, child_context, async_function=False, class_global_names=frozenset(class_local_names))
        )
        target_symbol = map_name(stmt.name)
        value_symbol = f"__clamp_class_method_{id(stmt)}"
        forms.append(
            f"(common-lisp:let (({value_symbol} {method_value})) "
            f"(|CLAMP.__CLAMP_INTERNALS__|::PY-SET-CALLABLE-OWNER {value_symbol} {type_symbol}) "
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.name)}) {value_symbol}) "
            f"(common-lisp:setf {target_symbol} {value_symbol}))"
        )
    elif isinstance(stmt, ast.AsyncFunctionDef):
        method_value = apply_function_decorators(
            stmt, child_context, codegen_function_lambda(stmt, child_context, async_function=True, class_global_names=frozenset(class_local_names))
        )
        target_symbol = map_name(stmt.name)
        value_symbol = f"__clamp_class_method_{id(stmt)}"
        forms.append(
            f"(common-lisp:let (({value_symbol} {method_value})) "
            f"(|CLAMP.__CLAMP_INTERNALS__|::PY-SET-CALLABLE-OWNER {value_symbol} {type_symbol}) "
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.name)}) {value_symbol}) "
            f"(common-lisp:setf {target_symbol} {value_symbol}))"
        )
    elif isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
        value_symbol = f"__clamp_class_value_{id(stmt)}"
        target_symbol = map_name(stmt.targets[0].id)
        forms.append(
            f"(common-lisp:let (({value_symbol} {codegen(stmt.value, child_context)})) "
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.targets[0].id)}) {value_symbol}) "
            f"(common-lisp:setf {target_symbol} {value_symbol}))"
        )
    elif isinstance(stmt, ast.Assign) and all(isinstance(target, ast.Name) for target in stmt.targets):
        value_symbol = f"__clamp_class_value_{id(stmt)}"
        assignments = []
        for target in stmt.targets:
            target_symbol = map_name(target.id)
            assignments.append(
                f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(target.id)}) {value_symbol}) "
                f"(common-lisp:setf {target_symbol} {value_symbol})"
            )
        forms.append(
            f"(common-lisp:let (({value_symbol} {codegen(stmt.value, child_context)})) "
            f"{' '.join(assignments)})"
        )
    elif isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], (ast.Tuple, ast.List)) and all(isinstance(elt, ast.Name) for elt in stmt.targets[0].elts):
        value_symbol = f"__clamp_class_value_{id(stmt)}"
        items_symbol = f"__clamp_class_items_{id(stmt)}"
        elts = stmt.targets[0].elts
        assignments = []
        for index, elt in enumerate(elts):
            item_symbol = f"__clamp_class_item_{id(stmt)}_{index}"
            target_symbol = map_name(elt.id)
            assignments.append(
                f"(common-lisp:let (({item_symbol} (common-lisp:nth {index} {items_symbol}))) "
                f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(elt.id)}) {item_symbol}) "
                f"(common-lisp:setf {target_symbol} {item_symbol}))"
            )
        forms.append(
            f"(common-lisp:let* (({value_symbol} {codegen(stmt.value, child_context)}) "
            f"({items_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-ITERABLE-TO-LIST {value_symbol}))) "
            f"(common-lisp:unless (common-lisp:= (common-lisp:length {items_symbol}) {len(elts)}) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE-TYPE |CLAMP.__CLAMP_INTERNALS__|:*PY-VALUE-ERROR-TYPE* {lisp_string('not enough values to unpack')})) "
            f"{' '.join(assignments)})"
        )
    elif isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Attribute):
        target = stmt.targets[0]
        if isinstance(target.value, ast.Name):
            receiver = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(target.value.id)})"
        else:
            receiver = codegen(target.value, child_context)
        forms.append(
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR {receiver} {lisp_string(target.attr)}) "
            f"{codegen(stmt.value, child_context)})"
        )
    elif isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name):
        annotations.append((stmt.target.id, codegen_class_annotation_value(stmt.annotation, child_context)))
        if stmt.value is not None:
            value_symbol = f"__clamp_class_value_{id(stmt)}"
            target_symbol = map_name(stmt.target.id)
            forms.append(
                f"(common-lisp:let (({value_symbol} {codegen(stmt.value, child_context)})) "
                f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.target.id)}) {value_symbol}) "
                f"(common-lisp:setf {target_symbol} {value_symbol}))"
            )
    elif isinstance(stmt, ast.If):
        condition = codegen(stmt.test, child_context)
        true_forms = []
        true_annotations = []
        for child_stmt in stmt.body:
            child_forms, child_annotations = codegen_class_body_statement(child_stmt, type_symbol, child_context, class_local_names)
            true_forms.extend(child_forms)
            true_annotations.extend(child_annotations)
        false_forms = []
        false_annotations = []
        for child_stmt in stmt.orelse:
            child_forms, child_annotations = codegen_class_body_statement(child_stmt, type_symbol, child_context, class_local_names)
            false_forms.extend(child_forms)
            false_annotations.extend(child_annotations)
        true_code = " ".join(true_forms) if true_forms else "COMMON-LISP::nil"
        false_code = " ".join(false_forms) if false_forms else "COMMON-LISP::nil"
        forms.append(
            f"(common-lisp:if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {condition}) "
            f"(common-lisp:progn {true_code}) (common-lisp:progn {false_code}))"
        )
        annotations.extend(true_annotations)
        annotations.extend(false_annotations)
    else:
        raise Exception(f"TODO: unsupported class body node {type(stmt)}")
    return forms, annotations

def class_runtime_bases(bases):
    result = []
    for base in bases:
        if isinstance(base, ast.Subscript):
            value = base.value
            if isinstance(value, ast.Name) and value.id == "Generic":
                continue
            if isinstance(value, ast.Attribute) and value.attr == "Generic":
                continue
        result.append(base)
    return result


def class_generic_parameter_nodes(bases):
    parameters = []
    for base in bases:
        if not isinstance(base, ast.Subscript):
            continue
        value = base.value
        is_generic = (isinstance(value, ast.Name) and value.id == "Generic") or (isinstance(value, ast.Attribute) and value.attr == "Generic")
        if not is_generic:
            continue
        slice_node = base.slice
        if isinstance(slice_node, ast.Tuple):
            parameters.extend(slice_node.elts)
        else:
            parameters.append(slice_node)
    return parameters


def codegen_class(node, context: Context):
    child_context = context.child()
    class_symbol = map_name(node.name)
    type_symbol = f"__clamp_class_{id(node)}"
    original_bases_symbol = f"__clamp_class_original_bases_{id(node)}"
    raw_bases_symbol = f"__clamp_class_raw_bases_{id(node)}"
    bases_symbol = f"__clamp_class_bases_{id(node)}"
    runtime_bases = class_runtime_bases(node.bases)
    original_base_values = " ".join(codegen(base, child_context) for base in node.bases)
    runtime_base_values = " ".join(codegen(base, child_context) for base in runtime_bases)
    original_bases_code = f"(common-lisp:list {original_base_values})" if original_base_values else "COMMON-LISP:NIL"
    runtime_bases_code = f"(common-lisp:list {runtime_base_values})" if runtime_base_values else "COMMON-LISP:NIL"
    raw_bases_code = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-RESOLVE-CLASS-BASES {runtime_bases_code})"
    bases_code = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-NORMALIZE-CLASS-BASES {raw_bases_symbol})"
    metaclass_code = "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    class_keyword_pairs = []
    for keyword in node.keywords:
        if keyword.arg is None:
            raise Exception("TODO: class **kwargs are not supported")
        value_code = codegen(keyword.value, child_context)
        if keyword.arg == "metaclass":
            metaclass_code = value_code
        else:
            class_keyword_pairs.append(f"(common-lisp:list {lisp_string(keyword.arg)} {value_code})")
    class_kwargs_code = (
        "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS " + " ".join(class_keyword_pairs) + ")"
        if class_keyword_pairs else
        "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS)"
    )
    forms = []
    annotations = []
    class_local_names = class_body_binding_names(node.body)
    for stmt in node.body:
        stmt_forms, stmt_annotations = codegen_class_body_statement(stmt, type_symbol, child_context, class_local_names)
        forms.extend(stmt_forms)
        annotations.extend(stmt_annotations)
    forms.insert(
        0,
        f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} \"__qualname__\") {lisp_string(node.name)})"
    )
    forms.insert(
        0,
        f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} \"__module__\") {lisp_string(context.module_name)})"
    )
    generic_parameter_nodes = class_generic_parameter_nodes(node.bases)
    if generic_parameter_nodes:
        parameter_values = " ".join(codegen(parameter, child_context) for parameter in generic_parameter_nodes)
        forms.insert(
            0,
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} \"__parameters__\") "
            f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TUPLE {parameter_values}))"
        )
    if annotations:
        annotation_pairs = " ".join(
            f"(common-lisp:list {lisp_string(name)} {value})"
            for name, value in annotations
        )
        forms.insert(
            0,
            f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} \"__annotations__\") "
            f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS {annotation_pairs}))"
        )
    forms_code = " ".join(forms) if forms else "COMMON-LISP::nil"
    if class_local_names:
        local_bindings = " ".join(
            f"({map_name(name)} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)"
            for name in class_local_names
        )
        forms_code = f"(common-lisp:let ({local_bindings}) {forms_code})"
    make_type = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TYPE :TYPE |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE* "
        f":NAME {lisp_string(node.name)} :BASES {bases_symbol} :BASICSIZE 1)"
    )
    class_expr = (
        f"(common-lisp:let* (({original_bases_symbol} {original_bases_code}) "
        f"({raw_bases_symbol} {raw_bases_code}) "
        f"({bases_symbol} {bases_code}) ({type_symbol} {make_type})) "
        f"{forms_code} "
        f"(|CLAMP.__CLAMP_INTERNALS__|::PY-BUILD-CLASS-FROM-TEMPLATE "
        f"{type_symbol} {lisp_string(node.name)} {raw_bases_symbol} {metaclass_code} {class_kwargs_code} {original_bases_symbol}))"
    )
    if node.decorator_list:
        for decorator in reversed(node.decorator_list):
            class_expr = (
                f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE "
                f"{codegen(decorator, child_context)} {class_expr})"
            )
    if context.top_level_stmt:
        return (
            f"(common-lisp:let (({type_symbol} {class_expr})) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {lisp_string(node.name)} {class_symbol} {type_symbol}))"
        )
    return f"(|CLAMP.__builtins__|:ASSIGN ({class_symbol} {class_expr}) {class_symbol})"

def codegen_funcall(node, context : Context):
    child_context = context.child()
    positional_parts = []
    simple_args = []
    expanded_call = False
    for arg in node.args:
        if isinstance(arg, ast.Starred):
            expanded_call = True
            if simple_args:
                positional_parts.append("(common-lisp:list " + " ".join(simple_args) + ")")
                simple_args = []
            positional_parts.append(f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ITERABLE-TO-LIST {codegen(arg.value, child_context)})")
        else:
            simple_args.append(codegen(arg, child_context))
    if simple_args:
        positional_parts.append("(common-lisp:list " + " ".join(simple_args) + ")")
    keyword_args = []
    kwarg_dicts = []
    for keyword in node.keywords:
        if keyword.arg is None:
            expanded_call = True
            kwarg_dicts.append(codegen(keyword.value, child_context))
        else:
            expanded_call = True
            keyword_args.extend([f":{keyword.arg}", codegen(keyword.value, child_context)])
    if expanded_call:
        positional_code = (
            "(common-lisp:append " + " ".join(positional_parts) + ")"
            if positional_parts else
            "COMMON-LISP::nil"
        )
        explicit_kwargs = "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS"
        explicit_kwargs += "".join(
            f" (common-lisp:list {lisp_string(keyword.arg)} {codegen(keyword.value, child_context)})"
            for keyword in node.keywords
            if keyword.arg is not None
        )
        explicit_kwargs += ")"
        kwargs_code = explicit_kwargs
        for kwarg_dict in kwarg_dicts:
            kwargs_code = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-DICT-MERGE {kwargs_code} {kwarg_dict})"
        if isinstance(node.func, ast.Attribute):
            owner = codegen(node.func.value, child_context)
            attr = codegen(node.func.attr, child_context)
            return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR-EXPANDED {owner} {attr} {positional_code} {kwargs_code})"
        target = codegen(node.func, child_context)
        if should_force_builtin_call(node.func, context):
            target = f"|CLAMP.__builtins__|:{node.func.id.upper()}"
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE-EXPANDED {target} {positional_code} {kwargs_code})"

    all_args = [*simple_args, *keyword_args]

    if isinstance(node.func, ast.Attribute):
        owner = codegen(node.func.value, child_context)
        attr = codegen(node.func.attr, child_context)
        args_str = " ".join(all_args)
        return (
            "(|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR "
            f"{owner} {attr}"
            + (f" {args_str}" if args_str else "")
            + ")"
        )

    target = codegen(node.func, child_context)
    args_str = " ".join(all_args)
    # Map builtins that must resolve before USE-PACKAGE takes effect.
    if should_force_builtin_call(node.func, context):
        target = f"|CLAMP.__builtins__|:{node.func.id.upper()}"
    if args_str:
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {target} {args_str})"
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {target})"

def codegen_joinedstr(node, context: Context):
    child_context = context.child()
    parts = []
    for value in node.values:
        if isinstance(value, ast.Constant) and isinstance(value.value, str):
            parts.append(lisp_string(value.value))
        elif isinstance(value, ast.FormattedValue):
            rendered_value = codegen(value.value, child_context)
            if value.conversion == -1:
                converted_value = rendered_value
                default_value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-STR {rendered_value})"
            elif value.conversion == ord("s"):
                converted_value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-STR {rendered_value})"
                default_value = converted_value
            elif value.conversion == ord("r"):
                converted_value = (
                    "(common-lisp:with-output-to-string (stream) "
                    f"(|CLAMP.__CLAMP_INTERNALS__|:PY-REPR {rendered_value} stream))"
                )
                default_value = converted_value
            elif value.conversion == ord("a"):
                converted_value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ASCII {rendered_value})"
                default_value = converted_value
            else:
                raise Exception(f"TODO: unsupported f-string conversion {value.conversion}")
            if value.format_spec is not None:
                format_spec = codegen(value.format_spec, child_context)
                parts.append(f"(|CLAMP.__CLAMP_INTERNALS__|:PY-FORMAT {converted_value} {format_spec})")
            else:
                parts.append(default_value)
        else:
            raise Exception(f"TODO: unsupported f-string part {type(value)}")
    if not parts:
        return '""'
    if len(parts) == 1:
        return parts[0]
    return "(common-lisp:concatenate 'common-lisp:string " + " ".join(parts) + ")"


def codegen_await(node, context: Context):
    if not context.in_async_function:
        raise Exception("'await' outside async function")
    awaited = codegen(node.value, context.child())
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-AWAIT {awaited})"


BUILTIN_CALL_NAMES = {"__import__", "print", "dir", "len", "bool", "globals", "getattr", "setattr", "delattr", "hasattr", "classmethod", "staticmethod", "property", "super", "open", "compile", "exec", "callable", "isinstance", "issubclass", "vars", "repr", "ascii", "str", "format", "type", "id", "iter", "next", "aiter", "anext", "reversed", "min", "max", "sum", "sorted", "list", "tuple", "dict", "set", "frozenset", "memoryview", "bytearray", "abs", "round", "hash", "pow", "divmod", "all", "any", "enumerate", "zip", "filter", "map", "range", "slice", "bin", "oct", "hex", "chr", "ord"}


def should_force_builtin_call(node, context: Context) -> bool:
    return (
        isinstance(node, ast.Name)
        and node.id.lower() in BUILTIN_CALL_NAMES
        and node.id not in context.module_bound_names
        and node.id not in context.local_names
    )


BUILTIN_VALUE_NAMES = {"bool", "int", "float", "complex", "str", "bytes", "object", "type", "list", "tuple", "dict", "set", "frozenset"}


def codegen_name(node, context: Context):
    if isinstance(node.ctx, ast.Load) and node.id in context.force_global_names:
        return f"(common-lisp:symbol-value (common-lisp:quote {map_name(node.id)}))"
    if (
        isinstance(node.ctx, ast.Load)
        and node.id in (BUILTIN_CALL_NAMES | BUILTIN_VALUE_NAMES)
        and node.id not in context.local_names
    ):
        builtin = f"|CLAMP.__builtins__|:{node.id.upper()}"
        if node.id in context.module_bound_names:
            symbol = map_name(node.id)
            return f"(common-lisp:if (common-lisp:boundp (common-lisp:quote {symbol})) {symbol} {builtin})"
        return builtin
    return map_name(node.id)


def codegen_yield(node, context: Context):
    if isinstance(node, ast.YieldFrom):
        if not context.in_generator_function or context.in_async_function:
            raise Exception("'yield from' outside synchronous generator")
        iterator_symbol = f"__clamp_yield_from_iter_{id(node)}"
        item_symbol = f"__clamp_yield_from_item_{id(node)}"
        found_symbol = f"__clamp_yield_from_found_{id(node)}"
        value = codegen(node.value, context.child())
        return (
            f"(common-lisp:let (({iterator_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-ITER {value}))) "
            f"(common-lisp:loop "
            f"(common-lisp:multiple-value-bind ({item_symbol} {found_symbol}) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-NEXT-ITEM {iterator_symbol}) "
            f"(common-lisp:unless {found_symbol} (common-lisp:return |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-GENERATOR-YIELD {item_symbol}))))"
        )
    value = codegen(node.value, context.child()) if node.value else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    if context.in_async_function:
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ASYNC-GENERATOR-YIELD {value})"
    if context.in_generator_function:
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-GENERATOR-YIELD {value})"
    raise Exception("'yield' outside function")


def lisp_string(value: str) -> str:
    # Raw C0 controls can terminate SBCL's string-input reader. Emit those
    # strings structurally while keeping ordinary strings readable.
    if any(ord(ch) < 32 and ch not in "\t\n\r" for ch in value):
        chars = " ".join(f"(common-lisp:code-char {ord(ch)})" for ch in value)
        return f"(common-lisp:coerce (common-lisp:list {chars}) 'common-lisp:string)"
    return '"' + value.replace('\\', '\\\\').replace('"', '\\"') + '"'


def module_uses_future_annotations(stmts) -> bool:
    for stmt in stmts:
        if isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Constant) and isinstance(stmt.value.value, str):
            continue
        if isinstance(stmt, ast.ImportFrom) and stmt.module == "__future__":
            if any(alias.name == "annotations" for alias in stmt.names):
                return True
            continue
        break
    return False


def module_binding_names(stmts) -> list[str]:
    names: list[str] = []

    def add(name: str):
        if name not in names:
            names.append(name)

    def visit_stmt(stmt):
        if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            add(stmt.name)
        elif isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                for name in target_binding_names(target):
                    add(name)
        elif isinstance(stmt, ast.AnnAssign):
            for name in target_binding_names(stmt.target):
                add(name)
        elif isinstance(stmt, (ast.For, ast.AsyncFor)):
            for name in target_binding_names(stmt.target):
                add(name)
            for child in [*stmt.body, *stmt.orelse]:
                visit_stmt(child)
        elif isinstance(stmt, (ast.With, ast.AsyncWith)):
            for item in stmt.items:
                if item.optional_vars:
                    for name in target_binding_names(item.optional_vars):
                        add(name)
            for child in stmt.body:
                visit_stmt(child)
        elif isinstance(stmt, ast.Import):
            for alias in stmt.names:
                add(alias.asname or alias.name.partition('.')[0])
        elif isinstance(stmt, ast.ImportFrom):
            if not any(alias.name == "*" for alias in stmt.names):
                for alias in stmt.names:
                    add(alias.asname or alias.name)
        elif isinstance(stmt, ast.If):
            for child in [*stmt.body, *stmt.orelse]:
                visit_stmt(child)
        elif isinstance(stmt, ast.Try):
            for child in [*stmt.body, *stmt.orelse, *stmt.finalbody]:
                visit_stmt(child)
            for handler in stmt.handlers:
                if handler.name:
                    add(handler.name)
                for child in handler.body:
                    visit_stmt(child)

    for stmt in stmts:
        visit_stmt(stmt)
    return names



def lisp_symbol_name(name: str) -> str:
    if name not in BUILTIN_CASED_NAMES and any(char.isupper() for char in name):
        return name
    return name.upper()

def codegen_shadow_module_bindings(node) -> str:
    names = [name for name in module_binding_names(node.body) if name not in {"__name__"}]
    if not names:
        return ""
    shadow_names = " ".join(lisp_string(lisp_symbol_name(name)) for name in names)
    return f"(common-lisp:shadow '( {shadow_names} ))\n"


def codegen_module(node, context : Context):
    header_code = (
        f"(common-lisp:in-package {lisp_string(context.package_name)})\n"
        f"(common-lisp:use-package \"CLAMP.__builtins__\")\n"
        f"{codegen_shadow_module_bindings(node)}"
    )
    source_code = "COMMON-LISP::nil" if context.source_path is None else lisp_string(context.source_path)
    enter_code = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ENTER-MODULE "
        f"{lisp_string(context.module_name)} {source_code} {lisp_string(context.package_name)})\n"
    )
    name_code = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL \"__name__\" {map_name('__name__')} "
        f"{lisp_string(context.module_name)})\n"
    )
    bound_names = frozenset(module_binding_names(node.body))
    body_context = replace(
        context,
        module_bound_names=bound_names,
        future_annotations=module_uses_future_annotations(node.body),
    )
    body_code = codegen_block(node.body, body_context)
    return (header_code + enter_code + name_code + body_code)

def codegen_return(node, context : Context):
    if not context.block_name:
        raise Exception("Trying to return but not inside a lexical scope.")
    retval = codegen(node.value, context.child())
    return f"(common-lisp:return-from {context.block_name} {retval})"




def codegen_assert(node, context: Context):
    child_context = context.child()
    test = codegen(node.test, child_context)
    message = codegen(node.msg, child_context) if node.msg else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    return (
        f"(common-lisp:unless (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {test}) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE "
        f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-EXCEPTION "
        f"|CLAMP.__CLAMP_INTERNALS__|:*PY-ASSERTION-ERROR-TYPE* {message})))"
    )
def codegen_raise(node, context: Context):
    if node.exc is None:
        if context.current_exception_symbol is None:
            raise Exception("bare raise outside exception handler")
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE {context.current_exception_symbol})"
    exception = codegen(node.exc, context.child())
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE {exception})"


def codegen_try_without_finally(node, context: Context):
    child_context = context.child()
    block_context = context if context.top_level_stmt else child_context
    condition_symbol = f"__clamp_try_condition_{id(node)}"
    exception_symbol = f"__clamp_try_exception_{id(node)}"
    normal_body = [*node.body, *node.orelse]
    body = codegen_block(normal_body, block_context) or "COMMON-LISP::nil"
    if not node.handlers:
        return f"(common-lisp:progn {body})"

    clauses = []
    bare_seen = False
    handler_context = replace(block_context, current_exception_symbol=exception_symbol)
    for handler in node.handlers:
        handler_body = codegen_block(handler.body, handler_context) or "COMMON-LISP::nil"
        if handler.name:
            handler_body = f"(common-lisp:let (({map_name(handler.name)} {exception_symbol})) {handler_body})"
        if handler.type is None:
            bare_seen = True
            clauses.append(f"(common-lisp:t {handler_body})")
        else:
            if bare_seen:
                raise Exception("default 'except:' must be last")
            handler_type = codegen(handler.type, child_context)
            clauses.append(
                f"((|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P "
                f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ISINSTANCE {exception_symbol} {handler_type})) "
                f"{handler_body})"
            )
    clauses.append(f"(common-lisp:t (common-lisp:error {condition_symbol}))")
    clause_code = " ".join(clauses)
    return (
        f"(common-lisp:handler-case (common-lisp:progn {body}) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION ({condition_symbol}) "
        f"(common-lisp:let (({exception_symbol} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION-VALUE {condition_symbol}))) "
        f"(common-lisp:cond {clause_code}))) "
        f"(common-lisp:unbound-variable ({condition_symbol}) "
        f"(common-lisp:let (({exception_symbol} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-LISP-ERROR-TO-EXCEPTION {condition_symbol}))) "
        f"(common-lisp:cond {clause_code}))))"
    )


def codegen_try(node, context: Context):
    if node.finalbody:
        try_body = codegen_try_without_finally(node, context)
        final_body = codegen_block(node.finalbody, context.child()) or "COMMON-LISP::nil"
        return f"(common-lisp:unwind-protect {try_body} (common-lisp:progn {final_body}))"
    return codegen_try_without_finally(node, context)


def codegen_binary_operator(node, context : Context):
    child_context = context.child()
    op = codegen(node.op, child_context)
    lhs = codegen(node.left, child_context)
    rhs = codegen(node.right, child_context)
    return f"({op} {lhs} {rhs})"


def codegen_compare(node, context: Context):
    child_context = context.child()
    operands = [node.left, *node.comparators]
    operand_codes = [codegen(operand, child_context) for operand in operands]
    op_codes = [codegen(op, child_context) for op in node.ops]

    if len(op_codes) == 1:
        return f"({op_codes[0]} {operand_codes[0]} {operand_codes[1]})"

    base = f"__clamp_compare_{id(node)}"

    def build(index: int, left_symbol: str) -> str:
        right_symbol = f"{base}_right_{index}"
        comparison_symbol = f"{base}_comparison_{index}"
        comparison = f"({op_codes[index]} {left_symbol} {right_symbol})"
        if index == len(op_codes) - 1:
            success = comparison_symbol
        else:
            success = build(index + 1, right_symbol)
        return (
            f"(common-lisp:let (({right_symbol} {operand_codes[index + 1]})) "
            f"(common-lisp:let (({comparison_symbol} {comparison})) "
            f"(common-lisp:if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {comparison_symbol}) "
            f"{success} |CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*)))"
        )

    left_symbol = f"{base}_left"
    return f"(common-lisp:let (({left_symbol} {operand_codes[0]})) {build(0, left_symbol)})"


def codegen_comprehension_loop(node, context: Context, emit_body, first_iterator_symbol=None):
    if not node.generators:
        return emit_body(context.child())
    if any(generator.is_async for generator in node.generators) and not context.in_async_function:
        raise Exception("asynchronous comprehension outside async function")

    base_context = context.child()

    def context_without_targets(current_context: Context, target) -> Context:
        target_names = set(target_binding_names(target))
        if not target_names:
            return current_context
        return replace(
            current_context,
            force_global_names=frozenset(
                name for name in current_context.force_global_names
                if name not in target_names
            ),
        )

    def truthy_filters(generator, active_context: Context):
        if not generator.ifs:
            return None
        checks = [
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {codegen(condition, active_context)})"
            for condition in generator.ifs
        ]
        return "(common-lisp:and " + " ".join(checks) + ")"

    def build_generator(index: int, active_context: Context) -> str:
        if index == len(node.generators):
            return emit_body(active_context)
        generator = node.generators[index]
        gen_id = f"{id(node)}_{index}"
        iterator_symbol = f"__clamp_comp_iterator_{gen_id}"
        item_symbol = f"__clamp_comp_item_{gen_id}"
        found_symbol = f"__clamp_comp_found_{gen_id}"
        loop_context = context_without_targets(active_context, generator.target)
        target_bindings = codegen_target_bindings(generator.target, active_context)
        target_store = codegen_store_target(generator.target, item_symbol, loop_context)
        body = build_generator(index + 1, loop_context)
        filter_code = truthy_filters(generator, loop_context)
        if filter_code:
            body = f"(common-lisp:when {filter_code} {body})"
        iter_fn = "PY-AITER" if generator.is_async else "PY-ITER"
        next_fn = "PY-ANEXT-ITEM" if generator.is_async else "PY-NEXT-ITEM"
        if index == 0 and first_iterator_symbol is not None:
            iterator_let = f"(({iterator_symbol} {first_iterator_symbol}))"
        else:
            iterable = codegen(generator.iter, active_context)
            iterator_let = f"(({iterator_symbol} (|CLAMP.__CLAMP_INTERNALS__|:{iter_fn} {iterable})))"
        return (
            f"(common-lisp:let ({target_bindings}) "
            f"(common-lisp:let {iterator_let} "
            f"(common-lisp:loop "
            f"(common-lisp:multiple-value-bind ({item_symbol} {found_symbol}) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:{next_fn} {iterator_symbol}) "
            f"(common-lisp:unless {found_symbol} (common-lisp:return)) "
            f"{target_store} "
            f"{body}))))"
        )

    return build_generator(0, base_context)


def codegen_generatorexp(node, context: Context):
    result_symbol = f"__clamp_genexp_result_{id(node)}"
    first_iterator_symbol = f"__clamp_genexp_first_iter_{id(node)}"
    child_context = context.child()

    def emit_body(active_context: Context):
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-GENERATOR-YIELD "
            f"{codegen(node.elt, active_context)})"
        )

    first_generator = node.generators[0]
    first_iter_fn = "PY-AITER" if first_generator.is_async else "PY-ITER"
    first_iterable = codegen(first_generator.iter, context)
    loop = codegen_comprehension_loop(
        node, context, emit_body, first_iterator_symbol=first_iterator_symbol
    )
    return (
        f"(common-lisp:let (({first_iterator_symbol} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:{first_iter_fn} {first_iterable}))) "
        f"(common-lisp:let (({result_symbol} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-GENERATOR \"<genexpr>\" "
        f"(common-lisp:lambda () {loop})))) "
        f"{result_symbol}))"
    )


def codegen_listcomp(node, context: Context):
    result_symbol = f"__clamp_listcomp_result_{id(node)}"
    child_context = context.child()

    def emit_body(active_context: Context):
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-APPEND {result_symbol} "
            f"{codegen(node.elt, active_context)})"
        )

    return (
        f"(common-lisp:let (({result_symbol} (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST))) "
        f"{codegen_comprehension_loop(node, context, emit_body)} {result_symbol})"
    )




def codegen_setcomp(node, context: Context):
    result_symbol = f"__clamp_setcomp_result_{id(node)}"
    child_context = context.child()

    def emit_body(active_context: Context):
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR {result_symbol} \"add\" "
            f"{codegen(node.elt, active_context)})"
        )

    return (
        f"(common-lisp:let (({result_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE |CLAMP.__builtins__|:SET))) "
        f"{codegen_comprehension_loop(node, context, emit_body)} {result_symbol})"
    )
def codegen_dictcomp(node, context: Context):
    result_symbol = f"__clamp_dictcomp_result_{id(node)}"
    child_context = context.child()

    def emit_body(active_context: Context):
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SETITEM {result_symbol} "
            f"{codegen(node.key, active_context)} {codegen(node.value, active_context)})"
        )

    return (
        f"(common-lisp:let (({result_symbol} (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS))) "
        f"{codegen_comprehension_loop(node, context, emit_body)} {result_symbol})"
    )

def codegen_dict(node, context: Context):
    child_context = context.child()
    result_symbol = f"__clamp_dict_result_{id(node)}"
    forms = []
    for key, value in zip(node.keys, node.values):
        if key is None:
            forms.append(f"(|CLAMP.__CLAMP_INTERNALS__|:PY-DICT-MERGE {result_symbol} {codegen(value, child_context)})")
        else:
            forms.append(
                f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SETITEM {result_symbol} "
                f"{codegen(key, child_context)} {codegen(value, child_context)})"
            )
    body = " ".join(forms)
    return (
        f"(common-lisp:let (({result_symbol} (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS))) "
        f"{body} {result_symbol})"
    )


def codegen_bool_operator(node, context: Context):
    child_context = context.child()
    op = codegen(node.op, child_context)
    values = " ".join(codegen(v, child_context) for v in node.values)
    return f"({op} {values})"


def codegen_subscript_store(node, value_code: str, context: Context):
    child_context = context.child()
    target = codegen(node.value, child_context)
    index = codegen(node.slice, child_context)
    return (
        "(|CLAMP.__CLAMP_INTERNALS__|:PY-SETITEM "
        f"{target} {index} {value_code})"
    )

def codegen_attribute_store(node, value_code: str, context: Context):
    child_context = context.child()
    target = codegen(node.value, child_context)
    attr = codegen(node.attr, child_context)
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SETATTR {target} {attr} {value_code})"


def codegen_delete(node, context: Context):
    if len(node.targets) != 1:
        return "(common-lisp:progn " + " ".join(
            codegen_delete(ast.Delete(targets=[target]), context)
            for target in node.targets
        ) + ")"
    target = node.targets[0]
    child_context = context.child()
    if isinstance(target, ast.Name):
        symbol = codegen(target, child_context)
        if context.top_level_stmt:
            return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-DEL-GLOBAL {lisp_string(target.id)} (common-lisp:quote {symbol}))"
        return f"(common-lisp:makunbound (common-lisp:quote {symbol}))"
    if isinstance(target, ast.Subscript):
        obj = codegen(target.value, child_context)
        index = codegen(target.slice, child_context)
        return (
            "(|CLAMP.__CLAMP_INTERNALS__|:PY-DELITEM "
            f"{obj} {index})"
        )
    if isinstance(target, ast.Attribute):
        obj = codegen(target.value, child_context)
        attr = codegen(target.attr, child_context)
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-DELATTR {obj} {attr})"
    raise Exception("TODO: unsupported delete target")

def codegen_slice(node, context: Context):
    child_context = context.child()
    lower = codegen(node.lower, child_context)
    upper = codegen(node.upper, child_context)
    step = codegen(node.step, child_context)
    return f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-SLICE {lower} {upper} {step})"


def codegen_augassign(node, context: Context):
    child_context = context.child()
    rhs = codegen(node.value, child_context)
    op = codegen(node.op, child_context)
    if isinstance(node.op, ast.Add):
        op = "|CLAMP.__CLAMP_INTERNALS__|:PY-IADD"
    elif isinstance(node.op, ast.Mult):
        op = "|CLAMP.__CLAMP_INTERNALS__|:PY-IMUL"

    if isinstance(node.target, ast.Name):
        target = codegen(node.target, child_context)
        value_code = f"({op} {target} {rhs})"
        if context.top_level_stmt:
            return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(node.target.id, child_context)} {target} {value_code})"
        return f"(common-lisp:setf {target} {value_code})"

    if isinstance(node.target, ast.Subscript):
        target = codegen(node.target.value, child_context)
        index = codegen(node.target.slice, child_context)
        current = (
            "(|CLAMP.__CLAMP_INTERNALS__|:PY-GETITEM "
            f"{target} {index})"
        )
        value_code = f"({op} {current} {rhs})"
        return (
            "(|CLAMP.__CLAMP_INTERNALS__|:PY-SETITEM "
            f"{target} {index} {value_code})"
        )

    if isinstance(node.target, ast.Attribute):
        current = codegen(node.target, child_context)
        value_code = f"({op} {current} {rhs})"
        return codegen_attribute_store(node.target, value_code, child_context)

    raise Exception("TODO: unsupported augmented assignment target")


def codegen_unary_operator(node, context: Context):
    child_context = context.child()
    op = codegen(node.op, child_context)
    operand = codegen(node.operand, child_context)
    return f"({op} {operand})"



def codegen_async_for(node, context: Context):
    if not context.in_async_function:
        raise Exception("'async for' outside async function")

    child_context = context.child()
    loop_id = id(node)
    iterator_symbol = f"__clamp_async_for_iterator_{loop_id}"
    item_symbol = f"__clamp_async_for_item_{loop_id}"
    found_symbol = f"__clamp_async_for_found_{loop_id}"
    loop_block_name = f"__clamp_async_loop_{loop_id}"
    loop_continue_name = f"__clamp_async_loop_continue_{loop_id}"
    loop_broke_name = f"__clamp_async_loop_broke_{loop_id}"
    target = codegen(node.target, child_context) if isinstance(node.target, ast.Name) else None
    target_bindings = "" if context.mutation_context else codegen_target_bindings(node.target, child_context)
    target_store = codegen_store_target(node.target, item_symbol, child_context)
    iterable = codegen(node.iter, child_context)
    body_context = replace(
        child_context,
        mutation_context=True,
        loop_block_name=loop_block_name,
        loop_continue_name=loop_continue_name,
        loop_broke_name=loop_broke_name,
    )
    body = codegen_block(node.body, body_context) or "COMMON-LISP::nil"
    loop_code = (
        f"(common-lisp:let (({loop_broke_name} COMMON-LISP::nil) "
        f"{target_bindings}) "
        f"(common-lisp:let (({iterator_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-AITER {iterable}))) "
        f"(common-lisp:block {loop_block_name} "
        f"(common-lisp:loop "
        f"(common-lisp:multiple-value-bind ({item_symbol} {found_symbol}) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ANEXT-ITEM {iterator_symbol}) "
        f"(common-lisp:unless {found_symbol} (common-lisp:return)) "
        f"{target_store} "
        f"(common-lisp:block {loop_continue_name} "
        f"(common-lisp:progn {body}))))))"
    )
    if node.orelse:
        else_code = codegen_block(node.orelse, child_context)
        loop_code += (
            f" (common-lisp:unless {loop_broke_name} "
            f"(common-lisp:progn {else_code} ))"
        )
    return loop_code + ")"



def codegen_with(node, context: Context):
    if len(node.items) > 1:
        nested = ast.With(
            items=node.items[1:],
            body=node.body,
            type_comment=getattr(node, "type_comment", None),
        )
        ast.copy_location(nested, node)
        outer = ast.With(
            items=[node.items[0]],
            body=[nested],
            type_comment=getattr(node, "type_comment", None),
        )
        ast.copy_location(outer, node)
        return codegen_with(outer, context)
    item = node.items[0]

    child_context = context.child()
    with_id = id(node)
    manager_symbol = f"__clamp_with_manager_{with_id}"
    exit_symbol = f"__clamp_with_exit_{with_id}"
    value_symbol = f"__clamp_with_value_{with_id}"
    condition_symbol = f"__clamp_with_condition_{with_id}"
    exception_symbol = f"__clamp_with_exception_{with_id}"
    handled_symbol = f"__clamp_with_handled_{with_id}"
    manager = codegen(item.context_expr, child_context)
    body = codegen_block(node.body, child_context) or "COMMON-LISP::nil"
    if item.optional_vars:
        target_bindings = codegen_target_bindings(item.optional_vars, child_context)
        target_store = codegen_store_target(item.optional_vars, value_symbol, child_context)
        if target_bindings and not context.mutation_context:
            body = f"(common-lisp:let ({target_bindings}) {target_store} {body})"
        else:
            body = f"(common-lisp:progn {target_store} {body})"
    none = "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    exit_call = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {exit_symbol}"
    return (
        f"(common-lisp:let* (({manager_symbol} {manager}) "
        f"({exit_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-BIND-CONTEXT-EXIT-CALLABLE "
        f"{manager_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR {manager_symbol} \"__exit__\"))) "
        f"({value_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR {manager_symbol} \"__enter__\"))) "
        f"(common-lisp:handler-case "
        f"(common-lisp:prog1 (common-lisp:progn {body}) "
        f"{exit_call} {none} {none} {none})) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION ({condition_symbol}) "
        f"(common-lisp:let* (({exception_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION-VALUE {condition_symbol})) "
        f"({handled_symbol} {exit_call} (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-OF {exception_symbol}) {exception_symbol} {condition_symbol}))) "
        f"(common-lisp:if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {handled_symbol}) "
        f"{none} (common-lisp:error {condition_symbol})))) "
        f"(common-lisp:error ({condition_symbol}) "
        f"(common-lisp:let (({handled_symbol} {exit_call} {none} {condition_symbol} {condition_symbol}))) "
        f"(common-lisp:if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {handled_symbol}) "
        f"{none} (common-lisp:error {condition_symbol}))))))"
    )


def codegen_async_with(node, context: Context):
    if not context.in_async_function:
        raise Exception("'async with' outside async function")
    if len(node.items) > 1:
        nested = ast.AsyncWith(
            items=node.items[1:],
            body=node.body,
            type_comment=getattr(node, "type_comment", None),
        )
        ast.copy_location(nested, node)
        outer = ast.AsyncWith(
            items=[node.items[0]],
            body=[nested],
            type_comment=getattr(node, "type_comment", None),
        )
        ast.copy_location(outer, node)
        return codegen_async_with(outer, context)
    item = node.items[0]

    child_context = context.child()
    with_id = id(node)
    manager_symbol = f"__clamp_async_with_manager_{with_id}"
    exit_symbol = f"__clamp_async_with_exit_{with_id}"
    value_symbol = f"__clamp_async_with_value_{with_id}"
    condition_symbol = f"__clamp_async_with_condition_{with_id}"
    exception_symbol = f"__clamp_async_with_exception_{with_id}"
    handled_symbol = f"__clamp_async_with_handled_{with_id}"
    manager = codegen(item.context_expr, child_context)
    body = codegen_block(node.body, child_context) or "COMMON-LISP::nil"
    if item.optional_vars:
        target_bindings = codegen_target_bindings(item.optional_vars, child_context)
        target_store = codegen_store_target(item.optional_vars, value_symbol, child_context)
        if target_bindings and not context.mutation_context:
            body = f"(common-lisp:let ({target_bindings}) {target_store} {body})"
        else:
            body = f"(common-lisp:progn {target_store} {body})"
    none = "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    exit_call = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-AWAIT "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {exit_symbol}"
    )
    return (
        f"(common-lisp:let* (({manager_symbol} {manager}) "
        f"({exit_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-BIND-CONTEXT-EXIT-CALLABLE "
        f"{manager_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR {manager_symbol} \"__aexit__\"))) "
        f"({value_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-AWAIT "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-CALL-ATTR {manager_symbol} \"__aenter__\")))) "
        f"(common-lisp:handler-case "
        f"(common-lisp:prog1 (common-lisp:progn {body}) "
        f"{exit_call} {none} {none} {none}))) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION ({condition_symbol}) "
        f"(common-lisp:let* (({exception_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION-VALUE {condition_symbol})) "
        f"({handled_symbol} {exit_call} (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-OF {exception_symbol}) {exception_symbol} {condition_symbol})))) "
        f"(common-lisp:if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {handled_symbol}) "
        f"{none} (common-lisp:error {condition_symbol})))) "
        f"(common-lisp:error ({condition_symbol}) "
        f"(common-lisp:let (({handled_symbol} {exit_call} {none} {condition_symbol} {condition_symbol})))) "
        f"(common-lisp:if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {handled_symbol}) "
        f"{none} (common-lisp:error {condition_symbol}))))))"
    )


def codegen_for(node, context: Context):
    child_context = context.child()
    loop_id = id(node)
    iterator_symbol = f"__clamp_for_iterator_{loop_id}"
    item_symbol = f"__clamp_for_item_{loop_id}"
    found_symbol = f"__clamp_for_found_{loop_id}"
    loop_block_name = f"__clamp_loop_{loop_id}"
    loop_continue_name = f"__clamp_loop_continue_{loop_id}"
    loop_broke_name = f"__clamp_loop_broke_{loop_id}"
    target = codegen(node.target, child_context) if isinstance(node.target, ast.Name) else None
    target_bindings = "" if context.mutation_context else codegen_target_bindings(node.target, child_context)
    target_store = codegen_store_target(node.target, item_symbol, child_context)
    iterable = codegen(node.iter, child_context)
    body_context = replace(
        child_context,
        mutation_context=True,
        loop_block_name=loop_block_name,
        loop_continue_name=loop_continue_name,
        loop_broke_name=loop_broke_name,
    )
    body = codegen_block(node.body, body_context) or "COMMON-LISP::nil"
    loop_code = (
        f"(common-lisp:let (({loop_broke_name} COMMON-LISP::nil) "
        f"{target_bindings}) "
        f"(common-lisp:let (({iterator_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-ITER {iterable}))) "
        f"(common-lisp:block {loop_block_name} "
        f"(common-lisp:loop "
        f"(common-lisp:multiple-value-bind ({item_symbol} {found_symbol}) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-NEXT-ITEM {iterator_symbol}) "
        f"(common-lisp:unless {found_symbol} (common-lisp:return)) "
        f"{target_store} "
        f"(common-lisp:block {loop_continue_name} "
        f"(common-lisp:progn {body}))))))"
    )
    if node.orelse:
        else_code = codegen_block(node.orelse, child_context)
        loop_code += (
            f" (common-lisp:unless {loop_broke_name} "
            f"(common-lisp:progn {else_code} ))"
        )
    return loop_code + ")"


def codegen_while(node, context: Context):
    child_context = context.child()
    loop_id = id(node)
    loop_block_name = f"__clamp_loop_{loop_id}"
    loop_continue_name = f"__clamp_loop_continue_{loop_id}"
    loop_broke_name = f"__clamp_loop_broke_{loop_id}"
    body_context = replace(
        child_context,
        mutation_context=True,
        loop_block_name=loop_block_name,
        loop_continue_name=loop_continue_name,
        loop_broke_name=loop_broke_name,
    )
    conditional = codegen(node.test, child_context)
    body = codegen_block(node.body, body_context) or "COMMON-LISP::nil"
    loop_code = (
        f"(common-lisp:let (({loop_broke_name} COMMON-LISP::nil)) "
        f"(common-lisp:block {loop_block_name} "
        "(common-lisp:loop "
        f"while (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {conditional}) "
        f"do (common-lisp:block {loop_continue_name} "
        f"(common-lisp:progn {body}))))"
    )
    if node.orelse:
        else_code = codegen_block(node.orelse, child_context)
        loop_code += (
            f" (common-lisp:unless {loop_broke_name} "
            f"(common-lisp:progn {else_code} ))"
        )
    return loop_code + ")"


def codegen_break(node, context: Context):
    if not context.loop_block_name or not context.loop_broke_name:
        raise Exception("Trying to break but not inside a loop.")
    return (
        f"(common-lisp:setf {context.loop_broke_name} COMMON-LISP::t) "
        f"(common-lisp:return-from {context.loop_block_name} COMMON-LISP::nil)"
    )


def codegen_continue(node, context: Context):
    if not context.loop_continue_name:
        raise Exception("Trying to continue but not inside a loop.")
    return f"(common-lisp:return-from {context.loop_continue_name} COMMON-LISP::nil)"


def codegen_if(node, context : Context):
    child_context = context.child()
    conditional = codegen(node.test, child_context)

    # If statement vs. If expression handling. Preserve top-level assignment
    # semantics for statements inside module-level branches.
    branch_context = context if context.top_level_stmt else child_context
    if isinstance(node.body, list):
        true_code = codegen_block(node.body, branch_context)
        true_branch = f"(common-lisp:progn {true_code})" if true_code else "COMMON-LISP::nil"
    else:
        true_branch = codegen(node.body, child_context)

    if isinstance(node.orelse, list):
        false_code = codegen_block(node.orelse, branch_context)
        false_branch = f"(common-lisp:progn {false_code})" if false_code else "COMMON-LISP::nil"
    else:
        false_branch = codegen(node.orelse, child_context)

    return f"(COMMON-LISP::if (|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {conditional}) {true_branch} {false_branch})"



def codegen_import_binding(context: Context, local_name: str, value_code: str) -> str:
    symbol = map_name(local_name)
    if context.top_level_stmt:
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {lisp_string(local_name)} {symbol} {value_code})"
    return f"(common-lisp:setf {symbol} {value_code})"


def codegen_import(node, context: Context):
    forms = []
    for alias in node.names:
        if alias.asname:
            bind_name = alias.asname
            value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-NAME {lisp_string(alias.name)} '(\"*\"))"
        else:
            bind_name = alias.name.partition('.')[0]
            value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-NAME {lisp_string(alias.name)})"
        forms.append(codegen_import_binding(context, bind_name, value))
    return "(common-lisp:progn " + " ".join(forms) + ")"


def codegen_import_from(node, context: Context):
    module_name = node.module or ""
    if any(alias.name == "*" for alias in node.names):
        if not context.top_level_stmt:
            raise Exception("import * only allowed at module level")
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-STAR "
            f"{lisp_string(module_name)} {node.level})"
        )
    fromlist = "'(" + " ".join(lisp_string(alias.name) for alias in node.names) + ")"
    module_symbol = f"__clamp_import_module_{id(node)}"
    bindings = []
    for alias in node.names:
        bind_name = alias.asname or alias.name
        value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-FROM {module_symbol} {lisp_string(alias.name)})"
        bindings.append(codegen_import_binding(context, bind_name, value))
    return (
        f"(common-lisp:let (({module_symbol} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-NAME "
        f"{lisp_string(module_name)} {fromlist} {node.level}))) "
        + " ".join(bindings)
        + ")"
    )

def codegen_import_block(node, rest, context: Context) -> str:
    rest_code = codegen_block(rest, context)
    if not rest_code:
        return codegen(node, context)

    bindings = []
    if isinstance(node, ast.Import):
        for alias in node.names:
            if alias.asname:
                bind_name = alias.asname
                value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-NAME {lisp_string(alias.name)} '(\"*\"))"
            else:
                bind_name = alias.name.partition('.')[0]
                value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-NAME {lisp_string(alias.name)})"
            bindings.append((bind_name, value))
    elif isinstance(node, ast.ImportFrom):
        module_name = node.module or ""
        if any(alias.name == "*" for alias in node.names):
            raise Exception("import * only allowed at module level")
        fromlist = "'(" + " ".join(lisp_string(alias.name) for alias in node.names) + ")"
        module_symbol = f"__clamp_import_module_{id(node)}"
        module_value = (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-NAME "
            f"{lisp_string(module_name)} {fromlist} {node.level})"
        )
        body = rest_code
        for alias in reversed(node.names):
            bind_name = alias.asname or alias.name
            value = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-IMPORT-FROM {module_symbol} {lisp_string(alias.name)})"
            body = f"(|CLAMP.__builtins__|:ASSIGN ({map_name(bind_name)} {value}) {body})"
        return f"(common-lisp:let (({module_symbol} {module_value})) {body})"
    else:
        raise Exception(f"Unsupported import block node: {type(node)}")

    body = rest_code
    for bind_name, value in reversed(bindings):
        body = f"(|CLAMP.__builtins__|:ASSIGN ({map_name(bind_name)} {value}) {body})"
    return body


BUILTIN_CASED_NAMES = {
    "BaseException", "Exception", "Warning", "UserWarning", "DeprecationWarning", "RuntimeWarning", "MemoryError",
    "StopIteration", "StopAsyncIteration", "RuntimeError", "AssertionError", "TypeError", "ValueError",
    "LookupError", "ImportError", "ModuleNotFoundError", "AttributeError", "NameError",
    "OSError", "FileNotFoundError", "TimeoutError", "NotImplemented", "Ellipsis",
}


CL_SYMBOL_COLLISION_NAMES = BUILTIN_CALL_NAMES | BUILTIN_VALUE_NAMES | {
    "t", "nil", "type", "class", "lambda", "let", "block", "return", "loop",
    "if", "or", "and", "not", "format", "function", "values", "setf",
    "hmac",
}


def map_name(name: str) -> str:
    if name in BUILTIN_CASED_NAMES:
        return name
    escaped = name.replace("\\", "\\\\").replace("|", "\\|")
    return f"|{escaped}|"


def codegen_bytes(value: bytes) -> str:
    values = " ".join(str(byte) for byte in value)
    return (
        "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-BYTES-FROM-VECTOR "
        f"(common-lisp:make-array {len(value)} "
        ":element-type '(common-lisp:unsigned-byte 8) "
        f":initial-contents '({values})))"
    )


def codegen_sequence_display(node, context: Context, maker: str) -> str:
    if not any(isinstance(elt, ast.Starred) for elt in node.elts):
        return (
            f"({maker}"
            + "".join(f" {codegen(elt, context.child())}" for elt in node.elts)
            + ")"
        )

    chunks = []
    for elt in node.elts:
        if isinstance(elt, ast.Starred):
            chunks.append(f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ITERABLE-TO-LIST {codegen(elt.value, context.child())})")
        else:
            chunks.append(f"(common-lisp:list {codegen(elt, context.child())})")
    return f"(common-lisp:apply #'{maker} (common-lisp:append {' '.join(chunks)}))"


codegen_handlers[type(None)] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
codegen_handlers[ast.Expr] = lambda node, context: codegen(node.value, context)
codegen_handlers[ast.Assign] = codegen_assign
codegen_handlers[ast.AnnAssign] = codegen_annassign
codegen_handlers[ast.AugAssign] = codegen_augassign
codegen_handlers[ast.Delete] = codegen_delete
codegen_handlers[ast.Import] = codegen_import
codegen_handlers[ast.ImportFrom] = codegen_import_from
codegen_handlers[ast.Pass] = lambda node, _: "COMMON-LISP::nil"
codegen_handlers[ast.Nonlocal] = lambda node, _: "COMMON-LISP::nil"
codegen_handlers[ast.Global] = lambda node, _: "COMMON-LISP::nil"
codegen_handlers[ast.FunctionDef] = codegen_function
codegen_handlers[ast.AsyncFunctionDef] = codegen_async_function
codegen_handlers[ast.ClassDef] = codegen_class
codegen_handlers[ast.Call] = codegen_funcall
codegen_handlers[ast.Lambda] = codegen_lambda
codegen_handlers[ast.NamedExpr] = codegen_namedexpr
codegen_handlers[ast.Await] = codegen_await
codegen_handlers[ast.Yield] = codegen_yield
codegen_handlers[ast.YieldFrom] = codegen_yield
codegen_handlers[ast.Raise] = codegen_raise
codegen_handlers[ast.Assert] = codegen_assert
codegen_handlers[ast.Try] = codegen_try
codegen_handlers[ast.List] = lambda node, context: codegen_sequence_display(
    node, context, "|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST"
)
codegen_handlers[ast.ListComp] = codegen_listcomp
codegen_handlers[ast.SetComp] = codegen_setcomp
codegen_handlers[ast.GeneratorExp] = codegen_generatorexp
codegen_handlers[ast.Set] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE |CLAMP.__builtins__|:SET "
    + codegen_sequence_display(node, context, "|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST")
    + ")"
)
codegen_handlers[ast.DictComp] = codegen_dictcomp
codegen_handlers[ast.Tuple] = lambda node, context: codegen_sequence_display(
    node, context, "|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TUPLE"
)
codegen_handlers[ast.Dict] = codegen_dict
codegen_handlers[ast.Attribute] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR "
    f"{codegen(node.value, context.child())} {codegen(node.attr, context.child())})"
)
codegen_handlers[ast.Slice] = codegen_slice
codegen_handlers[ast.Name] = codegen_name
codegen_handlers[ast.Module] = codegen_module
codegen_handlers[ast.Subscript] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:PY-GETITEM "
    f"{codegen(node.value, context.child())} {codegen(node.slice, context.child())})"
)
codegen_handlers[ast.If] = codegen_if
codegen_handlers[ast.IfExp] = codegen_if
codegen_handlers[ast.While] = codegen_while
codegen_handlers[ast.For] = codegen_for
codegen_handlers[ast.AsyncFor] = codegen_async_for
codegen_handlers[ast.AsyncWith] = codegen_async_with
codegen_handlers[ast.With] = codegen_with
codegen_handlers[ast.Break] = codegen_break
codegen_handlers[ast.Continue] = codegen_continue
codegen_handlers[ast.Add] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-ADD"
codegen_handlers[ast.Sub] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-SUB"
codegen_handlers[ast.Mult] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-MUL"
codegen_handlers[ast.MatMult] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-MATMUL"
codegen_handlers[ast.Div] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-TRUEDIV"
codegen_handlers[ast.FloorDiv] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-FLOORDIV"
codegen_handlers[ast.Mod] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-MOD"
codegen_handlers[ast.Pow] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-POW"
codegen_handlers[ast.BitOr] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-BITOR"
codegen_handlers[ast.BitAnd] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-BITAND"
codegen_handlers[ast.BitXor] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-BITXOR"
codegen_handlers[ast.LShift] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-LSHIFT"
codegen_handlers[ast.RShift] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-RSHIFT"
codegen_handlers[ast.BinOp] = codegen_binary_operator
codegen_handlers[ast.Compare] = codegen_compare
codegen_handlers[ast.BoolOp] = codegen_bool_operator
codegen_handlers[ast.UnaryOp] = codegen_unary_operator
codegen_handlers[ast.Constant] = codegen_constant
codegen_handlers[ast.JoinedStr] = codegen_joinedstr
codegen_handlers[ast.Return] = codegen_return
codegen_handlers[ast.Eq] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-EQ"
codegen_handlers[ast.NotEq] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-NE"
codegen_handlers[ast.Is] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-IS"
codegen_handlers[ast.IsNot] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-IS-NOT"
codegen_handlers[ast.In] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-CONTAINS"
codegen_handlers[ast.NotIn] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-NOT-CONTAINS"
codegen_handlers[ast.Lt] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-LT"
codegen_handlers[ast.LtE] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-LE"
codegen_handlers[ast.Gt] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-GT"
codegen_handlers[ast.GtE] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-GE"
codegen_handlers[ast.Not] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-NOT"
codegen_handlers[ast.USub] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-NEG"
codegen_handlers[ast.UAdd] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-POS"
codegen_handlers[ast.Invert] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-INVERT"
codegen_handlers[ast.Or] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-OR"
codegen_handlers[ast.And] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-AND"
codegen_handlers[int] = lambda node, _: str(node)
codegen_handlers[float] = lambda node, _: lisp_float(node)
codegen_handlers[str] = lambda node, _: lisp_string(node)
codegen_handlers[bytes] = lambda node, _: codegen_bytes(node)
codegen_handlers[bool] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:*PY-TRUE*" if node else "|CLAMP.__CLAMP_INTERNALS__|:*PY-FALSE*"

# TODO:
# Exception: Do not have support to codegen <class 'ast.Compare'> node with value Compare(left=Constant(value=5), ops=[Eq()], comparators=[Constant(value=2)])

def codegen_args(args, context: Context, default_symbols=None):
    #print(args.args)
    #print(args.defaults)
    #print(args.kw_defaults)
    #print(args.kwarg)
    #print(args.kwonlyargs)
    #print(args.posonlyargs)
    #print(args.vararg)
    default_symbols = default_symbols or []
    required_count = len(args.args) - len(default_symbols)
    required_args = [map_name(a.arg) for a in args.args[:required_count]]
    optional_args = [
        f"({map_name(arg.arg)} {default_symbol})"
        for arg, default_symbol in zip(args.args[required_count:], default_symbols)
    ]
    if optional_args:
        return " ".join([*required_args, "common-lisp:&optional", *optional_args])
    return " ".join(required_args)


def clamp_compiler(code, module_name="__main__", package_name="CLAMP", source_path=None):
    if CLAMP_VERBOSE:
        print("Preparing to compile:", code)
    code_tree = ast.parse(code)
    return codegen(
        code_tree,
        Context(
            top_level_stmt=True,
            module_name=module_name,
            package_name=package_name,
            source_path=source_path,
        ),
    )


def demo():
    v = clamp_compiler("""
x = 1
y = 2

def f(x):
  return x + 1

# prior to this, run (setq/setf even #'evenp)
def g(x, y, z):
    if even(x):
        return y
    else:
        return z
""")
    print(v)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        print(f"Compiling {filename}")
        contents = Path(filename).read_text()
        result = clamp_compiler(contents)
        print("Result:\n")
        print(result)
    else:
        demo()
