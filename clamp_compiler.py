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
        unpacked_symbol = f"__clamp_unpack_{id(target)}"
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


def codegen_assign(node, context : Context):
    if len(node.targets) != 1:
        raise Exception("TODO: destructuring bind")
    return codegen_store_target(node.targets[0], codegen(node.value, context.child()), context)


def node_contains_yield(node):
    return any(isinstance(child, (ast.Yield, ast.YieldFrom)) for child in ast.walk(node))


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
            raise Exception("TODO: destructuring bind")
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
    return first_code + ("\n" + rest_code if rest_code else "")



def codegen_args_with_keyword_support(args, context: Context, default_symbols=None, owner_id=None):
    default_symbols = default_symbols or []
    owner_id = owner_id or id(args)
    if args.posonlyargs or args.kwonlyargs or args.vararg or args.kwarg:
        raise Exception("TODO: unsupported function parameter shape")
    call_args = f"__clamp_call_args_{owner_id}"
    bound_args = f"__clamp_bound_args_{owner_id}"
    param_names = "'(" + " ".join(lisp_string(arg.arg) for arg in args.args) + ")"
    required_count = len(args.args) - len(default_symbols)
    defaults = (
        "(common-lisp:list " + " ".join(default_symbols) + ")"
        if default_symbols else
        "COMMON-LISP::nil"
    )
    lambda_list = f"common-lisp:&rest {call_args}"
    bindings = [
        f"({arg.arg} (common-lisp:nth {index} {bound_args}))"
        for index, arg in enumerate(args.args)
    ]
    body_prefix = (
        f"(common-lisp:let* (({bound_args} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-BIND-ARGS {lisp_string(owner_id if isinstance(owner_id, str) else str(owner_id))} "
        f"{param_names} {required_count} {defaults} {call_args})) "
        + " ".join(bindings)
        + ") "
    )
    body_suffix = ")"
    return lambda_list, body_prefix, body_suffix


def codegen_function(node, context : Context):
    child_context = context.child()

    default_symbols = [
        f"__clamp_default_{id(node)}_{index}"
        for index, _ in enumerate(node.args.defaults)
    ]
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(node.args, child_context, default_symbols, node.name)

    # Python is a Lisp-1, Common Lisp is a Lisp-2
    # For compiled Python code running in SBCL, we'll put functions and other variables in the
    # same namespace which means we need to use funcall/apply to invoke compiled Python functions.
    default_bindings = ""
    if node.args.defaults:
        default_bindings = (
            "(common-lisp:let ("
            + " ".join(
                f"({symbol} {codegen(default, child_context)})"
                for symbol, default in zip(default_symbols, node.args.defaults)
            )
            + ") "
        )
    setter = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(node.name, child_context)} {node.name} "
        if context.top_level_stmt
        else f"(common-lisp:setf {node.name} "
    )
    hed = (
        setter
        + f"{default_bindings}"
        + f"(common-lisp:lambda ({params}) {arg_body_prefix}(common-lisp:block {node.name} "
    )

    body_context = replace(child_context, block_name=node.name)
    bod = codegen_block(node.body, body_context)
    body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"

    definition = hed + body + ")))" + arg_body_suffix + ( ")" if default_bindings else "")
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
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(node.args, child_context, default_symbols, node.name)

    default_bindings = ""
    if node.args.defaults:
        default_bindings = (
            "(common-lisp:let ("
            + " ".join(
                f"({symbol} {codegen(default, child_context)})"
                for symbol, default in zip(default_symbols, node.args.defaults)
            )
            + ") "
        )
    setter = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {codegen(node.name, child_context)} {node.name} "
        if context.top_level_stmt
        else f"(common-lisp:setf {node.name} "
    )
    maker = "MAKE-PY-ASYNC-GENERATOR" if is_async_generator else "MAKE-PY-COROUTINE"
    hed = (
        setter
        + f"{default_bindings}"
        + f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string(node.name)} :COROUTINE-FUNCTION COMMON-LISP:T :ASYNC-GENERATOR-FUNCTION {'COMMON-LISP:T' if is_async_generator else 'COMMON-LISP:NIL'} :FN "
        + f"(common-lisp:lambda ({params}) {arg_body_prefix}"
        + f"(|CLAMP.__CLAMP_INTERNALS__|:{maker} {lisp_string(node.name)} "
        + f"(common-lisp:lambda () (common-lisp:block {node.name} "
    )

    body_context = replace(child_context, block_name=node.name, in_async_function=True)
    bod = codegen_block(node.body, body_context)
    body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"

    definition = hed + body + "))))))" + arg_body_suffix + ( ")" if default_bindings else "")
    rebind = decorated_function_rebind(node, context)
    if rebind:
        return f"(common-lisp:progn {definition} {rebind})\n"
    return definition + "\n"



def codegen_function_lambda(node, context: Context, async_function: bool = False):
    child_context = context.child()
    default_symbols = [
        f"__clamp_default_{id(node)}_{index}"
        for index, _ in enumerate(node.args.defaults)
    ]
    params, arg_body_prefix, arg_body_suffix = codegen_args_with_keyword_support(node.args, child_context, default_symbols, node.name)
    default_bindings = ""
    if node.args.defaults:
        default_bindings = (
            "(common-lisp:let ("
            + " ".join(
                f"({symbol} {codegen(default, child_context)})"
                for symbol, default in zip(default_symbols, node.args.defaults)
            )
            + ") "
        )
    if async_function:
        is_async_generator = node_contains_yield(node)
        maker = "MAKE-PY-ASYNC-GENERATOR" if is_async_generator else "MAKE-PY-COROUTINE"
        body_context = replace(child_context, block_name=node.name, in_async_function=True)
        bod = codegen_block(node.body, body_context)
        body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
        expr = (
            f"{default_bindings}(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-CALLABLE :NAME {lisp_string(node.name)} :COROUTINE-FUNCTION COMMON-LISP:T :ASYNC-GENERATOR-FUNCTION {'COMMON-LISP:T' if is_async_generator else 'COMMON-LISP:NIL'} :FN "
            f"(common-lisp:lambda ({params}) {arg_body_prefix}"
            f"(|CLAMP.__CLAMP_INTERNALS__|:{maker} {lisp_string(node.name)} "
            f"(common-lisp:lambda () (common-lisp:block {node.name} {body}))))"
            f"{arg_body_suffix})"
            + (")" if default_bindings else "")
        )
    else:
        body_context = replace(child_context, block_name=node.name)
        bod = codegen_block(node.body, body_context)
        body = f"(common-lisp:progn {bod} |CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*)" if bod else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
        expr = (
            f"{default_bindings}(common-lisp:lambda ({params}) {arg_body_prefix}"
            f"(common-lisp:block {node.name} {body}))"
            f"{arg_body_suffix}"
            + (")" if default_bindings else "")
        )
    return expr


def codegen_class(node, context: Context):
    if node.bases:
        raise Exception("TODO: class inheritance is not supported yet")
    child_context = context.child()
    class_symbol = map_name(node.name)
    type_symbol = f"__clamp_class_{id(node)}"
    forms = []
    for stmt in node.body:
        if isinstance(stmt, ast.Pass):
            continue
        if isinstance(stmt, ast.FunctionDef):
            forms.append(
                f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.name)}) "
                f"{codegen_function_lambda(stmt, child_context, async_function=False)})"
            )
        elif isinstance(stmt, ast.AsyncFunctionDef):
            forms.append(
                f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.name)}) "
                f"{codegen_function_lambda(stmt, child_context, async_function=True)})"
            )
        elif isinstance(stmt, ast.Assign) and len(stmt.targets) == 1 and isinstance(stmt.targets[0], ast.Name):
            forms.append(
                f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-TYPE-ATTR {type_symbol} {lisp_string(stmt.targets[0].id)}) "
                f"{codegen(stmt.value, child_context)})"
            )
        else:
            raise Exception(f"TODO: unsupported class body node {type(stmt)}")
    forms_code = " ".join(forms) if forms else "COMMON-LISP::nil"
    make_type = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TYPE :TYPE |CLAMP.__CLAMP_INTERNALS__|:*PY-TYPE-TYPE* "
        f":NAME {lisp_string(node.name)} :BASES (common-lisp:list |CLAMP.__CLAMP_INTERNALS__|:*PY-OBJECT-TYPE*) :BASICSIZE 1)"
    )
    if context.top_level_stmt:
        return (
            f"(common-lisp:let (({type_symbol} {make_type})) "
            f"{forms_code} "
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL {lisp_string(node.name)} {class_symbol} {type_symbol}))"
        )
    return (
        f"(|CLAMP.__builtins__|:ASSIGN ({class_symbol} {make_type}) "
        f"(common-lisp:let (({type_symbol} {class_symbol})) {forms_code} {class_symbol}))"
    )


def codegen_funcall(node, context : Context):
    child_context = context.child()
    args = [codegen(a, child_context) for a in node.args]
    keyword_args = []
    for keyword in node.keywords:
        if keyword.arg is None:
            raise Exception("TODO: **kwargs calls are not supported yet")
        keyword_args.extend([f":{keyword.arg}", codegen(keyword.value, child_context)])
    all_args = [*args, *keyword_args]

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
    if isinstance(node.func, ast.Name) and node.func.id.lower() in {"__import__", "print", "len", "bool", "callable", "isinstance", "repr", "ascii", "str", "type", "id", "iter", "next", "aiter", "anext", "reversed", "min", "max", "sum", "sorted", "list", "tuple", "abs", "round", "hash", "pow", "divmod", "all", "any", "enumerate", "zip", "filter", "map", "range", "slice", "bin", "oct", "hex", "chr", "ord"}:
        target = f"|CLAMP.__builtins__|:{node.func.id.upper()}"
    if args_str:
        return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {target} {args_str})"
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {target})"



def codegen_await(node, context: Context):
    if not context.in_async_function:
        raise Exception("'await' outside async function")
    awaited = codegen(node.value, context.child())
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-AWAIT {awaited})"


def codegen_yield(node, context: Context):
    if not context.in_async_function:
        raise Exception("TODO: synchronous generators are not supported yet")
    if isinstance(node, ast.YieldFrom):
        raise Exception("TODO: yield from is not supported in async generators")
    value = codegen(node.value, context.child()) if node.value else "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ASYNC-GENERATOR-YIELD {value})"


def lisp_string(value: str) -> str:
    return '"' + value.replace('\\', '\\\\').replace('"', '\\"') + '"'


def codegen_module(node, context : Context):
    header_code = (
        f"(common-lisp:in-package {lisp_string(context.package_name)})\n"
        f"(common-lisp:use-package \"CLAMP.__builtins__\")\n"
    )
    source_code = "COMMON-LISP::nil" if context.source_path is None else lisp_string(context.source_path)
    enter_code = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-ENTER-MODULE "
        f"{lisp_string(context.module_name)} {source_code} {lisp_string(context.package_name)})\n"
    )
    name_code = (
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SET-GLOBAL \"__name__\" __name__ "
        f"{lisp_string(context.module_name)})\n"
    )
    body_code = codegen_block(node.body, context)
    return (header_code + enter_code + name_code + body_code)

def codegen_return(node, context : Context):
    if not context.block_name:
        raise Exception("Trying to return but not inside a lexical scope.")
    retval = codegen(node.value, context.child())
    return f"(common-lisp:return-from {context.block_name} {retval})"


def codegen_raise(node, context: Context):
    if node.cause is not None:
        raise Exception("TODO: raise ... from ... is not supported yet")
    if node.exc is None:
        raise Exception("TODO: bare raise is not supported yet")
    exception = codegen(node.exc, context.child())
    return f"(|CLAMP.__CLAMP_INTERNALS__|:PY-RAISE {exception})"


def codegen_try_without_finally(node, context: Context):
    child_context = context.child()
    condition_symbol = f"__clamp_try_condition_{id(node)}"
    exception_symbol = f"__clamp_try_exception_{id(node)}"
    normal_body = [*node.body, *node.orelse]
    body = codegen_block(normal_body, child_context) or "COMMON-LISP::nil"
    if not node.handlers:
        return f"(common-lisp:progn {body})"

    clauses = []
    bare_seen = False
    for handler in node.handlers:
        handler_body = codegen_block(handler.body, child_context) or "COMMON-LISP::nil"
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
    return (
        f"(common-lisp:handler-case (common-lisp:progn {body}) "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION ({condition_symbol}) "
        f"(common-lisp:let (({exception_symbol} "
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-EXCEPTION-VALUE {condition_symbol}))) "
        f"(common-lisp:cond {' '.join(clauses)}))))"
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


def codegen_comprehension_loop(node, context: Context, emit_body):
    if not node.generators:
        return emit_body()
    if any(generator.is_async for generator in node.generators) and not context.in_async_function:
        raise Exception("asynchronous comprehension outside async function")

    child_context = context.child()

    def truthy_filters(generator):
        if not generator.ifs:
            return None
        checks = [
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-TRUTHY-P {codegen(condition, child_context)})"
            for condition in generator.ifs
        ]
        return "(common-lisp:and " + " ".join(checks) + ")"

    def build_generator(index: int) -> str:
        if index == len(node.generators):
            return emit_body()
        generator = node.generators[index]
        gen_id = f"{id(node)}_{index}"
        iterator_symbol = f"__clamp_comp_iterator_{gen_id}"
        item_symbol = f"__clamp_comp_item_{gen_id}"
        found_symbol = f"__clamp_comp_found_{gen_id}"
        target_bindings = codegen_target_bindings(generator.target, child_context)
        target_store = codegen_store_target(generator.target, item_symbol, child_context)
        body = build_generator(index + 1)
        filter_code = truthy_filters(generator)
        if filter_code:
            body = f"(common-lisp:when {filter_code} {body})"
        iter_fn = "PY-AITER" if generator.is_async else "PY-ITER"
        next_fn = "PY-ANEXT-ITEM" if generator.is_async else "PY-NEXT-ITEM"
        iterable = codegen(generator.iter, child_context)
        return (
            f"(common-lisp:let ({target_bindings}) "
            f"(common-lisp:let (({iterator_symbol} (|CLAMP.__CLAMP_INTERNALS__|:{iter_fn} {iterable}))) "
            f"(common-lisp:loop "
            f"(common-lisp:multiple-value-bind ({item_symbol} {found_symbol}) "
            f"(|CLAMP.__CLAMP_INTERNALS__|:{next_fn} {iterator_symbol}) "
            f"(common-lisp:unless {found_symbol} (common-lisp:return)) "
            f"{target_store} "
            f"{body}))))"
        )

    return build_generator(0)


def codegen_listcomp(node, context: Context):
    result_symbol = f"__clamp_listcomp_result_{id(node)}"
    child_context = context.child()

    def emit_body():
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-APPEND {result_symbol} "
            f"{codegen(node.elt, child_context)})"
        )

    return (
        f"(common-lisp:let (({result_symbol} (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST))) "
        f"{codegen_comprehension_loop(node, context, emit_body)} {result_symbol})"
    )


def codegen_dictcomp(node, context: Context):
    result_symbol = f"__clamp_dictcomp_result_{id(node)}"
    child_context = context.child()

    def emit_body():
        return (
            f"(|CLAMP.__CLAMP_INTERNALS__|:PY-SETITEM {result_symbol} "
            f"{codegen(node.key, child_context)} {codegen(node.value, child_context)})"
        )

    return (
        f"(common-lisp:let (({result_symbol} (|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS))) "
        f"{codegen_comprehension_loop(node, context, emit_body)} {result_symbol})"
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
    return f"(common-lisp:setf (|CLAMP.__CLAMP_INTERNALS__|:PY-OBJECT-ATTR {target} {attr}) {value_code})"


def codegen_delete(node, context: Context):
    if len(node.targets) != 1:
        raise Exception("TODO: unsupported delete target count")
    target = node.targets[0]
    if not isinstance(target, ast.Subscript):
        raise Exception("TODO: unsupported delete target")
    child_context = context.child()
    obj = codegen(target.value, child_context)
    index = codegen(target.slice, child_context)
    return (
        "(|CLAMP.__CLAMP_INTERNALS__|:PY-DELITEM "
        f"{obj} {index})"
    )

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
    exit_call = f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {exit_symbol} {manager_symbol}"
    return (
        f"(common-lisp:let* (({manager_symbol} {manager}) "
        f"({exit_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR {manager_symbol} \"__exit__\")) "
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
        f"(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE {exit_symbol} {manager_symbol}"
    )
    return (
        f"(common-lisp:let* (({manager_symbol} {manager}) "
        f"({exit_symbol} (|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR {manager_symbol} \"__aexit__\")) "
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

    # If statement vs. If expression handling
    if isinstance(node.body, list):
        true_code = codegen_block(node.body, child_context)
        true_branch = f"(common-lisp:progn {true_code})" if true_code else "COMMON-LISP::nil"
    else:
        true_branch = codegen(node.body, child_context)

    if isinstance(node.orelse, list):
        false_code = codegen_block(node.orelse, child_context)
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


def map_name(name: str) -> str:
    return name


def codegen_bytes(value: bytes) -> str:
    values = " ".join(str(byte) for byte in value)
    return (
        "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-BYTES-FROM-VECTOR "
        f"(common-lisp:make-array {len(value)} "
        ":element-type '(common-lisp:unsigned-byte 8) "
        f":initial-contents '({values})))"
    )


codegen_handlers[type(None)] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:*PY-NONE*"
codegen_handlers[ast.Expr] = lambda node, context: codegen(node.value, context)
codegen_handlers[ast.Assign] = codegen_assign
codegen_handlers[ast.AugAssign] = codegen_augassign
codegen_handlers[ast.Delete] = codegen_delete
codegen_handlers[ast.Import] = codegen_import
codegen_handlers[ast.ImportFrom] = codegen_import_from
codegen_handlers[ast.Pass] = lambda node, _: "COMMON-LISP::nil"
codegen_handlers[ast.FunctionDef] = codegen_function
codegen_handlers[ast.AsyncFunctionDef] = codegen_async_function
codegen_handlers[ast.ClassDef] = codegen_class
codegen_handlers[ast.Call] = codegen_funcall
codegen_handlers[ast.Await] = codegen_await
codegen_handlers[ast.Yield] = codegen_yield
codegen_handlers[ast.YieldFrom] = codegen_yield
codegen_handlers[ast.Raise] = codegen_raise
codegen_handlers[ast.Try] = codegen_try
codegen_handlers[ast.List] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-LIST"
    + "".join(f" {codegen(elt, context.child())}" for elt in node.elts)
    + ")"
)
codegen_handlers[ast.ListComp] = codegen_listcomp
codegen_handlers[ast.DictComp] = codegen_dictcomp
codegen_handlers[ast.Tuple] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-TUPLE"
    + "".join(f" {codegen(elt, context.child())}" for elt in node.elts)
    + ")"
)
codegen_handlers[ast.Dict] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:MAKE-PY-DICT-FROM-PAIRS"
    + "".join(
        f" (common-lisp:list {codegen(key, context.child())} {codegen(value, context.child())})"
        for key, value in zip(node.keys, node.values)
    )
    + ")"
)
codegen_handlers[ast.Attribute] = lambda node, context: (
    "(|CLAMP.__CLAMP_INTERNALS__|:PY-LOOKUP-ATTR "
    f"{codegen(node.value, context.child())} {codegen(node.attr, context.child())})"
)
codegen_handlers[ast.Slice] = codegen_slice
codegen_handlers[ast.Name] = lambda node, _: map_name(node.id)
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
codegen_handlers[ast.Sub] = lambda node, _: "COMMON-LISP::-"
codegen_handlers[ast.Mult] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-MUL"
codegen_handlers[ast.Div] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-TRUEDIV"
codegen_handlers[ast.FloorDiv] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-FLOORDIV"
codegen_handlers[ast.Mod] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-MOD"
codegen_handlers[ast.Pow] = lambda node, _: "|CLAMP.__CLAMP_INTERNALS__|:PY-POW"
codegen_handlers[ast.BinOp] = codegen_binary_operator
codegen_handlers[ast.Compare] = codegen_compare
codegen_handlers[ast.BoolOp] = codegen_bool_operator
codegen_handlers[ast.UnaryOp] = codegen_unary_operator
codegen_handlers[ast.Constant] = lambda node, _: codegen(node.value)
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
codegen_handlers[float] = lambda node, _: str(node)
codegen_handlers[str] = lambda node, _: '"' + str(node) + '"' # TODO: escape nested quotes correctly
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
    required_args = [a.arg for a in args.args[:required_count]]
    optional_args = [
        f"({arg.arg} {default_symbol})"
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
