#!/usr/bin/env python

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


def codegen_assign(node, context : Context):
    # Standalone assignment form. Prefer codegen_block to orchestrate proper scoping.
    # Fallback: emit ASSIGN without a body.
    if len(node.targets) == 1:
        lhs = codegen(node.targets[0], context.child())
        rhs = codegen(node.value, context.child())
        if context.top_level_stmt:
            return f"(|CLAMP.__builtins__|:ASSIGN (:GLOBAL {lhs} {rhs}))"
        else:
            return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} {rhs}))"
    else:
        raise Exception("TODO: destructuring bind")


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
        # Only support simple name targets for now
        if not isinstance(target, ast.Name):
            raise Exception("TODO: non-name assignment targets")

        lhs = codegen(target, context.child())
        rhs = codegen(first.value, context.child())
        rest_code = codegen_block(rest, context)
        if context.top_level_stmt:
            # Top-level module assignment: set global and continue
            if rest_code:
                return f"(|CLAMP.__builtins__|:ASSIGN (:GLOBAL {lhs} {rhs}) {rest_code})"
            else:
                return f"(|CLAMP.__builtins__|:ASSIGN (:GLOBAL {lhs} {rhs}))"
        else:
            # Lexical binding within function or inner block
            if rest_code:
                return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} {rhs}) {rest_code})"
            else:
                return f"(|CLAMP.__builtins__|:ASSIGN ({lhs} {rhs}))"

    # Default: emit first form and then the rest
    first_code = codegen(first, context)
    rest_code = codegen_block(rest, context)
    return first_code + ("\n" + rest_code if rest_code else "")


def codegen_function(node, context : Context):
    child_context = context.child()

    params = codegen_args(node.args, child_context)

    # Python is a Lisp-1, Common Lisp is a Lisp-2
    # For compiled Python code running in SBCL, we'll put functions and other variables in the
    # same namespace which means we need to use funcall/apply to invoke compiled Python functions.
    hed = f"(common-lisp:setf {node.name} (lambda ({params}) (block {node.name} "

    body_context = replace(child_context, block_name=node.name)
    bod = codegen_block(node.body, body_context)

    return hed + bod + ")))\n"


def codegen_funcall(node, context : Context):
    child_context = context.child()
    target = codegen(node.func, child_context)
    args_str = " ".join([codegen(a, child_context) for a in node.args])
    # Map builtins.print to our package, case-insensitive
    if isinstance(node.func, ast.Name) and node.func.id.lower() == "print":
        target = "|CLAMP.__builtins__|:PRINT"
    return f"(common-lisp:funcall {target} {args_str})"


def codegen_module(node, context : Context):
    header_code = """(common-lisp:in-package :clamp) (common-lisp:use-package "CLAMP.__builtins__")\n"""
    name_code = """(common-lisp:setq __name__ "__main__")\n"""
    # Keep top-level context so ASSIGN can perform global SETQ at module level
    body_code = codegen_block(node.body, context)
    return (header_code + name_code + body_code)


def codegen_return(node, context : Context):
    if not context.block_name:
        raise Exception("Trying to return but not inside a lexical scope.")
    retval = codegen(node.value, context.child())
    return f"(return-from {context.block_name} {retval})"


def codegen_binary_operator(node, context : Context):
    child_context = context.child()
    op = codegen(node.op, child_context)
    lhs = codegen(node.left, child_context)
    rhs = codegen(node.right, child_context)
    return f"({op} {lhs} {rhs})"


def codegen_if(node, context : Context):
    child_context = context.child()
    conditional = codegen(node.test, child_context)

    # If statement vs. If expression handling
    if isinstance(node.body, list):
        true_branch = codegen_block(node.body, child_context)
    else:
        true_branch = codegen(node.body, child_context)

    if isinstance(node.orelse, list):
        false_branch = codegen_block(node.orelse, child_context)
    else:
        false_branch = codegen(node.orelse, child_context)

    return f"(COMMON-LISP::if {conditional} {true_branch} {false_branch})"


def map_name(name: str) -> str:
    return name


codegen_handlers[type(None)] = lambda node, _: "COMMON-LISP::nil"
codegen_handlers[ast.Expr] = lambda node, context: codegen(node.value, context)
codegen_handlers[ast.Assign] = codegen_assign
codegen_handlers[ast.FunctionDef] = codegen_function
codegen_handlers[ast.Call] = codegen_funcall
codegen_handlers[ast.Name] = lambda node, _: map_name(node.id)
codegen_handlers[ast.Module] = codegen_module
codegen_handlers[ast.If] = codegen_if
codegen_handlers[ast.IfExp] = codegen_if
codegen_handlers[ast.Add] = lambda node, _: "COMMON-LISP::+"
codegen_handlers[ast.Sub] = lambda node, _: "COMMON-LISP::-"
codegen_handlers[ast.Mult] = lambda node, _: "COMMON-LISP::*"
codegen_handlers[ast.Div] = lambda node, _: "COMMON-LISP::/"
codegen_handlers[ast.Pow] = lambda node, _: "COMMON-LISP::expt"
codegen_handlers[ast.BinOp] = codegen_binary_operator
codegen_handlers[ast.Constant] = lambda node, _: codegen(node.value)
codegen_handlers[ast.Return] = codegen_return
codegen_handlers[int] = lambda node, _: str(node)
codegen_handlers[float] = lambda node, _: str(node)
codegen_handlers[str] = lambda node, _: '"' + str(node) + '"' # TODO: escape nested quotes correctly
codegen_handlers[bool] = lambda node, _: "COMMON-LISP::t" if node else "COMMON-LISP::nil"

# TODO:
# Exception: Do not have support to codegen <class 'ast.Compare'> node with value Compare(left=Constant(value=5), ops=[Eq()], comparators=[Constant(value=2)])

def codegen_args(args, context: Context):
    #print(args.args)
    #print(args.defaults)
    #print(args.kw_defaults)
    #print(args.kwarg)
    #print(args.kwonlyargs)
    #print(args.posonlyargs)
    #print(args.vararg)
    return " ".join([a.arg for a in args.args])


def clamp_compiler(code):
    print("Preparing to compile:", code)
    code_tree = ast.parse(code)
    return codegen(code_tree, Context(top_level_stmt=True))


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
