#!/usr/bin/env python

"""
Python to Common Lisp Compiler
"""

import ast
from dataclasses import dataclass, replace
import sys
from pathlib import Path

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
        raise Exception(f"Do not have support to codegen {str(type(node))} node with value {ast.dump(node)}")


def codegen_assign(node, context : Context):
    if len(node.targets) == 1:
        lhs = codegen(node.targets[0], context.child())
        rhs = codegen(node.value, context.child())
        return f"(setf {lhs} {rhs})"
    else:
        raise Exception("TODO: destructuring bind")


def codegen_function(node, context : Context):
    child_context = context.child()

    params = codegen_args(node.args, child_context)

    # Python is a Lisp-1, Common Lisp is a Lisp-2
    # For compiled Python code running in SBCL, we'll put functions and other variables in the
    # same namespace which means we need to use funcall/apply to invoke compiled Python functions.
    hed = f"(setf {node.name} (lambda ({params}) (block {node.name} "

    body_context = replace(child_context, block_name=node.name)
    bod = "\n".join([codegen(n, body_context) for n in node.body])

    return hed + bod + ")))\n"


def codegen_funcall(node, context : Context):
    child_context = context.child()
    target = codegen(node.func, child_context)
    args_str = " ".join([codegen(a, child_context) for a in node.args])
    return f"(funcall {target} {args_str})"


def codegen_module(node, context : Context):
    child_context = context.child()
    header_code = """(in-package :clamp) (use-package "CLAMP.__builtins__")\n"""
    name_code = """(defvar __name__ "__main__")\n"""
    body_code = "\n".join([codegen(n, child_context) for n in node.body])
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
    true_branch = "\n".join([codegen(n, child_context) for n in node.body])
    false_branch = "\n".join([codegen(n, child_context) for n in node.orelse])
    return f"(if {conditional} {true_branch} {false_branch})"


codegen_handlers[ast.Expr] = lambda node, context: codegen(node.value, context)
codegen_handlers[ast.Assign] = codegen_assign
codegen_handlers[ast.FunctionDef] = codegen_function
codegen_handlers[ast.Call] = codegen_funcall
codegen_handlers[ast.Name] = lambda node, _: node.id
codegen_handlers[ast.Module] = codegen_module
codegen_handlers[ast.If] = codegen_if
codegen_handlers[ast.Add] = lambda node, _: "+"
codegen_handlers[ast.Mult] = lambda node, _: "*"
codegen_handlers[ast.BinOp] = codegen_binary_operator
codegen_handlers[ast.Constant] = lambda node, _: codegen(node.value)
codegen_handlers[ast.Return] = codegen_return
codegen_handlers[int] = lambda node, _: str(node)
codegen_handlers[float] = lambda node, _: str(node)
codegen_handlers[str] = lambda node, _: '"' + str(node) + '"' # TODO: escape nested quotes correctly

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

# prior to this, run (setf even #'evenp)
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
