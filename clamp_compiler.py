""" Python to Common Lisp Compiler
"""

import ast
import sys


codegen_handlers = {}


def codegen(node, top_level_stmt = False):
    """Recursive codegen of Common Lisp from a Python AST"""
    typ = type(node)
    if typ in codegen_handlers:
        return codegen_handlers[typ](node, top_level_stmt)
    else:
        raise Exception(f"Do not have support to codegen {str(type(node))} node with value {ast.dump(node)}")


def codegen_assign(node, top_level_stmt):
    if len(node.targets) == 1:
        return f"(setf {codegen(node.targets[0])} {codegen(node.value)})"
    else:
        raise Exception("TODO: destructuring bind")


def codegen_function(node, top_level_stmt):
    params = codegen_args(node.args)

    # Python is a Lisp-1, Common Lisp is a Lisp-2
    # For compiled Python code running in SBCL, we'll put functions and other variables in the
    # same namespace which means we need to use funcall/apply to invoke compiled Python functions.
    hed = f"(setf {node.name} (lambda ({params}) "
    bod = "\n".join([codegen(n, top_level_stmt) for n in node.body])

    return hed + bod + "))\n"


def codegen_funcall(node, top_level_stmt):
    args_str = " ".join([codegen(a, False) for a in node.args])
    return "(funcall " + codegen(node.func, top_level_stmt) + " " + args_str + ")"


def codegen_module(node, top_level_stmt):
    header_code = """(in-package :clamp) (use-package "CLAMP.__builtins__")\n"""
    name_code = """(defvar __name__ "__main__")\n"""
    body_code = "\n".join([codegen(n) for n in node.body])
    return (header_code + name_code + body_code)


codegen_handlers[ast.Expr] = lambda node, top_level_stmt: codegen(node.value, top_level_stmt)
codegen_handlers[ast.Assign] = codegen_assign
codegen_handlers[ast.FunctionDef] = codegen_function
codegen_handlers[ast.Call] = codegen_funcall
codegen_handlers[ast.Name] = lambda node, _: node.id
codegen_handlers[ast.Module] = codegen_module
codegen_handlers[ast.Add] = lambda node, _: "+"
codegen_handlers[ast.Mult] = lambda node, _: "*"
codegen_handlers[ast.BinOp] = lambda node, _: "(" + codegen(node.op) + " " + codegen(node.left) + " " + codegen(node.right) + ")"
codegen_handlers[ast.Constant] = lambda node, _: codegen(node.value)
codegen_handlers[ast.Return] = lambda node, top_level_stmt: codegen(node.value, top_level_stmt)
codegen_handlers[int] = lambda node, _: str(node)
codegen_handlers[float] = lambda node, _: str(node)
codegen_handlers[str] = lambda node, _: '"' + str(node) + '"' # TODO: escape nested quotes correctly

def codegen_args(args):
    #print(args.args)
    #print(args.defaults)
    #print(args.kw_defaults)
    #print(args.kwarg)
    #print(args.kwonlyargs)
    #print(args.posonlyargs)
    #print(args.vararg)
    return " ".join([a.arg for a in args.args])


def compile(code):
    print("Preparing to compile:", code)
    code_tree = ast.parse(code)
    return codegen(code_tree, True)


def demo():
    v = compile("""
x = 1
y = 2

def f(x):
  return x + 1
""")
    print(v)
