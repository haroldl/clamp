
#
# Python to Common Lisp Compiler
#

import ast

def codegen(node, top_level_stmt = False, indent_level = 0):
    """Recursive codegen of Common Lisp from a Python AST"""
    prefix = ("  " * indent_level)
    typ = type(node)
    if typ == ast.Name:
        return node.id
    elif typ == ast.Module:
        return "\n".join([codegen(n) for n in node.body])
    elif typ == ast.Assign:
        if len(node.targets) == 1:
            return prefix + "(setf " + codegen(node.targets[0]) + " " + codegen(node.value) + ")"
        else:
            raise Exception("TODO: destructuring bind")
    elif typ == ast.FunctionDef:
        params = codegen_args(node.args)
        hed = "(setf #'" + node.name + " (" + params + ")\n"
        bod = "\n".join([codegen(n, top_level_stmt, indent_level + 2) for n in node.body])
        return hed + bod + ")\n"
    elif typ == ast.BinOp:
        return prefix + "(" + codegen(node.op) + " " + codegen(node.left) + " " + codegen(node.right) + ")"
    elif typ == ast.Return:
        return codegen(node.value, top_level_stmt, indent_level)
    elif typ == ast.Constant:
        return codegen(node.value)
    elif typ == int:
        return str(node)
    elif typ == ast.Add:
        return "+"
    else:
        raise Exception("Do not have support to codegen " + str(node))


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
