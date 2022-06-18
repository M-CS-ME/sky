#!/usr/bin/env python3

import functools as ft
import sys

'''
== λ-Calculus Interpreter

=== Goals
1. β-Reduction
2. α-equivalence
3. Environment for Naming
4. REPL
5. File Support
'''

#== Helper Functions

def isselfeval(ex):
    return type(ex) == str or ex[0] == 'λ'

def islambda(ex):
    return ex[0] == 'λ'

def isdefine(ex):
    return ex[0] == 'def'

def isapp(ex):
    l = ft.reduce(lambda x, y: x or y, [islambda(i) for i in ex])
    return len(ex) >= 2 and l

def isapp_nested(ex):
    if type(ex) != list: return False
    if len(ex) < 2: return False
    a = False
    for i in ex:
        if i[0] == 'λ': a = True
        elif type(i) == list: a = isapp_nested(i)
    return True in [a, isapp(ex)] # python is retarded (True and not False => False)

def flatten(ex):
    o = []
    for i in ex:
        if type(i) == list and islambda(i):
            o.append(i)
        elif type(i) == list:
            o.extend(i)
        else:
            o.append(i)
    return o

def replace(l, a, b):
    o = []
    for i in l:
        if i == a: o.append(b)
        elif type(i) == list: o.append(replace(i, a, b))
        else: o.append(i)
    return o

#== Environment

class Env:
    def __init__(self):
        self.names = []
        self.vals  = []

    def isdef(self, name):
        if name in self.names:
            return True
        return False

    def val(self, name):
        return self.vals[self.names.index(name)]

    def defvar(self, name, val):
        self.names.append(name)
        self.vals.append(val)

envr = Env()

#== Evaluator

def apply(fun, arg, env):
    fun_args = fun[1]
    fun_expr = fun[2]

    if len(fun_args) == len(arg):
        for i, j in zip(fun_args, arg):
            fun_expr = replace(fun_expr, i, j)
        return eval(fun_expr, env)
    elif len(fun_args) > len(arg):
        fun_args = fun_args[:len(arg)]
        for i, j in zip(fun_args, arg):
            fun_expr = replace(fun_expr, i, j)
        return ['λ', fun[1][len(arg):], eval(fun_expr, env)]
    elif len(fun_args) < len(arg):
        arg = arg[:len(fun_args)]
        for i, j in zip(fun_args, arg):
            fun_expr = replace(fun_expr, i, j)
        o = [fun_expr]
        o.extend(arg[len(fun_args)])
        return eval(o, env)

def eval(ex, env):
    if ex == []:
        return None
    if isdefine(ex):
        return env.defvar(ex[1], ex[2])
    elif env.isdef(ex):
        return eval(env.val(ex), env)
    elif isapp(ex):
        if len(ex[1:]) > 1:
            return apply(ex[0], ex[1:], env)
        return apply(ex[0], [ex[1]], env)
    elif isselfeval(ex):
        return ex
    elif type(ex) == list:
        return [eval(i, env) for i in ex]

#== REPL and Parsing
# Parser copied straight from Peter Norvig's Lisp in Python article:
# http://www.norvig.com/lispy.html

def tokenize(s):
    return s.replace('(', ' ( ').replace(')', ' ) ').replace('\\', 'λ ').split()

def readtokens(tokens):
    if len(tokens) == 0:
        return []
    token = tokens.pop(0)
    if token == '(':
        L = []
        while tokens[0] != ')':
            L.append(readtokens(tokens))
        tokens.pop(0)
        return L
    elif token == ')':
        raise SyntaxError('Unexpected \')\'')
    else:
        return token

def parse(s):
    return readtokens(tokenize(s))

def outval(ast):
    return repr(ast).replace('[', ' [ ').replace(']', ' ] ').replace('\'', '').replace(',', '')

def repl(p='λ> '):
    while True:
        val = eval(parse(input(p)), envr)
        if val == 'exit':
            exit()
        elif val is not None:
            print(outval(val))

#== Reading From a File

def fread(fname):
    try:
        f = open(fname, 'r')
        lines = f.readlines()
        for l in lines:
            print(outval(eval(parse(l))))
    except:
        print("Error: file not found")

#== Main and Command Line Args

help = '''
Sky, a λ-Calculus Interpreter

Usage:
    sky [option] <arguments>

The options and args are:
    -s <file>   load and run <file>
    -e '<ex>'   evaluate <expr>
    -h          help
'''

def main():
    if len(sys.argv) == 1:
        repl()
    elif sys.argv[1] == '-h':
        print(help)
    elif sys.argv[1] == '-s' and len(sys.argv) == 3:
        fread(sys.argv[2])
    elif sys.argv[1] == '-e' and len(sys.argv) == 3:
        print(outval(eval(parse(sys.argv[2]), envr)))
    else:
        print("Error: bad args")

if __name__ == '__main__':
    main()
    #print(isapp_nested(parse(sys.argv[2])))
