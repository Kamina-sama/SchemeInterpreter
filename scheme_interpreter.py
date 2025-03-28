# -*- coding: utf-8 -*-
"""
Created on Wed Mar 26 14:08:52 2025

@author: LREIS24
"""
import re
import sys
from sys import exit
from abc import ABC
from copy import deepcopy
import inspect

sys.setrecursionlimit(10000)

#Where symbols and their values as ASTs (Abstract Syntax Trees, or since we are dealign with LISP, Symbolic Expressions, or S-expressions, or SEXPRs) will be stored. There are both global and locals scopes
class Env:
    def __init__(self):
        self.symbols = {'currentInput': 'stdin', 'currentOutput': 'stdout'}
        self.stack = []
    def _find_global(self, identifier: str):
        return self.symbols[identifier] if identifier in self.symbols else None
    def _find_local(self, identifier: str):
        if len(self.stack)==0:
            return None
        current_stack_frame = self.stack[-1]
        return current_stack_frame[identifier] if identifier in current_stack_frame else None
    def _find_symbol(self, identifier: str):
        local_result = self._find_local(identifier)
        if local_result:
            return local_result
        global_result = self._find_global(identifier)
        if global_result:
            return global_result
        return None
    def find_symbol(self, identifier: str):
        result = self._find_symbol(identifier)
        if result is None:
            raise Exception(f"Symbol not found: {identifier}")
        return result
    def push_stack_frame(self, stack_frame):
        self.stack.append(stack_frame)
    def stack_not_empty(self):
        return len(self.stack)!=0
    def replace_stack_frame(self, stack_frame):
        self.stack[-1] = stack_frame
    def merge_stack_frame(self, stack_frame):
        if len(self.stack)==0:
            self.stack.append(stack_frame)
        else:
            self.stack[-1].update(stack_frame)
    def pop_stack_frame(self):
        self.stack.pop()
    def get_current_frame(self):
        return self.stack[-1] if len(self.stack)>0 else None
    def make_repl_env():
        env = Env()
        env.symbols['number?'] = IsNumberProc()
        env.symbols['='] = SymbolEqualProc()
        env.symbols['<'] = SmallerThanProc()
        env.symbols['>'] = BiggerThanProc()
        env.symbols['+'] = AddProc()
        env.symbols['-'] = MinusProc()
        env.symbols['/'] = DivProc()
        env.symbols['*'] = MulProc()
        env.symbols['modulo'] = ModuloProc()
        env.symbols['expt'] = ExptProc()
        env.symbols['number->string'] = NumberToStringProc()
        env.symbols['integer->char'] = IntToCharProc()
        env.symbols['equal?'] = EqualProc()
        env.symbols['define'] = DefineProc()
        env.symbols['set!'] = DefineProc(overwrite=True)
        env.symbols['define-macro'] = DefineProc(macro_definition=True)
        env.symbols['if'] = IfProc()
        env.symbols['cond'] = CondProc()
        env.symbols['lambda'] = LambdaProc()
        env.symbols['write'] = WriteProc()
        env.symbols['newline'] = NewLineProc()
        env.symbols['exit'] = ExitProc()
        env.symbols['quit'] = ExitProc()
        env.symbols['not'] = NotProc()
        env.symbols['string?'] = IsStringProc()
        env.symbols['string=?'] = StringEqualProc()
        env.symbols['string-append'] = StringAppendProc()
        env.symbols['string-length'] = StringLengthProc()
        env.symbols['string-ref'] = StringRefProc()
        env.symbols['string->list'] = StringToListProc()
        env.symbols['string->number'] = StringToNumberProc()
        env.symbols['string-upcase'] = StringUpcaseProc()
        env.symbols['string-downcase'] = StringDowncaseProc()
        env.symbols['char?'] = IsCharProc()
        env.symbols['char=?'] = CharEqualProc()
        env.symbols['char->integer'] = CharToIntProc()
        env.symbols['char-upcase'] = CharUpcaseProc()
        env.symbols['char-downcase'] = CharDowncaseProc()
        env.symbols['quote'] = QuoteProc()
        env.symbols['quasiquote'] = QuasiquoteProc()
        env.symbols['eval'] = EvalProc()
        env.symbols['apply'] = ApplyProc()
        env.symbols['list?'] = IsListProc()
        env.symbols['pair?'] = IsPairProc()
        env.symbols['list'] = ListProc()
        env.symbols['append'] = AppendProc()
        env.symbols['reverse'] = ReverseProc()
        env.symbols['length'] = LengthProc()
        env.symbols['list-ref'] = ListRefProc()
        env.symbols['cons'] = ConsProc()
        env.symbols['car'] = CarProc()
        env.symbols['cdr'] = CdrProc()
        env.symbols['begin'] = BeginProc()
        env.symbols['load'] = LoadProc()
        env.symbols['void'] = VoidProc()
        #env.symbols['open-input-file'] = OpenInputFileProc()
        #env.symbols['close-port'] = ClosePortProc()
        #env.symbols['read-char'] = ReadCharProc()
        #env.symbols['call-with-input-file'] = CWIFProc()
        #env.symbols['eof-object?'] = IsEOFObjectProc()
        #env.symbols['call-with-output-file'] = CWIFProc()
        return env

class SEXPR(ABC):
    def __init__(self):
        #self.parent = parent
        pass
    def evaluate(self, env):
        return self
    def partial_evaluate(self, env):
        return self
    def has_symbol(self, symbol_id):
        return False
class Atom(SEXPR):
    pass
class List(SEXPR):
    def __init__(self, children: list[SEXPR], improper = False):
        super().__init__()
        self.improper = improper
        #for child in children:
            #child.parent = self
        self.children = children
    def evaluate(self, env):
        if len(self.children)==0:
            raise Exception("No procedure at the start of the list, cannot evaluate")
        fun_to_call: Procedure = self.children[0].evaluate(env)
        if not isinstance(fun_to_call, Procedure):
            raise Exception("First element of list is not a procedure, cannot evaluate")
        args = []
        for c in self.children[1:]:
            args.append(c if (fun_to_call.special_form or fun_to_call.macro) else c.evaluate(env)) #"special form" means the procedure needs more control of how their arguments are evaluated...
        return fun_to_call.apply(args, env)
    def partial_evaluate(self, env):
        if len(self.children)==0:
            return self
        ls = lambda : List([c.partial_evaluate(env) for c in self.children])
        operator: Symbol = self.children[0]
        if not isinstance(operator, Symbol):
            return ls()
        if operator.identifier != 'comma':
            return ls()
        if len(self.children)!=2:
            raise Exception(', operator requires exaclty 1 argument')
        arg = self.children[1]
        return arg.evaluate(env)
    def __eq__(self, other):
        if not isinstance(other, List):
            return False
        if len(self.children)!=len(other.children):
            return False
        return all(m==o for m,o in zip(self.children, other.children))
    def has_symbol(self, symbol_id):
        return any(c.has_symbol(symbol_id) for c in self.children)
class Number(Atom):
    def __init__(self, num: int):
        super().__init__()
        self.num = num
    def __eq__(self, other):
        if isinstance(other, Number):
            return self.num == other.num
        return False
class String(Atom):
    def __init__(self, text: str):
        super().__init__()
        self.text = text
    def __eq__(self, other):
        if isinstance(other, String):
            return self.text == other.text
        return False
class Char(Atom):
    def __init__(self, text: str):
        super().__init__()
        self.char = text
    def __eq__(self, other):
        if isinstance(other, Char):
            return self.char == other.char
        return False
class Bool(Atom):
    def __init__(self, val: bool):
        super().__init__()
        self.val = val
    def __eq__(self, other):
        if isinstance(other, Bool):
            return self.val == other.val
        return False
class Symbol(Atom):
    def __init__(self, identifier: str):
        super().__init__()
        self.identifier = identifier
    def evaluate(self, env: Env):
        return env.find_symbol(self.identifier)
    def __eq__(self, other):
        if isinstance(other, Symbol):
            return self.identifier == other.identifier
        return False
    def has_symbol(self, symbol_id):
        return self.identifier == symbol_id

class Procedure(SEXPR):
    def set_attributes(self, special_form: bool, variadic: bool, macro: bool):
        self.special_form = special_form
        self.variadic = variadic
        self.macro = macro
    def __init__(self, params: list[Symbol], body: list[SEXPR], identifier = ''):
        self.identifier = identifier
        self.body = body
        self.params = params
        self.set_attributes(False, False, False)
        self.closure_env = {}
        self.closure_symbols = set()
        if len(self.params)>=2:
            maybe_dot = self.params[-2]
            self.variadic = (maybe_dot.identifier == '.')
        if self.variadic:
            self.variadic_param = self.params[-1]
    """
        if self.identifier:
            return f'procedure:{self.identifier}({len(self.params)})'
        else:
            return f'procedure:lambda({len(self.params)})' """
    def update_closure_env(self, env: Env):
        for s in self.closure_symbols:
            frame = env.get_current_frame()
            if s in frame: #if this procedure's stack frame no longer exists (replaced by TCO), then we cannot be sure the symbol is here
                self.closure_env[s] = frame[s]
    def apply_closure_env(self, bindings: dict[str, SEXPR]):
        bindings.update(self.closure_env)
    def proc_exit(self, tail_call_optimized: bool, env: Env):
        self.update_closure_env(env)
        #only pop the frame if it belongs to the function originally 
        #(otherwise we pop more than we push! too many!)
        if not tail_call_optimized:
            env.pop_stack_frame()
    def apply(self, args: list[SEXPR], env: Env, tail_call_optimized = False):
        current_bindings = {}
        if not self.variadic:
            current_bindings = {p.identifier:a for p,a in zip(self.params, args)}
        else:
            non_variadic_params = self.params[:-2]
            non_variadic_arg_length = len(non_variadic_params)
            current_bindings = {p.identifier:a for p,a in zip(non_variadic_params, args)}
            variadic_args = args[non_variadic_arg_length:]
            vargs = List(variadic_args)
            current_bindings[self.variadic_param.identifier] = vargs if not self.macro else List([Symbol('quote'), vargs])
        self.apply_closure_env(current_bindings)
        
        #env.push_stack_frame(current_bindings)
        #last_val = None
        #body = self.body
        #for b in body:
        #    last_val = b.evaluate(env)
        #self.proc_exit(False, env)
        #return last_val if not self.macro else EvalProc().apply([last_val], env)

        if not tail_call_optimized or len(env.stack)==0:
            env.push_stack_frame(current_bindings)
        else:
            env.replace_stack_frame(current_bindings)
        
        body = self.body
        for b in body[:-1]:
            b.evaluate(env)
        tail = body[-1]
        
        is_tail_call = isinstance(tail, List) and len(tail.children)>0
        tail_func = tail.children[0].evaluate(env) if is_tail_call else None
        is_tail_call = is_tail_call and isinstance(tail_func, Procedure) and not tail_func.special_form
        
        last_val = None
        if is_tail_call:
            tail_args = [a.evaluate(env) for a in tail.children[1:]]
            how_many_args = len(inspect.signature(tail_func.apply).parameters)
            accepts_tail_call_arg = how_many_args == 3
            last_val = tail_func.apply(tail_args, env, True) if accepts_tail_call_arg else tail_func.apply(tail_args, env)
        else:
            last_val = tail.evaluate(env)
        self.proc_exit(tail_call_optimized, env)
        
        return last_val if not self.macro else EvalProc().apply([last_val], env)
    
class AddProc(Procedure):
    def __init__(self):
        self.identifier = '+'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        num = Number(0)
        for a in args:
            if not isinstance(a, Number):
                raise Exception("Procedure '+' expected numbers as arg")
            num.num += a.num
        return num
class MulProc(Procedure):
    def __init__(self):
        self.identifier = '*'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        num = Number(1)
        for a in args:
            if not isinstance(a, Number):
                raise Exception("Procedure '*' expected numbers as arg")
            num.num *= a.num
        return num
class ExptProc(Procedure):
    def __init__(self):
        self.identifier = 'expt'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=2:
            raise Exception('expt requires exactly 2 arguments')
        base: Number = args[0]
        power: Number = args[1]
        if (not isinstance(base, Number)) or (not isinstance(power, Number)):
            raise Exception('expt arguments must be numbers')
        return Number((base.num)**(power.num))
class ModuloProc(Procedure):
    def __init__(self):
        self.identifier = 'modulo'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=2:
            raise Exception("modulo requires exactly 2 arguments")
        num1: Number = args[0]
        num2: Number = args[1]
        if (not isinstance(num1, Number)) or (not isinstance(num2, Number)):
            raise Exception("modulo arguments must be numbers")
        num = Number(num1.num % num2.num)
        return num
class IsStringProc(Procedure):
    def __init__(self):
        self.identifier = 'string?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("string? requires exactly 2 arguments")
        arg = args[0]
        return Bool(isinstance(arg, String))
class IsCharProc(Procedure):
    def __init__(self):
        self.identifier = 'char?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("char? requires exactly 2 arguments")
        arg = args[0]
        return Bool(isinstance(arg, Char))
class IsBoolProc(Procedure):
    def __init__(self):
        self.identifier = 'bool?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("bool? requires exactly 2 arguments")
        arg = args[0]
        return Bool(isinstance(arg, Bool))
class IsNumberProc(Procedure):
    def __init__(self):
        self.identifier = 'number?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("number? requires exactly 2 arguments")
        arg = args[0]
        return Bool(isinstance(arg, Number))
class IsListProc(Procedure):
    def __init__(self):
        self.identifier = 'list?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("list? requires exactly 2 arguments")
        arg = args[0]
        return Bool(isinstance(arg, List))
class IsPairProc(Procedure):
    def __init__(self):
        self.identifier = 'pair?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("pair? requires exactly 2 arguments")
        arg = args[0]
        return Bool(isinstance(arg, List) and len(arg.children)>0)
class MinusProc(Procedure):
    def __init__(self):
        self.identifier = '-'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)==0:
            raise Exception("Expected at least one argument")
        if not isinstance(args[0], Number):
            raise Exception("Procedure '-' expected numbers as arg")
        num = deepcopy(args[0])
        if len(args) == 1:
            num.num *= -1
            return num
        for a in args[1:]:
            if not isinstance(a, Number):
                raise Exception("Procedure '-' expected numbers as arg")
            num.num -= a.num
        return num
class SmallerThanProc(Procedure):
    def __init__(self):
        self.identifier = '<'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)<2:
            raise Exception("< needs at least two arguments")
        for a in args:
            if not isinstance(a, Number):
                raise Exception("< expects numbers as args")
        for a0,a1 in zip(args, args[1:]):
            if not a0.num < a1.num:
                return Bool(False)
        return Bool(True)
class BiggerThanProc(Procedure):
    def __init__(self):
        self.identifier = '>'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)<2:
            raise Exception("> needs at least two arguments")
        for a in args:
            if not isinstance(a, Number):
                raise Exception("> expects numbers as args")
        for a0,a1 in zip(args, args[1:]):
            if not a0.num > a1.num:
                return Bool(False)
        return Bool(True)
class DivProc(Procedure):
    def __init__(self):
        self.identifier = '/'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)==0:
            raise Exception("Expected at least one argument")
        if not isinstance(args[0], Number):
            raise Exception("Procedure '/' expected numbers as arg")
        num = deepcopy(args[0])
        if len(args) == 1:
            num.num = (1/num.num)
            return num
        for a in args[1:]:
            if not isinstance(a, Number):
                raise Exception("Procedure '/' expected numbers as arg")
            num.num /= a.num
        return num
class WriteProc(Procedure):
    def __init__(self):
        self.identifier = 'display'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("Display procedure expects exactly 1 argument")
        toDisplay = args[0]
        val = print_scheme(toDisplay)
        print(val, end='')
        return None
class DefineProc(Procedure):
    def __init__(self, overwrite = False, macro_definition = False):
        self.identifier = 'define'
        self.macro_definition = macro_definition
        self.overwrite = overwrite
        self.set_attributes(True, False, False)
    def undo_procedure_syntatic_sugar(self, args: list[SEXPR]):
        function_signature: List = args[0]
        if len(function_signature.children) == 0:
            raise Exception('define procedure needs an identifier')
        func_identifier: Symbol = function_signature.children[0]
        if not isinstance(func_identifier, Symbol):
            raise Exception('define procedure\'s first argument must be a list that has a symbol to serve as an identifier as first element')
            
        param_list: list[SEXPR] = function_signature.children[1:]
        body: list[SEXPR] = args[1:]
        lambda_call: list[SEXPR] = [Symbol('lambda'), List(param_list)]
        lambda_call.extend(body)
        return [func_identifier, List(lambda_call)]
    def apply(self, args: list[SEXPR], env: Env):
        symb = args[0]

        #Treat for procedure definitions
        if isinstance(symb, List):
            args = self.undo_procedure_syntatic_sugar(args)
            symb = args[0]
        
        if not isinstance(symb, Symbol):
            raise Exception('define first argument must be a symbol')
        if len(args)!=2:
            raise Exception("Define expects exactly 2 arguments")
        val = args[1].evaluate(env)
        if isinstance(val, Procedure):
            val: Procedure = val
            val.macro = self.macro_definition
            val.identifier = symb.identifier
            if len(env.stack)>0:
                val.closure_symbols.add(symb.identifier)
                val.closure_env[symb.identifier] = val
        if len(env.stack)==0:
            if symb.identifier not in env.symbols or self.overwrite:
                env.symbols[symb.identifier] = val
            else:
                raise Exception('Cannot redefine a variable (use "set!")')
        else:
            if symb.identifier not in env.stack[-1] or self.overwrite:
                env.get_current_frame()[symb.identifier] = val
            else:
                raise Exception('Cannot redefine a variable (use "set!")')
        return None
class IfProc(Procedure):
    def __init__(self):
        self.identifier = 'if'
        self.set_attributes(True, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)<2:
            raise Exception("If expects at least 2 arguments (predicate and then clause)")
        if len(args)>3:
            raise Exception("If expects at most 3 arguments (predicate, then clause and else clause)")
        pred = args[0]
        then = args[1]
        result = pred.evaluate(env)
        if not isinstance(result, Bool) or result.val:
            return then.evaluate(env)
        if len(args)==3:
            else_clause = args[2]
            return else_clause.evaluate(env)
        return None
class CondProc(Procedure):
    def __init__(self):
        self.identifier = 'cond'
        self.set_attributes(True, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)==0:
            raise Exception("cond expects at least 1 argument")
        for a in args:
            if not isinstance(a, List):
                raise Exception("cond arguments must be lists")
            l: List = a
            if len(l.children) < 2:
                raise Exception("cond arguments must be lists with at least two elements (predicate and clause)")
            pred = l.children[0]
            clause = l.children[1:]
            pred_is_else = isinstance(pred, Symbol) and pred.identifier == 'else'
            result = pred.evaluate(env) if not pred_is_else else None
            if pred_is_else or not isinstance(result, Bool) or result.val:
                last_val = None
                #return BeginProc().apply(clause, env) also works
                for c in clause:
                    last_val = c.evaluate(env)
                return last_val
        return None
class EqualProc(Procedure):
    def __init__(self):
        self.identifier = 'equal?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args) != 2:
            raise Exception("equal? requires exactly 2 arguments")
        v1 = args[0]
        v2 = args[1]
        return Bool(v1 == v2)
class LambdaProc(Procedure):
    def __init__(self):
        self.identifier = 'lambda'
        self.set_attributes(True, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args) < 2:
            raise Exception("Lambda needs at least 2 args: a parameter list and a body expression")
        param_list: List = args[0]
        if not isinstance(param_list, List):
            raise Exception("Lambda first argument needs to be a list (a parameter list)")
        param_list: list[SEXPR] = param_list.children
        for p in param_list:
            if not isinstance(p, Symbol):
                raise Exception("Only symbols are allowed in a lambda parameter list")
        body = args[1:]
        
        proc = Procedure(param_list, body, 'lambda')
        """
        if the lambda is being called in a local scope (procedure being created inside another procedure call),
        if the body contains a symbol that is defined in the immediate local scope and that is not in the parameter list, then
        that symbol should be in the closure_env, holding the current value that is in that local scope.
        That is how we implement closures.
        """
        if env.stack_not_empty():
            current_frame = env.get_current_frame()
            param_list_symbols = [s.identifier for s in param_list]
            for symbol_id in current_frame:
                for b in body:
                    if b.has_symbol(symbol_id) and symbol_id not in param_list_symbols:
                        proc.closure_symbols.add(symbol_id)
                        proc.closure_env[symbol_id] = current_frame[symbol_id]

        return proc
class ExitProc(Procedure):
    def __init__(self):
        self.identifier = 'exit'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        exit(0)
class SymbolEqualProc(Procedure):
    def __init__(self):
        self.identifier = '='
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)==0:
            raise Exception("= Needs at least 1 argument")
        val = Bool(True)          
        for a in args:
            if not isinstance(a, Number):
                raise Exception('= arguments need to be numbers')
        if len(args)==1:
            return val
        og_num: Number = args[0]
        for a in args[1:]:
            if og_num.num != a.num:
                val.val = False
                return val
        return val
class NotProc(Procedure):
    def __init__(self):
        self.identifier = 'not'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("Not needs exactly 1 argument")
        arg = args[0]
        if not isinstance(arg, Bool):
            return Bool(False)
        arg: Bool = arg
        return Bool(not arg.val)
class NewLineProc(Procedure):
    def __init__(self):
        self.identifier = 'neline'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=0:
            raise Exception('newline does not receive any argument')
        print('')
class QuoteProc(Procedure):
    def __init__(self):
        self.identifier = 'quote'
        self.set_attributes(True, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception('quote only receives 1 argument')
        arg = args[0]
        return arg
class QuasiquoteProc(Procedure):
    def __init__(self):
        self.identifier = 'quasiquote'
        self.set_attributes(True, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception('quote only receives 1 argument')
        arg = args[0]
        return arg.partial_evaluate(env)
class EvalProc(Procedure):
    def __init__(self):
        self.identifier = 'eval'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception('eval receives exactly 1 argument')
        arg: List = args[0]
        if not isinstance(arg, List):
            return arg.evaluate(env)
        if len(arg.children)==0:
            raise Exception('cannot eval empty list')
        proc: Procedure = arg.children[0].evaluate(env)
        if not isinstance(proc, Procedure):
            raise Exception('first entry in the list is not a procedure')
        if proc.identifier != 'quote':
            return arg.evaluate(env)
        quoted_arg = arg.children[1]
        return quoted_arg.evaluate(env)
class CarProc(Procedure):
    def __init__(self):
        self.identifier = 'car'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception('car only receives 1 argument')
        arg: List = args[0]
        if not isinstance(arg, List):
            raise Exception('car receives a list as an argument')
        if len(arg.children)==0:
            raise Exception('cannot apply car procedure to empty list')
        car = arg.children[0]
        return car
class CdrProc(Procedure):
    def __init__(self):
        self.identifier = 'cdr'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception('cdr only receives 1 argument')
        arg: List = args[0]
        if not isinstance(arg, List):
            raise Exception('cdr receives a list as an argument')
        if len(arg.children)==0:
            raise Exception('cannot apply cdr procedure to empty list')
        val = List(arg.children[1:])
        return val
class StringAppendProc(Procedure):
    def __init__(self):
        self.identifier = 'string-append'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        val = String('')
        for a in args:
            if not isinstance(a, String):
                raise Exception("string-append arguments all need to be strings")
            val.text += a.text
        return val
class StringEqualProc(Procedure):
    def __init__(self):
        self.identifier = 'string=?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)==0:
            raise Exception('string=? expects at least 2 arguments')
        for a in args:
            if not isinstance(a, String):
                raise Exception("string=? arguments all need to be strings")
        for a0,a1 in zip(args, args[1:]):
            if a0.text != a1.text:
                return Bool(False)
        return Bool(True)
class CharEqualProc(Procedure):
    def __init__(self):
        self.identifier = 'char=?'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)==0:
            raise Exception('char=? expects at least 2 arguments')
        for a in args:
            if not isinstance(a, Char):
                raise Exception("char=? arguments all need to be characters")
        for a0,a1 in zip(args, args[1:]):
            if a0.char != a1.char:
                return Bool(False)
        return Bool(True)
class ConsProc(Procedure):
    def __init__(self):
        self.identifier = 'cons'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=2:
            raise Exception("cons require exactly 2 arguments")
        car = args[0]
        cdr = args[1]
        
        if isinstance(cdr, List):
            cdr_copy: List = deepcopy(cdr)
            cdr_copy.children.insert(0, car)
            return cdr_copy
        else:
            return List([car, cdr], True)
class AppendProc(Procedure):
    def __init__(self):
        self.identifier = 'append'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        appended_lists = List([])
        for a in args:
            if not isinstance(a, List):
                raise Exception("append arguments all need to be lists")
            appended_lists.children.extend(a.children)
        return appended_lists
class ReverseProc(Procedure):
    def __init__(self):
        self.identifier = 'reverse'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception('reverse requires exacly 1 argument')
        ls: List = args[0]
        if not isinstance(ls, List):
            raise Exception('reverse argument needs to be a list')
        return List(list(reversed(ls.children)))
class StringLengthProc(Procedure):
    def __init__(self):
        self.identifier = 'string-length'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("string-length requires exactly 1 argument")
        arg = args[0]
        if not isinstance(arg, String):
            raise Exception("string-length argument needs to be a string")
        return Number(len(arg.text))
class StringRefProc(Procedure):
    def __init__(self):
        self.identifier = 'string-ref'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=2:
            raise Exception("string-length requires exactly 2 arguments")
        s: String = args[0]
        idx: Number = args[1]
        if not isinstance(s, String):
            raise Exception("string-ref first argument needs to be a string")
        if not isinstance(idx, Number):
            raise Exception("string-ref second argument needs to be a number")
        return Char(s.text[int(idx.num)])
class ListProc(Procedure):
    def __init__(self):
        self.identifier = 'list'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        return List(args)
class BeginProc(Procedure):
    def __init__(self):
        self.identifier = 'begin'
        self.set_attributes(True, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        last_val = None
        for a in args:
            last_val = a.evaluate(env)
        return last_val
class ListRefProc(Procedure):
    def __init__(self):
        self.identifier = 'list-ref'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=2:
            raise Exception('list-ref requires exactly 2 arguments (a list and an index)')
        ls: List = args[0]
        idx: Number = args[1]
        if not isinstance(ls, List):
            raise Exception('list-ref first argument needs to be a list')
        if not isinstance(idx, Number):
            raise Exception('list-ref second argument needs to be a number')
        return ls.children[idx.num]
class LengthProc(Procedure):
    def __init__(self):
        self.identifier = 'length'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("length requires exactly 1 argument")
        arg = args[0]
        if not isinstance(arg, List):
            raise Exception("length argument needs to be a list")
        return Number(len(arg.children))
class StringToListProc(Procedure):
    def __init__(self):
        self.identifier = 'string->list'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("string->list requires exactly 1 argument")
        arg: String = args[0]
        if not isinstance(arg, String):
            raise Exception("string->list argument needs to be a string")
        char_list = List([Char(c) for c in arg.text])
        return char_list
class StringUpcaseProc(Procedure):
    def __init__(self):
        self.identifier = 'string-upcase'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("string-upcase requires exactly 1 argument")
        arg: String = args[0]
        if not isinstance(arg, String):
            raise Exception("string-upcase argument needs to be a string")
        return String(arg.text.upper())
class StringDowncaseProc(Procedure):
    def __init__(self):
        self.identifier = 'string-downcase'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("string-downcase requires exactly 1 argument")
        arg: String = args[0]
        if not isinstance(arg, String):
            raise Exception("string-downcase argument needs to be a string")
        return String(arg.text.lower())
class CharUpcaseProc(Procedure):
    def __init__(self):
        self.identifier = 'char-upcase'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("char-upcase requires exactly 1 argument")
        arg: Char = args[0]
        if not isinstance(arg, Char):
            raise Exception("char-upcase argument needs to be a character")
        return Char(arg.char.upper())
class CharDowncaseProc(Procedure):
    def __init__(self):
        self.identifier = 'char-upcase'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("char-upcase requires exactly 1 argument")
        arg: Char = args[0]
        if not isinstance(arg, Char):
            raise Exception("char-upcase argument needs to be a character")
        return Char(arg.char.lower())
class StringToNumberProc(Procedure):
    def __init__(self):
        self.identifier = 'string->number'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("string->number requires exactly 1 argument")
        arg: String = args[0]
        if not isinstance(arg, String):
            raise Exception("string->number argument needs to be a string")
        text = arg.text
        if bool(re.match(r"^-?\d+(.\d+)?$", text)):
            is_int = bool(re.match(r"^-?\d+$", text))
            return Number(int(text) if is_int else float(text))
        else:
            raise Exception("string->number cannot convert non-number string")
class NumberToStringProc(Procedure):
    def __init__(self):
        self.identifier = 'number->string'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("number->string requires exactly 1 argument")
        arg: Number = args[0]
        if not isinstance(arg, Number):
            raise Exception("number->string argument needs to be a number")
        return String(str(arg.num))
class IntToCharProc(Procedure):
    def __init__(self):
        self.identifier = 'integer->char'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("integer->char requires exactly 1 argument")
        arg: Number = args[0]
        if not isinstance(arg, Number):
            raise Exception("integer->char argument needs to be a number")
        return Char(chr(arg.num))
class CharToIntProc(Procedure):
    def __init__(self):
        self.identifier = 'char->integer'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("char->integer requires exactly 1 argument")
        arg: Char = args[0]
        if not isinstance(arg, Char):
            raise Exception("char->integer argument needs to be a character")
        return Number(ord(arg.char))
class LoadProc(Procedure):
    def __init__(self):
        self.identifier = 'load'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=1:
            raise Exception("load requires exactly 1 argument")
        arg: String = args[0]
        if not isinstance(arg, String):
            raise Exception("load argument needs to be a string")
        fpath = arg.text
        with open(fpath, 'r') as file:
            content = file.read().strip()
            content = f'(begin {content})'
            evaluate(read(content), env)
        return None
class VoidProc(Procedure):
    def __init__(self):
        self.identifier = 'void'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)!=0:
            raise Exception("void requires exactly 0 arguments")
        return None
class ApplyProc(Procedure):
    def __init__(self):
        self.identifier = 'apply'
        self.set_attributes(False, False, False)
    def apply(self, args: list[SEXPR], env: Env):
        if len(args)<2:
            raise Exception("apply requires at least 2 arguments")
        func: Procedure = args[0]
        if not isinstance(func, Procedure):
            raise Exception('apply first argument must be a procedure')
        ls: List = args[-1]
        if not isinstance(ls, List):
            raise Exception('apply last argument must be a list')
        rest: list[SEXPR] = args[1:-1]
        to_apply = rest
        to_apply.extend(ls.children)
        return func.apply(to_apply, env)

repl_env = Env.make_repl_env()

def read(text: str):
    def tokenize2(text: str):
        tokens = []
        inside_string = False
        i = 0
        l = len(text)
        acc = ""
        while i<l:
            t = text[i]
            if t.isspace():
                if inside_string:
                    acc+=t
                elif acc != '':
                    tokens.append(acc)
                    acc = ''
                i+=1
                continue
            if t in ['(', ')', '[', ']'] and not inside_string:
                if acc!='':
                    tokens.append(acc)
                    acc = ''
                if not inside_string:
                    tokens.append(t)
                else:
                    acc+=t
                i+=1
                continue
            if t == "'" or t == '`' and not inside_string:
                tokens.append(t)
                i+=1
                continue
            if t == ',' and not inside_string:
                if text[i+1]=='@':
                    tokens.append(text[i:i+2])
                    i+=2
                else:
                    tokens.append(t)
                    i+=1
                continue
            if t=='.' and not inside_string and not text[i-1].isdigit() and not text[i+1].isdigit():
                tokens.append(t)
                i+=1
                continue
            if t=='"':
                acc+=t
                if inside_string:
                    inside_string=False
                    tokens.append(acc)
                    acc = ''
                else:
                    inside_string=True
                i+=1
                continue
            acc+=t
            i+=1
        if acc!='':
            tokens.append(acc)
        return tokens
                    
    def tokenize(text: str):
        tokens = [t for t in re.split(r'\s+', text) if t!='']
        tokens2 = []
        for t in tokens:
            result = [r for r in re.split(r'([\(\)\[\]])', t) if r!='']
            tokens2.extend(result)
        return tokens2
    def read_list(tokens: list[str]):
        beginning_token = tokens[0]
        end_token = ')' if beginning_token == '(' else ']'
        ls = []
        
        tokens_read_total = 1
        i = 1
        valid_list = False
        while i<len(tokens):
            if tokens[i] == end_token:
                tokens_read_total += 1
                valid_list = True
                break
            form, tokens_read = read_form(tokens[i:])
            if form is None:
                break
            ls.append(form)
            tokens_read_total += tokens_read
            i += tokens_read

        return (List(ls) if valid_list else None, tokens_read_total)
    def read_atom(tokens: list[str]):
        token = tokens[0]
        if bool(re.match(r"^-?\d+(.\d+)?$", token)):
            is_int = bool(re.match(r"^-?\d+$", token))
            return (Number(int(token) if is_int else float(token)), 1)
        elif token[0]=='"' and token[-1]=='"':
            return (String(token[1:-1]), 1)
        elif len(token) == 2 and token[0] == '#' and (token[1] == 't' or token[1] == 'f'):
            return (Bool(token[1] == 't'), 1)
        elif token[0] == '#' and token[1]=='\\':
            return (Char(token[2:]), 1)
        else:
            return (Symbol(token), 1)
    def read_quote(tokens: list[str]):
        assert tokens[0] == "'"
        ast, tokens_read = read_form(tokens[1:])
        new_ast = List([Symbol('quote'), ast])
        return (new_ast, tokens_read+1)
    def read_quasiquote(tokens: list[str]):
        assert tokens[0] == "`"
        ast, tokens_read = read_form(tokens[1:])
        new_ast = List([Symbol('quasiquote'), ast])
        return (new_ast, tokens_read+1)
    def read_comma(tokens: list[str]):
        assert tokens[0] == ","
        ast, tokens_read = read_form(tokens[1:])
        new_ast = List([Symbol('comma'), ast])
        return (new_ast, tokens_read+1)
    def read_form(tokens: list[str]):
        if len(tokens)==0:
            return (None,0)
        if tokens[0]=='(' or tokens[0]=='[':
            return read_list(tokens)
        if tokens[0]=="'":
            return read_quote(tokens)
        if tokens[0]=="`":
            return read_quasiquote(tokens)
        if tokens[0]==",":
            return read_comma(tokens)
        return read_atom(tokens)
    
    tokens = tokenize2(text)
    ast = read_form(tokens)[0]
    return ast

def evaluate(ast, env):
    return ast.evaluate(env)

def print_scheme(value):
    from io import StringIO
    
    def print_atom_ast(ast: Atom, output: StringIO):
        if isinstance(ast, Number):
            num: Number = ast
            output.write(str(num.num))
        elif isinstance(ast, Bool):
            b: Bool = ast
            output.write('#t' if b.val else '#f')
        elif isinstance(ast, Char):
            c: Char = ast
            output.write(f'#\\{c.char}')
        elif isinstance(ast, String):
            text: String = ast
            output.write(f'"{text.text}"')
        elif isinstance(ast, Symbol):
            s: Symbol = ast
            output.write(s.identifier)
    def print_list_ast(ast: List, output: StringIO):
        output.write('(')
        amount = len(ast.children)
        for idx,c in enumerate(ast.children):
            print_ast(c, output)
            if idx != amount - 1:
                output.write(' ')
            if ast.improper and idx == amount-2:
                output.write('. ')
        output.write(')')
    def print_ast(ast, output: StringIO):
        if ast is None:
            return
        if isinstance(ast, Atom):
            print_atom_ast(ast, output)
        elif isinstance(ast, Procedure):
            output.write(f'procedure:{ast.identifier}')
        elif isinstance(ast, List):
            print_list_ast(ast, output)
        
    output = StringIO()
    print_ast(value, output)
    value = output.getvalue()
    output.close()
    return value

commands = []
previous_is_complete_command = True

def get_current_command(commands, previous_is_complete_command: bool):
    def amount_of_digits(num):
        return len(str(abs(num)))
    text_input = ''
    if previous_is_complete_command:
        text_input = input(f'{len(commands)}> ')
    else:
        identation_spaces = '  ' + ''.join([' ' for _ in range(amount_of_digits((len(commands))))]) #One to account for the ">", the others to account for the amount of commands already executed
        new_input = input(identation_spaces).strip()
        text_input = commands[-1] + ' ' + new_input
    return text_input

def rep(command = '', dont_store_command = False):
    global previous_is_complete_command
    text_input = get_current_command(commands, previous_is_complete_command) if not command else command
    ast = read(text_input)

    if ast is not None or len(commands) == 0:
        if previous_is_complete_command:
            commands.append(text_input)
        else:
            commands[-1] = text_input
    elif not previous_is_complete_command:
        commands[-1] = text_input
    else:
        commands.append(text_input)

    if ast is None:
        previous_is_complete_command = False
        return
    previous_is_complete_command = True
    
    v = evaluate(ast, repl_env)
    print(print_scheme(v))

def load_lib(lib_path: str):
    global commands
    evaluate(read(f'(load "{lib_path}")'), repl_env)
    commands = []

def main():
    load_lib("std_lib.scm")
    load_lib("test.scm")
    while True:
        try:
            rep()
        except Exception as e:
            print(f'Exception occurred: {e}') 
            
    print("unreachable code yay!")
    
if __name__=="__main__":
   main()