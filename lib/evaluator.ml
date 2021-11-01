module Evaluator = struct
include Object
include Ast

let evalExpression exp env = match exp with
| Ast.IntegerLiteral i -> (Object.Integer i, env)
| Ast.StringLiteral i -> (Object.Error i, env)
| Ast.Identifier i -> (Object.Error i, env)
| Ast.BooleanLiteral _ -> (Object.Error "hoge", env)
| Ast.PrefixExpression _ ->(Object.Error "hoge", env)
| Ast.InfixExpression _ -> (Object.Error "hoge", env)
| Ast.IfExpression _ -> (Object.Error "hoge", env)
| Ast.FunctionLiteral _ -> (Object.Error "hoge", env)
| Ast.ArrayLiteral _ -> (Object.Error "hoge", env)
| Ast.CallExpression _ -> (Object.Error "hoge", env)
| Ast.IndexExpression _ -> (Object.Error "hoge", env)

let evalStatement stm env = match stm with
| Ast.ExpressionStatement i -> evalExpression i.exp env
| Ast.ReturnStatement _ -> (Object.Error "ret", env)
| Ast.LetStatment _ -> (Object.Error "let", env)
| Ast.BlockStatement _ -> (Object.Error "bl", env)

let evalProgram (program:Ast.program) env = let rec revs slist ev = match slist with
  | [] -> (Object.Empty, ev)
  | h::[] -> evalStatement h ev
  | h::t -> match evalStatement h ev with
    | (Object.Error i, e) -> (Object.Error i, e)
    | (_, e) -> revs t e
in match revs program.statements env with
| (ob, _) -> ob

end
