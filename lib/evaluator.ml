module Evaluator = struct
include Object
include Ast

let evalPrefixExpression op right = match op with
| "!" -> (match right with
  | Object.Boolean boolean -> Object.Boolean (Bool.not boolean)
  | Object.Null -> Object.Boolean true
  | _ -> Object.Boolean false)
| "-" -> (match right with
  | Object.Integer i -> (Object.Integer (- i))
  | ob -> Object.Err ("unknown operator: -" ^ Object.objToString ob))
| _ -> Object.Err (Printf.sprintf "unknow operator: %s %s"  op (Object.objToString right))

let evalIntegerInfixExpression op left right = match op with
| "+" -> Object.Integer (left + right)
| "-" -> Object.Integer (left - right)
| "*" -> Object.Integer (left * right)
| "/" -> Object.Integer (left / right)
| "<" -> Object.Boolean (left < right)
| ">" -> Object.Boolean (left > right)
| "==" -> Object.Boolean (left = right)
| "!=" -> Object.Boolean (left != right)
| _ -> Object.Err (Printf.sprintf "unknown operator: %s %s %s" (string_of_int left) op (string_of_int right))

let evalStringInfixExpression op left right = match op with
| "+" -> Object.Strng (left ^ right)
| _ -> Object.Err (Printf.sprintf "unknown operator: %s %s %s" op left right)

let evalInfixExpression op left right = match (left, right) with
| (Object.Integer l, Object.Integer r) -> evalIntegerInfixExpression op l r
| (Object.Strng l, Object.Strng r) -> evalStringInfixExpression op l r
| (l, r) -> if Object.typeEq l r
  then (match op with
  | "==" -> Object.Boolean (Object.eq left right)
  | "!=" -> Object.Boolean (Object.eq left right |> Bool.not)
  | _ -> Object.Err (Printf.sprintf "unknown operator: %s %s %s" (Object.objToString l) op (Object.objToString r)))
  else Object.Err (Printf.sprintf "type mismatch: %s %s %s" (Object.objToString l) op (Object.objToString r))


let rec evalExpression exp env = match exp with
| Ast.IntegerLiteral i -> (Object.Integer i, env)
| Ast.StringLiteral i -> (Object.Strng i, env)
| Ast.Identifier i -> (Object.Err i, env)
| Ast.BooleanLiteral i -> (Object.Boolean i, env)
| Ast.PrefixExpression {op; right;} -> let (r, ev) = evalExpression right env
  in (evalPrefixExpression op r, ev)
| Ast.InfixExpression {op; left; right;} -> let (l, ev) = evalExpression left env
  in let (r, e) = evalExpression right ev
  in (evalInfixExpression op l r, e)
| Ast.IfExpression _ -> (Object.Err "hoge", env)
| Ast.FunctionLiteral i -> (Object.Func {prms = i.prms; body = i.body;}, env)
| Ast.ArrayLiteral _ -> (Object.Err "hoge", env)
| Ast.CallExpression _ -> (Object.Err "hoge", env)
| Ast.IndexExpression _ -> (Object.Err "hoge", env)

let evalStatement stm env = match stm with
| Ast.ExpressionStatement i -> evalExpression i.exp env
| Ast.ReturnStatement _ -> (Object.Err "ret", env)
| Ast.LetStatment _ -> (Object.Err "let", env)
| Ast.BlockStatement _ -> (Object.Err "bl", env)

let evalProgram (program:Ast.program) env = let rec revs slist ev = match slist with
  | [] -> (Object.Empty, ev)
  | h::[] -> evalStatement h ev
  | h::t -> match evalStatement h ev with
    | (Object.Err i, e) -> (Object.Err i, e)
    | (_, e) -> revs t e
in match revs program.statements env with
| (ob, _) -> ob

end
