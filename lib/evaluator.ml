module Evaluator = struct
include Object
include Ast
include Env

let isTruthy = function
| Object.Boolean boolean -> boolean
| Object.Null -> false
| _ -> true

let evalPrefixExpression op right = match op with
| "!" -> Object.Boolean (Bool.not (isTruthy right))
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

let extendFunctionEnv prms args env = let nenv = Env.newEnclosedEnv env
in let rec refe pms ags = match pms, ags with
  | ([], []) -> ()
  | ((Ast.Identifier idt)::it, obj::ot) -> let _ = Env.set nenv idt obj in refe it ot
  | _, _ -> raise (Failure "err env")
in refe prms args; nenv

let rec evalExpression exp env = match exp with
| Ast.IntegerLiteral i -> (Object.Integer i, env)
| Ast.StringLiteral i -> (Object.Strng i, env)
| Ast.Identifier i -> (match Env.get env i with
  | Some v -> (v, env)
  | None -> (Object.Err ("unbound identifier" ^ i), env))
| Ast.BooleanLiteral i -> (Object.Boolean i, env)
| Ast.PrefixExpression {op; right;} -> let (r, ev) = evalExpression right env
  in (evalPrefixExpression op r, ev)
| Ast.InfixExpression {op; left; right;} -> let (l, ev) = evalExpression left env
  in let (r, e) = evalExpression right ev
  in (evalInfixExpression op l r, e)
| Ast.IfExpression i -> evalIfExpression i.cond i.cons i.alt env
| Ast.FunctionLiteral i -> (Object.Func {prms = i.prms; body = i.body;}, env)
(* | Ast.CallExpression _ -> (Object.Err "hoge", env) *)
| Ast.CallExpression i -> (match evalExpression i.fn env with
  | Object.Err i, ev -> (Object.Err i, ev)
  | Object.Func fn, ev -> (match evalExpressions fn.prms ev with
    | [Object.Err i], e -> (Object.Err i, e)
    | args, e ->  (match fn.body with
      | Ast.BlockStatement stm -> (applyFunction fn.prms stm.stms args e, e)
      | bd -> (Object.Err ("expected BlockStatement got " ^ Ast.stmToString bd), e)))
  | exp, ev -> (Object.Err (Object.objToString exp), ev))
| Ast.ArrayLiteral _ -> (Object.Err "hoge", env)
| Ast.IndexExpression _ -> (Object.Err "hoge", env)

and evalIfExpression cond cons alt env = match evalExpression cond env with
  | (Object.Err i, ev) -> (Object.Err i, ev)
  | (obj, ev) -> if isTruthy obj
    then match cons with
      | Ast.BlockStatement stm -> evalBlockStatement stm.stms ev
      | _ -> (Object.Err "expected BlockStatement", ev)
    else (match alt with
      | Some (Ast.BlockStatement stm) -> evalBlockStatement stm.stms ev
      | Some _ -> (Object.Err "expected BlockStatement", ev)
      | None -> (Object.Null, ev))

and evalExpressions exps env = let rec ree exps ev = match exps with
  | [] -> ([], ev)
  | h::t -> match evalExpression h ev with
    | (Object.Err i, _) -> raise (Failure i)
    | (obj, e) -> let (ob, envr) = ree t e in (obj::ob, envr)
in try ree exps env with
| Failure i -> ([Object.Err i], env)
| _ -> ([Object.Err "unknown error"], env)

and applyFunction prms body args env = let nenv = extendFunctionEnv prms args env in
match evalBlockStatement body nenv with
| (Object.ReturnValue i, _) -> i
| (obj, _) -> obj

and evalStatement stm env = match stm with
| Ast.ExpressionStatement i -> evalExpression i.exp env
| Ast.BlockStatement i -> evalBlockStatement i.stms env
| Ast.ReturnStatement i -> evalReturnStatemen i.value env
| Ast.LetStatment i -> evalLetStatement i.idt i.value env

and evalBlockStatement (blstm: Ast.statement list) env = let rec rebs slist ev = match slist with
  | [] -> (Object.Empty, ev)
  | h::[] -> evalStatement h ev
  | h::t -> match evalStatement h ev with
    | (Object.Err i, e) -> (Object.Err i, e)
    | (Object.ReturnValue i, e) -> (i, e)
    | (_, e) -> rebs t e
in match rebs blstm env with
| (ob, e) -> (ob, e)

and evalReturnStatemen exp env = (match evalExpression exp env with
  | (Object.Err i, ev) -> (Object.Err i, ev)
  | (obj, ev) -> (Object.ReturnValue obj, ev))

and evalLetStatement idt value env = match idt with
  | Ast.Identifier ident -> (match evalExpression value env with
    | (Object.Err i, ev) -> (Object.Err i, ev)
    | (obj, ev) -> (Env.set ev ident obj, ev))
  | node -> (Object.Err ("expected IDENTIFIER got " ^ (Ast.expToString node)), env)

let evalProgram (program:Ast.program) env = let rec revs slist ev = match slist with
  | [] -> (Object.Empty, ev)
  | h::[] -> (match evalStatement h ev with
    | (Object.ReturnValue i, e) -> (i, e)
    | (obj, e) -> (obj, e))
  | h::t -> match evalStatement h ev with
    | (Object.ReturnValue i, e) -> (i, e)
    | (Object.Err i, e) -> (Object.Err i, e)
    | (_, e) -> revs t e
in match revs program.statements env with
| (ob, _) -> ob

end
