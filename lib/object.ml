module Object = struct
include Ast

  type obj =
  | Integer of int
  | Boolean of bool
  | Strng of string
  | ReturnValue of obj
  | Func of {prms: Ast.expression list; body: Ast.statement}
  | Builtin of (obj list -> obj)
  | Arry of obj list
  | Empty
  | Null
  | Err of string

  let newEnv = []

  let eq a b = match (a, b) with
  | Integer a, Integer b -> a = b
  | a, b -> a = b

  let rec objToString = function
  | Integer i -> "Integer (" ^ string_of_int i ^ ")"
  | Boolean i -> "Boolean (" ^ string_of_bool i  ^ ")"
  | Strng i -> "String (" ^ i ^ ")"
  | ReturnValue i -> "ReturnValue (" ^ objToString i ^ ")"
  | Func i -> "Function (prms " ^ Ast.expsToString i.prms ^ ", body " ^ Ast.stmToString i.body ^ ")"
  | Builtin _ -> "Builtin"
  | Arry i -> objsToString i
  | Empty -> "Empty"
  | Null -> "Null"
  | Err i -> i

  and objsToString = function
  | [] -> ""
  | h::t -> "[ " ^ objToString h ^ " ]" ^ (objsToString t)

  let pp ppf ob = Fmt.pf ppf "Object = %s" (objToString ob)

end
