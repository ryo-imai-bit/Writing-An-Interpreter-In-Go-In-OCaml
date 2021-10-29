module Ast = struct
  include Token
  type expression =
  | IntegerLiteral of int
  | StringLiteral of string
  | Identifier of string
  | PrefixExpression of {op: string; right: expression}
  | InfixExpression of {op: string; left: expression; right: expression;}

  type statement =
  | LetStatment of {idt: expression; value: expression;}
  | ReturnStatement of {value: expression;}
  | ExpressionStatement of {exp: expression;}

  type program = {
    statements: statement list
  }

  let rec expToString = function
  | IntegerLiteral i -> "(INT " ^ string_of_int i ^ ") "
  | StringLiteral i -> "(STR " ^ i ^ ") "
  | Identifier i -> "(IDT " ^ i ^ ") "
  | PrefixExpression i -> "(PREFIX {op: " ^ i.op ^ " right:{" ^ expToString i.right ^ "}}) "
  | InfixExpression i -> "(INFIX {op: " ^ i.op ^ " left:{" ^ expToString i.left ^ "}"
    ^ " right:{" ^ expToString i.right ^ "}}) "

  let stmToString = function
  | LetStatment i -> "LET:" ^ expToString i.idt ^ " " ^ expToString i.value
  | ReturnStatement i -> "RET:" ^ expToString i.value
  | ExpressionStatement i -> "EXP:" ^ expToString i.exp

  let stmsToString stmts = let rec rrc = function
  | [] -> ""
  | h::t -> (stmToString h) ^ " " ^ rrc t
  in rrc stmts

  let eq a b = a = b
  let pp ppf st = Fmt.pf ppf "Statement = %s" (stmToString st)
end
