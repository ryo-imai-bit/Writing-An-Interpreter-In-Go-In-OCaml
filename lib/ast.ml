module Ast = struct
  include Token
  type expression =
  | IntegerLiteral of int
  | StringLiteral of string
  | Identifier of string
  | PrefixExpression of {op: string; right: expression}
  | InfixExpression of {tok: Token.token; op: string; left: expression; right: expression;}

  type statement =
  | LetStatment of {idt: expression; value: expression}
  | ReturnStatement
  | ExpressionStatement

  type program = {
    statements: statement list
  }

  let rec expToString = function
  | IntegerLiteral i -> "(int:" ^ string_of_int i ^ ") "
  | StringLiteral i -> "(str:" ^ i ^ ") "
  | Identifier i -> "(idt:" ^ i ^ ") "
  | PrefixExpression i -> "(PREFIX {op: " ^ i.op ^ " right:{" ^ expToString i.right ^ "}}) "
  | InfixExpression i -> "(INFIX {op: " ^ i.op ^ " tok: " ^ Token.tokenToString i.tok
    ^ " left:{" ^ expToString i.left ^ "}"
    ^ " right:{" ^ expToString i.right ^ "}}) "

  let stmToString = function
  | LetStatment i -> "let:" ^ expToString i.idt ^ " " ^ expToString i.value
  | ReturnStatement -> "ret:"
  | ExpressionStatement -> "exp:"

  let stmsToString stmts = let rec rrc = function
  | [] -> ""
  | h::t -> (stmToString h) ^ " " ^ rrc t
  in rrc stmts

  let eq a b = a = b
  let pp ppf st = Fmt.pf ppf "Statement = %s" (stmToString st)
end
