module Ast = struct
  include Token
  type expression =
  | IntegerLiteral of int
  | StringLiteral of string
  | Identifier of string
  | BooleanLiteral of bool
  | PrefixExpression of {op: string; right: expression}
  | InfixExpression of {op: string; left: expression; right: expression;}
  | IfExpression of {cond: expression; cons: statement; alt: statement option;}
  | FunctionLiteral of {prms: expression list; body: statement}
  | ArrayLiteral of {elms: expression list;}
  | CallExpression of {fn: expression; args: expression list;}
  | IndexExpression of {left: expression; index: expression}

  and statement =
  | LetStatment of {idt: expression; value: expression;}
  | ReturnStatement of {value: expression;}
  | ExpressionStatement of {exp: expression;}
  | BlockStatement of {stms: statement list}

  type program = {
    statements: statement list
  }

  let rec expToString = function
  | IntegerLiteral i -> "(INT " ^ string_of_int i ^ ") "
  | StringLiteral i -> "(STR " ^ i ^ ") "
  | Identifier i -> "(IDT " ^ i ^ ") "
  | BooleanLiteral i -> "(BOOL " ^ string_of_bool i ^ ") "
  | PrefixExpression i -> "(PREFIX {op: " ^ i.op ^ " right:{" ^ expToString i.right ^ "}}) "
  | InfixExpression i -> "(INFIX {op: " ^ i.op ^ " left:{" ^ expToString i.left ^ "}"
    ^ " right:{" ^ expToString i.right ^ "}}) "
  | IfExpression i -> let alt = match i.alt with
    | Some i -> stmToString i
    | None -> "" in "(IF {cond: " ^ expToString i.cond ^
    " cons: " ^ stmToString i.cons
    ^ alt ^ "})"
  | FunctionLiteral i -> "(FN {prms: " ^ expsToString i.prms ^ " body: " ^ stmToString i.body ^ "})"
  | ArrayLiteral i -> expsToString i.elms
  | CallExpression i -> "(CALLEXP {fn: " ^ expToString i.fn ^ " args: " ^ expsToString i.args ^ "})"
  | IndexExpression i -> "(INDEXEXP {left: " ^ expToString i.left ^ " index: " ^ expToString i.index ^ "})"

  and expsToString = function
  | [] -> ""
  | h::t -> "[ " ^ expToString h ^ " ]" ^ (expsToString t)

  and stmToString = function
  | LetStatment i -> "LET:" ^ expToString i.idt ^ " " ^ expToString i.value
  | ReturnStatement i -> "RET:" ^ expToString i.value
  | ExpressionStatement i -> "EXP:" ^ expToString i.exp
  | BlockStatement i -> let rec rstmts = function
      | [] -> ""
      | h::t -> "[ " ^ stmToString h ^ " ]" ^ rstmts t
    in "BLOCK:" ^ rstmts i.stms

  let stmsToString stmts = let rec rrc = function
  | [] -> ""
  | h::t -> (stmToString h) ^ " " ^ rrc t
  in rrc stmts

  let eq a b = a = b
  let pp ppf st = Fmt.pf ppf "Statement = %s" (stmToString st)
end
