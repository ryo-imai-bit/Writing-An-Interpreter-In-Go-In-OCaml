module Ast = struct
  include Token
  type node =
  | Expression of expression
  | Statement of statement

  and expression =
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
  | MacroLiteral of {prms: expression list; body: statement}

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
  | MacroLiteral i -> "(MACRO {prms: " ^ expsToString i.prms ^ " body: " ^ stmToString i.body ^ "})"

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

  let rec modifyExpression (modifier:expression -> expression) node = let nd = match node with
    | InfixExpression i -> InfixExpression {
      i with
        left = modifyExpression modifier i.left;
        right = modifyExpression modifier i.right;
      }
    | PrefixExpression i -> PrefixExpression {
        i with
          right = modifyExpression modifier i.right;
      }
    | IfExpression i -> (match i.alt with
      | Some alt -> IfExpression {
          cond = modifyExpression modifier i.cond;
          cons = modifyStatement modifier i.cons;
          alt = Some (modifyStatement modifier alt);
      }
      | None -> IfExpression {
          cond = modifyExpression modifier i.cond;
          cons = modifyStatement modifier i.cons;
          alt = None;
      })
    | FunctionLiteral i -> FunctionLiteral {
        prms = List.map (modifyExpression modifier) i.prms;
        body = modifyStatement modifier i.body;
      }
    | ArrayLiteral i -> ArrayLiteral {
        elms = List.map (modifyExpression modifier) i.elms;
      }
    | IndexExpression i -> IndexExpression {
        left = modifyExpression modifier i.left;
        index = modifyExpression modifier i.index;
      }
    | CallExpression i -> (match i.fn with
      | Identifier idt when idt = "unquote" -> CallExpression {
          i with
          args = List.map (modifyExpression modifier) i.args;
        }
      | _ -> CallExpression i)
    | exp -> exp
    in modifier nd

  and modifyStatement modifier stm = match stm with
  | LetStatment i -> LetStatment {
      idt = modifyExpression modifier i.idt;
      value = modifyExpression modifier i.value;
    }
  | ReturnStatement i -> ReturnStatement {
      value = modifyExpression modifier i.value;
    }
  | ExpressionStatement i -> ExpressionStatement {
      exp = modifyExpression modifier i.exp;
    }
  | BlockStatement i -> BlockStatement {
      stms = List.map (modifyStatement modifier) i.stms;
    }


  let stmsToString stmts = let rec rrc = function
  | [] -> ""
  | h::t -> (stmToString h) ^ " " ^ rrc t
  in rrc stmts

  let eq a b = a = b
  let pp ppf st = Fmt.pf ppf "Statement = %s" (stmToString st)
end
