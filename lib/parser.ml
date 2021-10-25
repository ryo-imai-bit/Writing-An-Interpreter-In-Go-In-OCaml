module Parser = struct
  include Lexer
  include Token
  include Ast

  type parser = {
    l: Lexer.lexer;
    curToken: Token.token;
    peekToken: Token.token;
    errors: string list;
  }

  let newParser lex = let (le, tok) = Lexer.nextToken lex
    in let (_, tk) = Lexer.nextToken le
in {
      l = le;
      curToken = tok;
      peekToken = tk;
      errors = [];
    }

  let nextToken prs = let (le, _) = Lexer.nextToken prs.l
    in let (_, t) = Lexer.nextToken le in {
      l = le;
      curToken = prs.peekToken;
      peekToken = t;
      errors = prs.errors;
    }

  let parseToEnd prs = let rec toEnd pr = match pr.curToken with
  | {literal = _; t_type = Token.EOF} -> pr
  | _ -> toEnd pr
  in toEnd prs

  let parseIntegerLiteral prs = (nextToken prs, Ast.IntegerLiteral (int_of_string prs.curToken.literal))

  let parseIdentifier prs = (nextToken prs, Ast.Identifier prs.curToken.literal)

  let parseStringLiteral prs = (nextToken prs, Ast.StringLiteral prs.curToken.literal)

  let parseExpression prs = match prs.curToken with
  | {literal = _; t_type = Token.INT} -> parseIntegerLiteral prs
  | {literal = _; t_type = Token.IDENT} -> parseIdentifier prs
  | {literal = _; t_type = Token.STRING} -> parseStringLiteral prs
  | _ -> parseStringLiteral prs

  (* let parseLetStatement prs = (nextToken prs |> nextToken |> nextToken |> nextToken, Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1;}) *)
  let parseLetStatement prs = match prs.curToken with
  | {literal = _; t_type = Token.IDENT} -> (nextToken prs |> nextToken |> nextToken, Some (Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1}))
  | tok -> let errp = parseToEnd prs
    in ({
      l = errp.l;
      curToken = errp.curToken;
      peekToken = errp.peekToken;
      errors = prs.errors @ [Token.tokenToString tok]
    }, None)

  let parseStatement prs = match prs.curToken with
  | {literal = "let"; t_type = Token.LET} -> nextToken prs |> parseLetStatement
  | _ -> (nextToken prs, Some Ast.ReturnStatement)
  (* let parseProgram _ _ = [Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1}] *)

  let parseProgram prs lst = let rec rpp prs prg = match prs.curToken with
  | {literal = _; t_type = Token.EOF} -> (prs, prg)
  | _ -> let (ps, st) = parseStatement prs in match st with
    | Some stm -> rpp ps (prg@[stm])
    | None -> (ps, prg)
  in rpp prs lst

  let eq prsa prsb = Token.eq prsa.curToken prsb.curToken && Token.eq prsa.peekToken prsb.peekToken

  let pp ppf prs = Fmt.pf ppf "Parser = { %s }" ("curToken:" ^ Token.tokenToString prs.curToken ^ " peekToken:" ^ Token.tokenToString prs.peekToken)
end
