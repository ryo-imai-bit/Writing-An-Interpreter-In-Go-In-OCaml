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

  let parseIntegerLiteral prs = (nextToken prs, Some (Ast.IntegerLiteral (int_of_string prs.curToken.literal)))

  let parseIdentifier prs = (nextToken prs, Some (Ast.Identifier prs.curToken.literal))

  let parseStringLiteral prs = (nextToken prs, Some (Ast.StringLiteral prs.curToken.literal))

  let errorParse prs tok = ({
    l = prs.l;
    curToken = prs.curToken;
    peekToken = prs.peekToken;
    errors = prs.errors @ [Token.tokenToString tok]
  }, None)

  (* stringもBANG, MINUSとくっついてしまう *)
  let rec parseExpression prs = match prs.curToken with
  | {literal = _; t_type = Token.INT} -> parseIntegerLiteral prs
  | {literal = _; t_type = Token.IDENT} -> parseIdentifier prs
  | {literal = _; t_type = Token.STRING} -> parseStringLiteral prs
  | {literal; t_type = Token.BANG}
  | {literal; t_type = Token.PLUS}
  | {literal; t_type = Token.MINUS} -> (let (pr, exp) = parseExpression (nextToken prs) in match exp with
    | Some ex -> (pr, Some (Ast.PrefixExpression {op = literal; right = ex}))
    | None -> (pr, None))
  | _ -> parseStringLiteral prs

  (* let rec parsePrefixExpression prs = match prs.curToken with
  | {literal; t_type = Token.BANG}
  | {literal; t_type = Token.MINUS} -> (let (pr, exp) = parseExpression (nextToken prs) in match exp with
    | Some ex -> (pr, Ast.PrefixExpression {op = literal; right = ex})
    | None -> (pr, None))
  | tok -> errorParse prs tok *)

  let parseLetStatement prs = match prs.curToken with
  | {literal; t_type = Token.IDENT} -> let pr = nextToken prs in (match pr.curToken with
    | {literal = _; t_type = Token.ASSIGN} -> let (p, exp) = parseExpression (nextToken pr)
      in (match exp with
      | Some e -> (p, Some (Ast.LetStatment {
              idt = Ast.Identifier literal;
              value = e
            }))
      | None -> (p, None))
    | tk -> errorParse pr tk)
  | tok -> errorParse prs tok

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
