module Parser = struct
  include Lexer
  include Token
  include Ast

  type parser = {
    l: Lexer.lexer;
    curToken: Token.token;
    peekToken: Token.token;
  }

  let newParser lex = let (le, tok) = Lexer.nextToken lex
    in let (_, tk) = Lexer.nextToken le
    in {
      l = le;
      curToken = tok;
      peekToken = tk
    }

  let nextToken prs = let (le, _) = Lexer.nextToken prs.l
    in let (_, t) = Lexer.nextToken le in {
      l = le;
      curToken = prs.peekToken;
      peekToken = t;
    }

  let parseLetStatement prs = (nextToken prs |> nextToken |> nextToken |> nextToken, Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1;})

  let parseStatement prs = match prs.curToken with
  | {literal = "let"; t_type = Token.LET} -> parseLetStatement prs
  | _ -> (nextToken prs, Ast.ReturnStatement)
  (* let parseProgram _ _ = [Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1}] *)

  let parseProgram prs = let rec rpp prs prg = match prs.curToken with
  | {literal = _; t_type = Token.EOF} -> prg
  | _ -> let (ps, st) = parseStatement prs in rpp ps prg@[st]
  in rpp prs

  let eq prsa prsb = Token.eq prsa.curToken prsb.curToken && Token.eq prsa.peekToken prsb.peekToken

  let pp ppf prs = Fmt.pf ppf "Parser = { %s }" ("curToken:" ^ Token.tokenToString prs.curToken ^ " peekToken:" ^ Token.tokenToString prs.peekToken)
end
