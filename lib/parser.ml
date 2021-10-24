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

  let nextToken prs = let (le, tok) = Lexer.nextToken prs.l in {
    l = le;
    curToken = prs.peekToken;
    peekToken = tok;
  }

  (* let parseLetStatement prs = {} *)

  let parseStatement prs = (nextToken prs, Ast.LetStatment {idt = Ast.Identifier prs.curToken.literal; value = Ast.IntegerLiteral (int_of_string prs.curToken.literal);})

  let parseProgram prs = let rec rpp prs prg = match prs.curToken with
  | {literal = _; t_type = Token.EOF} -> prg
  | _ -> let (ps, st) = parseStatement prs in rpp ps prg@[st]
  in rpp prs

end
