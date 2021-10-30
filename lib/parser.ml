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

  let prefixPrecedence = 7
  let lowest = 1

  let precedence (tok:Token.token_type) = match tok with
  | Token.EQ
  | Token.NOT_EQ -> 2
  | Token.LT
  | Token.GT -> 3
  | Token.PLUS
  | Token.MINUS -> 4
  | Token.SLASH
  | Token.ASTERISK -> 5
  | Token.LPAREN -> 6
  | Token.LBRACKET -> 8
  | _ -> lowest

  let newParser lex = let (le, tok) = Lexer.nextToken lex
    in let (_, tk) = Lexer.nextToken le
      in {
            l = le;
            curToken = tok;
            peekToken = tk;
            errors = [];
        }

  let rec errorsToString errors = match errors with
  | [] -> ""
  | h::[] -> h
  | h::t -> h ^ ", " ^ (errorsToString t)

  let nextToken prs = let (le, _) = Lexer.nextToken prs.l
    in let (_, t) = Lexer.nextToken le in {
      l = le;
      curToken = prs.peekToken;
      peekToken = t;
      errors = prs.errors;
    }

  let errorParse prs message = ({
    l = prs.l;
    curToken = prs.curToken;
    peekToken = prs.peekToken;
    errors = prs.errors @ [" " ^ message ^ "@" ^ Token.tokenToString prs.curToken]
  }, None)

  let peekPrecedence prs = precedence prs.peekToken.t_type

  let parseIntegerLiteral prs = (prs, Some (Ast.IntegerLiteral (int_of_string prs.curToken.literal)))

  let parseIdentifier prs = (prs, Some (Ast.Identifier prs.curToken.literal))

  let parseStringLiteral prs = (prs, Some (Ast.StringLiteral prs.curToken.literal))

  let parseBooleanLiteral prs = (prs, Some (Ast.BooleanLiteral (bool_of_string prs.curToken.literal)))

  let parseBlockstatement prs parseStatement = let rec rpstms prs stmts = match stmts with
  | Some slist -> (match prs.curToken.t_type with
    | Token.EOF -> errorParse prs "expected RBRACE"
    | Token.RBRACE -> (prs, Some slist)
    | _ -> match parseStatement prs with
      | (ps, Some stm) -> rpstms (nextToken ps) (Some (slist@[stm]))
      | (ps, None) -> (ps, None))
  | None -> (prs, None)
  in match rpstms prs (Some []) with
  | (pr, Some slist) -> (pr, Some (Ast.BlockStatement {stms = slist;}))
  | (pr, None) -> (pr, None)

  let parseIfExpression prs parseExpression parseStatement =
    if prs.peekToken.t_type = Token.LPAREN
    then (match parseExpression (nextToken prs |> nextToken) lowest parseStatement with
      | (pr, Some exp) -> let pr = nextToken pr in if pr.curToken.t_type = Token.RPAREN && pr.peekToken.t_type = Token.LBRACE
        then (match parseBlockstatement (nextToken pr |> nextToken) parseStatement with
          | (par, Some ex) -> let ps = nextToken par in if ps.curToken.t_type = Token.ELSE
            then if ps.peekToken.t_type = Token.LBRACE
              then (match parseBlockstatement (nextToken ps |> nextToken) parseStatement with
              | (p, Some expr) -> (p, Some (Ast.IfExpression {cond = exp; cons = ex; alt = Some expr}))
              | (p, None) -> (p, None))
              else errorParse ps "expected LBRACE"
            else (par, Some (Ast.IfExpression {cond = exp; cons = ex; alt = None;}))
          | (par, None) -> (par, None))
        else errorParse pr "expected RPAREN"
      | (pr, None) -> (pr, None))
    else errorParse prs "expected LPAREN"

  let parsePrefixExpression par parseExpression parseStatement = (match par.curToken with
  | {literal = _; t_type = Token.INT} -> parseIntegerLiteral par
  | {literal = _; t_type = Token.IDENT} -> parseIdentifier par
  | {literal = _; t_type = Token.STRING} -> parseStringLiteral par
  | {literal = _; t_type = Token.FALSE}
  | {literal = _; t_type = Token.TRUE} -> parseBooleanLiteral par
  | {literal = _; t_type = Token.IF} -> parseIfExpression par parseExpression parseStatement
  | {literal = _; t_type = Token.LPAREN} -> (match (parseExpression (nextToken par) lowest parseStatement) with
    | (pr, Some exp) -> if pr.peekToken.t_type = Token.RPAREN
      then (nextToken pr, Some exp)
      else errorParse pr "expected RPAREN"
    | (pr, None) -> (pr, None))
  | {literal; t_type = Token.BANG}
  | {literal; t_type = Token.MINUS} -> (let (pr, exp) = (parseExpression (nextToken par) prefixPrecedence parseStatement)
    in match exp with
    | Some ex -> (pr, Some (Ast.PrefixExpression {op = literal; right = ex}))
    | None -> (pr, None))
  | _ -> errorParse par "No matching prefix parse")

  let parseInfixExpression par lexp parseExpression parseStatement = match par.curToken with
  | {literal; t_type = Token.SLASH}
  | {literal; t_type = Token.ASTERISK}
  | {literal; t_type = Token.EQ}
  | {literal; t_type = Token.NOT_EQ}
  | {literal; t_type = Token.LT}
  | {literal; t_type = Token.GT}
  | {literal; t_type = Token.MINUS}
  | {literal; t_type = Token.PLUS} -> (match parseExpression (nextToken par) (precedence par.curToken.t_type) parseStatement with
    | (pr, Some exp) -> (pr, Some (Ast.InfixExpression {op = literal; left = lexp; right = exp;}))
    | (pr, None) -> (pr, None))
  | _ -> errorParse par "no matching infix parse"

  let rec parseExpression prs pcd parseStatement = match (parsePrefixExpression prs parseExpression parseStatement) with
  | (ps, Some le) ->
    let rec rParseInfix pr pcd lexp = (if pcd < (peekPrecedence pr)
      then (match (parseInfixExpression (nextToken pr) lexp parseExpression parseStatement) with
      | (pars, Some re) -> rParseInfix pars pcd re
      | (pars, None) -> (pars, None))
      else (pr, Some lexp))
    in rParseInfix ps pcd le
  | (ps, None) -> (ps, None)

  let parseLetStatement prs parseStatement = match prs.curToken with
  | {literal; t_type = Token.IDENT} -> let pr = nextToken prs in (match pr.curToken with
    | {literal = _; t_type = Token.ASSIGN} -> let (p, exp) = parseExpression (nextToken pr) lowest parseStatement
      in (match exp with
      | Some e -> let sp = if Token.isSemicolon p.peekToken then nextToken p else p
          in (sp, Some (Ast.LetStatment {
              idt = Ast.Identifier literal;
              value = e
          }))
      | None -> (p, None))
    | _  -> errorParse pr "expected ASSIGN")
  | _ -> errorParse prs "expectd IDENT"

  let parseReturnStatement prs parseStatement = match parseExpression prs lowest parseStatement with
  | (ps, Some exp) -> (let p = if Token.isSemicolon ps.peekToken then nextToken ps else ps
        in (p, Some (Ast.ReturnStatement {
          value = exp;
        })))
  | (ps, None) -> (ps, None)

  let parseExpressionStatement prs parseStatement = match parseExpression prs lowest parseStatement with
  | (ps, Some exp) -> (let p = if Token.isSemicolon ps.peekToken then nextToken ps else ps
        in (p, Some (Ast.ExpressionStatement {
          exp = exp;
        })))
  | (ps, None) -> (ps, None)

  let rec parseStatement prs = match prs.curToken with
  | {literal = _; t_type = Token.LET} -> parseLetStatement (nextToken prs) parseStatement
  | {literal = _; t_type = Token.RETURN} -> parseReturnStatement (nextToken prs) parseStatement
  | _ -> parseExpressionStatement prs parseStatement

  let parseProgram prs lst = let rec rpp prs prg = match prs.curToken with
  | {literal = _; t_type = Token.EOF} -> (prs, prg)
  | _ -> match parseStatement prs with
    | (ps, Some stm) -> rpp (nextToken ps) (prg@[stm])
    | (ps, None) -> (ps, prg)
  in rpp prs lst

  let eq prsa prsb = Token.eq prsa.curToken prsb.curToken && Token.eq prsa.peekToken prsb.peekToken

  let pp ppf prs = Fmt.pf ppf "Parser = { %s }" ("curToken:" ^ Token.tokenToString prs.curToken ^ " peekToken:" ^ Token.tokenToString prs.peekToken)
end
