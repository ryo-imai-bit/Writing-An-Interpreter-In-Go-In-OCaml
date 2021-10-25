module Token = struct
  include Fmt

  type token_type =
  | ILLEGAL
  | EOF
  (* identifers and literals *)
  | IDENT
  | INT
  | STRING
  (* operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | EQ
  | NOT_EQ
  (* delimeters *)
  | COMMA
  | COLLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  (* keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
    (* todo MACRO *)

  type token = {
    literal: string;
    t_type: token_type;
  }

  let tokenToString tok = match tok.t_type with
  | ILLEGAL -> "ILLEGAL:" ^ tok.literal
  | EOF -> "EOF"
  (* identifers and literals *)
  | IDENT -> "IDENT:" ^ tok.literal
  | INT -> "INT:" ^ tok.literal
  | STRING -> "STRING:" ^ tok.literal
  (* operators *)
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | NOT_EQ -> "NOT_EQ"
  (* delimeters *)
  | COMMA -> "COMMA"
  | COLLON -> "COLLON"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  (* keywords *)
  | FUNCTION -> "FUNCTION"
  | LET -> "LET"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"

  let keywords = function
    | "fn" -> FUNCTION
    | "let" -> LET
    | "if" -> IF
    | "else" -> ELSE
    | "true" -> TRUE
    | "false" -> FALSE
    | "return" -> RETURN
    | _ -> IDENT

  let newToken t_type literal = {t_type = t_type; literal = literal}

  let eq a b = a.literal = b.literal && a.t_type = b.t_type

  let pp ppf tk = Fmt.pf ppf "Token =%s" (tokenToString tk)
  (* (token_to_string tok) *)
end
