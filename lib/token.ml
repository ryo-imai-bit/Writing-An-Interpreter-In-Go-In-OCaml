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
  | LAPREN
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


  let eq tok_a tok_b = tok_a = tok_b

  let pp ppf _ = Fmt.pf ppf "Token =%s" "token"
  (* (token_to_string tok) *)
end
