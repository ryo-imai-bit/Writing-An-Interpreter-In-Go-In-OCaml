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
  | RPRACE
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


  let tokensEq tok_a tok_b = match (tok_a, tok_b) with
  | tok_a, tok_b -> tok_a = tok_b

  let prettyPrint ppf _ = Fmt.pr ppf "Token =" 
  (* (token_to_string tok) *)
end
