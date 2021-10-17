module Lexer = struct
  let a = "hoge"
  let is_digit c = '0' <= c && c <= '9'
end
  (* type token_type =
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
    | RETURN *)
    (* todo MACRO *)

  (* type token = {
    literal: string;
    t_type: token_type;
  } *)
