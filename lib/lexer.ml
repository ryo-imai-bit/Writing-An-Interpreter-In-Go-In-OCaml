module Lexer = struct
  include Token

  type lexer = {
    input: string;
    position: int;
    readPosition: int;
    ch: char;
  }

  let is_letter c = 'a' <= c && c <= 'z' || 'A' <= c  && c <= 'Z' || c == '_'

  let is_digit c = '0' <= c && c <= '9'

  let null_byte = '\x00'

  let readChar lex =
    if lex.readPosition >= String.length(lex.input)
      then {lex with position = lex.readPosition; readPosition = lex.readPosition + 1; ch = null_byte}
    else
      {lex with position = lex.readPosition; readPosition = lex.readPosition + 1; ch = String.get lex.input lex.readPosition}

  let peekChar lex =
    if lex.readPosition >= String.length(lex.input)
      then null_byte
    else
      String.get lex.input lex.readPosition


  let rec skipWhitespace lex = match lex.ch with
    | ' '
    | '\n'
    | '\t'
    | '\r'
    | '\b' -> skipWhitespace (readChar lex)
    | _ -> lex

  let nextToken lex = let le = skipWhitespace lex in
    match le.ch with
    | ' '
    | '\n'
    | '\t'
    | '\r'
    | '\b' -> failwith "skipwhitespace wrong"
    | '=' -> if peekChar le = '='
      then (readChar le |> readChar, Token.EQ)
      else (readChar le, Token.ASSIGN)
    | '+' -> (readChar le, Token.PLUS)
    | '-' -> (readChar le, Token.MINUS)
    | '!' -> if peekChar le = '='
      then (readChar le |> readChar, Token.NOT_EQ)
      else (readChar le, Token.BANG)
    | '*' -> (readChar le, Token.ASTERISK)
    | '/' -> (readChar le, Token.SLASH)
    | ',' -> (readChar le, Token.COMMA)
    | ':' -> (readChar le, Token.COLLON)
    | ';' -> (readChar le, Token.SEMICOLON)
    | '(' -> (readChar le, Token.LAPREN)
    | ')' -> (readChar le, Token.RPAREN)
    | '{' -> (readChar le, Token.LBRACE)
    | '}' -> (readChar le, Token.RBRACE)
    | '[' -> (readChar le, Token.LBRACKET)
    | ']' -> (readChar le, Token.RBRACKET)
    (* | '"' -> (readChar le, Token.STRING) *)
    | '\x00' -> (readChar le, Token.EOF)
    | _ -> (le, Token.IDENT)

  let newLexer input = let lex = {
    input = input;
    position = 0;
    readPosition = 0;
    ch = null_byte;
  } in readChar lex

end
