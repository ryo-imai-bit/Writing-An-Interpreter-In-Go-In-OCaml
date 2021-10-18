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


  let rec skipWhitespace lex = match lex.ch with
    | ' '
    | '\n'
    | '\t'
    | '\r'
    | '\b' -> skipWhitespace (readChar lex)
    | _ -> lex

  let nextToken lex token = let le = skipWhitespace lex in
    match le.ch with
    | ' '
    | '\n'
    | '\t'
    | '\r'
    | '\b' -> failwith "skipwhitespace wrong"
    | '=' -> (readChar le, Token.ASSIGN)
    | _ -> (le, token)
end
