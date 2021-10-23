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

  let rc lex =
    if lex.readPosition >= String.length(lex.input)
      then null_byte
    else
      String.get lex.input lex.readPosition

  let readChar lex =
      {lex with position = lex.readPosition; readPosition = lex.readPosition + 1; ch = rc lex}

  let peekChar lex =
    if lex.readPosition >= String.length(lex.input)
      then null_byte
    else
      String.get lex.input lex.readPosition

  let read lex f = let le = f lex in (le, String.sub le.input lex.position (le.position - lex.position))

  let readString lex = let rec rrc le = match le.ch with
  | '\x00'
  | '"' -> le
  | _ -> readChar le |> rrc
    in let (le, str) = read lex rrc in (readChar le, str)

  let readIdent lex = let rec rrc le = match le.ch with
  | '\x00' -> le
  | c -> if is_letter c then (readChar le |> rrc) else le
    in read lex rrc

  let readNum lex = let rec rrc le = match le.ch with
  | '\x00' -> le
  | i -> if is_digit i then (readChar le |> rrc) else le
    in read lex rrc

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
      then (readChar le |> readChar, Token.newToken Token.EQ (Char.escaped le.ch ^ Char.escaped (peekChar le)))
      else (readChar le, Token.newToken Token.ASSIGN (Char.escaped le.ch))
    | '+' -> (readChar le, Token.newToken Token.PLUS (Char.escaped le.ch))
    | '-' -> (readChar le, Token.newToken Token.MINUS (Char.escaped le.ch))
    | '!' -> if peekChar le = '='
      then (readChar le |> readChar, Token.newToken Token.NOT_EQ (Char.escaped le.ch ^ Char.escaped (peekChar le)))
      else (readChar le, Token.newToken Token.BANG (Char.escaped le.ch))
    | '*' -> (readChar le, Token.newToken Token.ASTERISK (Char.escaped le.ch))
    | '/' -> (readChar le, Token.newToken Token.SLASH (Char.escaped le.ch))
    | ',' -> (readChar le, Token.newToken Token.COMMA (Char.escaped le.ch))
    | ':' -> (readChar le, Token.newToken Token.COLLON (Char.escaped le.ch))
    | ';' -> (readChar le, Token.newToken Token.SEMICOLON (Char.escaped le.ch))
    | '(' -> (readChar le, Token.newToken Token.LPAREN (Char.escaped le.ch))
    | ')' -> (readChar le, Token.newToken Token.RPAREN (Char.escaped le.ch))
    | '{' -> (readChar le, Token.newToken Token.LBRACE (Char.escaped le.ch))
    | '}' -> (readChar le, Token.newToken Token.RBRACE (Char.escaped le.ch))
    | '[' -> (readChar le, Token.newToken Token.LBRACKET (Char.escaped le.ch))
    | ']' -> (readChar le, Token.newToken Token.RBRACKET (Char.escaped le.ch))
    | '"' -> let (le, str) = (readChar le |> readString) in (le, Token.newToken Token.STRING str)
    | '\x00' -> (readChar le, Token.newToken Token.EOF "")
    | c -> if is_letter c
      then let (le, str) = readIdent le in (le, Token.newToken (Token.keywords str) str)
      else if is_digit c
        then let (le, num) = readNum le in (le, Token.newToken Token.INT num)
        else (readChar le, Token.newToken Token.ILLEGAL (Char.escaped c))

  let newLexer input = let lex = {
    input = input;
    position = 0;
    readPosition = 0;
    ch = null_byte;
  } in readChar lex

end
