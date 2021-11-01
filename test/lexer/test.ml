open Interpreter
include Lexer
include Token

module To_test = struct
  let lex str = let rec gTokens lx ls =
      let (lex, tok) = Lexer.nextToken lx in match tok with
      | {t_type = Token.EOF; literal = ""} -> (lex, ls @ [tok])
      | _ -> gTokens lex (ls @ [tok])
    in let (_, tlist) = gTokens (Lexer.newLexer str) [] in tlist
end

let token_testable = Alcotest.testable Token.pp Token.eq

let test_same_tok () = Alcotest.(check (list token_testable))
  "same token"
  [
    Token.newToken Token.ASSIGN "="
  ; Token.newToken Token.PLUS "+"
  ; Token.newToken Token.MINUS "-"
  ; Token.newToken Token.SLASH "/"
  ; Token.newToken Token.ASTERISK "*"
  ; Token.newToken Token.BANG "!"
  ; Token.newToken Token.COMMA ","
  ; Token.newToken Token.SEMICOLON ";"
  ; Token.newToken Token.COLLON ":"
  ; Token.newToken Token.LPAREN "("
  ; Token.newToken Token.RPAREN ")"
  ; Token.newToken Token.LBRACE "{"
  ; Token.newToken Token.RBRACE "}"
  ; Token.newToken Token.LBRACKET "["
  ; Token.newToken Token.RBRACKET "]"
  ; Token.newToken Token.BANG "!"
  ; Token.newToken Token.EQ "=="
  ; Token.newToken Token.ASSIGN "="
  ; Token.newToken Token.ASSIGN "="
  ; Token.newToken Token.BANG "!"
  ; Token.newToken Token.BANG "!"
  ; Token.newToken Token.NOT_EQ "!="
  ; Token.newToken Token.STRING "hogehoge1 ya"
  ; Token.newToken Token.IDENT "aieu"
  ; Token.newToken Token.INT "2222"
  ; Token.newToken Token.FUNCTION "fn"
  ; Token.newToken Token.ILLEGAL "~"
  ; Token.newToken Token.ILLEGAL "#"
  ; Token.newToken Token.LET "let"
  ; Token.newToken Token.IF "if"
  ; Token.newToken Token.ELSE "else"
  ; Token.newToken Token.TRUE "true"
  ; Token.newToken Token.FALSE "false"
  ; Token.newToken Token.RETURN "return"
  ; Token.newToken Token.EOF ""
  ]
  (To_test.lex "= + - /*!,;:(){}[] ! === =!!!=
    \"hogehoge1 ya\"aieu2222fn~#let if else
  true false return")

let test_same_tok2 () = Alcotest.(check (list token_testable))
  "same token"
  [
    Token.newToken Token.LET "let"
  ; Token.newToken Token.IDENT "a"
  ; Token.newToken Token.ASSIGN "="
  ; Token.newToken Token.IDENT "b"
  ; Token.newToken Token.FUNCTION "fn"
  ; Token.newToken Token.LPAREN "("
  ; Token.newToken Token.IDENT "a"
  ; Token.newToken Token.COMMA ","
  ; Token.newToken Token.IDENT "b"
  ; Token.newToken Token.RPAREN ")"
  ; Token.newToken Token.LBRACE "{"
  ; Token.newToken Token.IDENT "a"
  ; Token.newToken Token.PLUS "+"
  ; Token.newToken Token.IDENT "b"
  ; Token.newToken Token.RBRACE "}"
  ; Token.newToken Token.INT "1"
  ; Token.newToken Token.LT "<"
  ; Token.newToken Token.INT "2"
  ; Token.newToken Token.EQ "=="
  ; Token.newToken Token.TRUE "true"
  ; Token.newToken Token.INT "1"
  ; Token.newToken Token.GT ">"
  ; Token.newToken Token.INT "2"
  ; Token.newToken Token.EQ "=="
  ; Token.newToken Token.FALSE "false"
  ; Token.newToken Token.EOF ""
  ]
  (To_test.lex "let a = b
  fn (a, b) {
    a + b
  }
  1 < 2 == true
  1 > 2 == false
  ")



(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          test_case "token list" `Slow test_same_tok;
          test_case "token list" `Slow test_same_tok2;
        ];
    ]
