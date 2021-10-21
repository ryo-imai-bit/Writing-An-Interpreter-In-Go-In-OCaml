open Interpreter
include Lexer
include Token

module To_test = struct
  let lex str = let rec gTokens lx ls =
      let (lex, tok) = Lexer.nextToken lx in match tok with
      | Token.EOF -> (lex, ls @ [tok])
      | _ -> gTokens lex (ls @ [tok])
    in let (_, tlist) = gTokens (Lexer.newLexer str) [] in tlist
end

let token_testable = Alcotest.testable Token.pp Token.eq

let test_same_tok () = Alcotest.(check (list token_testable))
  "same token"
  [
    Token.ASSIGN
  ; Token.PLUS
  ; Token.MINUS
  ; Token.SLASH
  ; Token.ASTERISK
  ; Token.BANG
  ; Token.COMMA
  ; Token.SEMICOLON
  ; Token.COLLON
  ; Token.LAPREN
  ; Token.RPAREN
  ; Token.LBRACE
  ; Token.RBRACE
  ; Token.LBRACKET
  ; Token.RBRACKET
  ; Token.BANG
  ; Token.EQ
  ; Token.ASSIGN
  ; Token.BANG
  ; Token.NOT_EQ
  ; Token.EOF
  ]
  (To_test.lex "= + - /*!,;:(){}[] ! == =! !=")



(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          test_case "token list" `Slow test_same_tok;
        ];
          (* {test_case "ident"     `Quick test_same_tok_ident;] *)
    ]
