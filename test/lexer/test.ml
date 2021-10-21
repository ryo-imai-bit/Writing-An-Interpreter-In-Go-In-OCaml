open Interpreter
include Lexer
include Token

module To_test = struct
  let lex str = let rec gTokens lx ls =
      if lx.ch = Lexer.null_byte
        then (lx, ls)
      else let (lex, tok) = Lexer.nextToken lx in gTokens lex (ls @ [tok])
    in let (_, tlist) = gTokens (Lexer.newLexer str) [] in tlist
end

let token_testable = Alcotest.testable Token.pp Token.eq

let test_same_tok () = Alcotest.(check (list token_testable))
  "same token"
  [
    Token.ASSIGN
  ; Token.PLUS
  ; Token.MINUS
  ; Token.ASSIGN
  ; Token.ASSIGN
  ; Token.ASSIGN
  ]
  (To_test.lex "  = + - = ==")

let test_same_tok_ident () = Alcotest.(check (list token_testable))
  "same token"
  [
    Token.PLUS
  ; Token.PLUS
  ; Token.PLUS
  ]
  (To_test.lex "  + + +")


(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          test_case "assign" `Slow test_same_tok;
          test_case "ident"     `Slow test_same_tok_ident;
        ];
          (* {test_case "ident"     `Quick test_same_tok_ident;] *)
    ]
