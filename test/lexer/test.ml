open Interpreter
include Lexer
include Token

module To_test = struct
  let lex a = let (_, lex) = Lexer.newLexer a |> Lexer.nextToken in lex
end

let token_testable = Alcotest.testable Token.pp Token.eq

let test_same_tok () = Alcotest.(check token_testable)
  "same token"
  Token.ASSIGN
  (To_test.lex "  =")

let test_same_tok_ident () = Alcotest.(check token_testable)
  "same token"
  Token.IDENT
  (To_test.lex "  +")


(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          Alcotest.test_case "assign" `Quick test_same_tok;
          test_case "ident"     `Quick test_same_tok_ident;
        ];
          (* {test_case "ident"     `Quick test_same_tok_ident;] *)
    ]
