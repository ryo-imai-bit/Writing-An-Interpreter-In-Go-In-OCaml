open Interpreter
include Lexer
include Token

module To_test = struct
  let lex = Lexer.nextToken
end

let token_testable = Alcotest.testable Token.prettyPrint Token.tokensEq

let test_tok = Alcotest.(check (token_testable)) 
  "same token" 
  Token.ASSIGN 
  To_test.lex {input: "=", position = 0, readPosition: 0, ch = Token.null_byte} []

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "digit", [
          test_case "Lower case"     `Quick test_is_digit;
        ];
    ]
