open Interpreter
include Parser
include Ast

module To_test = struct
  let ast lex = Parser.parseProgram (Parser.newParser lex) []
end

let ast_testable = Alcotest.testable Ast.pp Ast.eq

let test_same_ast () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1;}
  ]
  (Lexer.newLexer "let a = 1" |> To_test.ast)



(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          test_case "token list" `Slow test_same_ast;
        ];
          (* {test_case "ident"     `Quick test_same_tok_ident;] *)
    ]
