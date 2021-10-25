open Interpreter
include Parser
include Ast
include Lexer

module To_test = struct
  let newparser lex = let le = Parser.newParser lex in le.peekToken
  let nextToken prs = Parser.nextToken prs
  let ast lex = Parser.parseProgram (Parser.newParser lex) []
end

let ast_testable = Alcotest.testable Ast.pp Ast.eq
let token_testable = Alcotest.testable Token.pp Token.eq
let parser_testable = Alcotest.testable Parser.pp Parser.eq

let test_same_ast () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.IntegerLiteral 1;}
  ]
  (Lexer.newLexer "let a = 1" |> To_test.ast)


let test_parser () = Alcotest.(check (token_testable))
  "same ast"
  {t_type = Token.IDENT; literal = "a"}
  (Lexer.newLexer "let a = 1" |> To_test.newparser)

let test_next_tok () = Alcotest.(check (parser_testable))
  "same parser"
  {l = (Lexer.newLexer "let a = 1"); curToken = {literal = "a"; t_type = Token.IDENT}; peekToken = {literal = "="; t_type = Token.ASSIGN}}
  (Lexer.newLexer "let a = 1" |> Parser.newParser |> To_test.nextToken)


(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          (* test_case "token list" `Slow test_same_ast; *)
          test_case "currentToken" `Slow test_parser;
          test_case "nextToken" `Slow test_next_tok;
        ];
    ]
