open Interpreter
include Parser
include Ast
include Lexer

module To_test = struct
  let newparser lex = let le = Parser.newParser lex in le.peekToken
  let nextToken prs = Parser.nextToken prs
  let ast lex = let (_, stm) = Parser.parseProgram (Parser.newParser lex) []
    in stm
end

let ast_testable = Alcotest.testable Ast.pp Ast.eq
let token_testable = Alcotest.testable Token.pp Token.eq
let parser_testable = Alcotest.testable Parser.pp Parser.eq

let test_statements () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.ReturnStatement {value = Ast.Identifier "hoge"};
    Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.InfixExpression {
      op = "+";
      left = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;};
      right = Ast.InfixExpression {
        op = "*";
        left = Ast.IntegerLiteral 1;
        right = Ast.IntegerLiteral 5;
      };
    }};
    Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "+";
      left = Ast.PrefixExpression {op = "!"; right = Ast.Identifier "hogehoge";};
      right = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 12; right = Ast.IntegerLiteral 3;};
    }};
  ]
  (Lexer.newLexer "return hoge;let a = -1 + 1 * 5; !hogehoge + 12 * 3" |> To_test.ast)

let test_let_statements () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;}};
    Ast.LetStatment {idt = Ast.Identifier "a"; value = Ast.InfixExpression {
      op = "+";
      left = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;};
      right = Ast.InfixExpression {
        op = "*";
        left = Ast.IntegerLiteral 1;
        right = Ast.IntegerLiteral 5;
      };
    }};
  ]
  (Lexer.newLexer "let a = -1;let a = -1 + 1 * 5" |> To_test.ast)

let test_return_statements () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.ReturnStatement {value = Ast.Identifier "hoge"};
    Ast.ReturnStatement {value = Ast.InfixExpression {
      op = "-";
      left = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 123; right = Ast.IntegerLiteral 23;};
      right = Ast.Identifier "hoge" ;
    }};
  ]
  (Lexer.newLexer "return hoge; return 123 * 23 - hoge" |> To_test.ast)

let test_expression_statements () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.ExpressionStatement {exp = Ast.InfixExpression {op = "*"; left = Ast.Identifier "hogehoge"; right = Ast.Identifier "iu"}};
    Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "*";
      left = Ast.IntegerLiteral 100;
      right = Ast.IntegerLiteral 12;
    }};
  ]
  (Lexer.newLexer "hogehoge * iu; 100 * 12" |> To_test.ast)


let test_prefix () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.ExpressionStatement {exp = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;}}
  ]
  (Lexer.newLexer "-1" |> To_test.ast)

let test_infix () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.LetStatment {
      idt = Ast.Identifier "a"; value = Ast.InfixExpression {
        op = "+";
        left = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1};
        right = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1};
      }
    };
    Ast.LetStatment {idt = Ast.Identifier "aiai"; value = Ast.InfixExpression {
        op = "-";
        left = Ast.InfixExpression {
          op = "+";
          left = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;};
          right = Ast.InfixExpression {
            op = "*";
            left = Ast.IntegerLiteral 1;
            right = Ast.IntegerLiteral 5;
          };
        };
        right = Ast.InfixExpression {
          op = "/";
          left = Ast.IntegerLiteral 12;
          right = Ast.IntegerLiteral 3;
        }
      };
    };
  ]
  (Lexer.newLexer "let a = -1 + -1; let aiai = -1 + 1 * 5 - 12 / 3" |> To_test.ast)


let test_parser () = Alcotest.(check (token_testable))
  "same ast"
  {t_type = Token.IDENT; literal = "a"}
  (Lexer.newLexer "let a = 1" |> To_test.newparser)

let test_next_tok () = Alcotest.(check (parser_testable))
  "same parser"
  {l = (Lexer.newLexer "let a = 1"); errors = []; curToken = {literal = "a"; t_type = Token.IDENT}; peekToken = {literal = "="; t_type = Token.ASSIGN;}}
  (Lexer.newLexer "let a = 1" |> Parser.newParser |> To_test.nextToken)


(* Run it *)
let () =
  let open Alcotest in
  run "Lexer" [
      "nextToken", [
          test_case "parse statements" `Slow test_statements;
          test_case "parse LetStatement" `Slow test_let_statements;
          test_case "parse ReturnStatement" `Slow test_return_statements;
          test_case "parse ExpressionStatement" `Slow test_expression_statements;
          test_case "parse prefix" `Slow test_prefix;
          test_case "parse infix" `Slow test_infix;
          test_case "currentToken" `Slow test_parser;
          test_case "nextToken" `Slow test_next_tok;
        ];
    ]
