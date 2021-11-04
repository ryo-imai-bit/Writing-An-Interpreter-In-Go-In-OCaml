open Interpreter
include Parser
include Ast
include Lexer

module To_test = struct
  let newparser lex = let le = Parser.newParser lex in le.peekToken
  let nextToken prs = Parser.nextToken prs
  let ast lex = let (_, prg) = Parser.parseProgram (Parser.newParser lex) [] in prg.statements
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
      right = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 12; right = Ast.BooleanLiteral true ;};
    }};
    Ast.ExpressionStatement {exp = Ast.BooleanLiteral false};
    Ast.ExpressionStatement {exp = Ast.IfExpression {
      cond = Ast.InfixExpression {
        op = "==";
        left = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 2; right = Ast.IntegerLiteral 3};
        right = Ast.Identifier "hoge";
      };
      cons = Ast.BlockStatement {stms = [
        Ast.LetStatment {idt = Ast.Identifier "hoge"; value = Ast.IntegerLiteral 3};
        Ast.ExpressionStatement {
          exp = Ast.InfixExpression {op = "=="; left = Ast.Identifier "foo"; right = Ast.Identifier "hoge";}
        };
      ]};
      alt = Some (Ast.BlockStatement {stms = [
        Ast.ExpressionStatement {exp = Ast.Identifier "hoge"};
      ]});
    };
    };
    Ast.ExpressionStatement {
      exp = Ast.ArrayLiteral {
        elms = [
          Ast.IntegerLiteral 1;
          Ast.IntegerLiteral 3;
          Ast.FunctionLiteral {
            prms = [
              Ast.Identifier "a";
              Ast.Identifier "b";
            ];
            body = Ast.BlockStatement {
              stms = [
                Ast.ExpressionStatement {
                  exp = Ast.InfixExpression {
                    op = "*";
                    left = Ast.IntegerLiteral 12;
                    right = Ast.IntegerLiteral 3;
                  }
                };
              ]
            };
          };
        ]
      };
    };
    Ast.ExpressionStatement {
      exp = Ast.IndexExpression {left = Ast.Identifier "hoge"; index = Ast.Identifier "hogehoge"};
    };
    Ast.ExpressionStatement {
      exp = Ast.IndexExpression {left = Ast.Identifier "fuga"; index = Ast.Identifier "kame"};
    };
  ]
  (Lexer.newLexer "return hoge;
    let a = -1 + 1 * 5;
    !hogehoge + 12 * true;
    false;
    if (2 * 3 == hoge) { let hoge = 3; foo == hoge;} else {hoge};
    [1, 3, fn (a, b) { 12 * 3}];
    hoge[hogehoge];
    fuga[kame];
  " |> To_test.ast)

let test_let_statements () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.LetStatment {
      idt = Ast.Identifier "a";
      value = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;}
    };
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
      left = Ast.InfixExpression {
        op = "*"; left = Ast.IntegerLiteral 123; right = Ast.IntegerLiteral 23;
      };
      right = Ast.Identifier "hoge" ;
    }};
  ]
  (Lexer.newLexer "return hoge; return 123 * 23 - hoge" |> To_test.ast)

let test_expression_statements () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.ExpressionStatement {
      exp = Ast.InfixExpression {op = "*"; left = Ast.Identifier "hogehoge"; right = Ast.Identifier "iu"}
    };
    Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "*";
      left = Ast.IntegerLiteral 100;
      right = Ast.IntegerLiteral 12;
    }};
    Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "==";
      left = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 123; right = Ast.IntegerLiteral 21;};
      right = Ast.Identifier "hoge";
    }};
    Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "==";
      left = Ast.InfixExpression {op = ">"; left = Ast.IntegerLiteral 1; right = Ast.IntegerLiteral 2;};
      right = Ast.BooleanLiteral true;
    }};
    Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "==";
      left = Ast.InfixExpression {op = "<"; left = Ast.IntegerLiteral 1; right = Ast.IntegerLiteral 2;};
      right = Ast.BooleanLiteral true;
    }};
  ]
  (Lexer.newLexer "
  hogehoge * iu;
  100 * 12;
  123 * 21 == hoge;
  1 > 2 == true;
  (1 < 2) == true;
  " |> To_test.ast)


let test_prefix () = Alcotest.(check (list ast_testable))
  "same ast"
  [
    Ast.ExpressionStatement {exp = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;}};
    Ast.ExpressionStatement {
      exp = Ast.InfixExpression {
        op = "*";
        left = Ast.IntegerLiteral 1;
        right = Ast.InfixExpression {
          op = "+";
          left = Ast.InfixExpression {
            op = "-";
            left = Ast.IntegerLiteral 2;
            right = Ast.IntegerLiteral 1;
          };
          right = Ast.IntegerLiteral 3;
        };
      };
    };
    Ast.ExpressionStatement {exp = Ast.FunctionLiteral {
      prms = [Ast.Identifier "x"; Ast.Identifier "y";];
      body = Ast.BlockStatement {
        stms = [
          Ast.ExpressionStatement {
            exp = Ast.InfixExpression {
              op = "*";
              left = Ast.Identifier "x";
              right = Ast.Identifier "y";
            };
          };
        ]}
    }};
  ]
  (Lexer.newLexer "
    -1;
    1 * (2 - 1 + 3);
    fn (x, y) { x * y; };
  " |> To_test.ast)

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
        };
      };
    };
    Ast.ExpressionStatement {exp = Ast.CallExpression {
        fn = Ast.Identifier "aie";
        args = [
          Ast.InfixExpression {op = "+"; left = Ast.IntegerLiteral 1; right = Ast.IntegerLiteral 2;}
        ];
      }
    };
    Ast.ExpressionStatement {exp = Ast.CallExpression {
        fn = Ast.FunctionLiteral {
          prms = [
            Ast.Identifier "a";
            Ast.Identifier "b";
          ];
          body = Ast.BlockStatement {
            stms = [
              Ast.ReturnStatement {
                value = Ast.InfixExpression {
                  op = "*";
                  left = Ast.Identifier "a";
                  right = Ast.IntegerLiteral 2;
                }
              };
              Ast.ExpressionStatement {exp = Ast.Identifier "b"};
            ]
          };
        };
        args = [
          Ast.InfixExpression {op = "+"; left = Ast.IntegerLiteral 1; right = Ast.IntegerLiteral 2;};
          Ast.Identifier "aiu";
        ];
      }
    };
  ]
  (Lexer.newLexer "
  let a = -1 + -1;
  let aiai = -1 + 1 * 5 - 12 / 3;
  aie(1+2);
  fn(a, b) { return a * 2; b }(1+2, aiu);
  " |> To_test.ast)


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
