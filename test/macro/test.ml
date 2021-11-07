open Interpreter
include Parser
include Ast
include Object
include Evaluator
include Env
include Macro

module To_test = struct
  let evals strlst =let rec reval = (function
    | [] -> []
    | h::t -> let (_, prg) = Parser.parseProgram (Parser.newParser (Lexer.newLexer h)) []
    in let p, e = (Macro.defineMacros prg.statements (Env.newEnv ()))
    in (Macro.expandMacros p e)::(reval t))
  in reval strlst

end

let ast_testable = Alcotest.testable Ast.pp Ast.eq

let test_macro_int () = Alcotest.(check (list (list ast_testable)))
  "same objs"
  [
    [
      Ast.ExpressionStatement {exp = Ast.InfixExpression {
      op = "-";
      left = Ast.InfixExpression {
        op = "*";
        left = Ast.IntegerLiteral 12;
        right = Ast.IntegerLiteral 3;
      };
      right = Ast.InfixExpression {
        op = "-";
        left = Ast.IntegerLiteral 9;
        right = Ast.IntegerLiteral 1;
      }
      }};
    ];
    [
      Ast.ExpressionStatement {exp = Ast.IfExpression {
        cond = Ast.PrefixExpression {
          op = "!";
          right = Ast.InfixExpression {
            op = ">";
            left = Ast.IntegerLiteral 10;
            right = Ast.IntegerLiteral 5;
          };
        };
        cons = Ast.BlockStatement {stms = [
          Ast.ExpressionStatement {
            exp = Ast.CallExpression {
              fn = Ast.Identifier "put";
              args = [Ast.StringLiteral "not greater"];
            }
          }
        ]};
        alt = Some (Ast.BlockStatement {stms = [
          Ast.ExpressionStatement {
            exp = Ast.CallExpression {
              fn = Ast.Identifier "put";
              args = [Ast.StringLiteral "greater"];
            }
          }
        ]});
      }
      }
    ];
    [
      Ast.ExpressionStatement {exp = Ast.InfixExpression {
        op = "-";
        left = Ast.CallExpression {fn = Ast.FunctionLiteral {
            prms = [
              Ast.Identifier "a";
            ];
            body = Ast.BlockStatement {
              stms = [
                Ast.ExpressionStatement {
                  exp = Ast.InfixExpression {
                    op = "-";
                    left = Ast.Identifier "a";
                    right = Ast.IntegerLiteral 2;
                  }
                };
              ]
            }
          };
          args = [
            Ast.IntegerLiteral 2;
              ]
        };
        right = Ast.CallExpression {fn =
          Ast.FunctionLiteral {
            prms = [
              Ast.Identifier "a";
            ];
            body = Ast.BlockStatement {
              stms = [
                Ast.ExpressionStatement {
                  exp = Ast.InfixExpression {
                    op = "-";
                    left = Ast.Identifier "a";
                    right = Ast.IntegerLiteral 1;
                  }
                };
              ]
            };
          };
          args = [
            Ast.IntegerLiteral 1;
          ]
          };
      }
      }
    ]
  ]
  (To_test.evals
    [
      "
let a = macro (a, b) { quote(unquote(a) - unquote(b)) };
a(12 * 3, 9 - 1);
    ";
    "
    let unless = macro(condition, consequence, alternative) {
      quote(if (!(unquote(condition))) {
          unquote(consequence);
      } else {
          unquote(alternative);
      });
    };

    unless(10 > 5, put(\"not greater\"), put(\"greater\"));
    ";
    "
    let a = macro (a, b) { quote(unquote(a)(2) - unquote(b)(1)) };
    a(fn(a) { a -2 }, fn(a) { a - 1 });
    ";
  ]
  )

(* Run it *)
let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "expansion macro int" `Slow test_macro_int;
        ];
    ]
