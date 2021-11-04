open Interpreter
include Parser
include Ast
include Object
include Evaluator
include Env

module To_test = struct
  let evals strlst = let rec reval = function
    | [] -> []
    | h::t -> let (ps, prg) = Parser.parseProgram (Parser.newParser (Lexer.newLexer h)) []
    in let obj = Evaluator.evalProgram ps.errors prg (Env.newEnv)
    in obj::(reval t)
  in reval strlst
end

let obj_testable = Alcotest.testable Object.pp Object.eq

let test_quote () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Quote (Ast.IntegerLiteral 5);
    Object.Quote (Ast.InfixExpression {
      op = "+";
      left = Ast.IntegerLiteral 5;
      right = Ast.IntegerLiteral 8;
    });
    Object.Quote (Ast.Identifier "foobar");
    Object.Quote (Ast.InfixExpression {
      op = "*";
      left = Ast.Identifier "foobar";
      right = Ast.Identifier "barfoo";
    });
    Object.Quote (Ast.InfixExpression {
      op = "*";
      left = Ast.Identifier "foobar";
      right = Ast.Identifier "barfoo";
    });
    Object.Quote (Ast.IntegerLiteral 8);
  ]
  (To_test.evals [
    "quote(5)";
    "quote(5 + 8)";
    "quote(foobar)";
    "quote(foobar * barfoo)";
    "let foobar = 8;
    quote(foobar * barfoo)";
    "let foobar = 8;
    quote(unquote(foobar))";
  ])

(* Run it *)
let () =
  let open Alcotest in
  run "quote unquote" [
      "quote", [
          test_case "quote" `Slow test_quote;
        ];
    ]
