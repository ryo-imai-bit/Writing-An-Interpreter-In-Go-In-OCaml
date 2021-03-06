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
    in let obj = Evaluator.evalProgram ps.errors prg.statements (Env.newEnv ())
    in obj::(reval t)
  in reval strlst
end

let obj_testable = Alcotest.testable Object.pp Object.eq

let test_fibonacci () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 55;
  ]
  (To_test.evals [
    "let fib = fn (a) {
      if (a == 0) {return 0}
      else {
        if (a == 1) { return 1 }
        else { return fib (a-1) + fib (a-2) }
      }
    };
    fib (10)";
  ])

let test_exp_stms () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 2;
    Object.Boolean false;
    Object.Strng "hoge";
    Object.Integer (-1);
  ]
  (To_test.evals [
    "1; 2";
    "true;false";
    "\"hoge\"";
    "if (!false) { 1 * (2 - 3) / 1 } else { 12 - 3 }";
  ])

let test_ret_stms () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer (-27);
    Object.Strng "hogehoge";
  ]
  (To_test.evals ["return -2 * 12 - 3"; "12; return \"hogehoge\"; -123 * 1";])

let test_let_stms () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer (-27);
    Object.Strng "hogehoge";
  ]
  (To_test.evals ["let a = -2 * 12 - 3; a"; "let a = \"hogehoge\"; -123 * 1; let b = a; b;";])

let test_prefix_int () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer (-2);
    Object.Integer (0);
  ]
  (To_test.evals ["-2"; "-0";])

let test_prefix_bool () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Boolean true;
    Object.Boolean false;
    Object.Boolean false;
    Object.Boolean false;
  ]
  (To_test.evals [
    "!false";
    "!true";
    "!12;";
    "!\"hoge\""
  ])

let test_infix () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Boolean true;
    Object.Boolean false;
  ]
  (To_test.evals [
    "(1 < 2) == true;";
    "(33 < 0) != false";
  ])


let test_infix_int () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Boolean true;
    Object.Boolean true;
    Object.Boolean false;
    Object.Boolean true;
    Object.Integer 96;
    Object.Integer 5;
    Object.Integer (-71);
    Object.Integer (-19);
  ]
  (To_test.evals [
    "-122 == -122";
    "-33 != 12";
    "-12 > 3";
    "-333 < 500";
    "12 * 80 / 10";
    "23 / 4 - 0";
    "-23 - 4 * 12";
    "-23 + 4";
  ])

let test_infix_str () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Strng "brad pitt";
    Object.Strng "anata daredesuka";
    Object.Boolean false;
    Object.Boolean true;
    Object.Boolean true;
    Object.Boolean false;
  ]
  (To_test.evals [
    "\"brad\" + \" \" + \"pitt\"";
    "\"anata \" + \"daredesuka\"";
    "\"anata\" == \"daredesuka\"";
    "\"anata\" != \"daredesuka\"";
    "\"anata\" == \"anata\"";
    "\"anata\" != \"anata\"";
  ])

let test_infix_arr () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Arry [Object.Integer 1; Object.Integer 2; Object.Integer 3];
    Object.Boolean true;
    Object.Boolean false;
    Object.Boolean false;
    Object.Boolean true;
  ]
  (To_test.evals [
    "[1] + [2, 3]";
    "[2, 3] == [2, 3]";
    "[2, 3] != [2, 3]";
    "[1] == [2, 3]";
    "[1] != [2, 3]";
  ])

let test_if_exp () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 20;
    Object.Strng "hoge";
    Object.Integer 3;
    Object.Empty;
  ]
  (To_test.evals [
    "if (true) { 32 - 12; }";
    "if (!true) { 33 / 3; } else { \"hoge\" }";
    "if (12) { 2; 3; }";
    "if (true) {}";
  ])

let test_int () = Alcotest.(check (list obj_testable))
  "same objs"
  [Object.Integer 2]
  (To_test.evals ["1; 2"])

let test_str () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Strng "hogehoge";
    Object.Strng "orau==";
  ]
  (To_test.evals ["\"hogehoge\""; "\"orau==\"";])

let test_bool () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Boolean true;
    Object.Boolean false;
  ]
  (To_test.evals ["true;"; "false"])

let test_func_lit () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Func {
      prms = [Ast.Identifier "a"; Ast.Identifier "b"];
      body = Ast.BlockStatement {
        stms = [
          Ast.ExpressionStatement {
            exp = Ast.InfixExpression {
              op = "*";
              left = Ast.Identifier "a";
              right = Ast.Identifier "b";
            }
          }
        ]
      }
    };
  ]
  (To_test.evals ["fn (a, b) {a * b}"])

let test_call_exp () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 3;
    Object.Strng "hogehoge";
    Object.Strng "hoge";
    Object.Arry [
      Object.Integer 2;
      Object.Integer 4;
      Object.Integer 6;
    ];
  ]
  (To_test.evals [
    "fn(a,b) {a+b}(1,2)";
    "let a = \"hoge\"; fn (b, c) {let d = b + c; d}(\"hoge\", \"hoge\");";
    "let a = \"hoge\"; fn (b, c) {let a = b + c; a}(1, 3); a;";
    "let map = fn (arr, func) { if (arr == []) { return []; } else { return [func(first(arr))] + map(rest(arr), func) } };
      map([1, 2, 3], fn (int) { return int * 2 });
    ";
  ])

let test_array_lit () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Arry [
      Object.Integer 1;
      Object.Integer 2
    ];
    Object.Arry [
      Object.Strng "hogehoge";
      Object.Boolean true;
      Object.Strng "hoge";
    ];
    Object.Arry [
      Object.Arry [
        Object.Strng "hogehoge";
        Object.Boolean true;
        Object.Strng "hoge";
      ];
      Object.Integer 1;
    ];
  ]
  (To_test.evals [
    "[1, 2];";
    "[\"hogehoge\", true, \"hoge\"]";
    "[[\"hogehoge\", true, \"hoge\"], 1]";
  ])

let test_ind_exp () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 2;
    Object.Arry [
      Object.Strng "hogehoge";
      Object.Boolean true;
      Object.Strng "hoge";
    ];
    Object.Arry [
      Object.Integer 1;
      Object.Strng "hoge";
      Object.Integer 1;
    ];
  ]
  (To_test.evals [
    "[1, 2][1];";
    "let a = [\"hogehoge\", true, \"hoge\"]; let b = [a]; b[0]";
    "let a = fn (a, b) {[ [a, b, 1] ]}; a(1, \"hoge\")[0]";
  ])

let test_builtin () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 2;
    Object.Integer 3;
    Object.Func {
      prms = [Ast.Identifier "a"; Ast.Identifier "b"];
      body = Ast.BlockStatement {
        stms = [
          Ast.ExpressionStatement {
            exp = Ast.InfixExpression {
              op = "*";
              left = Ast.Identifier "a";
              right = Ast.Identifier "b";
            }
          }
        ]
      }
    };
    Object.Strng "hoge";
    Object.Arry [
      Object.Integer 22;
      Object.Strng "hoge";
    ];
    Object.Arry [
      Object.Integer 22;
      Object.Strng "hoge";
      Object.Integer 33;
    ];
  ]
  (To_test.evals [
    "len([1, 2]);";
    "len([fn(a) {a + 1;}, 22, \"hoge\"])";
    "first([fn(a, b) {a * b;}, 22, \"hoge\"])";
    "last([fn(a, b) {a * b;}, 22, \"hoge\"])";
    "rest([fn(a, b) {a * b;}, 22, \"hoge\"])";
    "push([22, \"hoge\"], 33)";
  ])


(* Run it *)
let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "eval Fibonacci" `Slow test_fibonacci;
          test_case "eval ExpressionStatements" `Slow test_exp_stms;
          test_case "eval ReturnStatements" `Slow test_ret_stms;
          test_case "eval LetStatements" `Slow test_let_stms;
          test_case "eval Prefix Integer" `Slow test_prefix_int;
          test_case "eval Prefix Boolean" `Slow test_prefix_bool;
          test_case "eval Infix" `Slow test_infix;
          test_case "eval Infix Integer" `Slow test_infix_int;
          test_case "eval Infix String" `Slow test_infix_str;
          test_case "eval Infix Array" `Slow test_infix_arr;
          test_case "eval If Expression" `Slow test_if_exp;
          test_case "eval Integer" `Slow test_int;
          test_case "eval String" `Slow test_str;
          test_case "eval Boolean" `Slow test_bool;
          test_case "eval FunctionLiteral" `Slow test_func_lit;
          test_case "eval CallExpression" `Slow test_call_exp;
          test_case "eval ArrayLiteral" `Slow test_array_lit;
          test_case "eval IndexExpression" `Slow test_ind_exp;
          test_case "eval Builtin" `Slow test_builtin;
        ];
    ]
