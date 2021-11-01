open Interpreter
include Parser
include Ast
include Object
include Evaluator

module To_test = struct
  let evals strlst = let rec reval = function
    | [] -> []
    | h::t -> let prg = Parser.parseProgram (Parser.newParser (Lexer.newLexer h)) []
    in let obj = Evaluator.evalProgram prg (Object.newEnv)
    in obj::(reval t)
  in reval strlst
end

let obj_testable = Alcotest.testable Object.pp Object.eq

let test_exp_stms () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 2;
    Object.Boolean false;
    Object.Strng "hoge";
  ]
  (To_test.evals ["1; 2"; "true;false"; "\"hoge\""])

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
  (To_test.evals ["!false"; "!true"; "!12;"; "!\"hoge\""])

let test_infix () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Boolean true;
    Object.Boolean false;
  ]
  (To_test.evals ["(1 < 2) == true;"; "(33 < 0) != false";])


let test_infix_int () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Boolean true;
    Object.Boolean false;
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
  ]
  (To_test.evals ["\"brad\" + \" \" + \"pit\""; "\"anata \" + \"daredesuka\"";])

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

(* Run it *)
let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "eval ExpressionStatements" `Slow test_exp_stms;
          test_case "eval Prefix Integer" `Slow test_prefix_int;
          test_case "eval Prefix Boolean" `Slow test_prefix_bool;
          test_case "eval Infix" `Slow test_infix;
          test_case "eval Infix Integer" `Slow test_infix_int;
          test_case "eval Infix String" `Slow test_infix_str;
          test_case "eval Integer" `Slow test_int;
          test_case "eval String" `Slow test_str;
          test_case "eval Boolean" `Slow test_bool;
        ];
    ]
