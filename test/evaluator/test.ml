open Interpreter
include Parser
include Ast
include Object
include Evaluator

module To_test = struct
  let eval str = let prg = Parser.parseProgram (Parser.newParser (Lexer.newLexer str)) []
  in Evaluator.evalProgram prg (Object.newEnv)
end

let obj_testable = Alcotest.testable Object.pp Object.eq

let test_exp_stms () = Alcotest.(check obj_testable)
  "same objs"
  (Object.Integer 2)
  (To_test.eval "1; 2")

let test_int () = Alcotest.(check obj_testable)
  "same objs"
  (Object.Integer 2)
  (To_test.eval "1; 2")

let test_str () = Alcotest.(check obj_testable)
  "same objs"
  (Object.Strng "hogehoge")
  (To_test.eval "\"hogehoge\"")

let test_bool () = Alcotest.(check obj_testable)
  "same objs"
  (Object.Boolean false)
  (To_test.eval "true; false")

(* Run it *)
let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "eval ExpressionStatements" `Slow test_exp_stms;
          test_case "eval Integer" `Slow test_int;
          test_case "eval String" `Slow test_str;
          test_case "eval Boolean" `Slow test_bool;
        ];
    ]
