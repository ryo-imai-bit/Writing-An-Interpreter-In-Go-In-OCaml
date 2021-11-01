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

let test_exp_stm () = Alcotest.(check obj_testable)
  "same objs"
  (Object.Integer 2)
  (To_test.eval "1; 2;")

(* Run it *)
let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "eval Integer" `Slow test_exp_stm;
        ];
    ]
