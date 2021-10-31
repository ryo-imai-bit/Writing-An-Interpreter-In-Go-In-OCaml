open Interpreter
include Parser
include Ast
include Object

module To_test = struct
  let eval str = let stms = let (pr, stm) = Parser.parseProgram (Parser.newParser (Lexer.newLexer str)) []
    in if pr.errors = [] then stm else raise (Failure (Parser.errorsToString pr.errors))
  in Object.eval stms (Object.newEnv)
end

let obj_testable = Alcotest.testable Object.pp Object.eq

let test_exp_stm () = Alcotest.(check (list obj_testable))
  "same objs"
  [
    Object.Integer 1
  ]
  (To_test.eval "1;")

(* Run it *)
let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "eval Integer" `Slow test_exp_stm;
        ];
    ]
