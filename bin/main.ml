(* let () = print_endline Interpreter.Lexer.a *)
include Interpreter

let () = try while true do Interpreter.run () done with End_of_file -> ()
