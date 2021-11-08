(* let () = print_endline Interpreter.Lexer.a *)
include Interpreter

let () =
  print_endline "HEllo! This is the Monkey programming language!\n";
  print_endline "Feel free to type in commands\n";
  try while true do Interpreter.run () done with End_of_file -> ()
