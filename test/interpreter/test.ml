module To_test = struct
  let lex = Interpreter.Lexer.is_digit
end

let test_is_digit () = Alcotest.(check bool) "same bool" true (To_test.lex '1')

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "digit", [
          test_case "Lower case"     `Quick test_is_digit;
        ];
    ]
