module Lexer = Lexer.Lexer
module Token = Token.Token
module Ast = Ast.Ast
module Parser = Parser.Parser
module Evaluator = Evaluator.Evaluator
module Object = Object.Object
module Env = Env.Env
module Builtin = Builtin.Builtin

let prompt = ">>"
let monkey = {|            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----''|}


let run () = print_string prompt;
  let input = read_line ()
  in let (ps, prg) = Parser.parseProgram (Parser.newParser (Lexer.newLexer input)) []
  in print_endline (match (Evaluator.evalProgram ps.errors prg (Env.newEnv)) with
    | Object.Empty -> ""
    | Object.Err i -> monkey ^ "\nWoops! We ran into some monkey business here!\n Error: " ^ i
    | obj -> Object.objToString obj)
