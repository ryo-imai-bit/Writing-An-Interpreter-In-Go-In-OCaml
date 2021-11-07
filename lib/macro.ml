module Macro = struct
include Ast
include Env
include Object
include Evaluator

  let isMacro node = match node with
  | Ast.LetStatment i -> (match i.value with
    | Ast.MacroLiteral _ -> true
    | _ -> false)
  |_ -> false
  let defineMacros prg env = let rec addMacro pg ev: Ast.statement list * Env.env = match pg with
  | [] -> (pg, ev)
  | h::t -> (match h with
    | Ast.LetStatment {
        idt = Ast.Identifier i; value = Ast.MacroLiteral m
      } ->let _ = Env.set ev i (Object.Macro {prms = m.prms; body = m.body;}) in addMacro t ev
    | _ -> let p, e = addMacro t ev in h::p, e)
  in addMacro prg env

  let expandMacros prg env = let modifier nd = match nd with
    | Ast.CallExpression {fn = Ast.Identifier idt; args} -> (match Env.get env idt with
      | Some (Object.Macro m) -> let nenv = Env.extendMacroEnv m.prms args env
        in (match Evaluator.evalStatement m.body nenv with
        | (Object.Quote q, _) -> q
        | _ -> nd)
      | _ -> nd)
    | _ -> nd
  in let rec expandMacro = function
    | [] -> []
    | h::t -> (Ast.modifyStatement modifier h)::(expandMacro t)
  in expandMacro prg

end
