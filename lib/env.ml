module Env = struct
  include Object
  include Ast

  type env = {
    store: (string, Object.obj) Hashtbl.t;
    outer: env option;
  }

  let newEnv () = {
    store = Hashtbl.create 100;
    outer = None;
  }

  let getEnv = {
    store = Hashtbl.create 100;
    outer = None;
  }

  let newEnclosedEnv env = {
    store = Hashtbl.create 100;
    outer = Some env;
  }

  let rec get env key = if Hashtbl.mem env.store key
    then Some (Hashtbl.find env.store key)
    else (match env.outer with
      | Some env -> get env key
      | None -> None)

  let set env key value = Hashtbl.add env.store key value; value

let extendFunctionEnv prms args env = let nenv = newEnclosedEnv env
in let rec refe pms ags = match pms, ags with
  | [], [] -> ()
  | (Ast.Identifier idt)::it, obj::ot -> let _ = set nenv idt obj in refe it ot
  | _, _ -> raise (Failure "extend env failed")
in refe prms args; nenv

let extendMacroEnv prms args env = let nenv = newEnclosedEnv env
in let rec refe pms ags = match pms, ags with
  | [], [] -> ()
  | (Ast.Identifier idt)::it, obj::ot -> let _ = set nenv idt (Object.Quote obj) in refe it ot
  | _, _ -> raise (Failure "extend env failed")
in refe prms args; nenv

end
