module Env = struct
  include Object

  type env = {
    store: (string, Object.obj) Hashtbl.t;
    outer: env option;
  }

  let newEnv = {
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

end
