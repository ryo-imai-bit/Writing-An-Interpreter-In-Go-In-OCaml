module Builtin = struct
  include Object

  type func = {
    store: (string, Object.obj) Hashtbl.t;
    outer: env option;
  }

  let rec get name = match name with
  | "len" -> (fun objs -> let rec len = function
    | [] -> 0
    | h::t -> 1 + len t) in (match objs with
      | h::[] -> Some (len h)
      | _ -> None))
  | _ -> None

end
