module Builtin = struct
  include Object

  (* string -> (obj list -> obj option) option *)
  let get name :(Object.obj list -> Object.obj option) option = match name with
  | "len" -> Some (fun objs -> let rec len = function
      | [] -> 0
      | _::t -> 1 + len t
    in (match objs with
      | (Object.Arry i)::[] -> Some (Object.Integer (len i))
      | _ -> None))
  | "first" -> Some (fun objs -> (match objs with
    | (Object.Arry i)::[] -> (match i with
      | h::_ -> Some h
      | _ -> None)
    | _ -> None))
  | "last" -> Some (fun objs -> (match objs with
    | (Object.Arry i)::[] -> let rec last = function
        | [] -> None
        | h::[] -> Some h
        | _::t -> last t
      in last i
    | _ -> None))
  | "rest" -> Some (fun objs -> (match objs with
    | (Object.Arry i)::[] -> (match i with
      | _ ::t -> Some (Object.Arry t)
      | _ -> None)
    | _ -> None))
  | "push" -> Some (fun objs -> (match objs with
    | (Object.Arry i)::obj::[] -> Some (Object.Arry (i@[obj]))
    | _ -> None))
  | "put" -> Some (fun objs -> (match objs with
    | (Object.Strng i)::[] -> Printf.printf "%s\n" i ; Some Object.Empty
    | _ -> None))
  | _ -> None

end
