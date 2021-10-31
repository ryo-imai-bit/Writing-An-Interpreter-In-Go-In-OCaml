module Object = struct
include Ast

  type obj =
  | Integer of int

  let eval _ _ = [Integer 1]

  let newEnv = []

  let eq a b = match (a, b) with
  | Integer a, Integer b -> a = b

  let objToString = function
  | Integer i -> string_of_int i

  let pp ppf ob = Fmt.pf ppf "Object = %s" (objToString ob)

end
