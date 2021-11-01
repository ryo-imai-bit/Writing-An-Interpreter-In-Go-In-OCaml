module Object = struct
include Ast

  type obj =
  | Integer of int
  | Empty
  | Error of string

  let newEnv = []

  let eq a b = match (a, b) with
  | Integer a, Integer b -> a = b
  | a, b -> a = b

  let objToString = function
  | Integer i -> string_of_int i
  | Empty -> "empty"
  | Error i -> i

  let pp ppf ob = Fmt.pf ppf "Object = %s" (objToString ob)

end
