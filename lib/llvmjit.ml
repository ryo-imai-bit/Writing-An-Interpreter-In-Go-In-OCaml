module Llvmjit = struct
  include Llvm
  include Ast

  exception Error of string

  let rec jit_exp exp double_type builder = match exp with
    | Ast.IntegerLiteral n -> Llvm.const_float double_type (float_of_int n)
    | Ast.InfixExpression i  -> (
          let lval = jit_exp i.left double_type builder in
          let rval = jit_exp i.right double_type builder in
          match i.op with
          | "+" -> Llvm.build_fadd lval rval "addtmp" builder
          | "-" -> Llvm.build_fsub lval rval "subtmp" builder
          | "*" -> Llvm.build_fmul lval rval "multmp" builder
          | "/" -> Llvm.build_fdiv lval rval "multmp" builder
          | _ -> raise (Error "unknown operator"))
    | _ -> raise (Error "unknown operator")

  let jit prg double_type builder = match prg with
    | h::[] -> (match h with
      | Ast.ExpressionStatement i -> jit_exp i.exp double_type builder
      | _ -> raise (Error "unknown statement"))
    | _ -> raise (Error "unknown statements")

  let run prg =
    let context = Llvm.global_context () in
    let the_module = Llvm.create_module context "my module" in
    let builder = Llvm.builder context in
    let double_type = Llvm.double_type context in
    let top_func_type = Llvm.function_type double_type [||] in
    let the_function = Llvm.declare_function "__sample" top_func_type the_module in
    let bb = Llvm.append_block context "entry" the_function in
    let () = Llvm.position_at_end bb builder in
    let ret_val = jit prg double_type builder in
    let () = ignore @@ Llvm.build_ret ret_val builder in
    let _ = Llvm_executionengine.initialize () in
    let the_execution_engine = Llvm_executionengine.create the_module in
    let () = Llvm.dump_value the_function in
    let fp = Llvm_executionengine.get_function_address
        (* function_name *)
        "__sample"
        (Foreign.funptr Ctypes.(void @-> returning double))
        the_execution_engine in
    print_endline @@ string_of_float @@ fp ()

end
