(* $Id$ *)
signature ENV = 
sig
    (* type access *)
    type ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty}
val base_tenv : ty Symbol.table (* type environment *)
val base_venv : enventry Symbol.table (* value environment *)
end
