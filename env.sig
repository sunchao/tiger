signature ENV =
sig
    (* type access *)
    type ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level,
                                     label: Temp.label,
                                     formals: ty list, result: ty}
val base_tenv : ty Symbol.table (* type environment *)
val base_venv : enventry Symbol.table (* value environment *)
end
