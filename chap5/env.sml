(* $Id$ *)
structure Env : ENV =
struct

structure S = Symbol
structure T = Types

type ty = Types.ty
datatype enventry = VarEntry of {ty: ty}
                  | FunEntry of {formals: ty list, result: ty}

type fun_info = string * ty list * ty

val base_tenv
  = S.enter(S.enter(
            S.empty,
            S.symbol("int"),
            T.INT),
            S.symbol("string"),
            T.STRING)


val base_funs : fun_info list =
    [("print",[T.STRING],T.UNIT),
     ("flush",[],T.UNIT),
     ("getchar",[],T.STRING),
     ("ord",[T.STRING],T.INT),
     ("chr",[T.INT],T.STRING),
     ("size",[T.STRING],T.INT),
     ("substring",[T.STRING,T.INT,T.INT],T.INT),
     ("concat",[T.STRING,T.STRING],T.STRING),
     ("not",[T.INT],T.INT),
     ("exit",[T.INT],T.UNIT)]

val base_venv =
    List.foldr (fn (f_info,env) =>
                   (S.enter (env,S.symbol(#1f_info),
                             FunEntry{formals=(#2f_info),result=(#3f_info)})))
               S.empty base_funs

end


