structure Env : ENV =
struct

structure S = Symbol
structure T = Types

type ty = Types.ty
datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                  | FunEntry of {level: Translate.level,
                                 label: Temp.label,
                                 formals: ty list, result: ty}

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
    List.foldr
      (fn ((name,formals,result),env) =>
          let val label = Temp.namedlabel name in
            S.enter (env,S.symbol(name),
                     FunEntry{level=Translate.newLevel
                                        {parent=Translate.outermost,
                                         name=label,
                                         formals=map (fn _ => false) formals},
                              label=label,
                              formals=formals,
                              result=result})
          end)
      S.empty base_funs

end


