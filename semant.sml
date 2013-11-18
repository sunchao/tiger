structure Semant : SEMANT =
struct

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table

structure A = Absyn
structure E = Env
structure S = Symbol
structure T = Types
structure R = Translate

val err = ErrorMsg.error

(* Return value when type checking failed. We do this for now.
However, it makes more sense to set it differently so that type
checking won't emit more unnecessary information *)
val err_result = {exp=R.errexp,ty=T.NIL}

type tdecs = {name: Symbol.symbol, ty: T.ty, pos: int}


(* N.B: entries in tenv will sometimes be named type,
therefore we need to use actual_ty whenever possible. *)

(* we cannot do T.RECORD here, since it might be recursive *)

fun actual_ty (ty:T.ty,pos) =
    case ty of
      T.NAME(sym,tyref) =>
      (case (!tyref) of
         NONE => (err pos ("undefined type " ^ S.name(sym)); T.NIL)
       | SOME(ty) => actual_ty (ty,pos))
    | T.ARRAY(t,u) => T.ARRAY(actual_ty(t,pos),u)
    | _ => ty

fun type_mismatch (expected, actual, pos) =
    let in
    err pos ("expected " ^ expected ^ " type, but " ^ actual ^ " found");
    err_result
    end

fun type2str (ty: T.ty) =
    case ty of
      T.NIL => "nil"
    | T.UNIT => "unit"
    | T.INT => "int"
    | T.STRING => "string"
    | T.ARRAY(t,_) => "array of " ^ type2str(t)
    | T.NAME(sym,_) => "name of " ^ S.name(sym)
    | T.RECORD(_,_) => "record" (* TODO: improve it *)

fun checktype (t1:T.ty, t2:T.ty, pos) =
    let val t = actual_ty(t1,pos) in
      if (t <> t2) then
        case (t,t2) of
          (T.RECORD(_,_),T.NIL) => ()
        | (T.NIL,T.RECORD(_,_)) => ()
        | (_,_) => (type_mismatch(type2str(t),type2str(t2),pos);())
      else ()
    end

fun checkdup (nil,nil) = ()
  | checkdup (name::rest, pos::poss) =
    if (List.all (fn (x) => (name <> x)) rest) then checkdup(rest,poss)
    else err pos ("duplicated definition: " ^ S.name name)

fun transExp (venv,tenv,level,break) =
    let fun trexp (A.NilExp) = {exp=R.nilexp,ty=T.NIL}

      | trexp (A.IntExp(n)) = {exp=R.intlit(n),ty=T.INT}

      | trexp (A.StringExp(s,_)) = {exp=R.strlit(s),ty=T.STRING}

      | trexp (A.OpExp{left,oper=oper,right,pos}) =
        let
          val {exp=le,ty=lt} = trexp left
          val {exp=re,ty=rt} = trexp right

          datatype KIND = ARITH | COMP | EQ

          fun classify (oper) : KIND =
              case oper of
                A.PlusOp => ARITH
              | A.MinusOp => ARITH
              | A.TimesOp => ARITH
              | A.DivideOp => ARITH
              | A.LtOp => COMP
              | A.GtOp => COMP
              | A.LeOp => COMP
              | A.GeOp => COMP
              | A.EqOp => EQ
              | A.NeqOp => EQ

          fun checkarith () = (checkint(lt,pos); checkint(rt,pos))

          fun checkeq () =
              case lt of
                T.INT => checktype(T.INT,rt,pos)
              | T.STRING => checktype(T.STRING,rt,pos)
              | T.ARRAY(t,u) => checktype(T.ARRAY(t,u), rt, pos)
              | T.RECORD(fs,u) => checktype(T.RECORD(fs,u),rt,pos)
              | _ =>
                (err pos
                     ("can only check equality on "
                      ^ "int, string, array or record types,"
                      ^ " found " ^ type2str(lt)))

          fun checkcomp () =
              case lt of
                T.INT => checktype(T.INT, rt, pos)
              | T.STRING => checktype(T.STRING, rt, pos)
              | _ =>
                (err pos ("can only compare int or string "
                          ^ "for ordering, found " ^ type2str(lt)))
        in
          case classify(oper) of
            ARITH => (checkarith(); {exp=R.binop(oper,le,re),ty=T.INT})
          | COMP => (checkcomp(); {exp=R.relop(oper,le,re),ty=T.INT})
          | EQ => (checkeq(); {exp=R.relop(oper,le,re),ty=T.INT})
        end

      | trexp (A.VarExp(var)) = trvar var

      | trexp (A.RecordExp{fields,typ,pos}) =
        (case S.look(tenv,typ) of
          NONE =>
          (err pos ("record type " ^ S.name typ ^ " not found");
           err_result)
        | SOME(t) =>
          case actual_ty(t,pos) of
            T.RECORD(ftypes,u) =>
            let
              val flist = map (fn (_,e,pos) => (trexp e,pos)) fields
              val fts = map (fn ({exp,ty},pos) => (ty,pos)) flist
              val fes = map (fn ({exp,ty},_) => exp) flist
            in
              checkrecord(ftypes,fts,pos);
              {exp=R.record(fes),ty=T.RECORD(ftypes,u)}
            end
          | t => type_mismatch("record",type2str(t),pos))

      | trexp (A.SeqExp(exps)) =
        let val es = map (fn (exp,_) => #exp (trexp exp)) exps
            val ty = if List.null exps
                     then T.UNIT else #ty (trexp (#1 (List.last exps)))
        in {exp=R.sequence(es),ty=ty} end

      | trexp (A.AssignExp{var,exp,pos}) =
        let val {exp=vexp,ty=vty} = trvar var
            val {exp=eexp,ty=ety} = trexp exp in
          checktype(vty,ety,pos);
          {exp=R.assign(vexp,eexp),ty=T.UNIT}
        end

      | trexp (A.IfExp{test, then', else', pos}) =
        let val {exp=then_exp,ty=then_ty} = trexp then'
            val {exp=test_exp,ty=test_ty} = trexp test in
          checktype(T.INT,test_ty,pos);
          let val else_exp =
                  case else' of
                    NONE => (checktype(T.UNIT,then_ty,pos);NONE)
                  | SOME(e) =>
                    let val {exp=exp,ty=else_ty} = trexp e in
                      checktype(then_ty,else_ty,pos); SOME(exp)
                    end
          in {exp=R.ifelse(test_exp,then_exp,else_exp),ty=then_ty}
          end
        end

      | trexp (A.WhileExp{test,body,pos}) =
        let
          val done_label = Temp.newlabel()
          val {exp=test_exp,ty=test_ty} = trexp test
          val {exp=body_exp,ty=body_ty} =
              transExp(venv,tenv,level,done_label) body
        in
          checktype(T.INT,test_ty,pos);
          checktype(T.UNIT,body_ty,pos);
          {exp=R.loop(test_exp,body_exp,done_label),ty=T.UNIT}
        end

      | trexp (A.BreakExp(_)) = {exp=R.break(break),ty=T.UNIT}

      | trexp (A.LetExp{decs,body,pos}) =
        let
          val {venv=venv',tenv=tenv',exps=dexps} =
              foldl (fn (d,{venv,tenv,exps}) =>
                        let val {venv=venv1,tenv=tenv1,exps=exps1} =
                                transDec(venv,tenv,level,d,break)
                        in {venv=venv1,tenv=tenv1,exps=exps@exps1} end)
                    {venv=venv,tenv=tenv,exps=nil} decs;
          val {exp=bexp,ty=bty} = transExp(venv',tenv',level,break) body
          in {exp=R.letexp(dexps,bexp),ty=bty}
        end

      | trexp (A.ArrayExp{typ,size,init,pos}) =
        (case S.look(tenv,typ) of
           NONE =>
           (err pos ("type " ^ S.name(typ) ^ " not found");
            err_result)
         | SOME(t) =>
           let val at = actual_ty(t,pos) in
             case at of
               T.ARRAY(aat,g) =>
               let val {exp=size_exp,ty=size_ty} = trexp size
                   val {exp=init_exp,ty=init_ty} = trexp init in
                 checktype(T.INT,size_ty,pos);
                 checktype(aat,init_ty,pos);
                 {exp=R.array(size_exp,init_exp),ty=T.ARRAY(aat,g)}
               end
             | t => type_mismatch("array", type2str(t), pos)
           end)

      | trexp (A.ForExp{var,escape,lo,hi,body,pos}) =
        (* rewrite to let .. while *)
        let
          val limit = S.symbol "limit"
          val ivar = A.SimpleVar(var,pos)
          val limitvar = A.SimpleVar(limit,pos)
          val letdecs =
              [A.VarDec{
                 name=var,
                 escape=escape,
                 typ=NONE,
                 init=lo,
                 pos=pos},
               A.VarDec{
                 name=limit,
                 escape=ref false,
                 typ=NONE,
                 init=hi,
                 pos=pos}]

          val loop =
              A.WhileExp{
                test=A.OpExp{
                    left=A.VarExp(ivar),
                    oper=A.LeOp,
                    right=A.VarExp(limitvar),
                    pos=pos
                  },
                body=A.SeqExp[(body,pos),
                              (A.AssignExp{
                                var=ivar,
                                exp=A.OpExp{
                                  left=A.VarExp(ivar),
                                  oper=A.PlusOp,
                                  right=A.IntExp(1),
                                  pos=pos
                              },pos=pos},pos)],
                pos=pos}
        in
          trexp (A.LetExp{decs=letdecs,body=loop,pos=pos})
        end

      | trexp (A.CallExp{func,args,pos}) =
        case S.look(venv,func) of
          NONE =>
          (err pos ("function " ^ S.name(func) ^ " is not defined");
           err_result)
        | SOME(E.VarEntry{access,ty}) =>
          (err pos ("function expected, but variable of type: "
                    ^ type2str(ty) ^ " found"); err_result)
        | SOME(E.FunEntry{level=funlevel,label,formals,result}) =>
          (* if result is unit, we return a stm, otherwise return a exp *)
          let
            val argexps = map trexp args in
            checkformals(formals,argexps,pos);
            {exp=R.call(level,funlevel,label,map #exp argexps,result=T.UNIT),
             ty=actual_ty(result,pos)}
          end

          (* usage of simple local variable *)
        and trvar (A.SimpleVar(id,pos)) =
            (case S.look(venv,id)
             of SOME(E.VarEntry{access,ty}) =>
                {exp=R.simpleVar(access,level),ty=actual_ty(ty,pos)}
              | SOME(_) =>
                (err pos ("expected variable, but function found"); err_result)
              | NONE =>
                (err pos ("undefined variable: " ^ S.name id); err_result))

          | trvar (A.FieldVar(v,id,pos)) =
            let val {exp,ty} = trvar v in
              case ty of
                T.RECORD(flist,_) =>
                (case List.find (fn x => (#1x) = id) flist of
                  NONE =>
                  (err pos ("id: " ^ S.name id ^ " not found");
                   {exp=R.errexp,ty=T.NIL})
                | SOME(rv) =>
                    {exp=R.fieldVar(exp,id,map #1 flist),
                     ty=actual_ty(#2rv,pos)})
              | t =>
                (err pos ("expected record type, but "
                          ^ type2str(t) ^ " found"); err_result)
            end

          | trvar (A.SubscriptVar(v,e,pos)) =
            let val {exp,ty} = trvar v in
              case actual_ty(ty,pos) of
                T.ARRAY(t,_) =>
                let val {exp=exp1,ty=ty1} = trexp e in
                  case ty1 of
                    T.INT => {exp=R.subscriptVar(exp,exp1),ty=t}
                  | t =>
                    (err pos ("array subscript should be int, but "
                              ^ type2str(t) ^ " found"); err_result)
                end
              | t => type_mismatch("array", type2str(t), pos)
            end

        and checkint (ty,pos) =
            case ty of
              T.INT => ()
            | t => (type_mismatch("INT", type2str(t), pos);())

        and checkformals (ts,es,pos) =
            let val le = length(es)
                val lt = length(ts)
            in if (lt <> le) then
                 err pos (Int.toString(lt) ^ " args needed, but "
                          ^ Int.toString(le) ^ " given")
               else app (fn (t,e) => checktype(t, #ty e, pos))
                        (ListPair.zip(ts,es))
            end

        and checkrecord (ts,fs,pos) =
            if (length(ts) <> length(fs)) then
              err pos
                  (Int.toString(length(ts)) ^ " fields needed, but "
                   ^ Int.toString(length(fs)) ^ " given")
            else
              app (fn (t,(ty,pos)) => checktype(#2t, ty, pos))
                  (ListPair.zip(ts,fs))
    in trexp
    end

and transDec (venv,tenv,level,A.VarDec{name,escape,typ,init,pos},break) =
    let
      val {exp,ty} = transExp(venv,tenv,level,break) init
      val acc = Translate.allocLocal(level)(!escape)
      val varexp = R.simpleVar(acc,level)
    in
      case typ of
        NONE =>
        (if (ty = T.NIL) then (err pos "can't use nil") else ();
         {tenv=tenv,
          venv=S.enter(venv,name,E.VarEntry{access=acc,ty=ty}),
          exps=[R.assign(varexp,exp)]})

      | SOME((tname,pos)) =>
        case S.look (tenv,tname) of
          NONE =>
          (err pos ("type " ^ S.name tname ^ " not found");
           {tenv=tenv,
            venv=S.enter(venv,name,E.VarEntry{access=acc,ty=ty}),
            exps=[]})

        | SOME(dty) =>
          let
            val at = actual_ty(dty,pos) in
            checktype(dty,ty,pos);
            {tenv=tenv,
             venv=S.enter(venv,name,E.VarEntry{access=acc,ty=at}),
             exps=[R.assign(varexp,exp)]}
          end
    end

  (*  Type declaration maybe recursive, therefore
   *  we need two steps: first fill the tenv with
   *  "empty" headers, then pass the tenv to
   *  transTy and get the values  *)
  | transDec (venv,tenv,level,A.TypeDec(tdecs),break) =
    let
      val tenv' =
        foldl (fn ({name,...},env) =>
          S.enter(env,name,T.NAME(name,ref NONE))) tenv tdecs

      val tenv'' =
        foldl (fn ({name,ty,...},env) =>
          (case S.look(env,name) of
             SOME(T.NAME(n,r)) =>
             (r := SOME(transTy(env,ty)); env))) tenv' tdecs

        fun checkcycle(seen,to,pos) =
            case to of
              NONE => (err pos "type not found"; false)
            | SOME(t) =>
              case t of
                T.NAME(s2,r) =>
                if (List.all (fn (x) => x <> s2) seen)
                then checkcycle(s2::seen,!r,pos) else false
              | _ => true

        (* Two options:
           1. check all errors and print;
           2. stop at first error and print the message.
         *)
        fun checkeach(nil) = ()
          | checkeach({name,ty,pos}::ds) =
            case S.look(tenv'',name) of
              SOME(T.NAME(_,r)) =>
              if (not (checkcycle([name], !r, pos))) then
                (err pos ("name type: " ^ S.name(name)
                          ^ " involved in cyclic definition."))
              else checkeach(ds)

    (* every cycle on mutually recursive types must include
     * a array or record *)
    in checkeach(tdecs);
       checkdup(map #name tdecs,map #pos tdecs);
       {venv=venv, tenv=tenv'',exps=nil}
    end

  (* Things to check for a function:
     1. result type exists and match,
     2. formal type exists and match,
     3. no duplicate formal names,
     4. body type checks. *)
  | transDec (venv,tenv,level,A.FunctionDec(fundecs),break) =
    let
      (* first pass on a fundec, check formal types,
       * and store header info in the venv. *)
      fun transfun ({name,params,result,body,pos},env) =
        let val rt =
            case result of
              NONE => T.UNIT (* procedure - should return unit *)
            | SOME(t,pos) =>
              (case S.look(tenv,t) of
                 SOME t => t
               | NONE =>
                 (err pos ("result type: " ^ S.name(t) ^ " not found.");
                  T.NIL))
          val fs =
            map (fn ({typ,name,...}: A.field) =>
              case S.look(tenv,typ) of
                SOME t => t
              | NONE =>
                (err pos ("type: " ^ S.name typ
                          ^ " for method parameter: "
                          ^ S.name name ^ " not found"); T.UNIT)) params

          val es = map (fn {escape,...} => !escape) params
        in
          checkdup(map #name params, map #pos params);
          S.enter(env,name,
                  E.FunEntry{
                    level=Translate.newLevel{parent=level,
                                             name=name,
                                             formals=es},
                    label=name,formals=fs,result=rt})
        end
    in
      let
        val venv' = foldl transfun venv fundecs

        (* second pass on a fundec: do type checking, put
         * VarEntry on venv, and check body *)
        fun transbody ({name,params,result,body,pos},{tenv,venv}) =
            let
              val SOME(E.FunEntry{result,level=newlevel,...}) =
                  S.look(venv',name)

              fun transparam ({name,escape,typ,pos},access) =
                  case S.look(tenv,typ) of
                      SOME t => {access=access,name=name,ty=t}
                    | NONE =>
                      (err pos ("method param type: "
                                ^ S.name(typ) ^ " not found.");
                       {access=access,name=name,ty=T.UNIT})
              val params' = ListPair.map
                transparam (params,Translate.formals newlevel)
              val venv'' =
                  (foldl (fn ({access,name,ty},env) =>
                    S.enter(env,name,E.VarEntry{access=access,ty=ty}))
                         venv' params')
              val {exp,ty} = transExp(venv'',tenv,newlevel,break) body
            in checktype(result,ty,pos);
               Translate.procEntryExit(newlevel,exp);
               {venv=venv',tenv=tenv}
            end
      in checkdup(map #name fundecs,map #pos fundecs);
        let val {venv,tenv} = foldl transbody {tenv=tenv,venv=venv} fundecs
        in {venv=venv,tenv=tenv,exps=nil} end
      end
    end

and transTy (tenv,A.NameTy(sym,pos)) =
    (* detect mutually recursive types *)
    (case S.look(tenv,sym) of SOME(t) => t)

  | transTy (tenv,A.RecordTy(fields)) =
    (checkdup(map #name fields, map #pos fields);
     T.RECORD(
     (map (fn {name,escape,typ,pos} =>
              case S.look(tenv,typ) of
                SOME(t) => (name,t)
              | NONE => (err pos
                             ("undefined type " ^ S.name typ);
                         (name,T.UNIT))) fields), ref()))

  | transTy (tenv,A.ArrayTy(sym,pos)) =
    case S.look(tenv,sym) of
      SOME(t) => T.ARRAY(t,ref())
    | NONE => (err pos ("undefined type " ^ S.name sym);
               T.ARRAY(T.NIL,ref()))

fun transProg(exp:Absyn.exp) =
    let
      val _ = R.reset() (* clear fragment list *)
      val mainlevel =
          R.newLevel{parent=R.outermost,
                             name=Temp.namedlabel "main",
                             formals=[]}
      val {exp,ty} =
          transExp(E.base_venv,E.base_tenv,mainlevel,Temp.newlabel())(exp)
    in
      R.procEntryExit(mainlevel,exp);
      R.getResult()
    end
end
