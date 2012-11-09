structure Semant : SEMANT = 
struct

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table

structure A = Absyn
structure E = Env
structure S = Symbol
structure T = Types

val err = ErrorMsg.error
val err_type = T.UNIT
                                  
type expty = {exp: Translate.exp, ty: T.ty}

type tdecs = {name: Symbol.symbol, ty: T.ty, pos: int}


(* N.B: entries in tenv will sometimes be named type,
 * therefore we need to use actual_ty whenever possible *)

fun actual_ty(ty:T.ty,pos) = 
    case ty of
      T.NAME(sym,tyref) =>
      (case (!tyref) of
         NONE => (err pos ("undefined type " ^ S.name(sym)); T.NIL)
       | SOME(ty) => actual_ty (ty,pos))
    | T.ARRAY(t,u) => T.ARRAY(actual_ty(t,pos),u)
    | _ => ty

fun type2str(ty: T.ty) =
    case ty of
      T.NIL => "NIL"
    | T.UNIT => "UNIT"
    | T.INT => "INT"
    | T.STRING => "STRING"
    | T.ARRAY(t,_) => "ARRAY of " ^ type2str(t)
    | T.NAME(sym,_) => "NAME of " ^ S.name(sym)
    | T.RECORD(_,_) => "RECORD" (* improve it! *)

fun checktype(t1:T.ty, t2:T.ty, pos) =
    let val t = actual_ty(t1,pos) in
      if (t <> t2) then
        case (t,t2) of 
          (T.RECORD(_,_),T.NIL) => ()
        | (T.NIL,T.RECORD(_,_)) => ()
        | (_,_) => 
          err pos 
              ("type mismatch: " ^ 
               "expected " ^ type2str(t) ^
               " but " ^ type2str(t2) ^ " found")
      else ()
    end

fun checkdup([], []) = ()
  | checkdup(name::rest, pos::poss) =
    if (List.all (fn (x) => (name <> x)) rest) then checkdup(rest,poss)
    else err pos ("duplicated definition " ^ S.name(name))
    
    
fun transExp(venv, tenv) =
    let fun trexp(A.NilExp) = {exp=(),ty=T.NIL}

          | trexp(A.IntExp(_)) = {exp=(),ty=T.INT}

          | trexp(A.StringExp(_)) = {exp=(),ty=T.STRING}

          | trexp(A.OpExp{left,oper=_,right,pos}) =
            (checkint(trexp left,pos);
             checkint(trexp right,pos);
             {exp=(),ty=T.INT})

          | trexp(A.VarExp(var)) = trvar var

          | trexp(A.RecordExp{fields,typ,pos}) =
            let val rt = S.look(tenv,typ) in
              case rt of
                NONE => 
                (err pos ("record type " ^ S.name(typ) ^ " not defined");
                 {exp=(),ty=T.NIL}) (* FIXME: not sure T.NIL will work *)
              | SOME(t) => 
                let val at = actual_ty(t,pos) in
                  (case at of 
                     T.RECORD(ftypes,u) =>
                     (checkRecord(ftypes,fields,pos);
                      {exp=(),ty=T.RECORD(ftypes,u)})
                   | t =>
                     (err pos ("expected record type, but " ^ 
                               type2str(t) ^ " found.");
                      {exp=(), ty=T.NIL}))
                end
            end

          | trexp(A.SeqExp(exps)) = 
            (case exps of
               nil => {exp=(),ty=T.UNIT}
             | e :: nil => (trexp (#1 e))
             | hd :: tl => (trexp (#1 hd); trexp (A.SeqExp(tl))))

          | trexp(A.AssignExp{var,exp,pos}) =
            (checktype(#ty (trvar var), #ty (trexp exp), pos);
             {exp=(),ty=T.UNIT})

          | trexp(A.IfExp{test, then', else', pos}) = 

             let val lt = #ty (trexp then') in
               checktype(T.UNIT,lt,pos);
               checktype(T.INT,#ty (trexp test),pos);
               case else' of
                 NONE => ()
               | SOME(elseexp) =>
                 if (lt <> #ty (trexp elseexp)) then
                   (err pos ("types from then (" ^ 
                             type2str(lt) ^ ") and else (" ^
                             type2str(#ty (trexp elseexp)) ^ ") differ"))
                 else ();
               {exp=(),ty=lt} (* use type from then branch as result *)
             end
                      
          | trexp(A.WhileExp{test,body,pos}) =
            (checktype(T.INT, #ty (trexp test), pos);
             checktype(T.UNIT, #ty (trexp body), pos);
             {exp=(),ty=T.UNIT})

          | trexp(A.LetExp{decs,body,pos}) =
            let val {venv=venv',tenv=tenv'} =  
              foldl (fn (d,{venv,tenv}) => transDec(venv,tenv,d))
                    {venv=venv,tenv=tenv} decs
            in 
                (transExp(venv',tenv') body)
            end

          | trexp(A.ArrayExp{typ,size,init,pos}) = 
            (case S.look(tenv,typ) of
               NONE => 
               (err pos ("ARRAY type '" ^ S.name(typ) ^ "' not found");
                {exp=(),ty=T.UNIT})
             | SOME(t) => 
                 (case actual_ty(t,pos) of
                    T.ARRAY(t,g) => 
                    (checktype(T.INT,#ty (trexp size), pos);
                     checktype(t,#ty (trexp init),pos);
                     {exp=(),ty=T.ARRAY(actual_ty(t,pos),g)})
                  | t => 
                    (err pos ("expected ARRAY type, but '" ^ 
                              type2str(t) ^ "' found");
                     {exp=(),ty=T.UNIT})))
                
          | trexp(A.ForExp{var,escape,lo,hi,body,pos}) =
            (checktype(T.INT,#ty (trexp lo),pos);
             checktype(T.INT,#ty (trexp hi),pos);
             let val venv' = S.enter(venv,var,E.VarEntry{ty=T.INT}) in
               checktype(T.UNIT, #ty (transExp(venv',tenv) body),pos);
               {exp=(),ty=T.UNIT}
             end)

          | trexp(A.BreakExp(_)) = {exp=(),ty=T.NIL}

          | trexp(A.CallExp{func,args,pos}) =
            case S.look(venv,func) of
              NONE => 
              (err pos ("FUNCTION '" ^ S.name(func) ^ "' is not defined");
               {exp=(),ty=T.NIL}) (* safest type to use *)
            | SOME(E.FunEntry{formals,result}) => 
              (checkFormals(formals,args,pos);
               {exp=(),ty=result})
            | SOME(E.VarEntry{ty}) => 
              (err pos ("expected FUNCTION type, but VAR type '" ^ 
               type2str(ty) ^ "' found.");
               {exp=(),ty=T.NIL})
              
        and trvar(A.SimpleVar(id,pos)) = 
            (case S.look(venv,id)
              of SOME(E.VarEntry{ty}) =>
                 {exp=(),ty=actual_ty(ty,pos)}
               | SOME(_) => 
                 (err pos ("expected VAR type, but FUNCTION type found.");
                  {exp=(), ty=T.NIL})
               | NONE => 
                 (err pos ("undefined variable " ^ S.name id);
                  {exp=(),ty=T.NIL})) (* FIXME: what type to use? *)

          | trvar(A.FieldVar(v,id,pos)) = 
            let val r = trvar v in
              case #ty r of 
                T.RECORD(flist,_) =>
                (case List.find (fn x => (#1 x) = id) flist of
                   SOME(rv) => {exp=(),ty=(#2 rv)}
                 | NONE => (err pos ("id '" ^ S.name id ^ "' not found.");
                            {exp=(),ty=T.NIL}))
              | t => (err pos ("expected RECORD, but " ^
                    type2str(t) ^ " found"); {exp=(),ty=T.NIL})
            end

          | trvar(A.SubscriptVar(v,e,pos)) =
            let val r = trvar v in
              case #ty r of
                T.ARRAY(t,_) =>
                (case (#ty (trexp e)) of
                   T.INT => {exp=(),ty=t})
              | t => (err pos ("expected ARRAY type, but " ^
                             type2str(t) ^ " found");
                      {exp=(),ty=T.NIL})
            end


        and checkint({exp,ty},pos) = 
            case ty of T.INT => ()
                     | t => 
                       err pos ("INT expected, but " 
                                ^ type2str(t) ^ " found")
                                
        and checkFormals(ts,es,pos) =
            if (length(ts) <> length(es)) then
              err pos (Int.toString(length(ts)) ^ " arguments needed, but "
                       ^ Int.toString(length(es)) ^ " provided")
            else app (fn (t,e) => 
                         checktype(t, #ty (trexp (e)), pos))
                     (ListPair.zip(ts,es))
              
        and checkRecord(ts,fs,pos) =
            if (length(ts) <> length(fs)) then
              err pos (Int.toString(length(ts)) ^ " fields needed, but "
                       ^ Int.toString(length(fs)) ^ " provided")
            else app (fn (t,f) => 
                         checktype(#2 t, #ty (trexp (#2 f)), #3 f))
                     (ListPair.zip(ts,fs))
    in trexp
    end

and transDec(venv,tenv,A.VarDec{name,escape,typ,init,pos}) =
    let val {exp,ty} = transExp(venv,tenv) init
    in case typ of
         NONE => ()
       | SOME((tname,pos)) => 
         case S.look (tenv,tname) of
           NONE => err pos ("type " ^ S.name(tname) ^ " not defined")
         | SOME(dty) => (* FIXME: dty can be/contain named type *)
           checktype(dty,ty,pos);
       {tenv=tenv,venv=S.enter(venv,name,E.VarEntry{ty=ty})}
    end

  (* type declaration maybe recursive, therefore
     we need two steps: first fill the tenv with
     "empty" headers, then pass the tenv to 
     transTy and get the values
   *)
  | transDec(venv,tenv,A.TypeDec(tdecs)) =
            
    let val tenv' = 
            foldl (fn ({name,ty,pos},env) => 
                      S.enter(env,name,T.NAME(name,ref NONE))) tenv tdecs
        val tenv'' = 
            foldl (fn ({name,ty,pos},env) => 
                      (case S.look(env,name) of 
                         SOME(T.NAME(n,r)) =>
                         (r := SOME(transTy(env,ty)); env))) tenv' tdecs

        fun checkcycle(seen,to,pos) = 
            case to of
              NONE => (err pos ("type not found"); false)
            | SOME(t) =>
              case t of 
                T.NAME(s2,r) => 
                if (List.all (fn (x) => x <> s2) seen) then 
                  checkcycle(s2::seen,!r,pos)
                else false 
              | _ => true
              
        (* two options: 
         * 1. check all errors and print them;
         * 2. stop at first error and print *)
        fun checkeach([]) = ()
          | checkeach({name,ty,pos}::ds) =
            case S.look(tenv'',name) of
              SOME(T.NAME(_,r)) => 
              if (not (checkcycle([name], !r, pos))) then
                (err pos ("NAME type " ^ S.name(name) ^ " is involved"
                          ^ " in a cyclic definition."))
              else checkeach(ds)

    (* every cycle on mutually recursive types must include 
     * a array or record *)
    in checkeach(tdecs);
       checkdup(map #name tdecs,map #pos tdecs);
       {venv=venv, tenv=tenv''}
    end
    
  (* 
   * Things to check for a function:
   * 1. result type exists and match,
   * 2. formal type exists and match,
   * 3. no duplicate formal names,
   * 4. body type checks.
   *)
  | transDec(venv,tenv,A.FunctionDec(fundecs)) =
    let 
      (* first pass on a fundec, 
       check formal types,
       store header info in the venv.
       *)
      fun transfun({name,params,result,body,pos},env) =
          let 
            (* check result type *)
            val result_ty = 
                case result of
                  NONE => T.UNIT (* procedure, should return unit *)
                | SOME(rt,pos) => 
                  (case S.look(tenv,rt) of
                     SOME t => t
                   | NONE => 
                     (err pos ("result type " 
                               ^ S.name(rt) ^ " not found.");
                      T.NIL))
            val fs = 
                map (fn {name,escape,typ,pos} =>
                        case S.look(tenv,typ) of
                          SOME t => t
                        | NONE => 
                          (err pos ("parameter type '" ^ 
                           S.name(typ) ^ "' not found"); T.UNIT)) params
          in checkdup(map #name params, map #pos params);
            S.enter(env,name,E.FunEntry{formals=fs,result=result_ty})
          end
    in
      let 
        val venv' = foldl transfun venv fundecs
                    
        (* second pass on a fundec,
         * do type checking, put VarEntry on venv, and
         * check body *)
        fun transbody({name,params,result,body,pos},{tenv,venv}) =
            let 
              (* since we already has the FunEntry on
               * venv, we can get the formal list *) 
              fun transparam{name,escape,typ,pos} =
                  case S.look(tenv,typ) of
                    SOME t => {name=name,ty=t}
                  (* set type to unit if not found *)
                  | NONE => 
                    (err pos ("param type '" ^ S.name(typ) ^ "' not found.");
                     {name=name,ty=T.UNIT})
                    
              val params' = map transparam params
                            
              val venv'' = 
                  (foldl (fn ({name,ty}, env) => 
                             S.enter(env,name,E.VarEntry{ty=ty}))
                         venv' params')
            in
              transExp(venv'',tenv) body;
              {venv=venv'', tenv=tenv}
            end
      in checkdup(map #name fundecs,map #pos fundecs);
        foldl transbody {tenv=tenv,venv=venv} fundecs
      end
    end


and transTy(tenv,A.NameTy(sym,pos)) =
    (* detect mutually recursive types *)
    (case S.look(tenv,sym) of SOME(t) => t)
    
  | transTy(tenv,A.RecordTy(fields)) =
    (checkdup(map #name fields, map #pos fields);
     T.RECORD(
     (map (fn (f) => 
              case S.look(tenv,#typ f) of
                SOME(t) => (#name f,t)
              | NONE => (err (#pos f) 
                             ("undefined type '" ^ S.name (#typ f) ^ "'");
                         ((#name f),T.UNIT))) fields), ref()))
    
  | transTy(tenv,A.ArrayTy(sym,pos)) =
    case S.look(tenv,sym) of
      SOME(aty) => T.ARRAY(aty,ref())
    | NONE => (err pos ("undefined type `" ^ S.name sym ^ "'");
               T.ARRAY(err_type,ref()))
              

fun transProg(exp:Absyn.exp) = 
    (transExp(E.base_venv,E.base_tenv)(exp); ())
end
