(* $Id: semant.sml,v 1.2 2012/10/17 15:24:28 csun Exp csun $ *)
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

fun actual_ty (ty:T.ty,pos) = 
     case ty of
         T.NAME(sym,tyref) =>
         (case (!tyref) of
             NONE => (err pos ("undefined type " ^ S.name(sym));err_type)
           | SOME(ty) => actual_ty (ty,pos))
       | _ => ty


fun transExp(venv, tenv) =

    let 
        fun trexp (A.NilExp) = {exp=(),ty=T.NIL}
          | trexp (A.IntExp(_)) = {exp=(),ty=T.INT}
          | trexp (A.StringExp(_)) = {exp=(),ty=T.STRING}
          | trexp (A.OpExp{left,oper=_,right,pos}) =
            (checkInt(trexp left,pos);
             checkInt(trexp right,pos);
             {exp=(),ty=T.INT})
          | trexp (A.VarExp(var)) = trvar (var)

          | trexp (A.RecordExp{fields=fs,typ=rname,pos}) =
            let 
                val rty = S.look(tenv,rname) 
            in
                case rty
                 of NONE => 
                    (err pos "record type not defined";
                     {exp=(),ty=err_type})
                  | SOME(T.RECORD(ts,_)) =>
                    (checkRecord(ts,fs,pos);{exp=(),ty=err_type})
                  | SOME(_) =>
                    (err pos ("expected record type, but other found.");
                     {exp=(), ty=err_type})
            end

          | trexp (A.SeqExp(exps)) = 
            (case exps of
                 nil => {exp=(), ty=T.UNIT}
               | hd :: tl => (trexp (#1 hd); trexp (A.SeqExp(tl))))

          | trexp (A.AssignExp{var,exp,pos}) =
            if ((#ty (trvar var)) <> (#ty (trexp exp))) then
                (err pos ("type mismatch in assignment");
                 {exp=(), ty=err_type})
            else {exp=(), ty=(#ty (trexp exp))}

          | trexp (A.IfExp{test, then', else', pos}) = 
            if (#ty (trexp test) <> T.INT)
            then (err pos ("type for if condition should be int");
                  {exp=(), ty=err_type})
            else
                let
                    val lty = #ty (trexp (then')) 
                in
                    case else' 
                     of NONE => {exp=(), ty=lty}
                      | SOME(elseexp) =>
                        if (lty <> #ty (trexp (elseexp))) then
                            (err pos ("types from both branches should be same");
                             {exp=(), ty=err_type})
                        else {exp=(), ty=T.UNIT}
                end

          | trexp (A.WhileExp{test,body,pos}) =
            if (#ty (trexp test) <> T.INT) then
                (err pos "type for condition should be int";
                 {exp=(),ty=err_type})
            else (trexp body; {exp=(),ty=T.UNIT})

          | trexp (A.LetExp{decs,body,pos}) =
            let 
                val {venv=venv',tenv=tenv'} =  
                    foldl
                    (fn (dec,{venv,tenv}) => transDec(venv,tenv,dec))
                    {venv=venv,tenv=tenv} decs
            in 
                transExp(venv',tenv') body
            end

          | trexp (A.BreakExp(_)) = {exp=(),ty=T.NIL}

          | trexp (A.ArrayExp{typ,size,init,pos}) = 
            case S.look(tenv,typ) of
                SOME(T.ARRAY(t,_)) =>
                if (#ty (trexp(size)) <> T.INT) then
                    (err pos ("array size should be integer");
                     {exp=(),ty=T.UNIT})
                else
                    
                    
                                      
                    

          | trexp (A.CallExp{func,args,pos}) =
            case S.look(venv,func) of
                NONE => 
                (err pos ("function" ^ S.name(func) ^ " not defined. ");
                 {exp=(),ty=T.UNIT})
              | SOME(E.FunEntry{formals,result}) => 
                (checkFormals(formals,args,pos);
                 {exp=(),ty=result})
                 
              
                
        and trvar (A.SimpleVar(id,pos)) = 
            (case S.look (venv,id)
                  of SOME(E.VarEntry{ty}) =>
                     {exp=(),ty=actual_ty (ty,pos)}
                   | SOME(_) => 
                     (err pos ("expected variable, but function found.");
                      {exp=(), ty=T.INT})
                   | NONE => 
                     (err pos ("undefined variable " ^ S.name id);
                      {exp=(),ty=T.INT}))

        and checkInt ({exp,ty},pos) = 
            case ty of T.INT => ()
                     | _ => err pos "integer required"

        and checkFormals ([],[],pos) = ()
          | checkFormals (ts,[],pos) = err pos "missing argument"
          | checkFormals ([],es,pos) = err pos "extra argument"
          | checkFormals (t::ts,e::es,pos) = 
            if (#ty (trexp e)) <> t
            (* TODO: print type info *)
            then err pos "argument type doesn't match" 
            else checkFormals(ts,es,pos)
                 
        and checkRecord ([],[],pos) = ()
          | checkRecord (ts,[],pos) = err pos "extra fields"
          | checkRecord ([],fs,pos) = err pos "extra types"
          | checkRecord (t::rt,f::rf,pos) = 
            if (#1 t) <> (#1 f) 
            then err pos "type name wrong"
            else if (#2 t) <> (#ty (trexp (#2 f))) 
            then err (#3 f) "type doesn't match" 
            else checkRecord(rt,rf,pos)
    in
        trexp
    end

and transDec(venv,tenv,A.VarDec{name,escape,typ,init,pos}) =
    let 
        val {exp,ty} = transExp(venv,tenv) init
    in 
        case typ
         of NONE =>
            {tenv=tenv,venv=S.enter(venv,name,E.VarEntry{ty=ty})}
          | SOME((tname,pos)) => 
            let 
                val doo = S.look (tenv, tname) 
            in
                case doo of
                    NONE => (err pos "type not defined"; {tenv=tenv,venv=venv})
                  | SOME(dty) => 
                    if (dty <> ty) then
                        (err pos "declaration type mismatch";
                         {tenv=tenv,venv=venv})
                    else {tenv=tenv,venv=S.enter(venv,name,E.VarEntry{ty=ty})}
            end
    end

  (* type declaration maybe recursive, therefore
     we need two steps: first fill the tenv with
     "empty" headers, then pass the tenv to 
     transTy and get the values
   *)
  | transDec(venv,tenv,A.TypeDec(tdecs)) =
    let 
        val tenv' = 
            foldl 
                (fn ({name,ty,pos},env) => 
                    S.enter (env, name, T.NAME(name, ref NONE)))
                tenv tdecs

        val tenv'' = 
            foldl
            (fn ({name,ty,pos},env) => 
                S.enter(env,name,transTy(env,ty)))
            tenv' tdecs
    in
        {venv=venv, tenv=tenv''}
    end

  (* 
   * Things to check for a function:
   * 1. result type exists and match,
   * 2. formal type exists and match,
   * 3. no duplicate formal names,
   * 4. body type checks.
   *)
  | transDec (venv,tenv,A.FunctionDec(fundecs)) =
    let 
        (* first pass on a fundec, 
         check formal types,
         store header info in the venv.
         *)
        fun transfun ({name,params,result,body,pos},env) =
            let 
                (* check result type *)
                val result_ty = 
                    case result of
                        NONE => T.UNIT
                      | SOME(rt,pos) => 
                        case S.look(tenv,rt) of
                            NONE => (err pos ("result type " 
                                              ^ S.name(rt) ^ " not found.");
                                     T.UNIT) (* not sure here *)
                          | SOME t => t

                val fs = 
                    map 
                    (fn {name,escape,typ,pos} =>
                        case S.look(tenv,typ) of
                            SOME t => t
                          | NONE => T.UNIT)
                    params
            in
                S.enter(env,name,E.FunEntry{formals=fs,result=result_ty})
            end
    in
        let 
            val venv' = foldl transfun venv fundecs

            (* second pass on a fundec,
             do type checking, put VarEntry on venv, and
             check body
             *)
            fun transbody ({name,params,result,body,pos},{tenv,venv}) =
                let 
                    (* since we already has the FunEntry on
                     venv, we can get the formal list
                     *) 
                    fun transparam {name,escape,typ,pos} =
                        case S.look(tenv,typ) of
                            SOME t => {name=name,ty=t}
                          (* set type to unit if not found *)
                          | NONE => 
                            (err pos ("param type " ^ S.name(typ) ^ " not found.");
                             {name=name,ty=T.UNIT})
                        
                    val params' = map transparam params

                    val venv'' = 
                        (foldl 
                        (fn ({name,ty}, env) => 
                            S.enter(env,name,E.VarEntry{ty=ty}))
                        venv' params')

                in
                    transExp(venv'',tenv) body;
                    {venv=venv'', tenv=tenv}
                end
        in
            foldl transbody {tenv=tenv,venv=venv} fundecs
        end
    end


and transTy(tenv,A.NameTy(sym,pos)) =
    T.NAME(sym, ref(S.look(tenv,sym)))

  | transTy(tenv,A.RecordTy(fields)) =
    T.RECORD(
    (map (fn (field) => 
             case S.look(tenv,#typ field) of
                 SOME(fty) => (#name field,fty)
               | NONE => (err (#pos field) 
                              ("undefined type `" ^ S.name (#typ field) ^ "'");
                          ((#name field),err_type)))
         fields), ref())

  | transTy(tenv,A.ArrayTy(sym,pos)) =
    case S.look(tenv,sym) of
        SOME(aty) => T.ARRAY(aty,ref())
      | NONE => (err pos ("undefined type `" ^ S.name sym ^ "'");
                 T.ARRAY(err_type,ref()))


fun transProg(exp:Absyn.exp) = 
    (transExp(E.base_venv,E.base_tenv)(exp); ())

end
