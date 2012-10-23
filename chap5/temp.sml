          | trexp (A.SeqExp []) = {exp=(),ty=T.UNIT}
          | trexp (A.SeqExp (e::[])) = (trexp #1e)
          | trexp (A.SeqExp es) = 
            let val last = #ty (trexp List.last es) in
                {exp=(),ty = last}
            end
          | trexp (A.AssignExp{var,exp,pos}) = 
            let val trv = trvar (var)
                val tre = trexp (exp) 
            in
                if (#ty trv) <> (#ty tre) then 
                    err pos "assignment type mismatch"
                else ();
                {exp=(),ty=T.UNIT}
            end
          | trexp (A.CallExp{func=fname,args=exps,pos}) = 
            let val rty = Symbol.look (venv,fname) in
            case rty of
                SOME(E.FunEntry{formals,result}) => 
                (checkFormals (formals,exps,pos); {exp=(),ty=result})
              | NONE => ((err pos ("should be a function type")); {exp=(),ty=err_type})
            end
          | trexp (A.IfExp{test,then',else',pos}) = 
            let val testty = trexp test
                val thenty = trexp then'
            in
                case else' of
                    SOME(else_exp) => 
                    if (#ty thenty) <> (#ty (trexp else_exp)) then
                        err pos "both if branches should have same type"
                    else ()
                  | NONE => ();
                {exp=(),ty=(#ty thenty)}
            end





