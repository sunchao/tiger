structure PrintAbsyn : 
     sig val print : TextIO.outstream * Absyn.exp -> unit end =
struct

  structure A = Absyn

fun print (outstream, e0) =
 let fun say s =  TextIO.output(outstream,s)
  fun sayln s= (say s; say "\n") 

  fun indent 0 = ()
    | indent i = (say " "; indent(i-1))

  fun opname A.PlusOp = "PlusOp"
    | opname A.MinusOp = "MinusOp"
    | opname A.TimesOp = "TimesOp"
    | opname A.DivideOp = "DivideOp"
    | opname A.EqOp = "EqOp"
    | opname A.NeqOp = "NeqOp"
    | opname A.LtOp = "LtOp"
    | opname A.LeOp = "LeOp"
    | opname A.GtOp = "GtOp"
    | opname A.GeOp = "GeOp"

  fun dolist d f [a] = (sayln ""; f(a,d+1))
    | dolist d f (a::r) = (sayln ""; f(a,d+1); say ","; dolist d f r)
    | dolist d f nil = ()


  fun var(A.SimpleVar(s,p),d) = (indent d; say "SimpleVar("; 
			         say(Symbol.name s); say ")")
    | var(A.FieldVar(v,s,p),d) = (indent d; sayln "FieldVar(";
				  var(v,d+1); sayln ",";
				  indent(d+1); say(Symbol.name s); say ")")
    | var(A.SubscriptVar(v,e,p),d) = (indent d; sayln "SubscriptVar(";
				      var(v,d+1); sayln ",";
				      exp(e,d+1); say ")")
  and exp(A.VarExp v, d) = (indent d; sayln "VarExp("; var(v,d+1); say ")")
    | exp(A.NilExp, d) = (indent d; say "NilExp")
    | exp(A.IntExp i, d) = (indent d; say "IntExp("; say(Int.toString i);
			    say ")")
    | exp(A.StringExp(s,p),d) = (indent d; say "StringExp(\"";
				 say s; say "\")")
    | exp(A.CallExp{func,args,pos},d) =
		        (indent d; say "CallExp("; say(Symbol.name func);
			 say ",["; dolist d exp args; say "])")
    | exp(A.OpExp{left,oper,right,pos},d) =
		(indent d; say "OpExp("; say(opname oper); sayln ",";
		 exp(left,d+1); sayln ","; exp(right,d+1); say ")")
    | exp(A.RecordExp{fields,typ,pos},d) =
	    let fun f((name,e,pos),d) = 
			(indent d; say "("; say(Symbol.name name);
			 sayln ","; exp(e,d+1);
			 say ")")
	     in indent d; say "RecordExp("; say(Symbol.name typ); 
	        sayln ",["; dolist d f fields; say "])" 
	    end
    | exp(A.SeqExp l, d) = (indent d; say "SeqExp["; dolist d exp (map #1 l); 
			    say "]")
    | exp(A.AssignExp{var=v,exp=e,pos},d) = 
		(indent d; sayln "AssignExp("; var(v,d+1); sayln ",";
		 exp(e,d+1); say ")")
    | exp(A.IfExp{test,then',else',pos},d) =
		(indent d; sayln "IfExp("; exp(test,d+1); sayln ",";
		 exp(then',d+1);
		 case else' of NONE => ()
			| SOME e => (sayln ","; exp(e,d+1));
		 say ")")
    | exp(A.WhileExp{test,body,pos},d) =
		(indent d; sayln "WhileExp("; exp(test,d+1); sayln ",";
		 exp(body,d+1); say ")")
    | exp(A.ForExp{var=v,escape=b,lo,hi,body,pos},d) =
		(indent d; sayln "ForExp(";
		 say(Symbol.name v); say ","; say(Bool.toString (!b)); sayln ",";
		 exp(lo,d+1); sayln ","; exp(hi,d+1); sayln ",";
		 exp(body,d+1); say ")")
    | exp(A.BreakExp p, d) = (indent d; say "BreakExp")
    | exp(A.LetExp{decs,body,pos},d) =
		(indent d; say "LetExp([";
		 dolist d dec decs; sayln "],"; exp(body,d+1); say")")
    | exp(A.ArrayExp{typ,size,init,pos},d) =
	        (indent d; say "ArrayExp("; say(Symbol.name typ); sayln ",";
		 exp(size,d+1); sayln ","; exp(init,d+1); say ")")


  and dec(A.FunctionDec l, d) = 
	    let fun field({name,escape,typ,pos},d) = 
			(indent d; say "("; say(Symbol.name name);
			 say ","; say(Bool.toString(!escape)); 
			 say ","; say(Symbol.name typ); say ")")
		fun f({name,params,result,body,pos},d) =
		   (indent d; say "("; say (Symbol.name name); say ",[";
		    dolist d field params; sayln "],";
		    case result of NONE => say "NONE"
			 | SOME(s,_) => (say "SOME("; say(Symbol.name s); say ")");
		    sayln ","; exp(body,d+1); say ")")
	     in indent d; say "FunctionDec["; dolist d f l; say "]"
	    end
    | dec(A.VarDec{name,escape,typ,init,pos},d) =
	   (indent d; say "VarDec("; say(Symbol.name name); say ",";
	    say(Bool.toString (!escape)); say ",";
	    case typ of NONE => say "NONE" 
		      | SOME(s,p)=> (say "SOME("; say(Symbol.name s); say ")");
            sayln ","; exp(init,d+1); say ")")
    | dec(A.TypeDec l, d) = 
	 let fun tdec({name,ty=t,pos},d) = (indent d; say"("; 
				  	    say(Symbol.name name); sayln ",";
					    ty(t,d+1); say ")")
	  in indent d; say "TypeDec["; dolist d tdec l; say "]"
         end
   
  and ty(A.NameTy(s,p), d) = (indent d; say "NameTy("; say(Symbol.name s);
			      say ")")
    | ty(A.RecordTy l, d) =  
		let fun f({name,escape,typ,pos},d) =
			(indent d; say "("; say (Symbol.name name);
		         say ","; say (Bool.toString (!escape)); say ",";
			 say (Symbol.name typ); say ")")
	         in indent d; say "RecordTy["; dolist d f l; say "]"
		end
    | ty(A.ArrayTy(s,p),d) = (indent d; say "ArrayTy("; say(Symbol.name s);
			      say ")")

 in  exp(e0,0); sayln ""; TextIO.flushOut outstream
end

end

