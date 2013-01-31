structure MipsGen :> CODEGEN =
struct

structure Frame : FRAME = MipsFrame
structure A = Assem
structure T = Tree

fun codegen (frame) (stm:Tree.stm) : Assem.instr list =
    let val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x :: !ilist
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end
                          
        fun int2str n = 
            if n >= 0 then Int.toString(n) else ("-" ^ Int.toString(~n))

        fun munchStm (T.SEQ(a,b)) = (munchStm(a); munchStm(b))
                                    
          | munchStm (T.LABEL lab) = 
            emit(A.LABEL{assem=Symbol.name(lab) ^ ":\n",lab=lab})
                 
          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)), e2)) =
            emit(A.OPER{assem="sw `s0, " ^ int2str i ^ "(`s1)\n",
                        src=[munchExp e1, munchExp e2],
                        dst=[],jump=NONE})

          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)), e2)) =
            emit(A.OPER{assem="sw `s0, " ^ int2str i ^ "(`s1)\n",
                        src=[munchExp e1, munchExp e2],
                        dst=[],jump=NONE})
            
          | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) = 
            emit(A.OPER{assem="sw `s0, " ^ int2str i ^ "($zero)\n",
                        src=[munchExp e2],dst=[],jump=NONE})

          | munchStm (T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER{assem="sw `s0, 0(`s1)\n",
                        src=[munchExp e2, munchExp e1],
                        dst=[],jump=NONE})

          | munchStm (T.MOVE((T.TEMP i, T.CONST n))) = 
            emit(A.OPER{assem="li `d0, " ^ int2str n ^ "\n",
                        src=[],dst=[i],jump=NONE})

          | munchStm (T.MOVE((T.TEMP i, e2))) =
            emit(A.OPER{assem="move `d0, `s0\n",
                        src=[munchExp e2],dst=[i],jump=NONE})

          | munchStm (T.JUMP(T.NAME lab,_)) =
            emit(A.OPER{assem="j `j0\n",src=[],dst=[],jump=SOME([lab])})

          | munchStm (T.JUMP(e,labels)) =
            emit(A.OPER{assem="jr `s0\n",src=[munchExp e],
                        dst=[],jump=SOME(labels)})
                        
          | munchStm (T.EXP(T.CALL(e,args))) =
            emit(A.OPER{assem="jal `s0\n",
                        src=munchExp(e)::munchArgs(0,args),
                        dst=nil, (* TODO *)
                        jump=NONE})

          | munchStm (T.EXP e) = (munchExp e; ())

        and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
            result(fn r => emit(A.OPER{
                                assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                src=[munchExp e1],dst=[r],jump=NONE}))

          | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e2))) =
            result(fn r => emit(A.OPER{
                                assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                src=[munchExp e2],dst=[r],jump=NONE}))

          | munchExp (T.MEM(T.CONST i)) =
            result(fn r => emit(A.OPER{
                                assem="lw `d0, " ^ int2str i ^ "($zero)\n",
                                src=[],dst=[r],jump=NONE}))

          | munchExp (T.MEM(e1)) =
            result(fn r => emit(A.OPER{
                                assem="lw `d0, 0(`s0)\n",
                                src=[munchExp e1],dst=[r],jump=NONE}))

          | munchExp (T.BINOP(T.PLUS,e1,T.CONST i)) =
            result(fn r => emit(A.OPER{
                               assem="addi `d0, `s0, " ^ int2str i ^ "\n",
                               src=[munchExp e1],dst=[r],jump=NONE}))

          | munchExp (T.BINOP (T.PLUS,T.CONST i,e1)) =
            result(fn r => emit(A.OPER{
                               assem="addi `d0, `s0, " ^ int2str i ^ "\n",
                               src=[munchExp e1],dst=[r],jump=NONE}))

          | munchExp (T.CONST i) = 
            result(fn r => emit(A.OPER{
                               assem="li `d0, " ^ int2str i ^ "\n",
                               src=[],dst=[r],jump=NONE}))

          | munchExp (T.BINOP(T.PLUS,e1,e2)) = 
            result(fn r => emit(A.OPER{
                               assem="add `d0, `s1, `s2\n",
                               src=[munchExp e1,munchExp e2],
                               dst=[r],jump=NONE}))

          | munchExp (T.TEMP t) = t

          | munchExp (T.NAME label) = 
            result(fn r => emit(A.OPER{
                                assem="la `d0, " ^ Symbol.name label ^ "\n",
                                src=[],dst=[r],jump=NONE}))

          | munchExp (T.CALL(e,args)) = Frame.RV

        and munchArgs (_, nil) = nil
          | munchArgs (n, arg :: rest) = nil (* TODO *)

    in munchStm stm;
       rev(!ilist)
    end

end
