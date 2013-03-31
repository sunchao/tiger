(* test suite for liveness module *)

structure TestLiveness = struct
structure A = Assem
structure T = Temp

fun test instrs =
    let val (fgraph,nodelist) = MakeGraph.instrs2graph instrs
        val (igraph,liveout) = Liveness.interferenceGraph fgraph
    in
      MakeGraph.show(TextIO.stdOut,fgraph);
      Liveness.show(TextIO.stdOut,igraph)
    end

val instrs1 = 
    let val L1 = T.newlabel ()
        val a = T.newtemp ()
        val b = T.newtemp ()
        val c = T.newtemp () in
      [A.OPER {assem="1",src=[],dst=[a],jump=NONE},
       A.LABEL {assem="2",lab=L1},
       A.OPER {assem="3",src=[a],dst=[b],jump=NONE},
       A.OPER {assem="4",src=[b,c],dst=[c],jump=NONE},
       A.OPER {assem="5",src=[b],dst=[a],jump=NONE},
       A.OPER {assem="6",src=[a],dst=[],jump=SOME [L1]},
       A.OPER {assem="7",src=[c],dst=[],jump=NONE}]
    end

fun main () = test instrs1
       
end
