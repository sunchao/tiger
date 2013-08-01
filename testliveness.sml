(* test suite for liveness module *)

structure TestLiveness = struct
structure A = Assem
structure T = Temp

val tempList = 
    [("a",T.newtemp()),
     ("b",T.newtemp()),
     ("c",T.newtemp()),
     ("d",T.newtemp()),
     ("e",T.newtemp()),
     ("f",T.newtemp()),
     ("g",T.newtemp()),
     ("h",T.newtemp()),
     ("i",T.newtemp()),
     ("j",T.newtemp()),
     ("k",T.newtemp()),
     ("l",T.newtemp()),
     ("m",T.newtemp()),
     ("n",T.newtemp()),
     ("o",T.newtemp()),
     ("p",T.newtemp()),
     ("q",T.newtemp()),
     ("r",T.newtemp()),
     ("s",T.newtemp()),
     ("t",T.newtemp()),
     ("u",T.newtemp()),
     ("v",T.newtemp()),
     ("w",T.newtemp()),
     ("x",T.newtemp()),
     ("y",T.newtemp()),
     ("z",T.newtemp())]

fun temp2str t = #1(valOf(List.find (fn (k,v) => v = t) tempList))

fun str2temp s = #2(valOf(List.find (fn (k,v) => k = s) tempList))

val instrs1 = 
    let val L1 = T.newlabel ()
        val a = str2temp "a"
        val b = str2temp "b"
        val c = str2temp "c" in
      [A.OPER {assem="1",src=[],dst=[a],jump=NONE},
       A.LABEL {assem="2",lab=L1},
       A.OPER {assem="3",src=[a],dst=[b],jump=NONE},
       A.OPER {assem="4",src=[b,c],dst=[c],jump=NONE},
       A.OPER {assem="5",src=[b],dst=[a],jump=NONE},
       A.OPER {assem="6",src=[a],dst=[],jump=SOME [L1]},
       A.OPER {assem="7",src=[c],dst=[],jump=NONE}]
    end

val instrs2 = 
    let val g = str2temp "g"
        val h = str2temp "h"
        val f = str2temp "f"
        val e = str2temp "e"
        val m = str2temp "m"
        val b = str2temp "b"
        val c = str2temp "c"
        val d = str2temp "d"
        val k = str2temp "k" 
        val j = str2temp "j"
    in 
      [ A.OPER {assem="0", src=[], dst=[k,j],jump=NONE},
        A.OPER {assem="1", src=[j],dst=[g],jump=NONE},
        A.OPER {assem="2", src=[k],dst=[h],jump=NONE},
        A.OPER {assem="3", src=[g,h],dst=[f],jump=NONE},
        A.OPER {assem="4", src=[j],dst=[e],jump=NONE},
        A.OPER {assem="5", src=[j],dst=[m],jump=NONE},
        A.OPER {assem="6", src=[f],dst=[b],jump=NONE},
        A.OPER {assem="7", src=[e],dst=[c],jump=NONE},
        A.MOVE {assem="8", src=c,dst=d},
        A.OPER {assem="9", src=[m],dst=[k],jump=NONE},
        A.MOVE {assem="10", src=b,dst=j},
        A.OPER {assem="11",src=[d,k,j],dst=[],jump=NONE}
      ]
    end

fun showLiveout (nodes,liveout) = 
    app 
      (fn n => 
          TextIO.output(TextIO.stdOut,
                        ("Liveout for " ^ (Graph.nodename n) ^
                         ": {" ^ 
                         (String.concatWith 
                            ","
                            (map temp2str (liveout n))) ^ "}\n")))
      nodes

fun test instrs =
    let val (fgraph,nodelist) = MakeGraph.instrs2graph instrs
        val (igraph,liveout) = Liveness.interferenceGraph fgraph
    in
      MakeGraph.show(TextIO.stdOut,fgraph,temp2str);
      Liveness.show(TextIO.stdOut,igraph,temp2str);
      showLiveout (nodelist, liveout)
    end

fun main () = test instrs2
       
end
