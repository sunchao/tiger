structure TestColoring = struct

structure T = Temp
structure A = Assem
structure Frame = MipsFrame

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

val temps2 =
    map str2temp ["g","h","f","e","m","b","c","d","k","j"]

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
        A.OPER {assem="11",src=[d,k,j],dst=[],jump=NONE}]
    end

structure TestRegAlloc : REG_ALLOC =
struct
structure Frame = MipsFrame
type allocation = Frame.register Temp.Table.table

val registers = ["1","2","3","4"]

val initial = Temp.Table.empty

(* a simple version of alloc:
 * we dont have spill or coalesing right now, so no change on instrs
 * alloc is simply the output from Color.color *)
fun alloc (instrs,frame) : Assem.instr list * allocation =
    let 
      val (graph,nodes) = MakeGraph.instrs2graph instrs
	    val (igraph,liveout) = Liveness.interferenceGraph graph
	    val (alloc,temps) = SimpleColor.color{interference=igraph,
					                                  initial=initial,
					                                  spillCost=(fn _ => 1),
					                                  registers=registers}
    in (instrs,alloc) end
end

exception NotGood

fun println str = TextIO.output (TextIO.stdOut, str ^ "\n")

fun print_alloc (temps: Temp.temp list,
                 alloc:Frame.register T.Table.table,
                 p: Temp.temp -> string) =
    app 
      (fn t => 
          let val r = 
                  case T.Table.look(alloc,t) of
                      SOME r => r
                    | NONE => 
                      (println ("No mapping for " ^ (p t) ^ " found.");
                       raise NotGood)
          in println ((p t) ^ " --> " ^ r ^ "\n") end)
      temps

fun print_temp_mapping temps = 
    app
      (fn t => 
          (TextIO.output(TextIO.stdOut,
                         (temp2str t) ^ " ==> " ^ 
                         (Temp.makestring t) ^ "\n")))
      temps

fun test (instrs,temps) = 
    let
      val (instrs2,alloc) = 
          TestRegAlloc.alloc
            (instrs, Frame.newFrame{name=Temp.namedlabel "F",formals=[]})
    in
      print_temp_mapping temps;
      print_alloc(temps,alloc,temp2str)
    end

fun main () = test (instrs2,temps2)

end
