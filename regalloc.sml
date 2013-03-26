structure RegAlloc : REG_ALLOC =
struct
structure Frame = MipsFrame
type allocation = Frame.register Temp.Table.table

(* a simple version of alloc:
 * we dont have spill or coalesing right now, so no change on instrs
 * alloc is simply the output from Color.color *)
fun alloc (instrs,frame) : Assem.instr list * allocation =
    let val (graph,nodes) = MakeGraph.instrs2graph instrs
	val (igraph,liveout) = Liveness.interferenceGraph graph
	val (alloc,temps) = Color.color{interference=igraph,
					initial=Frame.tempMap,
					spillCost=(fn _ => 1),
					registers=Frame.registers}
    in (instrs,alloc) end
end
