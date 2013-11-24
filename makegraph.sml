signature MAKE_GRAPH =
sig
  val instrs2graph: Assem.instr list -> Flow.flowgraph
  val show: TextIO.outstream * Flow.flowgraph * (Temp.temp -> string) -> unit
end

structure MakeGraph : MAKE_GRAPH =
struct

structure T = Temp
structure F = Flow


(*
 * Algorithm:
 * first pass: make node for each instr,
 * second pass: for each OPER instr, search in the instr list
 *  for a label that it jumps to, and make a new edge from the
 *  node of this instr to the node of the label.
 * third pass: connect all the nodes in sequential order (which
 *  are not connected by explicit jump in the original instr)
 *
 * This is probably too inefficient, as it requires O(n^3) time.
 * But, I'll leave improvement for future.
 *
 *)

fun show(out,nodes,p) =
  let
    fun plist f list = "[ " ^ String.concatWith "," (map f list) ^ "]"
    fun pnode (n as Flow.Node{id,...}) = Int.toString id
  in
    app
      (fn (n as F.Node{id,def,use,ismove,succ,prev,liveout}) =>
        TextIO.output(
          out,
          ("n" ^ (Int.toString id) ^ ": " ^
           "def: " ^ (plist p def) ^ "\n" ^
           "use: " ^ (plist p use) ^ "\n" ^
           "succ: " ^ (plist pnode (!succ)) ^ "\n" ^
           "prev: " ^ (plist pnode (!prev)) ^ "\n" ^
           "liveout: " ^ (plist T.makestring (!liveout)) ^ "\n"))
      )
      nodes
  end

(* create graph from the instruction list *)
fun instrs2graph instrs =
  let
    fun make_node(instr,(nodelist,count)) =
      let
        val (a,b,c) =
          case instr of
              Assem.OPER{assem,dst,src,jump} => (dst,src,false)
            | Assem.LABEL{assem,lab} => (nil,nil,false)
            | Assem.MOVE{assem,dst,src} => ([dst],[src],true)

        val new_node =
          F.Node{id=count,def=a,use=b,ismove=c,
                 succ=ref nil,prev=ref nil,liveout=ref nil}
      in
        (nodelist @ [new_node], count+1)
      end

    (* we have to maintain the order of nodelist wrt instrs *)
    val (nodelist,_) = foldl make_node (nil,0) instrs

    val complist = ListPair.zip (instrs, nodelist)

    fun do_make_edge(from as F.Node{succ,...}, to as F.Node{prev,...}) =
      if List.exists (fn n => n = to) (!succ) then ()
      else (succ := to :: (!succ); prev := from :: (!prev))

    (* only connect x with y if x doesn't jump. *)
    fun connect nil = ()
      | connect [(i,x)] = ()
      | connect ((i1,x)::((i2,y)::rest)) =
        (case i1 of
           Assem.OPER{jump=SOME j,...} => ()
          | _ => do_make_edge(x,y);
         connect ((i2,y)::rest))

    fun do_jump(instr,node) =
      let
        fun f l =
          case List.find
            (fn (i,n) =>
              case i of
                Assem.LABEL{lab,...} => l = lab
               | _ => false
            ) complist
           of
            SOME((_,n)) => do_make_edge(node,n)
      in
        case instr of
          Assem.OPER{jump=SOME(jlist),...} => (map f jlist; ())
         | _ => ()
      end

  in
    map do_jump complist;
    connect complist;
    nodelist
  end
end
