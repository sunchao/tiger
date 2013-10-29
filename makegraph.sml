signature MAKE_GRAPH =
sig
  val instrs2graph: Assem.instr list ->
                    Flow.flowgraph * Graph.node list
  val show: TextIO.outstream * Flow.flowgraph * (Temp.temp -> string) -> unit
end

structure MakeGraph : MAKE_GRAPH =
struct

structure T = Temp
structure G = Graph
structure GT = G.Table
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

fun show (out,F.FGRAPH{control,def,use,ismove},p) =
    let
      fun process1 node =
          TextIO.output(out,
                        (G.nodename node) ^ "\t" ^
                        "adj[" ^ (String.concatWith ", "
                        (map G.nodename (G.adj node))) ^
                        "] succ[" ^ (String.concatWith ", "
                        (map G.nodename (G.succ node))) ^
                        "] def[" ^ (String.concatWith ", "
                        (map p (valOf(GT.look(def,node))))) ^
                        "] use[" ^ (String.concatWith ", "
                        (map p (valOf(GT.look(use,node))))) ^
                        "] ismove=" ^ Bool.toString(valOf(GT.look(ismove,node)))
                        ^ "\n")
    in app process1 (G.nodes control) end

fun instrs2graph instrs =
    let
      fun make_node ((F.FGRAPH{control,def,use,ismove},nodelist), instr) =
          let val node = G.newNode control
              val (a,b,c) =
                  case instr of
                    Assem.OPER{assem,dst,src,jump} => (dst,src,false)
                  | Assem.LABEL{assem,lab} => (nil,nil,false)
                  | Assem.MOVE{assem,dst,src} => ([dst],[src],true)
          in
            (F.FGRAPH{control=control,
                    def=GT.enter(def,node,a),
                    use=GT.enter(use,node,b),
                    ismove=GT.enter(ismove,node,c)
                   },
             nodelist @ [node])
          end

      (* we have to maintain the order of nodelist wrt instrs *)
      val (igraph,nodelist) =
          foldl (fn (i,(ig,ns)) => make_node((ig,ns),i))
                (F.FGRAPH{control=G.newGraph (),
                          def=GT.empty,
                          use=GT.empty,
                          ismove=GT.empty},nil) instrs

      val complist = ListPair.zip (instrs, nodelist)

      fun do_make_edge (from,to) =
          if List.exists (fn n => G.eq(to,n)) (G.adj from)
          then () else G.mk_edge{from=from,to=to}

      fun connect nil = ()
        | connect [x] = ()
        | connect (x::(y::rest)) =
          (do_make_edge(x,y); connect (y::rest))

      fun do_jump (instr,node) =
          let fun f l =
                  case List.find
                           (fn (i,n) =>
                               case i of
                                 Assem.LABEL{lab,...} => l = lab
                               | _ => false
                           ) complist of
                    SOME((_,n)) => do_make_edge(node,n)
          in case instr of
               Assem.OPER{jump=SOME(jlist),...} => (map f jlist; ())
             | _ => ()
          end

    in
      map do_jump complist;
      connect nodelist;
      (igraph,nodelist)
    end
end
