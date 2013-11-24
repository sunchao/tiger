signature LIVENESS =
sig
  structure I : sig
              datatype status = INGRAPH of int (* degree *)
                              | REMOVED
                              | COLORED of string

              datatype node = NODE of {temp: Temp.temp,
                                       adj: node list ref,
                                       status: status ref}
            end

  datatype igraph = IGRAPH of {graph: I.node list,
                               moves: (I.node*I.node) list}

  val interferenceGraph : Flow.flowgraph -> igraph

  val show : TextIO.outstream * igraph -> unit

end

structure TempKey : ORD_KEY =
struct
type ord_key = Temp.temp
fun compare (t1,t2) = String.compare(Temp.makestring t1,Temp.makestring t2)
end

structure Liveness :> LIVENESS =
struct

structure I =
struct
datatype status = INGRAPH of int (* degree *)
                | REMOVED
                | COLORED of string

datatype node = NODE of {temp: Temp.temp,
                         adj: node list ref,
                         status: status ref}
end


datatype igraph = IGRAPH of {graph: I.node list,
                             moves: (I.node*I.node) list}

structure F = Flow
structure S = ListSetFn(TempKey)
structure T = Temp
structure TT = Temp.Table
structure GT = IntMapTable(type key=int fun getInt n = n)
structure Frame = MipsFrame

type liveSet = S.set
type liveMap = liveSet GT.table
type tempEdge = {src:Temp.temp,dst:Temp.temp}

fun show(output, IGRAPH{graph,moves}) =
  let
    fun p_status s =
      case s of
        I.INGRAPH(d) => "INGRAPH(" ^ Int.toString(d) ^ ")"
      | I.REMOVED => "REMOVED"
      | I.COLORED(r) => "COLORED(" ^ r ^ ")"

	  fun do1(node as I.NODE{temp,adj,status}) =
      TextIO.output(
        output,
        ("{temp=" ^ (T.makestring temp) ^ "," ^
         "status=" ^ p_status (!status) ^ "}\n"))
  in
    app do1 graph
  end

fun interferenceGraph flowgraph =
  let

    fun println s = print(s ^ "\n")

    fun look(table: liveMap,key) = valOf(GT.look(table,key))

    (* recursively compute liveSet, until reaches a fix point *)
    fun iter(livein_map,liveout_map) =
      let
        (* Record whether we've reached a fix point *)
        val changed = ref false

        (* Given livein map, compute liveout set for node n *)
        fun compute_out(mi,F.Node{succ,...}) =
          foldl (fn (F.Node{id,liveout,...},s) => S.union(look(mi,id),s))
                S.empty (!succ)

        (* Compute new livein and liveout map *)
        val (new_livein_map, new_liveout_map) =
          foldl
            (fn (x as F.Node{id,def,use,...}, (mi,mo)) =>
              let
                val oldin = look(mi,id) (* Save old livein set *)
                val oldout = look(mo,id) (* Save old liveout set *)
                val usex = S.addList(S.empty,use)
                val defx = S.addList(S.empty,def)
                val newin = S.union(usex,S.difference(oldout,defx))
                val newout = compute_out(mi,x)
              in
                if S.equal(newin,oldin) andalso S.equal(newout,oldout)
                then () else changed := true;
                (GT.enter(mi,id,newin),GT.enter(mo,id,newout))
              end
            ) (livein_map,liveout_map) flowgraph
      in
        if !changed then iter (new_livein_map,new_liveout_map)
        else new_liveout_map
      end

    (* Make a empty map that maps each node in graph to an empty set *)
    fun make_empty_map () =
      foldl (fn (F.Node{id,...},m) => GT.enter(m,id,S.empty)) GT.empty
            (List.rev flowgraph)

    (* A map from a graph node id to its liveout set *)
    val liveout_map : liveMap = iter ((make_empty_map ()), (make_empty_map ()))


    (* set liveout for each node *)
    val _ =
      app
        (fn F.Node{id,liveout,...} =>
          case GT.look(liveout_map,id) of
              SOME s => liveout := S.listItems(s)
            | NONE => ErrorMsg.impossible("liveout map is not one-to-one")
        ) flowgraph;
  in
    (* now for each node n in the flow graph, suppose
     * there is a newly define temp d, and temporaries
     * t1, ..., tn are in liveout set of node n. Then,
     * we add edge (d,t1), ..., (d,tn) to the igraph.
     * Mappings between temps and igraph nodes are also recorded.
     * The rules for adding interference edges are:
     * 1. At any nonmove instruction that defines a variable a, where the
     *   live-out variables are b1,...bj, add interference edges
     *   (a,b1),...,(a,bj).
     * 2. At a move instruction a <- c, where variables b1,...,bj are live-out,
     *   add interference edges (a,b1),...,(a,bj) for any bi that is not
     *   the same as c. *)
    let
      fun find_edges(node as F.Node{def,liveout,...}) : tempEdge list =
        let
          fun f t = foldl
                      (fn (t',l) =>  (* don't add self-edge *)
                        if t <> t' then {src=t,dst=t'}::l else l)
                      nil (!liveout)
        in foldl (fn (t,l) => (f t) @ l) nil def end

      val all_edges : tempEdge list =
        foldl (fn (n,l) => find_edges(n) @ l) nil flowgraph

      val (tmap, outgraph) =
        foldl
          (fn (t,(table,nodes)) =>
            case TT.look(table,t) of
              NONE =>
              let
                val nn = I.NODE{temp=t,
                                adj=ref nil,
                                status=ref(I.INGRAPH(0))}
              in
                (TT.enter(table,t,nn), nn::nodes)
              end
             | SOME _ => (table,nodes)
          )
          (TT.empty,nil)
          (foldl
            (fn (F.Node{def,use,...},acc) => acc@def@use)
            nil flowgraph)

      fun looktemp t = valOf(TT.look(tmap,t))

      val allmoves =
        foldl
          (fn (F.Node{def,use,ismove,...}, moves) =>
            if ismove then
              let val ([src],[dst]) = (use,def)
              in (looktemp(src),looktemp(dst)) :: moves end
            else moves
          ) nil flowgraph

    in
      app
        (fn {src,dst} => (* don't add duplicate edges *)
          let
            val (srcn as I.NODE{temp=t1,adj=adj1,status=st1},
                 dstn as I.NODE{temp=t2,adj=adj2,status=st2})
                = (looktemp(src),looktemp(dst))
            val d1 = case (!st1) of I.INGRAPH(d) => d
            val d2 = case (!st2) of I.INGRAPH(d) => d
          in
            if
              List.exists (fn x => x = dstn) (!adj1) andalso
              List.exists (fn x => x = srcn) (!adj2)
            then ()
            else
              let in
                adj1 := dstn::(!adj1);
                adj2 := srcn::(!adj2);
                st1 := I.INGRAPH(d1+1);
                st2 := I.INGRAPH(d2+1)
              end
          end
        ) all_edges;

      IGRAPH{graph=outgraph, moves=allmoves}
    end
  end
end
