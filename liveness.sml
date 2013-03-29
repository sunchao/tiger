signature LIVENESS = 
sig
  datatype igraph =
           IGRAPH of {graph: Graph.graph,
                      tnode: Temp.temp -> Graph.node,
                      gtemp: Graph.node -> Temp.temp,
                      moves: (Graph.node * Graph.node) list}

  val interferenceGraph : 
      Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list)

  val show : TextIO.outstream * igraph -> unit

end

structure TempKey : ORD_KEY = 
struct
type ord_key = Temp.temp
fun compare (t1,t2) = 
    String.compare(Temp.makestring t1,Temp.makestring t2)
end

structure Liveness :> LIVENESS = 
struct 

structure G = Graph
structure GT = Graph.Table
structure S = ListSetFn(TempKey)
structure T = Temp
structure TT = Temp.Table
structure Frame = MipsFrame

datatype igraph =
         IGRAPH of {graph: G.graph,
                    tnode: T.temp -> G.node,
                    gtemp: G.node -> T.temp,
                    moves: (G.node * G.node) list}
                   
type liveSet = S.set
type liveMap = liveSet GT.table
type tempEdge = {src:Temp.temp,dst:Temp.temp}

                                                   
fun show (output,IGRAPH{graph,tnode,gtemp,moves}) = 
    let
	val node2str = Frame.temp_name o gtemp
	fun process1 node =
	    TextIO.output(output,
			  ((node2str node) ^ " -> {" ^
			   (String.concatWith 
				", "
				(map node2str (Graph.adj node))) ^ "}\n"))
    in app process1 (Graph.nodes graph) end

    
fun interferenceGraph
        (flowgraph as Flow.FGRAPH{control,def,use,ismove} ) = 
  let 
    val igraph = G.newGraph ()
                 
    val allnodes = G.nodes control
                   
    fun look (table,key) = valOf(GT.look(table,key))

    fun set2str set = "{" ^ (String.concatWith ", "
                             (map Temp.makestring (S.listItems set))) ^ "}"

    fun println s = print(s ^ "\n")

    (* iteratively compute liveSet, until reaches a fix point *)
    fun iter (livein_map,liveout_map) = 
      let 
        val changed = ref false
        fun compute_out (m,n) = 
            foldl (fn (x,s) => S.union(look(m,x),s))
                  S.empty (G.succ n)
        val (new_livein_map, new_liveout_map) = 
            foldl
              (fn (x,(mi,mo)) =>
                  let val oldin = look(mi,x)
                      val oldout = look(mo,x) 
                      val usex = S.addList(S.empty,look(use,x))
                      val defx = S.addList(S.empty,look(def,x))
                      val newin = S.union(usex,S.difference(oldout,defx))
                      val newout = compute_out(mi,x) 
                  in
                    if S.equal(newin,oldin) andalso S.equal(newout,oldout)
                    then () else changed := true;
                    (GT.enter(mi,x,newin),GT.enter(mo,x,newout))
                  end
              ) (livein_map,liveout_map) (G.nodes control)
      in
        if !changed then iter (new_livein_map,new_liveout_map)
        else new_liveout_map
      end

    fun make_empty_map () =
        foldl (fn (n,m) => GT.enter(m,n,S.empty)) GT.empty allnodes

    val liveout = iter ((make_empty_map ()), (make_empty_map ()))
  in
    (* now for each node n in the flow graph, suppose
     * there is a newly define temp d, and temporaries
     * t1, ..., tn are in liveout set of node n. Then,
     * we add edge (d,t1), ..., (d,tn) to the igraph.
     * Mappings between temps and igraph nodes are also recorded. *)
    let

      fun find_edges node : tempEdge list = 
        let
          val defset = look(def,node)
          val outset = S.listItems(look(liveout,node))
          fun f t = foldl (fn (t',l) => {src=t,dst=t'}::l) nil outset
        in foldl (fn (t,l) => (f t) @ l) nil defset end
        
      val all_edges : tempEdge list = 
          foldl (fn (n,l) => find_edges(n) @ l) nil allnodes
          
      fun make_table tlist = 
        foldl
          (fn (t,(t2n,n2t)) => 
            case TT.look(t2n,t) of
              SOME(n) => (t2n,n2t)
            | NONE =>
              let val n = Graph.newNode(igraph) in
                (TT.enter(t2n,t,n),GT.enter(n2t,n,t)) end
          ) (TT.empty,GT.empty) tlist
        
      val (temp2node,node2temp) = 
        make_table ((map #src all_edges) @ (map #dst all_edges))
          
      fun looktemp (t: T.temp) : G.node = valOf(TT.look(temp2node,t))
                                                
      fun looknode (n:G.node) : T.temp = valOf(GT.look(node2temp,n))
                                                
      fun lookliveout (n:G.node) : T.temp list = 
	        S.listItems(valOf(GT.look(liveout,n)))

      val allmoves = 
        foldl
          (fn (n,l) =>
            case GT.look(ismove,n) of
              SOME(b) => 
              if b then 
                let val [src] = look(use,n)
                    val [dst] = look(def,n) in
                  (looktemp(src),looktemp(dst)) :: l
                end
              else l
          ) nil allnodes
                                                  
    in app (fn {src,dst} => 
               G.mk_edge{from=looktemp(src),to=looktemp(dst)})
           all_edges;
       (IGRAPH{graph=igraph,tnode=looktemp,gtemp=looknode,moves=allmoves},
        lookliveout)
    end
  end
end
