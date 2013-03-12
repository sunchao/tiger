(* A simplified implementation of register coloring:
 * currently no spilling or coalescing *)

signature WORK_LIST = sig
  type 'a worklist
  exception Empty

  val empty : 'a worklist
  val is_empty : 'a worklist -> bool
  val add : ('a worklist * 'a) -> 'a worklist
  val remove : 'a worklist -> ('a worklist * 'a)
end

structure FIFOWorkList : WORK_LIST =
struct 
type 'a worklist = 'a list 
exception Empty 
val empty = nil
fun is_empty wl = List.null wl
fun add (wl,elem) = elem :: wl
fun remove nil = raise Empty
  | remove (x :: xs) = (xs, x)
end

structure Color : COLOR = 
struct 

exception NotEnoughRegister

structure G = Graph
structure GT = Graph.Table
structure TT = Temp.Table
structure Frame = MipsFrame
structure CS = BinarySetFn(type ord_key = Frame.register
                           val compare = String.compare)
type allocation = Frame.register TT.table

fun color {interference = Liveness.IGRAPH{graph,tnode,gtemp,moves},
           initial, spillCost, registers} = 
    let 

      (* total number of registers available *)
      val K = List.length registers

      (* whether this node has significant degree *)
      fun is_significant node = List.length (G.adj node) >= K

      (* given a graph, return a worklist 
       * contains all low-degree nodes in the graph *)
      fun initial graph = 
          List.foldl
              (fn (x,l) => 
                  if (is_significant x) then l else FIFOWorkList.add(l,x))
              FIFOWorkList.empty (G.nodes graph)

      (* a table from node to its degree, somewhat duplicated *)
      fun degrees graph = 
          List.foldl 
              (fn (x,table) => G.Table.enter(table,x,length (G.adj x)))
              G.Table.empty (G.nodes graph)

      (* find a node in the graph that has degree less than K.
       * Return SOME(n) if such a node is found, otherwise NONE *)
      fun findnode graph : G.node option = 
          List.find (fn (x) => List.length (G.adj x) <= K) (G.nodes graph)

      (* do one step of the simplify phase. we pop the top element
       * from the worklist, put it on stack, and push all its neighboring
       * nodes which have degree less than K into the worklist. *)
      fun simplify (degree,inlist,instack) =
          let
            val (worklist,node) = FIFOWorkList.remove inlist

            val (newtable,newlist) = 
                List.foldl
                    (fn (n,(table,worklist)) => 
                        case G.Table.look(table,n) of 
                          SOME(d) => 
                          let val nt = G.Table.enter(table,n,d-1) in
                            if d = K then (nt,FIFOWorkList.add(worklist,n))
                            else (nt,worklist)
                          end)
                    (degree,inlist) (G.adj node)
          in
            (newtable,newlist,node :: instack)
          end

      (* repeatly simplify the graph. if we end up with an empty
       * worklist, then check all nodes are on the stack (which means
       * no spill). If not, raise a exception (sorry cannot handle it) *)
      fun iter (indegree,inworklist,instack) =
          if FIFOWorkList.is_empty inworklist then
            (* worklist is empty, check if all nodes are in stack *)
            if length instack = length (G.nodes graph) then instack
            else raise NotEnoughRegister
          else iter(simplify(indegree, inworklist, instack))
    in 
      let 
        val selectstack = iter (degrees graph, initial graph, nil)
        
        val initial_alloc : allocation = TT.empty

        (* all available colors (registers) *)
        val ok_colors = CS.addList (CS.empty, registers)

        (* pick a color from a set, here we just choose the first one *)
        fun pick colors = List.hd (CS.listItems colors)

        (* keep poping nodes from stack, and assign color for them *)
        fun assign_colors (stack, alloc) =
            case stack of
              nil => (* great! it's done *) alloc
            | x :: xs => 
              let
                (* refine colors, based on the existing coloring of neighbors,
                 * using information in alloc *)
                fun elim_color (neighbors, colors) : CS.set =
                    if CS.isEmpty colors 
                    then raise NotEnoughRegister
                    else 
                      case neighbors of
                        nil => colors
                      | x :: xs => 
                        case TT.look (alloc,x) of
                          NONE => elim_color (xs,colors)
                        | SOME r => elim_color (xs, CS.delete (colors,r))
                                           
                val neighbors = map gtemp (G.adj x)

                val c = pick (elim_color(neighbors, ok_colors))
              in
                assign_colors (xs, TT.enter (alloc,(gtemp x),c))
              end
      in
        (assign_colors(selectstack,initial_alloc),nil)
      end
    end                                              
end
  
