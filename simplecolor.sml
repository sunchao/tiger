structure SimpleColor : COLOR =
struct

structure Frame = MipsFrame
structure TT = Temp.Table
structure GT = Graph.Table

(* Register Set *)
structure RS = ListSetFn(
    type ord_key = Frame.register
    fun compare (r1,r2) = String.compare(r1,r2))
                        
(* Node Set *)
structure NS = ListSetFn(
    type ord_key = Graph.node
    fun compare (n1,n2) = Graph.compare(n1,n2))

(* Raised when we need to spill some register *)
exception NotEnoughRegister
            
type allocation = Frame.register TT.table

fun println str = TextIO.output (TextIO.stdOut, str ^ "\n")

fun color {interference = Liveness.IGRAPH{graph,tnode,gtemp,moves},
           initial=initAlloc, spillCost, registers} = 
    let 
        val K = List.length registers
                            
        (* precolorTable is a mapping from temp to register,
         * while initial is a list of uncolored nodes *)
        val (precolorTable, initial) = 
            List.foldl
              (fn (n,(pt,ini)) => 
                  let val t = gtemp n in
                    case TT.look (initAlloc,t) of
                        SOME r => (TT.enter(pt,t,r),ini)
                      | NONE => (pt,n::ini)
                  end)
              (TT.empty,[]) (Graph.nodes graph)

        (* A map from graph node to its degree *)
        val degreeMap : int GT.table = 
            List.foldl 
              (fn (x,tb) => GT.enter(tb,x,List.length (Graph.adj x)))
              GT.empty (Graph.nodes graph)

        (* Lookup degree for a graph node, assuming it is inside the map *)
        fun degree (dm,n) = let val SOME(d) = GT.look(dm,n) in d end

        (* Create initial worklist *)
        fun makeWorklist initial = 
            List.filter (fn n => List.length (Graph.adj n) < K) initial

        (* decrement degree for graph node n, return
         * modified degreeMap and a (possibly augmented) simplify worklist *)
        fun decrementDegree (n:Graph.node,
                             dm:int Graph.Table.table,
                             worklist:Graph.node list) =
            (* only decrement those non-precolored nodes - for *)
            (* precolored nodes, we treat as if they have infinite *)
            (* degree, since we shouldn't reassign them to different registers *)
            let val t = gtemp n in
              case TT.look(initAlloc,t) of
                  SOME _ => (dm,worklist)
                | NONE => 
                  let 
                    val d = degree (dm,n)
                    val dm' = GT.enter(dm,n,d-1) (* update n's degree *)
                  in if (d = K) then (dm',worklist@[n]) else (dm',worklist) end
            end
            
        (* adjacenent nodes *)     
        fun adjacent n = Graph.adj n

        (* Simplify the graph by keep removing the first node from simplify
         * worklist and add to select stack. At same time, decrement degree
         * for adjacent nodes of the removed node *)
        fun simplify (stack:Graph.node list,
                      worklist:Graph.node list,
                      dm:int GT.table) :
            (Graph.node list * Graph.node list * int GT.table) = 
            case worklist of
                [] => (stack,[],dm)
              | n::wl => 
                let
                  val (dm', wl') = 
                      List.foldl
                        (fn (m,(dm,wl)) => decrementDegree (m,dm,wl))
                        (dm,wl) (adjacent n)
                in simplify (n::stack,wl',dm') end

        (* Assign color to all nodes on select stack. The parameter
         * colored is all nodes that are already assigned a color. *)
        fun assignColors (stack:Graph.node list,colored:allocation) = 
            case stack of
                [] => colored
              | n::ns => 
                let
                  val availableColors =
                      List.foldl
                        (fn (w,cset) =>
                            case TT.look(colored,gtemp w) of
                                SOME c => 
                                if RS.member(cset,c) then
                                  RS.delete(cset,c) else cset
                              | NONE => cset)
                        (RS.addList(RS.empty,registers)) (Graph.adj n)
                in 
                  (* If no available colors, we throw exception *)
                  if RS.isEmpty(availableColors) then
                    raise NotEnoughRegister
                  else 
                    (* Choose a color from available colors, and assign
                     * it to node n. Also, mark this node as colored *)
                    assignColors(ns,
                      TT.enter(colored, gtemp n,
                               List.hd(RS.listItems(availableColors))))
                end
    in
      let
        val worklist = makeWorklist initial
        val (stack,nodes,dm) = simplify ([],worklist,degreeMap)
      in
        (assignColors (stack,precolorTable),nil)
      end
    end
end
