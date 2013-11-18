(* Simple register coloring with spilling. *)

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


(* We maintain two worklists: for simplify and spilling. *)

type allocation = Frame.register TT.table

type worklists = Graph.node list * Graph.node list


fun remove l n = List.filter (fn (x) => not (Graph.eq(x,n))) l

fun println str = TextIO.output (TextIO.stdOut, str ^ "\n")

fun color {interference = Liveness.IGRAPH{graph,tnode,gtemp,moves},
           initial=initAlloc, spillCost, registers} =
    let
      (* # of colors available *)
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

      (* A map from graph nodes to their *initial* degree *)
      val degreeMap : int GT.table =
          List.foldl
              (fn (x,tb) => GT.enter(tb,x,List.length (Graph.adj x)))
              GT.empty (Graph.nodes graph)

      (* Lookup degree for a graph node, assuming it is inside the map *)
      fun degree (dm,n) = let val SOME(d) = GT.look(dm,n) in d end

      (* Create initial worklist *)
      fun makeWorklists initial : worklists =
          List.partition (fn n => List.length (Graph.adj n) < K) initial

      (* decrement degree for graph node n, return
       * modified degreeMap and a (possibly augmented) simplify worklist *)
      fun decrementDegree (n:Graph.node,
                           dm:int Graph.Table.table,
                           wls as (simplify,spilling)) =
          (* only decrement those non-precolored nodes - for *)
          (* precolored nodes, we treat as if they have infinite *)
          (* degree, since we shouldn't reassign them to different registers *)
          let val t = gtemp n in
            case TT.look(initAlloc,t) of
                SOME _ => (dm,wls)
              | NONE =>
                let
                  val d = degree (dm,n)
                  val dm' = GT.enter(dm,n,d-1) (* update n's degree *)
                in if (d = K) then
                       (dm',(simplify@[n],remove spilling n))
                   else (dm',wls) end
          end

      (* adjacenent nodes *)
      fun adjacent (n,st) =
          List.filter (fn r => not (List.exists (fn t => Graph.eq(r,t)) st))
                 (Graph.adj n)

      (* simplify the graph by keep removing the first node from simplify
       * worklist and add to select stack. At same time, decrement degree
       * for adjacent nodes of the removed node. *)
      fun simplify (stack:Graph.node list,
                    (si,sp):worklists,
                    dm:int GT.table) =
          case si of
              nil => (stack,sp,dm)
            | n::si' =>
              let
                  val (dm', wls') =
                    List.foldl
                        (fn (m,(dm,wls)) => decrementDegree (m,dm,wls))
                        (dm,(si',sp)) (adjacent(n,stack))
              in simplify (n::stack,wls',dm') end

      (* select a node for spill, according to spill cost *)
      fun selectSpill ((si,sp): worklists) =
          let fun f min tlist =
                  case tlist of
                      nil => min
                    | r::rs =>
                      let val c1 = spillCost min
                          val c2 = spillCost r in
                          if Real.>=(c1,c2)
                          then f r rs else f min rs
                      end
          in case sp of
                 r::rs =>
                 let val min = f r rs in
                   (min::si,remove sp min)
                 end
          end

      (* assign color to all nodes on select stack. The parameter
       * colored is all nodes that are already assigned a color. *)
      fun assignColors (stack:Graph.node list,spills,colored:allocation) =
          case stack of
              nil => (colored,spills)
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
                (* if no available colors, add the node to spills *)
                if RS.isEmpty(availableColors) then
                  assignColors(ns, (gtemp n)::spills, colored)
                else
                  (* choose a color from available colors, and assign
                   * it to node n. Also, mark this node as colored *)
                  assignColors(ns,spills,
                               TT.enter(colored, gtemp n,
                                        List.hd(RS.listItems(availableColors))))
              end

      (* the main *loop* *)
      fun iter (sti, wls: worklists, dmi) =
          let val (sto,spills,dmo) = simplify (sti,wls,dmi)
          in
            if List.length(spills) = 0 then sto
             else iter(sto,selectSpill([],spills),dmo)
          end
    in
      let
        val wls = makeWorklists initial
        val stack = iter([],wls,degreeMap)
      in
        assignColors(stack,[],precolorTable)
      end
    end
end
