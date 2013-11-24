(* Simple register coloring with spilling, but without coalescing. *)

structure SimpleColor : COLOR =
struct

structure Frame = MipsFrame
structure TT = Temp.Table
structure L = Liveness
structure LI = Liveness.I

(* Register Set *)
structure RS = ListSetFn(
    type ord_key = Frame.register
    fun compare (r1,r2) = String.compare(r1,r2))


(* We maintain two worklists: for simplify and spilling. *)

type allocation = Frame.register TT.table

type worklists = LI.node list * LI.node list

fun remove l n = List.filter (fn (x) => x <> n) l

fun println str = TextIO.output (TextIO.stdOut, str ^ "\n")

fun color{interference = L.IGRAPH{graph,moves},
          initial=initAlloc, spillCost, registers} =
    let
      (* # of colors available *)
      val K = List.length registers

      (* precolorTable is a mapping from temp to register,
       * while initial is a list of uncolored nodes *)
      val (precolorTable, initial) =
          List.foldl
              (fn (n as LI.NODE{temp,adj,status},(pt,ini)) =>
                  case TT.look(initAlloc,temp) of
                        SOME r => (TT.enter(pt,temp,r),ini)
                      | NONE => (pt,n::ini))
              (TT.empty,nil) graph

      (* A map from graph nodes to their *initial* degree *)
      val degreeMap : int TT.table =
          List.foldl
              (fn (LI.NODE{temp,adj,...},tb) =>
                  TT.enter(tb,temp,List.length (!adj)))
              TT.empty (graph)

      (* Lookup degree for a graph node, assuming it is inside the map *)
      fun degree (dm,LI.NODE{temp,...}) = valOf(TT.look(dm,temp))

      (* Create initial worklist *)
      fun makeWorklists initial : worklists =
          List.partition (fn LI.NODE{adj,...} => List.length (!adj) < K) initial

      (* decrement degree for graph node n, return
       * modified degreeMap and a (possibly augmented) simplify worklist *)
      fun decrementDegree (n as LI.NODE{temp,adj,status},
                           dm: int TT.table,
                           wls as (simplify,spilling)) =
          (* only decrement those non-precolored nodes - for *)
          (* precolored nodes, we treat as if they have infinite *)
          (* degree, since we shouldn't reassign them to different registers *)
          case TT.look(initAlloc,temp) of
              SOME _ => (dm,wls)
            | NONE =>
              let
                val d = degree(dm,n)
                val dm' = TT.enter(dm,temp,d-1) (* update n's degree *)
              in if (d = K) then
                   (dm',(simplify@[n],remove spilling n))
                 else (dm',wls)
              end

      (* adjacenent nodes *)
      fun adjacent (LI.NODE{adj,...},st) =
          List.filter (fn r => not (List.exists (fn t => t = r) st))
                      (!adj)

      (* simplify the graph by keep removing the first node from simplify
       * worklist and add to select stack. At same time, decrement degree
       * for adjacent nodes of the removed node. *)
      fun simplify (stack: LI.node list,
                    (si,sp): worklists,
                    dm: int TT.table) =
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
          let fun f (min as LI.NODE{temp=t',...}) tlist =
                  case tlist of
                      nil => min
                    | (r as LI.NODE{temp=t,...})::rs =>
                      let val c1 = spillCost t'
                          val c2 = spillCost t in
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
      fun assignColors (stack: LI.node list, spills, colored: allocation) =
          case stack of
              nil => (colored,spills)
            | LI.NODE{temp=n,adj,...}::ns =>
              let
                val availableColors =
                    List.foldl
                        (fn (w as LI.NODE{temp,...}, cset) =>
                            case TT.look(colored,temp) of
                                SOME c =>
                                if RS.member(cset,c) then
                                  RS.delete(cset,c) else cset
                              | NONE => cset)
                        (RS.addList(RS.empty,registers)) (!adj)
              in
                (* if no available colors, add the node to spills *)
                if RS.isEmpty(availableColors) then
                  assignColors(ns, n::spills, colored)
                else
                  (* choose a color from available colors, and assign
                   * it to node n. Also, mark this node as colored *)
                  assignColors(ns,spills,
                               TT.enter(colored, n,
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
