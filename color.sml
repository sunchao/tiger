(* The book recommends associating each node with
a membership flag. WE defined it here, but leave it for future improvement *)

structure Color : COLOR =
struct

structure Frame = MipsFrame

structure NS = ListSetFn(
  type ord_key = Graph.node
  fun compare (n1,n2) = Graph.compare(n1,n2))

structure MS = BinarySetFn(
  type ord_key = Graph.node*Graph.node
  fun compare ((n1,n2), (n1',n2')) =
      case Graph.compare(n1,n2) of
          EQUAL => Graph.compare(n1',n2')
        | od => od)

structure RS = ListSetFn(
    type ord_key = Frame.register
    fun compare (r1,r2) = String.compare(r1,r2))

structure WL = NS
structure GT = Graph.Table
structure TT = Temp.Table
structure T = Temp

type allocation = Frame.register TT.table

(* coloring function *)
fun color {interference = Liveness.IGRAPH{graph,tnode,gtemp,moves},
           initial=initAlloc, spillCost, registers} =
    let
      val simplifyWL : Graph.node list ref = ref nil
      val freezeWL : Graph.node list ref = ref nil
      val spillWL : Graph.node list ref = ref nil
      val coalescedMS = ref MS.empty
      val constrainedMS = ref MS.empty
      val frozenMS = ref MS.empty
      val worklistMS = ref MS.empty
      val activeMS = ref MS.empty
      val spillNS = ref NS.empty
      val coalescedNS = ref NS.empty
      val coloredNS = ref NS.empty
      val selectStack : Graph.node list ref = ref nil
      val colorTable : allocation ref = ref TT.empty

      fun remove l n = List.filter (fn (x) => not (Graph.eq(x,n))) l

      (* # of colors available *)
      val K = List.length registers

      (* precolorTable is a mapping from temp to register,
       * while initial is a list of uncolored nodes *)
      val (precolored, initial) =
          List.foldl
              (fn (n,(pt,ini)) =>
                  let val t = gtemp n in
                    case TT.look (initAlloc,t) of
                        SOME r => (TT.enter(pt,t,r),ini)
                      | NONE => (pt,n::ini)
                  end)
              (TT.empty,[]) (Graph.nodes graph)

      (* A map from graph nodes to their *initial* degree *)
      val degreeMap : int GT.table ref =
          ref (List.foldl
                   (fn (x,tb) => GT.enter(tb,x,List.length (Graph.adj x)))
                   GT.empty (Graph.nodes graph))

      (* Lookup degree for a graph node, assuming it is inside the map *)
      fun degree n = let val SOME(d) = GT.look(!degreeMap,n) in d end

      (* Create initial worklist *)
      fun makeWorklists initial =
          List.partition (fn n => List.length (Graph.adj n) < K) initial

      (* decrement degree for graph node n, return
       * modified degreeMap and a (possibly augmented) simplify worklist *)
      fun decrementDegree (n:Graph.node) =
          (* only decrement those non-precolored nodes - for *)
          (* precolored nodes, we treat as if they have infinite *)
          (* degree, since we shouldn't reassign them to different registers *)
          let val t = gtemp n in
            case TT.look(initAlloc,t) of
                SOME _ => ()
              | NONE =>
                let
                  val d = degree n
                in
                  degreeMap := GT.enter(!degreeMap,n,d-1); (* update n's degree *)
                  if (d = K) then
                     (simplifyWL := (!simplifyWL)@[n];
                      spillWL := remove (!spillWL) n)
                   else ()
                end
          end

      (* adjacenent nodes *)
      fun adjacent n =
          List.filter (fn r => not (List.exists (fn t => Graph.eq(r,t)) (!selectStack)))
                      (Graph.adj n)

      (* simplify the graph by keep removing the first node from simplify
       * worklist and add to select stack. At same time, decrement degree
       * for adjacent nodes of the removed node.
       * precondition: simplifyWL not nil. *)
      fun simplify () =
          case (!simplifyWL) of
              n::ns =>
              let in
                simplifyWL := ns;
                selectStack := n::(!selectStack);
                List.app (fn r => decrementDegree r) (adjacent n)
              end

      fun selectSpill () =
          let fun f min tlist =
                  case tlist of
                      nil => min
                    | r::rs =>
                      let val c1 = spillCost min
                          val c2 = spillCost r in
                          if Real.>=(c1,c2)
                          then f r rs else f min rs
                      end
          in case (!spillWL) of
                 r::rs =>
                 let val min = f r rs in
                   spillWL := remove (!spillWL) min;
                   simplifyWL := min::(!simplifyWL)
                 end
          end

      fun pickColor (regs: RS.set) : Frame.register
          = List.hd(RS.listItems(regs))

      (* assign color to all nodes on select stack. The parameter
       * colored is all nodes that are already assigned a color. *)
      fun assignColors (colored: allocation) : allocation =
          case (!selectStack) of
              nil => colored
            | n::ns =>
              let val availableColors =
                      List.foldl
                          (fn (w,cset) =>
                              case TT.look(colored, gtemp w) of
                                  SOME c =>
                                  if RS.member(cset,c) then
                                    RS.delete(cset,c) else cset
                                | NONE => cset)
                          (RS.addList(RS.empty,registers)) (Graph.adj n)
              in
                selectStack := ns;
                if RS.isEmpty(availableColors) then
                  (spillNS := NS.add((!spillNS), n);
                   assignColors(colored))
                else
                  let val r = pickColor(availableColors) in
                    assignColors(TT.enter(colored, gtemp n, r))
                  end
              end

      (* the main *loop* *)
      fun iter () =
          let in
            if (not (List.null (!simplifyWL))) then (simplify(); iter())
            else if (not (List.null (!spillWL))) then (selectSpill(); iter())
            else ()
          end
    in
      let
        val (si,sp) = makeWorklists initial
      in
        simplifyWL := si;
        spillWL := sp;
        iter();
        (assignColors(precolored),
         (map gtemp (NS.listItems (!spillNS))))
      end
    end
end

