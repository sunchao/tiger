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
      case Graph.compare(n1,n1') of
          EQUAL => Graph.compare(n2,n2')
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
fun color{interference = Liveness.IGRAPH{graph,tnode,gtemp,movesg},
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
      val colored : allocation ref = ref TT.empty
      val moveList : MS.set GT.table ref = ref GT.empty

      val precolored = ref NS.empty
      val initial : Graph.node list ref = ref nil
      val alias : Graph.node GT.table ref = ref GT.empty

      fun println s = print (s ^ "\n")

      fun remove l n = List.filter (fn (x) => not (Graph.eq(x,n))) l

      fun member l n = List.exists (fn x => Graph.eq(x,n)) l

      (* # of colors available *)
      val K = List.length registers


      (* precolorTable is a mapping from temp to register,
       * while initial is a list of uncolored nodes *)
      fun build () =
          let
            fun addMove(n: Graph.node, mv: Graph.node * Graph.node) =
                case GT.look(!moveList,n) of
                    NONE => moveList := GT.enter(!moveList,n,MS.singleton(mv))
                  | SOME ms => moveList := GT.enter(!moveList,n,MS.add(ms,mv))
          in
            (* initialize colored and precolored *)
            List.app
                (fn n =>
                    let val t = gtemp n in
                      case TT.look (initAlloc,t) of
                          SOME r =>
                          let in
                            colored := TT.enter(!colored,t,r);
                            precolored := NS.add(!precolored,n)
                          end
                        | NONE =>
                          initial := n::(!initial)
                    end)
                (Graph.nodes graph);

            (* associate each node with a empty move set *)
            List.app
                (fn n => moveList := GT.enter(!moveList, n, MS.empty))
                (Graph.nodes graph);

            (* iniitalize worklistMS and moveList *)
            List.app
                (fn (m as (src,dst)) =>
                    let in
                      addMove(src, m);
                      addMove(dst, m);
                      worklistMS := MS.add(!worklistMS, m)
                    end)
                moves
          end


      (* A map from graph nodes to their *initial* degree *)
      val degreeMap : int GT.table ref =
          ref (List.foldl
                   (fn (x,tb) => GT.enter(tb,x,List.length (Graph.adj x)))
                   GT.empty (Graph.nodes graph))

      (* Lookup degree for a graph node, assuming it is inside the map *)
      fun degree n = valOf(GT.look(!degreeMap,n))

      (* check invariants, which should hold after build() is called *)
      fun checkInv loc =
          (* 1, degree invariants *)
          let
            val wls = (!simplifyWL)@(!freezeWL)@(!spillWL)

            fun check_degree () =
                List.app
                    (fn u =>
                        if (degree u) <>
                           NS.numItems(
                             NS.intersection(
                               (NS.addList(NS.empty,Graph.adj u),
                                NS.addList(!precolored,wls))))
                        then ErrorMsg.impossible(
                            loc ^
                            " (degree): node " ^
                            (Graph.nodename u) ^
                            " degree wrong.")
                        else ())
                    wls

            fun check_simplify () =
                List.app
                    (fn u =>
                        if degree u >= K then
                          ErrorMsg.impossible(
                            loc ^
                            " (freeze): node " ^
                            (Graph.nodename u) ^
                            " has degree >= K")
                        else if
                          not (MS.isEmpty(
                                  MS.intersection(
                                    valOf(GT.look(!moveList,u)),
                                    MS.union(!activeMS,!worklistMS))))
                        then
                          ErrorMsg.impossible(
                            loc ^
                            " (freeze): node " ^
                            (Graph.nodename u) ^
                            " has empty intersection.")
                        else ())
                    (!simplifyWL)

            fun check_freeze () =
                List.app
                    (fn u =>
                        if degree u >= K then
                          ErrorMsg.impossible(
                            loc ^
                            " (freeze): node " ^
                            (Graph.nodename u) ^
                            " has degree >= K")
                        else if
                          (MS.isEmpty(
                          MS.intersection(
                            valOf(GT.look(!moveList,u)),
                            MS.union(!activeMS,!worklistMS))))
                        then
                          ErrorMsg.impossible(
                            loc ^
                            " (freeze): node " ^
                            (Graph.nodename u) ^
                            " has empty intersection. length of moveList[u]: " ^
                            (Int.toString(MS.numItems(valOf(GT.look(!moveList,u))))))
                        else ())
                    (!freezeWL)

            fun check_spill () =
                List.all
                    (fn u => degree u >= K)
                    (!spillWL)
          in
            check_degree();
            check_simplify();
            check_freeze();
            check_spill();
            println(loc ^ ": invariant check passed")
          end

      fun nodeMoves n =
          let in
            case GT.look(!moveList,n) of
                NONE =>
                ErrorMsg.impossible (
                  "in nodeMoves, should have entry " ^
                  "for node " ^ (Graph.nodename n))
              | SOME ms =>
                MS.intersection(ms,MS.union(!activeMS,!worklistMS))
          end

      fun moveRelated n = not (MS.isEmpty (nodeMoves n))

      (* Create initial worklist *)
      fun makeWorklist () =
          let in
            checkInv("begin of makeWorklist");
            List.app
                (fn n =>
                    if (degree n) >= K then
                      spillWL := n::(!spillWL)
                    else if moveRelated n then
                      freezeWL := n::(!freezeWL)
                    else
                      simplifyWL := n::(!simplifyWL))
                (!initial);
            checkInv("end of makeWorklist")
          end


      fun enableMoves(nodes: NS.set) =
          let
            fun enable1 n =
                MS.app
                    (fn m =>
                        if MS.member(!activeMS,m) then
                          let in
                            activeMS := MS.delete(!activeMS,m);
                            worklistMS := MS.add(!worklistMS,m)
                          end
                        else ())
                    (nodeMoves n)
          in
            NS.app enable1 nodes
          end


      fun addWorklist u =
          if not (NS.member(!precolored,u)) andalso
             (not (moveRelated u)) andalso (degree u < K)
          then
            let in
              freezeWL := remove (!freezeWL) u;
              simplifyWL := u::(!simplifyWL)
            end
          else ()

      fun getAlias n =
          if NS.member(!coalescedNS,n)
          then getAlias(valOf(GT.look(!alias,n)))
          else n

      (* adjacenent nodes *)
      fun adjacent n =
          NS.difference(NS.addList(NS.empty,Graph.adj n),
                        NS.union(NS.addList(NS.empty,!selectStack),
                                 !coalescedNS))

      (* decrement degree for graph node n, return
       * modified degreeMap and a (possibly augmented) simplify worklist *)
      fun decrementDegree (n:Graph.node) : unit =
          (* only decrement those non-precolored nodes - for *)
          (* precolored nodes, we treat as if they have infinite *)
          (* degree, since we shouldn't reassign them to different registers *)
          let
            val t = gtemp n
          in
            case TT.look(initAlloc,t) of
                SOME _ => ()
              | NONE =>
                let
                  val d = degree n
                in
                  degreeMap := GT.enter(!degreeMap,n,d-1); (* update n's degree *)
                  if (d = K) then
                    let in
                      enableMoves(NS.union(NS.singleton(n),adjacent n));
                      spillWL := remove (!spillWL) n;
                      if moveRelated n then
                        freezeWL := n::(!freezeWL)
                      else
                        simplifyWL := n::(!simplifyWL)
                    end
                  else ()
                end
          end

      fun inAdj(u,v) = member (Graph.adj u) v

      fun OK(t,r) =
          degree t < K orelse
          NS.member(!precolored,t) orelse
          inAdj(t,r)

      fun conservative nodes =
          let
            val k = ref 0
          in
            NS.app
                (fn (n) =>
                    if degree n >= K
                    then k := !k + 1
                    else ()
                ) nodes;
            (!k < K)
          end

      fun addEdge(u,v) =
          if not (inAdj(u,v)) andalso not (Graph.eq(u,v)) then
            let in
              if (not (NS.member(!precolored,u))) then
                (Graph.mk_edge{from=u,to=v};
                 degreeMap := GT.enter(!degreeMap,u,(degree u)+1))
              else ();
              if (not (NS.member(!precolored,v))) then
                (Graph.mk_edge{from=v,to=u};
                 degreeMap := GT.enter(!degreeMap,v,(degree v)+1))
              else ()
            end
          else ()

      fun combine(u,v) =
          let in
            checkInv("begin of combine");
            if member (!freezeWL) v then
              freezeWL := remove (!freezeWL) v
            else
              spillWL := remove (!spillWL) v;
            coalescedNS := NS.add(!coalescedNS,v);
            alias := GT.enter(!alias,v,u);
            let
              val mv_u = valOf(GT.look(!moveList,u))
              val mv_v = valOf(GT.look(!moveList,v))
            in
              moveList := GT.enter(!moveList,u,MS.union(mv_u,mv_v));
              enableMoves(NS.singleton(v))
            end;
            NS.app (fn t => (addEdge(t,u); decrementDegree(t))) (adjacent v);
            if degree u >= K andalso member (!freezeWL) u
            then
              let in
                freezeWL := remove (!freezeWL) u;
                spillWL := u::(!spillWL)
              end
            else ();
            checkInv("end of combine")
          end

      fun coalesce () =
          let
            val _ = println("worklistMS.length: " ^
                          (Int.toString (MS.numItems (!worklistMS))))
            val m as (x',y') = hd(MS.listItems(!worklistMS))
            val x = getAlias(x')
            val y = getAlias(y')
            val (u,v) = if NS.member(!precolored,y) then (y,x) else (x,y)
            val () = worklistMS := MS.delete(!worklistMS,m)
            fun allOK (u,nodes) = not (NS.exists (fn t => not (OK(t,u))) nodes)
          in
            if Graph.eq(u,v) then
              let in
                println("In case 1");
                coalescedMS := MS.add(!coalescedMS, m);
                addWorklist(u)
              end
            else
              if NS.member(!precolored,v) orelse inAdj(u,v)
              then
                let in
                  println("In case 2");
                  constrainedMS := MS.add(!constrainedMS, m);
                  addWorklist(u);
                  addWorklist(v)
                end
              else
                if (NS.member(!precolored,u) andalso allOK(u,adjacent v))
                   orelse
                   ((not (NS.member(!precolored,u))) andalso
                    conservative(NS.union(adjacent(u), adjacent(v))))
                then
                  let in
                    println("In case 4");
                    coalescedMS := MS.add(!coalescedMS, m);
                    combine(u,v);
                    addWorklist(u)
                  end
                else
                  let in
                    println("In case 5");
                    activeMS := MS.add(!activeMS, m)
                  end
          end

      fun freezeMoves (u) =
          let
            fun freeze1 (m as (x,y)) =
                let
                  val v =
                      if Graph.eq(getAlias(y),getAlias(u))
                      then getAlias(x) else getAlias(y)
                  in
                    activeMS := MS.delete(!activeMS, m);
                    frozenMS := MS.add(!frozenMS, m);
                    if MS.isEmpty(nodeMoves(v)) andalso (degree v) < K then
                      let in
                        freezeWL := remove (!freezeWL) v;
                        simplifyWL := v::(!simplifyWL)
                      end
                    else ()
                end
          in
            MS.app freeze1 (nodeMoves u)
          end

      fun freeze () =
          case (!freezeWL) of
              u::us =>
              let in
                freezeWL := us;
                simplifyWL := u::(!simplifyWL);
                freezeMoves(u)
              end

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
                NS.app (fn r => decrementDegree r) (adjacent n)
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
                   simplifyWL := min::(!simplifyWL);
                   freezeMoves(min)
                 end
          end

      fun pickColor (regs: RS.set) : Frame.register
          = List.hd(RS.listItems(regs))

      (* assign color to all nodes on select stack. The parameter
       * colored is all nodes that are already assigned a color. *)
      fun assignColors () : allocation =
          case (!selectStack) of
              nil =>
              let in
                println("In assignColors");
                NS.app
                    (fn n =>
                        let
                          val c = valOf(TT.look(!colored, gtemp (getAlias(n))))
                        in
                          colored := TT.enter(!colored,gtemp n,c)
                        end)
                    (!coalescedNS);
                !colored
              end
            | n::ns =>
              let
                val availableColors =
                    List.foldl
                        (fn (w,cset) =>
                            case TT.look(!colored, gtemp w) of
                                SOME c =>
                                if RS.member(cset,c) then
                                  RS.delete(cset,c) else cset
                              | NONE => cset)
                        (RS.addList(RS.empty,registers)) (Graph.adj n)
              in
                selectStack := ns;
                if RS.isEmpty(availableColors) then
                  spillNS := NS.add((!spillNS), n)
                else
                  let val r = pickColor(availableColors) in
                    coloredNS := NS.add(!coloredNS,n);
                    colored := TT.enter(!colored, gtemp n, r)
                  end;
                assignColors()
              end

      (* the main *loop* *)
      fun iter () =
          let in
            if (not (List.null (!simplifyWL))) then (simplify(); iter())
            else if (not (MS.isEmpty (!worklistMS))) then (coalesce(); iter())
            else if (not (List.null (!freezeWL))) then (freeze(); iter())
            else if (not (List.null (!spillWL))) then (selectSpill(); iter())
            else ()
          end
    in
      let
      in
        build();
        (* println("worklistMS: " ^ Int.toString(MS.numItems(!worklistMS))); *)
        makeWorklist();
        iter();
        (assignColors(), (map gtemp (NS.listItems (!spillNS))))
      end
    end
end

