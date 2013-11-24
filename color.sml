(* The book recommends associating each node with
a membership flag. WE defined it here, but leave it for future improvement *)

structure Color : COLOR =
struct

structure LI = Liveness.I
structure Frame = MipsFrame

(* move set *)
structure NS = BinarySetFn(
  type ord_key = LI.node
  fun compare(LI.NODE{temp=t1,...},LI.NODE{temp=t2,...})
      = String.compare(Temp.makestring t1,Temp.makestring t2))

structure MS = BinarySetFn(
  type ord_key = LI.node*LI.node
  fun compare((LI.NODE{temp=t1,...},
                LI.NODE{temp=t2,...}),
               (LI.NODE{temp=t1',...},
                LI.NODE{temp=t2',...})) =
    case String.compare(Temp.makestring t1,Temp.makestring t1') of
      EQUAL => String.compare(Temp.makestring t2,Temp.makestring t2')
     | od => od)

(* register set *)
structure RS = ListSetFn(
    type ord_key = Frame.register
    fun compare (r1,r2) = String.compare(r1,r2))

structure WL = NS
structure TT = Temp.Table
structure T = Temp

type allocation = Frame.register TT.table

(* coloring function *)
fun color{interference = Liveness.IGRAPH{graph,moves},
          initial=initAlloc, spillCost, registers} =
  let
    val simplifyWL : LI.node list ref = ref nil
    val freezeWL : LI.node list ref = ref nil
    val spillWL : LI.node list ref = ref nil

    val coalescedMS = ref MS.empty
    val constrainedMS = ref MS.empty
    val frozenMS = ref MS.empty
    val worklistMS = ref MS.empty
    val activeMS = ref MS.empty

    val spillNS = ref NS.empty
    val coalescedNS = ref NS.empty
    val coloredNS = ref NS.empty

    val selectStack : LI.node list ref = ref nil
    val colored : allocation ref = ref TT.empty
    val moveList : MS.set TT.table ref = ref TT.empty

    val precolored = ref NS.empty
    val initial : LI.node list ref = ref nil
    val alias : LI.node TT.table ref = ref TT.empty

    fun println s = print (s ^ "\n")

    fun remove l n = List.filter (fn (x) => x <> n) l

    fun member l n = List.exists (fn x => x = n) l

    fun nodename(LI.NODE{temp,...}) = Frame.temp_name temp

      (* # of colors available *)
    val K = List.length registers

    (* get degree of a node *)
    fun degree(LI.NODE{status,...}) =
        case (!status) of
            LI.INGRAPH(d) => d
          | _  => ErrorMsg.impossible("calling degree on removed or colored")

    (* precolorTable is a mapping from temp to register,
     * while initial is a list of uncolored nodes *)
    fun build () =
        let
          fun addMove(LI.NODE{temp,adj,status}, mv) =
              let
                val s =
                    case TT.look(!moveList,temp) of
                        NONE => MS.empty
                      | SOME ms => ms
              in moveList := TT.enter(!moveList,temp,MS.add(s,mv))
              end
        in
          (* initialize colored and precolored *)
          app
              (fn (n as LI.NODE{temp,adj,status}) =>
                  case TT.look (initAlloc,temp) of
                      SOME r =>
                      let in
                        colored := TT.enter(!colored,temp,r);
                        precolored := NS.add(!precolored,n)
                      end
                    | NONE =>
                      initial := n::(!initial)
              )
              (graph);

          (* associate each node with a empty move set *)
          app
              (fn LI.NODE{temp,...} =>
                  moveList := TT.enter(!moveList,temp,MS.empty))
              (graph);

          (* iniitalize worklistMS and moveList *)
          app
              (fn (m as (src,dst)) =>
                  let in
                    if (not (NS.member(!precolored,src)))
                    then addMove(src, m) else ();
                    if (not (NS.member(!precolored,dst)))
                    then addMove(dst, m) else ();
                    worklistMS := MS.add(!worklistMS, m)
                  end)
              moves
        end

    fun nodeMoves(LI.NODE{temp,adj,status}) =
        case TT.look(!moveList,temp) of
            SOME ms =>
            MS.intersection(ms,MS.union(!activeMS,!worklistMS))

    fun moveRelated n = not (MS.isEmpty (nodeMoves n))

    (* Create initial worklist *)
    fun makeWorklist () =
        app
            (fn (n as LI.NODE{temp,adj,status}) =>
                case (!status) of
                    LI.INGRAPH(d) =>
                    if d >= K then
                      spillWL := n::(!spillWL)
                    else if moveRelated n then
                      freezeWL := n::(!freezeWL)
                    else
                      simplifyWL := n::(!simplifyWL)
                  | _ => ErrorMsg.impossible("error in makeWorklist"))
            (!initial);

    fun enableMoves(nodes: NS.set) =
        let
          fun enable1 n =
              MS.app
                  (fn (m as (x,y)) =>
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


    fun addWorklist (u as LI.NODE{temp,status,adj}) =
        case (!status) of
            LI.INGRAPH(d) =>
            (if not (NS.member(!precolored,u)) andalso
                (not (moveRelated u)) andalso (d < K)
             then
               let in
                 freezeWL := remove (!freezeWL) u;
                 simplifyWL := u::(!simplifyWL)
               end
             else ())
          | _ => ErrorMsg.impossible("error in addWorklist")

    fun getAlias (n as LI.NODE{temp,...}) =
        if NS.member(!coalescedNS,n)
        then getAlias(valOf(TT.look(!alias,temp)))
        else n

    (* adjacenent nodes *)
    fun adjacent (n as LI.NODE{temp,adj,...}) =
        NS.difference(NS.addList(NS.empty,(!adj)),
                      NS.union(NS.addList(NS.empty,!selectStack),
                               !coalescedNS))

    (* decrement degree for graph node n, return
     * modified degreeMap and a (possibly augmented) simplify worklist *)
    fun decrementDegree(n as LI.NODE{temp,adj,status}) : unit =
        (* only decrement those non-precolored nodes - for *)
        (* precolored nodes, we treat as if they have infinite *)
        (* degree, since we shouldn't reassign them to different registers *)
        case (!status) of
            LI.INGRAPH(d) =>
            (let in
               case TT.look(initAlloc,temp) of
                   SOME _ => ()
                 | NONE =>
                   let in
                     status := LI.INGRAPH(d-1);
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
             end)
          | _ => ErrorMsg.impossible("error in decrementDegree")

    (* whether v is in adj of u.
     * TODO: replace with more efficient adjSet *)
    fun inAdj(LI.NODE{adj,...}, v) = member (!adj) v

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

    (* add new edge to graph *)
    fun addEdge(u as LI.NODE{temp=tu,adj=adju,status=stu},
                v as LI.NODE{temp=tv,adj=adjv,status=stv}) =
        case (!stu,!stv) of
            (LI.INGRAPH(du),LI.INGRAPH(dv)) =>
            if not (inAdj(u,v)) andalso u <> v then
              let in
                if (not (NS.member(!precolored,u))) then
                  (adju := v::(!adju); stu := LI.INGRAPH(du+1))
                else ();
                if (not (NS.member(!precolored,v))) then
                  (adjv := u::(!adjv); stv := LI.INGRAPH(dv+1))
                else ()
              end
            else ()
          | (_,_) => ErrorMsg.impossible("calling addEdge on removed or colored")

    fun combine(u as LI.NODE{temp=tu,...},
                v as LI.NODE{temp=tv,...}) =
        let in
          if member (!freezeWL) v then
            freezeWL := remove (!freezeWL) v
          else
            spillWL := remove (!spillWL) v;
          coalescedNS := NS.add(!coalescedNS,v);
          alias := TT.enter(!alias,tv,u);
          let
            val mv_u = valOf(TT.look(!moveList,tu))
            val mv_v = valOf(TT.look(!moveList,tv))
          in
            moveList := TT.enter(!moveList,tu,MS.union(mv_u,mv_v));
            enableMoves(NS.singleton(v))
          end;
          NS.app (fn t => (addEdge(t,u); decrementDegree(t))) (adjacent v);
          if degree u >= K andalso member (!freezeWL) u
          then
            let in
              freezeWL := remove (!freezeWL) u;
              spillWL := u::(!spillWL)
            end
          else ()
        end

    fun coalesce () =
        let
          val m as (x',y') = hd(MS.listItems(!worklistMS))
          val x = getAlias(x')
          val y = getAlias(y')
          val (u,v) = if NS.member(!precolored,y) then (y,x) else (x,y)
          val () = worklistMS := MS.delete(!worklistMS,m)
          fun allOK (u,nodes) = not (NS.exists (fn t => not (OK(t,u))) nodes)
        in
          if u = v then
            let in
              coalescedMS := MS.add(!coalescedMS, m);
              addWorklist(u)
            end
          else
            if NS.member(!precolored,v) orelse inAdj(u,v)
            then
              let in
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
                  coalescedMS := MS.add(!coalescedMS, m);
                  combine(u,v);
                  addWorklist(u)
                end
              else
                activeMS := MS.add(!activeMS, m)
        end

    fun freezeMoves (u) =
        let
          fun freeze1 (m as (x as LI.NODE{temp=tx,...},y as LI.NODE{temp=ty,...})) =
              let
                val v = if getAlias(y) = getAlias(u)
                        then getAlias(x) else getAlias(y)
              in
                activeMS := MS.delete(!activeMS, m);
                frozenMS := MS.add(!frozenMS, m);
                if MS.isEmpty(nodeMoves(v)) andalso (degree v) < K
                   andalso not (NS.member(!precolored,v))
                then
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
        let
          fun f (min as LI.NODE{temp=t',...}) tlist =
              case tlist of
                  nil => min
                | (r as LI.NODE{temp=t,...})::rs =>
                  let val c1 = spillCost t'
                      val c2 = spillCost t in
                    if Real.>=(c1,c2)
                    then f r rs else f min rs
                  end
        in
          case (!spillWL) of
              r::rs =>
              let val (min as LI.NODE{temp,...}) = f r rs in
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
              NS.app
                  (fn (n as LI.NODE{temp=tn,...}) =>
                      let
                        val LI.NODE{temp=t,...} = getAlias(n)
                        val c = valOf(TT.look(!colored,t))
                      in
                        colored := TT.enter(!colored,tn,c)
                      end)
                  (!coalescedNS);
              !colored
            end
          | (n as LI.NODE{temp=tn,adj=adjn,...})::ns =>
            let
              val availableColors =
                  List.foldl
                      (fn (w as LI.NODE{temp=tw,...},cset) =>
                          let
                            val (w' as LI.NODE{temp=tw',...}) = getAlias(w)
                          in
                            if NS.member(NS.union(!coloredNS,!precolored),w')
                            then
                              case TT.look(!colored,tw') of
                                  SOME c =>
                                  let in
                                    if RS.member(cset,c) then
                                      RS.delete(cset,c)
                                    else cset
                                  end
                            else cset
                          end)
                      (RS.addList(RS.empty,registers)) (!adjn)
            in
              selectStack := ns;
              if RS.isEmpty(availableColors) then
                spillNS := NS.add(!spillNS, n)
              else
                let val r = pickColor(availableColors) in
                  coloredNS := NS.add(!coloredNS,n);
                  colored := TT.enter(!colored,tn,r)
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
      makeWorklist();
      iter();
      (assignColors(),
       map (fn (LI.NODE{temp,...}) => temp) (NS.listItems (!spillNS)))
    end
  end
end

