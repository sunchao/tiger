(* The book recommends associating each node with
a membership flag. WE defined it here, but leave it for future improvement *)
 

structure Color : COLOR = 
struct 

structure NS = ListSetFn(
  type ord_key = Graph.node
  fun compare (n1,n2) = Graph.compare(n1,n2))

structure MS = BinarySetFn(
  type ord_key = Graph.node*Graph.node
  fun compare ((n1,n2), (n1',n2')) =
      case Graph.compare(n1,n2) of
          EQUAL => Graph.compare(n1',n2')
        | od => od)

structure Frame = MipsFrame
structure WL = NS
structure GT = Graph.Table
structure T = Temp

exception NotEnoughRegister
type allocation = Frame.register T.Table.table

(* the main function *)
fun color {interference = Liveness.IGRAPH{graph,tnode,gtemp,moves},
           initial=initAlloc, spillCost, registers} = 
    let
      val K = List.length registers

      val (precolorL, initialL) = 
          let fun p node = 
                  case T.Table.look (initAlloc,gtemp node) of
                      SOME _ => true
                    | NONE => false
          in List.partition p (Graph.nodes graph) end

      (* a mapping from a node to the list of moves it is associated with *)
      val moveList = 
          List.foldl 
            (fn (m,t) => 
                let fun add (t,n) = 
                        case GT.look (t,n) of
                            SOME rest => GT.enter(t,n,m::rest)
                          | NONE => GT.enter (t,n,[m])
                in add(add(t,#1 m), #2 m) end) 
            GT.empty moves
                       
      val initial = NS.addList(NS.empty,initialL)
      val precolored = NS.addList(NS.empty,precolorL)

      val alias = ref GT.empty

      val adjSet = 
          ref (List.foldl
                 (fn (x,s) =>
                     MS.addList(s,map (fn (y) => (x,y)) (Graph.adj x)))
                 MS.empty (Graph.nodes graph))

      val adjList =
          ref (NS.foldl
                 (fn (u,tb) => 
                     GT.enter(tb,u,NS.addList(NS.empty,Graph.adj u)))
              (GT.empty) initial)

      val simplifyWL = ref WL.empty
      val freezeWL = ref WL.empty
      val spillWL = ref WL.empty
      val coalescedMS = ref MS.empty
      val constrainedMS = ref MS.empty
      val frozenMS = ref MS.empty
      val worklistMS = ref MS.empty
      val activeMS = ref MS.empty

      val selectStack = ref nil
      val coalescedNS = ref NS.empty
      val coloredNS = ref NS.empty

      val degrees = 
          ref (List.foldl 
                 (fn (x,tb) => GT.enter(tb,x,List.length (Graph.adj x)))
                 GT.empty (Graph.nodes graph))


      val nodeMoves =
          ref (List.foldl
                 (fn (n,tb) =>
                     case GT.look(moveList,n) of
                         NONE => GT.enter(tb,n,MS.empty)
                       | SOME ms => 
                         GT.enter(tb,n,
                                  MS.intersection
                                    (MS.addList(MS.empty,ms),
                                     (MS.union(!activeMS,!worklistMS)))))
                 GT.empty (Graph.nodes graph))
                                 

      fun addEdge (u,v) =
          if (not (MS.member(!adjSet,(u,v))) andalso
              (not (Graph.eq(u,v)))) then
            let in
              adjSet := MS.add(!adjSet,(u,v));
              adjSet := MS.add(!adjSet,(v,u));
              if not (NS.member(precolored,u)) then
                let val SOME(old) = GT.look(!adjList,u)
                    val SOME(d) = GT.look(!degrees,u) in
                  adjList := GT.enter(!adjList,u,NS.add(old,v));
                  degrees := GT.enter(!degrees,u,d+1)
                end
              else ();
              if not (NS.member(precolored,v)) then
                let val SOME(old) = GT.look(!adjList,v)
                    val SOME(d) = GT.look(!degrees,v) in
                  adjList := GT.enter(!adjList,v,NS.add(old,u));
                  degrees := GT.enter(!degrees,v,d+1)
                end
              else ()
            end
          else ()

      fun nodeMove (n) = let val SOME(m) = GT.look(!nodeMoves,n) in m end

      fun degree n = let val SOME(d) = GT.look(!degrees,n) in d end

      fun adjacent (n) = 
          NS.difference
            (NS.addList(NS.empty,Graph.adj n),
             (NS.union(NS.addList(NS.empty,!selectStack),!coalescedNS)))

      fun moveRelated (n) = MS.isEmpty (nodeMove(n))

      fun makeWorkList () = 
          NS.app
            (fn (n) =>
                if (degree n) >= K then
                  spillWL := NS.add(!spillWL,n)
                else if moveRelated(n) then
                  freezeWL := NS.add(!freezeWL,n)
                else
                  simplifyWL := NS.add(!simplifyWL,n)
            )
            initial 

      fun enableMoves (nodes) = 
          NS.app
            (fn (n) =>
                MS.app
                  (fn (m) =>
                      if (MS.member(!activeMS,m)) then
                        (activeMS := MS.delete(!activeMS,m);
                         worklistMS := MS.add(!worklistMS,m))
                      else ())
                  (nodeMove(n))
            ) nodes

      fun decrementDegree (n) =
          let val d = degree n in
            degrees := GT.enter(!degrees,n,d-1);
            if (d = K) then
              let in
                enableMoves(NS.union(NS.singleton(n),adjacent(n)));
                spillWL := NS.delete(!spillWL,n);
                if moveRelated(n) then
                  freezeWL := NS.add(!freezeWL,n)
                else 
                  simplifyWL := NS.add(!simplifyWL,n)
              end
            else ()
          end

      fun simplify () =
          NS.app
            (fn (n) => 
                let in
                  simplifyWL := NS.delete(!simplifyWL,n);
                  selectStack := n :: !selectStack;
                  NS.app decrementDegree (adjacent(n))
                end)
            (!simplifyWL)

      fun addWorkList (u) =
          if (not (NS.member(precolored,u)) andalso 
              not (moveRelated(u)) andalso (degree u) < K) then
            (freezeWL := NS.delete(!freezeWL,u);
             simplifyWL := NS.add(!simplifyWL,u))
          else ()

      fun OK (t,r) = 
          (degree t) < K orelse NS.member(precolored,t) 
          orelse MS.member(!adjSet,(t,r))
                          
      fun conservative (nodes) : bool = 
          let val k = ref 0 in
            NS.app (fn (n) => if (degree n) > K then k := !k + 1 else ()) 
                   nodes;
            !k < K
          end
              
      fun getAlias (n) = 
          if NS.member(!coalescedNS,n) then
            let val SOME(m) = GT.look(!alias,n) in getAlias(m) end
          else n                   
                   
      fun combine (u,v) =
          let in
            if NS.member(!freezeWL,v) then
              freezeWL := NS.delete(!freezeWL,v)
            else
              spillWL := NS.delete(!spillWL,v);
            coalescedNS := NS.add(!coalescedNS,v);
            alias := GT.enter(!alias,v,u);
            nodeMoves := GT.enter(!nodeMoves,u,
                                  MS.union(nodeMove(u),nodeMove(v)));
            NS.app (fn (t) => (addEdge(t,u);decrementDegree(t))) (adjacent(v));
            if (degree u) >= K andalso NS.member(!freezeWL,u) then
              (freezeWL := NS.delete(!freezeWL,u);
               spillWL := NS.add(!spillWL,u))
            else ()
          end
            
      fun coalesce () = 
          MS.app
            (fn (m) => 
                let 
                  val (x,y) = m
                  val x = getAlias(x)
                  val y = getAlias(y) 
                  val (u,v) = 
                      if NS.member(precolored,y) then (y,x)
                      else (x,y)
                in
                  worklistMS := MS.delete(!worklistMS,m);
                  if Graph.eq(u,v) then
                    (coalescedMS := MS.add(!coalescedMS,m);
                     addWorkList(u))
                  else
                    if (NS.member(precolored,v) 
                        orelse MS.member(!adjSet,(u,v))) then
                      (constrainedMS := MS.add(!constrainedMS,m);
                       addWorkList(u); addWorkList(v))
                    else if 
                      ((NS.member(precolored,u) andalso
                        (List.all (fn (t) => OK(t,u))
                                  (NS.listItems((adjacent(u)))))) orelse
                       (not (NS.member(precolored,u)) andalso
                        conservative(NS.union(adjacent(u),adjacent(v))))) 
                    then 
                      let in
                        coalescedMS := MS.add(!coalescedMS,m);
                        combine(u,v);
                        addWorkList(u)
                      end
                    else activeMS := MS.add(!activeMS,m)
                end)
            (!worklistMS)

    in (Temp.Table.empty, nil)
    end

end
