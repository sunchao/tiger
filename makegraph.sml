signature MAKE_GRAPH = 
sig 
  val instrs2graph: Assem.instr list ->
                    Flow.flowgraph * Graph.node list
end

structure MakeGraph : MAKE_GRAPH = 
struct

fun instrs2graph instrs =
    let
      val graph = Graph.newGraph()
      val def = Graph.Table.empty
      val use = Graph.Table.empty
      val ismove = Graph.Table.empty

      fun make_node(instr: Assem.instr) = 
          let val node = Graph.newNode graph 
              val (a,b,c) = 
                  case instr of 
                    Assem.OPER{assem,dst,src,jump} => (dst,src,false)
                  | Assem.LABEL{assem,lab} => (nil,nil,false)
                  | Assem.MOVE{assem,dst,src} => ([dst],[src],true)
          in
            Graph.Table.enter(def,node,a);
            Graph.Table.enter(use,node,b);
            Graph.Table.enter(ismove,node,c);
            node
          end

      val nodelist = map make_node instrs
      val complist = ListPair.zip (instrs, nodelist)

      fun do_jump(instr,node) = 
          let fun f l =
                  case List.find
                           (fn (i,n) => 
                               case i of 
                                 Assem.LABEL{lab,...} => l = lab
                               | _ => false
                           ) complist of
                    SOME((_,n)) => Graph.mk_edge{from=node,to=n}
          in case instr of
               Assem.OPER{jump=SOME(jlist),...} => map f jlist
          end
          
      fun connect nil = ()
        | connect [x] = ()
        | connect (x::y::rest) = 
          (Graph.mk_edge{from=x,to=y}; connect (y::rest))
          
    in
      map do_jump complist;
      connect nodelist; 
      (Flow.FGRAPH{control=graph,def=def,use=use,ismove=ismove},
       nodelist)
    end
end
