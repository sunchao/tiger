structure RegAlloc : REG_ALLOC =
struct
structure A = Assem
structure Frame = MipsFrame
structure T = Temp
structure Tr = Tree
type allocation = Frame.register T.Table.table

fun rewrite (instrs:A.instr list, frame, spills) : A.instr list =
    let
      (* rewrite one temp *)
      fun rewrite1 (instrs:A.instr list, t:T.temp) =
          let
            val ae = Frame.exp (Frame.allocLocal(frame) true) (Tr.TEMP Frame.FP)

            (* generate fetch or store instruction *)
            fun gen_instrs (is_def:bool, t:T.temp) =
                if is_def then MipsGen.codegen(frame)(Tr.MOVE(ae,Tr.TEMP t))
                else MipsGen.codegen(frame)(Tr.MOVE(Tr.TEMP t,ae))

            (* allocate new temp for each occurence of t in dus, replace
             * the occurence with the new temp. *)
            fun alloc_du (is_def:bool, dus:T.temp list, t) =
                if List.exists (fn (t') => t = t') dus then
                  let val nt = T.newtemp () in
                    (gen_instrs(is_def,nt),
                     map (fn (t') => if t = t' then nt else t') dus)
                  end
                else ([],dus)

            (* transform one instruction for one spilled temp *)
            fun trans_instr instr =
                case instr of
                    A.OPER{assem,dst,src,jump} =>
                    let val (store,dst') = alloc_du(true,dst,t)
                        val (fetch,src') = alloc_du(false,src,t)
                    in (fetch@[A.OPER{assem=assem,dst=dst',
                                      src=src',jump=jump}]@store)
                    end
                  | A.MOVE{assem,dst,src} =>
                    let val (store,[dst']) = alloc_du(true,[dst],t)
                        val (fetch,[src']) = alloc_du(false,[src],t)
                    in (fetch@[A.MOVE{assem=assem,dst=dst',src=src'}]@store)
                    end
                  | instr => [instr]
          in
            List.foldl (fn (i,acc) => acc @ trans_instr i) nil instrs
          end

      val format0 = A.format(Temp.makestring)
    in
      List.foldl (fn (t,ins) => rewrite1(ins,t)) instrs spills
    end

fun alloc (instrs,frame) : A.instr list * allocation =
    let
      fun tempname alloc temp =
          case Temp.Table.look(alloc,temp) of
              SOME(r) => r
            | NONE => Frame.temp_name temp

      val (graph,nodes) = MakeGraph.instrs2graph instrs
      val (igraph,liveout) = Liveness.interferenceGraph graph

      fun spillCost node =
          let
              val Flow.FGRAPH{control,def,use,ismove} = graph
              val Liveness.IGRAPH{graph,tnode,gtemp,moves} = igraph
              val temp = gtemp node
              fun look (table,key) = valOf(Graph.Table.look(table,key))
              fun f (s,t) = if List.exists (fn x => x = t) s then 1 else 0
              val num_du =
                  foldl (fn (n,acc) =>
                            acc + f(look(def,n),temp) +
                            f(look(use,n),temp))
                        0 nodes
              val interferes = length(Graph.adj node)
          in
              (Real.fromInt(num_du) / Real.fromInt(interferes))
          end

      val (alloc_table,spills) = SimpleColor.color{interference=igraph,
					                                         initial=Frame.tempMap,
					                                         spillCost=spillCost,
                                                   registers=Frame.registers}
      val format0 = A.format(tempname alloc_table)
      val format1 = A.format(Temp.makestring)
    in
      if List.length spills = 0 then (instrs,alloc_table)
      else alloc(rewrite(instrs,frame,spills),frame)
    end
end
