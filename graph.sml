structure Graph :> GRAPH =
struct
  type node' = int
  type temp = Temp.temp

  datatype noderep = NODE of {succ: node' list, pred: node' list}

  val emptyNode = NODE{succ=[],pred=[]}

  val bogusNode = NODE{succ=[~1],pred=[]}

  fun isBogus(NODE{succ= ~1::_,...}) = true
    | isBogus _ = false

  structure A = DynamicArrayFn(struct open Array
				    type elem = noderep
				    type vector = noderep vector
				    type array = noderep array
                             end)

  type graph = A.array

  type node = graph * node'
  fun eq((_,a),(_,b)) = a=b

  fun augment (g: graph) (n: node') : node = (g,n)

  fun newGraph() = A.array(0,bogusNode)

  fun nodes g = let val b = A.bound g
                    fun f i = if isBogus( A.sub(g,i)) then nil
			           else (g,i)::f(i+1)
		 in f 0			     
                end

  fun succ(g,i) = let val NODE{succ=s,...} = A.sub(g,i) 
		   in map (augment g) s 
		  end
  fun pred(g,i) = let val NODE{pred=p,...} = A.sub(g,i)
                     in map (augment g) p 
		  end
  fun adj gi = pred gi @ succ gi

  fun newNode g = (* binary search for unused node *)
    let fun look(lo,hi) =
               (* i < lo indicates i in use
                  i >= hi indicates i not in use *)
            if lo=hi then (A.update(g,lo,emptyNode); (g,lo))
            else let val m = (lo+hi) div 2
                  in if isBogus(A.sub(g,m)) then look(lo,m) else look(m+1,hi)
                 end
     in look(0, 1 + A.bound g)
    end

  exception GraphEdge
  fun check(g,g') = (* if g=g' then () else raise GraphEdge *) ()

  fun delete(i,j::rest) = if i=j then rest else j::delete(i,rest)
    | delete(_,nil) = raise GraphEdge

  fun diddle_edge change {from=(g:graph, i),to=(g':graph, j)} = 
      let val _ = check(g,g')
          val NODE{succ=si,pred=pi} = A.sub(g,i)
          val _ = A.update(g,i,NODE{succ=change(j,si),pred=pi})
          val NODE{succ=sj,pred=pj} = A.sub(g,j)
          val _ = A.update(g,j,NODE{succ=sj,pred=change(i,pj)})
       in ()
      end

  val mk_edge = diddle_edge (op ::)
  val rm_edge = diddle_edge delete

  structure Table = IntMapTable(type key = node
				fun getInt(g,n) = n)


  fun nodename(g,i:int) = "n" ^ Int.toString(i)

end

