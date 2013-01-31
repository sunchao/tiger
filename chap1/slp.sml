type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* the following are exercises *)

(* calculate maximum number of args in print statement *)
fun maxargs (s:stm) : int =
    case s of 
	CompoundStm (s1,s2) => Int.max (maxargs s1, maxargs s2)
      | AssignStm (_,e) => (maxargs_expr e)
      | PrintStm l => (List.length l)
and maxargs_expr (e:exp) : int = 
    case e of 
	IdExp _ => 0
      | NumExp _ => 0
      | OpExp (e1,_,e2) => Int.max (maxargs_expr e1, maxargs_expr e2)
      | EseqExp (s1,e1) => Int.max (maxargs s1, maxargs_expr e1)

(* environment for the little interpreter *)
type table = (id * int) list 

fun update (tb,id,int) = (id,int) :: tb

fun lookup (tb:table,id:id) : int =
    case tb of 
	nil => 0
      | ((id',v) :: tb') =>
	if id = id' then v
	else lookup (tb',id)

fun print_op (b:binop) : unit = 
    case b of 
	Plus => print "+"
      | Minus => print "-"
      | Times => print "*"
      | Div => print "/"

fun print_exp (e:exp) : unit =
    case e of
	IdExp id => print id
      | NumExp n => print (Int.toString n)
      | OpExp (e1,b,e2) => (print_exp e1; print_op b; print_exp e2)
      | EseqExp (_,e1) => print_exp e1
    
fun interpStm (s,tb) : table =
    case s of 
	CompoundStm (s1,s2) => interpStm (s2,(interpStm (s1,tb)))
      | AssignStm (id,e1) =>
	let 
	    val (v,tb1) = interpExp (e1,tb)
	in
	    update (tb1,id,v)
	end
      | PrintStm elist =>
	case elist of
	    nil => tb
	  | (h::t) =>
	    let 
		val (_,tb1) = (print_exp h; interpExp (h,tb))
	    in
		interpStm (PrintStm t,tb1)
	    end

and interpExp (e,tb) : int * table =
    case e of 
	IdExp id => (lookup (tb,id),tb)
      | NumExp n => (n,tb)
      | OpExp (e1,b,e2) => 
	let 
	    val (v1,tb1) = interpExp (e1,tb)
	    val (v2,tb2) = interpExp (e2,tb1)
	in
	    case b of
		Plus => ((v1+v2),tb2)
	      | Minus => ((v1-v2),tb2)
	      | Times => ((v1*v2),tb2)
	      | Div => ((v1 div v2),tb2)
	end
      | EseqExp (s,e) => interpExp (e,(interpStm (s,tb)))
	
	    
(* exercises *)

type key = string

datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert (key, LEAF) = TREE (LEAF, key, LEAF)
  | insert (key, TREE (l,k,r)) = 
    if key < k then
	TREE (insert (key,l),k,r)
    else if key > k then
	TREE (l,k, insert (key,r))
    else TREE (l,key,r)

fun member (key, LEAF) = false
  | member (key, TREE (l,k,r)) = 
    if key < k then 
	member (key,l)
    else if key > k then
	member (key,r)
    else true

(* extend the program to include not just 
	  membership, but the mapping of key to bindings *)








    
	
