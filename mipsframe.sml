structure MipsFrame: FRAME = struct

structure T = Tree
structure A = Assem
structure S = Symbol
structure TP = Temp

type register = string
type frame = {name: TP.label, formals: bool list, size: int ref}
datatype access = InFrame of int | InReg of TP.temp

datatype frag =
         PROC of {body:T.stm, frame:frame}
	     | STRING of TP.label * string

val wordSize = 4

(* expression evaluation and results of a functioin *)
val v0 = TP.newtemp()
val v1 = TP.newtemp()

(* arguments *)
val a0 = TP.newtemp()
val a1 = TP.newtemp()
val a2 = TP.newtemp()
val a3 = TP.newtemp()

(* temporary - not preserved across call *)
val t0 = TP.newtemp()
val t1 = TP.newtemp()
val t2 = TP.newtemp()
val t3 = TP.newtemp()
val t4 = TP.newtemp()
val t5 = TP.newtemp()
val t6 = TP.newtemp()
val t7 = TP.newtemp()

(* saved temporary - preserved across call *)
val s0 = TP.newtemp()
val s1 = TP.newtemp()
val s2 = TP.newtemp()
val s3 = TP.newtemp()
val s4 = TP.newtemp()
val s5 = TP.newtemp()
val s6 = TP.newtemp()
val s7 = TP.newtemp()

(* temporary - not preserved across call *)
val t8 = TP.newtemp()
val t9 = TP.newtemp()


val ZERO = TP.newtemp() (* constant 0 *)
val GP = TP.newtemp() (* pointer for global area *)
val FP = TP.newtemp() (* frame pointer *)
val SP = TP.newtemp() (* stack pointer *)
val RA = TP.newtemp() (* return address *)
val RV = TP.newtemp() (* TODO: change it to refer to an existing reg *)

val specialargs = [RV,FP,SP,RA,ZERO]

val argregs = [a0,a1,a2,a3] (* TODO: not sure it's right *)

val calleesaves = [s0,s1,s2,s3,s4,s5,s6,s7]

val callersaves = [t0,t1,t2,t3,t4,t5,t6,t7]

val tempMap = foldl
  (fn ((k,v),tb) => TP.Table.enter(tb,v,k)) TP.Table.empty  
  [("zero",ZERO),("a0",a0),("a1",a1),("a2",a2),("a3",a3),
   ("t0",t0),("t1",t1),("t2",a0),("t3",a0),
   ("t4",a0),("t5",a0),("t6",a0),("t7",a0),
   ("s0",a0),("s1",a0),("s2",a0),("s3",a0),
   ("s4",a0),("s5",a0),("s6",a0),("s7",a0),
   ("t8",a0),("t9",a0),("GP",a0),("FP",a0),
   ("v0",v0),("v1",v1),("SP",a0),("RA",a0)]
               
val registers = [] (* TODO: fill this *)

fun string (label, str) : string = 
    S.name label ^ ": .asciiz \"" ^ str ^ "\"\n"

(* make a new frame *)
fun newFrame {name: TP.label, formals: bool list} = 
    {name=name,formals=formals,size=ref 1}

fun name ({name,...}: frame) = name

(* TODO: fix this? *)
fun formals ({formals,...}: frame): access list = 
    let fun iter (nil,_) = nil
          | iter (b :: rest,offset) = 
            let val fr = if b then InFrame(offset) else InReg(TP.newtemp())
            in fr :: iter(rest,offset+wordSize) end
    in iter(formals,0)
    end

(* allocate a local variable either on frame or in register *)
fun allocLocal ({size,...}: frame) escape = 
    if (escape) then 
      let val ret = InFrame(!size*wordSize) in
        size := !size + wordSize; ret end
    else InReg(TP.newtemp()) (* TODO: fix this *)

fun exp (InFrame(k))= (fn (temp) => T.MEM(T.BINOP(T.PLUS,temp,T.CONST k)))
  | exp (InReg(temp)) = (fn (_) => T.TEMP temp)


fun externalCall (s,args) = T.CALL(T.NAME(TP.namedlabel s), args)


(* TODO: implement it *)
fun procEntryExit1 (frame,body) : T.stm = body (* for preliminary testing *)

fun procEntryExit2 (frame,body) = 
    body @ 
    [A.OPER{assem="",
            src=[ZERO,RA,SP]@calleesaves,
            dst=[],jump=SOME[]}]
    

fun procEntryExit3 ({name,formals,size},body) =
    {prolog="PROCEDURE " ^ S.name name ^ "\n",
     body=body,epilog="END " ^ S.name name ^ "\n"}

end
