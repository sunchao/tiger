structure MipsFrame: FRAME = struct

structure T = Tree
structure A = Assem

type register = string
type frame = {name: Temp.label, formals: bool list, size: int ref}
datatype access = InFrame of int | InReg of Temp.temp

datatype frag =
         PROC of {body:T.stm, frame:frame}
	     | STRING of Temp.label * string

val wordSize = 4

(* expression evaluation and results of a functioin *)
val v0 = Temp.newtemp()
val v1 = Temp.newtemp()

(* arguments *)
val a0 = Temp.newtemp()
val a1 = Temp.newtemp()
val a2 = Temp.newtemp()
val a3 = Temp.newtemp()

(* temporary - not preserved across call *)
val t0 = Temp.newtemp()
val t1 = Temp.newtemp()
val t2 = Temp.newtemp()
val t3 = Temp.newtemp()
val t4 = Temp.newtemp()
val t5 = Temp.newtemp()
val t6 = Temp.newtemp()
val t7 = Temp.newtemp()

(* saved temporary - preserved across call *)
val s0 = Temp.newtemp()
val s1 = Temp.newtemp()
val s2 = Temp.newtemp()
val s3 = Temp.newtemp()
val s4 = Temp.newtemp()
val s5 = Temp.newtemp()
val s6 = Temp.newtemp()
val s7 = Temp.newtemp()

(* temporary - not preserved across call *)
val t8 = Temp.newtemp()
val t9 = Temp.newtemp()


val ZERO = Temp.newtemp() (* constant 0 *)
val GP = Temp.newtemp() (* pointer for global area *)
val FP = Temp.newtemp() (* frame pointer *)
val SP = Temp.newtemp() (* stack pointer *)
val RA = Temp.newtemp() (* return address *)
val RV = Temp.newtemp() (* TODO: change it to refer to an existing reg *)

val specialargs = [RV,FP,SP,RA,ZERO]

val argregs = [a0,a1,a2,a3] (* TODO: not sure it's right *)

val calleesaves = [s0,s1,s2,s3,s4,s5,s6,s7]

val callersaves = [t0,t1,t2,t3,t4,t5,t6,t7]

val tempMap = foldl
  (fn ((k,v),tb) => Temp.Table.enter(tb,v,k)) Temp.Table.empty  
  [("zero",ZERO),("a0",a0),("a1",a0),("a2",a0),("a3",a0),
   ("t0",a0),("t1",a0),("t2",a0),("t3",a0),
   ("t4",a0),("t5",a0),("t6",a0),("t7",a0),
   ("s0",a0),("s1",a0),("s2",a0),("s3",a0),
   ("s4",a0),("s5",a0),("s6",a0),("s7",a0),
   ("t8",a0),("t9",a0),("GP",a0),("FP",a0),
   ("v0",v0),("v1",v1),("SP",a0),("RA",a0)]
               
val registers = [] (* TODO: fill this *)

fun string (label, str) : string = 
    Symbol.name label ^ ": .asciiz \"" ^ str ^ "\"\n"

(* make a new frame *)
fun newFrame {name: Temp.label, formals: bool list} = 
    {name=name,formals=formals,size=ref 1}

fun name ({name,...}: frame) = name

(* TODO: fix this? *)
fun formals ({formals,...}: frame): access list = 
    let fun iter (nil,_) = nil
          | iter (b :: rest,offset) = 
            let val fr = if b then InFrame(offset) else InReg(Temp.newtemp())
            in fr :: iter(rest,offset+wordSize) end
    in iter(formals,0)
    end

(* allocate a local variable either on frame or in register *)
fun allocLocal ({size,...}: frame) escape = 
    if (escape) then 
      let val ret = InFrame(!size*wordSize) in
        size := !size + wordSize; ret end
    else InReg(Temp.newtemp()) (* TODO: fix this *)

fun exp (InFrame(k))= (fn (temp) => T.MEM(T.BINOP(T.PLUS,temp,T.CONST k)))
  | exp (InReg(temp)) = (fn (_) => T.TEMP temp)


fun externalCall (s,args) = T.CALL(T.NAME(Temp.namedlabel s), args)


(* TODO: implement it *)
fun procEntryExit1 (frame,body) : T.stm = body (* for preliminary testing *)

fun procEntryExit2 (frame,body) = 
    body @ 
    [A.OPER{assem="",
            src=[ZERO,RA,SP]@calleesaves,
            dst=[],jump=SOME[]}]
    

fun procEntryExit3 ({name,formals,size},body) =
    {prolog="PROCEDURE " ^ Symbol.name name ^ "\n",
     body=body,epilog="END " ^ Symbol.name name ^ "\n"}

end
