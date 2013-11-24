structure MipsFrame: FRAME = struct

structure T = Tree
structure A = Assem
structure S = Symbol
structure TP = Temp

type register = string
datatype access = InFrame of int | InReg of TP.temp
type frame = { name: TP.label, formals: access list,
               locals: int ref, instrs: T.stm list }
datatype frag =
         PROC of {body:T.stm, frame:frame}
	     | STRING of TP.label * string

exception TooManyArgs of string

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
val RV = TP.newtemp()

val specialargs = [RV,FP,SP,RA]

val argregs = [a0,a1,a2,a3]

val calleesaves = [s0,s1,s2,s3,s4,s5,s6,s7]

val callersaves = [t0,t1,t2,t3,t4,t5,t6,t7]

val reglist =
    [("$a0",a0),("$a1",a1),("$a2",a2),("$a3",a3),
     ("$t0",t0),("$t1",t1),("$t2",t2),("$t3",t3),
     ("$t4",t4),("$t5",t5),("$t6",t6),("$t7",t7),
     ("$s0",s0),("$s1",s1),("$s2",s2),("$s3",s3),
     ("$s4",s4),("$s5",s5),("$s6",s6),("$s7",s7),
     ("$fp",FP),("$v0",RV),("$sp",SP),("$ra",RA)]


val tempMap = foldl
  (fn ((k,v),tb) => TP.Table.enter(tb,v,k)) TP.Table.empty reglist

fun temp_name t =
    case TP.Table.look(tempMap,t) of
	      SOME(r) => r
      | NONE => Temp.makestring t

(* a list of all register name, which can be used for coloring *)
val registers = map (fn (x) => case TP.Table.look(tempMap,x) of
                        SOME(r) => r) (argregs @ calleesaves @ callersaves @ specialargs)

fun string (label, str) : string =
    S.name label ^ ": .asciiz \"" ^ str ^ "\"\n"

fun exp (InFrame(k))= (fn (temp) => T.MEM(T.BINOP(T.PLUS,temp,T.CONST k)))
  | exp (InReg(temp)) = (fn (_) => T.TEMP temp)

(* make a new frame object, which also includes "view shift" instructions.
 * Note: right now we only pass parameters through register a0-a3. For
 * functions with more than 4 paramters, we just give up.. *)
fun newFrame {name: TP.label, formals: bool list} =
    let
      val n = List.length formals
      fun iter(nil,_) = nil
        | iter(b::rest,offset) =
          if b then InFrame(offset)::iter(rest,offset+wordSize)
          else InReg(TP.newtemp())::iter(rest,offset)
      val accs : access list = iter(formals,wordSize) (* save old FP at offset 0 *)
      fun view_shift(acc,r) = T.MOVE(exp acc (T.TEMP FP), T.TEMP r)
      val shift_instrs = ListPair.map view_shift (accs,argregs)
    in if n <= List.length argregs then
         {name=name,formals=accs,locals=ref 0,instrs=shift_instrs}
       else raise TooManyArgs("Too many arguments: " ^ Int.toString(~n))
    end

fun name ({name,...}: frame) = name

fun formals ({formals,...}: frame): access list = formals

(* allocate a local variable either on frame or in register *)
fun allocLocal ({locals,...}: frame) escape =
    if (escape) then
      let val ret = InFrame((!locals+1)*(~wordSize)) in
        locals := !locals + 1; ret end
    else InReg(TP.newtemp())


fun externalCall (s,args) = T.CALL(T.NAME(TP.namedlabel s), args)

fun seq nil = T.EXP(T.CONST 0)
  | seq [st] = st
  | seq (st :: rest) = T.SEQ(st,seq(rest))

(* for each incoming register parameter, move it to the place
 * from which it is seem from within the function. This could be
 * a frame location (for escaping parameters) or a fresh temporary.*)
fun procEntryExit1 (frame,body) : T.stm =
  let
    val args = #instrs frame
    val pairs =  map (fn r => (allocLocal frame false,r)) (RA::calleesaves)
    val saves = map (fn (a,r) => T.MOVE(exp a (T.TEMP FP),T.TEMP r)) pairs
    val restores = map (fn (a,r) => T.MOVE(T.TEMP r,exp a (T.TEMP FP)))
                       (List.rev pairs)
  in seq(args @saves @ [body] @ restores) end

fun procEntryExit2 (frame,body) =
    body @
    [A.OPER{assem="",
            src=[ZERO,RA,SP]@calleesaves,
            dst=[],jump=SOME[]}]

fun procEntryExit3 ({name,formals,locals,instrs},body) =
    let val offset = (!locals + (List.length argregs))*wordSize in
      {prolog=S.name name ^ ":\n" ^
              "\tsw\t$fp\t0($sp)\n" ^ (* save old FP *)
              "\tmove\t$fp\t$sp\n" ^ (* make SP to be new FP *)
              "\taddiu\t$sp\t$sp\t-" ^ Int.toString(offset) ^ "\n" (* make new SP *),
       body=body,
       epilog="\tmove\t$sp\t$fp\n" ^ (* restore old SP *)
              "\tlw\t$fp\t0($sp)\n" ^ (* restore old FP *)
              "\tjr\t$ra\n\n" (* jump to return address *)
      }
    end
end
