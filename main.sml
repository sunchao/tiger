structure Main = struct

structure Tr = Translate
structure Frame : FRAME = MipsFrame
structure F = Frame
structure A = Assem

fun getsome (SOME x) = x

fun addtab instrs =
    map (fn (i) =>
            case i of
              l as A.LABEL _ => l
            | A.OPER{assem,src,dst,jump} =>
              A.OPER{assem="\t"^assem,src=src,dst=dst,jump=jump}
            | A.MOVE{assem,dst,src} =>
              A.MOVE{assem="\t"^assem,src=src,dst=dst}
        ) instrs

fun tempname alloc temp =
    case Temp.Table.look(alloc,temp) of
        SOME(r) => r
      | NONE => Frame.temp_name temp

fun emitproc out (F.PROC{body,frame}) =
    let val _ = print ("emit " ^ Symbol.name (Frame.name frame) ^ "\n")
	      val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	      val instrs = List.concat(map (MipsGen.codegen frame) stms')
        val instrs2 = Frame.procEntryExit2 (frame,instrs)
        val format1 = Assem.format(Frame.temp_name)
        val (instrs2',alloc) = RegAlloc.alloc(instrs2,frame)
        val {prolog,body,epilog} = Frame.procEntryExit3(frame,instrs2')
        val instrs'' = addtab body
        val format0 = Assem.format(tempname alloc)
    in
      TextIO.output(out,prolog);
      app (fn i => TextIO.output(out,(format0 i) ^ "\n")) instrs'';
      TextIO.output(out,epilog)
    end

  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

fun emitstr out (F.STRING(lab,str)) = TextIO.output(out,Frame.string(lab,str))

fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
	     handle e => (TextIO.closeOut out; raise e)
    end

fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn;
                     Semant.transProg absyn)
        val (progs,strs) =
            List.partition
                (fn (x) => case x of
                               F.PROC(_) => true
                             | _ => false) frags
    in
      withOpenFile (filename ^ ".s")
	                 (fn out =>
                       let in
                         TextIO.output(out,"\t.globl main\n");
                         TextIO.output(out,"\t.data\n");
                         app (emitstr out) strs;
                         TextIO.output(out,"\n\t.text\n");
                         app (emitproc out) progs
                       end)
    end

fun main (cmd: string, args: string list) =
    let in app compile args; 0 end
end
