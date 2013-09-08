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
        val _ = print "tree before canon:\n";
        val _ = Printtree.printtree(TextIO.stdOut,body);
	      val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val _ = print "tree after canon:\n"
        val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms';
	      val instrs = List.concat(map (MipsGen.codegen frame) stms')
        val (instrs',alloc) = RegAlloc.alloc(instrs,frame)
        val {prolog,body,epilog} = Frame.procEntryExit3(frame,instrs')
        val instrs'' = addtab body
        val format0 = Assem.format(tempname alloc)
    in  
      TextIO.output(out,prolog);
      app (fn i => TextIO.output(out,(format0 i) ^ "\n")) instrs'';
      TextIO.output(out,epilog)
    end

  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

fun withOpenFile fname f = 
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out) 
	     handle e => (TextIO.closeOut out; raise e)
    end 
    
fun compile filename = 
    let val absyn = Parse.parse filename
        val _ = PrintAbsyn.print(TextIO.stdOut,absyn)
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in 
      withOpenFile (filename ^ ".s") 
	                 (fn out => (app (emitproc out) frags))
    end
    
end



