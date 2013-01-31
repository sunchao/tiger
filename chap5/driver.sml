structure Main =
struct

fun compile filename = 
    let val ast = Parse.parse filename in
        print ("filename: " ^ filename ^ "\n");
        print ("ast: \n");
        PrintAbsyn.print(TextIO.stdOut, ast);
        print "output: \n";
        Semant.transProg(ast)
        handle _ => ErrorMsg.error 1 "type checked failed\n"
    end

fun main(cmd: string, args: string list): OS.Process.status = 
    let in app compile args; 0
    end
end
                   
