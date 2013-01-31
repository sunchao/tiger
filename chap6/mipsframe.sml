structure MipsFrame: FRAME = struct

structure T = Temp

type frame = {name: T.label, formals: bool list, count: int}
datatype access = InFrame of int | InReg of T.temp

val word_size = 4

fun newFrame {name: T.label, formals: bool list} = 
    {name=name,formals=formals,count=0}

fun name ({name,...}: frame) = name

(* for now, assume every formal is on stack *)
fun formals ({formals,...}: frame): access list = 
    let 
      fun f ([],_) = []
        | f (_ :: rest,offset) = 
          InFrame(offset) :: f(rest,offset+word_size)
    in f(formals,0)
    end

fun allocLocal ({count,...}: frame) escape = 
    if (escape) then InFrame(count*word_size) else InReg(T.newtemp())

end
