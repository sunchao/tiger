structure Translate : TRANSLATE = 
struct

structure Frame : FRAME = MipsFrame

datatype level = Top 
               | Lev of {parent: level, frame: Frame.frame}

type exp = unit 
type access = level * Frame.access

val outermost = Top

fun newLevel {parent,name,formals} = 
    Lev{parent=parent,frame=Frame.newFrame{name=name,formals=true::formals}}
    

fun formals lev =
    case lev of
      Top => []
    | Lev{parent=parent,frame=frame} => 
      (map (fn (x) => (lev,x)) (Frame.formals frame))

fun allocLocal lev escape = 
    case lev of 
      Lev{parent=_,frame=frame} => (lev,Frame.allocLocal frame escape)

end
