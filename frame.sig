signature FRAME =
sig
  type frame
  type access

  (* don't know why on the book the type is different *)
  val newFrame : {name: Temp.label,
                  formals: bool list} -> frame
  val name: frame -> Temp.label
  val formals: frame -> access list
  val allocLocal : frame -> bool -> access
  val string : Tree.label * string -> string

  val FP: Temp.temp
  val SP: Temp.temp
  val RV: Temp.temp (* for return value *)
  val RA: Temp.temp
  val wordSize: int
  val exp: access -> Tree.exp -> Tree.exp

  type register = string


  val registers : register list

  val argregs : Temp.temp list
  val callersaves : Temp.temp list
  val calleesaves : Temp.temp list

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list ->
                       {prolog:string,body:Assem.instr list,epilog:string}

  val tempMap : register Temp.Table.table
  val temp_name : Temp.temp -> string

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val externalCall : string * Tree.exp list -> Tree.exp

end
