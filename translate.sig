signature TRANSLATE =
sig 
  type level
  type access (* not the same as Frame.access *)
  type exp
  type frag (* fragments *)
  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
                  formals: bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access
  val simpleVar : access * level -> exp
  val subscriptVar : exp * exp -> exp
  val fieldVar : exp * Symbol.symbol * Symbol.symbol list -> exp
  val intlit : int -> exp
  val strlit : string -> exp
  val relop : Absyn.oper * exp * exp -> exp
  val binop : Absyn.oper * exp * exp -> exp
  val ifelse : exp * exp * exp option -> exp
  val record : exp list -> exp
  val array : exp * exp -> exp
  val loop : exp * exp * Temp.label -> exp
  val break : Temp.label -> exp
  val call : level * level * Temp.label * exp list * bool -> exp
  val assign : exp * exp -> exp
  val sequence : exp list -> exp
  val nilexp : exp
  val letexp : exp list * exp -> exp

  val getResult : unit -> frag list
  val reset : unit -> unit

  structure Frame : FRAME
  val procEntryExit : level * exp -> unit

  val errexp : exp


end
