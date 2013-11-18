(* $Id: types.sml,v 1.3 2012/10/14 20:25:56 csun Exp $ *)
structure Types =
struct

(* only interesting you can do is to compare address *)
type unique = unit ref

datatype ty =
         RECORD of (Symbol.symbol * ty) list * unique
       | NIL
	   | UNIT
       | INT
       | STRING
       | ARRAY of ty * unique
	   | NAME of Symbol.symbol * ty option ref (* for mutually recursive types *)
end
