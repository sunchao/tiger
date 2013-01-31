functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* $Log: tiger.grm,v $
 * Revision 1.4  2012/02/15 22:09:02  csun
 * added log
 * *)

structure A = Absyn

fun sym s = Symbol.symbol s

fun makeBinExp(left,oper,right,pos) = 
    A.OpExp{left = left,
	    oper = oper,
	    right = right,
	    pos = pos}

type tydec = {name: A.symbol, ty: A.ty, pos: A.pos}


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\207\000\005\000\207\000\007\000\207\000\009\000\207\000\
\\011\000\207\000\013\000\207\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\207\000\026\000\207\000\
\\030\000\207\000\031\000\207\000\034\000\207\000\035\000\207\000\
\\037\000\207\000\038\000\207\000\042\000\207\000\043\000\207\000\
\\044\000\207\000\000\000\
\\001\000\001\000\208\000\005\000\208\000\007\000\208\000\009\000\208\000\
\\011\000\208\000\013\000\208\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\208\000\026\000\208\000\
\\030\000\208\000\031\000\208\000\034\000\208\000\035\000\208\000\
\\037\000\208\000\038\000\208\000\042\000\208\000\043\000\208\000\
\\044\000\208\000\000\000\
\\001\000\001\000\209\000\005\000\209\000\007\000\209\000\009\000\209\000\
\\011\000\209\000\013\000\209\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\209\000\026\000\209\000\
\\030\000\209\000\031\000\209\000\034\000\209\000\035\000\209\000\
\\037\000\209\000\038\000\209\000\042\000\209\000\043\000\209\000\
\\044\000\209\000\000\000\
\\001\000\001\000\210\000\005\000\210\000\007\000\210\000\009\000\210\000\
\\011\000\210\000\013\000\210\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\210\000\026\000\210\000\
\\030\000\210\000\031\000\210\000\034\000\210\000\035\000\210\000\
\\037\000\210\000\038\000\210\000\042\000\210\000\043\000\210\000\
\\044\000\210\000\000\000\
\\001\000\001\000\211\000\005\000\211\000\007\000\211\000\009\000\211\000\
\\011\000\211\000\013\000\211\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\211\000\026\000\211\000\
\\030\000\211\000\031\000\211\000\034\000\211\000\035\000\211\000\
\\037\000\211\000\038\000\211\000\042\000\211\000\043\000\211\000\
\\044\000\211\000\000\000\
\\001\000\001\000\212\000\005\000\212\000\007\000\212\000\009\000\212\000\
\\011\000\212\000\013\000\212\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\018\000\028\000\025\000\212\000\026\000\212\000\
\\030\000\212\000\031\000\212\000\034\000\212\000\035\000\212\000\
\\037\000\212\000\038\000\212\000\042\000\212\000\043\000\212\000\
\\044\000\212\000\000\000\
\\001\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\001\000\002\000\041\000\000\000\
\\001\000\002\000\051\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\081\000\000\000\
\\001\000\002\000\107\000\012\000\106\000\028\000\105\000\000\000\
\\001\000\002\000\109\000\000\000\
\\001\000\002\000\129\000\000\000\
\\001\000\002\000\134\000\000\000\
\\001\000\002\000\136\000\000\000\
\\001\000\002\000\138\000\000\000\
\\001\000\002\000\144\000\000\000\
\\001\000\002\000\149\000\000\000\
\\001\000\006\000\091\000\027\000\090\000\000\000\
\\001\000\006\000\122\000\000\000\
\\001\000\006\000\133\000\019\000\132\000\000\000\
\\001\000\006\000\147\000\000\000\
\\001\000\008\000\092\000\000\000\
\\001\000\009\000\078\000\000\000\
\\001\000\009\000\100\000\000\000\
\\001\000\009\000\121\000\000\000\
\\001\000\011\000\087\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\001\000\011\000\099\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\001\000\013\000\097\000\000\000\
\\001\000\013\000\130\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\030\000\077\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\034\000\112\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\035\000\076\000\000\000\
\\001\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\035\000\135\000\000\000\
\\001\000\019\000\089\000\000\000\
\\001\000\019\000\098\000\000\000\
\\001\000\019\000\142\000\000\000\
\\001\000\019\000\143\000\000\000\
\\001\000\027\000\075\000\000\000\
\\001\000\027\000\120\000\000\000\
\\001\000\037\000\071\000\000\000\
\\001\000\038\000\103\000\000\000\
\\001\000\039\000\118\000\000\000\
\\152\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\153\000\042\000\040\000\043\000\039\000\044\000\038\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\044\000\038\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\002\000\111\000\000\000\
\\164\000\000\000\
\\165\000\005\000\140\000\000\000\
\\166\000\000\000\
\\167\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\168\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\169\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\170\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\171\000\042\000\040\000\000\000\
\\172\000\000\000\
\\173\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\174\000\000\000\
\\175\000\005\000\102\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\176\000\000\000\
\\177\000\002\000\083\000\000\000\
\\178\000\000\000\
\\179\000\005\000\126\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\180\000\000\000\
\\181\000\008\000\050\000\010\000\049\000\012\000\048\000\014\000\047\000\000\000\
\\182\000\010\000\018\000\014\000\017\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\039\000\116\000\000\000\
\\186\000\000\000\
\\187\000\027\000\019\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\196\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\197\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\198\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\
\\031\000\113\000\000\000\
\\199\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\200\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\025\000\021\000\026\000\020\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\017\000\029\000\018\000\028\000\000\000\
\\204\000\017\000\029\000\018\000\028\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\213\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\000\000\
\\214\000\015\000\031\000\016\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\020\000\026\000\021\000\025\000\022\000\024\000\
\\023\000\023\000\024\000\022\000\000\000\
\\215\000\002\000\016\000\003\000\015\000\004\000\014\000\008\000\013\000\
\\016\000\012\000\029\000\011\000\032\000\010\000\033\000\009\000\
\\036\000\008\000\040\000\007\000\041\000\006\000\000\000\
\\216\000\000\000\
\\217\000\007\000\080\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\019\000\027\000\020\000\026\000\021\000\025\000\
\\022\000\024\000\023\000\023\000\024\000\022\000\025\000\021\000\
\\026\000\020\000\000\000\
\\218\000\000\000\
\"
val actionRowNumbers =
"\007\000\077\000\082\000\047\000\
\\083\000\096\000\048\000\008\000\
\\007\000\007\000\007\000\104\000\
\\085\000\084\000\076\000\009\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\007\000\007\000\
\\007\000\007\000\048\000\066\000\
\\048\000\048\000\052\000\044\000\
\\010\000\011\000\012\000\042\000\
\\036\000\034\000\086\000\027\000\
\\106\000\013\000\072\000\007\000\
\\068\000\079\000\030\000\091\000\
\\103\000\102\000\005\000\003\000\
\\006\000\004\000\002\000\001\000\
\\100\000\101\000\099\000\098\000\
\\051\000\067\000\050\000\049\000\
\\053\000\104\000\038\000\022\000\
\\026\000\007\000\007\000\007\000\
\\087\000\105\000\007\000\078\000\
\\032\000\039\000\031\000\028\000\
\\070\000\081\000\045\000\014\000\
\\007\000\015\000\058\000\035\000\
\\094\000\093\000\106\000\089\000\
\\007\000\080\000\088\000\069\000\
\\007\000\097\000\054\000\046\000\
\\058\000\055\000\062\000\043\000\
\\029\000\023\000\007\000\007\000\
\\107\000\074\000\007\000\070\000\
\\016\000\033\000\007\000\024\000\
\\017\000\037\000\092\000\073\000\
\\018\000\090\000\071\000\057\000\
\\056\000\063\000\007\000\019\000\
\\060\000\007\000\040\000\064\000\
\\041\000\059\000\020\000\095\000\
\\007\000\007\000\025\000\074\000\
\\065\000\021\000\075\000\060\000\
\\061\000\000\000"
val gotoT =
"\
\\001\000\149\000\002\000\003\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\035\000\004\000\034\000\005\000\033\000\006\000\032\000\
\\007\000\031\000\008\000\030\000\000\000\
\\000\000\
\\002\000\040\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\041\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\042\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\044\000\016\000\002\000\017\000\001\000\018\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\050\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\051\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\052\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\053\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\054\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\055\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\056\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\057\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\058\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\059\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\060\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\061\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\062\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\063\000\016\000\002\000\017\000\001\000\000\000\
\\003\000\064\000\004\000\034\000\005\000\033\000\006\000\032\000\
\\007\000\031\000\008\000\030\000\000\000\
\\007\000\031\000\008\000\065\000\000\000\
\\003\000\066\000\004\000\034\000\005\000\033\000\006\000\032\000\
\\007\000\031\000\008\000\030\000\000\000\
\\003\000\067\000\004\000\034\000\005\000\033\000\006\000\032\000\
\\007\000\031\000\008\000\030\000\000\000\
\\004\000\034\000\005\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\077\000\000\000\
\\000\000\
\\014\000\080\000\000\000\
\\002\000\082\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\084\000\012\000\083\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\044\000\016\000\002\000\017\000\001\000\018\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\091\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\092\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\093\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\094\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\099\000\000\000\
\\000\000\
\\000\000\
\\009\000\102\000\000\000\
\\002\000\106\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\010\000\108\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\112\000\000\000\
\\000\000\
\\002\000\113\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\115\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\117\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\121\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\122\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\015\000\123\000\000\000\
\\002\000\125\000\016\000\002\000\017\000\001\000\000\000\
\\013\000\126\000\000\000\
\\000\000\
\\000\000\
\\002\000\129\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\135\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\011\000\137\000\000\000\
\\002\000\139\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\143\000\016\000\002\000\017\000\001\000\000\000\
\\002\000\144\000\016\000\002\000\017\000\001\000\000\000\
\\000\000\
\\015\000\146\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\148\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 150
val numrules = 67
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
 | exp_seq_t of unit ->  ( ( A.exp * A.pos )  list)
 | exp_seq of unit ->  ( ( A.exp * A.pos )  list)
 | lval_t of unit ->  (A.var) | lval of unit ->  (A.var)
 | rcd_t of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | rcd of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | args_t of unit ->  (A.exp list) | args of unit ->  (A.exp list)
 | tyfs_t of unit ->  (A.field list) | tyfs of unit ->  (A.field list)
 | ty of unit ->  (A.ty) | fundecs of unit ->  (A.fundec list)
 | fundec of unit ->  (A.fundec) | vardec of unit ->  (A.dec)
 | tydecs of unit ->  (tydec list) | tydec of unit ->  (tydec)
 | decs of unit ->  (A.dec list) | exp of unit ->  (A.exp)
 | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | (T 45) => "DEC"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.decs (fn _ => (nil))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.tydecs tydecs1, tydecs1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (tydecs as tydecs1) = tydecs1
 ()
 val  (decs as decs1) = decs1 ()
 in ([A.TypeDec(tydecs)]@decs)
end)
 in ( LrTable.NT 2, ( result, tydecs1left, decs1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (vardec as vardec1) = vardec1
 ()
 val  (decs as decs1) = decs1 ()
 in ([vardec]@decs)
end)
 in ( LrTable.NT 2, ( result, vardec1left, decs1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.fundecs fundecs1, fundecs1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (fundecs as fundecs1) = 
fundecs1 ()
 val  (decs as decs1) = decs1 ()
 in ([A.FunctionDec(fundecs)]@decs)
end)
 in ( LrTable.NT 2, ( result, fundecs1left, decs1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.tydecs (fn _ => let val  (
tydec as tydec1) = tydec1 ()
 in ([tydec])
end)
 in ( LrTable.NT 4, ( result, tydec1left, tydec1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.tydecs tydecs1, _, tydecs1right)) :: ( _, ( 
MlyValue.tydec tydec1, tydec1left, _)) :: rest671)) => let val  result
 = MlyValue.tydecs (fn _ => let val  (tydec as tydec1) = tydec1 ()
 val  (tydecs as tydecs1) = tydecs1 ()
 in ([tydec]@tydecs)
end)
 in ( LrTable.NT 4, ( result, tydec1left, tydecs1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ({name = sym(ID),
		  ty = ty,
		  pos = TYPEleft})
end)
 in ( LrTable.NT 3, ( result, TYPE1left, ty1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(sym(ID),IDleft))
end)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfs tyfs1, _
, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (tyfs as tyfs1) = tyfs1 ()
 in (A.RecordTy(tyfs))
end)
 in ( LrTable.NT 8, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(sym(ID),ARRAYleft))
end)
 in ( LrTable.NT 8, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.tyfs (fn _ => (nil)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.tyfs_t tyfs_t1, _, tyfs_t1right)) :: ( _, (
 MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.tyfs (fn _ => let val  ID1
 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfs_t as tyfs_t1) = tyfs_t1 ()
 in (
[{name = sym(ID1),
		   escape = ref true,
		   typ = sym(ID2),
		   pos = ID1left}]@tyfs_t
)
end)
 in ( LrTable.NT 9, ( result, ID1left, tyfs_t1right), rest671)
end
|  ( 13, ( rest671)) => let val  result = MlyValue.tyfs_t (fn _ => (
nil))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.tyfs_t tyfs_t1, _, tyfs_t1right)) :: ( _, (
 MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _
, ( _, (COMMAleft as COMMA1left), _)) :: rest671)) => let val  result
 = MlyValue.tyfs_t (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfs_t as tyfs_t1) = tyfs_t1 ()
 in (
[{name = sym(ID1),
		   escape = ref true,
		   typ = sym(ID2),
		   pos = COMMAleft}]@tyfs_t
)
end)
 in ( LrTable.NT 10, ( result, COMMA1left, tyfs_t1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec{name = sym(ID),
			  escape = ref true,
			  typ = NONE,
			  init = exp,
			  pos = VARleft}
)
end)
 in ( LrTable.NT 5, ( result, VAR1left, exp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec{name = sym(ID1),
			  escape = ref true,
			  typ = SOME(sym(ID2),ID2left),
			  init = exp,
			  pos = VARleft}
)
end)
 in ( LrTable.NT 5, ( result, VAR1left, exp1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfs tyfs1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let
 val  result = MlyValue.fundec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (tyfs as tyfs1) = tyfs1 ()
 val  (exp as exp1) = exp1 ()
 in (
{name = sym(ID),
		  params = tyfs,
		  result = NONE,
		  body = exp,
		  pos = FUNCTIONleft}
)
end)
 in ( LrTable.NT 6, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfs tyfs1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (
FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result = 
MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfs as tyfs1) = tyfs1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
{name = sym(ID1),
		  params = tyfs,
		  result = SOME(sym(ID2),ID2left),
		  body = exp,
		  pos = FUNCTIONleft}
)
end)
 in ( LrTable.NT 6, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 7, ( result, fundec1left, fundec1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.fundecs fundecs1, _, fundecs1right)) :: ( _
, ( MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) => let val 
 result = MlyValue.fundecs (fn _ => let val  (fundec as fundec1) = 
fundec1 ()
 val  (fundecs as fundecs1) = fundecs1 ()
 in ([fundec]@fundecs)
end)
 in ( LrTable.NT 7, ( result, fundec1left, fundecs1right), rest671)

end
|  ( 21, ( rest671)) => let val  result = MlyValue.args (fn _ => (nil)
)
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 22, ( ( _, ( MlyValue.args_t args_t1, _, args_t1right)) :: ( _, (
 MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.args (fn _ => let val  (exp as exp1) = exp1 ()
 val  (args_t as args_t1) = args_t1 ()
 in ([exp]@args_t)
end)
 in ( LrTable.NT 11, ( result, exp1left, args_t1right), rest671)
end
|  ( 23, ( rest671)) => let val  result = MlyValue.args_t (fn _ => (
nil))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( MlyValue.args_t args_t1, _, args_t1right)) :: ( _, (
 MlyValue.exp exp1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671))
 => let val  result = MlyValue.args_t (fn _ => let val  (exp as exp1)
 = exp1 ()
 val  (args_t as args_t1) = args_t1 ()
 in ([exp]@args_t)
end)
 in ( LrTable.NT 12, ( result, COMMA1left, args_t1right), rest671)
end
|  ( 25, ( rest671)) => let val  result = MlyValue.rcd (fn _ => (nil))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 26, ( ( _, ( MlyValue.rcd_t rcd_t1, _, rcd_t1right)) :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.rcd (fn _ =>
 let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (rcd_t as rcd_t1) = rcd_t1 ()
 in ([(sym(ID),exp,IDleft)]@rcd_t)
end)
 in ( LrTable.NT 13, ( result, ID1left, rcd_t1right), rest671)
end
|  ( 27, ( rest671)) => let val  result = MlyValue.rcd_t (fn _ => (nil
))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( MlyValue.rcd_t rcd_t1, _, rcd_t1right)) :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, IDleft, _))
 :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.rcd_t (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (rcd_t as rcd_t1) = rcd_t1 ()
 in ([(sym(ID),exp,IDleft)]@rcd_t)
end)
 in ( LrTable.NT 14, ( result, COMMA1left, rcd_t1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lval (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.SimpleVar(sym(ID),IDleft))
end)
 in ( LrTable.NT 15, ( result, ID1left, ID1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.lval_t lval_t1, lval_t1left, lval_t1right))
 :: rest671)) => let val  result = MlyValue.lval (fn _ => let val  (
lval_t as lval_t1) = lval_t1 ()
 in (lval_t)
end)
 in ( LrTable.NT 15, ( result, lval_t1left, lval_t1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.lval_t (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
A.FieldVar(A.SimpleVar(sym(ID1),ID1left),
			    sym(ID2),ID1left))

end)
 in ( LrTable.NT 16, ( result, ID1left, ID2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lval_t lval_t1, (lval_tleft as lval_t1left), _)) :: rest671))
 => let val  result = MlyValue.lval_t (fn _ => let val  (lval_t as 
lval_t1) = lval_t1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lval_t,sym(ID),lval_tleft))
end)
 in ( LrTable.NT 16, ( result, lval_t1left, ID1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lval_t (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(A.SimpleVar(sym(ID),IDleft),
							exp,IDleft))

end)
 in ( LrTable.NT 16, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lval_t lval_t1, (lval_tleft as 
lval_t1left), _)) :: rest671)) => let val  result = MlyValue.lval_t
 (fn _ => let val  (lval_t as lval_t1) = lval_t1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(lval_t,exp,lval_tleft))
end)
 in ( LrTable.NT 16, ( result, lval_t1left, RBRACK1right), rest671)

end
|  ( 35, ( ( _, ( MlyValue.lval lval1, lval1left, lval1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lval
 as lval1) = lval1 ()
 in (A.VarExp(lval))
end)
 in ( LrTable.NT 1, ( result, lval1left, lval1right), rest671)
end
|  ( 36, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING,STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp{left = A.IntExp(0),
					 oper = A.MinusOp,
					 right = exp, 
					 pos = MINUSleft}
)
end)
 in ( LrTable.NT 1, ( result, MINUS1left, exp1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (exp_seq as exp_seq1) = 
exp_seq1 ()
 in (A.SeqExp(exp_seq))
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.args args1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as 
ID1) = ID1 ()
 val  (args as args1) = args1 ()
 in (
A.CallExp{func = sym(ID),
					   args = args,
					   pos = IDleft})

end)
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.rcd rcd1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as 
ID1) = ID1 ()
 val  (rcd as rcd1) = rcd1 ()
 in (
A.RecordExp{fields = rcd,
						     typ = sym(ID),
						     pos = IDleft}
)
end)
 in ( LrTable.NT 1, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.ArrayExp{typ = sym(ID),
						    size = exp1,
						    init = exp2,
						    pos = IDleft}
)
end)
 in ( LrTable.NT 1, ( result, ID1left, exp2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.lval lval1, (lvalleft as lval1left), _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (lval as lval1) = lval1
 ()
 val  (exp as exp1) = exp1 ()
 in (
A.AssignExp{var = lval,
					     exp = exp,
					     pos = lvalleft}
)
end)
 in ( LrTable.NT 1, ( result, lval1left, exp1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.IfExp{test = exp1,
					 then' = exp2,
					 else' = SOME(exp3),
					 pos = IFleft}
)
end)
 in ( LrTable.NT 1, ( result, IF1left, exp3right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp{test = exp1,
					 then' = exp2,
					 else' = NONE,
					 pos = IFleft}
)
end)
 in ( LrTable.NT 1, ( result, IF1left, exp2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.WhileExp{test = exp1,
					    body = exp2,
					    pos = WHILEleft}
)
end)
 in ( LrTable.NT 1, ( result, WHILE1left, exp2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp{var = sym(ID),
						  escape = ref true,
						  lo = exp1,
						  hi = exp2,
						  body = exp3,
						  pos = FORleft}
)
end)
 in ( LrTable.NT 1, ( result, FOR1left, exp3right), rest671)
end
|  ( 49, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 50, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp_seq 
exp_seq1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, (
 _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (exp_seq as exp_seq1) = exp_seq1 ()
 in (
A.LetExp{decs = decs,
					  body = A.SeqExp(exp_seq),
					  pos = LETleft}
)
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.PlusOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.MinusOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.DivideOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.TimesOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.EqOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.NeqOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.GtOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.LtOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.GeOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (makeBinExp(exp1,A.LeOp,
					    exp2,exp1left))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp{test = exp1,
					 then' = exp2,
					 else' = SOME(A.IntExp(0)),
					 pos = exp1left}
)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp{test = exp1,
					 then' = A.IntExp(1),
					 else' = SOME(exp2),
					 pos = exp1left}
)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 63, ( rest671)) => let val  result = MlyValue.exp_seq (fn _ => (
nil))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 64, ( ( _, ( MlyValue.exp_seq_t exp_seq_t1, _, exp_seq_t1right))
 :: ( _, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671))
 => let val  result = MlyValue.exp_seq (fn _ => let val  (exp as exp1)
 = exp1 ()
 val  (exp_seq_t as exp_seq_t1) = exp_seq_t1 ()
 in ([(exp,expleft)]@exp_seq_t)
end)
 in ( LrTable.NT 17, ( result, exp1left, exp_seq_t1right), rest671)

end
|  ( 65, ( rest671)) => let val  result = MlyValue.exp_seq_t (fn _ =>
 (nil))
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 66, ( ( _, ( MlyValue.exp_seq_t exp_seq_t1, _, exp_seq_t1right))
 :: ( _, ( MlyValue.exp exp1, expleft, _)) :: ( _, ( _, SEMICOLON1left
, _)) :: rest671)) => let val  result = MlyValue.exp_seq_t (fn _ =>
 let val  (exp as exp1) = exp1 ()
 val  (exp_seq_t as exp_seq_t1) = exp_seq_t1 ()
 in ([(exp,expleft)]@exp_seq_t)
end)
 in ( LrTable.NT 18, ( result, SEMICOLON1left, exp_seq_t1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun DEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end
