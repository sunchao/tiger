structure Translate : TRANSLATE =
struct

structure Frame : FRAME = MipsFrame

datatype level = Top
               | Lev of {parent: level, frame: Frame.frame} * unit ref

type access = level * Frame.access

structure T = Tree
structure F = Frame


datatype exp = Ex of T.exp
             | Nx of T.stm
             | Cx of Temp.label * Temp.label -> T.stm

type frag = F.frag
structure A = Absyn

val outermost = Top

val fragments : frag list ref = ref nil

fun reset () = fragments := nil

fun getResult () = !fragments

val errexp = Ex(T.CONST 0)

fun newLevel {parent,name,formals} =
    Lev({parent=parent,
        frame=Frame.newFrame{name=name,formals=true::formals}}, ref ())

(* Return formals associated with the frame in this level,
 * excluding the static link (first element of the list *)
fun formals lev =
    case lev of
      Top => nil
    | Lev({parent=parent,frame=frame},_) =>
      let val formals = tl (Frame.formals frame) in
      (map (fn (x) => (lev,x)) formals) end

(* Allocate a new local variable either
 * on frame or in register (depending on whether it escapes) *)
fun allocLocal lev escape =
    case lev of
      Lev({parent=_,frame=frame},_) => (lev,Frame.allocLocal frame escape)

fun seq stmlist =
    case stmlist of
      [s] => s
    | [s1,s2] => T.SEQ(s1,s2)
    | stm :: rest => T.SEQ(stm,seq(rest))

(* unEx : exp -> Tree.exp *)
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp()
        val t = Temp.newlabel()
        val f = Temp.newlabel()
    in T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                  genstm(t,f),
                  T.LABEL f,
                  T.MOVE(T.TEMP r, T.CONST 0),
                  T.LABEL t],
              T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

(* unNx : exp -> Tree.stm *)
fun unNx (Ex e) = T.EXP e
  | unNx (Nx s) = s
  | unNx (Cx genstm) =
    let val t = Temp.newlabel() in genstm(t,t); T.LABEL t end

(* unCx : exp -> (Temp.label*Temp.label->Tree.stm *)
fun unCx (Cx genstm) = genstm
  | unCx (Ex (T.CONST 0)) = (fn (t,f) => T.JUMP(T.NAME f,[f]))
  | unCx (Ex (T.CONST 1)) = (fn (t,f) => T.JUMP(T.NAME t,[t]))
  | unCx (Ex e) = (fn (t,f) => T.CJUMP(T.EQ, e, T.CONST 0, f, t))
  | unCx (Nx _) = raise ErrorMsg.Error


val nilexp = Ex(T.CONST(0))

fun intlit (n: int) : exp = Ex(T.CONST(n))

fun strlit (s: string) : exp =
    let val t = List.find
      (fn (x) =>
          case x of
            F.PROC _ => false
          | F.STRING(_,s') => s = s') (!fragments)
    in case t of
         NONE =>
         let val l = Temp.newlabel() in
         (fragments := F.STRING(l,s) :: !fragments; Ex(T.NAME(l))) end
       | SOME(F.STRING(lab,_)) => Ex(T.NAME(lab))
    end

fun binop (oper,e1,e2) : exp =
    let
      val left = unEx(e1)
      val right = unEx(e2)
      val treeop =
          case oper of
            A.PlusOp => T.PLUS
          | A.MinusOp => T.MINUS
          | A.TimesOp => T.MUL
          | A.DivideOp => T.DIV
    in Ex(T.BINOP(treeop,left,right))
    end

fun relop (oper,e1,e2) : exp =
    let
      val left = unEx(e1)
      val right = unEx(e2)
      val treeop =
          case oper of
            A.EqOp => T.EQ
          | A.NeqOp => T.NE
          | A.LtOp => T.LT
          | A.LeOp => T.LE
          | A.GtOp => T.GT
          | A.GeOp => T.GE
    in Cx((fn (t,f) => T.CJUMP(treeop,left,right,t,f))) end

fun memplus (e1:T.exp,e2:T.exp) = T.MEM(T.BINOP(T.PLUS,e1,e2))

(* fetch static links between the level of use (the
 * level passed to simpleVar) and the level of definition
 * (the level within the variable's access) *)
fun simpleVar (access,level): exp =
    let val (Lev(_,defref),defaccess) = access
        fun iter (curlevel, acc) =
            let val Lev({parent,frame},curref) = curlevel
            in if (defref = curref) then
                 Frame.exp(defaccess)(acc)
               else let val staticlink = hd(Frame.formals frame)
                    in iter(parent,Frame.exp(staticlink)(acc))
                    end
            end
    in Ex(iter(level,T.TEMP(Frame.FP))) end

fun subscriptVar (base,offset): exp =
    Ex(memplus(unEx(base),
               T.BINOP(T.MUL,unEx(offset),T.CONST(Frame.wordSize))))

fun fieldVar (base,id,flist) : exp =
    (* pre-condition: id in flist *)
    let fun findindex (index,elem,list) =
            if elem = hd(list) then index
            else findindex(index+1,elem,tl(list)) in
      Ex(memplus(unEx(base),
                 T.BINOP(T.MUL,T.CONST(findindex(0,id,flist)),
                                    T.CONST(Frame.wordSize))))
    end

(* The elseexp could be NONE, in which case the
 * result must be unit, and thus we return CONST(0) *)
fun ifelse (testexp,thenexp,elseexp: exp option) : exp =
    let
      val r = Temp.newtemp() (* hold result *)
      val t = Temp.newlabel()
      val f = Temp.newlabel()
      val finish = Temp.newlabel()
      val testfun  = unCx(testexp) in
      case thenexp of
          Ex(e) =>
          (case elseexp of
              SOME(e') => (* Nx possible? *)
              Ex(T.ESEQ(seq[testfun(t,f),
                            T.LABEL t, T.MOVE(T.TEMP r, e),
                            T.JUMP (T.NAME finish, [finish]),
                            T.LABEL f, T.MOVE(T.TEMP r, unEx(e')),
                            T.JUMP (T.NAME finish, [finish]),
                            T.LABEL finish],
                        T.TEMP r)))
        | Nx(thenstm) =>
          (case elseexp of
               NONE =>
               Nx(seq[testfun(t,f),
                       T.LABEL t, thenstm,
                       T.LABEL f])
             | SOME(st) => (* must be a statement *)
               Nx(seq[testfun(t,f),
                      T.LABEL t, thenstm,
                      T.JUMP (T.NAME finish, [finish]),
                      T.LABEL f, unNx(st),
                      T.JUMP (T.NAME finish, [finish]),
                      T.LABEL finish]))
        | Cx(cf) => (* TODO: fix this *)
          let
            val z = Temp.newlabel ()
          in
            case elseexp of
                SOME(e) =>
                Cx(fn(t',f') =>
                      seq[testfun(t,f),
                          T.LABEL t, cf(t',f'),
                          T.LABEL f, (unCx e)(t',f')])
          end
    end

(* creating a record (see page 164) *)
fun record (fields) : exp =
    let
      val r = Temp.newtemp()
      val init =
          T.MOVE(
          T.TEMP r,
          F.externalCall(
          "allocRecord", [T.CONST(length(fields)*F.wordSize)]))

      fun loop (fields,index) =
          case fields of
            nil => nil
          | e :: rest =>
            T.MOVE(
            memplus(T.TEMP r, T.CONST(index*F.wordSize)),
            unEx(e)) :: loop(rest,index+1)
    in Ex(T.ESEQ(seq(init::loop(fields,0)),T.TEMP r))
    end


fun array (size, init) : exp =
    Ex(F.externalCall("initArray", [unEx(size),unEx(init)]))

fun assign (left,right) : exp =
    Nx(T.MOVE(unEx(left),unEx(right)))

fun loop (test, body, done_label) =
    let val test_label = Temp.newlabel()
        val body_label = Temp.newlabel() in
      Nx(seq[
      T.LABEL test_label,
      T.CJUMP(T.EQ,unEx(test),T.CONST 0, done_label, body_label),
      T.LABEL body_label,
      unNx(body),
      T.JUMP(T.NAME test_label, [test_label]),
      T.LABEL done_label])
    end

fun break (label) : exp = Nx(T.JUMP(T.NAME label, [label]))

fun call (_,Lev({parent=Top,...},_),label,exps,isprocedure) : exp = (* external call *)
    if isprocedure
    then Nx(T.EXP(F.externalCall(Symbol.name label,map unEx exps)))
    else Ex(F.externalCall(Symbol.name label,map unEx exps))

  | call (uselevel,deflevel,label,exps,isprocedure) : exp =
    (* find the difference
     * of static nesting depth between uselevel and deflevel *)
    let
      fun depth level =
            case level of
              Top => 0
            | Lev({parent,...},_) => 1 + depth(parent)
      val diff = depth uselevel - depth deflevel + 1
      fun iter (d,curlevel) =
          if d = 0 then T.TEMP Frame.FP
          else
            let val Lev({parent,frame},_) = curlevel in
              Frame.exp(hd(Frame.formals frame))(iter(d-1,parent))
            end
      val call = T.CALL(T.NAME label,(iter(diff,uselevel)) :: (map unEx exps))
    in if isprocedure
       then Nx(T.EXP(call)) else Ex(call)
    end

(* result is the last exp. note that the last sequence
 * might be a statement, which makes the whole sequence statement. *)
fun sequence (exps: exp list) =
    let val len = length exps in
      if len = 0 then Nx(T.EXP(T.CONST 0)) (* () is a statement *)
      else if len = 1 then hd(exps)
      else
        let val first = seq(map unNx (List.take(exps,length(exps)-1)))
            val last = List.last(exps) in
          case last of
            Nx(s) => Nx(T.SEQ(first,s))
          | _ => Ex(T.ESEQ(first,unEx(last)))
        end
    end

fun letexp (decs,body) =
    let val len = List.length decs in
      if len = 0 then body
      else if len = 1 then Ex(T.ESEQ(unNx(hd(decs)),unEx(body)))
      else let val s = map unNx decs in Ex(T.ESEQ(seq s,unEx(body))) end
    end


fun procEntryExit (Lev({frame,...},_),body) =
    let val body' =
            Frame.procEntryExit1(frame,T.MOVE(T.TEMP Frame.RV,unEx(body)))
    in fragments := Frame.PROC{frame=frame,body=body'} :: !fragments
    end
end
