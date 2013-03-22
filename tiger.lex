type pos = int

(* for some strange reason I need to include the following code
 * (otherwise it wont compile). 
 * See http://www.smlnj.org/doc/ML-Yacc/mlyacc004.html *)
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token


val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun str2int(s) = foldl (fn(a,r) => ord(a)-ord(#"0")+10*r) 0 (explode s)



%%
DIGIT=[0-9]+;
COMMENT=\/\*.*\*\/;
SPACE=[\ \t\b\f\r]+;
IDENTIFIER=[a-zA-Z][a-zA-Z0-9]*;
QUOTE=[\"];
NONQUOTE=[^\"];

%header (functor TigerLexFun (structure Tokens:Tiger_TOKENS));
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

"+"     => (Tokens.PLUS(yypos,yypos+1));
"-"     => (Tokens.MINUS(yypos,yypos+1));
"*"     => (Tokens.TIMES(yypos,yypos+1));
"/"     => (Tokens.DIVIDE(yypos,yypos+1));
"," 	=> (Tokens.COMMA(yypos,yypos+1));
"&"		=> (Tokens.AND(yypos,yypos+1));
"|"     => (Tokens.OR(yypos,yypos+1));
"="     => (Tokens.EQ(yypos,yypos+1));
">="	=> (Tokens.GE(yypos,yypos+2));
">"		=> (Tokens.GT(yypos,yypos+1));
"<="	=> (Tokens.LE(yypos,yypos+2));
"<"		=> (Tokens.LT(yypos,yypos+1));
"<>"	=> (Tokens.NEQ(yypos,yypos+2));
"."		=> (Tokens.DOT(yypos,yypos+1));
":"     => (Tokens.COLON(yypos,yypos+1));
";"     => (Tokens.SEMICOLON(yypos,yypos+1));
":="	=> (Tokens.ASSIGN(yypos,yypos+2));
"{"     => (Tokens.LBRACE(yypos,yypos+1));
"}"     => (Tokens.RBRACE(yypos,yypos+1));
"("     => (Tokens.LPAREN(yypos,yypos+1));
")"     => (Tokens.RPAREN(yypos,yypos+1));
"["     => (Tokens.LBRACK(yypos,yypos+1));
"]"     => (Tokens.RBRACK(yypos,yypos+1));

var  	 => (Tokens.VAR(yypos,yypos+3));
function => (Tokens.FUNCTION(yypos,yypos+8));
break    => (Tokens.BREAK(yypos,yypos+5));
of       => (Tokens.OF(yypos,yypos+2));
end      => (Tokens.END(yypos,yypos+3));
in       => (Tokens.IN(yypos,yypos+2));
nil      => (Tokens.NIL(yypos,yypos+3));
let      => (Tokens.LET(yypos,yypos+3));
do       => (Tokens.DO(yypos,yypos+2));
to       => (Tokens.TO(yypos,yypos+2));
for      => (Tokens.FOR(yypos,yypos+3));
while    => (Tokens.WHILE(yypos,yypos+5));
else     => (Tokens.ELSE(yypos,yypos+4));
then     => (Tokens.THEN(yypos,yypos+4));
if       => (Tokens.IF(yypos,yypos+2));
array    => (Tokens.ARRAY(yypos,yypos+5));
type     => (Tokens.TYPE(yypos,yypos+4));

{QUOTE}{NONQUOTE}*{QUOTE} => (Tokens.STRING(yytext,yypos,yypos+size yytext));
{IDENTIFIER} => (Tokens.ID(yytext,yypos,yypos+size yytext));
{DIGIT}      => (Tokens.INT(str2int yytext,yypos,yypos+size yytext));
{COMMENT}    => (continue());
{SPACE}      => (continue());
.            => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

