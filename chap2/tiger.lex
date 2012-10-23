(* $Id: tiger.lex,v 1.2 2011/03/28 03:07:29 csun Exp $ *)

type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun str2int(s) = foldl (fn(a,r) => ord(a)-ord(#"0")+10*r) 0 (explode s)

%%
DIGIT=[0-9]+;
COMMENT=\/\*.*\*\/;
SPACE=[\ \t\b\f\r]+;
IDENTIFIER=[a-z][a-z0-9]*;
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

(* operators *)
"+"     => (Tokens.PLUS(yypos,yypos+1));
"-"     => (Tokens.MINUS(yypos,yypos+1));
"*"     => (Tokens.TIMES(yypos,yypos+1));
"/"     => (Tokens.DIVIDE(yypos,yypos+1));
","	=> (Tokens.COMMA(yypos,yypos+1));
"|"     => (Tokens.OR(yypos,yypos+1));
"="     => (Tokens.EQ(yypos,yypos+1));
":"     => (Tokens.COLON(yypos,yypos+1));
";"     => (Tokens.SEMICOLON(yypos,yypos+1));
"{"     => (Tokens.LBRACE(yypos,yypos+1));
"}"     => (Tokens.RBRACE(yypos,yypos+1));
"("     => (Tokens.LPAREN(yypos,yypos+1));
")"     => (Tokens.RPAREN(yypos,yypos+1));
"["     => (Tokens.LBRACK(yypos,yypos+1));
"]"     => (Tokens.RBRACK(yypos,yypos+1));

(* keywords *)
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

{IDENTIFIER} => (Tokens.ID(yytext,yypos,yypos+size yytext));
{DIGIT}      => (Tokens.INT(str2int yytext,yypos,yypos+size yytext));
{COMMENT}    => (continue());
{SPACE}      => (continue());
.            => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

