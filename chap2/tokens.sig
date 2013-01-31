signature Tiger_TOKENS =
sig
type linenum (* = int *)
type token
val TYPE:  linenum * linenum -> token
val VAR:  linenum * linenum -> token
val FUNCTION:  linenum * linenum -> token
val BREAK:  linenum * linenum -> token
val OF:  linenum * linenum -> token
val END:  linenum * linenum -> token
val IN:  linenum * linenum -> token
val NIL:  linenum * linenum -> token
val LET:  linenum * linenum -> token
val DO:  linenum * linenum -> token
val TO:  linenum * linenum -> token
val FOR:  linenum * linenum -> token
val WHILE:  linenum * linenum -> token
val ELSE:  linenum * linenum -> token
val THEN:  linenum * linenum -> token
val IF:  linenum * linenum -> token
val ARRAY:  linenum * linenum -> token
val ASSIGN:  linenum * linenum -> token
val OR:  linenum * linenum -> token
val AND:  linenum * linenum -> token
val GE:  linenum * linenum -> token
val GT:  linenum * linenum -> token
val LE:  linenum * linenum -> token
val LT:  linenum * linenum -> token
val NEQ:  linenum * linenum -> token
val EQ:  linenum * linenum -> token
val DIVIDE:  linenum * linenum -> token
val TIMES:  linenum * linenum -> token
val MINUS:  linenum * linenum -> token
val PLUS:  linenum * linenum -> token
val DOT:  linenum * linenum -> token
val RBRACE:  linenum * linenum -> token
val LBRACE:  linenum * linenum -> token
val RBRACK:  linenum * linenum -> token
val LBRACK:  linenum * linenum -> token
val RPAREN:  linenum * linenum -> token
val LPAREN:  linenum * linenum -> token
val SEMICOLON:  linenum * linenum -> token
val COLON:  linenum * linenum -> token
val COMMA:  linenum * linenum -> token
val STRING: (string) *  linenum * linenum -> token
val INT: (int) *  linenum * linenum -> token
val ID: (string) *  linenum * linenum -> token
val EOF:  linenum * linenum -> token
end
