structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor VaporeonLexFun(structure Tokens: Vaporeon_TOKENS));
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
[~]?{digit}+  => (Tokens.NUM (Rational.fromDecimal yytext, !pos, !pos));
[~]?{digit}*"."{digit}*"("{digit}+")" => (Tokens.NUM (Rational.fromDecimal yytext, !pos, !pos));
[~]?{digit}*"."{digit}+ => (Tokens.NUM  (Rational.fromDecimal yytext, !pos, !pos));

"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
.      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());
