%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
%}
%option noyywrap
%%
([1-9][0-9]*) { sscanf(yytext, "%u", &yylval.count); return COUNT; }
[A-Z][a-z]* { strcpy(yylval.name, yytext); return ITEM; }
\+ { return PLUS; }
-> { return EQL; }
\( { return GROUP_START; }
\) { return GROUP_END; }
[ \t\r\n]* {}
. yyerror("");
%%
