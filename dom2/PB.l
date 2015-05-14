%{
#include <stdio.h>
#include <stdlib.h>
%}
%option noyywrap
%%
s  return STRDCL;
p  return PRINT;
[a-o|q|r|t-z] yylval.id = yytext[0]; return ID;
\"[a-zA-Z0-9]*\" strcpy(yylval.str, yytext + 1); yylval.str[yyleng - 2] = '\0'; return STRING;
[ \t\n\r]* {}
. yyerror("");
%%
