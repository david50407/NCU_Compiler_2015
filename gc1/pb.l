%{
#include <stdio.h>
#include <stdlib.h>
%}
%option noyywrap
%%
l[0-9]+      strcpy(yylval.str, yytext); return LEVEL;
[A-Z][a-z]*  strcpy(yylval.str, yytext); return FirstName;
[a|c-z]+     strcpy(yylval.str, yytext); return LastName;
[0-9]+       strcpy(yylval.str, yytext); return CID;
b            strcpy(yylval.str, yytext); return BATTLE;
[ \t\n\r]* {}
. yyerror("");
%%
