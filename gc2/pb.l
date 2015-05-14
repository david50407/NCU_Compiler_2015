%{
#include <stdio.h>
#include <stdlib.h>
%}
%option noyywrap

STR [A-Za-z0-9]+
IP ([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])

%%
^\/\/.*$ { printf("--%s--\n", yytext + 2); }
^[A-Z][A-Za-z]*$ { printf("%s is a word\n", yytext); }
^{IP}\.{IP}\.{IP}\.{IP}$ { printf("%s is an IP address\n", yytext); }
^[^@ \n\t]+@{STR}(\.{STR})+$ { printf("%s is an email address\n", yytext); }
.|[ \n\t\r] {}
%%

int yyerror(char * err) { return 0; }

int main()
{
	yylex();
	return 0;
}
