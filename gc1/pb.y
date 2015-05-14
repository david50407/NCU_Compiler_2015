%{
#include <stdio.h>
#include <stdlib.h>
char buffer[25565] = {0};
int lbuf = 0;
%}
%union {
char str[25565];
}
%token LEVEL
%token FirstName
%token LastName
%token CID
%token BATTLE
%%
proc   : dcl stmt
		   ;
dcl    : level id
			 ;
stmt   : battle id
		   ;
level  : LEVEL { lbuf += sprintf(buffer + lbuf, "level %s\n", yylval.str); }
			 ;
id     : cid
       | fn ln
       ;
cid    : CID { lbuf += sprintf(buffer + lbuf, "cid %s\n", yylval.str); }
		   ;
fn     : FirstName { lbuf += sprintf(buffer + lbuf, "FirstName %s\n", yylval.str); }
			 ;
ln     : LastName { lbuf += sprintf(buffer + lbuf, "LastName %s\n", yylval.str); }
			 ;
battle : BATTLE { lbuf += sprintf(buffer + lbuf, "battle b\n"); }
			 ;
%%
int yyerror(char *err) {
	fprintf(stdout, "invalid input\n");
	exit(0);
	return 0;
}
int main() {
	yyparse();
	printf("%s", buffer);
	return 0;
}
