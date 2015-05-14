%{
#include <stdio.h>
#include <stdlib.h>
char buffer[25565] = {0};
int lbuf = 0;
%}
%union {
char id;
char str[25565];
}
%token ID
%token STRDCL
%token STRING
%token PRINT
%%
proc   : dcl stmt
		   ;
dcl    : strdcl id string
			 ;
stmt   : print id
		   ;
id     : ID { lbuf += sprintf(buffer + lbuf, "id %c\n", yylval.id); }
		   ;
strdcl : STRDCL { lbuf += sprintf(buffer + lbuf, "strdcl s\n"); }
			 ;
			 ;
string : STRING { 
       lbuf += sprintf(buffer + lbuf, "quote \"\n");
			 lbuf += sprintf(buffer + lbuf, "string %s\n", yylval.str); 
       lbuf += sprintf(buffer + lbuf, "quote \"\n");
}
			 ;
print  : PRINT { lbuf += sprintf(buffer + lbuf, "print p\n"); }
			 ;
%%
int yyerror(char *err) {
	fprintf(stdout, "valid input\n");
	exit(0);
	return 0;
}
int main() {
	yyparse();
	printf("%s", buffer);
	return 0;
}
