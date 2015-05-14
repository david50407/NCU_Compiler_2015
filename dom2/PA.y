%{
#include <stdio.h>
#include <stdlib.h>
%}
%union {
int ival;
char id;
}
%token <ival> INUM
%token <id> ID
%token ASSIGN
%%
proc   : stmt
		   ;
stmt   : id assign val
		   ;
id     : ID { printf("id %c\n", yylval.id); }
		   ;
assign : ASSIGN { printf("assign =\n"); }
			 ;
val    : id
			 | INUM { printf("inum %d\n", yylval.ival); }
		   ;
%%
int yyerror(char *err) {
	fprintf(stdout, "valid input\n");
	return 0;
}
int yylex() {
	int ch;
	int i;
	ch = getc(stdin);
	while (ch == ' ' || ch == '\n') ch = getc(stdin);
	if ( ch >= 'a' && ch <= 'z' ) { /* [a-z] */
		yylval.id = (char)ch;
		return ID;
	}
	else if ( ch >= '0' && ch <= '9' ) { /* [0-9]+ */
		ungetc(ch, stdin);
		fscanf(stdin, "%d", &yylval.ival);
		return INUM;
	}
	else if ( ch == '=' ) /* = */
		return ASSIGN;
	return ch;
}
int main() {
	yyparse();
	return 0;
}
