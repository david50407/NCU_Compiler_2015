PA.yy.c:
	yacc -o PA.yy.c PA.y

PA.o: PA.yy.c
	gcc -Wall -o PA.o PA.yy.c

PB.lex.c:
	lex -o PB.lex.c PB.l

PB.yy.c:
	yacc -o PB.yy.c PB.y

PB.yylex.c: PB.lex.c PB.yy.c
	cat PB.yy.c PB.lex.c > PB.yylex.c

PB.o: PB.yylex.c
	gcc -Wall -o PB.o PB.yylex.c

PA: PA.o
	

PB: PB.o
	

