.phony: all clean


all: PB


clean: 
	rm PB.yylex.c PB.yy.c PB.lex.c PB.o

PB.lex.c: PB.l
	lex -o PB.lex.c PB.l

PB.yy.c: PB.y
	yacc -o PB.yy.c PB.y

PB.yylex.c: PB.lex.c PB.yy.c
	cat PB.yy.c PB.lex.c > PB.yylex.c

PB.o: PB.yylex.c
	gcc-4.9 --std=c89 -Wall -o PB.o PB.yylex.c


PB: PB.o
	
