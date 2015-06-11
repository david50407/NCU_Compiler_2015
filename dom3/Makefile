.phony: all clean


all: PB


clean: 
	rm PB.yylex.cpp PB.yy.cpp PB.lex.cpp PB.o

PB.lex.cpp: PB.l
	lex -o PB.lex.cpp PB.l

PB.yy.cpp: PB.y
	yacc -o PB.yy.cpp PB.y

PB.yylex.cpp: PB.lex.cpp PB.yy.cpp
	cat PB.yy.cpp PB.lex.cpp > PB.yylex.cpp

PB.o: PB.yylex.cpp
	gcc -Wall -o PB.o PB.yylex.cpp


PB: PB.o
	
