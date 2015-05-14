pb.o: pb.yylex.c
	gcc -Wall -o pb.o pb.yylex.c

pb.yylex.c: pb.lex.c pb.yy.c
	cat pb.yy.c pb.lex.c > pb.yylex.c

pb.yy.c:
	yacc -o pb.yy.c pb.y

pb.lex.c:
	lex -o pb.lex.c pb.l
