CC = gcc
CFLAGS = -g -Wall
YACC = bison
YFLAGS = -t -d -v
OBJS = mpc.o scan.o parse.tab.o astree.o symtab.o types.o decls.o \
       exprs.o stmts.o objects.o values.o tempreg.o gen_lib.o gen_code.o
INCS = project.h \
       astree.h decls.h exprs.h mpc.h objects.h parse.h scan.h stmts.h \
       structs.h symtab.h types.h values.h tempreg.h gen_lib.h gen_code.h

%.tab.c %.tab.h: %.y
	$(YACC) $(YFLAGS) $<

%.token: %.pas mpc
	mpc -t $<

%.parse: %.pas mpc
	mpc -p $<

%.check: %.pas mpc
	mpc -c $<

%.alloc: %.pas mpc
	mpc -a $<

%.spim:  %.pas mpc
	mpc $<

%.run:   %.spim
	$(spim) -file $<

# default target is parse driver
mpc: $(OBJS)

mpc.o: mpc.c $(INCS)

parse.tab.o: parse.tab.c $(INCS) 

parse.tab.c parse.tab.h: parse.y $(INCS)

scan.o:    scan.c     $(INCS) parse.tab.h

astree.o:   astree.c   $(INCS) parse.tab.h

exprs.o:    exprs.c    $(INCS) parse.tab.h

types.o:    types.c    $(INCS)

objects.o:  objects.c  $(INCS)

values.o:   values.c   $(INCS)

symtab.o:   symtab.c   $(INCS)

decls.o:    decls.c    $(INCS)

stmts.o:    stmts.c    $(INCS)

tempreg.o:  tempreg.c  $(INCS)

gen_lib.o:  gen_lib.c  $(INCS)

gen_code.o: gen_code.c $(INCS) parse.tab.h

tables: parse.tab.c

TAGS:
	etags *.[chy]

clean:
	rm -f *.o *.tab.* *.output mpc a.out core
	rm -f *.token *.parse *.check *.alloc *.spim
