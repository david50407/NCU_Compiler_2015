
OBJS	= libasm.o
PROGS	= libasm.a

all:	$(PROGS)

.SUFFIX: .s

%.o: %.s
	nasm -f elf $<

libasm.a: $(OBJS)
	ar rc libasm.a $(OBJS)

clean:
	rm -f *.o $(PROGS)

