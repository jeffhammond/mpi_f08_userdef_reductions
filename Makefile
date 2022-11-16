CC = mpiicc -cc=icx
CFLAGS = -g -Os

FC = mpiifort -fc=ifx
FCFLAGS = -g -Os

all: test_reduce.x mcve.x

%.x: %.F90 trampoline.o
	$(FC) $(FCFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm -f *.o *.x *.mod
	-rm -fr *.dSYM

