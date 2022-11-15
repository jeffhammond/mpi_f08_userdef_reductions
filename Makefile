CC = mpicc
CFLAGS = -g -Os

FC = mpifort
FCFLAGS = -g -Os

all: test_reduce.x

%.x: %.F90 trampoline.o
	$(FC) $(FCFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm -f *.o *.x *.mod
	-rm -fr *.dSYM

