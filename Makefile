CC = mpicc
CFLAGS = -g -Wall -Os

FC = mpifort
FCFLAGS = -g -Wall -Os

all: test_reduce.x

%.x: %.F90 trampoline.o
	$(FC) $(FCFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm -f *.o *.x *.mod
	-rm -fr *.dSYM

