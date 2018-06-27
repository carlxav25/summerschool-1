F95 := gfortran	

OPT_OPTIMIZE :=  -O2 -ffpe-trap=invalid,zero,overflow -ffree-line-length-none

FCFLAGS=-Wall

EXE=lap_test
OBJ=laplacian.o main.o

all: $(EXE)

laplacian.o: laplacian.F90
main.o: main.F90 laplacian.o

$(EXE): $(OBJ)
	$(F95) $(FCFLAGS) $(OPT_OPTIMIZE) $(OBJ) -o $@

%.o: %.F90
	$(F95) $(FCFLAGS)  -c $< -o $@

clean:
	rm -f $(EXE) a.out *.o *.mod *~ 



