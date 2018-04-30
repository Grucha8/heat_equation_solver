all:
	gfortran -ffree-form -std=f2008 -fimplicit-none -cpp -Wall -pedantic -o prog gausEl.F90 matrixOperations.F90 \
	averageCalc.F90 prog.F90
	./prog

clean:
	rm -rf prog 