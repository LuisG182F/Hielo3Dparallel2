# Variables
FC ?= gfortran
FFLAGS ?= -fopenmp -Wall -O2 -ffast-math -funroll-all-loops
#FFLAGS ?= -fopenmp -warn all -O2 -ffast-math

# Archivos fuente y objetos
SRC = presicion.f90 hielonn.f90 radio0.f90 radio1.f90 radio2.f90 cal_radio.f90 precipitado1.f90 Montecarlo.f90 poli_3D_Q.f90
OBJ = $(SRC:.f90=.o)

# Nombre del ejecutable
EXEC = fenix

# Regla por defecto
all: $(EXEC)

# Enlazado final del ejecutable
$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) -o $@

# Regla genérica para compilar cada .f90 a .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Limpiar archivos generados
clean:
	rm -f $(EXEC)
	rm -f gmon.out  
	rm -f *.mod *.o


# Perfilado
profile: $(EXEC)
	./$(EXEC)
	gprof $(EXEC) gmon.out > profile_report.txt
