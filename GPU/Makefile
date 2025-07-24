# Variables
FC ?= nvfortran
FFLAGS ?= -O2 -g -cuda -gpu=ccnative,fastmath -Wall 
#FFLAGS ?= -fopenmp -O2 -ffast-math

# Archivos fuente y objetos
SRC = RNGnew.cuf presicion.f90 hielonnGPU.cuf radio0.cuf radio1.cuf radio2.cuf cal_radio.cuf radio0cpu.f90 radio1cpu.f90 radio2cpu.f90 precipitado1CPU.f90 precipitado1GPU.cuf Montecarlo_GPU.cuf Gancho.cuf poli_3D_QGPU.f90
OBJ = $(patsubst %.f90,%.o,$(patsubst %.cuf,%.o,$(SRC)))

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

# Cambio: Añadir una regla para compilar cada .cuf a .o
%.o: %.cuf
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
