.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# PATH options
objdir = .obj

# Command-line options at make call
debug ?= 0 

## GFORTRAN OPTIONS ##
FC  = gfortran
INC_NC  = -I/opt/local/include
LIB_NC  = -L/opt/local/lib -lnetcdff -lnetcdf
INC_COORD = -I/Users/robinson/models/EURICE/coord/.obj
LIB_COORD = /Users/robinson/models/EURICE/coord/libcoordinates.a

FLAGS  = -I$(objdir) -J$(objdir) $(INC_COORD) $(INC_NC) 
LFLAGS = $(LIB_COORD) $(LIB_NC)

DFLAGS = -O3

DFLAGS = -O3
ifeq ($(debug), 1)  # ,underflow
    DFLAGS   = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow -fbacktrace -fcheck=all
endif

## Individual libraries or modules ##
$(objdir)/ncio.o: ncio.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Complete programs

grid_mapping: $(objdir)/ncio.o
	$(FC) $(DFLAGS) $(FLAGS) -o grid_mapping.x $^ grid_mapping.f90 $(LFLAGS)
	@echo " "
	@echo "    grid_mapping.x is ready."
	@echo " "

clean:
	rm -f *.x $(objdir)/*.o $(objdir)/*.mod

