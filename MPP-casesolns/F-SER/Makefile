MF=	Makefile

FC=	ifort
FFLAGS=	-O3

LFLAGS=	

EXE=	image

SRC= \
	image.f90 \
	pgmio.f90

#
# No need to edit below this line
#

.SUFFIXES:
.SUFFIXES: .f90 .o

OBJ=	$(SRC:.f90=.o)

.f90.o:
	$(FC) $(FFLAGS) -c $<

all:	$(EXE)

$(EXE):	$(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LFLAGS)

$(OBJ):	$(MF)

image.o:	pgmio.o

clean:
	rm -f $(OBJ) $(EXE) core
