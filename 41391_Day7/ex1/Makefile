TARGET	= mandelbrot
OBJS	= utilities.o main.o mandel.o timestamp.o writepng.o

OPT	= -g -O3
ISA	= 
PARA	= 

PNGWRITERPATH =	pngwriter
ARCH          ?= $(shell uname -m)
PNGWRTLPATH   = $(PNGWRITERPATH)/lib/$(ARCH)
PNGWRTIPATH   = $(PNGWRITERPATH)/include
PNGWRITERLIB  = $(PNGWRTLPATH)/libpngwriter.a

CCC	= gcc
CXX	= g++
CXXFLAGS= -I $(PNGWRTIPATH)

F90C  	= gfortran
F90FLAGS= $(OPT) $(ISA) $(PARA) $(XOPT)
.SUFFIXES: .f90

LIBS	= -L$(PNGWRTLPATH) -lpngwriter -lpng -lc -lstdc++

all: $(PNGWRITERLIB) $(TARGET)

$(TARGET): $(OBJS) 
	$(F90C) $(F90FLAGS) -o $@ $(OBJS) $(LIBS)

$(PNGWRITERLIB):
	@cd pngwriter/src && $(MAKE)

clean:
	@$(RM) *.o core

realclean: clean
	@cd pngwriter/src && $(MAKE) clean
	@$(RM) $(PNGWRITERLIB)
	@$(RM) $(TARGET)
	@$(RM) mandelbrot.png


%.o: %.f90
	$(F90C) $(F90FLAGS) -c $< -o $@

%.o: %.cc
	$(CXX) $(CXXFLAGS) -c $< -o $@

# dependencies
#
main.o : main.f90 utilities.o
timestamp.o: timestamp.f90
writepng.o: writepng.cc writepng.h
mandel.o: mandel.f90
utilities.o: utilities.f90
