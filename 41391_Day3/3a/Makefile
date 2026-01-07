# Build/run the 3b array-swap exercise

FC      := gfortran
FFLAGS  := -O3 -Wall
LDFLAGS :=

# Keep *all* artifacts inside 3b/ so we don't interfere with the top-level build.
OBJDIR  := build
BINDIR  := bin
TARGET  := main_3b

ifeq ($(OS),Windows_NT)
EXE       := $(BINDIR)\\$(TARGET).exe
MKDIR_BIN := if not exist $(BINDIR) mkdir $(BINDIR)
MKDIR_OBJ := if not exist $(OBJDIR) mkdir $(OBJDIR)
RM        := -del /Q
NULL      := 2>NUL
else
EXE       := $(BINDIR)/$(TARGET)
MKDIR_BIN := mkdir -p $(BINDIR)
MKDIR_OBJ := mkdir -p $(OBJDIR)
RM        := rm -f
NULL      :=
endif

OBJS := $(OBJDIR)/swap_arrays.o $(OBJDIR)/main.o

.PHONY: all run clean

all: $(EXE)

$(BINDIR):
	$(MKDIR_BIN)

$(OBJDIR):
	$(MKDIR_OBJ)

$(OBJDIR)/swap_arrays.o $(OBJDIR)/swap_arrays.mod: swap_arrays.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -c swap_arrays.f90 -o $(OBJDIR)/swap_arrays.o

$(OBJDIR)/main.o: main.f90 $(OBJDIR)/swap_arrays.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c main.f90 -o $(OBJDIR)/main.o

$(EXE): $(BINDIR) $(OBJS)
	$(FC) $(LDFLAGS) -o $(EXE) $(OBJS)

run: $(EXE)
	$(EXE)

clean:
	$(RM) $(OBJDIR)\\*.o $(OBJDIR)\\*.mod $(EXE) $(NULL)
