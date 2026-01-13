# === ex2: Matrix-vector multiply (OpenMP) ===
# Commands:
#   make        Build the executable
#   make run    Build (if needed) and run
#   make clean  Delete build artifacts
# Output:
#   Windows -> bin\main.exe
#   Unix    -> bin/main

# === Toolchain settings ===
FC      := gfortran
FFLAGS  := -O3 -Wall -fopenmp
LDFLAGS := -fopenmp

# === Output naming/location ===
BINDIR  := bin
OBJDIR  := build
TARGET  := main

# === OS-specific command wiring ===
ifeq ($(OS),Windows_NT)
EXE     := $(BINDIR)\$(TARGET).exe
MKDIR_BIN := if not exist $(BINDIR) mkdir $(BINDIR)
MKDIR_OBJ := if not exist $(OBJDIR) mkdir $(OBJDIR)
RM      := -del /Q
NULL    := 2>NUL
else
EXE     := $(BINDIR)/$(TARGET)
MKDIR_BIN := mkdir -p $(BINDIR)
MKDIR_OBJ := mkdir -p $(OBJDIR)
RM      := rm -f
NULL    :=
endif

# === Build inputs ===
OBJS := $(OBJDIR)/precision.o $(OBJDIR)/utilities.o $(OBJDIR)/matvec_kernels.o $(OBJDIR)/main.o

.PHONY: all run clean

all: $(EXE)

$(BINDIR):
	$(MKDIR_BIN)

$(OBJDIR):
	$(MKDIR_OBJ)

precision.o precision.mod: $(OBJDIR)/precision.o $(OBJDIR)/precision.mod

$(OBJDIR)/precision.o $(OBJDIR)/precision.mod: precision.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -c precision.f90 -o $(OBJDIR)/precision.o

utilities.o utilities.mod: $(OBJDIR)/utilities.o $(OBJDIR)/utilities.mod

$(OBJDIR)/utilities.o $(OBJDIR)/utilities.mod: utilities.f90 $(OBJDIR)/precision.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c utilities.f90 -o $(OBJDIR)/utilities.o

matvec_kernels.o matvec_kernels.mod: $(OBJDIR)/matvec_kernels.o $(OBJDIR)/matvec_kernels.mod

$(OBJDIR)/matvec_kernels.o $(OBJDIR)/matvec_kernels.mod: matvec_kernels.f90 $(OBJDIR)/precision.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c matvec_kernels.f90 -o $(OBJDIR)/matvec_kernels.o

main.o: $(OBJDIR)/main.o

$(OBJDIR)/main.o: main.f90 $(OBJDIR)/precision.mod $(OBJDIR)/utilities.mod $(OBJDIR)/matvec_kernels.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c main.f90 -o $(OBJDIR)/main.o

$(EXE): $(BINDIR) $(OBJS)
	$(FC) $(LDFLAGS) -o $(EXE) $(OBJS)

run: $(EXE)
	$(EXE)

clean:
	$(RM) $(OBJDIR)\*.o $(OBJDIR)\*.mod $(EXE) $(NULL)
