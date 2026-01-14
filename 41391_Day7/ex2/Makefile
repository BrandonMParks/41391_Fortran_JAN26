# === Project build entrypoint ===
# Commands:
#   make        Build the executable
#   make run    Build (if needed) and run
#   make clean  Delete build artifacts
# Output:
#   Windows -> bin\main.exe
#   Unix    -> bin/main

# === Toolchain settings ===
# Purpose: configure how compilation and linking are performed.
# - FC: Fortran compiler
# - FFLAGS: compiler flags used when building each .o (optimization/warnings)
# - LDFLAGS: linker flags used when creating the final executable
FC      := gfortran
FFLAGS  := -O3 -Wall -ffast-math -fopenmp
LDFLAGS := -fopenmp

# === Output naming/location ===
# Purpose: choose where the final executable goes and what it is called.
# - BINDIR: output directory
# - TARGET: base executable name (without .exe)
BINDIR  := bin
OBJDIR  := build
TARGET  := main

# === OS-specific command wiring ===
# Purpose: make the same targets work on Windows and on Unix-like shells.
# - EXE: executable path format differs (\ vs /)
# - MKDIR_P/RM/NULL: mkdir/delete/suppress-error idioms differ
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
# Purpose: list the compiled object files that will be linked into $(EXE).
# Note: Fortran modules must be compiled before files that USE them.
OBJS := $(OBJDIR)/precision.o $(OBJDIR)/utilities.o $(OBJDIR)/integrands.o $(OBJDIR)/main.o

# === Declared targets ===
# Purpose: these target names are actions, not files on disk.
.PHONY: all run clean

# === Default build target ===
# Purpose: this is what runs when you type `make` with no arguments.
all: $(EXE)

# === Output directory rule ===
# Purpose: ensure $(BINDIR) exists before we try to write $(EXE) into it.
$(BINDIR):
	$(MKDIR_BIN)

# === Build artifact directory rule ===
# Purpose: keep .o and .mod files out of the project root.
$(OBJDIR):
	$(MKDIR_OBJ)

# === Compile rules (source -> object + module files) ===
# Purpose: compile each .f90 into a .o, and produce any .mod files for modules.
# Dependency idea:
# - A file defining MODULE X produces X.mod
# - Any file that USEs module X must wait for X.mod to exist
precision.o precision.mod: $(OBJDIR)/precision.o $(OBJDIR)/precision.mod

$(OBJDIR)/precision.o $(OBJDIR)/precision.mod: precision.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -c precision.f90 -o $(OBJDIR)/precision.o


utilities.o utilities.mod: $(OBJDIR)/utilities.o $(OBJDIR)/utilities.mod

$(OBJDIR)/utilities.o $(OBJDIR)/utilities.mod: utilities.f90 $(OBJDIR)/precision.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c utilities.f90 -o $(OBJDIR)/utilities.o

integrands.o integrands.mod: $(OBJDIR)/integrands.o $(OBJDIR)/integrands.mod

$(OBJDIR)/integrands.o $(OBJDIR)/integrands.mod: integrands.f90 $(OBJDIR)/precision.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c integrands.f90 -o $(OBJDIR)/integrands.o

main.o: $(OBJDIR)/main.o

$(OBJDIR)/main.o: main.f90 $(OBJDIR)/precision.mod $(OBJDIR)/utilities.mod $(OBJDIR)/integrands.mod | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c main.f90 -o $(OBJDIR)/main.o

# === Link rule (objects -> executable) ===
# Purpose: combine object files into the final executable.
$(EXE): $(BINDIR) $(OBJS)
	$(FC) $(LDFLAGS) -o $(EXE) $(OBJS)

# === Convenience: build then run ===
# Purpose: run the executable, compiling/linking first if anything changed.
run: $(EXE)
	$(EXE)

# === Cleanup ===
# Purpose: delete compiler outputs so you can do a fresh rebuild.
clean:
	$(RM) $(OBJDIR)\*.o $(OBJDIR)\*.mod $(EXE) $(NULL)
