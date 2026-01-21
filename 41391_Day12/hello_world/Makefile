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
# - FC: Fortran compiler (MPI wrapper recommended)
# - FFLAGS: compiler flags used when building each .o (optimization/warnings)
# - LDFLAGS: linker flags used when creating the final executable
#
# Override if needed:
#   make FC=mpif90
# Note: GNU Make sets a built-in default FC=f77, so we set FC explicitly here.
# MSYS2 MPICH provides mpifort (and sometimes mpif90). mpifort is the most common.
FC      = mpif90 # mpifort
FFLAGS  := -O3 -Wall
LDFLAGS :=

# === Output naming/location ===
# Purpose: choose where the final executable goes and what it is called.
# - BINDIR: output directory
# - TARGET: base executable name (without .exe)
BINDIR  := bin
OBJDIR  := build
TARGET  := main

# === OS-specific command wiring ===
# Purpose: make the same targets work on Windows and on Unix-like shells.
# Note: MSYS2 provides a Unix-like shell on Windows; detect that via MSYSTEM.
ifneq ($(MSYSTEM),)
EXE     := $(BINDIR)/$(TARGET).exe
MKDIR_BIN := mkdir -p $(BINDIR)
MKDIR_OBJ := mkdir -p $(OBJDIR)
RM      := rm -f
NULL    :=
OBJ_GLOB := $(OBJDIR)/*.o
MOD_GLOB := $(OBJDIR)/*.mod
else ifeq ($(OS),Windows_NT)
EXE     := $(BINDIR)\$(TARGET).exe
MKDIR_BIN := if not exist $(BINDIR) mkdir $(BINDIR)
MKDIR_OBJ := if not exist $(OBJDIR) mkdir $(OBJDIR)
RM      := -del /Q
NULL    := 2>NUL
OBJ_GLOB := $(OBJDIR)\*.o
MOD_GLOB := $(OBJDIR)\*.mod
else
EXE     := $(BINDIR)/$(TARGET)
MKDIR_BIN := mkdir -p $(BINDIR)
MKDIR_OBJ := mkdir -p $(OBJDIR)
RM      := rm -f
NULL    :=
OBJ_GLOB := $(OBJDIR)/*.o
MOD_GLOB := $(OBJDIR)/*.mod
endif

# === Build inputs ===
# Purpose: list the compiled object files that will be linked into $(EXE).
# Note: Fortran modules must be compiled before files that USE them.
OBJS := $(OBJDIR)/precision.o $(OBJDIR)/main.o

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

precision.o precision.mod: $(OBJDIR)/precision.o $(OBJDIR)/precision.mod

$(OBJDIR)/precision.o $(OBJDIR)/precision.mod: precision.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -c precision.f90 -o $(OBJDIR)/precision.o

main.o: $(OBJDIR)/main.o

$(OBJDIR)/main.o: main.f90 $(OBJDIR)/precision.mod | $(OBJDIR)
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
	$(RM) $(OBJ_GLOB) $(MOD_GLOB) $(EXE) $(NULL)
