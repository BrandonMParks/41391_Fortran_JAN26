# === Project build entrypoint (4a) ===
# Commands:
#   make        Build the executable
#   make run    Build (if needed) and run
#   make clean  Delete build artifacts
# Output:
#   Windows -> bin\main_4a.exe
#   Unix    -> bin/main_4a

# === Toolchain settings ===
FC      := gfortran
FFLAGS  := -O3 -Wall
LDFLAGS :=

# === Output naming/location ===
# Keep *all* artifacts inside 4a/ so we don't interfere with the top-level build.
BINDIR  := bin
OBJDIR  := build
TARGET  := main_4a

# === OS-specific command wiring ===
ifeq ($(OS),Windows_NT)
EXE       := $(BINDIR)\$(TARGET).exe
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

# === Build inputs ===
OBJS := $(OBJDIR)/main.o

.PHONY: all run clean

all: $(EXE)

$(BINDIR):
	$(MKDIR_BIN)

$(OBJDIR):
	$(MKDIR_OBJ)

# === Compile rules (source -> object + module files) ===
$(OBJDIR)/main.o: main.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -J$(OBJDIR) -I$(OBJDIR) -c main.f90 -o $(OBJDIR)/main.o

# === Link rule (objects -> executable) ===
$(EXE): $(BINDIR) $(OBJS)
	$(FC) $(LDFLAGS) -o $(EXE) $(OBJS)

run: $(EXE)
	$(EXE)

clean:
	$(RM) $(OBJDIR)\*.o $(OBJDIR)\*.mod $(EXE) $(NULL)
