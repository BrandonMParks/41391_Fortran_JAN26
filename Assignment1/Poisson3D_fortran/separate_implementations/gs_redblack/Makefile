TARGET_BASE := poisson3d

# -----------------------------------------------------------------------------
# Cross-platform settings (Linux/macOS vs Windows)
# -----------------------------------------------------------------------------
USE_WINDOWS_CMDS := 0
ifeq ($(OS),Windows_NT)
	# GNU Make on Windows sometimes reports SHELL=sh.exe, but coreutils like rm/mkdir -p
	# may not exist. Force cmd.exe so built-ins work consistently.
	SHELL := cmd.exe
	.SHELLFLAGS := /C

	USE_WINDOWS_CMDS := 1
	EXE := .exe
	RM := del /F /Q
	RMDIR := rmdir /S /Q
	MKDIR := mkdir
	NULL := NUL
else
	EXE :=
	RM := rm -f
	RMDIR := rm -rf
	MKDIR := mkdir -p
	NULL := /dev/null
endif

target := $(TARGET_BASE)$(EXE)

# Output folders
BUILDDIR := build
OBJDIR := $(BUILDDIR)/obj
MODDIR := $(BUILDDIR)/mod
BINDIR := bin

BINTARGET := $(BINDIR)/$(target)

# cmd.exe built-ins treat '/' as option prefix; use backslashes there.
ifeq ($(USE_WINDOWS_CMDS),1)
	CMD_BUILDDIR := build
	CMD_OBJDIR := build\obj
	CMD_MODDIR := build\mod
	CMD_BINDIR := bin
	CMD_BINTARGET := bin\$(target)
endif

.SUFFIXES:
.SUFFIXES: .f90 .o .c

CC = gcc
FC = gfortran
FFLAGS = -O3 -ffast-math -funroll-loops -fopenmp -J$(MODDIR) -I$(MODDIR)

# Optional shared build macros (profiling, etc.)
include macros.mk
FFLAGS += $(FFLAGS_PROFILE)
LDFLAGS ?=
LDFLAGS += $(LDFLAGS_PROFILE)

OBJS = $(OBJDIR)/poisson3d.o $(OBJDIR)/precision.o $(OBJDIR)/pde_init.o $(OBJDIR)/utilities.o $(OBJDIR)/poisson_methods.o $(OBJDIR)/write_output.o $(OBJDIR)/write_vtk.o

LIBS =

.PHONY: all
all: $(BINTARGET)

.PHONY: run
run: $(BINTARGET)
ifeq ($(USE_WINDOWS_CMDS),1)
	@set OMP_DISPLAY_ENV=verbose & set OMP_DYNAMIC=false & $(CMD_BINTARGET) $(RUN_ARGS)
else
	@OMP_DISPLAY_ENV=verbose OMP_PLACES=cores OMP_PROC_BIND=close OMP_DYNAMIC=false OMP_WAIT_POLICY=active GOMP_SPINCOUNT=1000000 $(BINTARGET) $(RUN_ARGS)
endif

.PHONY: info
info:
	@echo OS=$(OS)
	@echo SHELL=$(SHELL)
	@echo USE_WINDOWS_CMDS=$(USE_WINDOWS_CMDS)
	@echo PROFILE=$(PROFILE)
	@echo FFLAGS_PROFILE=$(FFLAGS_PROFILE)
	@echo LDFLAGS_PROFILE=$(LDFLAGS_PROFILE)

.PHONY: new
new: clean $(BINTARGET)

.PHONY: clean realclean
clean:

ifeq ($(USE_WINDOWS_CMDS),1)
	@if exist "$(CMD_OBJDIR)\*.o" $(RM) "$(CMD_OBJDIR)\*.o" >$(NULL) 2>&1
	@if exist "$(CMD_MODDIR)\*.mod" $(RM) "$(CMD_MODDIR)\*.mod" >$(NULL) 2>&1
	@if exist "$(CMD_BINTARGET)" $(RM) "$(CMD_BINTARGET)" >$(NULL) 2>&1
else
	@$(RM) $(OBJS) $(MODDIR)/*.mod
	@$(RM) $(BINTARGET)
endif

# realclean is identical to clean here so folders are preserved
realclean: clean
# --- Old folder-deleting realclean recipe (disabled) ---
# realclean: clean
# ifeq ($(USE_WINDOWS_CMDS),1)
# 	@if exist "$(CMD_BUILDDIR)" $(RMDIR) "$(CMD_BUILDDIR)" >$(NULL) 2>&1
# 	@if exist "$(CMD_BINDIR)" $(RMDIR) "$(CMD_BINDIR)" >$(NULL) 2>&1
# else
# 	@$(RMDIR) $(BUILDDIR)
# 	@$(RMDIR) $(BINDIR)
# endif

# linking: the target depends on the objects
$(BINTARGET): $(OBJS) | $(BINDIR)
	$(FC) $(FFLAGS) $(LDFLAGS) $(OBJS) -o $@

$(OBJDIR)/%.o: %.f90 | $(OBJDIR) $(MODDIR)
	$(FC) -c $(FFLAGS) $< -o $@

$(OBJDIR)/%.o: %.c | $(OBJDIR)
	$(CC) -c $< -o $@

$(OBJDIR):
ifeq ($(USE_WINDOWS_CMDS),1)
	@if not exist "$(CMD_OBJDIR)" $(MKDIR) "$(CMD_OBJDIR)" >$(NULL) 2>&1
else
	@$(MKDIR) $(OBJDIR)
endif

$(MODDIR):
ifeq ($(USE_WINDOWS_CMDS),1)
	@if not exist "$(CMD_MODDIR)" $(MKDIR) "$(CMD_MODDIR)" >$(NULL) 2>&1
else
	@$(MKDIR) $(MODDIR)
endif

$(BINDIR):
ifeq ($(USE_WINDOWS_CMDS),1)
	@if not exist "$(CMD_BINDIR)" $(MKDIR) "$(CMD_BINDIR)" >$(NULL) 2>&1
else
	@$(MKDIR) $(BINDIR)
endif

# dependencies:
$(OBJDIR)/poisson3d.o: $(OBJDIR)/precision.o $(OBJDIR)/utilities.o $(OBJDIR)/poisson_methods.o $(OBJDIR)/write_output.o $(OBJDIR)/write_vtk.o
$(OBJDIR)/pde_init.o: $(OBJDIR)/precision.o
$(OBJDIR)/utilities.o: $(OBJDIR)/precision.o
$(OBJDIR)/poisson_methods.o: $(OBJDIR)/precision.o $(OBJDIR)/pde_init.o

$(OBJDIR)/write_output.o: $(OBJDIR)/precision.o

# -----------------------------------------------------------------------------
# Profiling helpers
# -----------------------------------------------------------------------------

# Command-line args for running the program from Make.
RUN_ARGS ?=

# Default profiling target.
.PHONY: profile profile-gprofng
profile: profile-gprofng

profile-gprofng:
ifeq ($(USE_WINDOWS_CMDS),1)
	@echo gprofng is not available in this Windows Makefile flow.
	@echo Run this target on a Linux machine where gprofng is installed.
else
	@command -v gprofng >/dev/null 2>&1 || { echo "gprofng not found in PATH"; exit 1; }
	@rm -rf gprofng_out.er profile_gprofng.er
	@$(MAKE) clean
	@$(MAKE) PROFILE=gprofng
	@gprofng collect app -o gprofng_out.er $(BINTARGET) $(RUN_ARGS)
	@gprofng display text gprofng_out.er > profile_gprofng.er
	@echo Wrote profile_gprofng.er
endif


