# Shared build macros (profiling)
#
# Usage:
#   make PROFILE=gprofng
#
# PROFILE modes:
#   none    : default, no profiling flags
#   gprofng : enable symbols for gprofng analysis

PROFILE ?= none

FFLAGS_PROFILE :=
LDFLAGS_PROFILE :=

ifeq ($(PROFILE),gprofng)
	FFLAGS_PROFILE += -g
endif
