#================================================================#
#  LaGriT Main Library Creation
# -------------------------------------------------------------- #
#  
#  Targets:
#    - lib : build the LaGriT src library
#    - clean : remove all object and mod files
#    - clobber : remove all object and mod files *and* library
#
#  Variables:
#    - WITHEXODUS : run with WITHEXODUS=1 to build with Exodus support
#    - DEBUG : run with DEBUG=1 to build with debug symbols
#    - CC, FC : change to use other compilers
#================================================================#
#
#  TODO:
#    - Add support for 32 bit machines


CC := gcc
FC := gfortran
FC90 := $(FC)
OBJDIR := objects/
WITHEXODUS := 1
DEBUG := 0
LIB := lg_main_lib.a

# Detect 64 or 32 bit arch; detect OS name
BIT_SIZE := $(shell getconf LONG_BIT)
OPSYS := $(shell uname -s)
EXO_INCLUDE_DIR := $(shell pwd)/../seacas/include

# Generate compiler flags and append OS-specific properties
CFLAGS := -m64
FFLAGS := -fcray-pointer -fdefault-integer-8 -m64

ifeq ($(OPSYS),Darwin)
	FFLAGS += -arch x86_64 -Dmacx64
	CFLAGS += -arch x86_64 -Dmacx64
else ifeq ($(OPSYS),Linux)
	FFLAGS += -Dlinx64
	CFLAGS += -Dlinx64
else ifeq ($(findstring CYGWIN_NT,$(OPSYS)),CYGWIN_NT)
	LINKERFLAGS += -Dwin64
	BUILDFLAGS  += -Dwin64
endif

ifeq ($(DEBUG),1)
	FFLAGS += -g -fbacktrace -ffpe-trap=invalid,zero,overflow
	CFLAGS += -g 
else
	FFLAGS += -O
	CFLAGS += -O
endif

FF90FLAGS = $(FFLAGS)

# Configure unique flags for dumpexodusII.f
# -cpp tells compiler to respect C-style #ifdef blocks

EXOFLAGS := -cpp
ifneq ($(WITHEXODUS),1)
	EXOFLAGS += -DNOEXODUS
endif


##########################################################################
# All Source files in directory are processed based on extension name. 
# ------------------------------------------------------------------

F77_OBJS := $(patsubst %.f,$(OBJDIR)%.o,$(wildcard *.f))
F77C_OBJS := $(patsubst %.F,$(OBJDIR)%.o,$(wildcard *.F))
F90_OBJS := $(patsubst %.f90,$(OBJDIR)%.o,$(wildcard *.f90))
CXX_OBJS := $(patsubst %.cpp,$(OBJDIR)%.o,$(wildcard *.cpp))


# If WITHEXODUS != true, remove C files from compilation that reference 'exodusII.h'
# This is a replacement to the old technique of: echo "" > exo_init_ext.c

ifneq ($(WITHEXODUS),1)
	C_OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(filter-out $(wildcard exo_*.c),$(wildcard *.c)))
else
	C_OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(wildcard *.c))
	CFLAGS += -I$(EXO_INCLUDE_DIR)
	FFLAGS += -I$(EXO_INCLUDE_DIR)
	FF90FLAGS += -I$(EXO_INCLUDE_DIR)
endif


##########################################################################
# action targets
# the first target in the file will run if make is not given a target
# ------------------------------------------------------------------

.PHONY: lib clean clobber help list
lib : lagrit.h $(OBJDIR) $(F90_OBJS) $(F77_OBJS) $(F77C_OBJS) $(C_OBJS) $(CXX_OBJS) 
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(F77C_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(CXX_OBJS)
	ar rcu $(LIB) $(F90_OBJS)
	@echo "Done : target = $@ ; lib = $(LIB)"

clean:
	rm -f $(OBJDIR)*.o $(OBJDIR)*.mod

clobber:
	-rm -f $(OBJDIR)*.o $(OBJDIR)*.mod
	-rm -fr objects_* *.a 


##########################################################################
# Dependencies
# In general define order: .mod (.o of .mod) .o and .f90
# Create end write to Makefile.depends (semi automatic)
#   head -1 Makefile.depends > ! makefile.tmp
#   makedepend -fmakefile.tmp *.f
#   makedepend -fmakefile.tmp *.c
#   edit to add OBJDIR in front of each .o name
#   mv makefile.tmp Makefile.depends
#
#   we prefer to find and copy the machine relevant 
#   lagrit.h or machine.h
#   this is built in a higher-level makefilem but can be copied
#   from one of the existing lagrit_*.h files in this directory
# ------------------------------------------------------------------
$(OBJDIR)%.o : %.f 
	$(FC) $(FFLAGS)  -c -o $@ $<

$(OBJDIR)%.o : %.F 
	$(FC) $(FFLAGS)  -c -o $@ $<

$(OBJDIR)%.o : %.c
	$(CC) $(CFLAGS)  -c -o $@ $<

$(OBJDIR)%.o : %.cpp
	$(CXX) $(CFLAGS) -c -o $@ $<

.SUFFIXES: .f90 .mod
$(F90_OBJS): $(OBJDIR)%.o: %.f90 ColoredGraphModule.mod GraphModule.mod LowPassFilterModule.mod
	$(FC90) $(FF90FLAGS) -c $< -o $@

%.mod : %.f90
	$(FC) $(FFLAGS) -c $< -o $(OBJDIR)$@

$(OBJDIR)dumpexodusII.o : dumpexodusII.f
	$(FC) $(FFLAGS) $(EXOFLAGS) -c -o $@ $<

$(OBJDIR)tempgable.o : tempgable.f
	$(FC) $(FFLAGS) -std=legacy -c -o $@ $<

$(OBJDIR) :; mkdir $(OBJDIR)

include Makefile.depends
