#================================================================#
#  LaGriT lg_util Library Creation
# -------------------------------------------------------------- #
#  
#  Targets:
#    - lib : build the LaGriT lg_util library
#    - clean : remove all object and mod files
#    - clobber : remove all object and mod files *and* library
#
#  Variables:
#    - DEBUG : run with DEBUG=1 to build with debug symbols
#    - CC, FC : change to use other compilers
#================================================================#
#  Changelog:
#    - 12/03/18 (DRL) : Refactoring to make code easier to read,
#                       bug fixes for macOS clang compilers
#
#  TODO:
#    - Add support for 32 bit machines

CC := gcc
FC := gfortran
OBJDIR := objects/
LIB := lg_util.a
DEBUG := 0

# Detect 64 or 32 bit
MOPT := $(shell getconf LONG_BIT)
OPSYS := $(shell uname -s)

# Generate compiler flags and append OS-specific properties
CFLAGS := -m64
FFLAGS := -fcray-pointer -fdefault-integer-8

ifeq ($(OPSYS),Darwin)
	FFLAGS += -Dmacx64 -m64
	CFLAGS += -Dmacx64
else ifeq ($(OPSYS),Linux)
	FFLAGS += -Dlinx64
	CFLAGS += -Dlinx64
endif

ifeq ($(DEBUG),1)
	FFLAGS += -g
	CFLAGS += -g
else
	FFLAGS += -O
	CFLAGS += -O
endif


##########################################################################
# Source  
# ------------------------------------------------------------------

F77_OBJS := $(patsubst %.f,$(OBJDIR)%.o,$(wildcard *.f))
C_OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(wildcard *.c))
F77C_OBJS = $(OBJDIR)mm2000.o


##########################################################################
# action targets
# the first target in the file will run if make is not given a target
# Note OBJDIR is a dependency that will be created if needed
# ------------------------------------------------------------------

lib : $(OBJDIR) mm2000.h ${F77_OBJS} $(C_OBJS) $(F77C_OBJS)
	ar rcu $(LIB) $(F77_OBJS) $(C_OBJS) $(F77C_OBJS)
	@echo "Done: target = $@, fileout = $(LIB)"

clean:
	rm -f $(OBJDIR)*.o $(OBJDIR)*.mod

clobber:
	rm -f *.o *.mod lg_util.a


##########################################################################
# Dependencies
# In general define order: .mod (.o of .mod) .o and .f90
# Note there are no f90 files in this util library. 
# Create end write to Makefile.depends (semi automatic)
#   head -1 Makefile.depends > ! makefile.tmp
#   makedepend -fmakefile.tmp *.f
#   makedepend -fmakefile.tmp *.c
#   edit to add OBJDIR in front of each .o name
#   mv makefile.tmp Makefile.depends
# ------------------------------------------------------------------

$(OBJDIR)%.o : %.f
	$(FC) $(FFLAGS) -c -o $@ $<

$(OBJDIR)%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

mm2000.h : mm2000_header.F 
	cp -p mm2000_m$(MOPT).h mm2000.h

$(OBJDIR) :; mkdir $(OBJDIR)

$(OBJDIR)mm2000.o : mm2000.h mm2000.F
	$(FC) $(FFLAGS) -c mm2000.F -o $(OBJDIR)mm2000.o  

include Makefile.depends