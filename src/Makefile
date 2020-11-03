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


CC = cl.exe
CXX = cl.exe
FC = ifort
FC90 = $(FC)
OBJDIR = objects/
WITHEXODUS = 0
DEBUG = 0
LIB = lg_main_lib.a

# Detect 64 or 32 bit arch; detect OS name
BIT_SIZE = 64
OPSYS = WIN_NT
EXO_INCLUDE_DIR =

# Generate compiler flags and append OS-specific properties
CFLAGS = -O -Dwin64 -m64
FFLAGS = -O -Dwin64 -m64 -safe-cray-ptr -integer-size 64 #-fcray-pointer -fdefault-integer-8 -m64

FF90FLAGS = $(FFLAGS)

# Configure unique flags for dumpexodusII.f
# -cpp tells compiler to respect C-style #ifdef blocks

EXOFLAGS = -cpp -DNOEXODUS


##########################################################################
# All Source files in directory are processed based on extension name. 
# ------------------------------------------------------------------
F77_SRC = *.f
F77C_SRC = *.F
F90_SRC = *.f90
C_SRC = *.c
CXX_SRC = *.cpp

F77_OBJS = $(F77_SRC:.f=.o)
F77C_OBJS = $(F77C_SRC:.F=.o)
F90_OBJS = $(F90_SRC:.f90=.o)
C_OBJS = $(C_SRC:.c=.o)
CXX_OBJS = $(CXX_SRC:.cpp=.o)

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
