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
#
#  TODO:
#    - Add support for 32 bit machines

CC = cl.exe
#icc
FC = ifort
LIB = lg_util.dll
DEBUG = 0

# Detect 64 or 32 bit
BIT_SIZE = 64
OPSYS = CYGWIN_NT

# Generate compiler flags and append OS-specific properties
CFLAGS = -O -m64 -Dwin64
FFLAGS = -O -safe-cray-ptr -integer-size 64 -Dwin64 -m64

# -safe-cray-ptr -> Qsafe-cray-ptr on Windows

#ifeq ($(DEBUG),1)
#	FFLAGS += -g -fbacktrace -ffpe-trap=invalid,zero,overflow
#	CFLAGS += -g 
#else
#	FFLAGS += -O
#	CFLAGS += -O
#endif


##########################################################################
# Source  
# ------------------------------------------------------------------

F77_SOURCES = *.f
C_SOURCES = *.c

F77_OBJS = $(F77_SOURCES:.f=.obj)
#$(patsubst %.f,$(OBJDIR)%.o,$(wildcard *.f))
#C_OBJS = $(patsubst %.c,$(OBJDIR)%.o,$(wildcard *.c))
C_OBJS = $(C_SOURCES:.c=.obj)

F77C_OBJS = mm2000.obj
#$(OBJDIR)mm2000.o


##########################################################################
# action targets
# the first target in the file will run if make is not given a target
# Note OBJDIR is a dependency that will be created if needed
# ------------------------------------------------------------------

lib : $(OBJDIR) mm2000.h $(F77_OBJS) $(C_OBJS) $(F77C_OBJS)
	lib /OUT:$(LIB) $(F77_OBJS) $(C_OBJS) $(F77C_OBJS)
	@echo "Done: target = $@, fileout = $(LIB)"

clean:
	del /F *.obj *.mod

clobber:
	del /F *.obj *.mod $(LIB)


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

%.obj : %.f
	$(FC) $(FFLAGS) -c -o $@ $<

%.obj : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

<<<<<<< HEAD
mm2000.h : mm2000_header.F 
	copy mm2000_m$(BIT_SIZE).h mm2000.h
=======
mm2000.h : mm2000_header.F
	cp -p mm2000_m$(BIT_SIZE).h mm2000.h

$(OBJDIR) :; mkdir $(OBJDIR)
>>>>>>> 05fbe7f5259e39732c405c929fdf5b50148250a1

mm2000.obj : mm2000.h mm2000.F
	$(FC) $(FFLAGS) -c mm2000.F -o mm2000.obj

include Makefile.depends