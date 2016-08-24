##########################################################################
# GNU Make version 3.79.1 or later
# USAGE (use make, gmake, or gnumake according to setup): 
# make list  (no action, write to screen only)
#    will write files and names used for this session and OS
#
# make OR make lib 
#    creates optimized for platform LIB_o.a and  OBJDIR_o 
# make MOPT=32 COPT=-g lib 
#    creates debug 32 bit for platform LIB_g.a and OBJDIR_g 
# make MOPT=64 lib 
# make lib 
#    sets 64 bit LIB64_o.a and OBJDIR64_o for platform 
# make MOPT=64 COPT=-g lib 
#    sets 64 bit LIB64_g.a and OBJDIR64_g for platform 
#    NOTE: make sure new mm2000.h is created from mm2000_header.F
#    This sets sizeof_int correctly to 8
#    copy mm2000_m64.h to mm2000.h to be explicit
#
# OBJDIR and LIB are formed by system query from make
# other compile flags are passed in from command line:
#
# make install 
#    creates or updates *.o in OBJDIR 
#    creates LIB_o.a in current directory 
#    creates LIB_o_DATETAG.a
#    copies both libs.a to LIBDIR
#    moves LIB_o_DATETAG.a OBJDIR
# make clean
#    removes *.o in OBJDIR (recommended before install)
# make clobber
#    removes all OBJDIRs and .a files 
# make help
#    list of targets and defined variables 
# make list
#    list of object files and variables 
# make options include:
#    -d (debug) -p (print rules and variables) -v (version)
#
# platform is detected by uname -s
# platform and compile mode are used to form lib name and object directories
# if LIBDIR or OBJDIR do not exist, they are created
# set compiler options
# COMPILER=ABSOFT or COMPILER=GFORTRAN
#
# Note: assumes Makefile and Makefile.depends
#
# Modified Mar 2015 by tamiller@lanl.gov to use 64bit as default
# Modified Mar 2015 by tamiller@lanl.gov to include Ubuntu 
# Modified Nov 2010 by tamiller@lanl.gov to use gfortran as default
# Created Jan 7 2008 by tamiller@lanl.gov
#
##########################################################################
# user defined variables
# ------------------------------------------------------------------
#
MAKEFILE  = Makefile
PROJ  = util
ROOTDIR= $(shell pwd)
LIBDIR= $(subst lg_util/src,lg_util/lib, $(ROOTDIR)) 
#
##########################################################################
#  auto variables (May also be set at command line, environment, or script) 
# ------------------------------------------------------------------
#
OPSYS = UNKNOWN 
OPSYS_FULL = $(shell uname -a )
OPSYS = $(shell uname -s )
DATETAG = $(shell date '+_%y%m%d')
NPROC = $(shell uname -p )
#
# set compiler and defaults
COMPILER = ABSOFT
COMPILER = GFORTRAN

ifneq ($(MOPT), 32)
MOPT = 64
endif

ifeq ($(COMPILER),GFORTRAN)
FC = gfortran
CC = gcc
endif

#
# Linux OS ------------------------------
ifeq ($(OPSYS), Linux)

OSTYP = $(shell lsb_release -si)
OSVER = $(shell lsb_release -sr)

ifeq ($(OSTYP), Ubuntu)
OSTAG = _ulin
else
OSTAG = _rlin
endif

FFLAGS = -f -m64 -i8 -Dlinx64 -YEXT_NAMES=ASIS -q -w 
FFLAGS = -fcray-pointer -fdefault-integer-8 -Dlinx64
CFLAGS = -m64 -Dlinx64

ifeq ($(MOPT), 32)
FFLAGS = -f -m32 -Dlin -YEXT_NAMES=ASIS -q -w 
FFLAGS = -fcray-pointer -fno-align-commons -m32 
FFLAGS = -fcray-pointer -m32 
CFLAGS = -m32 -Dlin
endif


FF90FLAGS = $FFLAGS 
endif

#
# Mac OSX ------------------------------
# Absoft9.5+ should not need flags
ifeq ($(OPSYS), Darwin)
OSTAG = _mac
FC = /Applications/Absoft9.5/bin/f77 
CC= /Applications/Absoft9.5/bin/acc 
CC = gcc
FFLAGS = -N113 -N90 -B19 -B100 -q -w
FFLAGS = -m32 -Dmac -YEXT_NAMES=ASIS -q 
CFLAGS = -m32 
#
# overwrite if this is intel mac
# these flags work for intel mac
ifeq ($(NPROC), i386)
OSTAG = _maci
FC = /Applications/Absoft10.2/bin/f77
FC90 = /Applications/Absoft10.2/bin/f90
CC= /Applications/Absoft10.2/bin/acc
CC = gcc
FFLAGS = -Dmac -m32 -q 
CFLAGS = -m32 -Dmac
FF90FLAGS = -m32 

ifeq ($(COMPILER),GFORTRAN)
FC = gfortran
FC90 = gfortran
CC= gcc
FFLAGS = -fcray-pointer -m32  
FF90FLAGS = -fcray-pointer -m32 

ifeq ($(MOPT), 64)
OSTAG = _maci64
FFLAGS = -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64 
FF90FLAGS = -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64 
CFLAGS = -m64 -Dmacx64
endif

endif


endif
endif
#
ifndef COPT
COPT = -O
endif
#
# form the name suffix based on platform and flags
# default optimized 32 bit
SUFF = $(OSTAG)_o
ifeq ($(MOPT), 64)
SUFF = $(OSTAG)64_o
endif
ifeq ($(COPT), -g)
SUFF = $(OSTAG)_g
ifeq ($(MOPT), 64)
SUFF = $(OSTAG)64_g
endif
endif
SUFFC =""
ifeq ($(COMPILER),GFORTRAN)
GCCV = $(shell gcc -dumpversion )
SUFFC = _gf$(GCCV)
endif
ifeq ($(COMPILER),ABSOFT)
SUFFC = _absoft
endif
# 
OBJDIR = objects$(SUFF)$(SUFFC)/
LIB   = $(PROJ)$(SUFF)$(SUFFC).a
PROG  = $(PROJ)$(SUFF)$(SUFFC)
LIBDATE   = $(PROJ)$(SUFF)$(DATETAG)$(SUFFC).a
PROGDATE  = $(PROJ)$(SUFF)$(DATETAG)$(SUFFC)
# 
##########################################################################
# Source  
# ------------------------------------------------------------------
#
F77_OBJS := $(patsubst %.f,$(OBJDIR)%.o,$(wildcard *.f))
#
C_OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(wildcard *.c))
#
F77C_OBJS = $(OBJDIR)mm2000.o
#
#
##########################################################################
# action targets
# the first target in the file will run if make is not given a target
# Note OBJDIR and LIBDIR are dependencies that will be created if needed
# ------------------------------------------------------------------
#
lib : $(OBJDIR) mm2000.h ${F77_OBJS} $(C_OBJS) $(F77C_OBJS)
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(F77C_OBJS)
	@echo "Done $@ $(LIB)"
#
install: $(OBJDIR) ${F77_OBJS} $(C_OBJS) $(F77C_OBJS) $(LIBDIR)
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(F77C_OBJS)
	cp -p $(LIB) $(LIBDIR)
	cp -p $(LIB) $(LIBDATE)
	cp -p $(LIBDATE) $(LIBDIR)
	mv $(LIBDATE) $(OBJDIR)
	@echo "Done $@ $(LIB) $(LIBDIR)"
#
clean:;   @rm -f $(OBJDIR)*.o mm2000.h
#
clobber:;  -rm -fr objects_* *.a 
#
##########################################################################
# targets for information reporting 
# ------------------------------------------------------------------
#
help:;  @echo " "
	@echo "Operating System Detected: $(OPSYS) "
	@echo "  $(OPSYS_FULL)"
	@echo " "
	@echo "USAGE: "
	@echo "make" 
	@echo "make lib"
	@echo "   Creates or Updates LIB and .o in OBJDIR" 
	@echo "make COPT=-g lib" 
	@echo "   Creates or Updates LIB and .o in OBJDIR" 
	@echo "make clean"
	@echo "   Remove .o files from OBJDIR"
	@echo "make clobber"
	@echo "   Remove all OBJDIR's and .a files"
	@echo "make install"
	@echo "   Update and Copy LIB and LIBDATE into LIBDIR"
	@echo "make list"
	@echo "   List all the files and variables"
	@echo " "
#
# ------------------------------------------------------------------
# list: list all files defined under makefile macros
# ------------------------------------------------------------------
list:;  
	@echo
	@echo "List OS: "$(OPSYS)" "$(OSTYP)
	@echo "List: "$(ROOTDIR)
	@echo "List F77 SOURCE FILES (F77_OBJS):"
	@echo $(F77_OBJS) $(F77C_OBJS)
	@echo "List C SOURCE FILES (C_OBJS):"
	@echo $(C_OBJS)
	@echo "List LIB: $(LIB) " 
	@echo "List ROOTDIR: $(ROOTDIR)" 
	@echo "List LIBDIR: $(LIBDIR)" 
	@echo "List OBJDIR: $(OBJDIR)" 
	@echo "List FC Compilers: $(FC)"
	@echo "List C  Compilers: $(CC)"
	@echo "List F Flags: "$(FFLAGS) 
	@echo "List C Flags: "$(CFLAGS)
	@echo " "
#
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
	$(FC) $(COPT) $(FFLAGS)  -c -o $@ $<

$(OBJDIR)%.o : %.c
	$(CC) $(COPT) $(CFLAGS)  -c -o $@ $<

# if compiler can not compile mm2000_header.F
# comment this section and copy mm2000_m32.h or m64.h to mm2000.h
mm2000.h : mm2000_header.F 
	@echo "Remove and re-make mm2000.h from mm2000_header.F"
	-rm -f mm2000.h
	$(CC) -E $(CFLAGS) mm2000_header.F -o mm2000.h 
	cp -p mm2000.h $(OBJDIR)

$(LIBDIR) :; mkdir $(LIBDIR)

$(OBJDIR) :; mkdir $(OBJDIR)

$(OBJDIR)mm2000.o : mm2000.h mm2000.F
	$(FC) $(COPT) $(FFLAGS)  -c mm2000.F -o $(OBJDIR)mm2000.o  

include Makefile.depends

