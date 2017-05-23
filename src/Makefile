#######################################################################
# GNU Make version 3.79.1 or later
# USAGE (make, gmake, or gnumake depending on system setup): 
# COMPILERS: gfortran 4.4 or higher and Absoft 10.1 or higher
#
# make [help or list] will print information without calling compiler
#
# NOTE: you can compile machine.h from machine_header.F
#       make mm2000.h
#       check that NBYTES_INT = 8 for 64bit
#       remove machine_header.F so it is not picked up in wildcards
#       or simply copy machine_m64.h to machine.h 
# 
# make OR make lib 
#    default creates or updates optimized *.o in OBJDIR_o 
#    default creates LIB_o.a in current directory 
# make COPT=-g lib 
#    creates or updates debug *.o in OBJDIR_g 
#    creates debug LIB_g.a in current directory
# make install 
#    creates or updates *.o in OBJDIR 
#    creates LIB_o.a in current directory 
#    creates LIB_o_DATETAG.a
#    moves LIB_o_DATETAG.a OBJDIR
# make clean
#    removes *.o in OBJDIR (recommended before install)
# make clobber
#    removes all OBJDIRs and .a files 
# command line make options include:
#    -d (debug) -p (print rules and variables) -v (version)
#
#######################################################
# The following OS have not been supported since 2010
# SunOS  flow 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Fire-880
# IRIX64 combust 6.5 10151453 IP27
# The following OS have not been supported since 2011
# Darwin Kernel Version 9.7.0  powerpc
#######################################################
#
# Modified Mar 2015 for Ubuntu and defaults to 64 bit
# Modified Oct 2013 to include Mac 10.6, 10.7, 10.8 with GCC gfortran 
# Modified Nov 2012 to work with Exodus II libraries (see dump/exo):
#   http://sourceforge.net/projects/exodusii/files/
#   Contact: Greg Sjaardema, Sandia National Laboratories, gdsjaar@sandia.gov
# Modified Aug 8 2011 to include work from Adam Cozzette and .cpp files 
# Modified Jan 1 2011 to include gfortran flags and .cpp files
# Modified July 7 2009 to update compiler flags for V2.100 release 
# Created Jan 7 2008 by tamiller@lanl.gov
#
##########################################################################
# user defined variables 
# if LIBDIR or OBJDIR do not exist, they are created
# platform and compile mode are used to form lib name and object directories
# ------------------------------------------------------------------
#
MAKEFILE  = Makefile
PROJ  = lagrit
ROOTDIR= $(shell pwd)
COMPILER = ABSOFT
COMPILER = GFORTRAN
#
# Use Enviornment variables if they exist 
ifndef LAGRIT_LIBS 
LIBDIR= $(shell pwd)
else
LIBDIR = $(LAGRIT_LIBS)
endif
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
NREL = $(shell uname -r )
SUFF  = ""
SUFFC = ""

# init to relevant info
ifneq ($(MOPT), 32)
MOPT = 64
endif

ifeq ($(COMPILER),GFORTRAN)
FC = gfortran
FC90 = gfortran 
CC= gcc 
CXX = g++
GCCV = $(shell gcc -dumpversion )
SUFFC = _gf$(GCCV)
SUFFC = _gcc
endif

ifeq ($(COMPILER), cygwin)
SUFFC = _cygwin
FC = /bin/gfortran
CC = /bin/gcc
CXX = /bin/c++
FC90 = /bin/gfortran

OSTAG = _cygwin

FFLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dwin64
FF90FLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dwin64
CFLAGS = -m64 -Dwin64
# -I/usr/include
endif

ifeq ($(COMPILER),ABSOFT)
FC = f77
FC90 = f90
CC = gcc
CXX = g++
SUFFC = _absoft
endif

#
# Linux OS ------------------------------
# RedHat RHEL or Ubuntu
# x86_64
#
ifeq ($(OPSYS), Linux)

OSTYP = $(shell lsb_release -si)
OSVER = $(shell lsb_release -sr)

ifeq ($(OSTYP), Ubuntu)
OSTAG = _ulin64
else
OSTAG = _rlin64
endif

FFLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64
FF90FLAGS = -fcray-pointer -fdefault-integer-8  -m64 -Dlinx64
CFLAGS = -m64 -Dlinx64

ifeq ($(MOPT), 32)
OSTAG = _lin32
CFLAGS = -m32 -Dlin
FFLAGS = -fcray-pointer -m32 
FF90FLAGS = -fcray-pointer -m32 
endif

# end linux options
endif

#
# Mac OSX ------------------------------
# Mac must be compiled by system since static versions
# of libSystem.dylib nor crt0.o are provided
# OS and name, release, and suffix:
# Darwin 13.x (Mac 10.9) Mavericks macOS13          
# Darwin 12.x (Mac 10.8) M. Lion   macOS12 with _GCC4.8
# Darwin 11.x (Mac 10.7) Lion      macOS11 with _GCC4.7
# Darwin 10.x (Mac 10.6) Snow L.   maci (32,64) with _gfort4.6
#
ifeq ($(OPSYS), Darwin)

$(info Detected - Darwin)

FC = gfortran
FC90 = gfortran
CC  = gcc 
CXX = g++
FFLAGS = -fcray-pointer -m32 -arch i386  
FF90FLAGS = -fcray-pointer -m32 -arch i386
CFLAGS = -m32 -arch i386 

# MAC OS Darwin 10.x Snow L. 32 bit  
ifeq ($(NPROC), i386)
$(info Detected - 32-bit architecture)
OSTAG = _maci32
endif

# MAC OS Darwin 10.x Snow L. 64 bit  
ifeq ($(MOPT), 64)
$(info Detected - 64-bit architecture)
OSTAG = _maci64
FFLAGS = -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64 
FF90FLAGS = -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64 
CFLAGS = -m64 -Dmacx64
endif

# MAC OS Darwin 11.x Lion 64 bit  
ifneq (, $(findstring 11. , $(NREL)) ) 
$(info Detected - macOS 11)
OSTAG = _macOS11
SUFFC = _gcc

FC = gfortran
FC90 = gfortran
CC = gcc
CXX = g++
#FC = /usr/gcc-4.7/bin/gfortran 
#FC90 = /usr/gcc-4.7/bin/gfortran
#CC= /usr/gcc-4.7/bin/gcc
#CXX = /usr/gcc-4.7/bin/g++

FFLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
FF90FLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
CFLAGS = -arch x86_64 -m64 -Dmacx64

# end OS 11
# MAC OS Darwin 12.x Mountain Lion 64 bit  
else ifneq (, $(findstring 12. , $(NREL)) ) 
$(info Detected - macOS 12)
OSTAG = _macOS12

SUFF = $(OSTAG)_g
SUFFC = _gcc

FC = gfortran
#FC = /usr/local/gfortran/bin/gfortran
#FC = /usr/gcc-4.8.1/bin/gfortran

FC90 = gfortran
#FC90 = /usr/local/gfortran/bin/gfortran
#FC90 = /usr/gcc-4.8.1/bin/gfortran

CC= gcc
#CC= /usr/local/bin/gcc
#CC= /usr/gcc-4.8.1/bin/gcc

CXX = g++
#CXX = /usr/local/bin/g++
#CXX = /usr/gcc-4.8.1/bin/g++

FFLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
FF90FLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
CFLAGS = -arch x86_64 -m64 -Dmacx64
endif
# end OS 12

# end Mac options
endif

# finish name extentsions with 
# SUFF _g or _o

#
ifndef COPT
COPT = -O
endif
#
# form the name suffix based on platform and flags
# default optimized 32 bit
SUFF = $(OSTAG)_o

# LIB is hardwired in for the sake of uniformity in the build process
# The gfortran command called after Make explicitly calls for this file
LIB   =  lagrit_ulin64_o_gcc.a

ifeq ($(COPT), -g)
SUFF = $(OSTAG)_g
LIB   =  lagrit_ulin64_g_gcc.a
endif
# 
OBJDIR = objects$(SUFF)$(SUFFC)/
#LIB   = $(PROJ)$(SUFF)$(SUFFC).a
PROG  = $(PROJ)$(SUFF)$(SUFFC)
LIBDATE   = $(PROJ)$(SUFF)$(DATETAG)$(SUFFC).a
PROGDATE  = $(PROJ)$(SUFF)$(DATETAG)$(SUFFC)
MODDIR = $(OBJDIR)$(SUFFC)
MODDIR = $(OBJDIR)
# 
##########################################################################
# All Source files in directory are processed based on extension name. 
# ------------------------------------------------------------------
#
F77_OBJS := $(patsubst %.f,$(OBJDIR)%.o,$(wildcard *.f))
#
C_OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(wildcard *.c))
#
CXX_OBJS := $(patsubst %.cpp,$(OBJDIR)%.o,$(wildcard *.cpp))
#
F77C_OBJS := $(patsubst %.F,$(OBJDIR)%.o,$(wildcard *.F))
#
F90_OBJS := $(patsubst %.f90,$(OBJDIR)%.o,$(wildcard *.f90))
#
##########################################################################
# action targets
# the first target in the file will run if make is not given a target
# ------------------------------------------------------------------
#
.PHONY: lib install clean clobber help list
lib : lagrit.h $(OBJDIR) $(F90_OBJS) $(F77_OBJS) $(F77C_OBJS) $(C_OBJS) $(CXX_OBJS) 
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(F77C_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(CXX_OBJS)
	ar rcu $(LIB) $(F90_OBJS)
	head -3 lagrit.h
	@echo "Done $@ $(LIB)"
#
install: lagrit.h $(OBJDIR) $(F90_OBJS) $(F77_OBJS) $(F77C_OBJS) $(C_OBJS) $(CXX_OBJS) $(LIBDIR) 
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(F77C_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(CXX_OBJS)
	ar rcu $(LIB) $(F90_OBJS)
	cp -p $(LIB) $(LIBDIR)
	cp -p $(LIB) $(LIBDATE)
	cp -p $(LIBDATE) $(LIBDIR)
	mv $(LIBDATE) $(OBJDIR)
	head -3 lagrit.h
	@echo "Done $@ $(LIB) $(LIBDIR)"
#
clean:;	rm -f $(OBJDIR)*.o  $(OBJDIR)*.mod
	rm -f *.o *.mod
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
	@echo "make or gmake" 
	@echo "make lib"
	@echo "   Creates or Updates LIB and .o in OBJDIR" 
	@echo "make COPT=-g lib" 
	@echo "   Creates or Updates LIB and .o in OBJDIR" 
	@echo "make COPT=-g MOPT=64 lib" 
	@echo "   Creates 64bit vers LIB and .o in OBJDIR" 
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
	@echo $(ROOTDIR)
	@echo "List F77 SOURCE FILES (F77_OBJS):"
	@echo $(F77_OBJS)
	@echo "List F77 C SOURCE FILES (F77C_OBJS):"
	@echo $(F77C_OBJS)
	@echo "List C SOURCE FILES (C_OBJS):"
	@echo $(C_OBJS)
	@echo "List C++ SOURCE FILES (CXX_OBJS):"
	@echo $(CXX_OBJS)
	@echo "List F90 SOURCE FILES (F90_OBJS):"
	@echo $(F90_OBJS)
	@echo "List LIB: $(LIB) " 
	@echo "List ROOTDIR: $(ROOTDIR)" 
	@echo "List OBJDIR: $(OBJDIR)" 
	@echo "List MODDIR: $(MODDIR)" 
ifndef LAGRIT_LIBS
	@echo "LAGRIT_LIBS undefined, install in: "$(LIBDIR)
else
	@echo "LAGRIT_LIBS defined, install in: "$(LIBDIR)
endif
	@echo "List FC Compilers: $(FC)"
	@echo "List FC90 Compilers: $(FC90)"
	@echo "List C  Compilers: $(CC)"
	@echo "List CFlags: $(FFLAGS) $(COPT),  $(FF90FLAGS) $(COPT), $(CFLAGS) $(COPT)"
	@echo "List OS: $(OPSYS) OS TAG: $(OSTAG) NAME SUFFIX $(SUFF)$(SUFFC)"
	cat lagrit$(OSTAG).h 
	@echo " "
#
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
#   commented out due to compiler and platform incompatibilities
#   instead, use copy before using this Makefile
#      cp lagrit_lin64.h lagrit.h
#      cp machine_m64.h machine.h
# ------------------------------------------------------------------
$(OBJDIR)%.o : %.f 
	cp lagrit$(OSTAG).h lagrit.h
	$(FC) $(COPT) $(FFLAGS)  -c -o $@ $<

#lagrit.h : 
#	@echo "using lagrit$(OSTAG).h"
#	chmod +w lagrit.h 
#	cp lagrit$(OSTAG).h lagrit.h
#	cat lagrit.h

.SUFFIXES: .f90 .mod
$(F90_OBJS): $(OBJDIR)%.o: %.f90 ColoredGraphModule.mod GraphModule.mod LowPassFilterModule.mod
	$(FC90) $(COPT) $(FF90FLAGS) -c $< -o $@

$(OBJDIR)%.o : %.c
	$(CC) $(COPT) $(CFLAGS)  -c -o $@ $<

$(OBJDIR)%.o : %.cpp
	$(CXX) $(COPT) $(CFLAGS) -c -o $@ $<

%.mod : %.f90
	$(FC) $(COPT) $(FFLAGS) -c $< -o $(OBJDIR)$@


# this is commented out due to platform issues
# cp machine_m64.h machine.h or machine_m32.h 
#machine.h : machine_header.h
#	@echo "--------------------------------------------------"
#	@echo "Remove and re-make machine.h from machine_header.F"
#	cp -p machine_header.h machine_header.F
#	$(CC) -E $(CFLAGS) machine_header.F -o machine.h
#	cp -p machine.h $(OBJDIR)
#	-rm -f machine_header.F
#	@echo "--------------------------------------------------"

$(OBJDIR)%.o : %.F 
	cp lagrit$(OSTAG).h lagrit.h
	$(FC) $(COPT) $(FFLAGS)  -c -o $@ $<

$(LIBDIR) :; mkdir $(LIBDIR)

$(OBJDIR) :; mkdir $(OBJDIR)

include Makefile.depends

# DO NOT DELETE
