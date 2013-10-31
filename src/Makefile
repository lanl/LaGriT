#######################################################################
# GNU Make version 3.79.1 or later
# USAGE (make, gmake, or gnumake depending on system setup): 
# COMPILERS: gfortran 4.4 or higher and Absoft 10.1 or higher
#
# make [help or list] will print information without calling compiler
#
# for 64 bit compiles add MOPT=64 to make command
# the following will compile library with debug and 64 bit options
# make MOPT=64 COPT=-g lib
#
# NOTE: make sure machine.h has compiled from machine_header.F
#       correctly so that NBYTES_INT = 8
#       remove machine_header.F so it is not picked up in wildcards
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
#    copies both libs.a to LIBDIR
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
# Created Jan 7 2008 by tamiller@lanl.gov
# Modified July 7 2009 to update compiler flags for V2.100 release 
# Modified Jan 1 2011 to include gfortran flags and .cpp files
# Modified Aug 8 2011 to include work from Adam Cozzette and .cpp files 
# Modified Nov 2012 to work with new exodus libraries (see dump/exo):
#   http://sourceforge.net/projects/exodusii/files/
#     Exodus II 5.22b
#     HDF5 version 1.8.6
#     netcdf-4.1.3
#     Contact: Greg Sjaardema, Sandia National Laboratories, gdsjaar@sandia.gov
# Modified Oct 2013 to include Mac 10.6, 10.7, 10.8 with GCC gfortran 
#
##########################################################################
# user defined variables 
# if LIBDIR or OBJDIR do not exist, they are created
# platform and compile mode are used to form lib name and object directories
# uname -s and uname -p are used to detect machine
# Note: assumes path to sh is defined on system, otherwise define here
# shell = /bin/sh
# ------------------------------------------------------------------
#
MAKEFILE  = Makefile
PROJ  = lagrit
ROOTDIR= $(shell pwd)
# assumes current directory is lagrit/src
LIBDIR= $(subst lagrit/src,lagrit/lib, $(ROOTDIR)) 
BINDIR= $(subst lagrit/src,lagrit/bin, $(ROOTDIR)) 
#
##########################################################################
#  auto variables (May also be set at command line, environment, or script) 
# ------------------------------------------------------------------
#
COMPILER = ABSOFT
COMPILER = GFORTRAN
OPSYS = UNKNOWN 
OPSYS_FULL = $(shell uname -a )
OPSYS = $(shell uname -s )
DATETAG = $(shell date '+_%y%m%d')
NPROC = $(shell uname -p )
NREL = $(shell uname -r )

# init to relevant info
FC   = notset
FC90 = notset
CC   = notset
CXX  = notset
SUFF  = ""
SUFFC = ""
#
# Linux OS ------------------------------
# Linux rainier 2.6.9-55.0.2.ELlargesmp #1 SMP 2007
# x86_64
#
ifeq ($(OPSYS), Linux)

OSTAG = _lin32
CFLAGS = -m32 -Dlin
FFLAGS = -fcray-pointer -m32 
FF90FLAGS = -fcray-pointer -m32 


FC = gfortran
FC90 = gfortran 
CC= gcc 
CXX - g++

ifeq ($(MOPT), 64)
OSTAG = _lin64
OBJDIR = lin_objects_m64
FFLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64
FF90FLAGS = -fcray-pointer -fdefault-integer-8  -m64 -Dlinx64
CFLAGS = -m64 -Dlinx64
endif

# compile gfortran on linux redhat
SUFFC = _gf4.5

# end linux options
endif

#
# Mac OSX ------------------------------
# Mac must be compiled by system since static versions
# of libSystem.dylib nor crt0.o are provided
# OS and name, release, and suffix:
# Darwin 13.x (Mac 10.9) Mavericks macOS13          
# Darwin 12.x (Mac 10.8) M. Lion   macOS12 with _GCC4.8
# Darwin 11.x (Mac 10.7) Lion      macOS11 with _GCC4.8
# Darwin 10.x (Mac 10.6) Snow L.   maci (32,64) with _gfort4.6
#
ifeq ($(OPSYS), Darwin)

FC = gfortran
FC90 = gfortran
CC  = gcc 
CXX = g++
FFLAGS = -fcray-pointer -m32 -arch i386  
FF90FLAGS = -fcray-pointer -m32 -arch i386
CFLAGS = -m32 -arch i386 

# MAC OS Darwin 10.x Snow L. 32 bit  
ifeq ($(NPROC), i386)
OSTAG = _maci32
OBJDIR = objects_maci/
endif

# MAC OS Darwin 10.x Snow L. 64 bit  
ifeq ($(MOPT), 64)
OSTAG = _maci64
OBJDIR = lin_objects_maci64/
FFLAGS = -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64 
FF90FLAGS = -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64 
CFLAGS = -m64 -Dmacx64
endif

# MAC OS Darwin 11.x Lion 64 bit  
ifneq (, $(findstring 11. , $(NREL)) ) 
OSTAG = _macOS11
SUFFC = _GCC4.7

FC = /usr/local/bin/gfortran
FC90 = /usr/local/bin/gfortran
CC= /usr/local/bin/gcc
CXX = /usr/local/bin/g++

FFLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
FF90FLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
CFLAGS = -arch x86_64 -m64 -Dmacx64

# end OS 11
# MAC OS Darwin 12.x Mountain Lion 64 bit  
else ifneq (, $(findstring 12. , $(NREL)) ) 
OSTAG = _macOS12

SUFF = $(OSTAG)_g
SUFFC = _gfort4.6
SUFFC = _gfort4.8
SUFFC = _GCC4.8

FC = gfortran
FC = /usr/local/gfortran/bin/gfortran
FC = /usr/gcc-4.8.1/bin/gfortran

FC90 = gfortran
FC90 = /usr/local/gfortran/bin/gfortran
FC90 = /usr/gcc-4.8.1/bin/gfortran

CC= gcc
CC= /usr/local/bin/gcc
CC= /usr/gcc-4.8.1/bin/gcc

CXX = g++
CXX = /usr/local/bin/g++
CXX = /usr/gcc-4.8.1/bin/g++

FFLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
FF90FLAGS = -arch x86_64 -fcray-pointer -m64 -fdefault-integer-8 -Dmacx64
CFLAGS = -arch x86_64 -m64 -Dmacx64
endif
# end OS 12

# end Mac options
endif

# finish name extentsions with 
# SUFF _g or _o
# SUFFC compiler ie, _GCC4.8

#
ifndef COPT
COPT = -O
endif
#
# form the name suffix based on platform and flags
# default optimized 32 bit
SUFF = $(OSTAG)_o

ifeq ($(COPT), -g)
SUFF = $(OSTAG)_g
endif
# 
OBJDIR = objects$(SUFF)$(SUFFC)/
LIB   = $(PROJ)$(SUFF)$(SUFFC).a
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
# Note OBJDIR and LIBDIR are dependencies that will be created if needed
# Improve make by having it recognize a MODDIR instead of current dir
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
clean:;   @rm -f $(OBJDIR)*.o  $(OBJDIR)*.mod
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
	@echo "F77 SOURCE FILES (F77_OBJS):"
	@echo $(F77_OBJS)
	@echo "F77 C SOURCE FILES (F77C_OBJS):"
	@echo $(F77C_OBJS)
	@echo "C SOURCE FILES (C_OBJS):"
	@echo $(C_OBJS)
	@echo "C++ SOURCE FILES (CXX_OBJS):"
	@echo $(CXX_OBJS)
	@echo "F90 SOURCE FILES (F90_OBJS):"
	@echo $(F90_OBJS)
	@echo "LIB: $(LIB) " 
	@echo "ROOTDIR: $(ROOTDIR)" 
	@echo "BINDIR: $(BINDIR)" 
	@echo "LIBDIR: $(LIBDIR)" 
	@echo "OBJDIR: $(OBJDIR)" 
	@echo "MODDIR: $(MODDIR)" 
	@echo "FC Compilers: $(FC)"
	@echo "FC90 Compilers: $(FC90)"
	@echo "C  Compilers: $(CC)"
	@echo "CFlags: $(FFLAGS) $(COPT),  $(FF90FLAGS) $(COPT), $(CFLAGS) $(COPT)"
	@echo "OS: $(OPSYS) OS TAG: $(OSTAG) NAME SUFFIX $(SUFF)$(SUFFC)"
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

.SUFFIXES: .f90
$(F90_OBJS): $(OBJDIR)%.o: %.f90
	$(FC90) $(COPT) $(FF90FLAGS) -c $< -o $@

$(OBJDIR)%.o : %.c
	$(CC) $(COPT) $(CFLAGS)  -c -o $@ $<

$(OBJDIR)%.o : %.cpp
	$(CXX) $(COPT) $(CFLAGS) -c -o $@ $<

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
