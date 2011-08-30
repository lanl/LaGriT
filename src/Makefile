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
# SGI and SUN OS is included here, but no longer supported
#
# Created Jan 7 2008 by tamiller@lanl.gov
# Modified July 7 2009 to update compiler flags for V2.100 release 
# Modified Jan 1 2011 to include gfortran flags and .cpp files
# Modified Aug 8 2011 to include work from Adam Cozzette and .cpp files 
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
CXX = g++
#
# Linux OS ------------------------------
# Linux rainier 2.6.9-55.0.2.ELlargesmp #1 SMP 2007
# x86_64
#
ifeq ($(OPSYS), Linux)
OSTAG = _lin
FC = /opt/absoft11.1/bin/f77 
FC90 = /opt/absoft10.0/bin/f90 
CC= gcc 
FFLAGS = -c -f -m32 -q -w 
FF90FLAGS = -m32 -YEXT_NAMES=LCS  
CFLAGS = -m32
OBJDIR = objects_lin/

ifeq ($(COMPILER),GFORTRAN)
FC = gfortran
FC90 = gfortran 
CC= gcc 

FFLAGS = -fcray-pointer -m32 
FF90FLAGS = -fcray-pointer -m32 
CFLAGS = -m32 -Dlin

ifeq ($(MOPT), 64)
OSTAG = _lin64
OBJDIR = lin_objects_m64/
FFLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 
FF90FLAGS = -fcray-pointer -fdefault-integer-8  -m64 -Dlinx64 
CFLAGS = -m64 -Dlinx64
endif

endif

endif
#
# Mac OSX ------------------------------
# Darwin darmok.lanl.gov 9.7.0 Darwin Kernel Version 9.7.0
# powerpc mac
# -N11 needed for assembler (large code)
#
ifeq ($(OPSYS), Darwin)
OSTAG = _mac
FC = /Applications/Absoft9.5/bin/f77 
FC90 = /Applications/Absoft9.5/bin/f90 
CC= gcc 
FFLAGS = -c -f -m32 -N11 -q -w 
FF90FLAGS = -m32 -YEXT_NAMES=LCS 
CFLAGS = -m32 
OBJDIR = objects_mac/

# intel mac
# Darwin i386 Kernel Version 10.7.0 RELEASE_X86_64 x86_64
#
ifeq ($(NPROC), i386)
OSTAG = _maci
FC = /Applications/Absoft10.2/bin/f77
FC90 = /Applications/Absoft10.2/bin/f90
CC= gcc
FFLAGS = -f -m32 -q
FF90FLAGS = -m32 
CFLAGS =  -m32
OBJDIR = objects_maci/
endif
#
endif
#######################################################
# The following OS have not been supported since 2010
# Use with caution as flags may be out of date
#
#
# SUN OS ------------------------------
# SunOS flow 5.10 Generic_137137-09 sun4u sparc SUNW,Sun-Fire-880
# sparc
#
ifeq ($(OPSYS), SunOS)
OSTAG = _sun
FC = f77 -Bstatic
FC90 = f90 -Bstatic
CC = gcc
FFLAGS = 
FF90FLAGS = -f77
FF90FLAGS =
CFLAGS = 
OBJDIR = objects_sun/
endif
#
# SGI 64 OS ------------------------------
# IRIX64 combust 6.5 10151453 IP27
#  mips 
# ADD FLAG for 64 bit SGI 
# needs work and testing
ifeq ($(OPSYS), IRIX64)
OSTAG = _sgi64
OSTAG = _sgi
FC = /bin/f77
FC90 = /bin/f90
CC= cc
FFLAGS = -c -g -64 -i8 -r10000
FFLAGS = -n32 -r10000 -avoid_gp_overflow
FF90FLAGS = -n32 -r10000
CFLAGS = $(FFLAGS)
OBJDIR = objects_sgi64/
OBJDIR = objects_sgi/
endif
#######################################################
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
SUFF2 =""
ifeq ($(COMPILER),GFORTRAN)
SUFF2 = _gfort4.5
endif
ifeq ($(COMPILER),ABSOFT)
SUFF2 = _absoft
endif
# 
OBJDIR = objects$(SUFF)$(SUFF2)/
LIB   = $(PROJ)$(SUFF)$(SUFF2).a
PROG  = $(PROJ)$(SUFF)$(SUFF2)
LIBDATE   = $(PROJ)$(SUFF)$(DATETAG)$(SUFF2).a
PROGDATE  = $(PROJ)$(SUFF)$(DATETAG)$(SUFF2)
MODDIR = $(OBJDIR)$(SUFF2)
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
	@echo "OS: $(OPSYS) OS TAG: $(OSTAG) NAME SUFFIX $(SUFF)$(SUFF2)"
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

machine.h : machine_header.h
	@echo "--------------------------------------------------"
	@echo "Remove and re-make machine.h from machine_header.F"
	cp -p machine_header.h machine_header.F
	$(CC) -E $(CFLAGS) machine_header.F -o machine.h
	cp -p machine.h $(OBJDIR)
	-rm -f machine_header.F
	@echo "--------------------------------------------------"

$(OBJDIR)%.o : %.F 
	cp lagrit$(OSTAG).h lagrit.h
	$(FC) $(COPT) $(FFLAGS)  -c -o $@ $<

$(LIBDIR) :; mkdir $(LIBDIR)

$(OBJDIR) :; mkdir $(OBJDIR)

include Makefile.depends

# DO NOT DELETE
