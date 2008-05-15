##########################################################################
# GNU Make version 3.79.1 or later
#
# in general to update libs and executable  
# on each platform
#    make install
#    make COPT=-g install
#
#    buildbin
#    or buildbin opt or buildbin debug
#    will build executable using libs in LIBDIR
# 
# USAGE: 
# make OR make lib 
#    creates or updates optimized *.o in OBJDIR_o 
#    creates LIB_o.a in current directory 
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
#
# Note: assumes Makefile and Makefile.depends
# Note: compatible with gmake which is suggested for Solaris
# Note: assumes path to sh is defined on system, otherwise define here
# shell = /bin/sh
#
# Created Jan 7 2008 by tamiller@lanl.gov
#
##########################################################################
# user defined variables
# ------------------------------------------------------------------
#
MAKEFILE  = Makefile
PROJ  = lagrit
ROOTDIR= $(shell pwd)
LIBDIR= $(subst lagrit/src,lagrit/lib, $(ROOTDIR)) 
BINDIR= $(subst lagrit/src,lagrit/bin, $(ROOTDIR)) 
#
##########################################################################
#  auto variables (May also be set at command line, environment, or script) 
# ------------------------------------------------------------------
#
OPSYS = UNKNOWN 
OPSYS_FULL = $(shell uname -a )
OPSYS = $(shell uname -s )
DATETAG = $(shell date '+_%y%m%d')
#
# ADD FLAG for 64 bit SGI 
# sgi 64 will need some work to compile correctly
# there are some 64bit settings in some include files
# and some character string declarations produce errors
# in dump_avs_nosb.f and writinit.f
# work dir is in
# /home/cwg/src/lagrit_v1.0/work/build_sgi/build_64bit/liblagrit
# and the directory above
#
# SGI 64 OS ------------------------------
ifeq ($(OPSYS), IRIX64)
OSTAG = _sgi64
OSTAG = _sgi
FC = /bin/f77 
FC90 = /bin/f90 
CC= cc
FFLAGS = -c -g -64 -i8 -r10000 
FFLAGS = -n32 -r10000
FF90FLAGS = $(FFLAGS)
CFLAGS = $(FFLAGS)
OBJDIR = sgi64_objects/
OBJDIR = sgi_objects/
endif
#
# Linux OS ------------------------------
ifeq ($(OPSYS), Linux)
OSTAG = _lin
FC = /opt/absoft10.0/bin/f77 
FC90 = /opt/absoft10.0/bin/f90 
CC= cc 
FFLAGS = -f -m32 -N113 -N90 -q -w 
FF90FLAGS = -m32 -YEXT_NAMES=LCS  
CFLAGS = -m32
OBJDIR = lin_objects/
endif
#
# Mac OSX ------------------------------
ifeq ($(OPSYS), Darwin)
OSTAG = _mac
FC = /Applications/Absoft/bin/f77 
FC90 = /Applications/Absoft/bin/f90 
CC= cc 
FFLAGS = -f -N113 -N90 -B19 -O -q -w 
FF90FLAGS = -YEXT_NAMES=LCS  
CFLAGS = 
OBJDIR = mac_objects/
endif
#
# SUN OS ------------------------------
ifeq ($(OPSYS), SunOS)
OSTAG = _sun
FC = /n/local_SunOS/forte7/SUNWspro/bin/f77
FC90 = /n/local_SunOS/forte7/SUNWspro/bin/f90
CC = /n/local_SunOS/forte7/SUNWspro/bin/cc
FFLAGS =
FF90FLAGS =   
CFLAGS =
OBJDIR = sun_objects/
endif
#
ifeq ($(COPT), -g)
SUFF = $(OSTAG)_g
else
SUFF = $(OSTAG)_o
endif
#
ifndef COPT
COPT = -O
endif
# 
OBJDIR = objects$(SUFF)/
LIB   = $(PROJ)$(SUFF).a
PROG  = $(PROJ)$(SUFF)
LIBDATE   = $(PROJ)$(SUFF)$(DATETAG).a
PROGDATE  = $(PROJ)$(SUFF)$(DATETAG)
MODDIR = $(OBJDIR)
# 
##########################################################################
# Source  
# ------------------------------------------------------------------
#
F77_OBJS := $(patsubst %.f,$(OBJDIR)%.o,$(wildcard *.f))
#
C_OBJS := $(patsubst %.c,$(OBJDIR)%.o,$(wildcard *.c))
#
#F90_OBJS := $(patsubst %.f90,$(OBJDIR)%.o,$(wildcard *.f90))
#
# works for mod in current directory
F90_OBJS := $(patsubst %.f90,%.o,$(wildcard *.f90))
#
##########################################################################
# action targets
# the first target in the file will run if make is not given a target
# Note OBJDIR and LIBDIR are dependencies that will be created if needed
# Improve make by having it recognize a MODDIR instead of current dir
# ------------------------------------------------------------------
#
lib : lagrit.h $(OBJDIR) $(F90_OBJS)  $(F77_OBJS) $(C_OBJS) 
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(F90_OBJS)
	mv $(F90_OBJS) $(OBJDIR)
	mv *.mod $(OBJDIR)
	cat lagrit.h
	cp lagrit.template.h lagrit.h
	@echo "Done $@ $(LIB)"
#
install: lagrit.h $(OBJDIR) $(F90_OBJS)  $(F77_OBJS) $(C_OBJS) $(LIBDIR) 
	ar rcu $(LIB) $(F77_OBJS)
	ar rcu $(LIB) $(C_OBJS)
	ar rcu $(LIB) $(F90_OBJS)
	mv $(F90_OBJS) $(OBJDIR)
	mv *.mod $(OBJDIR)
	cp -p $(LIB) $(LIBDIR)
	cp -p $(LIB) $(LIBDATE)
	cp -p $(LIBDATE) $(LIBDIR)
	mv $(LIBDATE) $(OBJDIR)
	cat lagrit.h
	cp lagrit.template.h lagrit.h
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
	@echo "C SOURCE FILES (C_OBJS):"
	@echo $(C_OBJS)
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
#$(OBJDIR).f90.o:
#	$(FC90) $(FF90FLAGS)  -c $@ $<

# works for .mod in current dir
%.o %.mod: %.f90
	$(FC90) $(FF90FLAGS) -c $<

$(OBJDIR)%.o : %.c
	$(CC) $(COPT) $(CFLAGS)  -c -o $@ $<


$(LIBDIR) :; mkdir $(LIBDIR)

$(OBJDIR) :; mkdir $(OBJDIR)

include Makefile.depends

