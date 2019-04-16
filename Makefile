#================================================================#
#  LaGriT Makefile
# -------------------------------------------------------------- #
#  
#  Targets:
#    - release : build a LaGriT binary with shared libs
#    - static : build a LaGriT binary with static libs
#    - exodus : build the Exodus library
#    - test : run the LaGriT test suite
#    - help : show a help screen
#    - header : build src/lagrit.h header file
#    - clean : remove all object and mod files
#    - clobber : remove all object and mod files *and* library
#
#  Variables:
#    CC (default: gcc) : C source compiler
#    FC (default: gfortran) : Fortran source compiler
#    FC90 (default: gfortran) : Fortran90 source compiler
#    WITH_EXODUS (default: 1) : Build with or without Exodus
#    DEBUG (default: 0) : Built in debug (1) or optimized (0) mode
#    EXO_LIB_DIR (default: LAGRIT_ROOT_DIR/seacas/lib) : ExodusII library location
#    EXE_NAME (default: src/lagrit) : binary filename for LaGriT
#    EXO_CMAKE_FLAGS (default: none) : Add custom CMake flags to pass to cmake-exodus
#================================================================#
#  TODO:
#    - Add support for 32 bit machines

CC := gcc
CXX := g++
FC := gfortran
FC90 := $(FC)
OBJDIR := objects/
WITH_EXODUS := 1

# Detect 64 or 32 bit arch; detect OS name
BIT_SIZE := $(shell getconf LONG_BIT)
OPSYS := $(shell uname -s)

MAJOR_VERSION := 3
MINOR_VERSION := 320
BUILD_DATE := $(shell date +%Y/%m/%d)
BUILD_TYPE := Release
EXE_NAME := src/lagrit
EXO_BUILD_DIR := $(shell pwd)
EXO_LIB_DIR := $(shell pwd)/seacas/lib
EXO_CMAKE_FLAGS := 

LG_UTIL_LIB := lg_util_lib.a
SRC_LIB := lagrit_lib.a

define LAGRIT_H_TEXT
c
c----------------------------------------------------------------
c Auto-generated LaGriT program banner
c
c Substitute the TAG strings with Date and Linux, Darwin, WIN, etc.
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
c----------------------------------------------------------------
c
      integer        v_major, v_minor
      parameter      (v_major=$(MAJOR_VERSION))
      parameter      (v_minor=$(MINOR_VERSION))
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/
c     os_name is used to find and write OS related files
c     make sure it is a version recognized in Makefile
c     and writinit.f for forming header info
      data os_name      /'$(OPSYS)'/
c
      data date_compile /'$(BUILD_DATE) $(BUILD_TYPE) '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
endef
export LAGRIT_H_TEXT

define LAGRIT_HELP
----------------------- LAGRIT MAKEFILE -----------------------

This is an automatic build tool for the Los Alamos Grid Toolbox.

USAGE:

  make [options] [target]

TARGETS:

- make release
    Optimized build (with shared libraries)

- make static
    Optimized build (with static libraries)

- make DEBUG=1 release
    Debug build (with shared libraries)

- make DEBUG=1 static
    Debug build (with static libraries)

- make exodus
    Download and build the SANDIA ExodusII Fortran library.
    By default, it will download to the current working directory.
    This can be changed by running

       make EXO_BUILD_DIR=/exodus/path/out/ exodus

- make test
    Run the LaGriT test suite on the created binary.
    If you have changed or moved LaGriT from src/lagrit, use
    the option EXE_NAME=/path/to/lagrit

OPTIONS:

    CC (default: gcc) : C source compiler
    FC (default: gfortran) : Fortran source compiler
    FC90 (default: gfortran) : Fortran90 source compiler
    WITH_EXODUS (default: 1) : Build with or without Exodus
    DEBUG (default: 0) : Built in debug (1) or optimized (0) mode
    EXO_LIB_DIR (default: LAGRIT_ROOT_DIR/seacas/lib) : ExodusII library location
    EXE_NAME (default: src/lagrit) : binary filename for LaGriT
    EXO_CMAKE_FLAGS (default: none) : Add custom CMake flags to pass to cmake-exodus

endef
export LAGRIT_HELP

# release, no exo, shared
BUILDLIBS := src/lagrit_main.o src/lagrit_fdate.o src/$(SRC_LIB) lg_util/src/$(LG_UTIL_LIB)

LINKERFLAGS := -fcray-pointer -fdefault-integer-8 -m64
BUILDFLAGS  := -fcray-pointer -fdefault-integer-8 -m64 -fno-sign-zero -lm -lstdc++
OSX_STATIC_LIBS := 

ifeq ($(OPSYS),Darwin)
	LINKERFLAGS += -Dmacx64
	BUILDFLAGS += -Dmacx64
	OSX_STATIC_LIBS := 
else ifeq ($(OPSYS),Linux)
	LINKERFLAGS += -Dlinx64
	BUILDFLAGS += -Dlinx64
endif

ifeq ($(DEBUG),1)
	LINKERFLAGS += -g -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal
	BUILDFLAGS +=  -g -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal
else
	LINKERFLAGS += -O -ffpe-summary=none
	BUILDFLAGS += -O -ffpe-summary=none
endif

ifeq ($(wildcard $(EXO_LIB_DIR)),)
        EXO_LIB_DIR := /usr/lib
endif

ifeq ($(WITH_EXODUS),1)
	BUILDFLAGS += -L$(EXO_LIB_DIR) -lexodus_for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -ldl
	FAIL_THRESH := 1
else
	FAIL_THRESH := 3
endif

.PHONY: release static header before clean clobber test exodus
release: BUILD_TYPE = Release
release: build

build : header before
	$(FC) -o $(EXE_NAME) $(BUILDLIBS) $(BUILDFLAGS)

header :
	@echo "$$LAGRIT_H_TEXT" > src/lagrit.h

before : src/lagrit_main.o src/lagrit_fdate.o
	make -C lg_util/src/ LIB=$(LG_UTIL_LIB) CC=$(CC) FC=$(FC) DEBUG=$(DEBUG)
	make -C src/ LIB=$(SRC_LIB) WITHEXODUS=$(WITH_EXODUS) CC=$(CC) FC=$(FC) EXO_INCLUDE_DIR=$(EXO_LIB_DIR)/../include DEBUG=$(DEBUG)

clean :
	make -C lg_util/src/ clean
	make -C src/ clean

clobber : 
	make -C lg_util/src/ clobber
	make -C src/ clobber
	rm src/*.o ; rm $(EXE_NAME)

test :
	@export LG_CWD=$(shell pwd); \
	 cd test/; \
	 python3 suite.py -f -l 1 -exe=$$LG_CWD/$(EXE_NAME) -hf=$(FAIL_THRESH);

help : 
	@echo "$$LAGRIT_HELP"

exodus :
	export CGNS=NO; \
	export MATIO=NO; \
	export SHARED=NO; \
	export LG_DIR=`pwd`; \
	export NEEDS_ZLIB=YES; \
	export GNU_PARALLEL=OFF; \
	export CC=$(CC); export CXX=$(CXX); export FC=$(FC); export FC90=$(FC90); \
	git clone https://github.com/gsjaardema/seacas.git $(EXO_BUILD_DIR)/seacas; \
	cd $(EXO_BUILD_DIR)/seacas; \
	export ACCESS=`pwd`; \
	./install-tpl.sh; \
	cd TPL; \
	../cmake-exodus $(EXO_CMAKE_FLAGS) -DFORTRAN=YES; \
	make && make install; \
	cd $(LG_DIR); \
	echo "Exodus successfully built!"; \
	echo "Build directory:"; \
	echo "   $(EXO_BUILD_DIR)/seacas/lib"; \
	echo ""
	echo "To compile LaGriT with Exodus, append the above"; \
	echo "path to LD_LIBRARY_PATH (on Linux) or DYLD_LIBRARY_PATH (on Mac)";\
	echo "and run \"make [options] [target]\"."; \
	echo ""; \
	echo "Alternately, run"; \
	echo "  make EXO_LIB_DIR=$(EXO_BUILD_DIR)/seacas/lib [target]"

static: BUILD_TYPE = Static
static: LINKERFLAGS += -static
static: BUILDFLAGS += -static-libgfortran -static-libgcc
static: build

%.o : %.f
	$(FC) $(LINKERFLAGS) -c -o $@ $<
