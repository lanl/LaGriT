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

CC = cl.exe
CXX = cl.exe
FC = ifort
FC90 = $(FC)
WITH_EXODUS = 0

# Detect 64 or 32 bit arch; detect OS name
BIT_SIZE = 64
OPSYS = CYGWIN_NT

MAJOR_VERSION = 3
MINOR_VERSION = 320
BUILD_DATE = 2020/11/02
#$(shell date +%Y/%m/%d)
BUILD_TYPE = Release
EXE_NAME = src/lagrit.exe
EXO_BUILD_DIR = 
EXO_LIB_DIR = 
EXO_CMAKE_FLAGS = 

LG_UTIL_LIB = lg_util.dll
SRC_LIB = lagrit_lib.dll

# release, no exo, shared
BUILDLIBS = src/lagrit_main.o src/lagrit_fdate.o src/$(SRC_LIB) lg_util/src/$(LG_UTIL_LIB)

LINKERFLAGS = -O -Dwin64 -m64 -safe-cray-ptr -integer-size 64 #-fcray-pointer -fdefault-integer-8 -m64
BUILDFLAGS  = -O -Dwin64 -m64 -safe-cray-ptr -integer-size 64 -lm -lstdc++ #-fno-sign-zero -fcray-pointer -fdefault-integer-8 -m64 -fno-sign-zero -lm -lstdc++
OSX_STATIC_LIBS = 
SYS_LNK_FLAGS =

FAIL_THRESH = 3
BUILD_TYPE = Release

.PHONY: release static header before clean clobber test exodus
release: build

build : before
	$(FC) -o $(EXE_NAME) $(SYS_LNK_FLAGS) $(BUILDLIBS) $(BUILDFLAGS)

before : src/lagrit_main.o src/lagrit_fdate.o
	cd lg_util\src
	nmake LIB=$(LG_UTIL_LIB) CC=$(CC) FC=$(FC) DEBUG=$(DEBUG)
	cd ..\..\src
	nmake LIB=$(SRC_LIB) WITHEXODUS=$(WITH_EXODUS) CC=$(CC) FC=$(FC) EXO_INCLUDE_DIR=$(EXO_LIB_DIR)/../include DEBUG=$(DEBUG)

clean :
	cd lg_util\src
	nmake clean
	cd ..\..\src
	nmake clean

clobber : 
	nmake -C lg_util/src/ clobber
	nmake -C src/ clobber
	del src/*.o
	del $(EXE_NAME)

src/lagrit_main.o : src/lagrit_main.f
	$(FC) $(LINKERFLAGS) -c -o $@ $<

src/lagrit_fdate.o : src/lagrit_fdate.f
	$(FC) $(LINKERFLAGS) -c -o $@ $<