#!/bin/bash
# append -v for verbose output during this script

#-------------------------------------------------------------------------------------------
# This script will download packages for, configure, and build LaGriT cross-platform.
# For bug reporting or suggestions on improvement, email livingston@lanl.gov
#-------------------------------------------------------------------------------------------
# Verified Architecture:
#           Verified OS':
#             - macOS Sierra
#             - Ubuntu 14.04
#             - Ubuntu 16.04
#             - Windows 10 with Linux shell
#
#           Verified Shells:
#             - Bash
#             - Cygwin
#
#           Verified Compilers:
#             - gcc/g++/gfortran
#-------------------------------------------------------------------------------------------

SCRIPT_VERSION="v0.5"

echo "========================================================"
echo "=================== Building LaGriT ===================="
echo "========================= $SCRIPT_VERSION ========================="
echo ""

# Change from pwd if not running inside LaGriT/
export LAGRIT_ROOT_DIR=`pwd`
# Change to match desired Exodus installation location
export EXODUS_ROOT_DIR=$LAGRIT_ROOT_DIR/TPL

# ----- Build variables -----------------
#  Edit these to match your configuration
#  Build flags can be edited under line 276
CC_COMPILER_LG='gcc'
CPP_COMPILER_LG='g++'
FORTRAN_COMPILER='gfortran'
FORTRAN90_COMPILER='gfortran'
LAGRIT_NAME='lagrit' # Final executable name

# ------ Internal variables -------------
BUILD_EXODUS=1 # Enabled by default
BUILD_LAGRIT=1 # Build LaGriT?
TEST_LAGRIT=1 # Test LaGriT after build?

BUILD_STATIC=0 # Used by argument parser
BUILD_DEBUG=0 # Used by argument parser
BUILD_RELEASE=0 # Used by argument parser

SKIPALL=0 # Used by --help flag & dependencies to skip build process
SKIP_BUILD_EXODUS=0

export ACCESS=$EXODUS_ROOT_DIR/seacas/lib

#----------- CHECK THAT REQUIRED PACKAGES ARE INSTALLED -----------#
# Building Exodus requires a number of terminal packages.
#  This checks if they exist and returns an error if not.
#  Build without Exodus (-se) if you do not want to install these.
DEPENDENCIES_INSTALLED=1
if ! [ -x "$(command -v wget)" ]; then
	echo 'Error: wget is not installed.' >&2
	DEPENDENCIES_INSTALLED=0
fi

if ! [ -x "$(command -v tar)" ]; then
	echo 'Error: tar is not installed.' >&2
	DEPENDENCIES_INSTALLED=0
fi

if ! [ -x "$(command -v unzip)" ]; then
	echo 'Error: unzip is not installed.' >&2
	DEPENDENCIES_INSTALLED=0
fi

if [ $DEPENDENCIES_INSTALLED -eq 0 ] ; then
	echo "Use a package installer such as brew, pip,"
	echo '  or apt-get to install the required dependencies.'
	echo '------------------------------------------------------'
	echo "Alternately, building without Exodus (-se) circumvents"
	echo "  these requirements."
	SKIPALL=1 # Essentially 'quits' the script
fi

#------------------ PARSE COMMAND LINE ARGUMENTS ------------------#
# Parses the command line for arguments.
#  At least one argument is required for build. No arguments
#  will result in the help screen being shown.
helpme()
{
	echo "--------------------------------------------------------"
	echo "This is an installer for building the Los Alamos"
	echo "  Grid Toolbox, along with required dependencies."
	echo ""
	echo "Run this script again with one of the options below"
	echo "   or visit lagrit.lanl.gov for more information."
	echo "--------------------------------------------------------"
	echo "  [-r, --release]"
	echo "       Release build (with shared libraries)"
	echo "  [-s, --static]"
	echo "       Release build (with static libraries)"
	echo "  [-d, --debug]"
	echo "       Debug build (with shared libraries)"
	echo "  [-se, --skipexodus]"
	echo "       Build release LaGriT without Exodus dependency"
	echo "  [-e=*, --exodus=PATH]"
	echo "       Path to existing Exodus build (if available)"
	echo "  [-h, --help]"
	echo "       Displays this help screen"
	SKIPALL=1 # Essentially 'quits' the script from the main body
}

# Checks if no arguments were passed
if [ -z "$@" ] ; then
	helpme
fi

for i in "$@"
do
case $i in
	-h|--help)
	helpme
	shift 
	;;
	-s|--static)
	BUILD_STATIC=1

	BUILD_EXODUS=1 # Needs to be optimized to remove this
	BUILD_DEBUG=0 # Needs to be optimized to remove this
	BUILD_RELEASE=0 # Needs to be optimized to remove this
	shift 
	;;
	-d|--debug)
	BUILD_DEBUG=1

	BUILD_STATIC=0 # Needs to be optimized to remove this
	BUILD_EXODUS=1 # Needs to be optimized to remove this
	BUILD_RELEASE=0 # Needs to be optimized to remove this
	shift
	;;
	-se|--skipexodus)
	BUILD_EXODUS=0
	SKIPALL=0 # Assumes it has been set to 1 in dependency check

	BUILD_STATIC=0 # Needs to be optimized to remove this
	BUILD_DEBUG=0 # Needs to be optimized to remove this
	BUILD_RELEASE=0 # Needs to be optimized to remove this
	shift 
	;;
	-r|--release)
	BUILD_RELEASE=1

	BUILD_STATIC=0 # Needs to be optimized to remove this
	BUILD_DEBUG=0 # Needs to be optimized to remove this
	BUILD_EXODUS=1 # Needs to be optimized to remove this
	shift 
	;;
	-e=*|--exodus=*)
	ACCESS="${i#*=}"
	BUILD_EXODUS=0
	shift # past argument=value
	;;
	*)
	helpme # unknown option
	
    ;;
esac
done


#--------------------- TEST IF EXODUS IS BUILT --------------------#
# The default location for building Exodus is in the LaGriT
#  directory. This scans that dir for Exodus.
test_exodus_exists()
{
	if [ -d "$EXODUS_ROOT_DIR/seacas-exodus/lib/" ] ; then
		echo "Exodus appears to have been already built."
		echo "  => $EXODUS_ROOT_DIR/seacas-exodus/lib/"
		
		while true; do
		    read -p "  Would you like to rebuild? [y/n] " yn
		    case $yn in
		        [Yy]* ) SKIP_BUILD_EXODUS=0; break;;
		        [Nn]* ) SKIP_BUILD_EXODUS=1; break;;
		        * ) echo "  Please answer yes (y) or no (n).";;
		    esac
		done
		
	fi
}

#------------------- PARSE LD_LIBRARY FOR EXODUS ------------------#
# This function (i) breaks LD_LIBRARY_PATH into an array of strings,
# and (ii) searches through each string for "exodus": it then asks
# the user if this is the path to Exodus they want to use
parse_ld_lib()
{

	lib_path=$LD_LIBRARY_PATH
	IFS=":"
	set $lib_path

	for word in $lib_path
	do
		if echo "${word,,}" | grep -q "exodus"; then
			echo "Found $word in LD_LIBRARY_PATH"

			while true; do
			    read -p "  Use this as Exodus build? [y/n] " yn
			    case $yn in
			        [Yy]* ) SKIP_BUILD_EXODUS=1; ACCESS=$word; break;;
			        [Nn]* ) SKIP_BUILD_EXODUS=0; break;;
			        * ) echo "  Please answer yes (y) or no (n).";;
			    esac
			done
		fi
	done

}

#------------------------ BUILD EXODUS 6.39 -----------------------#
# This function downloads Exodus and its dependencies (HDF5, netCDF)
# and builds them in the order laid out on the Seacas repo
build_exodus()
{
	mkdir $EXODUS_ROOT_DIR; cd $EXODUS_ROOT_DIR
	git clone https://github.com/gsjaardema/seacas.git
	cd seacas && export ACCESS=`pwd`
	
	# If you run into problems building HDF5,
	#  try uncommenting the below line:
	#export HDF5_ROOT=`pwd`

	# It is common to run into errors during the build of HDF5 and netCDF.
	#    https://github.com/gsjaardema/seacas/issues/61
	
	cd TPL && export TPL=`pwd`
	cd hdf5
	echo "Downloading and unpacking HDF5..."
	wget -O hdf5-1.10.1.gzip https://www.hdfgroup.org/package/gzip/?wpdmdl=4301 || exit 1
	tar zxf hdf5-1.10.1.gzip || exit 1
	rm hdf5-1.10.1.gzip 
	sed -i -e 's/--enable-debug=no/--enable-build-mode=debug/g' runconfigure.sh
	sed -i -e 's/--enable-production/--enable-build-mode=production/g' runconfigure.sh
	cd hdf5-1.10.1
	echo "   Done."
	
	if [ $BUILD_STATIC -eq 1 ] ; then
		cd ../
		sed -i -e 's/--enable-shared/--enable-static/g' runconfigure.sh
		cd hdf5-1.10.1
	fi
	
	sh ../runconfigure.sh
	make && make install || exit 1

	cd ../../netcdf

	echo "Downloading and unpacking netCDF..."
	wget -O netcdf-4.4.1.1.tar.gz ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4.4.1.1.tar.gz || exit 1
	tar zxf netcdf-4.4.1.1.tar.gz || exit 1
	rm netcdf-4.4.1.1.tar.gz
	echo "   Done."

	cd netcdf-4.4.1.1/include

	#Ctrl+V + Ctrl+I for tab in terminal

	# in the define statements are tabs - these are necessary for the command to complete
	sed -i -e 's/#define NC_MAX_DIMS	1024/#define NC_MAX_DIMS	65536/g' netcdf.h
	sed -i -e 's/#define NC_MAX_VARS	8192/#define NC_MAX_VARS	524288/g' netcdf.h

	cd ../
	mkdir build/
	cd build/
	
	if [ $BUILD_STATIC -eq 1 ] ; then
		tmp_dir=`pwd`
		cd ../..
		sed -i -e 's/-DBUILD_SHARED_LIBS:BOOL=ON/-DBUILD_SHARED_LIBS:BOOL=OFF/g' runcmake.sh
		cd $tmp_dir
	fi
	
	sh ../../runcmake.sh
	make && make install || exit 1

	cd $ACCESS
	
	# Disable Matio, CGNS, X11
	# Enable if you want the specific functionality offered
	# Visit the Seacas repo to learn more
	sed -i -e 's/HAVE_MATIO=ON/HAVE_MATIO=OFF/g' cmake-config
	sed -i -e 's/HAVE_CGNS=ON/HAVE_CGNS=OFF/g' cmake-config
	sed -i -e 's/HAVE_X11=ON/HAVE_X11=OFF/g' cmake-config
	
	mkdir build
	cd build

	if [ $BUILD_STATIC -eq 1 ] ; then
		cd ..
		sed -i -e 's/BUILD_SHARED_LIBS:BOOL=ON/BUILD_SHARED_LIBS:BOOL=OFF/g' cmake-exodus
		cd build
	fi

	echo "Building Exodus..."
	../cmake-config -DCMAKE_C_COMPILER="$CC_COMPILER_LG" -DCMAKE_CXX_COMPILER="$CPP_COMPILER_LG" -DCMAKE_Fortran_COMPILER="$FORTRAN_COMPILER"
	make && make install || exit 1
	
	# Verify correct header files got generated; if not, copy
	if [[ ! -f "../include/exodusII.h" ]]; then 
		echo "BUILD WARNING: exodusII.h not correctly generated"
		cp ../packages/seacas/libraries/exodus/include/exodusII.h ../include/exodusII.h
	fi
	
	if [[ ! -f "../include/exodusII.inc" ]]; then 
		echo "BUILD WARNING: exodusII.inc not correctly generated"
		cp ../packages/seacas/libraries/exodus_for/include/exodusII.inc ../include/exodusII.inc
	fi
	
	echo "   Exodus build complete."

}


#--------------------------- BUILD LAGRIT -------------------------#
# This function builds LaGriT based on arguments passed to the script
#  through the console. Flags can be changed depending on your
#  specific requirements and compiler.
build_lagrit()
{
	
	MAKEFLAG=''
	LINKERFLAGS=''
	
	cd $LAGRIT_ROOT_DIR
	export LAGRIT_UTIL_DIR=`pwd`/lg_util/src
	
	echo "Setting environment variables..."
	echo $ACCESS	

	# Static build
	if [ $BUILD_STATIC -eq 1 ] ; then
		echo "Configuring static build..."
		LINKERFLAGS=(-O -static  -fcray-pointer -fdefault-integer-8  -Dlinx64 -c -o)
		BUILDFLAGS=(-O -static -static-libgfortran -fcray-pointer -fdefault-integer-8 -Dlinx64 -o)
		BUILDLIBS=(lagrit_main.o lagrit_fdate.o  lagrit_ulin64_o_gcc.a $LAGRIT_UTIL_DIR/util_ulin64_o_gcc.a)
		BUILDSUFFIX=(-L$ACCESS -lexoIIv2for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lm -lz -ldl -lstdc++)
		MAKEFLAG='MOPT=64'

		# Default gcc/gfortran compiler on Mac behaves different for static flag
		if [ "$(uname)" == "Darwin" ]; then
			tmp=`pwd`
			cd $ACCESS
			
			# Hide dynamic libs so compiler will force-use static
			for file in *.dylib; do
				mv "$file" "`basename "$file" .dylib`.hidden"
			done
			cd $tmp
			
			# Change /usr/local/lib/libquadmath.a to match yours - should be identical
			# May not work - physically change the extension of libquadmath.*.dylib to .hidden,
			#    run script, and change back to force static .a
			LINKERFLAGS=(-g -fcray-pointer -fdefault-integer-8 -m64 -Dmacx64 -c -o)
			BUILDFLAGS=(-g -static-libgfortran -static-libgcc -fcray-pointer -fdefault-integer-8 -m64 -Dmacx64 -fno-sign-zero -o)
			BUILDSUFFIX=(-L$ACCESS /usr/local/lib/libquadmath.a -lexoIIv2for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lm -lz -ldl -lstdc++)
		fi
	fi
	
	# Debug with shared libs
	if [ $BUILD_DEBUG -eq 1 ] ; then
		echo "Configuring debug build..."
		LINKERFLAGS=(-g  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o)
		BUILDFLAGS=(-g -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o)
		BUILDLIBS=(lagrit_main.o lagrit_fdate.o lagrit_ulin64_o_gcc.a $LAGRIT_UTIL_DIR/util_ulin64_o_gcc.a)
		BUILDSUFFIX=(-L$ACCESS -lexoIIv2for -lexodus -lnetcdf -lm -lstdc++)
		MAKEFLAG='COPT=-g'
	fi
	
	# Build without Exodus
	if [ $BUILD_EXODUS -eq 0 ] ; then
		echo "Configuring release build without Exodus..."
		LINKERFLAGS=(-g  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o)
		BUILDFLAGS=(-g -Dlinx64 -static-libgfortran -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o)
		BUILDLIBS=(lagrit_main.o lagrit_fdate.o lagrit_ulin64_g_gcc.a $LAGRIT_UTIL_DIR/util_ulin64_o_gcc.a)
		BUILDSUFFIX=(-lm -lstdc++)
		MAKEFLAG='COPT=-g'
		
		cd "$LAGRIT_ROOT_DIR/src/"
		cp dumpexodusII.f dumpexodusII.f.withexo
		cp dumpexodusII.f.withnoexo dumpexodusII.f
		echo "" > exo_init_ext.c
		echo "" > exo_put_sets.c
		cd $LAGRIT_ROOT_DIR
	fi
	
	# Release with shared libs
	if [ $BUILD_RELEASE -eq 1 ] ; then
		echo "Configuring release build..."
		LINKERFLAGS=(-O  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o)
		BUILDFLAGS=(-O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o)
		BUILDLIBS=(lagrit_main.o lagrit_fdate.o  lagrit_ulin64_o_gcc.a $LAGRIT_UTIL_DIR/util_ulin64_o_gcc.a)
		BUILDSUFFIX=(-L$ACCESS -lexoIIv2for -lexodus -lnetcdf -lm -lstdc++)
		MAKEFLAG='MOPT=64'
	fi

	echo "   Done."
	echo "Building Library lg_util..."

	cd lg_util/src/
	make clean
	make $MAKEFLAG || exit 1

	echo "   Done."
	echo "Preparing LaGriT source directory..."

	cd ../../src/
	rm *.o; rm *.mod # make clean
	
	# Get exodusII.h and exodusII.inc from current version of ExodusII 
	cp $EXODUSII_HOME/include/exodusII.h . 
	cp $EXODUSII_HOME/include/exodusII.inc . 
	if [ ! -f "exodusII.inc" ] ;  then 
		echo "The file src/exodusII.inc not found, can not complete build." 
		echo "Not found in EXODUSII_HOME set as $EXODUSII_HOME" 
		echo " " 
                exit
	fi
	
	$FORTRAN_COMPILER ${LINKERFLAGS[*]} lagrit_main.o lagrit_main.f || exit 1
	$FORTRAN_COMPILER ${LINKERFLAGS[*]} lagrit_fdate.o lagrit_fdate.f || exit 1
	
	echo "   Done."
	echo "Building Library lagrit..."
	make $MAKEFLAG lib
	
	echo "   Done."
	echo "Building LaGriT executable..."
	
 	echo "$FORTRAN_COMPILER ${BUILDFLAGS[*]} $LAGRIT_NAME ${BUILDLIBS[*]} ${BUILDSUFFIX[*]}"
	$FORTRAN_COMPILER ${BUILDFLAGS[*]} $LAGRIT_NAME ${BUILDLIBS[*]} ${BUILDSUFFIX[*]} || exit 1
	echo "$LAGRIT_NAME   Done."
	
	if [ "$(uname)" == "Darwin" ]; then
		if [ $BUILD_EXODUS -eq 1 ] && [ $BUILD_STATIC -eq 0 ]; then
			export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$ACCESS
			echo ""
			echo "-------------------------------------------------------------------------"
			echo "IMPORTANT NOTICE: For macOS to correctly find Exodus libraries,"
			echo "  the following line must be appended to your ~./bashrc or ~./bash_profile:"
			echo 'export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:'"$ACCESS"
			echo ""
			echo "Otherwise, once this current terminal session ends, DYLD_LIBRARY_PATH"
			echo "  will revert back to its default state and you will have to manually"
			echo "  export the Exodus lib path each time you wish to run LaGriT in a new"
			echo "  Terminal session. This is a macOS-specific issue, since OS X Tiger."
			echo "-------------------------------------------------------------------------"
			read -n 1 -s -p "Press any key to continue "
		fi
		
		# For static build, reshow hidden shared libs
		if [ $BUILD_EXODUS -eq 1 ] && [ $BUILD_STATIC -eq 1 ]; then
			tmp=`pwd`
			cd $ACCESS
			for file in *.hidden; do
				mv "$file" "`basename "$file" .hidden`.dylib"
			done
			cd $tmp
		fi
	fi
	
}

#------------------------ RUN LAGRIT TESTS ------------------------#
# LaGriT includes a built-in test suite. This function calls it
#  after LaGriT is built. Zero tests will pass if the build is
#  unsuccessful.
test_lagrit()
{
	cd $LAGRIT_ROOT_DIR/test
	echo "Testing LaGriT Build..."
	python suite.py -f -l 1 -exe=$LAGRIT_ROOT_DIR'/src/'$LAGRIT_NAME
	echo "   Testing complete."
	echo "   See test/suite.py to run other test options."
	echo "  "
	
	echo "   LaGrit executable created and tested: $LAGRIT_ROOT_DIR/src/$LAGRIT_NAME"
	echo "  "
}


#---------------------------- MAIN BODY ---------------------------#
if [ $SKIPALL -eq 0 ] ; then
	if [ $BUILD_EXODUS -eq 1 ] ; then
		test_exodus_exists
		
		if [ $SKIP_BUILD_EXODUS -eq 0 ] ; then
			parse_ld_lib
		fi
		
		if [ $SKIP_BUILD_EXODUS -eq 0 ] ; then
			build_exodus
		fi
	fi

	if [ $BUILD_LAGRIT -eq 1 ] ; then
		build_lagrit
	fi

	if [ $TEST_LAGRIT -eq 1 ] ; then
		test_lagrit
	fi
fi

