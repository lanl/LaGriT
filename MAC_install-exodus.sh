# script to install seacas-exodus for build with LaGriT
# See detailed instructions at https://github.com/sandialabs/seacas
# Seacas Contact Gregory Sjaardema gsjaardema@gmail.com
#
# LaGriT does not need full seacas but does require
# exodus and associated libs with FORTRAN=YES
# libexodus.a      libexoIIv2for32.a  libhdf5_hl.a     libnetcdf.a
# libexodus_for.a  libhdf5.a          libhdf5_tools.a  libz.a
# Preferable to use versions newer than July 2022
set -e
set -x

# ==== USER SETTINGS ==============================
# Suggest Installing seacas in LaGriT/TPLs
SEACAS_INSTALL_DIR=${SEACAS_INSTALL_DIR:-"$(pwd)/TPLs/"}
# EXO_COMMIT_HASH=${EXO_COMMIT_HASH:-v2021-10-11}
# =================================================

# ==== MAC SPECIFIC COMPILER EXPORTS ===============

# ==== MAC SPECIFIC COMPILER EXPORTS ===============
if [[ $(command -v brew) != "" ]]; then
	echo "Homebrew found"
	export PATH="/usr/bin/:/opt/homebrew/lib/:/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
	export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
	export COMPILER=HomeBrew
	fortran_var=/opt/homebrew/bin/gfortran
	cc_var=/usr/bin/gcc
	cpp_var=/usr/bin/g++

elif [[ $(command -v port) != "" ]]; then
	echo "MacPorts found"
	export PATH="/opt/local/libexec/gnubin:/opt/local/bin:/opt/local/sbin:$PATH"
	export COMPILER=MacPorts
	fortran_var=$(find /opt/local/bin/gfortran-mp-*)
	cc_var=$(find /opt/local/bin/gcc-mp-*)
	cpp_var=$(find /opt/local/bin/g++-mp-*)

else
    echo "Please install compiler using Homebrew or MacPorts. Exiting."
    exit 1
fi

echo "Fortran Compiler: $fortran_var"
echo "C Compiler: $cc_var"
echo "C++ Compiler: $cpp_var"

FC="$fortran_var"
export FC="$fortran_var"

CC="$cc_var"
export CC="$cc_var"

CXX="$cpp_var"
export CXX="$cpp_var"

# ==== GET SOURCE  ================================
mkdir -p ${SEACAS_INSTALL_DIR}
cd ${SEACAS_INSTALL_DIR}

git clone https://github.com/sandialabs/seacas.git
cd seacas && export ACCESS=`pwd`

# ==== INSTALL REQUIRED LIBRARIES  ==================
# Use seacas/install-tpl.sh
# These flags are recommended for LaGriT
CGNS=NO MATIO=NO GNU_PARALLEL=NO FMT=NO SHARED=NO NEEDS_ZLIB=YES ./install-tpl.sh 

# Result: lib/libhdf5.a lib/libhdf5_hl.a lib/libhdf5_tools.a lib/libnetcdf.a
#
# ==== RUN CMAKE  ====================================

cd $ACCESS
mkdir build && cd build
FORTRAN=YES SHARED=NO ../cmake-exodus

# result:
# -- Build files have been written to: TPLs/seacas/build
#    HAVE_NETCDF: YES

# ==== BUILD and INSTALL EXODUS  =====================
# In directory seacas/build
make && make install

# ==== SCRIPT DONE  ==================================
# CHECK: libs and files used by lagrit located in TPLs/seacas
# 
# bsh% ls lib/*a
# lib/libexodus.a      lib/libexoIIv2for32.a  lib/libhdf5_hl.a     lib/libnetcdf.a
# lib/libexodus_for.a  lib/libhdf5.a        lib/libhdf5_tools.a  lib/libz.a
# 
# bsh% ls include/exo*
# include/exodus_config.h  include/exodusII.h  include/exodusII.inc  include/exodusII_par.h
# include/netcdf.h include/hdf5.h
# 
# Once Exodus is installed, use the following commands to build lagrit executable:
# 
# mkdir build/ && cd build/
# cmake .. -DLAGRIT_BUILD_EXODUS=ON
# make
# or
# make VERBOSE=1

