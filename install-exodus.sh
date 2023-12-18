# script to install seacas-exodus for build with LaGriT
# See detailed instructions at https://github.com/sandialabs/seacas
# Seacas Contact Gregory Sjaardema gsjaardema@gmail.com
#
# LaGriT does not need full full seacas but does require
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

# ==== GET SOURCE  ================================
mkdir -p ${SEACAS_INSTALL_DIR}
cd ${SEACAS_INSTALL_DIR}

git clone https://github.com/sandialabs/seacas.git 
cd seacas && export ACCESS=`pwd`

# ----------------------------------------------------------------------------------------
# Checkout a seacas tag that is known to work
# This is no longer needed for a linux build 
# git reset --hard v2023-11-27
# ----------------------------------------------------------------------------------------
# Suggestions from Greg to fix Exodus possible build errors
#
# Add options to TPLs/seacas/TPL/netcdf/runcmake.sh
#
#  cmake .. -DCMAKE_C_COMPILER:FILEPATH=${CC} \
#           -DENABLE_PNETCDF:BOOL=${MPI} \
#  ADD THESE OPTIONS:
#         -DENABLE_PLUGINS:BOOL=OFF \
#         -DENABLE_MULTIFILTERS:BOOL=NO \
#         -DENABLE_NCZARR_FILTERS:BOOL=OFF \
#         -DENABLE_TESTS:BOOL=OFF \
#
# FIX ERROR during lagrit build: routines or libs not found
# Modify ../cmake-exodus to add curl for linux systems
# Modify line (may leave curl off for some sytems, needed for linux)
#
#   EXTRA_LIB="-DSeacas_EXTRA_LINK_FLAGS=z;dl -DSEACASExodus_ENABLE_SHARED:BOOL=OFF"
# to add curl
#   EXTRA_LIB="-DSeacas_EXTRA_LINK_FLAGS=curl;z;dl -DSEACASExodus_ENABLE_SHARED:BOOL=OFF"
# ----------------------------------------------------------------------------------------


# FIX TPL/netcdf/runcmake.sh and cmake-exodus as described above
sed -i '69i \         -DENABLE_PLUGINS:BOOL=OFF \\\n         -DENABLE_MULTIFILTERS:BOOL=NO \\\n         -DENABLE_NCZARR_FILTERS:BOOL=OFF \\\n         -DENABLE_TESTS:BOOL=OFF \\' TPL/netcdf/runcmake.sh
sed -ie 's/DSeacas_EXTRA_LINK_FLAGS=z;dl/DSeacas_EXTRA_LINK_FLAGS=curl;z;dl/g' cmake-exodus

# Special handling for Cygwin
if [[ `uname -s` == *"CYGWIN"* ]]; then
	BUILD=NO ./install-tpl.sh;
	sed -i 's/defined(_WIN32) || defined(__CYGWIN__)/defined(_WIN32)/g' `ls -t -d TPL/zlib-* | head -1`/gzguts.h;
	export DOWNLOAD=NO;
fi;

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

