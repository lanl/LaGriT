# script to install seacas-exodus for build with LaGriT
# See full instructions at https://github.com/sandialabs/seacas
# Seacas Contact Gregory Sjaardema gsjaardema@gmail.com
#
# LaGriT does not need full full seacas but does require
# exodus and associated libs:
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

mkdir -p ${SEACAS_INSTALL_DIR}
cd ${SEACAS_INSTALL_DIR}

git clone https://github.com/sandialabs/seacas.git 
cd seacas && export ACCESS=`pwd`

# Special handling for Cygwin
if [[ `uname -s` == *"CYGWIN"* ]]; then
	BUILD=NO ./install-tpl.sh;
	sed -i 's/defined(_WIN32) || defined(__CYGWIN__)/defined(_WIN32)/g' `ls -t -d TPL/zlib-* | head -1`/gzguts.h;
	export DOWNLOAD=NO;
fi;

# Install needed third party libraries (TPLs)  
# These flags are recommended for LaGriT
CGNS=NO MATIO=NO GNU_PARALLEL=NO FMT=NO SHARED=NO NEEDS_ZLIB=YES ./install-tpl.sh 

# if netcdf fails, try turning off new features
# modify seacas/TPL/netcdf/runcmake.sh to look like this
#
#          -DCMAKE_INSTALL_LIBDIR:PATH=lib \
#          -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
#          -DENABLE_NETCDF_4:BOOL=ON \
#      -DENABLE_PLUGINS:BOOL=OFF \
#      -DENABLE_MULTIFILTERS:BOOL=NO \
#      -DENABLE_NCZARR_FILTERS:BOOL=OFF \
#      -DENABLE_TESTS:BOOL=OFF \
#          -DENABLE_PNETCDF:BOOL=${MPI} \
#          -DENABLE_CDF5=ON \
#          -DENABLE_MMAP:BOOL=ON \
#
# start again outside the script
# copy and paste remaining calls
# cd TPLs/seacas && export ACCESS=`pwd`
# CGNS=NO MATIO=NO GNU_PARALLEL=NO FMT=NO SHARED=NO NEEDS_ZLIB=YES ./install-tpl.sh

# Create cmake files for Exodus
# You can edit the cmake-exodus file to adjust compilers and settings
# FORTRAN must be set to YES

cd $ACCESS
mkdir build && cd build
FORTRAN=YES SHARED=NO ../cmake-exodus

# Build Exodus
# LaGriT will use files in lib and include
make && make install

