set -e
EXO_BUILD_DIR="$(pwd)/TPLs/build/"
EXO_INSTALL_DIR="$(pwd)/TPLs/install/"

#EXO_COMMIT_HASH=ba60a4d19516c433967581fbb8525c56f03b7c3e
EXO_COMMIT_HASH=v2021-10-11

mkdir -p ${EXO_BUILD_DIR}
mkdir -p ${EXO_INSTALL_DIR}

SEACAS_DIR=${EXO_BUILD_DIR}/seacas

git clone https://github.com/gsjaardema/seacas.git ${SEACAS_DIR} || echo "Already cloned"

cd ${SEACAS_DIR} && git checkout ${EXO_COMMIT_HASH} && export ACCESS=`pwd`

export FORTRAN=YES
export CGNS=OFF
export MATIO=OFF
export SHARED=YES
export LG_DIR=`pwd`
export NEEDS_ZLIB=YES
export GNU_PARALLEL=OFF
export BUILD=YES
export INSTALL_PATH=${EXO_INSTALL_DIR}

if [[ `uname -s` == *"CYGWIN"* ]]; then
		BUILD=NO ./install-tpl.sh;
		sed -i 's/defined(_WIN32) || defined(__CYGWIN__)/defined(_WIN32)/g' `ls -t -d TPL/zlib-* | head -1`/gzguts.h;
		export DOWNLOAD=NO;
fi;

./install-tpl.sh
cd TPL/
../cmake-exodus -DFORTRAN=YES
make && make install

#cmake .. \
#    -D SEACASProj_ENABLE_ALL_PACKAGES:BOOL=OFF \
#    -D SEACASProj_ENABLE_SEACASExodus:BOOL=ON \
#    -D CMAKE_INSTALL_PREFIX:PATH=${EXO_INSTALL_DIR} \
#    -D CMAKE_BUILD_TYPE=Debug \
#    -D BUILD_SHARED_LIBS:BOOL=ON \
#    -D SEACASProj_SKIP_FORTRANCINTERFACE_VERIFY_TEST:BOOL=ON \
#    -D TPL_ENABLE_Netcdf:BOOL=ON \
#    -D TPL_ENABLE_HDF5:BOOL=ON \
#    -D TPL_ENABLE_Matio:BOOL=OFF \
#    -D TPL_ENABLE_MPI=OFF \
#    -D TPL_ENABLE_CGNS:BOOL=OFF
#    #\
#    #-D Netcdf_LIBRARY_DIRS:PATH=${CONDA_PREFIX}/lib \
#    #-D Netcdf_INCLUDE_DIRS:PATH=${CONDA_PREFIX}/include \
#    #-D HDF5_ROOT:PATH=${CONDA_PREFIX} \
#    #-D HDF5_NO_SYSTEM_PATHS=ON \

make && make install