set -e
set -x

# ==== USER SETTINGS ==============================
EXO_BUILD_DIR=${EXO_BUILD_DIR:-"$(pwd)/TPLs/build/"}
EXO_INSTALL_DIR=${EXO_INSTALL_DIR:-"$(pwd)/TPLs/install/"}
EXO_COMMIT_HASH=${EXO_COMMIT_HASH:-v2021-10-11}
# =================================================

mkdir -p ${EXO_BUILD_DIR}
mkdir -p ${EXO_INSTALL_DIR}

SEACAS_DIR=${EXO_BUILD_DIR}/seacas

git clone https://github.com/gsjaardema/seacas.git ${SEACAS_DIR} || echo "Already cloned"

cd ${SEACAS_DIR} && git checkout ${EXO_COMMIT_HASH} && export ACCESS=`pwd`

# User-changable variables
export INSTALL_PATH=${EXO_INSTALL_DIR}
export SHARED=NO # Build shared libraries?

# These should not need to be changed
export FORTRAN=YES
export CGNS=OFF
export MATIO=OFF
export NEEDS_ZLIB=YES
export GNU_PARALLEL=OFF
export BUILD=YES

# Special handling for Cygwin
if [[ `uname -s` == *"CYGWIN"* ]]; then
	BUILD=NO ./install-tpl.sh;
	sed -i 's/defined(_WIN32) || defined(__CYGWIN__)/defined(_WIN32)/g' `ls -t -d TPL/zlib-* | head -1`/gzguts.h;
	export DOWNLOAD=NO;
fi;

# Build Exodus TPLs - HDF5, NetCDF, ZLIB
./install-tpl.sh
cd TPL/

# Build Exodus
../cmake-exodus -DFORTRAN=YES
make && make install
