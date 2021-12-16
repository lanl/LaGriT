EXO_COMMIT_HASH=ba60a4d19516c433967581fbb8525c56f03b7c3e
EXO_BUILD_DIR=./TPLs/

mkdir -p ${EXO_BUILD_DIR}
git clone https://github.com/gsjaardema/seacas.git ${EXO_BUILD_DIR}/seacas || echo "Already cloned!"
cd ${EXO_BUILD_DIR}/seacas

git checkout ${EXO_COMMIT_HASH}
export ACCESS=`pwd`

if [[ `uname -s` == *"CYGWIN"* ]]; then
		BUILD=NO ./install-tpl.sh;
		sed -i 's/defined(_WIN32) || defined(__CYGWIN__)/defined(_WIN32)/g' `ls -t -d TPL/zlib-* | head -1`/gzguts.h;
		export DOWNLOAD=NO;
fi;

./install-tpl.sh
cd TPL/

# Exodus build parameters
export CGNS=OFF
export MATIO=OFF
export SHARED=NO
export LG_DIR=`pwd`
export NEEDS_ZLIB=YES
export GNU_PARALLEL=OFF
export BUILD=YES

../cmake-exodus -DFORTRAN=YES

make && make install