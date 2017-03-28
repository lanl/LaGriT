#!/usr/bin/env bash
# under development tamiller@lanl.gov
# need option to edit lagrit.h with version 
# need to create dir if not created
# need version of clean to include dir removal

# turn echo on for script development
# set -x 
export curdir=`pwd`

# load Exodus II compiler libs
module purge 
module load gcc/4.8.4
module load exodusii/6.09/gcc-4.8.2-serial
module load lagrit/3.2.0/release/gcc-4.8.4
if [ ! -v EXODUSII_HOME ]
then
  echo " "
  echo "EXODUSII_HOME undefined, load module."
  echo " "
  exit 1
fi
if [ ! -v LAGRIT_DIR ]
then
  echo " "
  echo "LAGRIT_DIR undefined, load module."
  echo " "
  exit 1
fi

# make fresh executable dir
# rm -rf $LAGRIT_DIR
# mkdir -p $LAGRIT_DIR

# make lg_util library
cd $LAGRIT_UTIL_SRC_DIR
make clean
make install

# make lagrit library
cd $LAGRIT_SRC_DIR
make clean
make install

# compile executable
gfortran -O -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o lagrit $LAGRIT_LIBS/lagrit_ulin64_o_gcc.a $LAGRIT_LIBS/util_ulin64_o_gcc.a -lm -L$EXODUSII_HOME/lib -lexodus -lexoIIv2for -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++
cp lagrit $LAGRIT_DIR/bin
echo 'LaGriT installed: '$LAGRIT_EXE

cd $curdir

/home/dharp/script/chall ees-pkg $LAGRIT_DIR
