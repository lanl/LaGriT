## NOTES ##

- /Users/livingston/dev/python/tinerator/util/tpls/seacas/install/lib/

NOTE: does not build fortran bindings!

```
build_exodus() {
    _cwd=$(pwd)
    info "Building EXODUSII: ${EXODUS_SRC_DIR}"

    CC="${CC:-$(which gcc)}"
    CXX="${CXX:-$(which g++)}"
    FC="${FC:-$(which gfortran)}"
    EXO_GIT_HASH="${EXO_GIT_HASH:-d55c8ff06dd6875bae5bcbf86db7ea2e190ca901}"

    SEACAS_SRC_DIR=$1/seacas
    SEACAS_BUILD_DIR=${SEACAS_SRC_DIR}/build
    SEACAS_DIR=${SEACAS_SRC_DIR}/install

    TPL_ROOT_DIR="${TPL_ROOT_DIR:-$_script_dir}"

    PYTHON_PREFIX="$(cd $(dirname $(which python))/../; pwd)"
    CONDA_PREFIX="${CONDA_PREFIX:-$PYTHON_PREFIX}"

    info "Building EXODUSII: ${SEACAS_SRC_DIR}"

    git clone https://github.com/gsjaardema/seacas.git ${SEACAS_SRC_DIR}
    cd ${SEACAS_SRC_DIR} && git checkout ${EXO_GIT_HASH}
    mkdir -p $SEACAS_BUILD_DIR && mkdir -p $SEACAS_DIR && cd $SEACAS_BUILD_DIR

    cmake  \
        -D SEACASProj_ENABLE_ALL_PACKAGES:BOOL=OFF \
        -D SEACASProj_ENABLE_SEACASExodus:BOOL=ON \
        -D CMAKE_INSTALL_PREFIX:PATH=${SEACAS_DIR} \
        -D CMAKE_BUILD_TYPE=Debug \
        -D BUILD_SHARED_LIBS:BOOL=ON \
        \
        -D CMAKE_CXX_COMPILER:FILEPATH=${CXX} \
        -D CMAKE_C_COMPILER:FILEPATH=${CC} \
        -D CMAKE_Fortran_COMPILER:FILEPATH=${FC} \
        -D SEACASProj_SKIP_FORTRANCINTERFACE_VERIFY_TEST:BOOL=ON \
        -D TPL_ENABLE_Netcdf:BOOL=ON \
        -D TPL_ENABLE_HDF5:BOOL=ON \
        -D TPL_ENABLE_Matio:BOOL=OFF \
        -D TPL_ENABLE_MPI=OFF \
        -D TPL_ENABLE_CGNS:BOOL=OFF \
        \
        -D Netcdf_LIBRARY_DIRS:PATH=${CONDA_PREFIX}/lib \
        -D Netcdf_INCLUDE_DIRS:PATH=${CONDA_PREFIX}/include \
        -D HDF5_ROOT:PATH=${CONDA_PREFIX} \
        -D HDF5_NO_SYSTEM_PATHS=ON \
    ${SEACAS_SRC_DIR}

    make && make install

    export PYTHONPATH=${SEACAS_SRC_DIR}/install/lib:${PYTHONPATH}

    cd $_cwd
}
```


# Building LaGriT

```sh
$ git clone https://github.com/lanl/LaGriT.git
$ cd LaGriT/
$ mkdir build/ && cd build/
$ cmake .. && make
```

```sh
cmake .. -D LaGriT_BUILD_STATIC=ON
```

-DCMAKE_BUILD_TYPE=Debug
-DCMAKE_BUILD_TYPE=Release

cmake .. -DLaGriT_BUILD_STATIC=OFF -DCMAKE_BUILD_TYPE=Debug -D CMAKE_INSTALL_PREFIX=/Users/livingston/dev/lanl/LaGriT/LaGriT/install && make && make install

# Compiling Exodus

```sh
$ git clone https://github.com/gsjaardema/seacas.git
$ cd seacas && export ACCESS=`pwd`
$ mkdir build/ && cd build/
$ sh ../cmake-exodus
$     cmake  \
        -D SEACASProj_ENABLE_ALL_PACKAGES:BOOL=OFF \
        -D SEACASProj_ENABLE_SEACASExodus:BOOL=ON \
        -D CMAKE_INSTALL_PREFIX:PATH=${ACCESS}/install \
        -D CMAKE_BUILD_TYPE=Debug \
        -D BUILD_SHARED_LIBS:BOOL=ON \
        \
        -D CMAKE_CXX_COMPILER:FILEPATH=${CXX} \
        -D CMAKE_C_COMPILER:FILEPATH=${CC} \
        -D CMAKE_Fortran_COMPILER:FILEPATH=${FC} \
        -D SEACASProj_SKIP_FORTRANCINTERFACE_VERIFY_TEST:BOOL=ON \
        -D TPL_ENABLE_Netcdf:BOOL=ON \
        -D TPL_ENABLE_HDF5:BOOL=ON \
        -D TPL_ENABLE_Matio:BOOL=OFF \
        -D TPL_ENABLE_MPI=OFF \
        -D TPL_ENABLE_CGNS:BOOL=OFF \
        \
        -D Netcdf_LIBRARY_DIRS:PATH="$(nc-config --prefix)/lib" \
        -D Netcdf_INCLUDE_DIRS:PATH="$(nc-config --prefix)/include"
        #-D HDF5_ROOT:PATH=${CONDA_PREFIX} \
        #-D HDF5_NO_SYSTEM_PATHS=ON \
    ${ACCESS}
```
