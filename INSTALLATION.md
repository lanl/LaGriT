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

```sh
$ ctest
```

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
