# Linux Helps and Hints

Versions of LaGriT have been developed and tested on our local Linux Ubuntu servers using GNU compilers for many years. 
This platform is very stable and should not require any special changes to cmake or associated files.

### LaGriT build without Exodus

No known issues.

### LaGriT V3.3.3 build with Exodus

**install-exodus.sh**

Modifications for Exodus install, cmake, build were suggested by Greg at Seacas.

```
# FIX TPL/netcdf/runcmake.sh and cmake-exodus as described above
sed -i '69i \         -DENABLE_PLUGINS:BOOL=OFF \\\n         -DENABLE_MULTIFILTERS:BOOL=NO \\\n         -DENABLE_NCZARR_FILTERS:BOOL=OFF \\\n         -DENABLE_TESTS:BOOL=OFF \\' TPL/netcdf/runcmake.sh
sed -ie 's/DSeacas_EXTRA_LINK_FLAGS=z;dl/DSeacas_EXTRA_LINK_FLAGS=curl;z;dl/g' cmake-exodus
```

**CMakeLists.txt**

Exodus for LaGriT requires Exodus Fortran libraries, not always installed on systems.
For this reason we recommend installing Exodus in the local directory "TPLs".
CMakeLists.txt will look for needed libraries locally rather than system versions that may not work.

```
set(EXODUS_ROOT "${CMAKE_SOURCE_DIR}/TPLs/seacas")
set(Exodus_INCLUDE_DIR "${EXODUS_ROOT}/include")
set(Exodus_LIBRARIES "${EXODUS_ROOT}/lib")
```

See detailed instructions at https://github.com/sandialabs/seacas
Seacas Contact Gregory Sjaardema gsjaardema@gmail.com
