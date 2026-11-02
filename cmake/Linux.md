# Linux Helps and Hints

Versions of LaGriT have been developed and tested on our local Linux Ubuntu servers using GNU compilers for many years. 
This platform is very stable and should not require any special changes to cmake or associated files.

### LaGriT build without Exodus

No known issues.

### LaGriT build with Exodus

Use `cmake .. -DLAGRIT_BUILD_EXODUS=ON -DLAGRIT_FETCH_EXODUS=ON`; see [README.md](README.md) for details.

The Seacas build fixes suggested by Greg at Seacas are applied automatically by cmake/seacas_configure.cmake:
TPL/netcdf/runcmake.sh gets plugins, multifilters, nczarr filters, and tests turned off,
and cmake-exodus gets `curl` added to `Seacas_EXTRA_LINK_FLAGS` (static NetCDF needs -lcurl on Linux).
LaGriT links with `-lcurl -static-libgfortran -static-libgcc` on Linux.

Exodus for LaGriT requires Exodus Fortran libraries, not always installed on systems.
For this reason cmake builds Exodus locally in `build/_deps/seacas` rather than using system versions that may not work.
Use `-DEXODUS_ROOT=/path/to/seacas` to use a different install.

See detailed instructions at https://github.com/sandialabs/seacas
Seacas Contact Gregory Sjaardema gsjaardema@gmail.com
