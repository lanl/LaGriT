# MacOS Helps and Hints

LaGriT has been developed and used on MacOS machines for many years, using both Clang and GNU compilers.
For MacOS compilers and systems newer than those used in 2019, there may be issues.

LaGriT V3.3.3 builds with and without Exodus on MacOS Intel and M1 machines on Monterey, Ventura, and Sonoma.
As long as the build environment is set up correctly, the LaGriT build should be easy.

Newer compilers seem a bit tricky to setup up on the mac. LaGriT builds without Exodus are easy using cmake, and it is recommended.
The Exodus install and build will need a good compiler setup, as well as a good version of cmake or gmake.

### Compiler Errors

LaGriT uses C, C++, and Fortran compilers. You may have to set the correct compilers for your build environment. 
Find or install your preferred compilers and set your environment, for example:

```
export COMPILER=MacPorts
export CC=/opt/local/bin/gcc-mp-12
export CXX=/opt/local/bin/g++-mp-12
export FC=/opt/local/bin/gfortran-mp-12
```

See also ../MAC_EnvironmentalVar.sh for setting your compilers.
You can check to ensure they are there using “env” on the command line and looking for FC, CC, and CXX to be set.


### New Compilers show precision error for very small numbers

The XCode (14.x)? compilers are showing a precision error for very small numbers.
This does not alter the meshing results but can show differences in how diagonals are crossed and reporting statistics.

The MacPorts compilers shown above to not have this precision issue.
 
## Exodus Build Fails

The primary thing is making sure the right compilers are found and used. 
If compilers and tools are not setup properly, the Exodus build will fail at some point with error similar to:

```
[ 93%] Built target h5dump
[ 93%] Built target h5watch
[ 93%] Built target h5perf_serial
make[1]: /Library/Developer/CommandLineTools/usr/bin/make: Permission denied
```

Usually these strange permission errors are related to XCode resource management and parallel builds.   In the older XCode (14.x) these were all resolved with the gnu tools (particularly gmake) coming from macports. 

Install make from macports, and avoid /usr/bin/make as it doesn't work properly.
There's an additional catch for install make from macports.  It may be necessary to downgrade from current version 4.4.1 to 4.4.0.   

This solution worked on MacOS Intel and M1, Ventura and Sonoma. 

```
uninstall macports
uninstall old command line tools
install new command line tools (14.x Ventura, 15.x Sonoma)
install macports

These are the install commands used (wget is needed for Exodus):

sudo -E port install cmake
sudo -E port install gmake
sudo -E port install gcc12
sudo -E port install wget
```

When you install the latest macports, it will likely update your path in your account to include /opt/local/bin, and so it will have the port command in its path. Check your .bash_profile, .zprofile, or login file as appropriate:

```
export PATH="/opt/local/libexec/gnubin:/opt/local/bin:/opt/local/sbin:$PATH"
```




