# MacOS Helps and Hints

LaGriT has been developed and used on MacOS machines for many years, using both Clang and GNU compilers.
For MacOS compilers and systems newer than those used in 2019, there may be issues finding the correct compilers.
See below for known issues.

LaGriT V3.3.3 builds with and without Exodus on MacOS Intel and M1 machines on Monterey, Ventura, and Sonoma.

LaGriT builds without Exodus are usually successful and recommended before building a version with Exodus.

### Compiler Errors

LaGriT uses C, C++, and Fortran compilers. You may have to set the correct compilers for your build environment. The build may fail for newer models due to differences in naming conventions for both file structure and compilers. 

Find or install your preferred compilers and set your environment, for example:

```
export CC=/opt/local/bin/gcc-mp-12
export CXX=/opt/local/bin/g++-mp-12
export FC=/opt/local/bin/gfortran-mp-12
```

On newer machines, the problem is that mac / osx has stolen the name gcc and g++ for the Apple clang compiler.   And they don't provide fortran.
There are quite a few ways to work around this issue, but in any given case you have to be careful to make sure things are self-consistent.

Below are instructions for installing a set of compilers using MacPorts (Homebrew has recent issues). 

See also LaGriT/MAC_EnvironmentalVar.sh for setting your compilers.


## Fortran Compiler Not Found or Incompatible with C compilers

For LaGriT V3.3.3 on MacOS we had problems with compilers installed with Homebrew. Builds for LaGriT and Exodus were successful on Ventura and Sonoma using MacPorts and the following steps.

To use Macports you will need to install XCode command line tools.
Go to:https://developer.apple.com/download/all/ and select Sonoma Xcode 15.1, Ventura Xcode 14.3, or Early macOS 13.2.

Download and install macports:https://www.macports.org/install.php

Once Downloaded, Check to see if port command is found:
```
% port version
Version: 2.8.1
``` 

If port not found, set path. This should be in your appropriate login file such as .bash_profile.

```
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
```

selfupdate with verbose will take some time:
```sudo -E port -v selfupdate```

Use macports, Install CMake, GNU make, GNU compilers, (and wget for Exodus). Use the -E option to preserve environment varables such as your Proxies.

```
sudo -E port install cmake
sudo -E port install gmake
sudo -E port install gcc12
sudo -E port install wget
```

When done should have:

```
cmake version 3.24.4
gmake version 4.4.1
gcc version Apple Clang version 14.0.3
g++ version Apple Clang version 14.0.3
gfortran version 12.2.0
wget version 1.21.4
```

Make sure Cmake configurations for LaGriT and Exodus are using these tools and compilers.


 
## Exodus Build Fails with CommandLineTools Error

Usually these strange permission errors are related to XCode resource management and parallel builds.   In the older XCode (14.x) these were all resolved with the gnu tools (particularly gmake) coming from macports. 

```
[ 93%] Built target h5dump
[ 93%] Built target h5watch
[ 93%] Built target h5perf_serial
make[1]: /Library/Developer/CommandLineTools/usr/bin/make: Permission denied
```


Switch MacPort installed gmake to 4.4 release.
See also: https://trac.macports.org/wiki/howto/InstallingOlderPort

```
git clone --single-branch https://github.com/macports/macports-ports.git
cd macports-ports
find . -name gmake
```

Now either in a web browser or from the command line identify the hash of the version you want.
Then checkout that commit, the commands below will checkout version 4.4

```
git checkout 4423959
cd devel/gmake
sudo -E port install
```

The tail end of the output shows that the newer version is deactivated and replaced with the older version:
```
--->  Building gmake
--->  Staging gmake into destroot
--->  Installing gmake @4.4_0
--->  Deactivating gmake @4.4.1_0
--->  Cleaning gmake
--->  Activating gmake @4.4_0
--->  Cleaning gmake
--->  Scanning binaries for linking errors
--->  No broken files found.
--->  No broken ports found.
```


If you experience a weird permission error, and you have admin permissions set. You still might not have access to the files you just installed. 

```
Error: Unable to execute port: Could not open file: /Users/l113691/macports/macports-ports/devel/gmake/Portfile
```

Suggested fix, move the directory away from root and inherited permissions:
See suggestion at: https://superuser.com/questions/598824/previous-version-of-cgal-with-macports-error-unable-to-execute-port-could-no

```
cd macports-ports/devel
sudo mv gmake /private/tmp
cd /private/tmp
sudo -E port install
```

```
gmake --version
GNU Make 4.4
```

This solution worked on MacOS Intel and M1, Ventura and Sonoma for building LaGriT and Exodus.

## New Compilers show precision error for very small numbers

The XCode 15.0 compilers are showing a precision error for very small numbers.
This does not alter the meshing results but can show differences in how diagonals are crossed and reporting statistics.

This issue was observed using the following:
```
Cmake 3.24.4
Gmake 3.81
gcc 15.0.0
gcc-mp-13
g++-mp-13
gfortran-mp-13
```

The MacPorts compilers shown above do not have this precision issue.








