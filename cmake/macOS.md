# MacOS Helps and Hints

LaGriT has been developed and used on MacOS machines for many years, using both Clang and GNU compilers.
For MacOS compilers and systems newer than those used in 2019, there may be issues finding the correct compilers.
See below for known issues.

LaGriT V3.3.3 builds with and without Exodus on MacOS Intel and M1 machines on Monterey, Ventura, and Sonoma.

LaGriT builds without Exodus are usually successful and recommended before building a version with Exodus.

See **LaGriT/MAC-install-exodus.sh** which works for brew as well as macports. This script finds and sets compiler paths due to location and naming changes between homebrew and macports.

See also **LaGriT/MAC_EnvironmentalVar.sh** for setting your compilers.


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

Below are instructions for installing a set of compilers using MacPorts (Homebrew also works). 



## Fortran Compiler Not Found or Incompatible with C compilers (macports example)

For LaGriT V3.3.3 on MacOS the Builds for LaGriT and Exodus were successful on Ventura and Sonoma using MacPorts and Homebrew. This example uses macports.

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


 
## Exodus Build Fails with CommandLineTools Error (macports example)

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

## Exodus Build Fails with a make error that mentions an "sdk"

The error looks like this using macports:
```
/Library/Developer/CommandLineTools/SDKs/MacOSX15.0.sdk/usr/include/_wchar.h:90,
             	from /Library/Developer/CommandLineTools/SDKs/MacOSX15.0.sdk/usr/include/wchar.h:67,
             	from /opt/local/include/gcc14/c++/cwchar:44,
             	from /opt/local/include/gcc14/c++/bits/postypes.h:40,
             	from /opt/local/include/gcc14/c++/bits/char_traits.h:42,
             	from /opt/local/include/gcc14/c++/string:42,
             	from /Users/elm/packages/amanzi/repos/amanzi-1.5.1/src/error_handling/exceptions.hh:13,
             	from /Users/elm/packages/amanzi/repos/amanzi-1.5.1/src/error_handling/dbc.hh:13,
             	from /Users/elm/packages/amanzi/repos/amanzi-1.5.1/src/error_handling/dbc.cc:10:
/opt/local/lib/gcc14/gcc/aarch64-apple-darwin23/14.2.0/include-fixed/stdio.h:83:8: error: 'FILE' does not name a type
   83 | extern FILE *__stdinp;
  	|    	^~~~
```

or this using brew:
```
Building CXX object CMakeFiles/cmTC_c85e6.dir/testCXXCompiler.cxx.o
    /Library/Developer/CommandLineTools/usr/bin/g++   “-I/opt/homebrew/opt/llvm/include”  -arch arm64 -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX15.0.sdk -mmacosx-version-min=14.7 -MD -MT CMakeFiles/cmTC_c85e6.dir/testCXXCompiler.cxx.o -MF CMakeFiles/cmTC_c85e6.dir/testCXXCompiler.cxx.o.d -o CMakeFiles/cmTC_c85e6.dir/testCXXCompiler.cxx.o -c /Users/jhyman/src/LaGriT/TPLs/seacas/TPL/netcdf/netcdf-c/build/CMakeFiles/CMakeScratch/TryCompile-sEGBRU/testCXXCompiler.cxx
    clang++: error: no such file or directory: '“-I/opt/homebrew/opt/llvm/include”'
    gmake[1]: *** [CMakeFiles/cmTC_c85e6.dir/build.make:79: CMakeFiles/cmTC_c85e6.dir/testCXXCompiler.cxx.o] Error 1
    gmake[1]: Leaving directory '/Users/jhyman/src/LaGriT/TPLs/seacas/TPL/netcdf/netcdf-c/build/CMakeFiles/CMakeScratch/TryCompile-sEGBRU'
    gmake: *** [Makefile:127: cmTC_c85e6/fast] Error 2
```

**This error happens because newer Mac's require a newer version of x-code Command Line Tools to be installed. These tools don't work nicely with gcc-14. The sdk's for the newer tools are incompatible.** 

To fix the error:

1. Verify that you have an older MacOSX14.sdk installation
   ```
   ls /Library/Developer/CommandLineTools/SDKs
   ```
2. export older sds:
   ```
   export SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk/
   ```
 
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


## Compilers using Homebrew

Prior to installing compilers, you must have a working git, https://github.com/git-guides/install-git, and homebrew, https://github.com/homebrew, install.

```
brew install cmake
brew install make
brew install gcc
brew install wget
```
It may be necessary to export your compiler path as MACs like to use Clang whenever possible

```
export PATH=/usr/local/bin:$PATH
```

It some cases, creating an alias also helps:
```
alias gcc="gcc-13"
alias g++="g++-13"
alias cc="cc-13"
alias c++="c++-13"
```

When you run 
```
gcc –version
```
it should now list homebrew rather than clang.

## Clang and Implicit-int error

Newer versions of Clang have moved away from implicit-int and have begun reporting it as an error rather then a warning. It may be necessary to use the flag

```
-Wno-error=implicit-int
```
