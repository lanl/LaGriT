# Building LaGriT

LaGriT is built using [CMake](http://cmake.org/).

CMake is a cross-platform, open-source, open-source build system. It is used to generate build files that specify how to compile and link executables.

## Example Build Command

### Step 1: Configure the CMake Build Directory

In this step, CMake generates the files necessary to build LaGriT. By default (on Linux and macOS), what this means is that CMake will generate *Unix Makefiles* (the "generator") which are then used to build the project in a later step.

```bash
# From within the root "./LaGriT/" folder:

cmake \
    -S ./                       \ # Source directory
    -B ./build/                 \ # Build directory
    -G "Unix Makefiles"         \ # (OPTIONAL) Generator name
    -D LaGriT_BUILD_STATIC=ON   \ # (OPTIONAL) Builds LaGriT as a static binary
    -D Exodus_ROOT="/path/"       # (OPTIONAL) Build LaGriT with ExodusII support
```
After running this command, if you look in the "build/" directory, you will see the Makefiles that CMake generated. If you used a different generator, like "Visual Studio 17 2022", you will see a Visual Studio project file.

If you use a generator like Visual Studio or XCode, you can run:

```bash
cmake --open ./build/
```

and Visual Studio/XCode/etc. will open the generated project file. You can then compile, debug, and run the project from within your IDE. Neat!

### Step 2: Build LaGriT

In this step, CMake uses the generator ("Unix Makefiles", in this case) to build the project with the pre-generated build files.

```bash
# From within the root "./LaGriT/" folder:

cmake \
    --build ./build/    \ # Build directory
    --config Debug      \ # (OPTIONAL) Build configuration (Debug or Release)
    --parallel 16         # (OPTIONAL) Number of parallel compile jobs to run
```

## Changing the Generator

CMake doesn't actually directly build the code: it generates *build files*, which are used by the build system to actually build the code.

The build system can be specified by the `-G <generator-name>` flag. You can see a list of available generators by running `cmake --help-command-list`.

On the author's machine, the following generators are available:

```text
Generators
* Unix Makefiles               = Generates standard UNIX makefiles.
  Ninja                        = Generates build.ninja files.
  Ninja Multi-Config           = Generates build-<Config>.ninja files.
  Watcom WMake                 = Generates Watcom WMake makefiles.
  Xcode                        = Generate Xcode project files.
  CodeBlocks - Ninja           = Generates CodeBlocks project files.
  CodeBlocks - Unix Makefiles  = Generates CodeBlocks project files.
  CodeLite - Ninja             = Generates CodeLite project files.
  CodeLite - Unix Makefiles    = Generates CodeLite project files.
  Eclipse CDT4 - Ninja         = Generates Eclipse CDT 4.0 project files.
  Eclipse CDT4 - Unix Makefiles= Generates Eclipse CDT 4.0 project files.
  Kate - Ninja                 = Generates Kate project files.
  Kate - Unix Makefiles        = Generates Kate project files.
  Sublime Text 2 - Ninja       = Generates Sublime Text 2 project files.
  Sublime Text 2 - Unix Makefiles
                               = Generates Sublime Text 2 project files.
```

On Windows PCs with Visual Studio, CMake can generate Visual Studio project files.
