# CMake Build System

The CMake build system is a way of enabling LaGriT to
be cross-platform, cross-architecture, and cross-compiler.

See Build and Install Instructions: https://github.com/lanl/LaGriT#readme

There's a really good tutorial-overview on CMake here, it covers how to handle multiple build configurations: https://cliutils.gitlab.io/modern-cmake/


## Code Development

You can build as Debug (-g) or as Release with the CMake flag "-D CMAKE_BUILD_TYPE=[Debug|Release]".
There's two stages in CMake; the 'configure' stage (where you run 'cmake ..') and the build stage (where you run 'make' using the Makefile created by cmake)
      
The build type can only be set in the configure stage. One way to handle building both Debug and release is to make two build directories. Here's a short guide: https://riptutorial.com/cmake/example/7357/switching-between-build-types--e-g--debug-and-release

### For workflow:

Setup a work directory for dev and debug work (example using name "debug")

```mkdir debug/ && cd debug/ && cmake .. -D CMAKE_BUILD_TYPE=Debug```

Before changing code you might first add a directory and build a release (no debug) version:

```mkdir release/ && cd release/ && cmake .. -D CMAKE_BUILD_TYPE=Release```
      
Modify files in src/ 

Compile new executable using ```make``` in the debug/ directory.
      
### Example Directory structure:
      
```
LaGriT/
├── CMakeLists.txt
└── debug/
    └── lagrit executable, Makefile built by cmake, cmake files    
└── src/
    └── file.cpp, file.f, file.c, etc
```


## Header File configuration

CMake automatically configures two header files:

- `lg_util/src/mm2000.h`
- `src/lagrit.h`

The original files have the same name, but ending with `.in`.

CMake configures the files as following. Consider a line from `lagrit.h.in`:

```fortran
      parameter      (v_major=@PROJECT_VERSION_MAJOR@)
```

Here, `@PROJECT_VERSION_MAJOR@` is a CMake variable which will be replaced by CMake.

Here is how CMake configures the file:

```cmake
configure_file(
    ${SRC_CORE}/lagrit.h.in
    ${SRC_CORE}/lagrit.h
    @ONLY
)
```

`${SRC_CORE}/lagrit.h.in` is the 'template' file, and `${SRC_CORE}/lagrit.h` is the output file. The `@ONLY` line means to **only** replace variables between the `@` symbol.

The variable above - `@PROJECT_VERSION_MAJOR@` is an instrinsic CMake variable. You can define your own. For example, CMake code within `SetBitSize.cmake` configures the `lg_util/src/mm2000.h.in` file.
