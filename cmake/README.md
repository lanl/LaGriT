# CMake Build System

The CMake build system is a way of enabling LaGriT to
be cross-platform, cross-architecture, and cross-compiler.

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
