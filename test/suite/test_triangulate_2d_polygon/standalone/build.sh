#!/bin/sh
# build.sh [path/to/triangulate_lg.f]   (default ../../../src/triangulate_lg.f)
#
# Builds tri_driver: maketriangles_lg and its helpers taken straight
# from triangulate_lg.f, linked with a stand-in for LaGriT's memory
# manager. No LaGriT needed. Same flags as the LaGriT build.
SRC=${1:-../../../src/triangulate_lg.f}
FC=${FC:-gfortran}
CC=${CC:-cc}
cd "$(dirname "$0")" || exit 2
[ -f "$SRC" ] || { echo "no $SRC (give the path to triangulate_lg.f)"; exit 2; }
# everything from maketriangles_lg to the end of the file
sed -n '/^      subroutine maketriangles_lg/,$p' "$SRC" > tri_core.f
$CC -c mm_stub.c -o mm_stub.o || exit 1
$FC -fcray-pointer -fdefault-integer-8 -std=legacy -fno-sign-zero \
    -fno-range-check -O2 tri_driver.f tri_core.f mm_stub.o \
    -o tri_driver || exit 1
echo "built tri_driver from $SRC"
