#!/bin/sh
#
# run_kernel_tests.sh [path-to-del_vor]
#
# test_kernels: del_vor_selftest (built next to del_vor) checks the
# geometry kernels against independent answers, then the closure of
# the dual cells on the lanl/voronoi meshes. See 00README. POSIX sh,
# Python 3 (standard library). WORK and LOGS may be set to other
# folders.

# Default: the del_vor built in this checkout (build/del_vor), else
# del_vor on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    DEL_VOR=$1
elif [ -n "$_root" ] && [ -x "$_root/build/del_vor" ]; then
    DEL_VOR=$_root/build/del_vor
else
    DEL_VOR=del_vor
fi
case $DEL_VOR in
    */*) [ -f "$DEL_VOR" ] &&
         DEL_VOR="$(cd "$(dirname "$DEL_VOR")" && pwd)/$(basename "$DEL_VOR")" ;;
    *)   _p=$(command -v "$DEL_VOR" 2>/dev/null) && DEL_VOR=$_p ;;
esac
if [ ! -f "$DEL_VOR" ] || [ ! -x "$DEL_VOR" ]; then
    echo "cannot run del_vor '$DEL_VOR': not an executable file," >&2
    echo "nor a command on PATH. Pass a path, e.g. ../../../build/del_vor" >&2
    exit 2
fi

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_kernel_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

SELFTEST=$(dirname "$DEL_VOR")/del_vor_selftest
if [ ! -x "$SELFTEST" ]; then
    echo "cannot run $SELFTEST (built next to del_vor)" >&2
    exit 2
fi
echo "del_vor_selftest : $SELFTEST"
rc=0
G=../test_voronoi_gold/inputs
cp "$G/mesh_2D.inp" "$G/mesh_3D.inp" "$G/simple_3D.inp" "$WORK"/
( cd "$WORK" &&
  "$SELFTEST" mesh_2D.inp mesh_3D.inp simple_3D.inp \
      > selftest.out 2>&1 ) || rc=1
cp "$WORK/selftest.out" "$WORK/selftest_samples.txt" "$LOGDIR"/
python3 -I check_kernels.py "$WORK" "$LOGDIR" || rc=1
echo
exit $rc
