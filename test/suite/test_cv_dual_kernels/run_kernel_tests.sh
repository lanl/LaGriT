#!/bin/sh
#
# run_kernel_tests.sh [path-to-cv_dual]
#
# test_cv_dual_kernels: cv_dual_selftest (built next to cv_dual) checks the
# geometry kernels against independent answers, then the closure of
# the dual cells on the lanl/voronoi meshes. See 00README. POSIX sh,
# Python 3 (standard library). WORK and LOGS may be set to other
# folders.

# Default: the cv_dual built in this checkout (build/cv_dual), else
# cv_dual on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    CV_DUAL=$1
    # from LaGriT's run_all.sh: lagrit's path; cv_dual is beside it
    case $(basename "$1") in
        cv_dual*) ;;
        *) CV_DUAL=$(dirname "$1")/cv_dual ;;
    esac
elif [ -n "$_root" ] && [ -x "$_root/build/cv_dual" ]; then
    CV_DUAL=$_root/build/cv_dual
else
    CV_DUAL=cv_dual
fi
case $CV_DUAL in
    */*) [ -f "$CV_DUAL" ] &&
         CV_DUAL="$(cd "$(dirname "$CV_DUAL")" && pwd)/$(basename "$CV_DUAL")" ;;
    *)   _p=$(command -v "$CV_DUAL" 2>/dev/null) && CV_DUAL=$_p ;;
esac
if [ ! -f "$CV_DUAL" ] || [ ! -x "$CV_DUAL" ]; then
    echo "cannot run cv_dual '$CV_DUAL': not an executable file," >&2
    echo "nor a command on PATH. Pass a path, e.g. ../../../build/cv_dual" >&2
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

SELFTEST=$(dirname "$CV_DUAL")/cv_dual_selftest
if [ ! -x "$SELFTEST" ]; then
    echo "cannot run $SELFTEST (built next to cv_dual)" >&2
    exit 2
fi
echo "cv_dual_selftest : $SELFTEST"
rc=0
G=../test_cv_dual_voronoi_gold/inputs
cp "$G/mesh_2D.inp" "$G/mesh_3D.inp" "$G/simple_3D.inp" "$WORK"/
( cd "$WORK" &&
  "$SELFTEST" mesh_2D.inp mesh_3D.inp simple_3D.inp \
      > selftest.out 2>&1 ) || rc=1
cp "$WORK/selftest.out" "$WORK/selftest_samples.txt" "$LOGDIR"/
python3 -I check_kernels.py "$WORK" "$LOGDIR" || rc=1
echo
exit $rc
