#!/bin/sh
#
# run_poisson_lagrit_tests.sh [path-to-cv_dual]
#
# test_cv_dual_poisson_lagrit: cv_dual on five Poisson disk meshes from
# LaGriT's suites (test_poisson_3d cases 1 and 2, box 0.4 x 0.4 x 0.2;
# test_poisson_2d's three unit-square meshes), judged against the
# .stor files LaGriT wrote for them and against the exact volume.
# See 00README. POSIX sh, Python 3 (standard library). WORK and LOGS
# may be set to other folders.

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
if [ ! -f "./run_poisson_lagrit_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "cv_dual : $CV_DUAL"
rc=0
for m in test_3d_poi_01 test_3d_poi_02 output01_square_uniform \
         output02_square_var output03_square_var; do
    cp "inputs/$m.inp" "$WORK"/
    k=0
    for opts in \
        "-stor ${m}_keep.stor -zero_area keep -compress off" \
        "-stor $m.stor"
    do
        k=$((k + 1))
        # shellcheck disable=SC2086
        ( cd "$WORK" && "$CV_DUAL" -avs "$m.inp" $opts \
              > "$m.run$k.out" 2>&1 ) ||
            { echo "cv_dual failed on $m: $opts"; rc=1; }
        { echo "cv_dual -avs $m.inp $opts"; cat "$WORK/$m.run$k.out"; } \
            > "$LOGDIR/$m.run$k.out"
    done
done
python3 -I check_poisson_lagrit.py "$WORK" "$LOGDIR" inputs || rc=1
echo
exit $rc
