#!/bin/sh
#
# run_poisson_lagrit_tests.sh [path-to-del_vor]
#
# test_poisson_lagrit: del_vor on five Poisson disk meshes from
# LaGriT's suites (test_poisson_3d cases 1 and 2, box 0.4 x 0.4 x 0.2;
# test_poisson_2d's three unit-square meshes), judged against the
# .stor files LaGriT wrote for them and against the exact volume.
# See 00README. POSIX sh, Python 3 (standard library). WORK and LOGS
# may be set to other folders.

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
if [ ! -f "./run_poisson_lagrit_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
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
        ( cd "$WORK" && "$DEL_VOR" -avs "$m.inp" $opts \
              > "$m.run$k.out" 2>&1 ) ||
            { echo "del_vor failed on $m: $opts"; rc=1; }
        { echo "del_vor -avs $m.inp $opts"; cat "$WORK/$m.run$k.out"; } \
            > "$LOGDIR/$m.run$k.out"
    done
done
python3 -I check_poisson_lagrit.py "$WORK" "$LOGDIR" inputs || rc=1
echo
exit $rc
