#!/bin/sh
#
# run_options_tests.sh [path-to-del_vor]
#
# test_options: del_vor's keyword options (outputs, zero_area,
# zero_tol, compress, cv, lanl/voronoi's -type -o -compress), and
# refused commands, one case per line of cases.txt. See 00README.
# POSIX sh, Python 3 (standard library). WORK and LOGS may be set to
# other folders.

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
if [ ! -f "./run_options_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
G=../test_voronoi_gold/inputs
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name expect args about; do
    name=$(echo "$name" | tr -d ' ')
    mkdir -p "$WORK/$name"
    cp "$G/mesh_2D.inp" "$G/mesh_3D.inp" "$G/simple_3D.inp" "$WORK/$name"/
    # shellcheck disable=SC2086
    ( cd "$WORK/$name" && "$DEL_VOR" $args > run.out 2>&1
      echo $? > run.rc )
    { echo "del_vor$args"; cat "$WORK/$name/run.out"; } \
        > "$LOGDIR/$name.out"
done
python3 -I check_options.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
