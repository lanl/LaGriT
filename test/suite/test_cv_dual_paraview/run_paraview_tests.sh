#!/bin/sh
#
# run_paraview_tests.sh [path-to-del_vor]
#
# test_paraview: the ParaView files (paraview ROOT), one case per
# line of cases.txt, read back by vtu_read.py. See 00README.
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
if [ ! -f "./run_paraview_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
# meshes made by other suites' generators (no random numbers)
python3 -I ../test_dfn/make_dfn_inputs.py "$WORK/gen" \
    > "$LOGDIR/make_inputs.out" < /dev/null || exit 1
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name mesh args about; do
    name=$(echo "$name" | tr -d ' ')
    mesh=$(echo "$mesh" | tr -d ' ')
    case $mesh in gen/*) src=$WORK/$mesh ;; *) src=$mesh ;; esac
    mkdir -p "$WORK/$name"
    cp "$src" "$WORK/$name/"
    m=$(basename "$mesh")
    # shellcheck disable=SC2086
    ( cd "$WORK/$name" && "$DEL_VOR" -avs "$m" -stor "$name.stor" \
          -paraview "$name" -report "$name.report" $args \
          > run.out 2>&1 < /dev/null
      echo $? > run.rc )
    { echo "del_vor -avs $m -stor $name.stor -paraview $name -report $name.report $args"
      cat "$WORK/$name/run.out"; } > "$LOGDIR/$name.out"
done
python3 -I check_paraview.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
