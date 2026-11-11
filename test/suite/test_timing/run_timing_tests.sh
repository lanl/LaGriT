#!/bin/sh
#
# run_timing_tests.sh [path-to-del_vor]
#
# test_timing: del_vor's time per step and peak memory on meshes of
# growing size, and LaGriT's dump / stor on the same meshes. For
# development; not part of the default run_all.sh (run it with
# ONLY=timing sh run_all.sh, or alone). See 00README.
#
# Settings (environment variables):
#   LAGRIT=/path/to/lagrit   makes the meshes (if missing) and times
#                            LaGriT; without it only del_vor is timed,
#                            on meshes already in the mesh folder
#   DEL_VOR_BIG=FOLDER       big files outside git (default
#                            ~/lagrit_work/del_vor_dev/big_inputs);
#                            meshes in FOLDER/timing
#   SIZES_TET="1e4 ..."      tets per mesh (default 10000 30000 100000
#   SIZES_TRI="1e4 ..."      300000 1000000); triangles (default 10000
#                            100000 1000000); integers
#   REPEAT=3                 runs per measurement (the best is kept)
#
# Time on Claude's sandbox with the defaults: about 3 minutes (1
# minute of it making the meshes, once). 10 million tets: LaGriT's
# connect takes about a minute per million tets on a laptop; plan an
# hour for SIZES_TET up to 3e7.
# POSIX sh, Python 3 (standard library).

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
BIG=${DEL_VOR_BIG:-$HOME/lagrit_work/del_vor_dev/big_inputs}
MESHES=$BIG/timing

cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_timing_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
echo "lagrit  : ${LAGRIT:-not given (del_vor only)}"
echo "meshes  : $MESHES"
if [ -n "${LAGRIT:-}" ]; then
    case $LAGRIT in /*) ;; *) LAGRIT=$(pwd)/$LAGRIT ;; esac
    sh make_timing_meshes.sh "$LAGRIT" "$MESHES" || exit 1
fi
if ! ls "$MESHES"/*.inp > /dev/null 2>&1; then
    echo "no meshes in $MESHES: set LAGRIT=/path/to/lagrit to make them" >&2
    exit 1
fi
python3 -I timing.py "$DEL_VOR" "$MESHES" "$WORK" "$LOGDIR" "${LAGRIT:-}"
rc=$?
echo
exit $rc
