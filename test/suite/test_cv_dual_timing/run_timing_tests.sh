#!/bin/sh
#
# run_timing_tests.sh [path-to-cv_dual]
#
# test_cv_dual_timing: cv_dual's time per step and peak memory on meshes of
# growing size, and LaGriT's dump / stor on the same meshes. For
# development; LaGriT's run_all.sh runs it only with BIG=1 (BIG=1
# ONLY=test_cv_dual_timing sh run_all.sh), or run it alone. See 00README.
#
# Settings (environment variables):
#   LAGRIT=/path/to/lagrit   makes the meshes (if missing) and times
#                            LaGriT; without it only cv_dual is timed,
#                            on meshes already in the mesh folder
#   CV_DUAL_BIG=FOLDER       big files outside git (default
#                            ~/lagrit_work/del_vor_dev/big_inputs);
#                            meshes in FOLDER/timing
#   SIZES_TET="1e4 ..."      tets per mesh (default 10000 30000 100000
#   SIZES_TRI="1e4 ..."      300000 1000000); triangles (default 10000
#                            100000 1000000); integers
#   REPEAT=3                 runs per measurement (the best is kept)
#
# Time on a Linux workstation with the defaults: about 3 minutes (1
# minute of it making the meshes, once). 10 million tets: LaGriT's
# connect takes about a minute per million tets on a laptop; plan an
# hour for SIZES_TET up to 3e7.
# POSIX sh, Python 3 (standard library).

_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    CV_DUAL=$1
    # from LaGriT's run_all.sh: lagrit's path; cv_dual is beside it
    case $(basename "$1") in
        cv_dual*) ;;
        *) CV_DUAL=$(dirname "$1")/cv_dual
           # and that lagrit makes and times the meshes
           LAGRIT=${LAGRIT:-$1} ;;
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
BIG=${CV_DUAL_BIG:-$HOME/lagrit_work/del_vor_dev/big_inputs}
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

echo "cv_dual : $CV_DUAL"
echo "lagrit  : ${LAGRIT:-not given (cv_dual only)}"
echo "meshes  : $MESHES"
if [ -n "${LAGRIT:-}" ]; then
    case $LAGRIT in /*) ;; *) LAGRIT=$(pwd)/$LAGRIT ;; esac
    sh make_timing_meshes.sh "$LAGRIT" "$MESHES" || exit 1
fi
if ! ls "$MESHES"/*.inp > /dev/null 2>&1; then
    echo "no meshes in $MESHES: set LAGRIT=/path/to/lagrit to make them" >&2
    exit 1
fi
python3 -I timing.py "$CV_DUAL" "$MESHES" "$WORK" "$LOGDIR" "${LAGRIT:-}"
rc=$?
echo
exit $rc
