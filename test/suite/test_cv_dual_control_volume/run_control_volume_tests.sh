#!/bin/sh
#
# run_control_volume_tests.sh [path-to-lagrit]
#
# test_cv_dual_control_volume: LaGriT's command dump / control_volume
# (cv_dual through its entry point) on meshes LaGriT makes or reads,
# each case's deck NAME.lgi run in work/NAME, then the cv_dual program
# (built beside lagrit) on the mesh the deck wrote; check_control_volume.py
# compares them. See 00README. POSIX sh, Python 3 (standard library).
# WORK and LOGS may be set to other folders.

# Default: the lagrit built in this LaGriT checkout (build/lagrit), else
# lagrit on PATH; or pass a path as the first argument. cv_dual is the
# one beside lagrit.
_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    LAGRIT=$1
elif [ -n "$_root" ] && [ -x "$_root/build/lagrit" ]; then
    LAGRIT=$_root/build/lagrit
else
    LAGRIT=lagrit
fi
case $LAGRIT in
    */*) [ -f "$LAGRIT" ] &&
         LAGRIT="$(cd "$(dirname "$LAGRIT")" && pwd)/$(basename "$LAGRIT")" ;;
    *)   _p=$(command -v "$LAGRIT" 2>/dev/null) && LAGRIT=$_p ;;
esac
if [ ! -f "$LAGRIT" ] || [ ! -x "$LAGRIT" ]; then
    echo "cannot run lagrit '$LAGRIT': not an executable file," >&2
    echo "nor a command on PATH. Pass a path, e.g. ../../../build/lagrit" >&2
    exit 2
fi
CV_DUAL=$(dirname "$LAGRIT")/cv_dual
if [ ! -x "$CV_DUAL" ]; then
    echo "no cv_dual beside lagrit ($CV_DUAL)" >&2
    exit 2
fi

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_control_volume_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "lagrit  : $LAGRIT"
echo "cv_dual : $CV_DUAL"
# the meshes the decks read
python3 -I ../test_cv_dual_delaunay/make_delaunay_inputs.py "$WORK/gen" \
    > "$LOGDIR/make_inputs.out" || exit 1
INPUTS="../test_cv_dual_dfn/inputs/full_mesh.inp
../test_cv_dual_parity/inputs/two_mat.inp
../test_cv_dual_poisson_lagrit/inputs/test_3d_poi_01.inp
$WORK/gen/flip3d.inp"

grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name args rest; do
    name=$(echo "$name" | tr -d ' ')
    args=$(echo "$args" | sed 's/^ *//;s/ *$//')
    d=$WORK/$name
    mkdir -p "$d"
    cp "$name.lgi" "$d"/
    for f in $INPUTS; do cp "$f" "$d"/; done
    ( cd "$d" && "$LAGRIT" < "$name.lgi" > run.out 2>&1
      echo $? > run.rc )
    if [ "$args" != "-" ] && [ -f "$d/mesh.inp" ]; then
        mkdir -p "$d/prog"
        cp "$d/mesh.inp" "$d/prog/"
        # shellcheck disable=SC2086
        ( cd "$d/prog" && "$CV_DUAL" -avs mesh.inp $args \
              > run.out 2>&1 < /dev/null
          echo $? > run.rc )
    fi
    cp "$d/run.out" "$LOGDIR/$name.out"
    echo "ran $name"
done
python3 -I check_control_volume.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
