#!/bin/sh
#
# run_vs_matbld_tests.sh [path-to-lagrit]
#
# test_cv_dual_vs_matbld: LaGriT's own decks that write .stor, .uge
# and FEHM files, each run twice: as written (matbld) and with
# / cv_dual on every dump / stor, fehm, pflotran and zone_outside;
# check_vs_matbld.py compares the two runs' files and must explain
# every difference. See 00README. POSIX sh, Python 3 (standard library).
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

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_vs_matbld_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "lagrit  : $LAGRIT"
TEST=../..
# test_stor_utm's decks read meshes its generator writes
python3 -I ../test_stor_utm/make_stor_inputs.py "$WORK/gen" \
    > "$LOGDIR/make_inputs.out" || exit 1

grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name deck rest; do
    name=$(echo "$name" | tr -d ' ')
    deck=$(echo "$deck" | tr -d ' ')
    src=$TEST/$deck
    for v in matbld cv_dual; do
        d=$WORK/$name/$v
        mkdir -p "$d"
        # the deck's folder: its inputs (files only, not reference/)
        for f in "$(dirname "$src")"/*; do
            [ -f "$f" ] && cp "$f" "$d"/
        done
        case $deck in
            suite/test_stor_utm/*) cp "$WORK"/gen/* "$d"/ ;;
            suite/test_cv_dual_vs_matbld/*)
                cp ../test_cv_dual_poisson_lagrit/inputs/test_3d_poi_01.inp "$d"/ ;;
        esac
        ls "$d" > "$d/.inputs"
    done
    python3 -I prepare_decks.py "$src" "$WORK/$name/matbld" \
        "$WORK/$name/cv_dual" > "$WORK/$name/dumps.txt" || exit 1
    for v in matbld cv_dual; do
        ( cd "$WORK/$name/$v" && "$LAGRIT" < input.lgi > run.out 2>&1
          echo $? > run.rc )
        cp "$WORK/$name/$v/run.out" "$LOGDIR/${name}_$v.out"
    done
    echo "ran $name"
done
python3 -I check_vs_matbld.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
