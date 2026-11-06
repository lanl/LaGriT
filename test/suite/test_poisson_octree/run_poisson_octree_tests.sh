#!/bin/sh
#
# run_poisson_octree_tests.sh [path-to-lagrit]
#
# test_poisson_octree: createpts / poisson_disk / 3d_box with h_lookup
# octree: spacing, maximality, batching, seeds, K, the octree file. See 00README.
# About 15 s. POSIX sh, Python 3 (standard library). WORK and LOGS may be
# set to other folders.

# On exit, put the summary page in this folder (../lift_summary.py).
_SUITE=$(cd "$(dirname "$0")" && pwd)
trap 'python3 -I "$_SUITE/../lift_summary.py" "$_SUITE"' EXIT

# Default: the lagrit built in this LaGriT checkout (build/lagrit), else
# lagrit on PATH; or pass a path as the first argument.
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
if [ ! -f "./run_poisson_octree_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*

echo "lagrit : $LAGRIT"
cp h_point.mlgi "$WORK"/
for deck in po[0-9]*.lgi; do
    name=${deck%.lgi}
    cp "$deck" "$WORK"/
    # runs the deck in WORK, records its time and peak memory
    python3 -I check_poisson_octree.py run "$LAGRIT" "$WORK" "$deck"
    cp "$WORK/$name.out" "$LOGDIR/$name.stdout"
done
python3 -I check_poisson_octree.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
