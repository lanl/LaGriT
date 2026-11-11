#!/bin/sh
#
# run_stor_utm_tests.sh [path-to-lagrit]
#
# test_stor_utm: dump / stor (every writer: the 3D default
# anothermatbld3d with all, graph and hybrid; matbld3d_stor with none
# and coefs; 2D matbld2d_stor) and dump / pflotran on the same mesh at
# the origin and at UTM coordinates (7e5, 3.2e6, 1.5e3): the files must
# agree, the volumes add up, and match an independent computation. See
# 00README. POSIX sh, Python 3 (standard library). WORK and LOGS may be
# set to other folders; REF_STOR=DIR adds another run's volumes (its
# WORK folder) to the pictures.

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
if [ ! -f "./run_stor_utm_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*

echo "lagrit : $LAGRIT"
python3 -I make_stor_inputs.py "$WORK" || exit 1
for deck in st[0-9]*.lgi; do
    name=${deck%.lgi}
    cp "$deck" "$WORK"/
    ( cd "$WORK" && "$LAGRIT" < "$deck" > "$name.out" 2>&1 )
    cp "$WORK/$name.out" "$LOGDIR/$name.stdout"
done
python3 -I check_stor_utm.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
