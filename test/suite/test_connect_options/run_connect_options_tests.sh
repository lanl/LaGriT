#!/bin/sh
#
# run_connect_options_tests.sh [path-to-lagrit]
#
# test_connect_options: the options of connect in 2D and 3D: noadd,
# check_interface and old_order in any position, the point range with
# or without delaunay, the deprecated enclosing tet / triangle
# coordinates. See 00README. POSIX sh, Python 3 (standard library).
# WORK and LOGS may be set to other folders.

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
if [ ! -f "./run_connect_options_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*

echo "lagrit : $LAGRIT"
python3 -I make_connect_options.py "$WORK" || exit 1
for path in "$WORK"/o[abc][0-9]*.lgi; do
    deck=$(basename "$path")
    name=${deck%.lgi}
    ( cd "$WORK" && "$LAGRIT" < "$deck" > "$name.out" 2>&1 )
    cp "$WORK/$name.out" "$LOGDIR/$name.stdout"
done
python3 -I check_connect_options.py "$WORK" "$LOGDIR"
rc=$?
echo
echo "summary page: $LOGDIR/summary.html"
exit $rc
