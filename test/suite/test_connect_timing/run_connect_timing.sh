#!/bin/sh
#######################################################################
# run_connect_timing.sh [path-to-lagrit]
#
# Times LaGriT's connect in 2D and 3D as the number of vertices grows,
# for four vertex orders (random, sorted by x y z, by binned x y z,
# Z-order), and
# checks every mesh. See 00README.
#
#   quick (default)   1000 to 16000 vertices, about 15 s
#   BIG=1             1000 to 256000 vertices, about 5 minutes
#
# time_connect.py runs and times LaGriT; check_connect.py judges the
# meshes, fits the growth exponents and writes logs/summary.html.
# Exit status: nonzero on any FAIL (a timing is never a failure).
#
# POSIX sh. Python 3, standard library only.
#######################################################################

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

# Fail early and clearly if lagrit cannot be run (otherwise every row
# fails with exit status 127). sh does not see bash aliases or shell
# functions: pass a path, or a name of a file on PATH.
case $LAGRIT in
    */*) [ -f "$LAGRIT" ] &&
         LAGRIT="$(cd "$(dirname "$LAGRIT")" && pwd)/$(basename "$LAGRIT")" ;;
    *)   _p=$(command -v "$LAGRIT" 2>/dev/null) && LAGRIT=$_p ;;
esac
if [ ! -f "$LAGRIT" ] || [ ! -x "$LAGRIT" ]; then
    echo "cannot run lagrit '$LAGRIT': not an executable file," >&2
    echo "nor a command on PATH (a bash alias or function is not" >&2
    echo "visible to sh). Pass a path, e.g. ../../build/lagrit" >&2
    exit 2
fi

# Every rm below is relative to this directory: go there, and stop if
# this is not it (the runner itself must be here; never $HOME or /).
cd "$(dirname "$0")" || exit 2
if [ ! -f "./$(basename "$0")" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
LOGDIR=logs
WORK=work
mkdir -p $LOGDIR $WORK
rm -f "${LOGDIR:?}"/*
rm -rf "${WORK:?}"/*

SIZE=quick
[ "${BIG:-0}" = 1 ] && SIZE=big
echo "lagrit : $LAGRIT"
echo "sizes  : $SIZE (BIG=1: 1000 to 256000 vertices, about 5 minutes)"
python3 -I time_connect.py "$LAGRIT" "$LOGDIR" "$WORK" $SIZE || exit 1
python3 -I check_connect.py "$LOGDIR" "$WORK"
