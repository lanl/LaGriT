#!/bin/sh
#
# run_input_lines_tests.sh [path-to-lagrit]
#
# test_input_lines: how LaGriT reads its input lines: up to 80
# characters as always, 81 to 1024 used with a NOTE, longer lines and
# over-long joined commands refused with an ERROR (never cut or run in
# part), continuation lines of a refused command skipped, word tokens
# over 32 characters cut with a WARNING. See 00README. POSIX sh,
# Python 3 (standard library). WORK and LOGS may be set to other
# folders.

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
if [ ! -f "./run_input_lines_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*

echo "lagrit : $LAGRIT"
python3 -I make_input_lines.py "$WORK" || exit 1
for path in "$WORK"/il[0-9]*.lgi; do
    deck=$(basename "$path")
    name=${deck%.lgi}
    ( cd "$WORK" && "$LAGRIT" < "$deck" > "$name.out" 2>&1 )
    cp "$WORK/$name.out" "$LOGDIR/$name.stdout"
done
python3 -I check_input_lines.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
