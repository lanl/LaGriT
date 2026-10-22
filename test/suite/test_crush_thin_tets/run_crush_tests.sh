#!/bin/sh
#
# run_crush_tests.sh [path-to-lagrit]
#
# test_crush_thin_tets: planted NEEDLE, WEDGE, CAP and SLIVER tets in a
# regular lattice mesh (inside and on the boundary), a randomly jittered
# lattice, and 3D Poisson disk meshes, each run through crush_thin_tets;
# quality measured before, trip by trip and after; every tet left below
# TOLCRUSH explained. See 00README. POSIX sh, Python 3 (standard library).
#
# WORK and LOGS may be set to other folders (default work, logs).

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
if [ ! -f "./run_crush_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*

echo "lagrit : $LAGRIT"
python3 -I make_inputs.py "$WORK" || exit 1
cp ct_measure.mlgi ct_h_field.mlgi "$WORK"/
NFAIL=0
: > "$LOGDIR/table.txt"
for deck in ct[0-9]*.lgi; do
    name=${deck%.lgi}
    cp "$deck" "$WORK"/
    ( cd "$WORK" && "$LAGRIT" < "$deck" > "$name.out" 2>&1 )
    cp "$WORK/$name.out" "$LOGDIR/$name.stdout"
    python3 -I check_crush.py "$deck" "$WORK" "$LOGDIR" > "$LOGDIR/$name.check" 2>&1
    res=$(sed -n 's/^result //p' "$LOGDIR/$name.check")
    [ "$res" = PASS ] || NFAIL=$((NFAIL + 1))
    surv=$(grep "_survivors " "$LOGDIR/$name.check" | awk '{printf "%s%s", s, $2; s=", "}')
    printf '%-28s %-5s  left below TOLCRUSH: %s\n' "$name" "${res:-FAIL}" "$surv" | tee -a "$LOGDIR/table.txt"
done
python3 -I make_page.py "$WORK" "$LOGDIR" || echo "page not drawn"
echo
echo "summary page: $LOGDIR/summary.html"
N=$(ls ct[0-9]*.lgi | wc -l | tr -d ' ')
echo "pass $((N - NFAIL))   fail $NFAIL   (of $N decks)"
[ $NFAIL -eq 0 ]
