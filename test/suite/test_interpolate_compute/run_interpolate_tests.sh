#!/bin/sh
#######################################################################
# run_interpolate_tests.sh [path-to-lagrit]
#
# interpolate (voronoi, continuous, map) and compute (distance_field,
# signed_distance_field), each judged against values computed
# independently by check_ic.py; the stored lookups (keepatt: pt_gtg,
# el_gtg) with and without a dudded vertex, and after rmpoint /
# compress. See 00README. About 5 seconds.
#
# make_inputs.py writes the input meshes into work/; each ic*.lgi runs
# there. Exit status: nonzero on any FAIL.
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

echo "lagrit : $LAGRIT"
python3 -I make_inputs.py $WORK || exit 1
NPASS=0; NFAIL=0; NKNOWN=0
printf "%-36s %-6s %s\n" deck result detail > $LOGDIR/table.txt
for deck in ic*.lgi; do
    name=${deck%.lgi}
    cp "$deck" $WORK/
    ( cd $WORK && "$LAGRIT" < "$deck" > "$name.out" 2>&1 )
    cp $WORK/"$name.out" $LOGDIR/"$name.stdout"
    python3 -I check_ic.py "$deck" $WORK > $LOGDIR/"$name.check" 2>&1
    res=$(sed -n 's/^result //p' $LOGDIR/"$name.check")
    [ -n "$res" ] || res=FAIL
    det=$(sed -n 's/^why //p' $LOGDIR/"$name.check")
    [ -n "$det" ] || det="judged $(sed -n 's/^judged //p' $LOGDIR/"$name.check"), worst error $(sed -n 's/^worst_rel_error //p' $LOGDIR/"$name.check")"
    case $res in
        PASS)  NPASS=$((NPASS + 1)) ;;
        KNOWN) NKNOWN=$((NKNOWN + 1)); det=$(sed -n 's/^known //p' $LOGDIR/"$name.check") ;;
        *)     NFAIL=$((NFAIL + 1)) ;;
    esac
    printf "%-36s %-6s %s\n" "$name" "$res" "$det" | tee -a $LOGDIR/table.txt
done
python3 -I make_page.py $LOGDIR $WORK
# the demonstrations (not judged): demo/demo.lgi -> logs/demo.html
python3 -I demo/make_demo_inputs.py $WORK
cp demo/demo.lgi $WORK/
( cd $WORK && "$LAGRIT" < demo.lgi > demo.out 2>&1 )
cp $WORK/demo.out $LOGDIR/demo.stdout
python3 -I demo/make_demo_page.py $WORK $LOGDIR || echo "demo page not drawn (see logs/demo.stdout)"
echo
echo "summary page: logs/summary.html   demonstrations: logs/demo.html"
echo "pass $NPASS   known $NKNOWN   fail $NFAIL   (of $((NPASS + NKNOWN + NFAIL)) decks)"
[ "$NFAIL" -eq 0 ]
