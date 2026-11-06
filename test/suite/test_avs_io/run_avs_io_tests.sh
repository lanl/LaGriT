#!/bin/sh
#######################################################################
# run_avs_io_tests.sh [path-to-lagrit]
#
# read / avs and dump / avs: every cell type, hybrid meshes, files
# numbered from 0, comments, files with and without each attribute
# section, the dump options (iopt, avs1, att_node, att_elem, pt cells)
# and LaGriT's own files read back. Every mesh LaGriT reads is compared
# with the file it came from (check_avs.py reads AVS itself), and every
# file LaGriT writes is compared byte for byte with golden/ (written by
# LaGriT before any change to read / dump avs). The ak* decks document
# problems that were fixed (KNOWN for a problem not yet fixed). See
# 00README. About 2 seconds.
#
# make_inputs.py writes the input files into work/; each deck runs
# there. Exit status: nonzero on any FAIL or FIXED.
#
# GOLDEN=1 sh run_avs_io_tests.sh  rewrites golden/ from this run (only
# when the change to the output is intended; review the diff first).
# POSIX sh. Python 3, standard library only.
#######################################################################

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
if [ "${GOLDEN:-0}" = 1 ]; then
    mkdir -p golden
    rm -f golden/*
fi
NPASS=0; NFAIL=0; NKNOWN=0
printf "%-28s %-6s %s\n" deck result detail > $LOGDIR/table.txt
for deck in av*.lgi ak*.lgi; do
    name=${deck%.lgi}
    cp "$deck" $WORK/
    # a crash must not stop the suite (the ak* decks)
    ( cd $WORK && "$LAGRIT" < "$deck" > "$name.out" 2>&1 )
    cp $WORK/"$name.out" $LOGDIR/"$name.stdout"
    if [ "${GOLDEN:-0}" = 1 ]; then
        for f in $(sed -n 's/^# check: golden //p' "$deck"); do
            [ -f "$WORK/$f" ] && cp "$WORK/$f" golden/
        done
    fi
    python3 -I check_avs.py "$deck" $WORK > $LOGDIR/"$name.check" 2>&1
    res=$(sed -n 's/^result //p' $LOGDIR/"$name.check")
    [ -n "$res" ] || res=FAIL
    det=$(sed -n 's/^why //p' $LOGDIR/"$name.check")
    case $res in
        PASS)  NPASS=$((NPASS + 1)); det=$(grep -c '^check' $LOGDIR/"$name.check")" checks" ;;
        KNOWN) NKNOWN=$((NKNOWN + 1)); det=$(sed -n 's/^known //p' $LOGDIR/"$name.check") ;;
        *)     NFAIL=$((NFAIL + 1)) ;;
    esac
    printf "%-28s %-6s %s\n" "$name" "$res" "$det" | cut -c1-150 | tee -a $LOGDIR/table.txt
done
python3 -I make_page.py $LOGDIR
echo
echo "pass $NPASS   known $NKNOWN   fail $NFAIL   (of $((NPASS + NKNOWN + NFAIL)) decks)"
[ "$NFAIL" -eq 0 ]
