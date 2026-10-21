#!/bin/sh
#######################################################################
# run_error_tests.sh
#
# Suite A: Poisson disk error handling and reporting.
#
# Usage:   ./run_error_tests.sh [path-to-lagrit]
# Default: lagrit on PATH
#
#######################################################################
# WHERE THE MESSAGES GO
#
# LaGriT produces three separate streams, and writloga output does not
# necessarily appear on all of them:
#
#   stdout        whatever the shell captures by redirection
#   lagrit.log    the log of LaGriT commands executed
#   lagrit.out    a capture of screen output
#
# LaGriT OVERWRITES lagrit.log and lagrit.out on every run, so running
# tests in a loop destroys the evidence from all but the last one. An
# earlier version of this script captured stdout only and preserved
# nothing, so every test reported "no ERROR block" even where the
# driver had reported the error correctly.
#
# This version therefore:
#   - removes lagrit.log / lagrit.out before each run
#   - saves all three streams per test under logs/
#   - searches the UNION of the three, so it does not matter which
#     stream actually carries the banner
#######################################################################
# WHAT IS ASSERTED
#
#   err*   at least one ERROR block, no crash
#   warn*  a WARNING block, NO error block, run completes
#   all    every opened banner has a matching closing banner, AND at
#          least one block was seen somewhere in the suite
#
# That last clause matters. Zero opens and zero closes balance
# trivially, so without it a completely silent suite reports the
# balance check as a pass, which is exactly what happened twice while
# this harness was being debugged.
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
if [ "${CHECK_ONLY:-0}" -eq 0 ]; then
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
fi

# Every rm below is relative to this directory: go there, and stop if
# this is not it (the runner itself must be here; never $HOME or /).
cd "$(dirname "$0")" || exit 2
if [ ! -f "./$(basename "$0")" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
PASS=0
FAIL=0
LOGDIR=logs
TOTBLOCK=0

mkdir -p $LOGDIR
rm -f "${LOGDIR:?}"/*
rm -f out_*.inp

#----------------------------------------------------------------------
# Run one deck, preserving every output stream it produces.
# Leaves the combined text in $LOGDIR/<base>.all
#----------------------------------------------------------------------
run_deck () {
    base=$1
    rm -f lagrit.log lagrit.out
    $LAGRIT < $base.lgi > $LOGDIR/$base.stdout 2>&1
    RC=$?
    [ -f lagrit.log ] && cp lagrit.log $LOGDIR/$base.lagrit.log
    [ -f lagrit.out ] && cp lagrit.out $LOGDIR/$base.lagrit.out
    cat $LOGDIR/$base.stdout \
        $LOGDIR/$base.lagrit.log \
        $LOGDIR/$base.lagrit.out > $LOGDIR/$base.all 2>/dev/null
    return $RC
}

#----------------------------------------------------------------------
# Banner counting.
#
# poi_log_block writes, for every block:
#     ERROR POISSON DISK:     <- opener, trailing colon
#     ... message lines ...
#     ERROR POISSON DISK      <- closer, no colon
#
# NOTE: grep -c already prints 0 and exits 1 when there are no
# matches. An earlier version appended "|| echo 0", so both grep and
# echo emitted a zero and the variable held the two line string
# "0\n0". Every [ ] test then errored and fell through to the pass
# branch, reporting meaningless passes. Do not add it back.
#----------------------------------------------------------------------
count_open () {
    n=`grep -c "$1 POISSON DISK:" "$2" 2>/dev/null`
    [ -n "$n" ] || n=0
    echo "$n"
}
# Closers are counted by SUBTRACTION, not by anchoring to end of line.
#
# writlogb pads every message to an 80 column field, and the .all file
# concatenates three streams, so a line may carry trailing blanks or
# other content. An anchored pattern like "DISK *$" is brittle against
# that and reported zero closers for every test even though the
# closing banners were present. Counting every line that mentions the
# banner and removing the ones that are openers cannot be fooled that
# way, because the opener is exactly the banner plus a colon.
count_all () {
    n=`grep -c "$1 POISSON DISK" "$2" 2>/dev/null`
    [ -n "$n" ] || n=0
    echo "$n"
}
count_close () {
    a=`count_all  "$1" "$2"`
    o=`count_open "$1" "$2"`
    echo `expr $a - $o`
}

#----------------------------------------------------------------------
# Optional deck header lines, for the accumulator tests:
#
#     # blocks   : N              exactly N ERROR blocks
#     # contains : a | b | c      every text appears INSIDE the first
#                                 ERROR block (between its opener and
#                                 its closer)
#
# Both are judged on the deck's STDOUT ONLY. The .all file holds stdout
# AND lagrit.out, which repeat the same text, so every block there is
# seen twice (the "2 block/s" every deck reports).
#----------------------------------------------------------------------
header_value () {
    sed -n "s/^# *$1 *: *//p" "$2" | head -1
}
# Prints the lines of the first ERROR block in file $1.
first_error_block () {
    awk '/ERROR POISSON DISK:/ {inb=1; next}
         inb && /ERROR POISSON DISK/ {exit}
         inb {print}' "$1"
}

# Dump the banner context from a log, for diagnosing a failure without
# another round trip.
show_banners () {
    echo "        --- banner lines in $1 ---"
    grep -n "POISSON DISK" "$1" 2>/dev/null | head -12 | \
        sed 's/^/        /'
    echo "        --- end ---"
}

report () {
    if [ "$1" = "pass" ]; then
        PASS=`expr $PASS + 1`
        echo "  PASS  $2"
        echo "PASS  $2" >> $LOGDIR/table.txt
    else
        FAIL=`expr $FAIL + 1`
        echo "  FAIL  $2"
        echo "FAIL  $2" >> $LOGDIR/table.txt
        [ -n "$3" ] && echo "        $3"
    fi
}

echo "======================================================="
echo "Suite A - Poisson disk error handling"
echo "lagrit: $LAGRIT"
echo "======================================================="

#----------------------------------------------------------------------
echo
echo "--- error cases: expect an ERROR block, no crash ---"
for f in err*.lgi; do
    [ -e "$f" ] || continue
    base=`basename $f .lgi`
    run_deck $base
    rc=$?
    all=$LOGDIR/$base.all

    eopen=`count_open  ERROR $all`
    eclose=`count_close ERROR $all`
    TOTBLOCK=`expr $TOTBLOCK + $eopen`

    if [ "$rc" -gt 1 ]; then
        report fail "$base" "lagrit exit status $rc, possible crash"
    elif [ "$eopen" -lt 1 ]; then
        report fail "$base" "no ERROR block in $LOGDIR/$base.*"
        show_banners $all
    elif [ "$eopen" -ne "$eclose" ]; then
        report fail "$base" "$eopen openers but $eclose closers"
        show_banners $all
    else
        why=""
        want=`header_value blocks $f`
        if [ -n "$want" ]; then
            got=`count_open ERROR $LOGDIR/$base.stdout`
            [ "$got" -eq "$want" ] || \
                why="$got ERROR blocks on stdout, deck requires $want"
        fi
        texts=`header_value contains $f`
        if [ -z "$why" ] && [ -n "$texts" ]; then
            first_error_block $LOGDIR/$base.stdout > $LOGDIR/$base.block
            missing=`echo "$texts" | tr '|' '\n' | while read -r t; do
                [ -n "$t" ] && ! grep -qF -- "$t" $LOGDIR/$base.block \
                    && echo "$t"; done`
            [ -z "$missing" ] || \
                why="not inside the first ERROR block: $missing"
        fi
        if [ -n "$why" ]; then
            report fail "$base" "$why"
            show_banners $LOGDIR/$base.stdout
        elif [ -n "$want$texts" ]; then
            report pass "$base  (one block, all faults listed)"
        else
            report pass "$base  ($eopen block/s)"
        fi
    fi
done

#----------------------------------------------------------------------
echo
echo "--- warning cases: expect WARNING, no ERROR, run completes ---"
for f in warn*.lgi; do
    [ -e "$f" ] || continue
    base=`basename $f .lgi`
    run_deck $base
    rc=$?
    all=$LOGDIR/$base.all

    wopen=`count_open  WARNING $all`
    wclose=`count_close WARNING $all`
    eopen=`count_open  ERROR   $all`
    TOTBLOCK=`expr $TOTBLOCK + $wopen`

    if [ "$rc" -gt 1 ]; then
        report fail "$base" "lagrit exit status $rc"
    elif [ "$wopen" -lt 1 ]; then
        report fail "$base" "no WARNING block in $LOGDIR/$base.*"
        show_banners $all
    elif [ "$wopen" -ne "$wclose" ]; then
        report fail "$base" "$wopen openers but $wclose closers"
        show_banners $all
    elif [ "$eopen" -gt 0 ]; then
        report fail "$base" "warning case produced an ERROR block"
    else
        report pass "$base  ($wopen warning/s)"
    fi
done

#----------------------------------------------------------------------
echo
echo "--- all logs: every opened block is closed ---"
unbal=0
for all in $LOGDIR/*.all; do
    [ -e "$all" ] || continue
    for sev in ERROR WARNING; do
        o=`count_open  $sev $all`
        c=`count_close $sev $all`
        if [ "$o" -ne "$c" ]; then
            echo "  FAIL  `basename $all .all`: $sev $o open / $c closed"
            unbal=`expr $unbal + 1`
        fi
    done
done

if [ "$TOTBLOCK" -lt 1 ]; then
    report fail "block balance" \
      "NO blocks found anywhere - balance check is vacuous"
elif [ "$unbal" -eq 0 ]; then
    report pass "all blocks balanced ($TOTBLOCK blocks seen)"
else
    FAIL=`expr $FAIL + $unbal`
fi

#----------------------------------------------------------------------
echo
echo "======================================================="
echo "passed $PASS   failed $FAIL"
echo "all three output streams saved per test under $LOGDIR/"
echo "======================================================="
[ "$FAIL" -eq 0 ] || exit 1
exit 0
