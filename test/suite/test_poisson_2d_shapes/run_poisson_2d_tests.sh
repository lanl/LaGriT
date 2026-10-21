#!/bin/sh
#######################################################################
# run_poisson_2d_tests.sh
#
# Suite B, 2D: Poisson disk point set invariants and mesh quality.
#
# Usage:   ./run_poisson_2d_tests.sh [path-to-lagrit]
# Default: lagrit on PATH
#
# Each deck runs the sampler twice on a convex polygon: once with
# no_connect (raw points) and once with connect (mesh). The token is
# REQUIRED; leaving it off is an input error. check_poisson_2d.py
# measures the raw points for spacing, containment and count, and the
# mesh for element quality.
#######################################################################
# DECK CONVENTION
#
# Every p2d*.lgi carries three header lines that this script reads:
#
#   # polygon : <file>.poly
#   # h       : <value>
#   # expect  : valid | error
#
# and dumps raw_<deck basename>.inp and out_<deck basename>.inp.
# A deck expecting an error may also carry
#
#   # reason  : <text>
#
# (LaGriT echoes every deck line, comments included, into its output,
# so the reason is searched for in NON-comment lines only; otherwise
# the deck's own "# reason" line would always match.)
#
# and the refusal then counts only if <text> appears in the log.
# A deck with no expect line FAILS, so it cannot go unjudged.
#######################################################################
# RESULTS
#
#   PASS      expect valid: no crash, no ERROR, no WARNING, both
#             output files written, HARD checks pass
#   REFUSED   expect error: ERROR reported (for the stated reason),
#             no mesh written
#   FAIL      anything else
#
# The spacing, density and angle columns are REPORTED, NOT JUDGED,
# until they have been calibrated against real output. In particular
# it is not yet established whether h is a strict minimum distance
# between sampled points or a target the sampler approximates; the
# "close" column will answer that.
#
# EXIT STATUS is nonzero on any FAIL.
#######################################################################
# OUTPUT
#
#   logs/summary.html             EVERYTHING ON ONE PAGE: the table,
#                                 then each deck's notes and picture
#   logs/<deck>.svg               the picture for one deck
#   logs/<deck>.check             the checker's full report
#   logs/<deck>.all               all LaGriT output streams together
#
# Log handling is copied from run_error_tests.sh, which documents why:
# LaGriT overwrites lagrit.log / lagrit.out on every run, and the
# banners may land on any of the three streams.
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
REFU=0
LOGDIR=logs
TABLE=$LOGDIR/table.txt
HTML=$LOGDIR/summary.html

mkdir -p $LOGDIR
rm -f "${LOGDIR:?}"/*
rm -f raw_p2d*.inp out_p2d*.inp

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

# See run_error_tests.sh: grep -c already prints 0, do not add
# "|| echo 0".
count_open () {
    n=`grep -c "$1 POISSON DISK:" "$2" 2>/dev/null`
    [ -n "$n" ] || n=0
    echo "$n"
}

# Value of one "key value" line from the checker output.
getval () {
    grep "^ *$1 " "$2" 2>/dev/null | head -1 | awk '{print $2}'
}

report () {
    case "$1" in
        pass)    PASS=`expr $PASS + 1` ;;
        refused) REFU=`expr $REFU + 1` ;;
        *)       FAIL=`expr $FAIL + 1` ;;
    esac
}

echo "==========================================================="
echo "Suite B 2D - point set invariants and mesh quality"
echo "lagrit: $LAGRIT"
echo "==========================================================="
echo
FMT="%-22s %5s %7s %7s %7s %6s %7s %6s %7s %6s  %s\n"
printf "$FMT" test h points density "sep min" close "bnd cl" verts \
       "ang min" "poor%" result | tee $TABLE

for f in p2d*.lgi; do
    [ -e "$f" ] || continue
    base=`basename $f .lgi`
    poly=`grep '^# polygon' $f | awk '{print $4}'`
    h=`grep '^# h ' $f | awk '{print $4}'`
    exp=`grep '^# expect' $f | awk '{print $4}'`
    why_exp=`grep '^# reason' $f | sed 's/^# reason *: *//'`
    chk=$LOGDIR/$base.check

    short () {
        printf "%-22s %5s  %s\n" $base "$h" "$1" | tee -a $TABLE
    }

    case "$exp" in
        valid|error) ;;
        *) report fail
           short "FAIL  missing or unknown '# expect' line"
           continue ;;
    esac

    if [ -z "$poly" ] || [ ! -f "$poly" ]; then
        report fail
        short "FAIL  polygon file '$poly' not found"
        continue
    fi

    run_deck $base
    rc=$?
    all=$LOGDIR/$base.all
    nerr=`count_open ERROR $all`
    nwarn=`count_open WARNING $all`

    if [ "$rc" -gt 1 ]; then
        report fail
        short "FAIL  lagrit exit status $rc, possible crash"
        continue
    fi

    if [ "$exp" = "error" ]; then
        if [ "$nerr" -lt 1 ]; then
            report fail
            short "FAIL  expected an ERROR block, none reported"
        elif [ -n "$why_exp" ] && ! grep -v '^[[:space:]]*#' $all | grep -q -- "$why_exp"; then
            report fail
            short "FAIL  refused, but not for: $why_exp"
        elif [ -f out_$base.inp ]; then
            report fail
            short "FAIL  ERROR reported but a mesh was still written"
        else
            report refused
            short "REFUSED  ERROR reported, no mesh, as expected"
        fi
        continue
    fi

    why=""
    if [ "$nerr" -gt 0 ]; then
        why="ERROR block reported"
    elif [ "$nwarn" -gt 0 ]; then
        why="unexpected WARNING block"
    elif [ ! -f raw_$base.inp ]; then
        why="no raw points written (raw_$base.inp)"
    elif [ ! -f out_$base.inp ]; then
        why="no mesh written (out_$base.inp)"
    fi
    if [ -n "$why" ]; then
        report fail
        short "FAIL  $why"
        continue
    fi

    python3 check_poisson_2d.py $poly raw_$base.inp out_$base.inp $h \
            $LOGDIR/$base.svg > $chk 2>&1
    crc=$?
    if [ "$crc" -eq 0 ]; then
        report pass
        res=PASS
    else
        report fail
        res="FAIL  `grep -- '-> HARD' $chk | head -1 | \
              sed 's/.*HARD: //' | cut -c1-40`"
        [ -n "`grep -- '-> HARD' $chk`" ] || res="FAIL  checker error"
    fi
    vf=`grep '^ *vertices_found' $chk | awk '{print $2 "/" $4}'`
    printf "$FMT" $base "$h" "`getval raw_points $chk`" \
           "`getval density $chk`" "`getval sep_min $chk`" \
           "`getval close_points $chk`" \
           "`getval bnd_close_points $chk`" "$vf" \
           "`getval angle_min $chk`" "`getval poor_pct $chk`" \
           "$res" | tee -a $TABLE
done

#----------------------------------------------------------------------
# One page for the whole suite. The SVGs are inlined so the page is a
# single self contained file. See run_nonconvex_tests.sh.
#----------------------------------------------------------------------
esc () {
    sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'
}
TALLY="pass $PASS   refused $REFU   fail $FAIL"
{
    echo '<!DOCTYPE html><html><head><meta charset="utf-8">'
    echo '<title>Suite B 2D - point set invariants and mesh quality</title>'
    echo '<style>'
    echo 'body{font-family:sans-serif;margin:24px;max-width:1450px}'
    echo 'pre{font-size:12px;background:#f6f6f6;padding:8px;'
    echo '    overflow-x:auto}'
    echo 'svg{width:100%;height:auto;border:1px solid #ccc}'
    echo 'h2{font-size:14px;margin:28px 0 4px 0;font-family:monospace;'
    echo '   white-space:pre;overflow-x:auto}'
    echo 'section{border-top:2px solid #ddd}'
    echo '</style></head><body>'
    echo '<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>'
    echo '<h1>Suite B 2D - point set invariants and mesh quality</h1>'
    echo "<p>lagrit: `echo $LAGRIT | esc`<br>"
    echo "run: `date`</p>"
    echo "<p><b>$TALLY</b></p>"
    echo '<p>Spacing, density and angle columns are reported, not'
    echo 'judged, until calibrated. "sep min" and "close" are for'
    echo 'interior points; "bnd cl" counts close pairs on the polygon'
    echo 'boundary, where short spacing is by design.</p>'
    echo '<pre>'; esc < $TABLE; echo '</pre>'
    for f in p2d*.lgi; do
        [ -e "$f" ] || continue
        b=`basename $f .lgi`
        echo '<section>'
        echo "<h2>`grep "^$b " $TABLE | head -1 | esc`</h2>"
        echo '<details><summary>deck notes</summary><pre>'
        grep '^#' $f | esc
        echo '</pre></details>'
        if [ -f $LOGDIR/$b.check ]; then
            echo '<details><summary>full check report</summary><pre>'
            esc < $LOGDIR/$b.check
            echo '</pre></details>'
        fi
        if [ -f $LOGDIR/$b.svg ]; then
            cat $LOGDIR/$b.svg
        else
            echo '<p><i>no picture produced</i></p>'
        fi
        echo '</section>'
    done
    echo '</body></html>'
} > $HTML

echo
echo "ALL RESULTS ON ONE PAGE:  $HTML   (open in a browser)"
echo "per test detail:          $LOGDIR/*.check"
echo "all output streams:       $LOGDIR/"
echo
echo "==========================================================="
echo "$TALLY"
echo "==========================================================="
[ "$FAIL" -eq 0 ] || exit 1
exit 0
