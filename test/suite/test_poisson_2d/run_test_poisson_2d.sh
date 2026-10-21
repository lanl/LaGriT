#!/bin/sh
#######################################################################
# run_test_poisson_2d.sh
#
# Test harness around test_Poisson_2D.lgi.
#
# Usage:   ./run_test_poisson_2d.sh [path-to-lagrit]
# Default: lagrit on PATH
#
# Runs the deck ONCE, unchanged, then checks every mesh it writes.
# What each output is (its h and its field) is declared in
# test_Poisson_2D.cases, because the deck carries no machine readable
# annotations and is not edited by this harness.
#######################################################################
# RESULTS
#
#   run      the deck ran: no crash, no ERROR block, no WARNING block
#   <case>   one row per line of test_Poisson_2D.cases: the output was
#            written and check_test_poisson_2d.py's HARD checks pass
#   field    the checker's copy of the h field formula matches the
#            field user_h_field_att.mlgi actually dumped. If this
#            fails, the .mlgi has changed and the checker's
#            make_field() must be updated to match, otherwise every
#            spacing number is measured against the wrong field.
#
#   Each row is PASS or FAIL. EXIT STATUS is nonzero on any FAIL.
#
# The spacing, density, tracking and angle columns are REPORTED, NOT
# JUDGED, until calibrated. Spacing is measured on the final,
# SMOOTHED mesh, because the deck connects every point set.
#######################################################################
# OUTPUT
#
#   logs/summary.html                  EVERYTHING ON ONE PAGE
#   logs/<case>.svg                     one case's picture
#   logs/<case>.check                   the checker's full report
#   logs/run.*                          all LaGriT output streams
#
# Log handling follows run_error_tests.sh: LaGriT overwrites
# lagrit.log / lagrit.out on every run, and the banners may land on
# any of the three streams.
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
DECK=test_Poisson_2D
CASES=$DECK.cases
CHECK=check_test_poisson_2d.py
PASS=0
FAIL=0
LOGDIR=logs
TABLE=$LOGDIR/table.txt
HTML=$LOGDIR/summary.html

mkdir -p $LOGDIR
rm -f "${LOGDIR:?}"/*

count_open () {
    n=`grep -c "$1 POISSON DISK:" "$2" 2>/dev/null`
    [ -n "$n" ] || n=0
    echo "$n"
}

getval () {
    grep "^ *$1 " "$2" 2>/dev/null | head -1 | awk '{print $2}'
}

report () {
    if [ "$1" = "pass" ]; then
        PASS=`expr $PASS + 1`
    else
        FAIL=`expr $FAIL + 1`
    fi
}

# Case lines from the sidecar: no comments, no blanks.
cases () {
    grep -v '^ *#' $CASES | grep -v '^ *$'
}

echo "==========================================================="
echo "test_Poisson_2D harness"
echo "lagrit: $LAGRIT"
echo "==========================================================="

#----------------------------------------------------------------------
# Inputs present?
#----------------------------------------------------------------------
missing=""
for f in $DECK.lgi $CASES $CHECK unit_square.poly user_h_field_att.mlgi
do
    [ -f "$f" ] || missing="$missing $f"
done
if [ -n "$missing" ]; then
    echo "FAIL  missing input file(s):$missing"
    exit 1
fi

#----------------------------------------------------------------------
# Run the deck once. Remove every output first, so a file left over
# from an earlier run cannot be mistaken for this run's result.
#----------------------------------------------------------------------
cases | while read out poly h0 field width verify; do
    rm -f "$out"
    [ "$verify" != "-" ] && rm -f "$verify"
done

rm -f lagrit.log lagrit.out
$LAGRIT < $DECK.lgi > $LOGDIR/run.stdout 2>&1
RC=$?
[ -f lagrit.log ] && cp lagrit.log $LOGDIR/run.lagrit.log
[ -f lagrit.out ] && cp lagrit.out $LOGDIR/run.lagrit.out
cat $LOGDIR/run.stdout $LOGDIR/run.lagrit.log $LOGDIR/run.lagrit.out \
    > $LOGDIR/run.all 2>/dev/null

echo
FMT="%-30s %-8s %6s %7s %7s %5s %7s %7s %6s %5s  %s\n"
printf "$FMT" case field nodes density "int min" close "bnd med" \
       tracking "ang min" poor% result | tee $TABLE

nerr=`count_open ERROR $LOGDIR/run.all`
nwarn=`count_open WARNING $LOGDIR/run.all`
if [ "$RC" -gt 1 ]; then
    res="FAIL  lagrit exit status $RC, possible crash"
elif [ "$nerr" -gt 0 ]; then
    res="FAIL  ERROR block reported"
elif [ "$nwarn" -gt 0 ]; then
    res="FAIL  unexpected WARNING block"
else
    res="PASS"
fi
case "$res" in PASS) report pass ;; *) report fail ;; esac
printf "%-30s %s\n" "run $DECK.lgi" "$res" | tee -a $TABLE

#----------------------------------------------------------------------
# Check each case. A here document, not a pipe, so the PASS/FAIL
# counters are updated in THIS shell rather than a subshell.
#----------------------------------------------------------------------
cases > $LOGDIR/cases.list
while read out poly h0 field width verify; do
    base=`basename $out .inp`
    chk=$LOGDIR/$base.check
    [ "$width" = "-" ] && width=1
    fdesc=$field
    [ "$field" = "absx" ] && fdesc="absx $width"

    if [ ! -f "$out" ]; then
        report fail
        printf "%-30s %-8s  %s\n" $base "$fdesc" \
               "FAIL  output not written" | tee -a $TABLE
        continue
    fi

    python3 $CHECK --poly $poly --mesh $out --h0 $h0 --field $field \
            --width $width --svg $LOGDIR/$base.svg > $chk 2>&1
    if [ $? -eq 0 ]; then
        report pass
        res=PASS
    else
        report fail
        res="FAIL  `grep -- '-> HARD' $chk | head -1 | \
              sed 's/.*HARD: //' | cut -c1-40`"
        [ -n "`grep -- '-> HARD' $chk`" ] || res="FAIL  checker error"
    fi
    printf "$FMT" $base "$fdesc" "`getval nodes $chk`" \
           "`getval density $chk`" "`getval sep_min $chk`" \
           "`getval close_points $chk`" "`getval bnd_sep_median $chk`" \
           "`getval tracking_spread $chk`" "`getval angle_min $chk`" \
           "`getval poor_pct $chk`" "$res" | tee -a $TABLE

    if [ "$verify" != "-" ]; then
        vchk=$LOGDIR/field_$base.check
        if [ ! -f "$verify" ]; then
            report fail
            vres="FAIL  $verify not written"
        else
            python3 $CHECK --verify-field $verify --h0 $h0 \
                    --field $field --width $width > $vchk 2>&1
            if [ $? -eq 0 ]; then
                report pass
                vres="PASS  max rel error `getval field_max_relerr $vchk`"
            else
                report fail
                vres="FAIL  checker field does not match $verify"
            fi
        fi
        printf "%-30s %s\n" "field ($verify)" "$vres" | tee -a $TABLE
    fi
done < $LOGDIR/cases.list

#----------------------------------------------------------------------
# One page for everything. SVGs inlined, so the page stands alone.
#----------------------------------------------------------------------
esc () {
    sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'
}
TALLY="pass $PASS   fail $FAIL"
{
    echo '<!DOCTYPE html><html><head><meta charset="utf-8">'
    echo "<title>$DECK harness</title>"
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
    echo "<h1>$DECK harness</h1>"
    echo "<p>lagrit: `echo $LAGRIT | esc`<br>"
    echo "run: `date`</p>"
    echo "<p><b>$TALLY</b></p>"
    echo '<p>Spacing, density, tracking and angle columns are reported,'
    echo 'not judged, until calibrated. Spacing is measured on the'
    echo 'final smoothed mesh. "int" = interior nodes, "bnd" = nodes on'
    echo 'the polygon boundary.</p>'
    echo '<pre>'; esc < $TABLE; echo '</pre>'
    echo '<details><summary>the deck</summary><pre>'
    esc < $DECK.lgi
    echo '</pre></details>'
    echo '<details><summary>the cases file</summary><pre>'
    esc < $CASES
    echo '</pre></details>'
    while read out poly h0 field width verify; do
        b=`basename $out .inp`
        echo '<section>'
        echo "<h2>`grep "^$b " $TABLE | head -1 | esc`</h2>"
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
    done < $LOGDIR/cases.list
    echo '</body></html>'
} > $HTML

echo
echo "ALL RESULTS ON ONE PAGE:  $HTML   (open in a browser)"
echo "per case detail:          $LOGDIR/*.check"
echo "all output streams:       $LOGDIR/run.*"
echo
echo "==========================================================="
echo "$TALLY"
echo "==========================================================="
[ "$FAIL" -eq 0 ] || exit 1
exit 0
