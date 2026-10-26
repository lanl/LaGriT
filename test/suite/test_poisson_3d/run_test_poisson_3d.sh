#!/bin/sh
#
# run_test_poisson_3d.sh [--check-only] [path-to-lagrit]
#
# Runs test_Poisson_3D.lgi once (unchanged) and checks every output.
# See 00README. Exit status: 0 = every row PASS or SKIP, 1 = any FAIL.
#
#   --check-only   do not run lagrit; check the outputs already here
#                  (and the logs/ copies of lagrit.log / lagrit.out)
#
# POSIX sh. Python 3 standard library only (set PYTHON to override).

CHECK_ONLY=0
if [ "$1" = "--check-only" ]; then
    CHECK_ONLY=1
    shift
fi
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
PY=${PYTHON:-python3}

# Every rm below is relative to this directory: go there, and stop if
# this is not it (the runner itself must be here; never $HOME or /).
cd "$(dirname "$0")" || exit 2
if [ ! -f "./$(basename "$0")" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi

DECK=test_Poisson_3D.lgi
CASES=test_Poisson_3D.cases
CHECK=check_test_poisson_3d.py
RENDER=render_test_poisson_3d.py
SUITE=test_poisson_3d
SUMMARY=logs/summary.html

for f in $DECK $CASES $CHECK; do
    if [ ! -f "$f" ]; then
        echo "missing $f" >&2
        exit 2
    fi
done

# ----------------------------------------------------------------------
# run the deck
# ----------------------------------------------------------------------
if [ $CHECK_ONLY -eq 0 ]; then
    # Delete every expected output first, so a stale file from an
    # earlier run can never pass as a new result.
    rm -f $($PY $CHECK outputs $CASES) lagrit.log lagrit.out
    rm -rf logs
    mkdir logs
    echo "running $LAGRIT < $DECK  (under a minute)"
    t0=$(date +%s)
    "$LAGRIT" < $DECK > logs/$SUITE.stdout 2>&1
    STATUS=$?
    t1=$(date +%s)
    echo "lagrit exit status $STATUS after $((t1 - t0)) s"
    echo "$STATUS" > logs/lagrit.status
    # LaGriT overwrites these on every run: keep copies.
    [ -f lagrit.log ] && cp lagrit.log logs/lagrit.log
    [ -f lagrit.out ] && cp lagrit.out logs/lagrit.out
else
    mkdir -p logs
    [ -f lagrit.log ] && cp lagrit.log logs/lagrit.log
    [ -f lagrit.out ] && cp lagrit.out logs/lagrit.out
    [ -f logs/$SUITE.stdout ] || : > logs/$SUITE.stdout
    if [ -f logs/lagrit.status ]; then
        STATUS=$(cat logs/lagrit.status)
    else
        STATUS=0
        echo "check only: no recorded exit status, assuming 0"
    fi
fi

# Union of everything LaGriT printed, for the run row.
: > logs/$SUITE.all
for f in logs/$SUITE.stdout logs/lagrit.log logs/lagrit.out; do
    [ -f "$f" ] && cat "$f" >> logs/$SUITE.all
done

# ----------------------------------------------------------------------
# check, one row at a time
# ----------------------------------------------------------------------
ROWS="run deck"
for id in $($PY $CHECK ids $CASES); do
    ROWS="$ROWS case_$id"
done
ROWS="$ROWS merge field"

NFAIL=0
TABLE=logs/table.txt
printf '%-9s %-5s %s\n' row result summary > $TABLE
printf '%-9s %-5s %s\n' --------- ----- ------- >> $TABLE
echo
cat $TABLE
for row in $ROWS; do
    keys=logs/$row.keys
    t0=$(date +%s)
    case $row in
        run)    $PY $CHECK run $CASES logs/$SUITE.stdout logs/lagrit.out \
                    "$STATUS" > $keys 2>&1 ;;
        deck)   $PY $CHECK deck $CASES > $keys 2>&1 ;;
        case_*) $PY $CHECK case $CASES ${row#case_} logs/lagrit.out logs \
                    > $keys 2>&1 ;;
        merge)  $PY $CHECK merge $CASES > $keys 2>&1 ;;
        field)  $PY $CHECK field $CASES > $keys 2>&1 ;;
    esac
    t1=$(date +%s)
    res=$(sed -n 's/^result //p' $keys)
    sum=$(sed -n 's/^summary //p' $keys)
    if [ -z "$res" ]; then
        res=FAIL
        sum="checker crashed, see $keys"
    fi
    [ "$res" = FAIL ] && NFAIL=$((NFAIL + 1))
    printf '%-9s %-5s %s  (%ss)\n' "$row" "$res" "$sum" $((t1 - t0)) \
        | tee -a $TABLE
done

# ----------------------------------------------------------------------
# picture of all cases cut open (like a ParaView crinkle clip). Never
# fails the run; NO_RENDER=1 skips it.
# ----------------------------------------------------------------------
rm -f logs/mesh_overview.svg logs/mesh_overview.png \
      logs/mesh_overview_quality.png
if [ "${NO_RENDER:-0}" != 1 ] && [ -f $RENDER ]; then
    t0=$(date +%s)
    if $PY $RENDER $CASES logs > logs/render.out 2>&1; then
        echo "picture   done  logs/mesh_overview.png  ($(( $(date +%s) - t0 ))s)"
    else
        echo "picture   not made, see logs/render.out (does not fail the run)"
    fi
fi

# ----------------------------------------------------------------------
# one summary page: results table, then each case's numbers and picture
# ----------------------------------------------------------------------
{
    echo '<!doctype html><html><head><meta charset="utf-8">'
    echo "<title>$SUITE summary</title>"
    echo '<style>body{font-family:sans-serif;margin:16px;max-width:960px}'
    echo 'pre{background:#f4f4f4;padding:8px;font-size:12px;overflow-x:auto}'
    echo '.FAIL{color:#c00;font-weight:bold}.PASS{color:#070}'
    echo '.SKIP{color:#777}td{padding:2px 10px 2px 0}</style></head><body>'
    echo '<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>'
    echo "<h1>$SUITE</h1><p>$(date)  -  deck $DECK</p><table>"
    for row in $ROWS; do
        res=$(sed -n 's/^result //p' logs/$row.keys)
        sum=$(sed -n 's/^summary //p' logs/$row.keys | sed 's/&/\&amp;/g; s/</\&lt;/g')
        echo "<tr><td>$row</td><td class=\"${res:-FAIL}\">${res:-FAIL}</td><td>$sum</td></tr>"
    done
    echo '</table>'
    [ -f logs/mesh_overview.svg ] && cat logs/mesh_overview.svg
    for row in $ROWS; do
        echo "<h2>$row</h2><pre>"
        grep -v '^summary ' logs/$row.keys | sed 's/&/\&amp;/g; s/</\&lt;/g'
        echo '</pre>'
        id=${row#case_}
        [ "$id" != "$row" ] && [ -f logs/case_$id.svg ] && cat logs/case_$id.svg
    done
    echo '</body></html>'
} > $SUMMARY

echo
echo "summary page: $SUMMARY"
if [ $NFAIL -gt 0 ]; then
    echo "$NFAIL row(s) FAILED"
    exit 1
fi
echo "all rows PASS or SKIP"
exit 0
