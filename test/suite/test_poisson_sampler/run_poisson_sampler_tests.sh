#!/bin/sh
#
# run_poisson_sampler_tests.sh [--check-only] [path-to-lagrit]
#
# Runs the six sampler test decks (U, L, V in 2D and 3D, seeds 1-3 each)
# and checks every output. See 00README. These tests fail on the
# original Poisson disk sampler and pass with the fix applied Oct 11
# 2026.
# Exit status: 0 = every row PASS, 1 = any FAIL, 2 = cannot run.
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
CASES=poisson_sampler.cases
CHECK=check_poisson_sampler.py
SUITE=test_poisson_sampler
for f in $CASES $CHECK; do
    [ -f "$f" ] || { echo "missing $f" >&2; exit 2; }
done
DECKS=$($PY $CHECK decks $CASES)

# LaGriT keeps only the first 32 characters of a token: refuse longer
# ones (non-comment lines only) before running anything.
for d in $DECKS *.mlgi; do
    long=$(grep -v '^[[:space:]]*#' "$d" | tr '/' '\n' | sed 's/^ *//; s/ *&* *$//' \
           | awk 'length($0) > 32')
    if [ -n "$long" ]; then
        echo "$d: token longer than 32 characters: $long" >&2
        exit 2
    fi
done

if [ $CHECK_ONLY -eq 0 ]; then
    rm -f $($PY $CHECK outputs $CASES) lagrit.log lagrit.out
    rm -rf logs
    mkdir logs
    echo "lagrit: $LAGRIT"
    for d in $DECKS; do
        b=${d%.lgi}
        t0=$(date +%s)
        "$LAGRIT" < $d > logs/$b.stdout 2>&1
        st=$?
        t1=$(date +%s)
        [ -f lagrit.log ] && mv lagrit.log logs/$b.lagrit.log
        [ -f lagrit.out ] && mv lagrit.out logs/$b.lagrit.out
        echo "$st $((t1 - t0))" > logs/$b.status
        printf '  ran %-14s exit %s  %4s s\n' $d $st $((t1 - t0))
    done
else
    mkdir -p logs
fi

# ----------------------------------------------------------------------
# check every output
# ----------------------------------------------------------------------
NFAIL=0
TABLE=logs/table.txt
printf '%-7s %-5s %s\n' case result summary > $TABLE
printf '%-7s %-5s %s\n' ------- ----- ------- >> $TABLE
echo
cat $TABLE
for d in $DECKS; do
    b=${d%.lgi}
    # a POISSON DISK error block or a nonzero exit fails the deck's rows
    nerr=0
    [ -f logs/$b.stdout ] && nerr=$(grep -c 'ERROR POISSON DISK:' logs/$b.stdout)
    st=$(cut -d' ' -f1 logs/$b.status 2>/dev/null)
    echo "$nerr ${st:-?}" > logs/$b.deckcheck
done
for id in $($PY $CHECK ids $CASES); do
    keys=logs/$id.keys
    $PY $CHECK case $CASES $id logs > $keys 2>&1
    res=$(sed -n 's/^result //p' $keys)
    sum=$(sed -n 's/^summary //p' $keys)
    [ -z "$res" ] && { res=FAIL; sum="checker crashed, see $keys"; }
    deck=$(grep "^case $id " $CASES | sed 's/.* deck=\([^ ]*\).*/\1/')
    read nerr st < logs/${deck%.lgi}.deckcheck
    if [ "$nerr" != 0 ] || [ "$st" != 0 ]; then
        res=FAIL; sum="lagrit exit $st, $nerr POISSON DISK error block(s) | $sum"
        echo "result FAIL" >> $keys
    fi
    [ "$res" = FAIL ] && NFAIL=$((NFAIL + 1))
    printf '%-7s %-5s %s\n' "$id" "$res" "$sum" | tee -a $TABLE
done

# ----------------------------------------------------------------------
# one summary page
# ----------------------------------------------------------------------
{
    echo '<!doctype html><html><head><meta charset="utf-8">'
    echo "<title>$SUITE summary</title>"
    echo '<style>body{font-family:sans-serif;margin:16px;max-width:960px}'
    echo 'pre{background:#f4f4f4;padding:8px;font-size:12px;overflow-x:auto}'
    echo '.FAIL{color:#c00;font-weight:bold}.PASS{color:#070}td{padding:2px 10px 2px 0}</style></head><body>'
    echo '<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>'
    echo "<h1>$SUITE</h1><p>$(date)  -  lagrit $LAGRIT</p>"
    echo '<p>Expected: PASS (the fixed sampler). The original sampler FAILs 16 of 18 rows; see 00README.</p><table>'
    for id in $($PY $CHECK ids $CASES); do
        res=$(sed -n 's/^result //p' logs/$id.keys | tail -1)
        sum=$(sed -n 's/^summary //p' logs/$id.keys | sed 's/&/\&amp;/g; s/</\&lt;/g')
        echo "<tr><td><a href=\"#$id\">$id</a></td><td class=\"${res:-FAIL}\">${res:-FAIL}</td><td>$sum</td></tr>"
    done
    echo '</table>'
    for id in $($PY $CHECK ids $CASES); do
        echo "<h2 id=\"$id\">$id</h2><pre>"
        grep -v '^summary ' logs/$id.keys | sed 's/&/\&amp;/g; s/</\&lt;/g'
        echo '</pre>'
        [ -f logs/$id.svg ] && cat logs/$id.svg
    done
    echo '</body></html>'
} > logs/summary.html

echo
echo "summary page: logs/summary.html"
if [ $NFAIL -gt 0 ]; then
    echo "$NFAIL row(s) FAILED"
    exit 1
fi
echo "all rows PASS"
exit 0
