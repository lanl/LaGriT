#!/bin/sh
#######################################################################
# run_nonconvex_tests.sh
#
# Suite B, part 1: nonconvex polygon outline recovery.
#
# Usage:   ./run_nonconvex_tests.sh [path-to-lagrit]
# Default: lagrit on PATH
#
# Each deck meshes a sawtooth polygon at a different sample spacing h.
# check_recovery.py then measures how faithfully the clip recovered the
# outline.
#
# There is NO reference output. The correct answer is a function of the
# tooth width to h ratio, so a stored reference would not say whether
# the result is right.
#######################################################################
# DECK CONVENTION
#
# Every nc*.lgi carries three header lines that this script reads:
#
#   # polygon : <file>.poly
#   # h       : <value>
#   # expect  : recovered | degraded | error
#
# A deck expecting an error may also carry
#
#   # reason  : <text>
#
# (LaGriT echoes every deck line, comments included, into its output,
# so the reason is searched for in NON-comment lines only; otherwise
# the deck's own "# reason" line would always match.)
#
# and the refusal then counts only if <text> appears in the log, so a
# deck refused for the WRONG reason is a FAIL, not a REFUSED.
#
# A deck with a DOCUMENTED failure carries
#
#   # known    : <text found in check_recovery.py's HARD line>
#   # expected : <one line: what happens and where it is explained>
#
# and then reports KNOWN instead of FAIL, but only if EVERY hard check
# that failed contains <text>, so a new, different fault still FAILs.
#
# and dumps its mesh to out_<deck basename>.inp. A deck with no
# expect line FAILS, so a new deck cannot silently go unjudged.
#######################################################################
# THIS IS A STRESS TEST, NOT A PASS/FAIL GATE
#
# The decks push the algorithm past the point where it works, on
# purpose, to locate that point and show what failure looks like. A
# degraded mesh is therefore NOT reported as a pass. Every deck gets
# one of five results:
#
#   PASS        expected to recover, and did
#   DEGRADED    expected to degrade, and did. The algorithm FAILED
#               here; that failure is known and documented in the
#               deck header. See logs/<deck>.svg for what it looks
#               like.
#   REFUSED     expected to be refused by input validation, and was
#   UNEXPECTED  expected to degrade, but recovered. The failure
#               boundary has MOVED. Investigate, then update the
#               deck's expect line.
#   KNOWN       a documented failure: the deck's "# known :" text
#               matches EVERY hard check that failed. Not a fault in
#               the test. Its "# expected :" line says what to expect
#               and where it is explained; it does NOT need fixing to
#               keep the suite green.
#   FIXED       the deck lists a known failure but it did not happen.
#               Good news, but the deck now misdescribes the code:
#               remove the "# known" / "# expected" lines.
#   FAIL        anything else: a crash, a missing warning, a broken
#               hard check, a recovered deck over tolerance, a refused
#               deck that was not refused.
#
# EXIT STATUS is nonzero on any FAIL, UNEXPECTED or FIXED. It answers "does
# the suite still describe the algorithm correctly", not "did every
# mesh come out right". DEGRADED, REFUSED and KNOWN do not affect it,
# because they are the documented behaviour.
#######################################################################
# WHAT EACH RESULT REQUIRES
#
# Every deck:
#   - the polygon file exists before LaGriT is started
#   - no crash (lagrit exit status <= 1)
#
# expect = error
#   - an ERROR block, and NO mesh written. The driver must refuse the
#     polygon before the sampler runs.
#
# expect = recovered | degraded
#   - no ERROR block
#   - a WARNING block reporting the polygon as nonconvex. Every deck
#     here is nonconvex by design, so no warning means test_convex
#     has regressed and the clip was skipped.
#   - the mesh file was written
#   - the HARD checks in check_recovery.py: no element centroid
#     outside the polygon, no inverted elements
#
# expect = recovered, additionally
#   - mismatch within the tolerance in check_recovery.py
#
# expect = degraded
#   - mismatch OVER the tolerance. If it comes in under, the result
#     is UNEXPECTED, not a pass.
#
# Every deck that produces a mesh also gets logs/<deck>.svg, a
# picture of the outline, the mesh, and the exact uncovered and
# overhanging regions.
#
# At the end, ALL of it is collected into ONE page:
#
#     logs/summary.html
#
# the tally, the results table, then for every deck its result, its
# header notes and its picture. Open that one file in a browser.
#######################################################################
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
DEGR=0
REFU=0
UNEX=0
KNOW=0
FIXD=0
LOGDIR=logs
TABLE=$LOGDIR/table.txt
HTML=$LOGDIR/summary.html

mkdir -p $LOGDIR
rm -f "${LOGDIR:?}"/*
rm -f out_nc*.inp

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

# Value of one "key value" line from check_recovery.py output.
getval () {
    grep "^ *$1 " "$2" 2>/dev/null | head -1 | awk '{print $2}'
}

report () {
    case "$1" in
        pass)       PASS=`expr $PASS + 1` ;;
        degraded)   DEGR=`expr $DEGR + 1` ;;
        refused)    REFU=`expr $REFU + 1` ;;
        unexpected) UNEX=`expr $UNEX + 1` ;;
        known)      KNOW=`expr $KNOW + 1` ;;
        fixed)      FIXD=`expr $FIXD + 1` ;;
        *)          FAIL=`expr $FAIL + 1` ;;
    esac
}

echo "==========================================================="
echo "Suite B part 1 - nonconvex outline recovery"
echo "lagrit: $LAGRIT"
echo "==========================================================="
echo
FMT="%-24s %5s %-9s %8s %9s %9s %7s  %-9s %s\n"
printf "$FMT" test h expect "net %" "mismatch%" "overhang%" \
       verts recovery result | tee $TABLE

for f in nc*.lgi; do
    [ -e "$f" ] || continue
    base=`basename $f .lgi`
    poly=`grep '^# polygon' $f | awk '{print $4}'`
    h=`grep '^# h ' $f | awk '{print $4}'`
    exp=`grep '^# expect *:' $f | awk '{print $4}'`
    why_exp=`grep '^# reason *:' $f | sed 's/^# reason *: *//'`
    known=`grep '^# known *:' $f | sed 's/^# known *: *//'`
    expected=`grep '^# expected *:' $f | sed 's/^# expected *: *//'`
    rec=$LOGDIR/$base.recovery

    short () {
        printf "%-24s %5s %-9s  %s\n" $base "$h" "$exp" "$1" | \
            tee -a $TABLE
    }

    case "$exp" in
        recovered|degraded|error) ;;
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
    elif [ `count_open WARNING $all` -lt 1 ] || \
         ! grep -v '^[[:space:]]*#' $all | grep -q "nonconvex"; then
        why="no nonconvex WARNING: clip was skipped?"
    elif [ ! -f out_$base.inp ]; then
        why="no mesh written (out_$base.inp)"
    fi
    if [ -n "$why" ]; then
        report fail
        short "FAIL  $why"
        continue
    fi

    python3 check_recovery.py $poly out_$base.inp $LOGDIR/$base.svg \
            > $rec 2>&1
    crc=$?
    net=`getval net_area_pct $rec`
    mis=`getval mismatch_pct $rec`
    ovr=`getval overhang_pct $rec`
    vf=`grep '^ *vertices_found' $rec | awk '{print $2 "/" $4}'`
    qual=`grep 'RECOVERY:' $rec | awk '{print $2}'`

    nhard=`grep -c -- '-> HARD' $rec`
    nknown=0
    [ -n "$known" ] && nknown=`grep -- '-> HARD' $rec | grep -cF -- "$known"`
    if [ "$crc" -ne 0 ] && [ -n "$known" ] && [ "$nhard" -gt 0 ] && \
       [ "$nknown" -eq "$nhard" ]; then
        report known
        res="KNOWN  $expected"
    elif [ "$crc" -ne 0 ]; then
        report fail
        res="FAIL  `grep -- '-> HARD' $rec | head -1 | \
              sed 's/.*HARD: //' | cut -c1-40`"
    elif [ -n "$known" ]; then
        report fixed
        res="FIXED  known failure did not occur: update the deck"
    elif [ "$exp" = "recovered" ] && [ "$qual" != "recovered" ]; then
        report fail
        res="FAIL  mismatch over tolerance"
    elif [ "$exp" = "degraded" ] && [ "$qual" = "recovered" ]; then
        report unexpected
        res="UNEXPECTED  recovered: failure boundary moved"
    elif [ "$exp" = "degraded" ]; then
        report degraded
        res="DEGRADED"
    else
        report pass
        res=PASS
    fi
    printf "$FMT" $base "$h" "$exp" "$net" "$mis" "$ovr" "$vf" \
           "$qual" "$res" | tee -a $TABLE
done

echo
#----------------------------------------------------------------------
# One page for the whole suite.
#
# The SVGs are inlined rather than linked, so the page is a single
# self contained file that can be moved or mailed on its own.
# check_recovery.py gives each picture its own clip and mask IDs,
# which is what makes inlining several on one page safe.
#----------------------------------------------------------------------
esc () {
    sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'
}
TALLY="pass $PASS   degraded $DEGR   refused $REFU   known $KNOW   unexpected $UNEX   fixed $FIXD   fail $FAIL"
{
    echo '<!DOCTYPE html><html><head><meta charset="utf-8">'
    echo '<title>Suite B part 1 - nonconvex outline recovery</title>'
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
    echo '<h1>Suite B part 1 - nonconvex outline recovery</h1>'
    echo "<p>lagrit: `echo $LAGRIT | esc`<br>"
    echo "run: `date`</p>"
    echo "<p><b>$TALLY</b></p>"
    echo '<pre>'; esc < $TABLE; echo '</pre>'
    for f in nc*.lgi; do
        [ -e "$f" ] || continue
        b=`basename $f .lgi`
        echo '<section>'
        echo "<h2>`grep "^$b " $TABLE | head -1 | esc`</h2>"
        echo '<details><summary>deck notes</summary><pre>'
        grep '^#' $f | esc
        echo '</pre></details>'
        if [ -f $LOGDIR/$b.svg ]; then
            cat $LOGDIR/$b.svg
        else
            echo '<p><i>no mesh produced</i></p>'
        fi
        echo '</section>'
    done
    echo '</body></html>'
} > $HTML

echo "ALL RESULTS ON ONE PAGE:  $HTML   (open in a browser)"
echo "pictures of every mesh:   $LOGDIR/*.svg"
echo "missing polygon vertices: $LOGDIR/*.recovery"
echo "all output streams:       $LOGDIR/"
echo
echo "==========================================================="
echo "$TALLY"
echo "==========================================================="
[ "$FAIL" -eq 0 ] && [ "$UNEX" -eq 0 ] && [ "$FIXD" -eq 0 ] || exit 1
exit 0
