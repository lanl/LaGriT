#!/bin/sh
#######################################################################
# run_triangulate_tests.sh [path-to-lagrit]
#
# Runs every tri*.lgi deck and checks that triangulate made a valid
# triangulation of the deck's polygon. See 00README.
#
# Each deck header carries:
#   # polygon  : <file>.poly
#   # expect   : valid | error
# and optionally
#   # reason   : <text the ERROR must contain>      (expect error)
#   # warns    : <text a WARNING must contain>      (expect valid)
#   # check    : [refined] <mesh file> <poly>[,<poly>...]
#   # big      : yes         a slow deck: SKIPPED unless the runner is
#                            started with BIG=1 in the environment
#   # boundary_angle: <deg>  largest angle a cell of a REFINED mesh may
#                            have facing a boundary edge (90: the
#                            control volume / Delaunay condition) (HARD)
#   # picture  : small | large   size on the page: small = half width;
#                            large = full width, two pictures one over
#                            the other; default = as wide as fits
#   # min_angle: <degrees>   smallest cell angle every REFINED mesh
#                            must reach (HARD); without it the angle is
#                            only reported
# A deck with no "# check" line has one check: out_<deck>.inp against
# its "# polygon". With "# check" lines (one or more), each named mesh
# is checked against the named polygons; "refined" means the mesh has
# been refined / smoothed, so new vertices are allowed (see
# check_triangulate.py). The deck passes only if every check passes.
# A deck with no expect line FAILS, so a new test cannot go unjudged.
#
# Results:
#   PASS     expect valid: no ERROR, the mesh was written, and
#            check_triangulate.py's HARD checks pass (and the warns
#            text, if any, was printed). A deck with a refined check
#            shows "min angle A -> B deg": step 1 to the refined mesh.
#   REFUSED  expect error: an ERROR containing the reason text, and
#            no mesh written
#   SKIP     a "# big : yes" deck, not run (BIG=1 runs it)
#   FAIL     anything else
# Exit status: nonzero on any FAIL.
#
# LaGriT ECHOES every deck line into its output, comments included, so
# every search of the log below skips comment lines: a deck's own notes
# (which quote the messages it tests for) must never satisfy a check.
# LaGriT also keeps only the first 32 characters of a token, so a deck
# with a longer file name or token FAILS before it is run.
#
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
LOGDIR=logs
TABLE=$LOGDIR/table.txt
HTML=$LOGDIR/summary.html
mkdir -p $LOGDIR
rm -f "${LOGDIR:?}"/* out_tri*.inp
PASS=0; REFU=0; FAIL=0; SKIP=0
BIG=${BIG:-0}

hdr () {   # hdr <key> <deck>: value of "# key : value"
    sed -n "s/^# *$1 *: *//p" "$2" | head -1
}
# the log without echoed comment lines
nocomment () {
    grep -v '^[[:space:]]*#' "$1"
}
esc () {
    sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'
}

FMT="%-30s %-6s %8s %10s  %s\n"
printf "$FMT" test expect vertices seconds result | tee $TABLE
for f in tri*.lgi; do
    [ -e "$f" ] || continue
    base=`basename $f .lgi`
    poly=`hdr polygon $f`
    exp=`hdr expect $f`
    why=`hdr reason $f`
    warns=`hdr warns $f`
    minang=`hdr min_angle $f`
    bndang=`hdr boundary_angle $f`
    big=`hdr big $f`
    # the checks to run: "mode mesh polys" per line
    checks=$LOGDIR/$base.checks
    sed -n 's/^# *check *: *//p' $f | while read a b c; do
        if [ "$a" = refined ]; then echo "refined $b $c"; else echo "triangulated $a $b"; fi
    done > $checks
    [ -s $checks ] || echo "triangulated out_$base.inp $poly" > $checks
    polys=`awk '{print $3}' $checks | tr ',' '\n' | sort -u`
    nv=0
    for p in `awk 'NR==1{print $3}' $checks | tr ',' ' '`; do
        [ -f "$p" ] && nv=$((nv + `head -1 $p | awk '{print $1}'`))
    done
    res=""

    case "$exp" in valid|error) ;; *) res="FAIL  missing or unknown '# expect' line" ;; esac
    for p in $polys; do
        [ -z "$res" ] && [ ! -f "$p" ] && res="FAIL  polygon file '$p' not found"
    done
    if [ -z "$res" ]; then
        long=`nocomment $f | tr '/' '\n' | awk '{gsub(/^[ \t]+|[ \t]+$/,"")} length > 32' | head -1`
        [ -n "$long" ] && res="FAIL  token over 32 characters (LaGriT truncates it): $long"
    fi

    secs=-
    if [ -z "$res" ] && [ "$big" = yes ] && [ "$BIG" != 1 ]; then
        res="SKIP  slow deck, not run (BIG=1 run_triangulate_tests.sh runs it)"
    fi
    if [ -z "$res" ]; then
        rm -f lagrit.log lagrit.out
        t0=`date +%s`
        $LAGRIT < $f > $LOGDIR/$base.stdout 2>&1
        rc=$?
        secs=$(( `date +%s` - t0 ))
        [ -f lagrit.log ] && cp lagrit.log $LOGDIR/$base.lagrit.log
        [ -f lagrit.out ] && cp lagrit.out $LOGDIR/$base.lagrit.out
        cat $LOGDIR/$base.stdout $LOGDIR/$base.lagrit.log \
            $LOGDIR/$base.lagrit.out > $LOGDIR/$base.all 2>/dev/null
        nocomment $LOGDIR/$base.all > $LOGDIR/$base.msgs
        nerr=`grep -c -i "ERROR: triangulate\|Unable to triangulate\|ERROR: polygon has" $LOGDIR/$base.msgs`
        out1=`awk 'NR==1{print $2}' $checks`

        if [ "$rc" -gt 1 ]; then
            res="FAIL  lagrit exit status $rc, possible crash"
        elif [ "$exp" = error ]; then
            if [ "$nerr" -lt 1 ]; then
                res="FAIL  expected an ERROR, none reported"
            elif [ -n "$why" ] && ! grep -q -i -- "$why" $LOGDIR/$base.msgs; then
                res="FAIL  refused, but not for: $why"
            elif [ -f "$out1" ] && [ "`head -1 $out1 | awk '{print $2}'`" != 0 ]; then
                res="FAIL  ERROR reported but cells were still written"
            else
                res="REFUSED  ERROR reported, as expected"
            fi
        elif [ "$nerr" -gt 0 ]; then
            res="FAIL  `grep -i -m1 'Unable to triangulate\|ERROR: polygon has' $LOGDIR/$base.msgs | sed 's/^ *//' | cut -c1-50`"
        elif [ -n "$warns" ] && ! grep -q -- "$warns" $LOGDIR/$base.msgs; then
            res="FAIL  expected WARNING text not printed: $warns"
        else
            k=0
            while read mode mesh plist; do
                k=$((k + 1))
                tag=$base; [ $k -gt 1 ] && tag=${base}_$k
                [ -n "$res" ] && continue
                if [ ! -f "$mesh" ]; then
                    res="FAIL  no mesh written ($mesh)"
                    continue
                fi
                flag=""
                if [ "$mode" = refined ]; then
                    flag=--refined
                    [ -n "$minang" ] && flag="$flag --min-angle $minang"
                    [ -n "$bndang" ] && flag="$flag --boundary-angle $bndang"
                fi
                python3 check_triangulate.py $flag $plist $mesh \
                    $LOGDIR/$tag.svg > $LOGDIR/$tag.check 2>&1
                if [ $? -ne 0 ]; then
                    h=`grep -m1 -- '-> HARD' $LOGDIR/$tag.check | sed 's/.*HARD: //' | cut -c1-44`
                    [ -z "$h" ] && h="checker could not run, see $LOGDIR/$tag.check"
                    res="FAIL  $mesh: $h"
                    [ $k -eq 1 ] && res="FAIL  $h"
                fi
            done < $checks
            if [ -z "$res" ]; then
                res=PASS
                # a polygon with holes: say the holes are real holes
                nh=`awk '$1=="holes_in_polygons"{print $2}' $LOGDIR/$base.check`
                nl=`awk '$1=="boundary_loops"{print $2}' $LOGDIR/$base.check`
                hol=""
                [ "${nh:-0}" -gt 0 ] && hol="  $nh hole(s) real: $nl boundary loops"
                res="PASS$hol"
                # a refined mesh: show how the smallest angle changed
                if grep -q '^refined ' $checks; then
                    a1=`awk '$1=="min_angle_deg"{print $2}' $LOGDIR/$base.check`
                    a2=`awk '$1=="min_angle_deg"{print $2}' $LOGDIR/${base}_2.check`
                    p2=`awk '$1=="cells_below_20deg"{print $2}' $LOGDIR/${base}_2.check`
                    res="PASS  min angle $a1 -> $a2 deg, $p2 cells < 20 deg"
                    [ -n "$minang" ] && res="$res (need >= $minang)"
                    b2=`awk '$1=="boundary_angle_max_deg"{print $2}' $LOGDIR/${base}_2.check`
                    res="$res; boundary angle max $b2"
                    [ -n "$bndang" ] && res="$res (need <= $bndang)"
                    [ -n "$hol" ] && res="$res;$hol"
                fi
            fi
        fi
    fi
    case "$res" in
        PASS*)    PASS=$((PASS + 1)) ;;
        REFUSED*) REFU=$((REFU + 1)) ;;
        SKIP*)    SKIP=$((SKIP + 1)) ;;
        *)        FAIL=$((FAIL + 1)) ;;
    esac
    printf "$FMT" "$base" "$exp" "${nv:-?}" "$secs" "$res" | tee -a $TABLE
done

TALLY="pass $PASS   refused $REFU   skipped $SKIP   fail $FAIL"
{
    echo '<!DOCTYPE html><html><head><meta charset="utf-8">'
    echo '<title>triangulate tests</title><style>'
    echo 'body{font-family:sans-serif;margin:24px;max-width:1100px}'
    echo 'pre{font-size:12px;background:#f6f6f6;padding:8px;overflow-x:auto}'
    echo 'h2{font-size:14px;font-family:monospace;white-space:pre;margin:24px 0 4px}'
    echo 'section{border-top:2px solid #ddd}'
    echo '.pics{display:flex;flex-wrap:wrap;gap:16px;align-items:flex-start}'
    echo '.pic{flex:1 1 420px;min-width:0}.pic svg{width:100%;height:auto}'
    echo '.pics.small .pic{flex:0 1 50%;max-width:50%}'
    echo '.pics.large{flex-direction:column}.pics.large .pic{flex:1 1 auto;width:100%}'
    echo '</style></head><body>'
    echo '<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>'
    echo '<h1>triangulate tests</h1>'
    echo "<p>lagrit: `echo $LAGRIT | esc`<br>run: `date`</p>"
    echo "<p><b>$TALLY</b></p><pre>"; esc < $TABLE; echo '</pre>'
    echo '<p>Picture: polygon outline black, cells grey. A RED line is a'
    echo 'zero-area cell, a BLACK cell is inverted, an ORANGE edge is a'
    echo 'polygon edge not used by exactly one cell. An AMBER cell has an'
    echo 'angle under 20 degrees (a poor cell). A THICK MAGENTA edge (only'
    echo 'on meshes judged for it, after recon / 1) is a boundary edge whose'
    echo 'cell faces it with an angle over 90 degrees (circumcentre outside'
    echo 'the domain). A DASHED BLUE line is a'
    echo 'keyhole slit: listed twice in the polygon, it is an interior edge'
    echo 'of the mesh once triangulate merges its end vertices (the check'
    echo 'report counts the boundary loops: one per outline and per hole).'
    echo 'Decks with two meshes show them side by side: triangulated, then'
    echo 'refined and smoothed.</p>'
    for f in tri*.lgi; do
        b=`basename $f .lgi`
        echo '<section>'
        echo "<h2>`grep "^$b " $TABLE | head -1 | esc`</h2>"
        echo '<details><summary>deck notes</summary><pre>'
        grep '^#' $f | esc
        echo '</pre></details>'
        echo "<div class=\"pics `hdr picture $f`\">"
        k=0
        [ -f $LOGDIR/$b.checks ] && while read mode mesh plist; do
            k=$((k + 1))
            tag=$b; [ $k -gt 1 ] && tag=${b}_$k
            [ -f $LOGDIR/$tag.check ] || continue
            echo '<div class="pic">'
            [ $k -gt 1 ] || [ "`wc -l < $LOGDIR/$b.checks`" -gt 1 ] && \
                echo "<h3 style='font-size:13px'>$mesh ($mode)</h3>"
            echo '<details><summary>full check report</summary><pre>'
            esc < $LOGDIR/$tag.check
            echo '</pre></details>'
            [ -f $LOGDIR/$tag.svg ] && cat $LOGDIR/$tag.svg
            echo '</div>'
        done < $LOGDIR/$b.checks
        echo '</div>'
        echo '</section>'
    done
    echo '</body></html>'
} > $HTML

echo
echo "summary page: $HTML"
echo "$TALLY"
[ "$FAIL" -eq 0 ] || exit 1
exit 0
