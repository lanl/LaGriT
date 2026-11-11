#!/bin/sh
#
# run_all.sh [path-to-del_vor]
#
# Runs every del_vor test suite and prints one line per suite. Each
# suite's own runner, logs and summary page are unchanged; this only
# calls them. See README.md. Modeled on LaGriT's test/suite/run_all.sh.
#
# Exit status: 0 if every suite's runner exits 0, 1 otherwise.
# Full output of every runner: run_all.log (overwritten each time).
# One page for the whole run: summary.html (overwritten each time),
# linking to every suite's own summary.html.
#
# Settings (environment variables):
#   JOBS=N       run N suites at a time (default 1)
#   ONLY=a,b     only the suites with one of these tags (or names)
#
# Suites tagged dev (test_timing) run only when ONLY names them
# (ONLY=timing): they are for development, slow, and need LaGriT and
# big meshes.
#
# POSIX sh.

# Default: the del_vor built in this checkout (build/del_vor), else
# del_vor on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    DEL_VOR=$1
elif [ -n "$_root" ] && [ -x "$_root/build/del_vor" ]; then
    DEL_VOR=$_root/build/del_vor
else
    DEL_VOR=del_vor
fi
case $DEL_VOR in
    */*) [ -f "$DEL_VOR" ] &&
         DEL_VOR="$(cd "$(dirname "$DEL_VOR")" && pwd)/$(basename "$DEL_VOR")" ;;
    *)   _p=$(command -v "$DEL_VOR" 2>/dev/null) && DEL_VOR=$_p ;;
esac
if [ ! -f "$DEL_VOR" ] || [ ! -x "$DEL_VOR" ]; then
    echo "cannot run del_vor '$DEL_VOR': not an executable file," >&2
    echo "nor a command on PATH. Pass a path, e.g. ../../build/del_vor" >&2
    exit 2
fi

cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_all.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
TOP=$(pwd)
LOG=$TOP/run_all.log
PAGE=$TOP/summary.html
: > "$LOG"
rm -f "$PAGE" "$TOP"/.run_all_*.rc "$TOP"/.run_all_*.out

# Every suite: its directory, its runner, its usual run time in seconds
# and its tags.
SUITES="
test_kernels:run_kernel_tests.sh:1:kernels,2d,3d
test_voronoi_gold:run_voronoi_gold_tests.sh:1:gold,stor,uge,tough2,2d,3d
test_options:run_options_tests.sh:2:options,errors,2d,3d
test_poisson_lagrit:run_poisson_lagrit_tests.sh:2:lagrit,stor,3d
test_validity:run_validity_tests.sh:1:checks,errors,2d,3d
test_delaunay:run_delaunay_tests.sh:3:checks,delaunay,2d,3d
test_dfn:run_dfn_tests.sh:2:checks,dfn,stor,2d
test_parity:run_parity_tests.sh:10:parity,lagrit,stor,2d,3d,dfn
test_policy:run_policy_tests.sh:8:policy,hybrid,median,2d,3d,dfn
test_paraview:run_paraview_tests.sh:5:paraview,vtu,2d,3d,dfn
test_timing:run_timing_tests.sh:180:timing,dev
"

JOBS=${JOBS:-1}
has_tag () {          # has_tag TAGS WANTED: is a tag (or the name) in WANTED?
    for _w in $(echo "$2" | tr ',' ' '); do
        case ",$1," in *",$_w,"*) return 0 ;; esac
    done
    return 1
}

echo "del_vor : $DEL_VOR"
echo "JOBS=$JOBS"
RUN=""
for s in $SUITES; do
    dir=${s%%:*}; rest=${s#*:}; runner=${rest%%:*}; rest=${rest#*:}
    tags=${rest#*:}
    if [ -n "${ONLY:-}" ] && ! has_tag "$dir,$tags" "$ONLY"; then
        continue
    fi
    if [ -z "${ONLY:-}" ] && has_tag "$tags" dev; then
        continue
    fi
    RUN="$RUN $dir:$runner"
done

n=0
for s in $RUN; do
    dir=${s%%:*}; runner=${s#*:}
    (
        t0=$(date +%s)
        sh "$TOP/$dir/$runner" "$DEL_VOR" > "$TOP/.run_all_$dir.out" 2>&1
        rc=$?
        echo "$rc $(( $(date +%s) - t0 ))" > "$TOP/.run_all_$dir.rc"
    ) &
    n=$((n + 1))
    if [ "$n" -ge "$JOBS" ]; then
        wait
        n=0
    fi
done
wait

status=0
ROWS=""
for s in $RUN; do
    dir=${s%%:*}
    rc=1; secs=0
    [ -f "$TOP/.run_all_$dir.rc" ] && read rc secs < "$TOP/.run_all_$dir.rc"
    if [ "$rc" -eq 0 ]; then res=PASS; else res=FAIL; status=1; fi
    printf '%-24s %-5s %4s s\n' "$dir" "$res" "$secs"
    { echo "==== $dir ($res, $secs s)"; cat "$TOP/.run_all_$dir.out"; } >> "$LOG"
    ROWS="$ROWS<tr><td><a href=\"$dir/summary.html\">$dir</a></td><td class=\"$res\">$res</td><td>$secs s</td></tr>
"
    rm -f "$TOP/.run_all_$dir.rc" "$TOP/.run_all_$dir.out"
done

{
    echo '<!doctype html><html><head><meta charset="utf-8">'
    echo '<title>del_vor tests</title><style>'
    echo 'body{font-family:Helvetica,Arial,sans-serif;margin:24px}'
    echo 'td,th{padding:4px 10px;border-bottom:1px solid #ddd;text-align:left}'
    echo '.PASS{color:#1a7f37;font-weight:bold}.FAIL{color:#c00;font-weight:bold}'
    echo '</style></head><body>'
    if [ "$status" -eq 0 ]; then echo '<h1>del_vor tests: PASS</h1>'
    else echo '<h1>del_vor tests: FAIL</h1>'; fi
    echo "<p>del_vor: $DEL_VOR<br>$(date)</p>"
    echo '<table><tr><th>suite</th><th>result</th><th>time</th></tr>'
    printf '%s' "$ROWS"
    echo '</table><p>Full output: <a href="run_all.log">run_all.log</a></p>'
    echo '</body></html>'
} > "$PAGE"
echo "results: $PAGE"
exit $status
