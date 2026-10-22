#!/bin/sh
#
# run_all.sh [path-to-lagrit]
#
# Runs every test suite (seven for createpts/poisson_disk, one for the
# other createpts options, one for triangulate, one for interpolate and
# compute, LaGriT's level01 tests from ../level01, and, with BIG=1
# only, the connect timing suite), one after another, and prints one
# line per suite. Each suite's own runner, logs and summary page are
# unchanged; this only calls them. See README.md.
#
# Exit status: 0 if every suite's runner exits 0, 1 otherwise.
# Full output of every runner: run_all.log (overwritten each time).
# One page for the whole run: summary.html (overwritten each time),
# linking to every suite's own logs/summary.html.
#
# POSIX sh.

# Default: the lagrit built in this LaGriT checkout (build/lagrit), else
# lagrit on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd)
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
TOP=$(pwd)

# Resolved to a full path here, so it still works after cd into a
# suite.

LOG=$TOP/run_all.log
PAGE=$TOP/summary.html
: > "$LOG"
rm -f "$PAGE"
ROWS=""            # one "dir|result|seconds|start" line per suite

# suite directory, its runner, and its usual run time in seconds on
# a Mac, Release build (Oct 11 2026 run, 218 s;
# shown while it runs, so a stuck suite is easy to spot). Order: every
# Poisson suite, then triangulate.
SUITES="
test_poisson_command_errors:run_error_tests.sh:4
test_poisson_2d:run_test_poisson_2d.sh:6
test_poisson_2d_shapes:run_poisson_2d_tests.sh:4
test_poisson_nonconvex:run_nonconvex_tests.sh:6
test_poisson_3d:run_test_poisson_3d.sh:62
test_poisson_3d_refine_letters:run_poisson_3d_letters.sh:41
test_poisson_sampler:run_poisson_sampler_tests.sh:41
test_createpts:run_createpts_tests.sh:26
test_triangulate_2d_polygon:run_triangulate_tests.sh:22
test_lagrit_upstream:run_upstream_tests.sh:30
test_interpolate_compute:run_interpolate_tests.sh:10
test_crush_thin_tets:run_crush_tests.sh:15
test_element_angles:run_angle_tests.sh:2
test_connect_timing:run_connect_timing.sh:290:big
"

# The expected time of each suite: its time in the last run on this
# machine that PASSED it without BIG (run_all_usual.txt, kept by this
# script), else the number in SUITES above. BIG=1 adds the slow decks.
USUAL=$TOP/run_all_usual.txt
BIG_EXTRA=34          # tri21 (40 004 vertices), skipped unless BIG=1
usual_of () {         # usual_of DIR DEFAULT
    _u=""
    # the last stored time for DIR; a missing, zero or odd entry falls
    # back to the built-in number
    [ -f "$USUAL" ] && _u=$(awk -v d="$1" '$1 == d && $2+0 > 0 {t = $2} END {print t}' "$USUAL")
    echo "${_u:-$2}"
}
fmt_time () {         # 75 -> "1 min 15 s", 40 -> "40 s"
    if [ "$1" -ge 90 ]; then echo "$(( $1 / 60 )) min $(( $1 % 60 )) s"
    else echo "$1 s"; fi
}
clock_at () {         # clock_at EPOCH -> HH:MM (BSD date, then GNU date)
    date -r "$1" +%H:%M 2>/dev/null || date -d "@$1" +%H:%M 2>/dev/null
}
# an entry ending in :big (test_connect_timing) runs only with BIG=1
is_big () { case $1 in *:big) return 0 ;; esac; return 1; }
EST=0
for entry in $SUITES; do
    if is_big "$entry"; then
        [ "${BIG:-0}" = 1 ] || continue
        entry=${entry%:big}
    fi
    _d=${entry%%:*}
    EST=$(( EST + $(usual_of "$_d" "${entry##*:}") ))
done
[ "${BIG:-0}" = 1 ] && EST=$(( EST + BIG_EXTRA ))

NFAIL=0
TSTART=$(date +%s)
NEWUSUAL=""
echo "lagrit: $LAGRIT"
echo "the whole run is expected to take about $(fmt_time $EST)" \
     "(finishing about $(clock_at $(( TSTART + EST ))))"
[ "${BIG:-0}" = 1 ] && echo "    (BIG=1: includes about $BIG_EXTRA s for the slow decks)"
echo "while a suite runs, its own progress is in run_all.log:"
echo "    tail -f $LOG"
echo
printf '%-30s %-6s %6s  %s\n' suite result time "summary page"
printf '%-30s %-6s %6s  %s\n' ----- ------ ---- ------------
for entry in $SUITES; do
    if is_big "$entry"; then
        entry=${entry%:big}
        if [ "${BIG:-0}" != 1 ]; then
            printf '%-30s %-6s %6s  %s\n' "${entry%%:*}" SKIP - "only with BIG=1"
            ROWS="$ROWS${entry%%:*}|SKIPPED|-|-
"
            continue
        fi
    fi
    dir=${entry%%:*}
    rest=${entry#*:}
    runner=${rest%%:*}
    usual=$(usual_of "$dir" "${rest#*:}")
    if [ ! -f "$TOP/$dir/$runner" ]; then
        printf '%-30s %-6s %6s  %s\n' "$dir" MISSING - "no $dir/$runner"
        NFAIL=$((NFAIL + 1))
        ROWS="$ROWS$dir|MISSING|-|-
"
        continue
    fi
    echo "=================== $dir ===================" >> "$LOG"
    tstart=$(date +%H:%M:%S)
    printf '%-30s running  (started %s, usually about %s s)\n' \
        "$dir" "$tstart" "$usual"
    t0=$(date +%s)
    (cd "$TOP/$dir" && sh "./$runner" "$LAGRIT") >> "$LOG" 2>&1
    rc=$?
    t1=$(date +%s)
    if [ $rc -eq 0 ]; then res=PASS; else res=FAIL; NFAIL=$((NFAIL + 1)); fi
    page="$dir/logs/summary.html"
    [ -f "$TOP/$page" ] || page="(no page; see $dir/logs/)"
    printf '%-30s %-6s %5ss  %s\n' "$dir" "$res" $((t1 - t0)) "$page"
    ROWS="$ROWS$dir|$res|$((t1 - t0))|$tstart
"
    # a passing, non-BIG run of this suite becomes its usual time
    if [ "$res" = PASS ] && [ "${BIG:-0}" != 1 ] && [ $((t1 - t0)) -gt 0 ]; then
        NEWUSUAL="$NEWUSUAL$dir $((t1 - t0))
"
    fi
done
# keep the usual times: new ones from this run, old ones for the rest
{
    echo "# suite usual-seconds: last passing non-BIG run on this machine (run_all.sh)"
    printf '%s' "$NEWUSUAL"
    [ -f "$USUAL" ] && grep -v '^#' "$USUAL" | while read -r d t; do
        printf '%s' "$NEWUSUAL" | grep -q "^$d " || echo "$d $t"
    done
    true
} > "$USUAL.new" && mv "$USUAL.new" "$USUAL"
TTOTAL=$(( $(date +%s) - TSTART ))

# The per-suite times, one line each, for snapshot_results.sh.
{
    echo "# suite result seconds started  ($(date '+%Y-%m-%d %H:%M'), total $TTOTAL s)"
    printf '%s' "$ROWS" | tr '|' ' '
} > "$TOP/run_all_times.txt"

# ----------------------------------------------------------------------
# summary.html: one row per suite, linking to its own summary page
# (relative links, so the page works wherever the tree is), and each
# runner's console output, folded, from run_all.log.
# ----------------------------------------------------------------------
html_escape () { sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g'; }
{
    if [ $NFAIL -eq 0 ]; then overall="ALL PASS"; else overall="$NFAIL FAILED"; fi
    echo '<!doctype html><html><head><meta charset="utf-8">'
    echo '<title>LaGriT test suites</title>'
    echo '<style>body{font-family:sans-serif;margin:16px;max-width:1000px}'
    echo 'td,th{padding:3px 12px 3px 0;text-align:left}'
    echo '.PASS{color:#070;font-weight:bold}.FAIL,.MISSING{color:#c00;font-weight:bold}.SKIPPED{color:#777}'
    echo 'pre{background:#f4f4f4;padding:8px;font-size:12px;overflow-x:auto}'
    echo 'summary{cursor:pointer;margin:6px 0}'
    echo 'details:target summary,details.hit summary{background:#ffe9a8}</style></head><body>'
    echo "<h1>LaGriT test suites: <span class=\"$( [ $NFAIL -eq 0 ] && echo PASS || echo FAIL )\">$overall</span></h1>"
    echo "<p>$(date '+%Y-%m-%d %H:%M')  -  total $TTOTAL s<br>"
    echo "lagrit: $(echo "$LAGRIT" | html_escape)</p>"
    echo '<p>Click a suite name to open its runner output (console text) further down this page; click a results page to open it in a new tab.</p>'
    echo '<table><tr><th>suite</th><th>result</th><th>time</th><th>started</th><th>results page (new tab)</th></tr>'
    printf '%s' "$ROWS" | while IFS='|' read -r d r t st; do
        [ -n "$d" ] || continue
        # the suite's own page opens in a new tab; the suite name jumps
        # to (and opens) its runner output further down this page
        if [ -f "$TOP/$d/logs/summary.html" ]; then
            link="<a href=\"$d/logs/summary.html\" target=\"_blank\" rel=\"noopener\">$d/logs/summary.html</a>"
        else
            link="(no page: see the runner output below, or <a href=\"$d/logs/\" target=\"_blank\" rel=\"noopener\">$d/logs/</a>)"
        fi
        echo "<tr><td><a href=\"#$d\" title=\"show this suite's runner output, below\">$d</a></td><td class=\"$r\">$r</td><td>$t s</td><td>$st</td><td>$link</td></tr>"
    done
    echo '</table>'
    echo '<h2>Runner output</h2>'
    printf '%s' "$ROWS" | while IFS='|' read -r d r t st; do
        [ -n "$d" ] || continue
        echo "<details id=\"$d\"><summary><b>$d</b> <span class=\"$r\">$r</span></summary><pre>"
        awk -v want="=================== $d ===================" '
            $0 == want {on = 1; next}
            /^=================== .* ===================$/ {on = 0}
            on' "$LOG" | html_escape
        echo '</pre></details>'
    done
    # a click on a suite name opens its fold-out (a link alone would
    # only scroll to it, closed)
    cat <<'JS'
<script>
function openTarget() {
  var id = decodeURIComponent(location.hash.slice(1));
  var d = id && document.getElementById(id);
  if (d && d.tagName === 'DETAILS') {
    d.open = true;
    document.querySelectorAll('details.hit').forEach(function (x) { x.classList.remove('hit'); });
    d.classList.add('hit');
    d.scrollIntoView();
  }
}
window.addEventListener('hashchange', openTarget);
openTarget();
</script>
JS
    echo '</body></html>'
} > "$PAGE"

echo
echo "total time $(fmt_time $TTOTAL) (expected about $(fmt_time $EST))"
echo "all results on one page:     summary.html  (open in a browser)"
echo "full output of every runner: run_all.log"
if [ $NFAIL -gt 0 ]; then
    [ "${BIG:-0}" != 1 ] && echo "(slow decks were skipped; BIG=1 run_all.sh runs them)"
    echo "$NFAIL suite(s) did not exit cleanly"
    exit 1
fi
echo "all suites exited cleanly"
if [ "${BIG:-0}" != 1 ]; then
    echo "slow parts were SKIPPED (tri21, 40 004 vertices; test_connect_timing,"
    echo "about 5 minutes): BIG=1 run_all.sh runs them; do so after any change"
    echo "to triangulate_lg.f"
fi
exit 0
