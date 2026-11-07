#!/bin/sh
#
# run_all.sh [path-to-lagrit]
#
# Runs every test suite (seven for createpts/poisson_disk, one for the
# other createpts options, one for triangulate, one for interpolate and
# compute, one for read / dump avs, LaGriT's level01 tests from
# ../level01, and, with BIG=1 only, the connect timing suite), one
# after another, and prints one line per suite. Each suite's own
# runner, logs and summary page are unchanged; this only calls them.
# See README.md.
#
# Exit status: 0 if every suite's runner exits 0, 1 otherwise.
# Full output of every runner: run_all.log (overwritten each time).
# One page for the whole run: summary.html (overwritten each time),
# linking to every suite's own summary.html (in the suite's folder).
#
# Settings (environment variables; they combine; see README.md):
#   JOBS=N       run N suites at a time (default 1), the longest first
#   ONLY=a,b     only the suites with one of these tags (or names)
#   QUICK=1      leave out the suites tagged slow
#   CHANGED=1    only the suites the changes since master can affect
#   CHANGED=ref  ... since that branch or commit (select_changed.py)
#   BIG=1        also the slow decks and test_connect_timing
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

# Every suite: its directory, its runner, its usual run time in seconds
# (a Mac, Release build; shown while it runs, so a stuck suite is easy
# to spot) and its tags:
#   what it tests   poisson createpts triangulate connect recon
#                   interpolate quality io sets parser upstream errors,
#                   and 2d / 3d
#   slow            usually 20 s or more (QUICK=1 leaves these out)
#   big             runs only with BIG=1
# ONLY=tag,tag,... runs the suites with any of these tags (or names).
SUITES="
test_poisson_command_errors:run_error_tests.sh:4:poisson,errors,2d,3d
test_poisson_2d:run_test_poisson_2d.sh:6:poisson,2d
test_poisson_2d_shapes:run_poisson_2d_tests.sh:4:poisson,2d
test_poisson_nonconvex:run_nonconvex_tests.sh:6:poisson,2d
test_poisson_3d:run_test_poisson_3d.sh:62:poisson,3d,slow
test_poisson_3d_refine_letters:run_poisson_3d_letters.sh:41:poisson,3d,slow
test_poisson_sampler:run_poisson_sampler_tests.sh:41:poisson,2d,3d,slow
test_poisson_octree:run_poisson_octree_tests.sh:15:poisson,3d
test_createpts:run_createpts_tests.sh:26:createpts,2d,3d
test_triangulate_2d_polygon:run_triangulate_tests.sh:22:triangulate,2d
test_connect_2d:run_connect_2d_tests.sh:5:connect,2d
test_lagrit_upstream:run_upstream_tests.sh:30:upstream,2d,3d,slow
test_interpolate_compute:run_interpolate_tests.sh:10:interpolate,2d,3d
test_distance_field:run_distance_field_tests.sh:30:interpolate,3d
test_crush_thin_tets:run_crush_tests.sh:15:quality,3d
test_element_angles:run_angle_tests.sh:2:quality,2d,3d
test_improve_tets:run_improve_tests.sh:15:quality,3d
test_avs_io:run_avs_io_tests.sh:5:io
test_hextotet:run_hextotet_tests.sh:1:3d
test_set_logic:run_set_logic_tests.sh:1:sets
test_edge_lengths:run_edge_lengths_tests.sh:1:quality,2d,3d
test_input_lines:run_input_lines_tests.sh:2:parser
test_connect_options:run_connect_options_tests.sh:2:connect,2d,3d
test_connect_exact:run_connect_exact_tests.sh:40:connect,3d,slow
test_predicates:run_predicates_tests.sh:10:connect,2d,3d
test_connect_hull:run_connect_hull_tests.sh:20:connect,3d,slow
test_recon:run_recon_tests.sh:10:recon,2d,3d
test_connect_timing:run_connect_timing.sh:290:connect,2d,3d,big
"

# The expected time of each suite: its time in the last run on this
# machine that PASSED it without BIG, one suite at a time
# (run_all_usual.txt, kept by this script), else the number in SUITES
# above. BIG=1 adds the slow decks.
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
has_tag () {          # has_tag TAGS WANTED: is a tag (or the name) in WANTED?
    for _w in $(echo "$2" | tr ',' ' '); do
        case ",$1," in *",$_w,"*) return 0 ;; esac
    done
    return 1
}

# ---- which suites run -------------------------------------------------
# CHANGED=1 (or CHANGED=<git ref>): only the suites that what has changed
# since the base can affect; see select_changed.py and suite_sources.txt.
CHANGED_SEL=""
if [ -n "${CHANGED:-}" ]; then
    CHANGED_SEL=$(python3 -I "$TOP/select_changed.py" "$TOP" "$CHANGED") || {
        echo "CHANGED: could not work out what changed (see above); running everything" >&2
        CHANGED_SEL=ALL
    }
fi
WHY_FILE=$TOP/.run_all_why.txt     # select_changed.py's reasons, one line per suite
JOBS=${JOBS:-1}
case $JOBS in ''|*[!0-9]*|0) echo "JOBS must be a number, 1 or more" >&2; exit 2 ;; esac

SELECTED=""        # entries that run: dir:runner:usual
SKIPPED_ROWS=""    # dir|why
for entry in $SUITES; do
    dir=${entry%%:*}
    rest=${entry#*:}
    runner=${rest%%:*}
    rest=${rest#*:}
    def=${rest%%:*}
    tags=$dir,${rest#*:}
    why=""
    if [ ! -f "$TOP/$dir/$runner" ]; then
        why="MISSING: no $dir/$runner"
    elif has_tag "$tags" big && [ "${BIG:-0}" != 1 ]; then
        why="only with BIG=1"
    elif [ "${QUICK:-0}" = 1 ] && has_tag "$tags" slow; then
        why="slow: left out by QUICK=1"
    elif [ -n "${ONLY:-}" ] && ! has_tag "$tags" "$ONLY"; then
        why="not in ONLY=$ONLY"
    elif [ -n "$CHANGED_SEL" ] && [ "$CHANGED_SEL" != ALL ] && \
         ! printf '%s\n' $CHANGED_SEL | grep -qx "$dir"; then
        why="not affected by the changes"
    fi
    if [ -n "$why" ]; then
        SKIPPED_ROWS="$SKIPPED_ROWS$dir|$why
"
    else
        SELECTED="$SELECTED $dir:$runner:$(usual_of "$dir" "$def")"
    fi
done

# Expected time: one at a time, the sum; with JOBS > 1, the longest
# suites are started first and each goes to the first free slot.
EST=$(printf '%s\n' $SELECTED | awk -F: -v j="$JOBS" 'NF {t[++n] = $3}
    END { for (i = 1; i <= n; i++) for (k = i + 1; k <= n; k++) if (t[k] > t[i]) {x = t[i]; t[i] = t[k]; t[k] = x}
          for (s = 1; s <= j; s++) f[s] = 0
          for (i = 1; i <= n; i++) { m = 1; for (s = 2; s <= j; s++) if (f[s] < f[m]) m = s; f[m] += t[i] }
          e = 0; for (s = 1; s <= j; s++) if (f[s] > e) e = f[s]; print e + 0 }')
[ "${BIG:-0}" = 1 ] && EST=$(( EST + BIG_EXTRA ))
# with JOBS > 1 the longest go first, so the slots stay full at the end
if [ "$JOBS" -gt 1 ]; then
    ORDER=$(printf '%s\n' $SELECTED | awk -F: 'NF {print $3, $0}' | sort -k1,1nr | cut -d' ' -f2)
else
    ORDER=$SELECTED
fi
NSEL=$(printf '%s\n' $SELECTED | grep -c .)

NFAIL=0
TSTART=$(date +%s)
NEWUSUAL=""
echo "lagrit: $LAGRIT"
[ -n "${ONLY:-}" ] && echo "ONLY=$ONLY"
[ "${QUICK:-0}" = 1 ] && echo "QUICK=1: the slow suites are left out"
if [ -n "$CHANGED_SEL" ]; then
    [ -f "$WHY_FILE" ] && sed 's/^/    /' "$WHY_FILE"
fi
echo "$NSEL suite(s) to run, $JOBS at a time; expected about $(fmt_time $EST)" \
     "(finishing about $(clock_at $(( TSTART + EST ))))"
[ "${BIG:-0}" = 1 ] && echo "    (BIG=1: includes about $BIG_EXTRA s for the slow decks)"
echo "each suite's output goes to run_all.log when it finishes; while it runs:"
echo "    tail -f $TOP/.run_all/<suite>.log"
echo
printf '%-30s %-6s %6s  %s\n' suite result time "summary page"
printf '%-30s %-6s %6s  %s\n' ----- ------ ---- ------------
# the suites left out, one line per reason
printf '%s' "$SKIPPED_ROWS" | awk -F'|' 'NF {n = $1; sub(/^test_/, "", n)
        if (!($2 in l)) o[++k] = $2; l[$2] = l[$2] " " n; c[$2]++}
    END {for (i = 1; i <= k; i++) printf "SKIP %d (%s):%s\n", c[o[i]], o[i], l[o[i]]}' | fold -s -w 78
[ -n "$SKIPPED_ROWS" ] && echo

# ---- run them, JOBS at a time -----------------------------------------
RUN=$TOP/.run_all
rm -rf "$RUN"
mkdir -p "$RUN"
# Ctrl-C stops the suites still running, not only this script
trap 'echo; echo "stopped: killing the suites still running"; kill 0 2>/dev/null; exit 130' INT TERM

start_suite () {      # start_suite DIR RUNNER: in the background
    (
        _t0=$(date +%s)
        _st=$(date +%H:%M:%S)
        (cd "$TOP/$1" && sh "./$2" "$LAGRIT") > "$RUN/$1.log" 2>&1
        _rc=$?
        echo "$_rc $_t0 $(date +%s) $_st" > "$RUN/$1.res.tmp"
        mv "$RUN/$1.res.tmp" "$RUN/$1.res"
    ) &
}
RUNNING=0
reap () {             # report every suite that has finished
    for _e in $STARTED; do
        _d=${_e%%:*}
        [ -f "$RUN/$_d.res" ] && [ ! -f "$RUN/$_d.done" ] || continue
        read -r _rc _t0 _t1 _st < "$RUN/$_d.res"
        : > "$RUN/$_d.done"
        RUNNING=$((RUNNING - 1))
        if [ "$_rc" -eq 0 ]; then _r=PASS; else _r=FAIL; fi
        _p="$_d/summary.html"
        [ -f "$TOP/$_p" ] || _p="(no page; see $_d/logs/)"
        printf '%-30s %-6s %5ss  %s\n' "$_d" "$_r" $((_t1 - _t0)) "$_p"
    done
}
STARTED=""
for e in $ORDER; do
    while [ "$RUNNING" -ge "$JOBS" ]; do
        reap
        [ "$RUNNING" -ge "$JOBS" ] && sleep 1
    done
    d=${e%%:*}
    rest=${e#*:}
    printf '%-30s running  (started %s, usually about %s s)\n' \
        "$d" "$(date +%H:%M:%S)" "${rest#*:}"
    start_suite "$d" "${rest%%:*}"
    STARTED="$STARTED $e"
    RUNNING=$((RUNNING + 1))
done
while [ "$RUNNING" -gt 0 ]; do
    reap
    [ "$RUNNING" -gt 0 ] && sleep 1
done
wait

# ---- collect, in the order of SUITES ----------------------------------
for entry in $SUITES; do
    dir=${entry%%:*}
    if [ -f "$RUN/$dir.res" ]; then
        read -r rc t0 t1 st < "$RUN/$dir.res"
        echo "=================== $dir ===================" >> "$LOG"
        cat "$RUN/$dir.log" >> "$LOG"
        if [ "$rc" -eq 0 ]; then res=PASS; else res=FAIL; NFAIL=$((NFAIL + 1)); fi
        ROWS="$ROWS$dir|$res|$((t1 - t0))|$st
"
        # a passing, non-BIG run of this suite, one suite at a time,
        # becomes its usual time (side by side they take longer)
        if [ "$res" = PASS ] && [ "${BIG:-0}" != 1 ] && [ "$JOBS" = 1 ] && [ $((t1 - t0)) -gt 0 ]; then
            NEWUSUAL="$NEWUSUAL$dir $((t1 - t0))
"
        fi
    else
        w=$(printf '%s' "$SKIPPED_ROWS" | awk -F'|' -v d="$dir" '$1 == d {print $2}')
        case $w in
            MISSING*) ROWS="$ROWS$dir|MISSING|-|$w
"; NFAIL=$((NFAIL + 1)) ;;
            *) ROWS="$ROWS$dir|SKIPPED|-|${w:--}
" ;;
        esac
    fi
done
rm -rf "$RUN"
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
        if [ -f "$TOP/$d/summary.html" ]; then
            link="<a href=\"$d/summary.html\" target=\"_blank\" rel=\"noopener\">$d/summary.html</a>"
        else
            link="(no page: see the runner output below, or <a href=\"$d/logs/\" target=\"_blank\" rel=\"noopener\">$d/logs/</a>)"
        fi
        if [ "$r" = SKIPPED ] || [ "$r" = MISSING ]; then
            echo "<tr><td>$d</td><td class=\"$r\">$r</td><td>-</td><td colspan=\"2\">$(echo "$st" | html_escape)</td></tr>"
            continue
        fi
        echo "<tr><td><a href=\"#$d\" title=\"show this suite's runner output, below\">$d</a></td><td class=\"$r\">$r</td><td>$t s</td><td>$st</td><td>$link</td></tr>"
    done
    echo '</table>'
    echo '<h2>Runner output</h2>'
    printf '%s' "$ROWS" | while IFS='|' read -r d r t st; do
        [ -n "$d" ] || continue
        [ "$r" = SKIPPED ] || [ "$r" = MISSING ] && continue
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
