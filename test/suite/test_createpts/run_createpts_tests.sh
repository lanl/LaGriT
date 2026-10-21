#!/bin/sh
#######################################################################
# run_createpts_tests.sh [path-to-lagrit]
#
# Runs every cp*.lgi deck listed in DECKS.txt (createpts) and judges
# it; other cp*.lgi (old decks) are named and skipped. See 00README.
#
# Each deck header carries (all read only from lines starting "#"):
#   # group   : xyz | rtz | rtp | line | brick | errors
#   # expect  : valid | error
#   # reason  : <text the ERROR must contain>          (expect error)
#   # known   : <a documented failure, in one line>
#   # warns   : <text a WARNING line must contain>     (expect valid)
#   # spec, # points, # mesh, # filter, # open, # attr, # unused_ok
#                                read by check_createpts.py
#
# Results:
#   PASS      expect valid: lagrit ran, check_createpts.py's HARD checks
#             pass and nothing undocumented was reported
#   REPORTED  expect valid, HARD checks pass, but the documentation does
#             not say what should happen (# open) or the checker noted
#             something to look at (NOTE lines): see the row's report
#   REFUSED   expect error: an ERROR containing the reason text
#   KNOWN     a deck with "# known": it failed, as documented
#   FIXED     a deck with "# known": it now passes (update the deck)
#   FAIL      anything else
# Exit status: nonzero on any FAIL.
#
# LaGriT ECHOES every deck line into its output, comments included, so
# every search of the log skips comment lines. LaGriT keeps only the
# first 32 characters of a token, so a deck with a longer token FAILS
# before it is run.
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
rm -f "${LOGDIR:?}"/* out_cp*.inp lagrit.log lagrit.out
PASS=0; REP=0; REFU=0; KNOWN=0; FIXED=0; FAIL=0

hdr () {   # hdr <key> <deck>: value of "# key : value"
    sed -n "s/^# *$1 *: *//p" "$2" | head -1
}
nocomment () {
    grep -v '^[[:space:]]*#' "$1"
}
esc () {
    sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'
}

FMT="%-34s %-7s %8s  %s\n"
printf "$FMT" test group vertices result | tee $TABLE
# decks not in DECKS.txt (left over from an older archive) are not run
STALE=""
for f in cp*.lgi; do
    [ -e "$f" ] || continue
    if [ -f DECKS.txt ] && ! grep -qx -- "$f" DECKS.txt; then
        STALE="$STALE $f"
        continue
    fi
    base=`basename $f .lgi`
    grp=`hdr group $f`
    exp=`hdr expect $f`
    why=`hdr reason $f`
    known=`hdr known $f`
    warns=`hdr warns $f`
    res=""
    case "$exp" in valid|error) ;; *) res="FAIL  missing or unknown '# expect' line" ;; esac
    if [ -z "$res" ]; then
        # (lines starting with * are LaGriT comments: not checked)
        long=`nocomment $f | grep -v '^ *\*' | tr '/,\t ' '\n\n\n\n' | awk '{gsub(/^[ \t]+|[ \t]+$/,"")} length > 32' | head -1`
        [ -n "$long" ] && res="FAIL  token over 32 characters (LaGriT truncates it): $long"
    fi
    if [ -z "$res" ]; then
        # LaGriT reads only the first 80 columns of an input line
        wide=`grep -v '^ *[#*]' $f | awk 'length > 80 {print NR; exit}'`
        [ -n "$wide" ] && res="FAIL  a line over 80 characters (LaGriT reads 80; use &): line $wide"
    fi
    npts=-
    if [ -z "$res" ]; then
        rm -f lagrit.log lagrit.out
        # in a subshell whose stderr is discarded, so a crash is not
        # announced on the terminal ("Segmentation fault: 11"); the
        # exit status still says it, and the table reports it
        ( $LAGRIT < $f > $LOGDIR/$base.stdout 2>&1; exit $? ) 2>/dev/null
        rc=$?
        [ -f lagrit.log ] && cp lagrit.log $LOGDIR/$base.lagrit.log
        [ -f lagrit.out ] && cp lagrit.out $LOGDIR/$base.lagrit.out
        cat $LOGDIR/$base.stdout $LOGDIR/$base.lagrit.log \
            $LOGDIR/$base.lagrit.out > $LOGDIR/$base.all 2>/dev/null
        nocomment $LOGDIR/$base.all > $LOGDIR/$base.msgs
        nerr=`grep -c -i 'ERROR\|Illegal' $LOGDIR/$base.msgs`
        if [ "$rc" -gt 1 ]; then
            res="FAIL  lagrit exit status $rc (crash: `grep -m1 -o 'SIG[A-Z]*' $LOGDIR/$base.stdout`)"
        elif [ "$exp" = error ]; then
            if [ "$nerr" -lt 1 ]; then
                res="FAIL  expected an ERROR, none reported"
            elif [ -n "$why" ] && ! grep -q -i -- "$why" $LOGDIR/$base.msgs; then
                res="FAIL  refused, but not for: $why"
            else
                res="REFUSED  `grep -i -m1 -- "${why:-ERROR}" $LOGDIR/$base.msgs | sed 's/^ *//; s/  */ /g' | cut -c1-72`"
            fi
        elif grep -q -i 'error in command' $LOGDIR/$base.msgs; then
            res="FAIL  a command failed: `grep -i -m1 'error in command' $LOGDIR/$base.msgs | sed 's/^ *//; s/  */ /g' | cut -c1-60`"
        else
            python3 check_createpts.py $f $LOGDIR/$base > $LOGDIR/$base.check 2>&1
            crc=$?
            npts=`awk '$1=="vertices_written"{print $2}' $LOGDIR/$base.check`
            if [ $crc -ne 0 ]; then
                h=`grep -m1 -- '-> HARD' $LOGDIR/$base.check | sed 's/.*HARD: //' | cut -c1-70`
                [ -z "$h" ] && h="checker could not run, see $LOGDIR/$base.check"
                res="FAIL  $h"
            elif [ -n "$warns" ] && ! grep -i 'warn' $LOGDIR/$base.msgs | grep -q -i -- "$warns"; then
                res="FAIL  no WARNING containing: $warns"
            elif grep -q 'NOTE:' $LOGDIR/$base.check; then
                res="REPORTED  `grep -m1 'NOTE:' $LOGDIR/$base.check | sed 's/.*NOTE: //' | cut -c1-70`"
            else
                res=PASS
            fi
        fi
    fi
    if [ -n "$known" ]; then
        case "$res" in
            FAIL*) res="KNOWN  $known" ;;
            *)     res="FIXED  (was: $known) now: $res" ;;
        esac
    fi
    case "$res" in
        PASS*)     PASS=$((PASS + 1)) ;;
        REPORTED*) REP=$((REP + 1)) ;;
        REFUSED*)  REFU=$((REFU + 1)) ;;
        KNOWN*)    KNOWN=$((KNOWN + 1)) ;;
        FIXED*)    FIXED=$((FIXED + 1)) ;;
        *)         FAIL=$((FAIL + 1)) ;;
    esac
    printf "$FMT" "$base" "${grp:--}" "${npts:--}" "$res" | tee -a $TABLE
done
rm -f lagrit.log lagrit.out

TALLY="pass $PASS   reported $REP   refused $REFU   known $KNOWN   fixed $FIXED   fail $FAIL"
if [ -n "$STALE" ]; then
    n=`echo $STALE | wc -w | tr -d ' '`
    echo
    echo "NOT RUN: $n old deck(s) not in DECKS.txt (left by an older archive):"
    for f in $STALE; do echo "    $f"; done
    echo "delete them:  rm$STALE"
fi
{
    echo '<!DOCTYPE html><html><head><meta charset="utf-8">'
    echo '<title>createpts tests</title><style>'
    echo 'body{font-family:sans-serif;margin:24px;max-width:1100px}'
    echo 'pre{font-size:12px;background:#f6f6f6;padding:8px;overflow-x:auto}'
    echo 'h2{font-size:14px;font-family:monospace;white-space:pre-wrap;margin:24px 0 4px}'
    echo 'h1 + p, .grp{margin:4px 0}.grp{font-size:18px;border-bottom:2px solid #999;margin-top:36px}'
    echo 'section{border-top:1px solid #ddd;scroll-margin-top:56px}'
    echo '#list a{text-decoration:none}#list a:hover{text-decoration:underline}'
    echo 'a.top{font-size:12px;font-weight:normal;margin-left:12px}'
    echo '.pics{display:flex;flex-wrap:wrap;gap:16px;align-items:flex-start}'
    echo '.pic{flex:1 1 380px;min-width:0;max-width:540px}.pic svg{width:100%;height:auto}'
    echo '.pic.wide{flex:1 1 100%;max-width:1100px}'
    echo '.cm{width:100%;box-sizing:border-box;font:13px sans-serif;padding:6px;margin:6px 0;min-height:2.4em;border:1px solid #bbb;border-radius:4px;background:#fffef5}'
    echo '.cm.has{border-color:#e8a200;background:#fff6dd}'
    echo '#cbar{position:sticky;top:0;z-index:5;background:#223;color:#fff;padding:8px 12px;margin:0 -24px 12px;display:flex;gap:12px;align-items:center;flex-wrap:wrap}'
    echo '#cbar button{font:13px sans-serif;padding:4px 10px;cursor:pointer}'
    echo '#cbar .msg{font-size:12px;opacity:.85}'
    echo '</style></head><body>'
    echo '<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>'
    echo '<div id="cbar"><b>Comments: <span id="ccount">0</span></b>'
    echo '<button onclick="cmCopy()">Copy all comments</button>'
    echo '<button onclick="cmSave()">Save as file</button>'
    echo '<button onclick="cmClear()">Clear all</button>'
    echo '<span class="msg" id="cmsg">Type under any row; an empty box means no change. Comments are kept in this browser until you clear them.</span></div>'
    echo '<h1>createpts tests</h1>'
    echo "<p id=\"runinfo\">lagrit: `echo $LAGRIT | esc`<br>run: `date`</p>"
    # each test name in the list links to its section (user, Oct 16)
    echo "<p><b>$TALLY</b></p><pre id=\"list\">"
    esc < $TABLE | sed 's|^\(cp[0-9][0-9]*_[A-Za-z0-9_]*\)|<a href="#\1">\1</a>|'
    echo '</pre>'
    echo '<p>Each row: the vertices createpts wrote (left; seen from the'
    echo '+x, -y side, z up; flat sets from above; darker = nearer) and the'
    echo 'mesh made from them (right: connect, or the brick cells; the outer'
    echo 'faces, shaded). A RED RING marks vertices at the same place, with'
    echo 'their count. Expected vertex positions come from the documentation; see'
    echo '00README and the opening comment of check_createpts.py.</p>'
    last=""
    for f in cp*.lgi; do
        case " $STALE " in *" $f "*) continue ;; esac
        b=`basename $f .lgi`
        g=`hdr group $f`
        if [ "$g" != "$last" ]; then
            echo "<h2 class=\"grp\">$g</h2>"
            last=$g
        fi
        echo "<section id=\"$b\">"
        echo "<h2>`grep "^$b " $TABLE | head -1 | esc` <a class=\"top\" href=\"#list\">back to the list</a></h2>"
        echo "<textarea class=\"cm\" data-test=\"$b\" data-result=\"`grep "^$b " $TABLE | head -1 | sed 's/^[^ ]* *[^ ]* *[^ ]* *//' | esc | sed 's/\"/\&quot;/g'`\" placeholder=\"comment on $b (empty = no change)\"></textarea>"
        echo '<details><summary>LaGriT control file</summary><pre>'
        esc < $f
        echo '</pre></details>'
        [ -f $LOGDIR/$b.check ] && {
            echo '<details><summary>check report</summary><pre>'
            esc < $LOGDIR/$b.check
            echo '</pre></details>'
        }
        echo '<div class="pics">'
        [ -f $LOGDIR/${b}_pts.svg ] && { echo '<div class="pic">'; cat $LOGDIR/${b}_pts.svg; echo '</div>'; }
        # a picture of several meshes (# picture : panels) gets a row
        # of its own, full width
        case "`hdr picture $f`" in
            panels*) [ -f $LOGDIR/${b}_mesh.svg ] && { echo '</div><div class="pics"><div class="pic wide">'; cat $LOGDIR/${b}_mesh.svg; echo '</div>'; } ;;
            *)       [ -f $LOGDIR/${b}_mesh.svg ] && { echo '<div class="pic">'; cat $LOGDIR/${b}_mesh.svg; echo '</div>'; } ;;
        esac
        echo '</div></section>'
    done
    cat <<'JS'
<script>
// Per-test comments: kept in this browser (localStorage, per suite),
// copied or saved as one text file to send back to Claude.
var CM_SUITE = 'test_createpts';
var CM_KEY = 'lagrit-comments:' + CM_SUITE + ':';
function cmBoxes() { return Array.prototype.slice.call(document.querySelectorAll('textarea.cm')); }
function cmStore(k, v) { try { if (v) localStorage.setItem(CM_KEY + k, v); else localStorage.removeItem(CM_KEY + k); } catch (e) {} }
function cmLoad(k) { try { return localStorage.getItem(CM_KEY + k) || ''; } catch (e) { return ''; } }
function cmMark(t) { t.classList.toggle('has', t.value.trim() !== ''); t.style.height = 'auto'; t.style.height = (t.scrollHeight + 2) + 'px'; }
function cmCount() { var n = cmBoxes().filter(function (t) { return t.value.trim() !== ''; }).length; document.getElementById('ccount').textContent = n; return n; }
function cmText() {
  var lines = ['# comments on ' + CM_SUITE + ' (' + document.getElementById('runinfo').innerText.replace(/\s+/g, ' ').trim() + ')',
               '# saved ' + new Date().toString(), ''];
  cmBoxes().forEach(function (t) {
    var v = t.value.trim();
    if (!v) return;
    lines.push('== ' + t.dataset.test + '   [' + t.dataset.result + ']');
    v.split('\n').forEach(function (l) { lines.push('   ' + l); });
    lines.push('');
  });
  if (lines.length === 3) lines.push('(no comments)');
  return lines.join('\n') + '\n';
}
function cmMsg(s) { document.getElementById('cmsg').textContent = s; }
function cmCopy() {
  var s = cmText();
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(s).then(function () { cmMsg('Copied ' + cmCount() + ' comment(s): paste them into the chat.'); },
      function () { cmFallback(s); });
  } else { cmFallback(s); }
}
function cmFallback(s) {
  var ta = document.createElement('textarea'); ta.value = s; document.body.appendChild(ta); ta.select();
  try { document.execCommand('copy'); cmMsg('Copied ' + cmCount() + ' comment(s).'); } catch (e) { cmMsg('Copy failed: use Save as file.'); }
  document.body.removeChild(ta);
}
function cmSave() {
  var a = document.createElement('a');
  a.href = URL.createObjectURL(new Blob([cmText()], {type: 'text/plain'}));
  a.download = CM_SUITE + '_comments.txt';
  document.body.appendChild(a); a.click(); document.body.removeChild(a);
  cmMsg('Saved ' + CM_SUITE + '_comments.txt (in Downloads): attach it in the chat.');
}
function cmClear() {
  if (!confirm('Clear every comment on this page?')) return;
  cmBoxes().forEach(function (t) { t.value = ''; cmStore(t.dataset.test, ''); cmMark(t); });
  cmCount(); cmMsg('All comments cleared.');
}
cmBoxes().forEach(function (t) {
  t.value = cmLoad(t.dataset.test); cmMark(t);
  t.addEventListener('input', function () { cmStore(t.dataset.test, t.value.trim() ? t.value : ''); cmMark(t); cmCount(); });
});
cmCount();
</script>
JS
    echo '</body></html>'
} > $HTML

echo
echo "summary page: $HTML"
echo "$TALLY"
[ "$FAIL" -eq 0 ] || exit 1
exit 0
