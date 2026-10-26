#!/usr/bin/env python3
"""check_upstream.py TESTS LOGDIR WORK KNOWN

Judges the LaGriT test-suite runs made by run_upstream_tests.sh.

TESTS   LaGriT's test/ folder (level01/<test>/reference/outx3dgen and
        testlagrit/diff.py are read from it; nothing is written there)
LOGDIR  logs/: tests.txt (level|test|exit status|seconds), run_info.txt;
        this script writes table.txt and summary.html there
WORK    work/: one folder per test, where lagrit ran
KNOWN   known.txt: "test | reason" for documented failures

Each test is judged on three things:
  same      outx3dgen equals reference/outx3dgen by LaGriT's own rules,
            with LaGriT's own code (testlagrit/diff.py, loaded from
            TESTS): remove_junk_from_filestream() drops blank and
            comment lines and the banner; diff_chunk() compares lines
            word by word, numbers to an ABSOLUTE 0.001, anything else
            exactly. Two corrections of ours:
            1. The differing blocks are found here (difflib opcodes).
               diff.py's own diff() reads a one-line block's header
               ("@@ -39 +39 @@", one line each) as 0 lines and compares
               nothing, so it passes ANY isolated one-line difference
               (Oct 16 2026: a reference volume changed from 1.0 to 1.1
               passed). Its verdict is still recorded (lagrit_diff) and
               a disagreement is shown on the page.
            2. A negative zero equals zero ("-0.0000000j" vs
               "0.0000000j"): the sign of a zero depends on compiler
               and platform (diff_chunk's own comment: "check for -0
               and 0", not done).
  exit      lagrit's exit status is 0 (a crash gives 139, a Fortran
            STOP may still give 0)
  finished  the log has "LaGriT successfully completed" (every
            reference has it)
  files     every output file kept in the test's reference/ folder (or
            in this suite's truth/) equals the one the test wrote
            (compare_outputs.py: meshes as meshes, numbers within a
            relative 1e-6, date lines skipped). User, Oct 17 2026.
  own       for a test in extra_checks.py, that check passes (pset:
            every vertex set file against the sets' definitions)
PASS = all of these. A test in known.txt that fails is KNOWN; one that
passes is FIXED (update known.txt). Anything else is FAIL.
notes.txt: a short explanation per test, shown on its row.

Python 3, standard library only.
"""
import contextlib
import difflib
import html
import importlib
import io
import logging
import os
import re
import sys
import types

FINISH = "LaGriT successfully completed"
MAX_DIFF_LINES = 400          # lines of diff detail kept on the page


class Capture(logging.Handler):
    """Collects what testlagrit's diff() logs, one test at a time."""
    def __init__(self):
        super().__init__(level=logging.DEBUG)
        self.lines = []

    def emit(self, record):
        self.lines.append(record.getMessage())


def load_lagrit_diff(tests):
    """Load TESTS/testlagrit/diff.py without running the package's
    __init__.py (it imports find_lagrit.py, which needs distutils,
    gone from Python 3.12). Its logger.py calls logging.basicConfig()
    with a log file: a handler on the root logger first makes that a
    no-op, so no file is written."""
    cap = Capture()
    root = logging.getLogger()
    root.addHandler(cap)
    root.setLevel(logging.DEBUG)
    pkg = types.ModuleType("testlagrit")
    pkg.__path__ = [os.path.join(tests, "testlagrit")]
    sys.modules["testlagrit"] = pkg
    mod = importlib.import_module("testlagrit.diff")
    return mod, cap


# a minus sign in front of a zero value ("-0.0000000", "-0.000E+00",
# "-0.0000000j"), not an exponent's minus ("1.0E-05")
NEG_ZERO = re.compile(r"(?<![0-9A-Za-z.+-])-(?=0*\.?0+(?:[EeDd][+-]?[0-9]+)?(?![0-9.]))")


class _Sink:
    def write(self, *args):
        pass


def strict_compare(mod, out, ref):
    """(number of differing lines, LaGriT's printed detail) by LaGriT's
    own rules (see the header), blocks found with difflib."""
    def lines(path):
        raw = open(path, errors="replace").readlines()
        return [NEG_ZERO.sub("", x)
                for x in mod.remove_junk_from_filestream(raw)]
    r, t = lines(ref), lines(out)
    fails, shown = 0, []
    sm = difflib.SequenceMatcher(None, r, t, autojunk=False)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal":
            continue
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            ifa, _, _ = mod.diff_chunk(r[i1:i2], t[j1:j2], i2 - i1,
                                       j2 - j1, _Sink())
        if ifa:
            fails += ifa
            shown.append("@@ reference line %d, output line %d: %d "
                         "differ" % (i1 + 1, j1 + 1, ifa))
            shown.extend(buf.getvalue().splitlines())
    return fails, shown


def read_known(path):
    known = {}
    if os.path.isfile(path):
        for line in open(path):
            line = line.strip()
            if not line or line.startswith("#") or "|" not in line:
                continue
            name, reason = line.split("|", 1)
            known[name.strip()] = reason.strip()
    return known


def read_notes(path):
    """notes.txt: "== test" then the text, to the next "==" line"""
    out, cur = {}, None
    if os.path.isfile(path):
        for line in open(path):
            if line.startswith("== "):
                cur = line[3:].strip()
                out[cur] = ""
            elif cur and not (line.startswith("#") and not out[cur]):
                out[cur] += line
    return {k: v.strip() for k, v in out.items()}


def load_module(name):
    """a module next to this file (python3 -I does not put this folder
    on sys.path)"""
    import importlib.util
    here = os.path.dirname(os.path.abspath(__file__))
    spec = importlib.util.spec_from_file_location(
        name, os.path.join(here, name + ".py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def make_pictures(drawer, pictures, tests, lev, name, work, logdir):
    """[(svg path relative to logs/, title, description, caption)] for
    the test, as pictures.jobs() lists them: its own AVS mesh files
    (*.inp not among its inputs), the picture run's end state if it
    wrote none, the source/sink pairs of interpolate commands, and the
    extra pictures of pictures.txt. A pair shares one colour scale."""
    src = os.path.join(tests, lev, name)
    wdir = os.path.join(work, name)
    own = sorted(f for f in os.listdir(wdir)
                 if f.endswith(".inp") and not os.path.exists(os.path.join(src, f)))
    todo = pictures.jobs(name, own, wdir, os.path.join(work, "_pic_" + name))
    # a source drawn beside a horizontal sink is seen from above too
    top = set()
    for j in todo:
        if j["pair"] and j["file"].endswith("_sink.inp") and os.path.isfile(j["file"]):
            try:
                m = drawer.read_avs(j["file"])
                n = drawer.plane_normal(m["V"]) if "V" in m else None
            except Exception:
                n = None
            if n is not None and abs(n[2]) > 0.999:
                top.add(j["pair"])
    boxes, sinkbox = {}, {}                 # one x, y frame per pair
    for j in todo:
        if j["pair"] in top and os.path.isfile(j["file"]):
            try:
                V = drawer.read_avs(j["file"]).get("V") or []
            except Exception:
                V = []
            if V:
                b = (min(p[0] for p in V), min(p[1] for p in V),
                     max(p[0] for p in V), max(p[1] for p in V))
                o = boxes.get(j["pair"], b)
                boxes[j["pair"]] = (min(o[0], b[0]), min(o[1], b[1]),
                                    max(o[2], b[2]), max(o[3], b[3]))
                if j["file"].endswith("_sink.inp"):
                    sinkbox[j["pair"]] = b
    # a sink much smaller than its source (less than half the shared
    # frame in both x and y; user, Oct 17: "the sink mesh image is
    # tiny") gets its own frame; the source shows where it lies
    marks = {}
    for pr, b in sinkbox.items():
        f = boxes[pr]
        fx = (b[2] - b[0]) / ((f[2] - f[0]) or 1)
        fy = (b[3] - b[1]) / ((f[3] - f[1]) or 1)
        if fx < 0.5 and fy < 0.5:
            del boxes[pr]
            marks[pr] = b
    ranges = {}
    for j in todo:
        if j["pair"] and os.path.isfile(j["file"]):
            try:
                r = drawer.attr_range(j["file"], j["color"])
            except Exception:
                r = None
            if r:
                lo, hi = ranges.get(j["pair"], r)
                ranges[j["pair"]] = (min(lo, r[0]), max(hi, r[1]))
    pics = []
    os.makedirs(os.path.join(logdir, "pics"), exist_ok=True)
    for k, j in enumerate(todo):
        rel = os.path.join("pics", "%s__%02d.svg" % (name, k + 1))
        try:
            if not os.path.isfile(j["file"]):
                raise ValueError("no file %s" % os.path.basename(j["file"]))
            desc = drawer.draw(j["file"], os.path.join(logdir, rel), j["title"],
                               j["color"], ranges.get(j["pair"]),
                               "top" if j["pair"] in top else None,
                               boxes.get(j["pair"]),
                               marks.get(j["pair"]) if j["file"].endswith("_src.inp")
                               else None)
        except Exception as e:             # a picture never stops the run
            desc = "not drawn: %s" % e
            rel = None
        pics.append((rel, j["title"], desc, j["caption"]))
    return pics


def main():
    tests, logdir, work, knownfile = sys.argv[1:5]
    mod, cap = load_lagrit_diff(tests)
    drawer = load_module("draw_meshes")
    pictures = load_module("pictures")
    comparer = load_module("compare_outputs")
    extra = load_module("extra_checks")
    notes = read_notes(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "notes.txt"))
    truth_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "truth")
    known = read_known(knownfile)
    info = {}
    for line in open(os.path.join(logdir, "run_info.txt")):
        k, _, v = line.rstrip("\n").partition("|")
        info[k] = v

    rows = []
    for line in open(os.path.join(logdir, "tests.txt")):
        lev, name, rc, secs = line.strip().split("|")
        rc = int(rc)
        out = os.path.join(work, name, "outx3dgen")
        ref = os.path.join(tests, lev, name, "reference", "outx3dgen")
        have_out = os.path.isfile(out)
        finished = have_out and FINISH in open(out, errors="replace").read()
        cap.lines = []
        same = False
        lagrit_same = None
        nfail = 0
        detail = []
        if have_out and os.path.isfile(ref):
            printed = io.StringIO()         # diff.py also print()s
            try:
                with contextlib.redirect_stdout(printed):
                    lagrit_same = bool(mod.diff(out, ref))
                nfail, detail = strict_compare(mod, out, ref)
                same = nfail == 0
            except Exception as e:          # report, never stop the run
                detail.append("comparison raised %r" % e)
        # the output files against the stored truth (compare_outputs.py)
        files, fbad = comparer.compare(name, os.path.join(tests, lev, name),
                                       os.path.join(work, name), truth_dir)
        # checks of our own (extra_checks.py)
        xok, xlines = True, []
        if name in extra.CHECKS:
            try:
                xok, xlines = extra.CHECKS[name](os.path.join(work, name))
            except Exception as e:
                xok, xlines = False, ["the check raised %r" % e]
        good = same and rc == 0 and finished and fbad == 0 and xok
        why = []
        if not have_out:
            why.append("no outx3dgen")
        elif not same:
            why.append("%d lines differ from reference" % nfail)
            if lagrit_same:
                why.append("LaGriT's diff.py misses it")
        if rc != 0:
            why.append("exit status %d%s" % (rc, " (crash)" if rc > 128 else ""))
        if have_out and not finished:
            why.append("did not finish")
        if fbad:
            why.append("%d output file(s) differ from the truth" % fbad)
        if not xok:
            why.append("our own check failed")
        if name in known:
            verdict = "FIXED" if good else "KNOWN"
        else:
            verdict = "PASS" if good else "FAIL"
        if verdict == "KNOWN":
            note = "%s [found: %s]" % (known[name], "; ".join(why))
        elif verdict == "FIXED":
            note = "now passes: remove it from known.txt (%s)" % known[name]
        else:
            note = "; ".join(why)
        pics = make_pictures(drawer, pictures, tests, lev, name, work, logdir)
        rows.append(dict(lev=lev, name=name, rc=rc, secs=int(secs), pics=pics,
                         files=files, xok=xok, xlines=xlines,
                         notes=notes.get(name, ""),
                         same=same, finished=finished, verdict=verdict,
                         note=note, why="; ".join(why), detail=detail))

    # table.txt (and the screen)
    fmt = "%-24s %-8s %-6s %5s  %s"
    table = [fmt % ("test", "level", "result", "time", "note")]
    for r in rows:
        table.append(fmt % (r["name"], r["lev"], r["verdict"],
                            "%ds" % r["secs"], r["note"]))
    count = {}
    for r in rows:
        count[r["verdict"]] = count.get(r["verdict"], 0) + 1
    summary = "pass %d   known %d   fixed %d   fail %d   (of %d tests)" % (
        count.get("PASS", 0), count.get("KNOWN", 0), count.get("FIXED", 0),
        count.get("FAIL", 0), len(rows))
    open(os.path.join(logdir, "table.txt"), "w").write("\n".join(table) + "\n")
    for t in table:
        print(t)
    print()
    print("summary page: logs/summary.html")
    print(summary)

    write_page(os.path.join(logdir, "summary.html"), rows, info, summary,
               work)
    return 1 if count.get("FAIL", 0) else 0


# Comment boxes (user, Oct 16 2026): the same as on the test_createpts
# page (its runner holds the original): one box per test, kept in this
# browser's localStorage per suite; "Copy all comments" / "Save as file"
# (test_lagrit_upstream_comments.txt) to send back.
COMMENT_JS = r"""<script>
// Per-test comments: kept in this browser (localStorage, per suite),
// copied or saved as one text file to send back to Claude.
var CM_SUITE = 'test_lagrit_upstream';
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
</script>"""


def write_page(path, rows, info, summary, work):
    e = html.escape
    colour = {"PASS": "#1a7f37", "KNOWN": "#9a6700", "FIXED": "#0969da",
              "FAIL": "#cf222e"}
    out = []
    out.append("""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>LaGriT test suite (upstream)</title>
<style>
body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;
  background:#fff;color:#1f2328;max-width:1100px}
h1{font-size:20px;margin:0 0 6px} p,li{font-size:14px;line-height:1.45}
table{border-collapse:collapse;font-size:13px;width:100%}
th,td{border-bottom:1px solid #d0d7de;padding:4px 8px;text-align:left;
  vertical-align:top}
th{background:#f6f8fa}
.v{font-weight:600}
pre{background:#f6f8fa;padding:8px;overflow-x:auto;font-size:12px;
  max-height:480px}
details{margin:4px 0} summary{cursor:pointer}
code{font-size:12px}
.top{margin:0 0 8px;font-size:13px}
.pics{display:flex;flex-wrap:wrap;gap:6px;margin:6px 0}
.pic{width:300px;font-size:11px;color:#555}
.pic img{width:300px;height:250px;border:1px solid #d0d7de;display:block}
.cap{flex:0 0 100%;font-size:12px;margin-top:4px}
.note{font-size:13px;background:#f6f8fa;border-left:3px solid #8c959f;padding:2px 10px;margin:6px 0;max-width:900px}
.note p{margin:6px 0}
table.ft{font-size:12px;width:auto}table.ft td{padding:1px 8px;border:none}
.cm{width:100%;box-sizing:border-box;font:13px sans-serif;padding:6px;margin:4px 0 6px;min-height:2.4em;border:1px solid #bbb;border-radius:4px;background:#fffef5}
.cm.has{border-color:#e8a200;background:#fff6dd}
#cbar{position:sticky;top:0;z-index:5;background:#223;color:#fff;padding:8px 12px;margin:0 -16px 12px;display:flex;gap:12px;align-items:center;flex-wrap:wrap}
#cbar button{font:13px sans-serif;padding:4px 10px;cursor:pointer}
#cbar .msg{font-size:12px;opacity:.85}
</style></head><body>
<p class="top"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<div id="cbar"><b>Comments: <span id="ccount">0</span></b>
<button onclick="cmCopy()">Copy all comments</button>
<button onclick="cmSave()">Save as file</button>
<button onclick="cmClear()">Clear all</button>
<span class="msg" id="cmsg">Type under any test; an empty box means no change. Comments are kept in this browser until you clear them.</span></div>
<h1>LaGriT's own test suite</h1>
""")
    out.append("<p><b>%s</b></p>" % e(summary))
    out.append("<p id=\"runinfo\">lagrit: <code>%s</code><br>tests: <code>%s</code> (%s)"
               % (e(info.get("lagrit", "")), e(info.get("tests", "")),
                  e(info.get("levels", ""))))
    if info.get("tree"):
        out.append("<br>tree: <code>%s</code>" % e(info["tree"]))
    out.append("</p>")
    odd = [r for r in rows if r["verdict"] != "PASS"]
    if odd:
        out.append("<p>Not passing:</p><ul>")
        for r in odd:
            out.append('<li><a href="#%s">%s</a> <span class="v" '
                       'style="color:%s">%s</span>: %s</li>'
                       % (e(r["name"]), e(r["name"]), colour[r["verdict"]],
                          r["verdict"], e(r["note"])))
        out.append("</ul>")
    out.append("<table><tr><th>test</th><th>level</th><th>result</th>"
               "<th>time</th><th>details</th></tr>")
    for r in rows:
        out.append('<tr id="%s"><td>%s</td><td>%s</td><td class="v" '
                   'style="color:%s">%s</td><td>%ds</td><td>'
                   % (e(r["name"]), e(r["name"]), e(r["lev"]),
                      colour[r["verdict"]], r["verdict"], r["secs"]))
        out.append('<textarea class="cm" data-test="%s" data-result="%s" '
                   'placeholder="comment on %s (empty = no change)"></textarea>'
                   % (e(r["name"]), e(r["verdict"] + (": " + r["note"] if r["note"] else "")),
                      e(r["name"])))
        if r["note"]:
            out.append("<div>%s</div>" % e(r["note"]))
        if r["verdict"] != "PASS" and r["detail"]:
            d = r["detail"]
            more = ""
            if len(d) > MAX_DIFF_LINES:
                more = "\n... %d more lines" % (len(d) - MAX_DIFF_LINES)
                d = d[:MAX_DIFF_LINES]
            out.append("<details><summary>LaGriT's diff (output vs "
                       "reference)</summary><pre>%s%s</pre></details>"
                       % (e("\n".join(d)), e(more)))
        if r["pics"]:
            out.append('<div class="pics">')
            for rel, title, desc, caption in r["pics"]:
                if caption:
                    out.append('<div class="cap"><code>%s</code></div>' % e(caption))
                if rel:
                    out.append('<div class="pic"><a href="%s" target="_blank">'
                               '<img loading="lazy" src="%s" alt="%s"></a></div>'
                               % (e(rel), e(rel), e(title + ": " + desc)))
                else:
                    out.append('<div class="pic">%s: %s</div>' % (e(title), e(desc)))
            out.append("</div>")
        else:
            out.append('<div class="pic">no mesh to draw</div>')
        if r["notes"]:
            out.append('<div class="note">%s</div>' % "".join(
                "<p>%s</p>" % e(par).replace("\n", "<br>")
                for par in r["notes"].split("\n\n")))
        if r["files"]:
            cnt = {}
            for _, v, _ in r["files"]:
                cnt[v] = cnt.get(v, 0) + 1
            bad = cnt.get("DIFFERENT", 0)
            head = ", ".join("%d %s" % (n, v) for v, n in sorted(cnt.items()))
            out.append('<details%s><summary>output files against the truth: '
                       '%s</summary><table class="ft">' % (" open" if bad else "", e(head)))
            for f, v, d in r["files"]:
                out.append('<tr><td>%s</td><td class="v" style="color:%s">%s</td>'
                           '<td>%s</td></tr>' % (e(f), "#cf222e" if v == "DIFFERENT"
                                                 else "#1a7f37" if v.startswith("same")
                                                 else "#777", e(v), e(d)))
            out.append("</table></details>")
        if r["xlines"]:
            out.append('<details%s><summary>our own check: %s</summary><pre>%s</pre>'
                       '</details>' % ("" if r["xok"] else " open",
                                       "PASS" if r["xok"] else "FAIL",
                                       e("\n".join(r["xlines"]))))
        deck = os.path.join(work, r["name"], "input.lgi")
        if os.path.isfile(deck):
            out.append("<details><summary>LaGriT control file</summary>"
                       "<pre>%s</pre></details>"
                       % e(open(deck, errors="replace").read()))
        out.append("</td></tr>")
    out.append("</table>")
    out.append(COMMENT_JS)
    out.append("</body></html>")
    open(path, "w").write("\n".join(out) + "\n")


if __name__ == "__main__":
    sys.exit(main())
