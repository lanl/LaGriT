#!/usr/bin/env python3
"""check_cmo_commands.py WORKDIR LOGDIR

Judges test_cmo_commands (Python 3, standard library only) from each
deck's output (WORKDIR/<deck>.out) against WORKDIR/expect.json, which
make_cmo_commands.py writes with the decks.

A deck PASSES when
  - LaGriT finished ("LaGriT successfully completed"), with no signal
    and no MMVERIFY message;
  - every text in "has" appears in the output, none in "lacks" does,
    and each text in "count" appears exactly that many times.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import os
import re
import sys

E = html.escape


def about():
    """{deck: what it holds}, from the list in make_cmo_commands.py"""
    out, cur = {}, None
    src = open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "make_cmo_commands.py")).read()
    for l in src.split('"""')[1].split("\n"):
        m = re.match(r"^  (cc\d\d_\w+)\s+(.*)$", l)
        if m:
            cur = m.group(1)
            out[cur] = m.group(2).strip()
        elif cur and l.startswith("      ") and l.strip():
            out[cur] += " " + l.strip()
        else:
            cur = None
    return out


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows = []
    ok_all = True
    for deck in sorted(exp):
        e = exp[deck]
        p = os.path.join(work, deck + ".out")
        text = open(p, errors="replace").read() if os.path.isfile(p) else ""
        fails = []
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for bad in ("Program received signal", "MMVERIFY"):
            if bad in text:
                fails.append('"%s" in the output' % bad)
        for t in e["has"]:
            if t not in text:
                fails.append('no "%s"' % t)
        for t in e["lacks"]:
            if t in text:
                fails.append('"%s" in the output' % t)
        for t, n in e["count"].items():
            got = text.count(t)
            if got != n:
                fails.append('"%s" %d times, expected %d' % (t, got, n))
        msgs = [l.rstrip() for l in text.splitlines()
                if l.startswith(("ERROR", "delatt -all-", "DELATT -all-", " Mesh Object does not exist",
                                 "  (an undefined")) or l.startswith((" a  ", " b  ", " imt1 "))]
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, msgs, e))
        print("  %-4s  %s%s" % (res, deck, ("  " + "; ".join(fails[:3])) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, r) for d, r, _, _, _ in rows)
    ABOUT = about()
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>cmo commands</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}pre{font-size:11px;background:#f6f8fa;padding:6px;margin:2px 0;white-space:pre-wrap}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">cmo, math and setatt: small fixes (GitHub #163, #108, #159, #31, #55)</h1>
<p>cmo / delatt / mo / -all- deletes every temporary attribute and keeps the permanent and standard ones; cmo / DELATT /
mo / -all- deletes every attribute but the standard ones (those cmo / create makes). cmo / status of a deleted mesh
object names it. math takes imt for imt1 and refuses an unknown attribute with an ERROR. cmo / setatt refuses a word
(an undefined define name) as the value of a number attribute.</p>
<p><b>What it does:</b> each deck runs the commands and the checker reads LaGriT's output for the texts it must and
must not hold. No mesh is drawn: these tests are about commands and their messages.</p>
<table><tr><th>deck</th><th>result</th><th>LaGriT's lines</th></tr>"""]
    for d, r, fails, msgs, e in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td><td><pre>%s</pre>%s</td></tr>'
                 % (E(d), E(ABOUT.get(d, "")), r, r,
                    E("\n".join(msgs) or "(none)"),
                    "".join('<div class="FAIL">%s</div>' % E(x) for x in fails)))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r == "PASS" for _, r, _, _, _ in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
