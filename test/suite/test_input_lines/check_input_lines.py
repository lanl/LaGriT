#!/usr/bin/env python3
"""check_input_lines.py WORKDIR LOGDIR

Judges test_input_lines (Python 3, standard library only) from each
deck's output (WORKDIR/<deck>.out) against WORKDIR/expect.json, which
make_input_lines.py writes with the decks.

A deck PASSES when
  - LaGriT finished ("LaGriT successfully completed"), with no signal,
    no MMVERIFY message and no "empty command stack" (LaGriT giving up
    the deck and going back to the terminal, which the old over-long
    continuation check did);
  - every mesh object in "mo" exists and none in "no_mo" does (from
    the cmo / status / brief at the end of the deck: a command that
    was skipped never made its mesh object, and a continuation line
    of a skipped command was not run on its own);
  - every text in "has" appears in the output;
  - each text in "count" appears exactly that many times.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import os
import re
import sys

E = html.escape


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
        for bad in ("Program received signal", "MMVERIFY", "empty command stack"):
            if bad in text:
                fails.append('"%s" in the output' % bad)
        names = set(re.findall(r"Mesh Object name:\s*(\S+)", text))
        for m in e["mo"]:
            if m not in names:
                fails.append("mesh object %s missing" % m)
        for m in e["no_mo"]:
            if m in names:
                fails.append("mesh object %s made (its command should have been skipped)" % m)
        for t in e["has"]:
            if t not in text:
                fails.append('no "%s"' % t)
        for t, n in e["count"].items():
            got = text.count(t)
            if got != n:
                fails.append('"%s" %d times, expected %d' % (t, got, n))
        msgs = [l.rstrip() for l in text.splitlines()
                if l.startswith(("NOTE:", "ERROR", "WARNING: token", "  It starts", "  Command skipped",
                                 "  LaGriT V3.3.4", "  joined with", "  \"")) or re.match(r"^  \d+ characters", l)]
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, msgs, e))
        print("  %-4s  %s%s" % (res, deck, ("  " + "; ".join(fails[:3])) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, r) for d, r, _, _, _ in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>input line limits</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}pre{font-size:11px;background:#f6f8fa;padding:6px;margin:2px 0;white-space:pre-wrap}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">Input line limits: long lines, continuation lines, long tokens</h1>
<p>A command line of up to 80 characters is read as always. From 81 to 1024 characters it is used in full with a NOTE
(LaGriT V3.3.4 and earlier silently ignored everything past column 80). Over 1024 characters, or over 16384 characters
for a command joined from &amp; continuation lines, it is an ERROR: the command is skipped, never cut or run in part,
its continuation lines are skipped with it, and LaGriT goes on with the next command. A word token over 32 characters
is cut to 32 with a WARNING. Each deck below says what it holds; the messages are LaGriT's own.</p>
<table><tr><th>deck</th><th>result</th><th>mesh objects made / not made</th><th>LaGriT's messages</th></tr>"""]
    for d, r, fails, msgs, e in rows:
        o.append('<tr><td>%s</td><td class="%s">%s</td><td>%s<br><i>not:</i> %s</td><td><pre>%s</pre>%s</td></tr>'
                 % (E(d), r, r, E(", ".join(e["mo"])), E(", ".join(e["no_mo"]) or "-"),
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
