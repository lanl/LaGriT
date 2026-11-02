#!/usr/bin/env python3
"""make_page.py LOGDIR: logs/summary.html from table.txt and the
*.check files of test_avs_io. Python 3, standard library only."""
import glob
import html
import os
import sys

E = html.escape
logs = sys.argv[1]
rows = open(os.path.join(logs, "table.txt")).read().split("\n")[1:]
here = os.path.dirname(os.path.abspath(__file__))
P = ['<!doctype html><html><head><meta charset="utf-8"><title>test_avs_io</title>',
     "<style>body{font-family:sans-serif;margin:16px;max-width:1100px}"
     "td{padding:2px 10px 2px 0;vertical-align:top}"
     ".PASS{color:#070;font-weight:bold}.KNOWN{color:#a60;font-weight:bold}"
     ".FAIL,.FIXED{color:#c00;font-weight:bold}"
     "pre{background:#f4f4f4;padding:6px;font-size:12px;margin:2px 0 10px}"
     "</style></head><body>",
     "<h1>test_avs_io: read / avs and dump / avs</h1>",
     "<p>Every mesh LaGriT reads is compared with the file it came from "
     "(the checker reads AVS itself); every file LaGriT writes is compared "
     "byte for byte with <code>golden/</code>. KNOWN = a documented problem "
     "not yet fixed; FIXED = it now passes, so its "
     "<code># known:</code> line should go. See 00README.</p>",
     "<table><tr><th>deck</th><th>result</th><th>what it tests / detail</th></tr>"]
for r in rows:
    w = r.split(None, 2)
    if len(w) < 2:
        continue
    name, res = w[0], w[1]
    det = w[2] if len(w) > 2 else ""
    deck = os.path.join(here, name + ".lgi")
    title = ""
    if os.path.exists(deck):
        t = [l[2:] for l in open(deck).read().split("\n")
             if l.startswith("# ") and not l.startswith(("# check:", "# known:"))]
        title = " ".join(t)
    chk = os.path.join(logs, name + ".check")
    body = open(chk).read() if os.path.exists(chk) else ""
    P.append('<tr><td><a href="%s.stdout">%s</a></td><td class="%s">%s</td>'
             '<td>%s<br><small>%s</small><details><summary>checks</summary>'
             '<pre>%s</pre></details></td></tr>'
             % (E(name), E(name), E(res), E(res), E(title), E(det), E(body)))
P.append("</table></body></html>")
open(os.path.join(logs, "summary.html"), "w").write("\n".join(P))
