#!/usr/bin/env python3
"""make_page.py WORKDIR LOGDIR

LOGDIR/summary.html for test_crush_thin_tets, from the LOGDIR/<tag>.json
files written by check_crush.py: per deck, the quality before and after
crush_thin_tets in crush's own measure (relative width) and in Carl
Gable's classes (dihedral and solid angles, edge ratio), the trip-by-trip
progress from LaGriT's output, a histogram of relative width before and
after, pictures of the thin tets before and after, and every tet left
below TOLCRUSH with the reason crush leaves it. Python 3, standard
library only.
"""
import glob
import html
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "test_lagrit_upstream"))
try:
    import draw_meshes
except ImportError:
    draw_meshes = None

E = html.escape


def picture(work, logdir, base, title):
    src = os.path.join(work, base + ".inp")
    if draw_meshes is None or not os.path.isfile(src):
        return ""
    svg = base + ".svg"
    try:
        draw_meshes.draw(src, os.path.join(logdir, svg), title, color="cell:relwidth",
                         vrange=(0.0, 0.3))
    except Exception:
        return ""
    return '<a href="%s"><img src="%s" width="360" alt="%s"></a>' % (svg, svg, E(title))


def hist_svg(before, after_list):
    """relative width histogram: before (grey) and each after (colours)"""
    W, H, bw = 420, 150, 18
    series = [("before", before, "#8c959f")] + after_list
    mx = max(max(h) for _, h, _ in series) or 1
    o = ['<svg viewBox="0 0 %d %d" width="%d" role="img" aria-label="relative width histogram">' % (W, H + 34, W)]
    n = len(series)
    for i in range(21):
        x0 = 30 + i * bw
        for j, (lab, h, col) in enumerate(series):
            v = h[i]
            hh = (H - 10) * (v / mx) ** 0.5 if v else 0     # square-root scale: small counts visible
            o.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"><title>%s: %d tets with relative width %.3f-%.3f</title></rect>'
                     % (x0 + j * (bw - 2) / n, H - hh, (bw - 2) / n, hh, col, lab, v, i * 0.025, (i + 1) * 0.025))
    for v in (0.0, 0.1, 0.2, 0.3, 0.4, 0.5):
        x = 30 + v / 0.025 * bw
        o.append('<line x1="%.1f" x2="%.1f" y1="%d" y2="%d" stroke="#d0d7de"/><text x="%.1f" y="%d" font-size="10" text-anchor="middle" fill="#57606a">%.1f</text>'
                 % (x, x, H, H + 4, x, H + 15, v))
    o.append('<text x="30" y="%d" font-size="10" fill="#57606a">relative width (square-root height scale; last bar: 0.5 and above)</text>' % (H + 30))
    o.append("</svg>")
    leg = " ".join('<span style="color:%s">&#9632;</span> %s' % (c, E(l)) for l, _, c in series)
    return "".join(o) + '<div style="font-size:12px">%s</div>' % leg


COLS = ["#0969da", "#bf8700", "#cf222e"]


def mrow(label, m, tol_list):
    c = m["carl"]
    kinds = []
    for t in tol_list:
        cv = m["crush_view"].get(str(t), {})
        s = ", ".join("%d %s" % (n, k.lower()) for k, n in cv.items() if n)
        kinds.append(s or "none")
    return ("<tr><td>%s</td><td>%d</td><td>%d</td><td>%.3f</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td>"
            "<td>%.3f</td><td>%.1f</td><td>%.1f</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td><b>%d</b></td><td>%s</td></tr>"
            % (E(label), m["vertices"], m["tets"], m["min_relwidth"], m["below"]["0.05"], m["below"]["0.1"],
               m["below"]["0.2"], m["below"]["0.3"], m["min_aspect"], m["min_dihedral"], m["max_dihedral"],
               c["SLIVER"], c["CAP"], c["NEEDLE"], c["WEDGE"], c["poor"], E(" | ".join(kinds))))


def main(work, logdir):
    runs = sorted(glob.glob(os.path.join(logdir, "ct*.json")))
    rows = [l.rstrip("\n") for l in open(os.path.join(logdir, "table.txt"))] if os.path.isfile(os.path.join(logdir, "table.txt")) else []
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>crush_thin_tets</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1350px;color:#1f2328;background:#fff}
h2{font-size:17px;margin:28px 0 6px}h3{font-size:14px;margin:14px 0 4px}p,li{font-size:14px;line-height:1.45;max-width:980px}
table{border-collapse:collapse;font-size:12px;margin:4px 0}td,th{border-bottom:1px solid #d0d7de;padding:2px 6px;text-align:right;vertical-align:top}
th{background:#f6f8fa}td:first-child,th:first-child{text-align:left}.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}
.row{display:flex;flex-wrap:wrap;gap:10px;align-items:flex-start}pre{font-size:12px;background:#f6f8fa;padding:6px;white-space:pre-wrap}
.why td{text-align:left}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">crush_thin_tets: from poor tets to better ones, and what is left</h1>
<p><b>Measures.</b> <i>Relative width</i> is crush_thin_tets' own measure: the smallest altitude divided by
sqrt(sum of squared edge lengths / 9); 1 for a regular tet, 0 for a flat one. crush works on every tet below TOLCRUSH.
<i>Carl's classes</i> are computed by LaGriT (ct_measure.mlgi, as in ex_id_tet_types): SLIVER min dihedral &le; 10&deg;,
max dihedral &ge; 170&deg; and max solid angle &le; 10&deg;; CAP max solid angle &ge; 170&deg;; NEEDLE edge ratio &le; 0.1 and
min dihedral &gt; 10&deg;; WEDGE edge ratio &le; 0.1 and min dihedral &le; 10&deg;; <b>poor</b>: any dihedral &le; 10&deg; or &ge; 170&deg;.
<i>crush's view</i> sorts each tet below TOLCRUSH by the first of crush's four cases it matches: NEEDLE (short edge),
WEDGE (a vertex near an edge), CAP (a vertex near the opposite face), SLIVER (opposite edges nearly crossing), or
UNMATCHED. <i>Trips</i>: crush repeats until a trip changes nothing (at most 10); the aspect-ratio counts are from
the quality report crush prints at the start of each trip.</p>
<p><b>Why tets are left</b> (replaying crush's own decisions on the final mesh, crushlib.Mesh.replay):
<i>CAP case switched off</i> &mdash; crush_thin_tets.f skips caps (<code>if (.false.)</code> around find_02);
<i>not 1% better</i> &mdash; every collapse crush could make would leave the worst tet nearby less than 1% better
(crush only acts when it improves the neighbourhood); <i>point types</i> &mdash; the merge would move a boundary or
interface vertex; <i>matched no case</i> &mdash; thin by altitude, but no distance is below TOLCRUSH &times; the scale
length (with the scale-length bug fixed in Oct 2026, this no longer happens in these tests).</p>
<table><tr><th>deck</th><th>result</th><th>left below TOLCRUSH (per TOLCRUSH)</th></tr>"""]
    for r in rows:
        p = r.split()
        if len(p) >= 2:
            o.append('<tr><td><a href="#%s">%s</a></td><td class="%s">%s</td><td>%s</td></tr>'
                     % (E(p[0][:4]), E(p[0]), E(p[1]), E(p[1]), E(r.split(":", 1)[1].strip() if ":" in r else "")))
    o.append("</table>")
    for path in runs:
        res = json.load(open(path))
        tag = res["tag"]
        tols = res["tols"]
        o.append('<h2 id="%s">%s <span class="%s">%s</span></h2>' % (E(tag), E(res["deck"]), E(res.get("result", "FAIL")), E(res.get("result", "FAIL"))))
        deck = os.path.join(HERE, res["deck"] + ".lgi")
        doc = []
        if os.path.isfile(deck):
            for l in open(deck):
                if l.startswith("#") and not l.startswith(("# check", "# expect", "#   ", "# " + res["deck"])):
                    doc.append(l[2:].rstrip())
                if not l.startswith("#"):
                    break
        o.append("<p>%s</p>" % E(" ".join(doc)))
        o.append("<p><b>Expected:</b> %s</p>" % E(res.get("expect") or ""))
        if res.get("why"):
            o.append('<p class="FAIL">%s</p>' % E("; ".join(res["why"])))
        if "before" not in res:
            continue
        o.append("<table><tr><th></th><th>vertices</th><th>tets</th><th>min rel. width</th><th>&lt;0.05</th><th>&lt;0.1</th><th>&lt;0.2</th><th>&lt;0.3</th>"
                 "<th>min aspect</th><th>min dih.</th><th>max dih.</th><th>Carl SLIVER</th><th>CAP</th><th>NEEDLE</th><th>WEDGE</th><th>poor</th>"
                 "<th>crush's view below TOLCRUSH %s</th></tr>" % E(" | ".join(str(t) for t in tols)))
        o.append(mrow("before", res["before"], tols))
        for t in tols:
            a = res["after"].get(str(t))
            if a:
                o.append(mrow("after, TOLCRUSH %g" % t, a, tols))
        o.append("</table>")
        # trips
        for i, t in enumerate(tols):
            a = res["after"].get(str(t))
            if not a or not a.get("trips"):
                continue
            tr = a["trips"]
            cells = []
            for k, q in enumerate(tr["trips"]):
                lab = "start" if k == 0 else ("end" if k == len(tr["trips"]) - 1 else "after trip %d" % k)
                cells.append("<td>%s<br>%s / %s<br>min %.3f<br>%s tets<br>%s merged</td>"
                             % (lab, q.get("aspect_below_0.1"), q.get("aspect_below_0.2"), q.get("min_aspect", 0),
                                q.get("elements"), q.get("merged") if k < len(tr["trips"]) - 1 else "&ndash;"))
            o.append("<h3>TOLCRUSH %g: %s trips, %s tets processed &mdash; per trip: tets with aspect ratio &lt;0.1 / &lt;0.2, min aspect, tets, vertices merged in that trip</h3><table><tr>%s</tr></table>"
                     % (t, tr.get("ntrips"), tr.get("processed"), "".join(cells)))
        # histogram and pictures
        after_h = [("after, TOLCRUSH %g" % t, res["after"][str(t)]["hist"], COLS[i % 3])
                   for i, t in enumerate(tols) if str(t) in res["after"]]
        what = ("tets below 0.3 near the planted tet" if res.get("pic_local")
                else "tets below relative width 0.3")
        pics = [picture(work, logdir, tag + "_pic_before", "%s before: %s" % (tag, what))]
        last = tols[-1]
        for t in tols:
            if str(t) in res["after"]:
                base = "%s_after_%s_pic" % (tag, ("%g" % t))
                if not os.path.isfile(os.path.join(work, base + ".inp")):
                    base = "%s_after_%s_pic" % (tag, str(t))
                if t == last or len(tols) == 1:
                    pics.append(picture(work, logdir, base, "%s after TOLCRUSH %g: %s" % (tag, t, what)))
        o.append('<div class="row"><div>%s</div>%s</div>' % (hist_svg(res["before"]["hist"], after_h), "".join(pics)))
        # survivors
        for t in tols:
            a = res["after"].get(str(t))
            if not a or not a.get("n_survivors"):
                continue
            rs = ", ".join("%d: %s" % (n, c) for c, n in sorted(a["reasons"].items(), key=lambda q: -q[1]))
            o.append("<h3>TOLCRUSH %g: %d tets left below TOLCRUSH (scale used: %s) &mdash; %s</h3>"
                     % (t, a["n_survivors"], E(a.get("scale_model", "")), E(rs)))
            o.append('<details><summary>each tet (thinnest first, up to 60)</summary><table class="why"><tr><th>tet</th><th>rel. width</th><th>crush kind</th><th>boundary vertices</th><th>why it is left</th></tr>')
            for s in a["survivors"]:
                o.append("<tr><td>%d</td><td>%.4f</td><td>%s</td><td>%d</td><td>%s</td></tr>"
                         % (s["tet"], s["relwidth"], E(s["kind"]), s["boundary_vertices"],
                            E(s["action"] or "; ".join(s["why"]))))
            o.append("</table></details>")
    o.append("</body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
