#!/usr/bin/env python3
"""dfn_tets_suite.py: run dfn_tets.py on every case of a suite, one after
another, and join the results into one page.

QUICK START

  python3 dfn_tets_suite.py ~/Downloads/dfn_mesh_suite --lagrit ~/lagrit/build/lagrit
  open ~/Downloads/dfn_mesh_suite_dfn_tets/summary.html

A case is a folder of the suite holding full_mesh.inp and metadata.json.
Each one runs as its own dfn_tets.py, in its own process, into
OUT/<case>/ (so a crash or a memory blow-up stops only that case), with
the same settings.

SETTINGS

  Any dfn_tets.py setting may be given here, and goes to every case:
    python3 dfn_tets_suite.py SUITE --lagrit ... --c 0.6 --slope 0.15
    python3 dfn_tets_suite.py SUITE --settings my.ini
  (python3 dfn_tets.py --write-settings my.ini writes a settings file
  with every setting explained.) --dfn, --json, --box, --out and --name
  are set here for each case.

  --only CASE ...   just these cases
  --sub-box SIDE    quick runs: mesh only a cube of this side in each
                    case's box, where the DFN is densest (or at 0 with
                    --sub-center origin). The whole DFN is still used, so
                    the h field in the cube is the full case's. A case
                    whose box is no bigger than SIDE runs whole. With 2.5,
                    the eight dfnWorks cases take about 3-4 minutes.
  --plan-only       size every case (lattice, memory, time), run nothing
  --sweep SETTING=V1,V2,...
                    run the suite once for each value of one dfn_tets.py
                    setting, into OUT/SETTING_V/, and write OUT/sweep.html
                    comparing them case by case (default OUT:
                    SUITE_sweep_SETTING). Example:
                      --sweep hmax_factor=10,20,50,100 --min-cells 2
  --out DIR         default: SUITE + "_dfn_tets" (+ "_quick_dfn_tets"
                    with --sub-box)

A case whose predicted memory is over max_memory_gb (default: 60% of this
computer's memory) is not run; the page says so and why. Raise it with
--max-memory-gb, at your own risk.

OUTPUT

  OUT/summary.html   every case: table, then each case's figures
  OUT/summary.json   the table's numbers
  OUT/<case>/        dfn_tets.py's output for that case (mesh: tets.inp)
"""
import argparse
import json
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import dfn_tets  # noqa: E402  (the page code is shared)

SET_PER_CASE = ("--dfn", "--json", "--box", "--out", "--name", "--write-settings")


def densest_center(dfn, side, Ls):
    """the centre of the side-cube, inside the box centred on 0, holding
    the most DFN vertices (vertices binned at side / 3, 3 x 3 x 3 blocks)"""
    cell = side / 3.0
    n = [max(3, int(L / cell)) for L in Ls]
    lo = [-L / 2.0 for L in Ls]
    cnt = {}
    with open(dfn) as f:
        nn = int(f.readline().split()[0])
        for _ in range(nn):
            w = f.readline().split()
            key = tuple(min(n[k] - 1, max(0, int((float(w[k + 1]) - lo[k]) / cell))) for k in range(3))
            cnt[key] = cnt.get(key, 0) + 1
    best, bc = -1, (0.0, 0.0, 0.0)
    for i in range(n[0] - 2):
        for j in range(n[1] - 2):
            for k in range(n[2] - 2):
                v = sum(cnt.get((i + a, j + b, k + c), 0) for a in range(3) for b in range(3) for c in range(3))
                cen = tuple(lo[m] + (q + 1.5) * cell for m, q in enumerate((i, j, k)))
                if v > best or (v == best and sum(x * x for x in cen) < sum(x * x for x in bc)):
                    best, bc = v, cen
    # keep the cube inside the box
    return tuple(min(Ls[m] / 2 - side / 2, max(-Ls[m] / 2 + side / 2, bc[m])) for m in range(3))


def main():
    ap = argparse.ArgumentParser(
        prog="dfn_tets_suite.py", formatter_class=argparse.RawDescriptionHelpFormatter,
        description="Run dfn_tets.py on every case of a suite; one page for all. "
                    "Run with no arguments for the quick start.",
        epilog="Every other option goes to dfn_tets.py for each case (python3 dfn_tets.py --help lists them).")
    ap.add_argument("suite", help="folder of cases (each with full_mesh.inp and metadata.json)")
    ap.add_argument("--out", help="output folder (default: SUITE_dfn_tets)")
    ap.add_argument("--only", nargs="+", metavar="CASE", help="just these cases")
    ap.add_argument("--sub-box", type=float, metavar="SIDE",
                    help="quick runs: mesh a cube of this side in each case's box (the whole DFN still sets h)")
    ap.add_argument("--sub-center", choices=("densest", "origin"), default="densest",
                    help="where the sub-box goes (default: where the DFN is densest)")
    ap.add_argument("--sweep", metavar="SETTING=V1,V2,...",
                    help="run the suite for each value of one setting; OUT/sweep.html compares them")
    if len(sys.argv) == 1:
        print(__doc__)
        return 0
    a, passthru = ap.parse_known_args()
    for p in passthru:
        if p.split("=")[0] in SET_PER_CASE:
            sys.exit("%s is set for each case by the suite; leave it out" % p)
    suite = os.path.abspath(os.path.expanduser(a.suite))
    if not os.path.isdir(suite):
        sys.exit("no folder %s" % suite)
    out = os.path.abspath(os.path.expanduser(a.out)) if a.out else \
        suite.rstrip("/") + ("_quick_dfn_tets" if a.sub_box else "_dfn_tets")
    if out == suite or out.startswith(suite + os.sep):
        sys.exit("--out must be outside the suite folder (nothing is written into the suite)")
    cases = sorted(d for d in os.listdir(suite)
                   if os.path.isfile(os.path.join(suite, d, "full_mesh.inp"))
                   and os.path.isfile(os.path.join(suite, d, "metadata.json")))
    if a.only:
        missing = [c for c in a.only if c not in cases]
        if missing:
            sys.exit("no such case: %s (cases: %s)" % (" ".join(missing), " ".join(cases)))
        cases = [c for c in cases if c in a.only]
    if not cases:
        sys.exit("no <case>/full_mesh.inp with metadata.json under %s" % suite)
    if a.sweep:
        name, _, vals = a.sweep.partition("=")
        name = name.strip().replace("-", "_")
        known = {n for n, d, t, h, g in dfn_tets.SETTINGS}
        if name not in known or name in ("dfn", "json", "box", "out", "name"):
            sys.exit("--sweep: %r is not a setting that can be swept (python3 dfn_tets.py --help lists them)" % name)
        flag = "--" + name.replace("_", "-")
        if any(p.split("=")[0] == flag for p in passthru):
            sys.exit("--sweep %s: leave %s out of the other options" % (name, flag))
        values = [v.strip() for v in vals.split(",") if v.strip()]
        if not values:
            sys.exit("--sweep needs values: --sweep %s=V1,V2,..." % name)
        if not a.out:
            out = suite.rstrip("/") + "_sweep_" + name + ("_quick" if a.sub_box else "")
        runs = []
        for v in values:
            print("=" * 72 + "\nsweep %s = %s\n" % (name, v) + "=" * 72, flush=True)
            sub = os.path.join(out, "%s_%s" % (name, v))
            results = run_suite(a, suite, sub, cases, passthru + [flag, v])
            runs.append((v, sub, results))
        open(os.path.join(out, "sweep.html"), "w").write(sweep_html(name, runs, suite, a, passthru))
        print("sweep page: %s" % os.path.join(out, "sweep.html"))
        return 0
    run_suite(a, suite, out, cases, passthru)
    return 0


def run_suite(a, suite, out, cases, passthru):
    os.makedirs(out, exist_ok=True)
    print("suite  %s (%d cases)\noutput %s\noptions for every case: %s\n"
          % (suite, len(cases), out, " ".join(passthru) or "(defaults)"), flush=True)
    results, sections, t_all = [], [], time.time()
    for k, c in enumerate(cases, 1):
        print("[%d/%d] " % (k, len(cases)), end="", flush=True)
        od = os.path.join(out, c)
        dfn, mj = os.path.join(suite, c, "full_mesh.inp"), os.path.join(suite, c, "metadata.json")
        where = ["--json", mj]
        Ls = [float(x) for x in json.load(open(mj))["domain_size"]]
        if a.sub_box and max(Ls) > a.sub_box:
            cen = densest_center(dfn, a.sub_box, Ls) if a.sub_center == "densest" else (0.0, 0.0, 0.0)
            h = a.sub_box / 2.0
            where = ["--box"] + ["%.6g" % (cen[m] - h) for m in range(3)] + ["%.6g" % (cen[m] + h) for m in range(3)]
            print("sub-box: side %g at (%.3g, %.3g, %.3g) of the %s box" % (
                a.sub_box, cen[0], cen[1], cen[2], " x ".join("%g" % L for L in Ls)), flush=True)
        cmd = [sys.executable, os.path.join(HERE, "dfn_tets.py"), "--dfn", dfn] + where + \
              ["--out", od, "--name", c] + passthru
        rc = subprocess.call(cmd)
        rj, sh = os.path.join(od, "result.json"), os.path.join(od, "section.html")
        if os.path.isfile(rj) and os.path.getmtime(rj) >= t_all:
            results.append(json.load(open(rj)))
            sections.append(open(sh).read() if os.path.isfile(sh) else "")
        else:
            print("  (no result from %s: exit %d; see the messages above)" % (c, rc), flush=True)
            if rc in (2,):          # argparse: a bad option, same for every case
                sys.exit(rc)
        print(flush=True)
    title = "DFN-driven Poisson disk tets: %s" % os.path.basename(suite)
    intro = dfn_tets.INTRO + "<p>Suite <code>%s</code>, %d cases, %s in all.%s</p>" % (
        dfn_tets.E(suite), len(cases), dfn_tets.hms(time.time() - t_all),
        (" <b>Quick run</b>: each case meshes only a cube of side %g (%s), with the whole DFN setting h; "
         "compare its numbers with other quick runs." % (a.sub_box, "where the DFN is densest"
                                                         if a.sub_center == "densest" else "at 0"))
        if a.sub_box else "")
    open(os.path.join(out, "summary.html"), "w").write(dfn_tets.page_html(title, intro, results, sections))
    json.dump([{k: r.get(k) for k in ("name", "status", "notes", "time", "peak", "plan", "res", "settings")}
               for r in results], open(os.path.join(out, "summary.json"), "w"), indent=1)
    done = sum(r["status"] == "done" for r in results)
    print("%d of %d cases done, %s. Page: %s" % (done, len(cases), dfn_tets.hms(time.time() - t_all),
                                               os.path.join(out, "summary.html")))
    return results



def sweep_html(name, runs, suite, a, passthru):
    E, hms, mem = dfn_tets.E, dfn_tets.hms, dfn_tets.mem
    o = ["<p>Suite <code>%s</code>%s; options for every run: <code>%s</code>. Each value's full page: %s.</p>" % (
        E(suite), (", quick runs in a cube of side %g" % a.sub_box) if a.sub_box else "", E(" ".join(passthru) or "defaults"),
        ", ".join("<a href='%s_%s/summary.html'>%s = %s</a>" % (E(name), E(v), E(name), E(v)) for v, sub, res in runs))]
    cases = []
    for v, sub, res in runs:
        for R in res:
            if R["name"] not in cases:
                cases.append(R["name"])
    head = ("<tr><th>%s</th><th>H_MAX</th><th>H_MAX set by</th><th>tet vertices at H_MAX</th>"
            "<th>farthest vertex from the DFN</th><th>tet vertices</th><th>tets</th>"
            "<th>near-DFN ratio: median (5&ndash;95%%)</th><th>tet edge / h asked: median</th>"
            "<th>angles after improve_tets</th><th>LaGriT time</th><th>LaGriT peak memory</th></tr>" % E(name))
    for c in cases:
        o.append("<h2>%s</h2><div class='scroll'><table>%s" % (E(c), head))
        for v, sub, res in runs:
            R = next((r for r in res if r["name"] == c), None)
            if R is None:
                continue
            P, X = R["plan"], R.get("res") or {}
            if not X:
                o.append("<tr><td>%s</td><td>%.4g</td><td>%s</td><td colspan='9' class='bad'>%s</td></tr>" % (
                    E(v), P["hmax"], E(P.get("hmax_by", "")), E("; ".join(R["notes"]) or R["status"])))
                continue
            o.append("<tr><td><a href='%s_%s/summary.html#%s'>%s</a></td><td>%.4g</td><td>%s</td><td>%.1f%%</td>"
                     "<td>%.3g</td><td>%s</td><td>%s</td><td>%.2f (%.2f&ndash;%.2f)</td><td>%.2f</td>"
                     "<td>%.1f&ndash;%.1f&deg;</td><td>%s</td><td>%s</td></tr>" % (
                         E(name), E(v), E(c), E(v), P["hmax"], E(P.get("hmax_by", "")), 100 * X.get("frac_hmax", 0),
                         X.get("d_far", 0), "{:,}".format(X["nv"]), "{:,}".format(X["ntet"]),
                         X.get("ratio_median", 0), X.get("ratio_p05", 0), X.get("ratio_p95", 0),
                         X.get("eh_median", 0), X["min_after"], X["max_after"],
                         hms(R["time"]) if R.get("time") else "-", mem(R["peak"]) if R.get("peak") else "-"))
        o.append("</table></div>")
    title = "Sweep of %s: %s" % (name, os.path.basename(suite))
    return ("<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, "
            "initial-scale=1'><title>%s</title>%s</head><body><h1>%s</h1>%s</body></html>\n"
            % (E(title), dfn_tets.CSS, E(title), "\n".join(o)))

if __name__ == "__main__":
    sys.exit(main())
