#!/usr/bin/env python3
"""check_set_logic.py WORK LOGS

test_set_logic checker. For each deck (eltset, pset), reads the table the
deck dumped (one 0/1 column r_<set> per set) and compares every member of
every set with the answer computed here from the definitions in the deck
header. Also: LaGriT finished, no ERROR, and the count LaGriT printed
("THE ELTSET u3 HAS 21 ELEMENTS") agrees. Writes LOGS/summary.html and
LOGS/table.txt; exit 0 if every check passes. Python 3, standard library.
"""
import html
import os
import sys

DECKS = {
    "sl01_eltset": {"n": 24, "what": "cells", "word": "ELTSET",
                    "a": range(1, 13), "b": range(7, 19), "c": range(1, 25, 2), "d": range(1, 25, 3)},
    "sl02_pset": {"n": 60, "what": "vertices", "word": "PSET",
                  "a": range(1, 31), "b": range(21, 46), "c": range(1, 61, 2), "d": range(1, 61, 3)},
}
OPS = [("u2", "union", "ab"), ("u3", "union", "abc"), ("u4", "union", "abcd"),
       ("i2", "inter", "ab"), ("i3", "inter", "abc"), ("i4", "inter", "abcd"),
       ("ic", "inter", "abc"),
       ("n1", "not", "a"), ("n2", "not", "ab"), ("n3", "not", "abc"), ("n4", "not", "abcd")]


def expected(D):
    full = set(range(1, D["n"] + 1))
    S = {s: set(D[s]) for s in "abcd"}
    out = dict(S)
    for name, op, args in OPS:
        sets = [S[x] for x in args]
        if op == "union":
            r = set().union(*sets)
        elif op == "inter":
            r = set.intersection(*sets)
        elif len(sets) == 1:
            r = full - sets[0]
        else:
            r = sets[0] - set().union(*sets[1:])
        out[name] = r
    return out


def read_table(path):
    with open(path) as f:
        h = f.readline().split()
        n_att = int(h[2]) or int(h[3])
        f.readline()
        names = [f.readline().split(",")[0].strip() for _ in range(n_att)]
        cols = {n: set() for n in names}
        for line in f:
            w = line.split()
            if len(w) != n_att + 1:
                continue
            i = int(w[0])
            for n, v in zip(names, w[1:]):
                if int(float(v)) == 1:
                    cols[n].add(i)
    return cols


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows, ok_all = [], True
    for deck, D in DECKS.items():
        out = open(os.path.join(work, deck + ".out"), errors="replace").read()
        printed = {}
        for l in out.splitlines():
            w = l.split()
            if len(w) >= 5 and w[0] == "THE" and w[1] == D["word"] and w[3] == "HAS":
                printed[w[2]] = int(w[4])
        fin = "LaGriT successfully completed" in out
        nerr = out.count("ERROR")
        rows.append((deck, "LaGriT finished, no ERROR", fin and nerr == 0,
                     "finished" if fin else "did not finish", "%d ERROR lines" % nerr))
        ok_all &= fin and nerr == 0
        tab = os.path.join(work, deck + ".table")
        if not os.path.isfile(tab):
            rows.append((deck, "table written", False, "no %s.table" % deck, ""))
            ok_all = False
            continue
        cols = read_table(tab)
        exp = expected(D)
        for name in list("abcd") + [o[0] for o in OPS]:
            op = next(((o, a) for n, o, a in OPS if n == name), ("set", name))
            label = "%s / %s / %s" % (name, op[0], " ".join(op[1])) if op[0] != "set" else "%s (defined by range)" % name
            got = cols.get("r_" + name)
            want = exp[name]
            good = got == want and printed.get(name) == len(want)
            ok_all &= good
            if got is None:
                detail = "no column r_%s" % name
            else:
                miss, extra = sorted(want - got), sorted(got - want)
                detail = "ok" if got == want else "missing %s; extra %s" % (miss[:12] or "-", extra[:12] or "-")
            rows.append((deck, label, good, "%d %s (expect %d)" % (len(got or ()), D["what"], len(want)),
                         "%s; LaGriT printed %s" % (detail, printed.get(name, "-"))))
    os.makedirs(logs, exist_ok=True)
    with open(os.path.join(logs, "table.txt"), "w") as t:
        for r in rows:
            t.write("%-12s %-36s %-5s %-28s %s\n" % (r[0], r[1], "PASS" if r[2] else "FAIL", r[3], r[4]))
    E = html.escape
    page = ["<!doctype html><html><head><meta charset='utf-8'><title>test_set_logic</title><style>"
            "body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;color:#1f2328;background:#fff}"
            "table{border-collapse:collapse;font-size:13px}td,th{border-bottom:1px solid #d0d7de;padding:3px 8px;"
            "text-align:left}.p{color:#1a7f37;font-weight:600}.f{color:#cf222e;font-weight:600}</style></head><body>",
            "<h1 style='font-size:20px'>test_set_logic: eltset and pset union / inter / not</h1>",
            "<p>Every member of every set is compared with the answer computed by the checker. "
            "eltset: 24 cells, a = 1&ndash;12, b = 7&ndash;18, c = odd, d = 1, 4, 7, ... "
            "pset: 60 vertices, a = 1&ndash;30, b = 21&ndash;45, c = odd, d = 1, 4, 7, ...</p><table>"
            "<tr><th>deck</th><th>check</th><th>result</th><th>size</th><th>detail</th></tr>"]
    for r in rows:
        page.append("<tr><td>%s</td><td>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td></tr>" % (
            E(r[0]), E(r[1]), "p" if r[2] else "f", "PASS" if r[2] else "FAIL", E(r[3]), E(r[4])))
    page.append("</table></body></html>")
    open(os.path.join(logs, "summary.html"), "w").write("\n".join(page) + "\n")
    npass = sum(r[2] for r in rows)
    print("test_set_logic: %d of %d checks pass" % (npass, len(rows)))
    for r in rows:
        if not r[2]:
            print("  FAIL %s %s: %s; %s" % (r[0], r[1], r[3], r[4]))
    return 0 if ok_all else 1


if __name__ == "__main__":
    sys.exit(main())
