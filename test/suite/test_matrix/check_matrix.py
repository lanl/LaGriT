"""check_matrix.py WORK LOGS

test_matrix: judges each case of cases.txt (run in WORK/NAME by
run_matrix_tests.sh). It reads the matrix file NAME_matrix.vtu (and
NAME_matrix.vti with matrix_bins) with test_paraview's reader, and
checks every point against the .stor and the mesh, worked out here.
Draws each matrix, writes summary.html in this suite's folder.
Python 3 standard library. Exit 1 on any FAIL.

Checks:
  exit       exit 0; the matrix file exists (the image only with
             matrix_bins)
  entries    one point per .stor entry of the selected rows (both
             halves and the diagonal), at (j, -i, 0), each row in
             increasing column order
  values     coefficient = A/d of the .stor (its value is -A/d),
             distance = the edge length from the mesh, area = A/d * d
             (1e-12 of the largest); the diagonal: coefficient 0,
             volume = the .stor volume
  mirror     (i, j) and (j, i) hold the same values
  class      0 diagonal, 1 boundary, 2 interior, 3 interface, worked
             out here from the mesh's faces (tets) or edges (triangles)
  flags      negative = A/d < 0; zero_area = |A| <= 1e-12 d^2 (tets),
             1e-12 d (triangles), and present only with zero_area keep
  material   material_i, material_j = imt1 of the mesh file
  alpha      alpha_min = the smallest alpha of the cells around the
             edge, from NAME_mesh.vtu (1 with Voronoi)
  angle      tets: the angle between the edge and the sum of its dual
             faces' vector areas in NAME_faces.vtu (1e-6 degrees);
             triangles with Voronoi: 0
  shape      the report's bandwidth and profile equal those of the
             whole .stor
  image      matrix_bins N (N at most the number of rows): each pixel's
             count, negative count and smallest coefficient equal
             those worked out here from the points
"""

import math
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'test_paraview'))
sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
from vtu_read import read_vtu, _arrays  # noqa: E402
from exact_check import read_avs  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, ramp  # noqa: E402
from check_voronoi_gold import read_stor  # noqa: E402

ZTOL = 1e-12


def read_imt(path, nn, nc):
    """imt1 of each vertex (0 if the file has none)."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    k = 1 + nn + nc
    if len(lines) <= k:
        return [0] * nn
    ncomp = [int(v) for v in lines[k].split()[1:]]
    names = [lines[k + 1 + a].split(',')[0].strip().lower()
             for a in range(len(ncomp))]
    if 'imt1' not in names and 'imt' not in names:
        return [0] * nn
    col = 1 + sum(ncomp[:names.index('imt1' if 'imt1' in names
                                     else 'imt')])
    vals = ' '.join(lines[k + 1 + len(ncomp):]).split()
    per = 1 + sum(ncomp)
    return [int(float(vals[i * per + col])) for i in range(nn)]


def read_vti(path):
    data = open(path, 'rb').read()
    k = data.find(b'<AppendedData')
    raw = b''
    if k >= 0:
        raw = data[data.index(b'_', k) + 1:]
        text = data[:k].decode()
    else:
        text = data.decode()
    ext = [int(v) for v in re.search(r'WholeExtent="([^"]+)"',
                                     text).group(1).split()]
    org = [float(v) for v in re.search(r'Origin="([^"]+)"',
                                       text).group(1).split()]
    spc = [float(v) for v in re.search(r'Spacing="([^"]+)"',
                                       text).group(1).split()]
    arr = {at['Name']: vals for at, vals, _ in _arrays(text, raw)}
    return ext, org, spc, arr


def edge_classes(m):
    """(a, b) -> 1 boundary, 2 interior, 3 interface (a < b, 0-based),
    as del_vor defines them (an interface edge wins over boundary)."""
    cls = {}
    if len(m.cells[0]) == 4:
        faces = {}
        for c, t in enumerate(m.cells):
            for f in ((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3)):
                key = tuple(sorted(t[q] for q in f))
                faces.setdefault(key, []).append(m.clr[c])
            for a in range(4):
                for b in range(a + 1, 4):
                    cls[tuple(sorted((t[a], t[b])))] = 2
        bnd, itf = set(), set()
        for key, cl in faces.items():
            es = [tuple(sorted((key[a], key[b]))) for a, b in
                  ((0, 1), (0, 2), (1, 2))]
            if len(cl) == 1:
                bnd.update(es)
            elif cl[0] != cl[1]:
                itf.update(es)
        for e in bnd:
            cls[e] = 1
        for e in itf:
            cls[e] = 3
        return cls
    groups = {}
    for c, t in enumerate(m.cells):
        for a, b in ((0, 1), (0, 2), (1, 2)):
            e = tuple(sorted((t[a], t[b])))
            groups.setdefault(e, []).append(m.clr[c])
    for e, cl in groups.items():
        cnt = {}
        for v in cl:
            cnt[v] = cnt.get(v, 0) + 1
        if len(cl) == 2 and len(cnt) == 2:
            cls[e] = 3
        elif 1 in cnt.values():
            cls[e] = 1
        else:
            cls[e] = 2
    return cls


def stor_rows(s):
    """row i (0-based) -> [(j, value)], in the .stor's order."""
    neq = s['neq']
    rows = []
    for i in range(neq):
        r = []
        for q in range(s['rp'][i] - neq - 1, s['rp'][i + 1] - neq - 1):
            k = s['ptr'][q]
            r.append((s['col'][q] - 1, s['val'][k - 1] if k > 0 else 0.0))
        rows.append(r)
    return rows


def selected(m, args, vol_rows):
    nn = len(m.xyz)
    if '-paraview_subset box' in args:
        b = [float(x) for x in args.split('box')[1].split()[:6]]
        return [b[0] <= p[0] <= b[3] and b[1] <= p[1] <= b[4] and
                b[2] <= p[2] <= b[5] for p in m.xyz]
    return [True] * nn


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows_out = []
    for name, mesh, args, about in read_cases(
            os.path.join(HERE, 'cases.txt'), 4):
        d = os.path.join(work, name)
        mname = os.path.basename(mesh)
        cmd = ('del_vor -avs %s -stor %s.stor -matrix %s_matrix.vtu '
               '-paraview %s -paraview_faces on -report %s.report %s'
               % (mname, name, name, name, name, args))
        checks = []

        def ok(c, v, good):
            checks.append((c, v, 'PASS' if good else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if good else 'FAIL'))

        rc = int(open(os.path.join(d, 'run.rc')).read())
        ok('exit', '%d (expected 0)' % rc, rc == 0)
        if rc != 0:
            rows_out.append((name, about, cmd, checks, None))
            continue
        nbins = 0
        if '-matrix_bins' in args:
            nbins = int(args.split('-matrix_bins')[1].split()[0])
        fx = os.path.join(d, name + '_matrix.vtu')
        fi = os.path.join(d, name + '_matrix.vti')
        ok('exit', 'matrix file%s' % (' and image' if nbins else ''),
           os.path.exists(fx) and os.path.exists(fi) == (nbins > 0))
        m = read_avs(os.path.join(d, mname))
        nn = len(m.xyz)
        tets = len(m.cells[0]) == 4
        imt = read_imt(os.path.join(d, mname), nn, len(m.cells))
        s = read_stor(os.path.join(d, name + '.stor'))
        srow = stor_rows(s)
        sel = selected(m, args, srow)
        keep = '-zero_area keep' in args
        g = read_vtu(fx)
        P = g.pdata
        npt = len(g.points)

        # entries and positions
        want = []
        for i in range(nn):
            if sel[i]:
                want += [(i, j) for j, _ in srow[i]]
        got = [(P['i'][p] - 1, P['j'][p] - 1) for p in range(npt)]
        pos_ok = all(g.points[p] == (float(P['j'][p]), -float(P['i'][p]),
                                     0.0) for p in range(npt))
        cells_ok = (len(g.cells) == npt and all(t == 1 for t in g.types)
                    and all(g.cells[p] == [p] for p in range(npt)))
        ok('entries', '%d points (%d selected rows, %d entries in the '
           '.stor)' % (npt, sum(sel), sum(len(r) for r in srow)),
           got == want and pos_ok and cells_ok)

        # values
        sval = {}
        for i in range(nn):
            for j, v in srow[i]:
                sval[(i, j)] = v
        vmax = max(abs(v) for v in s['val']) or 1.0
        lmax = max(math.dist(m.xyz[0], q) for q in m.xyz)
        e_c = e_d = e_a = e_v = 0.0
        diag_ok = True
        for p in range(npt):
            i, j = got[p]
            if i == j:
                e_v = max(e_v, abs(P['volume'][p] - s['vol'][i]))
                diag_ok &= (P['coefficient'][p] == 0.0 and
                            P['class'][p] == 0 and P['area'][p] == 0.0)
                continue
            dd = math.dist(m.xyz[i], m.xyz[j])
            e_c = max(e_c, abs(P['coefficient'][p] + sval[(i, j)]))
            e_d = max(e_d, abs(P['distance'][p] - dd))
            e_a = max(e_a, abs(P['area'][p] - P['coefficient'][p] * dd))
            diag_ok &= P['volume'][p] == 0.0
        vvmax = max(abs(v) for v in s['vol'])
        ok('values', 'coefficient %.1e, distance %.1e, area %.1e, '
           'volume %.1e of the largest' % (e_c / vmax, e_d / lmax,
                                           e_a / (vmax * lmax),
                                           e_v / vvmax),
           e_c <= 1e-12 * vmax and e_d <= 1e-14 * lmax and
           e_a <= 1e-12 * vmax * lmax and e_v <= 1e-12 * vvmax and
           diag_ok)

        # mirror
        at = {got[p]: p for p in range(npt)}
        names = [k for k in P if k not in ('i', 'j', 'material_i',
                                           'material_j')]
        bad = 0
        for (i, j), p in at.items():
            q = at.get((j, i))
            if q is None:
                continue
            if any(P[k][p] != P[k][q] for k in names):
                bad += 1
        ok('mirror', '%d pairs differ' % bad, bad == 0)

        # class, flags, material
        cls = edge_classes(m)
        bc = bn = bz = bm = 0
        nneg = nzero = 0
        cnt = {0: 0, 1: 0, 2: 0, 3: 0}
        for p in range(npt):
            i, j = got[p]
            c = P['class'][p]
            cnt[c] = cnt.get(c, 0) + 1
            if i == j:
                bm += (P['material_i'][p] != imt[i] or
                       P['material_j'][p] != imt[i])
                continue
            bc += c != cls[tuple(sorted((i, j)))]
            dd = math.dist(m.xyz[i], m.xyz[j])
            a = -sval[(i, j)] * dd
            z = abs(a) <= ZTOL * (dd * dd if tets else dd)
            bz += P['zero_area'][p] != (1 if z else 0)
            bn += P['negative'][p] != (1 if (a < 0 and not z) else 0)
            nneg += P['negative'][p]
            nzero += P['zero_area'][p]
            bm += (P['material_i'][p] != imt[i] or
                   P['material_j'][p] != imt[j])
        ok('class', 'diagonal %d, boundary %d, interior %d, interface %d'
           % (cnt[0], cnt[1], cnt[2], cnt[3]), bc == 0)
        ok('flags', '%d negative, %d zero_area points%s'
           % (nneg, nzero, '' if keep else ' (none without keep)'),
           bn == 0 and bz == 0 and (keep or nzero == 0))
        ok('material', 'materials %s' % sorted(set(imt)), bm == 0)

        # alpha_min from the mesh file
        gm = read_vtu(os.path.join(d, name + '_mesh.vtu'))
        vid = gm.pdata['vertex']
        amin_e, amin_v = {}, {}
        for c, conn in enumerate(gm.cells):
            al = gm.cdata['alpha'][c]
            vs = [vid[q] - 1 for q in conn]
            for a in range(len(vs)):
                amin_v[vs[a]] = min(amin_v.get(vs[a], 2.0), al)
                for b in range(a + 1, len(vs)):
                    e = tuple(sorted((vs[a], vs[b])))
                    amin_e[e] = min(amin_e.get(e, 2.0), al)
        ba = 0
        lo = 1.0
        for p in range(npt):
            i, j = got[p]
            w = amin_v[i] if i == j else amin_e[tuple(sorted((i, j)))]
            ba += abs(P['alpha_min'][p] - min(w, 1.0)) > 1e-15
            lo = min(lo, P['alpha_min'][p])
        ok('alpha', 'smallest alpha_min %.3g; %d differ from the mesh '
           'file' % (lo, ba), ba == 0)

        # angle
        if tets:
            gf = read_vtu(os.path.join(d, name + '_faces.vtu'))
            vsum = {}
            for c, conn in enumerate(gf.cells):
                i = gf.cdata['vertex_i'][c] - 1
                j = gf.cdata['vertex_j'][c] - 1
                q = [gf.points[t] for t in conn]
                va = [0.0, 0.0, 0.0]
                for a in range(1, len(q) - 1):
                    u = [q[a][k] - q[0][k] for k in range(3)]
                    w = [q[a + 1][k] - q[0][k] for k in range(3)]
                    cr = (u[1] * w[2] - u[2] * w[1],
                          u[2] * w[0] - u[0] * w[2],
                          u[0] * w[1] - u[1] * w[0])
                    va = [va[k] + 0.5 * cr[k] for k in range(3)]
                key = (min(i, j), max(i, j))
                sgn = 1.0 if i < j else -1.0
                t = vsum.setdefault(key, [0.0, 0.0, 0.0])
                for k in range(3):
                    t[k] += sgn * va[k]
            emax = 0.0
            amax = 0.0
            nskip = 0
            big = max(math.sqrt(sum(x * x for x in v))
                      for v in vsum.values())
            for p in range(npt):
                i, j = got[p]
                if i == j or P['zero_area'][p]:
                    continue
                a, b = min(i, j), max(i, j)
                va = vsum.get((a, b))
                if va is None:
                    continue
                e = [m.xyz[b][k] - m.xyz[a][k] for k in range(3)]
                na = math.sqrt(sum(x * x for x in va))
                ne = math.sqrt(sum(x * x for x in e))
                if na < 1e-6 * big:
                    # a tie's face: nearly no area, its direction is
                    # round-off (as test_paraview's planar check)
                    nskip += 1
                    continue
                cr = (va[1] * e[2] - va[2] * e[1],
                      va[2] * e[0] - va[0] * e[2],
                      va[0] * e[1] - va[1] * e[0])
                ang = math.degrees(math.atan2(
                    math.sqrt(sum(x * x for x in cr)),
                    sum(va[k] * e[k] for k in range(3))))
                emax = max(emax, abs(ang - P['angle'][p]))
                amax = max(amax, P['angle'][p])
            ok('angle', 'largest %.3g degrees; against the faces file '
               '%.1e (%d points of faces with nearly no area left out)'
               % (amax, emax, nskip), emax <= 1e-8)
        else:
            amax = max(P['angle'])
            ok('angle', 'largest %.1e degrees (Voronoi: 0)' % amax,
               amax <= 1e-9)

        # bandwidth and profile of the whole .stor
        rep = open(os.path.join(d, 'run.out')).read()
        mt = re.search(r'matrix entries (\d+), bandwidth (\d+), '
                       r'profile (\d+)', rep)
        bw = max(abs(i - j) for i in range(nn) for j, _ in srow[i])
        prof = sum(i - min(j for j, _ in srow[i]) for i in range(nn))
        nent = sum(len(r) for r in srow)
        ok('shape', 'entries %d, bandwidth %d, profile %d' % (nent, bw,
                                                              prof),
           mt is not None and [int(x) for x in mt.groups()] ==
           [nent, bw, prof])

        # the image
        img = None
        if nbins:
            ext, org, spc, arr = read_vti(fi)
            nb = min(nbins, nn)
            cnt = [0] * (nb * nb)
            neg = [0] * (nb * nb)
            cmn = [None] * (nb * nb)
            for p in range(npt):
                i, j = got[p]
                by = nb - 1 - (i * nb) // nn
                bx = (j * nb) // nn
                c = bx + nb * by
                cnt[c] += 1
                neg[c] += P['negative'][p]
                if i != j and (cmn[c] is None or
                               P['coefficient'][p] < cmn[c]):
                    cmn[c] = P['coefficient'][p]
            cmn = [0.0 if v is None else v for v in cmn]
            geo = (ext == [0, nb, 0, nb, 0, 0] and
                   abs(org[0] - 0.5) < 1e-12 and
                   abs(org[1] + nn + 0.5) < 1e-9 and
                   abs(spc[0] - nn / nb) < 1e-12 and
                   abs(spc[1] - nn / nb) < 1e-12)
            ok('image', '%d x %d pixels, %d entries, %d negative'
               % (nb, nb, sum(arr['count']), sum(arr['negative'])),
               geo and arr['count'] == cnt and arr['negative'] == neg
               and arr['min_coefficient'] == cmn)
            img = (nb, arr)

        pic = name + '.svg'
        picture(os.path.join(logs, pic), name, nn, got, P, img,
                '%s: %s' % (mname, args or 'defaults'))
        rows_out.append((name, about, cmd, checks, pic))

    what = ('runs del_vor with -matrix (and -matrix_bins), reads the '
            'matrix file back with test_paraview\'s reader, and checks '
            'every point against the .stor and the mesh: one point per '
            '.stor entry at (j, -i), both halves and the diagonal, with '
            'its coefficient, distance, area, class, flags, materials, '
            'alpha and angle; and the image\'s pixels against the '
            'points. The pictures are the matrices, row 1 at the top. '
            'Open NAME_matrix.vtu in ParaView (00README says how).')
    nfail = write_case_page(HERE, logs, 'test_matrix', what, rows_out)
    sys.exit(1 if nfail else 0)


COLORS = {0: '#111111', 1: '#2b6cb0', 2: '#9aa5b1', 3: '#1a7f37'}


def picture(path, name, nn, got, P, img, title):
    """The matrix (row 1 at the top): diagonal black, boundary blue,
    interior gray, interface green, negative red, zero-area hollow;
    beside it the image (count, darker = more)."""
    W = 900 if img else 470
    H = 520
    s = Svg(W, H)
    s.text(20, 26, title, size=14, bold=True)
    x0, y0, w = 30, 50, 420
    s.rect(x0, y0, w, w, fill='#fff', stroke='#888')
    sc = w / nn
    r = max(0.6, min(3.0, sc * 0.45))
    order = sorted(range(len(got)), key=lambda p: (P['negative'][p],
                                                   P['class'][p] == 0))
    for p in order:
        i, j = got[p]
        x = x0 + (j + 0.5) * sc
        y = y0 + (i + 0.5) * sc
        if P['negative'][p]:
            col = '#d62728'
        else:
            col = COLORS.get(P['class'][p], '#999')
        if P['zero_area'][p]:
            s.circle(x, y, r, fill='none', stroke='#e67e22', width=0.8)
        else:
            s.rect(x - r, y - r, 2 * r, 2 * r, fill=col)
    s.text(x0, y0 + w + 22, '%d rows, %d points; black diagonal, blue '
           'boundary, gray interior, green interface,' % (nn, len(got)),
           size=11)
    s.text(x0, y0 + w + 38, 'red negative, orange ring zero area',
           size=11)
    if img:
        nb, arr = img
        bx0 = 480
        s.rect(bx0, y0, w, w, fill='#fff', stroke='#888')
        cm = max(arr['count']) or 1
        q = w / nb
        for by in range(nb):
            for bx in range(nb):
                c = arr['count'][bx + nb * by]
                if c == 0:
                    continue
                t = math.log(1 + c) / math.log(1 + cm)
                fill = ramp(t)
                if arr['negative'][bx + nb * by]:
                    fill = '#d62728'
                s.rect(bx0 + bx * q, y0 + (nb - 1 - by) * q, q, q,
                       fill=fill)
        s.text(bx0, y0 + w + 22, 'matrix_bins image: %d x %d pixels, '
               'color = entries (log), red = a negative' % (nb, nb),
               size=11)
    s.save(path)


if __name__ == '__main__':
    main()
