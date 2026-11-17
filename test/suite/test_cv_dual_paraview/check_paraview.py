"""check_paraview.py WORK LOGS

test_paraview: judges each case of cases.txt (run in WORK/NAME by
run_paraview_tests.sh), reading the ParaView files back with
vtu_read.py (this suite's own reader, written from VTK's file format),
draws each tessellation, writes summary.html in this suite's folder.
Python 3 standard library. Exit 1 on any FAIL.

Checks:
  exit       exit 0; the .pvd lists the files written, and they exist
  select     the vertices that got a cell are exactly the subset asked
             for, worked out here from the mesh (all, flagged,
             material N, box)
  mesh       NAME_mesh.vtu: the input cells that touch those vertices;
             each vertex's volume equals the .stor
  2D, networks (polygons):
    area     each polygon's area, from its points, equals its share of
             the vertex's volume; a vertex's polygons add up to its
             .stor volume (1e-12 of the largest)
    per      networks: a vertex has one polygon per fracture it is in
  3D (polyhedra):
    closed   every face edge is matched once in the opposite direction
    volume   the volume from the faces (divergence theorem) equals the
             .stor volume (1e-12 of the largest), so the faces point
             outward
    planar   cv voronoi: every face with at least 1e-6 of the largest
             face's area is planar (within 1e-12 of the mesh size; a
             face of a co-spherical tie has no area and no plane);
             with the faces file, every point of the dual face of ij
             lies on the bisector plane of i and j (ties included);
             other policies: faces are the kernel's triangles (3
             points) or boundary pieces (4)
    faces    NAME_faces.vtu: each edge's dual face area along the edge
             equals A(ij) of the .stor (1e-11 of the largest)
    layout   poi3_legacy holds the same polyhedra as poi3_vor
  spikes     no polygon or face outline turns straight back on itself
             (out and back along one line): a midpoint or face point
             between two circumcenters is on their line but not
             always between them (an obtuse angle), so it is not
             drawn as a corner (Carl, Nov 11: "edge segments that are
             not part of the polygon boundary")
  negative   NAME_negative.vtu holds exactly the negative coefficients
             of the .stor at selected vertices (and is absent when
             there are none)
"""

import math
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
from vtu_read import (read_vtu, polyhedron_volume, closed,  # noqa: E402
                      planarity, spikes, sub, cross, dot)
from exact_check import read_avs  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, Projector, ramp  # noqa: E402
from check_voronoi_gold import read_stor, stor_pairs  # noqa: E402

ZTOL = 1e-12


def vec_area(pts, ids):
    """Vector area of a polygon (fanned from its first point)."""
    p0 = pts[ids[0]]
    s = (0.0, 0.0, 0.0)
    for a in range(1, len(ids) - 1):
        c = cross(sub(pts[ids[a]], p0), sub(pts[ids[a + 1]], p0))
        s = (s[0] + c[0] / 2, s[1] + c[1] / 2, s[2] + c[2] / 2)
    return s


def stor_info(path):
    st = read_stor(path)
    c = stor_pairs(st)
    return st['vol'], {(i, j): -v for (i, j), v in c.items() if i < j}


def picture(path, title, m, g, sel, caption):
    """2D and networks: the polygons, each in its own color, over the
    mesh; tets: the outer pieces of the control volumes (the faces
    that hold the cell's own vertex), each cell in its own color."""
    W, H = 640, 600
    s = Svg(W, H)
    s.text(16, 22, title, size=13, bold=True)
    view = 'plan' if all(abs(p[2]) < 1e-30 for p in m.xyz) else 'iso'
    pr = Projector(m.xyz, 20, 40, 600, 480, view=view)
    for c in m.cells:
        if m.nen == 3:
            s.polygon([pr(m.xyz[v]) for v in c], fill='none',
                      stroke='#bbbbbb', width=0.3)
    for c in range(len(g.cells)):
        col = ramp(((g.cdata['vertex'][c] * 0.6180339887) % 1.0))
        if g.faces[c] is None:
            s.polygon([pr(g.points[q]) for q in g.cells[c]], fill=col,
                      stroke='#222', width=0.5, opacity=0.75)
        else:
            vpt = None
            vtx = m.xyz[g.cdata['vertex'][c] - 1]
            for f in g.faces[c]:
                for q in f:
                    if g.points[q] == vtx:
                        vpt = q
            for f in g.faces[c]:
                if vpt in f:
                    s.polygon([pr(g.points[q]) for q in f], fill=col,
                              stroke='#333', width=0.3, opacity=0.85)
    s.text(20, H - 40, caption, size=11)
    s.text(20, H - 22, '%d of %d vertices have a cell here; colors tell '
           'the cells apart' % (sum(sel), len(sel)), size=11)
    s.save(path)


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    cases = read_cases(os.path.join(HERE, 'cases.txt'), 4)
    kept = {}
    for name, mesh, args, about in cases:
        d = os.path.join(work, name)
        mname = os.path.basename(mesh)
        cmd = ('del_vor -avs %s -stor %s.stor -paraview %s -report '
               '%s.report %s' % (mname, name, name, name, args))
        checks = []

        def ok(c, v, good):
            res = good if isinstance(good, str) else \
                ('PASS' if good else 'FAIL')
            checks.append((c, v, res))
            print('%s_%s %s %s' % (name, c, v, res))

        rc = int(open(os.path.join(d, 'run.rc')).read())
        ok('exit', '%d (expected 0)' % rc, rc == 0)
        if rc != 0:
            rows.append((name, about, cmd, checks, None))
            continue
        m = read_avs(os.path.join(d, mname))
        vol, coef = stor_info(os.path.join(d, name + '.stor'))
        vmax = max(abs(v) for v in vol)
        pvd = open(os.path.join(d, name + '.pvd')).read()
        listed = re.findall(r'file="([^"]+)"', pvd)
        ok('exit', 'the .pvd lists %s' % ', '.join(listed),
           all(os.path.exists(os.path.join(d, f)) for f in listed))
        g = read_vtu(os.path.join(d, name + '_cells.vtu'))
        gm = read_vtu(os.path.join(d, name + '_mesh.vtu'))

        # the subset, worked out here
        nv = len(m.xyz)
        if '-paraview_subset material' in args:
            n = int(args.split('material')[1].split()[0])
            sel = [False] * nv
            for c, cl in zip(m.cells, m.clr):
                if cl == n:
                    for v in c:
                        sel[v] = True
        elif '-paraview_subset box' in args:
            b = [float(x) for x in args.split('box')[1].split()[:6]]
            sel = [all(b[k] <= p[k] <= b[k + 3] for k in range(3))
                   for p in m.xyz]
        elif '-paraview_subset flagged' in args:
            sel = [False] * nv
            for (i, j), v in coef.items():
                if v < 0:
                    sel[i] = sel[j] = True
            al = gm.cdata['alpha']
            # the mesh file holds only cells at selected vertices; the
            # flagged ones are among them
            for c, a in zip(gm.cells, al):
                if a < 1.0:
                    for q in c:
                        sel[gm.pdata['vertex'][q] - 1] = True
        else:
            sel = [True] * nv
        got = set(v - 1 for v in g.cdata['vertex'])
        want = set(i for i in range(nv) if sel[i])
        ok('select', '%d of %d vertices have a cell' % (len(got), nv),
           got == want)
        ncell = sum(1 for c in m.cells if any(sel[v] for v in c))
        e = max(abs(gm.pdata['volume'][q] - vol[gm.pdata['vertex'][q] - 1])
                for q in range(len(gm.points)))
        ok('mesh', '%d cells (%d touch the subset); volumes %.1e'
           % (len(gm.cells), ncell, e / vmax),
           len(gm.cells) == ncell and e <= 1e-15 * vmax)

        three = m.nen == 4
        if not three:
            tot = {}
            worst_share = 0.0
            for c in range(len(g.cells)):
                a = norm3(vec_area(g.points, g.cells[c]))
                worst_share = max(worst_share,
                                  abs(a - g.cdata['volume'][c]))
                i = g.cdata['vertex'][c] - 1
                tot[i] = tot.get(i, 0.0) + a
            e = max(abs(tot[i] - vol[i]) for i in tot)
            ok('area', 'polygons against the .stor %.1e, against their '
               'own volume %.1e of the largest'
               % (e / vmax, worst_share / vmax),
               e <= 1e-12 * vmax and worst_share <= 1e-12 * vmax)
            per = {}
            for c, cl in zip(m.cells, m.clr):
                for v in c:
                    per.setdefault(v, set()).add(cl)
            npoly = {}
            for c in range(len(g.cells)):
                i = g.cdata['vertex'][c] - 1
                npoly[i] = npoly.get(i, 0) + 1
            bad = sum(1 for i in npoly if npoly[i] != len(per[i]))
            ok('per', '%d polygons for %d vertices; %d vertices in 2 or '
               'more fractures (or materials)'
               % (len(g.cells), len(npoly),
                  sum(1 for i in npoly if len(per[i]) > 1)),
               bad == 0 if len(set(m.clr)) > 1 and view_is_3d(m)
               else len(g.cells) == len(npoly))
        else:
            nclosed = sum(1 for f in g.faces if closed(f))
            ok('closed', '%d of %d polyhedra closed' % (nclosed,
                                                       len(g.faces)),
               nclosed == len(g.faces))
            e = 0.0
            for c in range(len(g.cells)):
                i = g.cdata['vertex'][c] - 1
                v = polyhedron_volume(g.points, g.faces[c],
                                      m.xyz[i])
                e = max(e, abs(v - vol[i]))
            ok('volume', 'volume from the faces against the .stor %.1e '
               'of the largest' % (e / vmax), e <= 1e-12 * vmax)
            size = max(math.dist(a, b) for a in m.xyz[:1] for b in m.xyz)
            # every face of 4 or more points is flat: Voronoi faces
            # (with the hybrid, those of edges whose cells all have
            # alpha 1) and boundary pieces; the hybrid's other faces
            # are the kernel's triangles
            fa = [(norm3(vec_area(g.points, f)), f) for fs in g.faces
                  for f in fs]
            big = max(a for a, _ in fa)
            poly = [f for a, f in fa if len(f) > 3 and a >= 1e-6 * big]
            ntri = sum(1 for _, f in fa if len(f) == 3)
            p = max([planarity(g.points, f) for f in poly] or [0.0])
            ok('planar', 'faces of 4 or more points flat to %.1e of the '
               'mesh size (%d of %d faces; %d triangles; the rest have '
               'nearly no area)' % (p / size, len(poly), len(fa), ntri),
               p <= 1e-12 * size)
            fpath = os.path.join(d, name + '_faces.vtu')
            if os.path.exists(fpath):
                gf = read_vtu(fpath)
                A = {}
                for c in range(len(gf.cells)):
                    i = gf.cdata['vertex_i'][c] - 1
                    j = gf.cdata['vertex_j'][c] - 1
                    e_ = sub(m.xyz[j], m.xyz[i])
                    a = dot(vec_area(gf.points, gf.cells[c]), e_) / \
                        norm3(e_)
                    k = (min(i, j), max(i, j))
                    A[k] = A.get(k, 0.0) + a
                amax = max(abs(v) for v in A.values())
                if '-cv' not in args:
                    bis = 0.0
                    for c in range(len(gf.cells)):
                        i = gf.cdata['vertex_i'][c] - 1
                        j = gf.cdata['vertex_j'][c] - 1
                        e_ = sub(m.xyz[j], m.xyz[i])
                        le = norm3(e_)
                        mid = tuple(0.5 * (a + b) for a, b in
                                    zip(m.xyz[i], m.xyz[j]))
                        for q in gf.cells[c]:
                            bis = max(bis, abs(dot(sub(gf.points[q], mid),
                                                   e_)) / le)
                    ok('bisector', 'every dual face point within %.1e of '
                       'the mesh size of its edge\'s bisector plane'
                       % (bis / size), bis <= 1e-12 * size)
                worst = 0.0
                for (i, j), cval in coef.items():
                    d_ = math.dist(m.xyz[i], m.xyz[j])
                    if (i, j) in A:
                        worst = max(worst, abs(A[(i, j)] - cval * d_))
                ok('faces', '%d dual faces (%d edges); area along the edge '
                   'against the .stor %.1e of the largest'
                   % (len(gf.cells), len(A), worst / amax),
                   worst <= 1e-11 * amax and len(A) >= len(coef))
            kept[name] = g
            if name == 'poi3_legacy' and 'poi3_vor' in kept:
                a = kept['poi3_vor']
                same = (a.points == g.points and a.cells == g.cells and
                        a.faces == g.faces)
                ok('layout', '%s against %s: same points and faces'
                   % (g.layout, a.layout), same and g.layout == 'legacy'
                   and a.layout == 'new')

        # outlines that turn back on themselves: none with Voronoi;
        # with the hybrid only folds, where stage A put a dual point
        # on a facet through the cell's vertex
        size = max(math.dist(a, b) for a in m.xyz[:1] for b in m.xyz)
        outl = []
        for c in range(len(g.cells)):
            own = m.xyz[g.cdata['vertex'][c] - 1]
            fs = [g.cells[c]] if g.faces[c] is None else g.faces[c]
            outl += [(f, own) for f in fs]
        big = max(norm3(vec_area(g.points, f)) for f, _ in outl)
        nsp = nfo = 0
        for f, own in outl:
            a, b = spikes(g.points, f, 1e-12 * size, 1e-6 * big, own)
            nsp += a
            nfo += b
        if '-cv' in args:
            ok('spikes', '%d outlines turn back on themselves, all folds '
               'at a dual point on a facet (%d)' % (nsp, nfo),
               nsp == nfo)
        else:
            ok('spikes', '%d outlines turn back on themselves' % nsp,
               nsp == 0)

        # negative coefficients
        want = set(k for k, v in coef.items() if v < 0 and
                   (sel[k[0]] or sel[k[1]]))
        npath = os.path.join(d, name + '_negative.vtu')
        if want:
            gn = read_vtu(npath)
            got = set((min(i, j) - 1, max(i, j) - 1) for i, j in
                      zip(gn.cdata['vertex_i'], gn.cdata['vertex_j']))
            ok('negative', '%d segments, %d negative coefficients in the '
               '.stor' % (len(got), len(want)), got == want)
        else:
            ok('negative', 'none in the .stor, no file',
               not os.path.exists(npath))

        pic = name + '.svg'
        picture(os.path.join(logs, pic), name, m, g, sel,
                '%s: %s' % (mname, args or 'defaults'))
        rows.append((name, about, cmd, checks, pic))

    what = ('runs del_vor with -paraview and reads the files back with '
            'this suite\'s own reader (vtu_read.py, written from VTK\'s '
            'file format, both polyhedron layouts, text and binary). '
            'Every control volume must be closed and have the volume of '
            'the .stor, measured from its own written faces; the '
            'subsets, the mesh file, the dual faces and the negative '
            'coefficients must match the .stor. Open the .pvd files in '
            'ParaView to see them (00README says how).')
    nfail = write_case_page(HERE, logs, 'test_paraview', what, rows)
    sys.exit(1 if nfail else 0)


def norm3(a):
    return math.sqrt(dot(a, a))


def view_is_3d(m):
    return not all(abs(p[2]) < 1e-30 for p in m.xyz)


if __name__ == '__main__':
    main()
