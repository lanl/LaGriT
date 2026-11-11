"""vtu_read.py: a reader for the .vtu files del_vor writes, and the
measures test_paraview checks. Python 3 standard library (struct for
binary), written from VTK's file format, not from del_vor's code.

    from vtu_read import read_vtu
    g = read_vtu('out_cells.vtu')
    g.points, g.cells (point ids per cell), g.types, g.faces (per
    polyhedron: list of faces, each a list of point ids), g.cdata,
    g.pdata (name -> list), g.version, g.layout ('new' or 'legacy')

Reads ASCII and appended raw data (UInt64 headers), and both of VTK's
polyhedron layouts: file version 2.3 and later face_connectivity /
face_offsets / polyhedron_to_faces / polyhedron_offsets; earlier,
faces / faceoffsets.
"""

import re
import struct

TYPES = {'Float64': ('d', 8), 'Float32': ('f', 4), 'Int64': ('q', 8),
         'Int32': ('i', 4), 'UInt8': ('B', 1), 'UInt64': ('Q', 8)}


class Grid:
    pass


def _arrays(text, raw):
    """name -> (attributes, values) for every DataArray, and where."""
    out = []
    for mt in re.finditer(r'<DataArray([^>]*?)(/>|>(.*?)</DataArray>)',
                          text, re.S):
        at = dict(re.findall(r'(\w+)="([^"]*)"', mt.group(1)))
        fmt, size = TYPES[at['type']]
        if at.get('format') == 'appended':
            off = int(at['offset'])
            n = struct.unpack('<Q', raw[off:off + 8])[0]
            vals = list(struct.unpack('<%d%s' % (n // size, fmt),
                                      raw[off + 8:off + 8 + n]))
        else:
            tok = mt.group(3).split()
            conv = float if fmt in 'fd' else int
            vals = [conv(t) for t in tok]
        out.append((at, vals, mt.start()))
    return out


def read_vtu(path):
    data = open(path, 'rb').read()
    k = data.find(b'<AppendedData')
    raw = b''
    if k >= 0:
        u = data.index(b'_', k)
        raw = data[u + 1:]
        text = data[:k].decode()
    else:
        text = data.decode()
    g = Grid()
    g.version = re.search(r'<VTKFile[^>]*version="([^"]+)"', text).group(1)
    arr = _arrays(text, raw)
    i_pd = text.find('<PointData')
    i_cd = text.find('<CellData')
    i_pt = text.find('<Points>')
    i_ce = text.find('<Cells>')
    g.pdata, g.cdata, cells = {}, {}, {}
    pts = None
    for at, vals, pos in arr:
        name = at.get('Name', '')
        if pos > i_ce >= 0:
            cells[name] = vals
        elif pos > i_pt >= 0:
            pts = vals
        elif pos > i_cd >= 0:
            g.cdata[name] = vals
        elif pos > i_pd >= 0:
            g.pdata[name] = vals
    g.points = [tuple(pts[3 * i:3 * i + 3]) for i in range(len(pts) // 3)]
    off = cells['offsets']
    con = cells['connectivity']
    g.types = cells['types']
    g.cells = [con[(off[c - 1] if c else 0):off[c]] for c in range(len(off))]
    g.faces = [None] * len(off)
    major, minor = [int(x) for x in g.version.split('.')[:2]]
    if (major, minor) >= (2, 3):
        g.layout = 'new'
        if 'polyhedron_offsets' in cells:
            fo = cells['face_offsets']
            fc = cells['face_connectivity']
            face = [fc[(fo[f - 1] if f else 0):fo[f]] for f in range(len(fo))]
            po = cells['polyhedron_offsets']
            pf = cells['polyhedron_to_faces']
            for c in range(len(po)):
                ids = pf[(po[c - 1] if c else 0):po[c]]
                if ids:
                    g.faces[c] = [face[f] for f in ids]
    else:
        g.layout = 'legacy'
        if 'faceoffsets' in cells:
            fs = cells['faces']
            fo = cells['faceoffsets']
            start = 0
            for c in range(len(fo)):
                if fo[c] < 0:
                    continue
                p = start
                nf = fs[p]
                p += 1
                lst = []
                for _ in range(nf):
                    n = fs[p]
                    lst.append(fs[p + 1:p + 1 + n])
                    p += 1 + n
                g.faces[c] = lst
                start = fo[c]
    return g


def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def polyhedron_volume(pts, faces, origin):
    """Divergence theorem: each face fanned from its first point, the
    tets (origin, p0, pa, pa+1) summed. Positive for outward faces."""
    v = 0.0
    for f in faces:
        p0 = sub(pts[f[0]], origin)
        for a in range(1, len(f) - 1):
            v += dot(p0, cross(sub(pts[f[a]], origin),
                               sub(pts[f[a + 1]], origin))) / 6.0
    return v


def closed(faces):
    """Every face edge (a, b) is matched by exactly one (b, a)."""
    e = {}
    for f in faces:
        for a in range(len(f)):
            k = (f[a], f[(a + 1) % len(f)])
            e[k] = e.get(k, 0) + 1
    return all(n == 1 and e.get((b, a), 0) == 1 for (a, b), n in e.items())


def polygon_area(pts, ids, normal):
    """Signed area of a polygon (any orientation in 3D) along normal."""
    p0 = pts[ids[0]]
    s = 0.0
    for a in range(1, len(ids) - 1):
        s += dot(cross(sub(pts[ids[a]], p0), sub(pts[ids[a + 1]], p0)),
                 normal) / 2.0
    return s


def planarity(pts, face):
    """Largest distance of a face's points from its plane (the plane
    of its summed normal, through its first point), in the units of
    the coordinates."""
    p0 = pts[face[0]]
    n = (0.0, 0.0, 0.0)
    for a in range(1, len(face) - 1):
        c = cross(sub(pts[face[a]], p0), sub(pts[face[a + 1]], p0))
        n = (n[0] + c[0], n[1] + c[1], n[2] + c[2])
    nn = dot(n, n) ** 0.5
    if nn == 0.0:
        return 0.0
    n = (n[0] / nn, n[1] / nn, n[2] / nn)
    return max(abs(dot(sub(pts[q], p0), n)) for q in face)
