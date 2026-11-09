"""make_figures.py: the figures of the del_vor manual, as hand-written
SVG computed from the coordinates shown. Python 3 standard library.

  python3 -I make_figures.py      writes delaunay_check.svg here

delaunay_check.svg, three panels:
  1. two triangles sharing edge ab, Delaunay: the circumcenters c1, c2
     are in the order of the normal n (from T1 into T2), delta > 0
  2. the same four vertices with the other diagonal: c2 lies behind
     c1, delta < 0, a violation
  3. a boundary triangle with an obtuse angle opposite its boundary
     edge: its circumcenter lies outside, s > 0 (reported, not fatal)
"""

import math
import os

HERE = os.path.dirname(os.path.abspath(__file__))


def circumcenter(a, b, c):
    ax, ay = a
    bx, by = b[0] - ax, b[1] - ay
    cx, cy = c[0] - ax, c[1] - ay
    d = 2 * (bx * cy - by * cx)
    ux = (cy * (bx * bx + by * by) - by * (cx * cx + cy * cy)) / d
    uy = (bx * (cx * cx + cy * cy) - cx * (bx * bx + by * by)) / d
    return (ax + ux, ay + uy)


class Panel:
    """World (x, y) -> SVG, y up; ox, oy the panel's top-left corner."""

    def __init__(self, ox, oy, scale, x0, y1):
        self.ox, self.oy, self.s, self.x0, self.y1 = ox, oy, scale, x0, y1

    def __call__(self, p):
        return (self.ox + (p[0] - self.x0) * self.s,
                self.oy + (self.y1 - p[1]) * self.s)


def esc(t):
    return t.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def main():
    out = []
    W, H = 900, 380
    out.append('<svg xmlns="http://www.w3.org/2000/svg" width="%d" '
               'height="%d" viewBox="0 0 %d %d" font-family="Helvetica, '
               'Arial, sans-serif">' % (W, H, W, H))
    out.append('<rect width="%d" height="%d" fill="#fff"/>' % (W, H))
    out.append('<defs><marker id="ar" viewBox="0 0 10 10" refX="9" '
               'refY="5" markerWidth="7" markerHeight="7" '
               'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 '
               'z" fill="#111"/></marker></defs>')

    def line(pr, p, q, col='#444', w=1.5, dash=None, arrow=False):
        (x1, y1), (x2, y2) = pr(p), pr(q)
        out.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                   'stroke="%s" stroke-width="%s"%s%s/>' % (
                       x1, y1, x2, y2, col, w,
                       ' stroke-dasharray="%s"' % dash if dash else '',
                       ' marker-end="url(#ar)"' if arrow else ''))

    def poly(pr, pts, fill, op=0.15):
        out.append('<polygon points="%s" fill="%s" fill-opacity="%s" '
                   'stroke="#444" stroke-width="1.5"/>' % (
                       ' '.join('%.1f,%.1f' % pr(p) for p in pts), fill,
                       op))

    def dot(pr, p, col='#111', r=4):
        x, y = pr(p)
        out.append('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"/>'
                   % (x, y, r, col))

    def text(x, y, t, size=13, col='#111', bold=False, anchor='start'):
        out.append('<text x="%.1f" y="%.1f" font-size="%s" fill="%s"%s '
                   'text-anchor="%s">%s</text>' % (
                       x, y, size, col, ' font-weight="bold"' if bold
                       else '', anchor, esc(t)))

    def label(pr, p, t, dx, dy, col='#111', size=13):
        x, y = pr(p)
        text(x + dx, y + dy, t, size=size, col=col)

    def circle(pr, c, p, col):
        x, y = pr(c)
        r = math.dist(c, p) * pr.s
        out.append('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" '
                   'stroke="%s" stroke-width="1" stroke-dasharray="4 3"/>'
                   % (x, y, r, col))

    blue, orange, red, green = '#2b6cb0', '#b7791f', '#c0392b', '#1a7f37'

    # panels 1 and 2: the same four vertices
    a, b = (0.0, -1.0), (0.0, 1.0)
    p, q = (-1.0, 0.15), (1.25, -0.2)
    for k, (title, flip) in enumerate((('Delaunay: delta > 0', False),
                                       ('not Delaunay: delta < 0',
                                        True))):
        pr = Panel(20 + 300 * k, 60, 95, -1.45, 1.6)
        if not flip:
            t1, t2 = (a, b, p), (b, a, q)      # shared edge a b
            e0, e1 = a, b
        else:
            t1, t2 = (p, q, b), (q, p, a)      # shared edge p q
            e0, e1 = p, q
        c1, c2 = circumcenter(*t1), circumcenter(*t2)
        poly(pr, t1, blue)
        poly(pr, t2, orange)
        circle(pr, c1, t1[0], blue)
        circle(pr, c2, t2[0], orange)
        # unit normal of the shared edge, pointing from T1 into T2
        ex, ey = e1[0] - e0[0], e1[1] - e0[1]
        L = math.hypot(ex, ey)
        n = (ey / L, -ex / L)
        third = [v for v in t2 if v not in (e0, e1)][0]
        if (third[0] - e0[0]) * n[0] + (third[1] - e0[1]) * n[1] < 0:
            n = (-n[0], -n[1])
        delta = (c2[0] - c1[0]) * n[0] + (c2[1] - c1[1]) * n[1]
        mid = ((e0[0] + e1[0]) / 2, (e0[1] + e1[1]) / 2)
        line(pr, e0, e1, col='#111', w=2.5)
        # n drawn from a point near e1, away from the Voronoi edge
        foot = (0.2 * e0[0] + 0.8 * e1[0], 0.2 * e0[1] + 0.8 * e1[1])
        tip = (foot[0] + 0.4 * n[0], foot[1] + 0.4 * n[1])
        line(pr, foot, tip, col='#111', w=1.5, arrow=True)
        label(pr, tip, 'n', 5, 5)
        line(pr, c1, c2, col=red if delta < 0 else green, w=3)
        dot(pr, c1, blue)
        dot(pr, c2, orange)
        # labels beside each circumcenter, along the edge direction
        ed = (ex / L, ey / L)
        for cc, nm, col in ((c1, 'c1', blue), (c2, 'c2', orange)):
            x, y = pr((cc[0] - 0.22 * ed[0], cc[1] - 0.22 * ed[1]))
            text(x, y + 5, nm, col=col, anchor='middle')
        for t, nm, col in ((t1, 'T1', blue), (t2, 'T2', orange)):
            o = [v for v in t if v not in (e0, e1)][0]
            pt = (0.4 * mid[0] + 0.6 * o[0], 0.4 * mid[1] + 0.6 * o[1])
            x, y = pr(pt)
            text(x, y + 5, nm, size=14, col=col, anchor='middle')
        text(pr.ox + 10, 40, title, size=15, bold=True)
        text(pr.ox + 10, 340, 'delta/L = (c2 - c1) . n / L = %+.3f'
             % (delta / L), size=13, col=red if delta < 0 else green)
        text(pr.ox + 10, 358, 'the shared edge (black), the Voronoi edge'
             ' c1 c2', size=11, col='#555')

    # panel 3: a boundary triangle, obtuse opposite its boundary edge
    pr = Panel(640, 60, 95, -1.25, 1.0)
    u, v, w = (-1.0, 0.0), (1.0, 0.0), (0.0, 0.6)
    c = circumcenter(u, v, w)
    out.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
               'fill="#888" fill-opacity="0.08"/>' % (
                   pr((-1.25, 0.95))[0], pr((-1.25, 0.95))[1],
                   2.5 * 95, 0.95 * 95))
    poly(pr, (u, v, w), blue)
    circle(pr, c, u, blue)
    line(pr, u, v, col='#111', w=2.5)
    line(pr, (0.55, 0.0), (0.55, -0.4), col='#111', arrow=True)
    label(pr, (0.55, -0.4), 'n_out', 6, 4)
    line(pr, (0.0, 0.0), c, col=orange, w=3)
    dot(pr, c, orange)
    label(pr, c, 'c', -16, 5, orange)
    label(pr, (0.0, 0.2), 'T', -5, 4, blue, 14)
    label(pr, (-1.2, 0.9), 'mesh', 0, 14, '#555', 12)
    label(pr, (-1.2, -0.05), 'outside', 0, 18, '#555', 12)
    label(pr, (0.0, 0.0), 'x', -14, 16, '#111', 12)
    s = (c[1] - 0.0) * -1.0 / 2.0
    text(pr.ox + 10, 40, 'boundary: circumcenter outside',
         size=15, bold=True)
    text(pr.ox + 10, 340, 's/L = (c - x) . n_out / L = %+.3f' % s,
         size=13, col=orange)
    text(pr.ox + 10, 358, 'reported, not fatal: a negative A/d is '
         'likely here', size=11, col='#555')
    out.append('</svg>')
    with open(os.path.join(HERE, 'delaunay_check.svg'), 'w') as f:
        f.write('\n'.join(out) + '\n')


if __name__ == '__main__':
    main()
