"""make_hfield_figures.py: the figures of HFIELD.md, as hand-written SVG
computed with the hfield rule. Python 3 standard library.

  python3 -I make_hfield_figures.py     writes hfield_rule.svg and
                                        hfield_inside.svg here

hfield_rule.svg: h along the x axis for two point sources (the first
example of HFIELD.md): source A at x = 0 (h_value 0.15, h_band 0.5,
h_growth 0.3), source B at x = 4 (h_value 0.02, the create default
h_growth 0.2), h_floor 0.06, h_ceiling 0.4. The two h_i dashed, h
solid. hfield / eval on 141 vertices of that line gives the same
values (checked when the figure was made).

hfield_inside.svg: h in the plane for an inside, surface source (a
closed polygon, h_value 0.05) and a point (h_value 0.1, h_band 0.3,
h_growth 0.3), h_floor 0.05, h_ceiling 0.3, h_growth 0.2.
"""

import math
import os

HERE = os.path.dirname(os.path.abspath(__file__))

_RAMP = [(0.00, (49, 54, 149)), (0.25, (69, 117, 180)),
         (0.50, (38, 166, 154)), (0.75, (171, 221, 64)),
         (1.00, (253, 231, 37))]


def ramp(t):
    t = min(1.0, max(0.0, t))
    for (t0, c0), (t1, c1) in zip(_RAMP, _RAMP[1:]):
        if t <= t1:
            u = (t - t0) / (t1 - t0)
            return '#%02x%02x%02x' % tuple(
                round(a + u * (b - a)) for a, b in zip(c0, c1))
    return '#%02x%02x%02x' % _RAMP[-1][1]


class Svg:
    def __init__(self, w, h):
        self.w, self.h, self.items = w, h, []

    def add(self, s):
        self.items.append(s)

    def line(self, x1, y1, x2, y2, color='#555', width=1.0, dash=None):
        d = ' stroke-dasharray="%s"' % dash if dash else ''
        self.add('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                 'stroke="%s" stroke-width="%.1f"%s/>'
                 % (x1, y1, x2, y2, color, width, d))

    def path(self, pts, color, width, dash=None):
        d = ' stroke-dasharray="%s"' % dash if dash else ''
        p = ' '.join('%.1f,%.1f' % q for q in pts)
        self.add('<polyline points="%s" fill="none" stroke="%s" '
                 'stroke-width="%.1f"%s/>' % (p, color, width, d))

    def text(self, x, y, s, size=12, anchor='start', color='#222',
             weight='normal'):
        self.add('<text x="%.1f" y="%.1f" font-size="%d" '
                 'text-anchor="%s" fill="%s" font-weight="%s" '
                 'font-family="Helvetica, Arial, sans-serif">%s</text>'
                 % (x, y, size, anchor, color, weight, s))

    def rect(self, x, y, w, h, fill):
        self.add('<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" '
                 'fill="%s"/>' % (x, y, w, h, fill))

    def save(self, name):
        with open(os.path.join(HERE, name), 'w', newline='\n') as f:
            f.write('<svg xmlns="http://www.w3.org/2000/svg" width="%d" '
                    'height="%d" viewBox="0 0 %d %d">\n'
                    '<rect width="100%%" height="100%%" fill="#fff"/>\n'
                    % (self.w, self.h, self.w, self.h))
            f.write('\n'.join(self.items) + '\n</svg>\n')


# ------------------------------------------------------------- the rule
FLOOR, CEIL = 0.06, 0.4
A = dict(x=0.0, H=0.15, band=0.5, growth=0.3)
B = dict(x=4.0, H=0.02, band=0.0, growth=0.2)


def h_i(s, x):
    return s['H'] + s['growth'] * max(0.0, abs(x - s['x']) - s['band'])


def h(x):
    return min(CEIL, max(FLOOR, min(h_i(A, x), h_i(B, x))))


def rule_figure():
    W, H = 760, 400
    x0, y0, pw, ph = 70, 30, 650, 300
    xlo, xhi, hmax = -1.5, 5.5, 0.55
    X = lambda x: x0 + pw * (x - xlo) / (xhi - xlo)  # noqa
    Y = lambda v: y0 + ph - ph * v / hmax  # noqa
    s = Svg(W, H)
    # axes and ticks
    s.line(x0, Y(0), x0 + pw, Y(0), '#444')
    s.line(x0, y0, x0, Y(0), '#444')
    for k in range(-1, 6):
        s.line(X(k), Y(0), X(k), Y(0) + 4, '#444')
        s.text(X(k), Y(0) + 17, str(k), 11, 'middle')
    for v in (0.1, 0.2, 0.3, 0.4, 0.5):
        s.line(x0 - 4, Y(v), x0, Y(v), '#444')
        s.text(x0 - 7, Y(v) + 4, '%.1f' % v, 11, 'end')
    s.text(x0 + pw / 2, Y(0) + 50, 'x (along the line through the '
           'two sources)', 12, 'middle')
    s.text(x0 - 50, y0 - 12, 'h', 13, 'start', '#222', 'bold')
    # floor and ceiling
    for v, name in ((CEIL, 'h_ceiling 0.4'), (FLOOR, 'h_floor 0.06')):
        s.line(x0, Y(v), x0 + pw, Y(v), '#999', 1.0, '5,4')
        s.text(x0 + pw - 2, Y(v) - 5, name, 11, 'end', '#666')
    # the two h_i, dashed, cut at the top of the plot
    xs = [xlo + (xhi - xlo) * i / 700 for i in range(701)]
    for src, col in ((A, '#e8833a'), (B, '#3a7be8')):
        pts = [(X(x), Y(min(hmax, h_i(src, x)))) for x in xs
               if h_i(src, x) <= hmax]
        s.path(pts, col, 1.5, '6,4')
    # h, solid
    s.path([(X(x), Y(h(x))) for x in xs], '#222', 3.0)
    # the sources
    for src, col, name in ((A, '#e8833a', 'source A'),
                           (B, '#3a7be8', 'source B')):
        xp = X(src['x'])
        s.add('<polygon points="%.1f,%.1f %.1f,%.1f %.1f,%.1f" fill="%s"/>'
              % (xp, Y(0) - 9, xp - 6, Y(0), xp + 6, Y(0), col))
        s.text(xp, Y(0) + 30, name, 11, 'middle', col, 'bold')
    # A: H and h_band
    s.text(X(0), Y(0.15) - 22, 'H = h_value 0.15', 11, 'middle', '#b85d1a')
    yb = Y(0.15) - 8
    s.line(X(-0.5), yb, X(0.5), yb, '#b85d1a', 1.2)
    for xb in (-0.5, 0.5):
        s.line(X(xb), yb - 4, X(xb), yb + 4, '#b85d1a', 1.2)
    s.text(X(0) , Y(0.15) + 16, 'h_band 0.5 on each side: h = H',
           10, 'middle', '#b85d1a')
    # A: the slope triangle, from x = 0.7 to 1.2
    xa, xb = 0.7, 1.2
    s.line(X(xa), Y(h_i(A, xa)), X(xb), Y(h_i(A, xa)), '#b85d1a', 1.0)
    s.line(X(xb), Y(h_i(A, xa)), X(xb), Y(h_i(A, xb)), '#b85d1a', 1.0)
    for k, t in enumerate(('h_growth 0.3: h grows', '0.3 per unit of',
                           'distance')):
        s.text(X(xb) + 8, Y(h_i(A, xa)) + 14 + 13 * k, t, 10, 'start',
               '#b85d1a')
    # the ceiling plateau, and B under the floor
    for k, t in enumerate(('h_A and h_B', 'both above', 'h_ceiling:',
                           'h = h_ceiling')):
        s.text(X(1.78), Y(0.36) + 13 * k, t, 10, 'middle', '#444')
    for k, t in enumerate(('H = 0.02 is below', 'h_floor: h = h_floor')):
        s.text(X(3.5), Y(0.115) + 13 * k, t, 10, 'end', '#2a5bb8')
    # legend
    lx, ly = X(2.55), Y(0.54) + 4
    s.line(lx, ly, lx + 30, ly, '#e8833a', 1.5, '6,4')
    s.text(lx + 36, ly + 4, 'h_A', 11)
    s.line(lx, ly + 16, lx + 30, ly + 16, '#3a7be8', 1.5, '6,4')
    s.text(lx + 36, ly + 20, 'h_B', 11)
    s.line(lx, ly + 32, lx + 30, ly + 32, '#222', 3.0)
    s.text(lx + 36, ly + 36,
           'h = min(h_ceiling, max(h_floor, the smaller h_i))', 11)
    s.save('hfield_rule.svg')


# -------------------------------------------------------- inside, surface
POLY = [(2.0, 0.5), (3.2, 0.5), (3.2, 1.0), (2.6, 1.0), (2.6, 1.9),
        (2.0, 1.9)]
PT = (0.5, 1.2)
F2, C2, G2 = 0.05, 0.3, 0.2
HP, BP, GP = 0.1, 0.3, 0.3
HS = 0.05


def d_seg(p, a, b):
    ax, ay = b[0] - a[0], b[1] - a[1]
    t = ((p[0] - a[0]) * ax + (p[1] - a[1]) * ay) / (ax * ax + ay * ay)
    t = max(0.0, min(1.0, t))
    return math.hypot(p[0] - a[0] - t * ax, p[1] - a[1] - t * ay)


def inside(p):
    c = False
    for i in range(len(POLY)):
        (x1, y1), (x2, y2) = POLY[i], POLY[(i + 1) % len(POLY)]
        if (y1 > p[1]) != (y2 > p[1]):
            if x1 + (p[1] - y1) * (x2 - x1) / (y2 - y1) > p[0]:
                c = not c
    return c


def h2(p):
    ds = 0.0 if inside(p) else min(
        d_seg(p, POLY[i], POLY[(i + 1) % len(POLY)])
        for i in range(len(POLY)))
    hs = HS + G2 * ds
    hp = HP + GP * max(0.0, math.dist(p, PT) - BP)
    return min(C2, max(F2, min(hs, hp)))


def inside_figure():
    xlo, xhi, ylo, yhi = 0.0, 3.6, 0.0, 2.4
    sc = 200.0
    ox, oy = 20, 30
    W, H = int(ox + sc * (xhi - xlo) + 130), int(oy + sc * (yhi - ylo) + 30)
    X = lambda x: ox + sc * (x - xlo)  # noqa
    Y = lambda y: oy + sc * (yhi - y)  # noqa
    s = Svg(W, H)
    n = 6
    step = 1.0 / (sc / n)
    nx = int(round((xhi - xlo) / step))
    ny = int(round((yhi - ylo) / step))
    for j in range(ny):
        y = ylo + j * step
        cols = []
        for i in range(nx):
            v = h2((xlo + (i + 0.5) * step, y + step / 2))
            q = round(24 * (v - F2) / (C2 - F2)) / 24
            cols.append(ramp(q))
        i = 0
        while i < nx:
            k = i
            while k + 1 < nx and cols[k + 1] == cols[i]:
                k += 1
            s.rect(X(xlo + i * step), Y(y + step),
                   n * (k - i + 1) + 0.3, n + 0.3, cols[i])
            i = k + 1
    pts = ' '.join('%.1f,%.1f' % (X(a), Y(b)) for a, b in POLY)
    s.add('<polygon points="%s" fill="none" stroke="#fff" '
          'stroke-width="2"/>' % pts)
    s.add('<circle cx="%.1f" cy="%.1f" r="4" fill="#fff"/>'
          % (X(PT[0]), Y(PT[1])))
    s.add('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" '
          'stroke="#fff" stroke-dasharray="4,3"/>'
          % (X(PT[0]), Y(PT[1]), sc * BP))
    s.text(X(2.3), Y(1.2), 'inside: h = 0.05', 11, 'middle', '#fff')
    s.text(X(PT[0]), Y(PT[1]) - sc * BP - 6, 'h_band 0.3', 11, 'middle',
           '#fff')
    s.text(X(PT[0]), Y(PT[1]) + 18, 'h = 0.1', 11, 'middle', '#fff')
    s.text(ox, 18, 'h in the plane: an inside, surface source and a '
           'point (dark: small h)', 12)
    # legend
    lx, ly, lh = X(xhi) + 30, oy + 20, 240
    for i in range(60):
        t = 1 - i / 59
        s.rect(lx, ly + lh * i / 60, 18, lh / 60 + 0.5, ramp(t))
    s.text(lx + 24, ly + 8, '%.2f h_ceiling' % C2, 11)
    s.text(lx + 24, ly + lh, '%.2f h_floor' % F2, 11)
    s.save('hfield_inside.svg')


if __name__ == '__main__':
    rule_figure()
    inside_figure()
