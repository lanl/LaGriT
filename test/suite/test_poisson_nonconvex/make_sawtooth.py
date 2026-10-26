#!/usr/bin/env python3
"""
Generate a sawtooth nonconvex polygon as an AVS UCD point set.

    python3 make_sawtooth.py --out saw_safe --width 25 --height 4 \\
                             --valley 1 --teeth 20 --growth linear
    python3 make_sawtooth.py --out saw_safe --from-json saw_safe.json

Writes <out>.poly and a <out>.json sidecar recording every parameter,
so any polygon can be regenerated exactly with --from-json.

Shape, traversed counter-clockwise as the driver requires:

    bottom edge   (0,0) -> (W,0)
    right edge    (W,0) -> (W,yv)
    top sawtooth  right to left, valley / peak / valley ... valley
    left edge     (0,yv) -> (0,0)

The top has 2*nteeth segments of horizontal width W/(2*nteeth). Both
ends are VALLEYS, so every tooth is a complete triangle with a valley
on each side:

    y = 1.0, P, 1.0, P, ... 1.0          (right to left)

CHANGE FROM THE FIRST VERSION: the right edge used to run up to a
PEAK at x = W, and the first top vertex was another peak, which left
a flat topped stub at the right end instead of a tooth. The polygon
area, the vertex count and the tooth width are unchanged in kind, but
polygons regenerated with this version are NOT identical to the old
ones. Re-run the suite after regenerating.

Peak envelope P(t), t = x/W in [0,1], selected with --growth:

    linear   P = yv + (H-yv) * t
    power    P = yv + (H-yv) * t**p
    exp      P = yv + (H-yv) * (exp(k*t)-1)/(exp(k)-1)

LIMITS THE DRIVER WILL ENFORCE, reported by this script:

    min_angle_deg   smallest vertex angle measured the way the driver
                    measures it (unsigned, so a notch counts too).
                    The driver refuses the polygon below 9.0 deg.

    min_edge        shortest edge. The driver refuses h > 2*min_edge.
                    Every top edge spans W/(2*nteeth) horizontally,
                    so h can NEVER exceed about twice the tooth half
                    width, whatever the envelope. That is a hard
                    ceiling on how far an h sweep can be pushed.

RECOVERY

Measured on the first real run: recovery holds while
tooth half width / h >= 1 and degrades below that, by LOSING tooth
area rather than by bridging notches.
"""
import argparse, math, json

ANGLE_MIN_DEG = 9.0        # POI_ANGLE_MIN_DEG in poi_params.h
EDGE_MIN_FACTOR = 0.5      # POI_EDGE_MIN_FACTOR in poi_params.h


def peak(t, yv, H, growth, param):
    if growth == 'linear':
        return yv + (H - yv) * t
    if growth == 'power':
        return yv + (H - yv) * (t ** param)
    if growth == 'exp':
        return yv + (H - yv) * (math.exp(param * t) - 1.0) / \
               (math.exp(param) - 1.0)
    raise ValueError(growth)


def build(W, H, yv, nteeth, growth, param):
    pts = [(0.0, 0.0), (W, 0.0)]           # bottom, left to right
    n = 2 * nteeth
    for i in range(n, -1, -1):             # top, right to left
        x = W * i / n
        if i % 2 == 0:
            pts.append((x, yv))            # valley, including both ends
        else:
            pts.append((x, peak(i / n, yv, H, growth, param)))
    return pts                             # closes from (0,yv) to (0,0)


def write_avs(fn, pts):
    with open(fn, 'w') as f:
        f.write(f"{len(pts)} 0 0 0 0 \n")
        for i, (x, y) in enumerate(pts, 1):
            f.write(f"{i} {x:.6f} {y:.6f} 0.0\n")


def shoelace(pts):
    a = 0.0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        x2, y2 = pts[(i + 1) % n]
        a += x1 * y2 - x2 * y1
    return a / 2.0


def limits(pts):
    """Smallest vertex angle (as the driver measures it), shortest
    edge."""
    n = len(pts)
    amin, emin = 180.0, float('inf')
    for i in range(n):
        p, q, r = pts[i - 1], pts[i], pts[(i + 1) % n]
        v1 = (p[0] - q[0], p[1] - q[1])
        v2 = (r[0] - q[0], r[1] - q[1])
        c = (v1[0]*v2[0] + v1[1]*v2[1]) / (math.hypot(*v1) *
                                          math.hypot(*v2))
        amin = min(amin, math.degrees(math.acos(max(-1.0, min(1.0, c)))))
        emin = min(emin, math.hypot(*v2))
    return amin, emin


if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('--out', required=True)
    ap.add_argument('--from-json', dest='from_json',
                    help='take all shape parameters from this sidecar')
    ap.add_argument('--width',  type=float, default=25.0)
    ap.add_argument('--height', type=float, default=25.0)
    ap.add_argument('--valley', type=float, default=1.0)
    ap.add_argument('--teeth',  type=int,   default=12)
    ap.add_argument('--growth', default='linear',
                    choices=['linear', 'power', 'exp'])
    ap.add_argument('--param',  type=float, default=2.0)
    a = ap.parse_args()

    if a.from_json:
        with open(a.from_json) as f:
            m = json.load(f)
        a.width, a.height, a.valley = m['width'], m['height'], m['valley']
        a.teeth, a.growth, a.param = m['teeth'], m['growth'], m['param']

    pts = build(a.width, a.height, a.valley, a.teeth, a.growth, a.param)
    write_avs(a.out + '.poly', pts)
    area = shoelace(pts)
    amin, emin = limits(pts)
    meta = dict(vertices=len(pts), area=area,
                tooth_half_width=a.width / (2.0 * a.teeth),
                width=a.width, height=a.height, valley=a.valley,
                teeth=a.teeth, growth=a.growth, param=a.param,
                ccw=(area > 0),
                min_angle_deg=amin, min_edge=emin,
                h_max=emin / EDGE_MIN_FACTOR,
                accepted_by_angle=(amin >= ANGLE_MIN_DEG))
    with open(a.out + '.json', 'w') as f:
        json.dump(meta, f, indent=1)
    print(f"{a.out+'.poly':22} {len(pts):4} verts  area {area:9.4f}  "
          f"half-tooth {meta['tooth_half_width']:6.4f}  "
          f"min angle {amin:6.2f}{'' if amin >= ANGLE_MIN_DEG else ' REFUSED'}"
          f"  h_max {meta['h_max']:6.4f}  ccw={area > 0}")
