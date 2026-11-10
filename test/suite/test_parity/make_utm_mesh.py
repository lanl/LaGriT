"""make_utm_mesh.py IN.inp OUT.inp

Writes IN.inp moved by (7e5, 3.2e6, 1.5e3), a UTM-size offset, with
17 significant digits so every coordinate is the exact double of the
moved value. test_parity's utm_tet case: the same tets as
test_3d_poi_01, far from the origin, where LaGriT's 3D writer loses
digits (it computes tet volumes from absolute coordinates).
Python 3 standard library.
"""

import sys

OFFSET = (7.0e5, 3.2e6, 1.5e3)


def main():
    src, dst = sys.argv[1], sys.argv[2]
    with open(src) as f:
        lines = f.read().split('\n')
    nn = int(lines[0].split()[0])
    out = [lines[0]]
    for ln in lines[1:1 + nn]:
        t = ln.split()
        x = [float(v) + o for v, o in zip(t[1:4], OFFSET)]
        out.append('%s %.17g %.17g %.17g' % (t[0], *x))
    out += lines[1 + nn:]
    with open(dst, 'w') as f:
        f.write('\n'.join(out))


if __name__ == '__main__':
    main()
