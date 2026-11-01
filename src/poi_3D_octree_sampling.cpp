/*
 * poi_3D_octree_sampling.cpp -- createpts / poisson_disk / 3d_box with
 * h_lookup octree: the sampler's neighbour grid on the octree leaves.
 *
 * The hex grid path uses one uniform grid of cells h / sqrt(3)
 * (h = h_spacing_scalar) over the whole box; a cell holds the number of
 * the first point whose disk covers the cell's min corner. Those cells are
 * small enough that every accepted point marks its own cell. Here each
 * octree leaf is split into m x m x m sub-cells with m = ceil(size / hmin),
 * hmin the smallest h at the leaf's 8 corners, and each sub-cell lists the
 * points inside it:
 *   search   every point in the sub-cells that meet the cube of half side
 *            R around a candidate x: R is at least the largest h at the
 *            corners of the leaves that meet the cube (worked out once per
 *            leaf), so every point whose disk can reach x is found (exact
 *            while |grad h| < 1; Poisson disk sampling needs far less)
 *   test     a candidate inside the disk of a point in its own sub-cell
 *            is rejected (cheap); then the full test, as before
 *   empty    a sub-cell is empty unless one point provably covers it:
 *            all 8 corners within max(r, the smallest h in the sub-cell)
 *   fill     in raster order over the box, each empty sub-cell gets
 *            numSamples darts; if none is accepted it is split in 8, and
 *            so on down to OCT_SMALLEST_BOX h; a box is dropped as soon
 *            as one point provably covers it (maximal sampling as in
 *            Ebeida et al. 2011, 2012); at the smallest size its centre
 *            and corners are tried last. The boxes left are reported:
 *            any vertex an exhaustive search could still add is inside
 *            them (most are covered by two disks, not one).
 * Find and fill gather the points near a sub-cell once and test its boxes
 * against that short list.
 * Called through the one-line branches at the top of the Domain methods
 * in poi_3D_neighborGrid.cpp and in testCandidate.
 */
#include <algorithm>
#include <array>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <utility>
#include <vector>

#include "poi_3D_domain.h"
#include "poi_helperFunctions.h"

using std::cout;
using std::endl;

// find and fill split a box that is not proven covered down to this
// fraction of h (the boxes left at that size are reported)
static const double OCT_SMALLEST_BOX = 1.0 / 16.0;


void Domain::octInitNeighborGrid() {
    cout << "Initializing Neighbor Grid (h_lookup octree)" << endl;
    // sampleAlongLine still works out hex grid cell indices (unused
    // here): give it the hex grid's cell size rather than nothing
    cellSize = h / sqrt(3.0);
    iCellSize = 1.0 / cellSize;
    const std::vector<poi_octree::Cell> &C = octree->cells();
    octOrd.assign(C.size(), -1);
    octLeaf.clear();
    octOff.clear();
    octM.clear();
    int64_t total = 0;
    int mmax = 0;
    octLeafHmax.clear();
    for (std::size_t c = 0; c < C.size(); c++) {
        if (C[c].first_child >= 0) continue;
        double hmin = 1e300, hmax = -1e300;
        for (int q = 0; q < 8; q++) {
            double hv = octree->vertexH(C[c].corner[q]);
            hmin = std::min(hmin, hv);
            hmax = std::max(hmax, hv);
        }
        double s = octree->cellSize(C[c].level);
        octLeafHmax.push_back(float(hmax * (1.0 + 1e-6)));
        int m = int(std::ceil(s / hmin - 1e-9));
        m = std::max(1, std::min(255, m));
        mmax = std::max(mmax, m);
        octOrd[c] = int32_t(octLeaf.size());
        octLeaf.push_back(int32_t(c));
        octOff.push_back(total);
        octM.push_back(uint8_t(m));
        total += int64_t(m) * m * m;
    }
    octOff.push_back(total);
    octHead.assign(std::size_t(total), 0);
    octNext.clear();
    octInitReach();
    cout << "  leaves " << octLeaf.size() << ", sub-cells " << total << " (at most " << mmax
         << " per side), " << (total * 4 + octLeaf.size() * 25 + C.size() * 4) / 1000000.0
         << " MB" << endl;
    for (unsigned int i = 0; i < numNodes; i++) {
        nodes[i].nodeNum = i + 1;
        getExclusionRadius(nodes[i]);
        tagNeighborCells(nodes[i]);
    }
    cout << "Initializing Grid Complete\n" << endl;
}

// about how many vertices the box will hold: its volume / h^3, with h
// the smallest at the corners of each leaf (an upper estimate)
double Domain::octEstimateNodes() {
    double lo[3] = {xMin, yMin, zMin}, hi[3] = {xMax, yMax, zMax}, n = 0.0;
    std::vector<int32_t> leaves;
    octree->leavesInBox(lo, hi, leaves);
    for (int32_t c : leaves) {
        const poi_octree::Cell &cl = octree->cells()[c];
        double mn[3], s = octree->cellSize(cl.level), hmin = 1e300, v = 1.0;
        octree->cellMin(cl, mn);
        for (int q = 0; q < 8; q++) hmin = std::min(hmin, octree->vertexH(cl.corner[q]));
        for (int d = 0; d < 3; d++) v *= std::max(0.0, std::min(hi[d], mn[d] + s) - std::max(lo[d], mn[d]));
        n += v / (hmin * hmin * hmin);
    }
    return n + 64.0;
}

int64_t Domain::octSubCellAt(double x, double y, double z) {
    int32_t c = octree->leafAt(x, y, z);
    const poi_octree::Cell &cl = octree->cells()[c];
    int32_t L = octOrd[c];
    int m = octM[L];
    double mn[3];
    octree->cellMin(cl, mn);
    double u = octree->cellSize(cl.level) / m;
    double p[3] = {x, y, z};
    int a[3];
    for (int d = 0; d < 3; d++) a[d] = std::max(0, std::min(m - 1, int(std::floor((p[d] - mn[d]) / u))));
    return octOff[L] + a[0] + m * (a[1] + int64_t(m) * a[2]);
}

void Domain::octSubCellMin(int64_t sc, double mn[3], double &size) {
    int32_t L = int32_t(std::upper_bound(octOff.begin(), octOff.end(), sc) - octOff.begin()) - 1;
    const poi_octree::Cell &cl = octree->cells()[octLeaf[L]];
    int m = octM[L];
    int64_t r = sc - octOff[L];
    int a = int(r % m), b = int((r / m) % m), c = int(r / (int64_t(m) * m));
    octree->cellMin(cl, mn);
    size = octree->cellSize(cl.level) / m;
    mn[0] += a * size;
    mn[1] += b * size;
    mn[2] += c * size;
}

// every point that may reach the box c0..c1 (as nodeNum, appended to
// out): the points in the sub-cells that meet the box grown by R, with R
// at least the largest h of the leaves that meet the grown box (r <= h)
void Domain::octGather(const double c0[3], const double c1[3], double R, std::vector<int> &out) {
    octSearchLeaves(c0, c1, R);
    gatherLeaves(c0, c1, out);
}

// the leaves that meet the box c0..c1 grown by R, with R grown to the
// largest h of those leaves (into octScratch, R into octSearchR)
// (c0..c1 inside one leaf: R is at least that leaf's octLeafReach, so
// one search does)
void Domain::octSearchLeaves(const double c0[3], const double c1[3], double R) {
    double lo[3], hi[3];
    R = std::max(R, double(octLeafReach[octOrd[octree->leafAt(c0[0], c0[1], c0[2])]]));
    for (int d = 0; d < 3; d++) { lo[d] = c0[d] - R; hi[d] = c1[d] + R; }
    octree->leavesInBox(lo, hi, octScratch);
    octSearchR = R;
}

// for each leaf, the smallest R (from its own largest h up) for which no
// leaf meeting the leaf grown by R has a larger h than R
void Domain::octInitReach() {
    const std::vector<poi_octree::Cell> &C = octree->cells();
    octLeafReach.assign(octLeaf.size(), 0.0f);
    std::vector<int32_t> lv;
    for (std::size_t L = 0; L < octLeaf.size(); L++) {
        const poi_octree::Cell &cl = C[octLeaf[L]];
        double mn[3], s = octree->cellSize(cl.level), R = octLeafHmax[L], lo[3], hi[3];
        octree->cellMin(cl, mn);
        for (int it = 0; it < 50; it++) {
            for (int d = 0; d < 3; d++) { lo[d] = mn[d] - R; hi[d] = mn[d] + s + R; }
            octree->leavesInBox(lo, hi, lv);
            double hm = 0.0;
            for (int32_t c : lv) hm = std::max(hm, double(octLeafHmax[octOrd[c]]));
            if (hm <= R) break;
            R = hm;
        }
        octLeafReach[L] = float(R * (1.0 + 1e-6));
    }
}

void Domain::gatherLeaves(const double c0[3], const double c1[3], std::vector<int> &out) {
    double R = octSearchR, lo[3], hi[3];
    for (int d = 0; d < 3; d++) { lo[d] = c0[d] - R; hi[d] = c1[d] + R; }
    const std::vector<int32_t> &scratch = octScratch;
    const poi_octree::Octree &T = *octree;
    const std::vector<int32_t> &ord = octOrd, &head = octHead, &next = octNext;
    const std::vector<int64_t> &off = octOff;
    const std::vector<uint8_t> &M = octM;
    const std::vector<poi_octree::Cell> &C = T.cells();
    for (int32_t c : scratch) {
        int32_t L = ord[c];
        int m = M[L];
        double mn[3];
        T.cellMin(C[c], mn);
        double u = T.cellSize(C[c].level) / m;
        int a0[3], a1[3];
        for (int d = 0; d < 3; d++) {
            a0[d] = std::max(0, int(std::floor((lo[d] - mn[d]) / u)));
            a1[d] = std::min(m - 1, int(std::floor((hi[d] - mn[d]) / u)));
        }
        for (int k = a0[2]; k <= a1[2]; k++)
            for (int j = a0[1]; j <= a1[1]; j++)
                for (int i = a0[0]; i <= a1[0]; i++)
                    for (int32_t n = head[off[L] + i + m * (j + int64_t(m) * k)]; n > 0; n = next[n - 1])
                        out.push_back(n);
    }
}

std::vector<int> Domain::octGetNeighborCellsRadius(const Point &point) {
    std::vector<int> nodeIDs;
    double c[3] = {point.x, point.y, point.z};
    octGather(c, c, point.radius, nodeIDs);
    return nodeIDs;  // each point is in one sub-cell: no duplicates
}

// check 4 of testCandidate on the octree: false at the first accepted
// point closer than max(r, its r)
bool Domain::octDiskFree(const Point &x) {
    double c[3] = {x.x, x.y, x.z};
    octSearchLeaves(c, c, x.radius);
    double R = octSearchR;
    const std::vector<poi_octree::Cell> &C = octree->cells();
    double r2 = x.radius * x.radius;
    for (int32_t cl : octScratch) {
        int32_t L = octOrd[cl];
        int m = octM[L];
        double mn[3];
        octree->cellMin(C[cl], mn);
        double u = octree->cellSize(C[cl].level) / m;
        int a0[3], a1[3];
        for (int d = 0; d < 3; d++) {
            a0[d] = std::max(0, int(std::floor((c[d] - R - mn[d]) / u)));
            a1[d] = std::min(m - 1, int(std::floor((c[d] + R - mn[d]) / u)));
        }
        for (int k = a0[2]; k <= a1[2]; k++)
            for (int j = a0[1]; j <= a1[1]; j++)
                for (int i = a0[0]; i <= a1[0]; i++)
                    for (int32_t n = octHead[octOff[L] + i + m * (j + int64_t(m) * k)]; n > 0; n = octNext[n - 1]) {
                        const Point &q = nodes[n - 1];
                        double dx = x.x - q.x, dy = x.y - q.y, dz = x.z - q.z;
                        double d2 = dx * dx + dy * dy + dz * dz;
                        if (d2 < r2 || d2 < q.radius * q.radius) return false;
                    }
    }
    return true;
}

// the list replaces the hex grid's tags: a point goes into the sub-cell
// it is in, once (it may be called before the point is in nodes, and
// again for a point already listed)
void Domain::octTagNeighborCells(const Point &point) {
    std::size_t n = std::size_t(point.nodeNum);
    if (octNext.size() < n) octNext.resize(std::max(n, 2 * octNext.size()), -1);
    if (octNext[n - 1] != -1) return;  // already listed (-1: not listed, 0: last)
    int64_t sc = octSubCellAt(point.x, point.y, point.z);
    octNext[n - 1] = octHead[sc];
    octHead[sc] = int32_t(n);
}

bool Domain::octOwnCellRejects(const Point &point) {
    for (int32_t n = octHead[octSubCellAt(point.x, point.y, point.z)]; n > 0; n = octNext[n - 1])
        if (distance3D(point, nodes[n - 1]) < nodes[n - 1].radius) return true;
    return false;
}

// The points whose disks can reach the sub-cell sc (min corner mn, size u)
void Domain::octNearSubCell(int64_t sc, const double mn[3], double u, std::vector<int> &out) {
    int32_t L = int32_t(std::upper_bound(octOff.begin(), octOff.end(), sc) - octOff.begin()) - 1;
    double c1[3] = {mn[0] + u, mn[1] + u, mn[2] + u};
    out.clear();
    octGather(mn, c1, octLeafHmax[L], out);
}

// h at (x, y, z) in leaf L (trilinear in its corner values)
double Domain::octLeafH(int32_t L, double x, double y, double z) {
    const poi_octree::Cell &cl = octree->cells()[octLeaf[L]];
    double mn[3], s = octree->cellSize(cl.level);
    octree->cellMin(cl, mn);
    double t[3] = {(x - mn[0]) / s, (y - mn[1]) / s, (z - mn[2]) / s};
    for (int d = 0; d < 3; d++) t[d] = std::min(1.0, std::max(0.0, t[d]));
    double v = 0.0;
    for (int q = 0; q < 8; q++) {
        double w = ((q & 1) ? t[0] : 1 - t[0]) * (((q >> 1) & 1) ? t[1] : 1 - t[1]) *
                   (((q >> 2) & 1) ? t[2] : 1 - t[2]);
        v += w * octree->vertexH(cl.corner[q]);
    }
    return v;
}

// true if one accepted point q provably rejects every candidate in the
// box lo..hi (inside leaf L): a candidate x is rejected when
// |x - q| < max(h(x), r_q), h(x) >= the smallest h at the box corners
// (h is trilinear in the leaf), and the box is inside a ball when its 8
// corners are
bool Domain::octBoxCovered(int32_t L, const double lo[3], const double hi[3], const std::vector<int> &near) {
    double c[8][3], hmin = 1e300;
    for (int q = 0; q < 8; q++) {
        c[q][0] = (q & 1) ? hi[0] : lo[0];
        c[q][1] = (q & 2) ? hi[1] : lo[1];
        c[q][2] = (q & 4) ? hi[2] : lo[2];
        hmin = std::min(hmin, octLeafH(L, c[q][0], c[q][1], c[q][2]));
    }
    for (int n : near) {
        const Point &p = nodes[n - 1];
        double D = std::max(p.radius, hmin), D2 = D * D;
        bool all = true;
        for (int q = 0; q < 8 && all; q++) {
            double dx = c[q][0] - p.x, dy = c[q][1] - p.y, dz = c[q][2] - p.z;
            all = dx * dx + dy * dy + dz * dz < D2;
        }
        if (all) return true;
    }
    return false;
}

// testCandidate (checks 1 and 4, radius scale 1) against the near list
// of the sub-cell x is in (leaf L): that list holds every point that can
// reject x. Sets x.radius.
bool Domain::octLocalAccept(Point &x, int32_t L, const std::vector<int> &near) {
    if (!inBoundingBox(x)) return false;
    double hx = octLeafH(L, x.x, x.y, x.z), h2 = hx * hx;
    for (int n : near) {
        const Point &p = nodes[n - 1];
        double dx = x.x - p.x, dy = x.y - p.y, dz = x.z - p.z, d2 = dx * dx + dy * dy + dz * dz;
        if (d2 < h2 || d2 < p.radius * p.radius) return false;
    }
    x.radius = hx;
    return true;
}

// the sub-cell sc clipped to the box (false if outside it), and its leaf
bool Domain::octSubCellBox(int64_t sc, double lo[3], double hi[3], int32_t &L) {
    double mn[3], u;
    octSubCellMin(sc, mn, u);
    L = int32_t(std::upper_bound(octOff.begin(), octOff.end(), sc) - octOff.begin()) - 1;
    double bmin[3] = {xMin, yMin, zMin}, bmax[3] = {xMax, yMax, zMax};
    for (int d = 0; d < 3; d++) {
        lo[d] = std::max(bmin[d], mn[d]);
        hi[d] = std::min(bmax[d], mn[d] + u);
        if (lo[d] > hi[d]) return false;
    }
    return true;
}

void Domain::octFindEmptyCells() {
    cout << "Finding Empty Cells (h_lookup octree)" << endl;
    octEmpty.clear();
    double blo[3] = {xMin, yMin, zMin}, bhi[3] = {xMax, yMax, zMax};
    std::vector<int32_t> leaves;
    octree->leavesInBox(blo, bhi, leaves);
    std::vector<std::pair<std::array<double, 3>, int64_t>> key;
    std::vector<int> near;
    for (int32_t c : leaves) {
        int32_t L = octOrd[c];
        int m = octM[L];
        for (int k = 0; k < m * m * m; k++) {
            int64_t sc = octOff[L] + k;
            double lo[3], hi[3];
            int32_t LL;
            if (!octSubCellBox(sc, lo, hi, LL)) continue;
            double mn[3], u;
            octSubCellMin(sc, mn, u);
            octNearSubCell(sc, mn, u, near);
            if (!octBoxCovered(L, lo, hi, near)) key.push_back({{mn[2], mn[1], mn[0]}, sc});
        }
    }
    // one raster order over the whole box (x fastest, z slowest), as the
    // hex grid path visits its cells
    std::sort(key.begin(), key.end());
    octEmpty.reserve(key.size());
    for (auto &e : key) octEmpty.push_back(e.second);
    cout << "There are " << octEmpty.size() << " empty cells" << endl;
}

// Maximal sampling in the spirit of Ebeida et al. (2011, 2012): a box is
// dropped only when one point provably covers it; otherwise numSamples
// darts are thrown into it, and if none is accepted it is split in 8,
// down to OCT_SMALLEST_BOX times its smallest h. The boxes still open at
// that size are counted: they bound what an exhaustive search could add.
unsigned int Domain::octFillEmptyCells() {
    Point newPoint;
    unsigned int newPoints = 0;
    cout << "Filling empty cells in the background grid (h_lookup octree)" << endl;
    cout << "\tCell#\t\tTotal Cells\tPercentage" << endl;
    double currentPercentage = 0;
    std::size_t n = octEmpty.size();
    std::vector<int> near;
    struct Box { double lo[3], hi[3]; };
    std::vector<Box> stack;
    long left = 0, boxes = 0;
    double leftVol = 0.0;
    for (std::size_t e = 0; e < n; e++) {
        double percentage = 100.0 * double(e) / double(n);
        if (percentage > currentPercentage) {
            currentPercentage += 5;
            cout << "\t" << e << "\t\t" << n << "\t\t" << std::setprecision(3) << percentage << " %" << endl;
        }
        int64_t sc = octEmpty[e];
        Box b0;
        int32_t L;
        if (!octSubCellBox(sc, b0.lo, b0.hi, L)) continue;
        double mn[3], u;
        octSubCellMin(sc, mn, u);
        octNearSubCell(sc, mn, u, near);
        stack.clear();
        stack.push_back(b0);
        while (!stack.empty()) {
            Box b = stack.back();
            stack.pop_back();
            boxes++;
            if (octBoxCovered(L, b.lo, b.hi, near)) continue;
            bool hit = false;
            for (unsigned int count = 0; count < numSamples && !hit; count++) {
                newPoint.x = b.lo[0] + uniformDistribution() * (b.hi[0] - b.lo[0]);
                newPoint.y = b.lo[1] + uniformDistribution() * (b.hi[1] - b.lo[1]);
                newPoint.z = b.lo[2] + uniformDistribution() * (b.hi[2] - b.lo[2]);
                if (octLocalAccept(newPoint, L, near)) {
                    numNodes++;
                    newPoints++;
                    newPoint.nodeNum = numNodes;
                    nodes.push_back(newPoint);
                    tagNeighborCells(newPoint);
                    near.push_back(int(numNodes));
                    hit = true;
                }
            }
            if (hit) {
                stack.push_back(b);  // look at what is left of it
                continue;
            }
            double side = std::max(b.hi[0] - b.lo[0], std::max(b.hi[1] - b.lo[1], b.hi[2] - b.lo[2]));
            double hb = octLeafH(L, 0.5 * (b.lo[0] + b.hi[0]), 0.5 * (b.lo[1] + b.hi[1]), 0.5 * (b.lo[2] + b.hi[2]));
            if (side <= OCT_SMALLEST_BOX * hb) {
                // last tries: its centre and its corners
                bool got = false;
                for (int q = 0; q < 9 && !got; q++) {
                    newPoint.x = q == 8 ? 0.5 * (b.lo[0] + b.hi[0]) : ((q & 1) ? b.hi[0] : b.lo[0]);
                    newPoint.y = q == 8 ? 0.5 * (b.lo[1] + b.hi[1]) : ((q & 2) ? b.hi[1] : b.lo[1]);
                    newPoint.z = q == 8 ? 0.5 * (b.lo[2] + b.hi[2]) : ((q & 4) ? b.hi[2] : b.lo[2]);
                    got = octLocalAccept(newPoint, L, near);
                }
                if (got) {
                    numNodes++;
                    newPoints++;
                    newPoint.nodeNum = numNodes;
                    nodes.push_back(newPoint);
                    tagNeighborCells(newPoint);
                    near.push_back(int(numNodes));
                    stack.push_back(b);
                    continue;
                }
                left++;
                leftVol += (b.hi[0] - b.lo[0]) * (b.hi[1] - b.lo[1]) * (b.hi[2] - b.lo[2]);
                continue;
            }
            double mid[3];
            for (int d = 0; d < 3; d++) mid[d] = 0.5 * (b.lo[d] + b.hi[d]);
            for (int q = 0; q < 8; q++) {
                Box c;
                for (int d = 0; d < 3; d++) {
                    bool up = (q >> d) & 1;
                    c.lo[d] = up ? mid[d] : b.lo[d];
                    c.hi[d] = up ? b.hi[d] : mid[d];
                }
                stack.push_back(c);
            }
        }
    }
    double vol = (xMax - xMin) * (yMax - yMin) * (zMax - zMin);
    cout << newPoints << " out of " << n << " empty cells have been filled" << endl;
    cout << "  maximality: " << boxes << " boxes looked at; any vertex that could" << endl;
    cout << "    still be added is in the " << left << " boxes left, each at most "
         << OCT_SMALLEST_BOX << " h" << endl;
    cout << "    (volume fraction " << leftVol / vol << ")" << endl;
    cout << "Filling empty cells in the background grid complete\n" << endl;
    return newPoints;
}
