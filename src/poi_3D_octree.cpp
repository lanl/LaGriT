/*
 * poi_3D_octree.cpp -- see poi_3D_octree.h.
 *
 * Build:
 *   1. level 0: cubic cells over the lookup box, rootsPerSide along its
 *      longest side (the domain is the box grown to whole cells);
 *   2. h at every vertex that has none yet, through the callback, in
 *      batches;
 *   3. split every leaf that the rule asks for (smallest corner h <
 *      K * size, above the finest level), and every leaf that a finer
 *      face neighbour two or more levels down forces (2:1 balance);
 *   4. back to 2 until a pass splits nothing.
 * Vertex positions are integers at the finest level, so a vertex shared
 * by several cells (hanging ones too) is found by its key.
 */
#include "poi_3D_octree.h"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <unordered_map>

namespace poi_octree {

typedef std::unordered_map<uint64_t, int32_t> KeyMap;

static inline uint64_t packKey(int64_t I, int64_t J, int64_t K) {
    return (uint64_t(I) << 42) | (uint64_t(J) << 21) | uint64_t(K);
}
static inline void unpackKey(uint64_t key, int64_t &I, int64_t &J, int64_t &K) {
    I = int64_t(key >> 42);
    J = int64_t((key >> 21) & 0x1FFFFF);
    K = int64_t(key & 0x1FFFFF);
}

int32_t Octree::vertexAt(int64_t I, int64_t J, int64_t K, bool &is_new) {
    KeyMap &m = *static_cast<KeyMap *>(map_);
    uint64_t key = packKey(I, J, K);
    auto it = m.find(key);
    if (it != m.end()) { is_new = false; return it->second; }
    int32_t v = int32_t(key_.size());
    key_.push_back(key);
    h_.push_back(0.0);
    m.emplace(key, v);
    pending_.push_back(v);
    is_new = true;
    return v;
}

void Octree::vertexXYZ(int32_t v, double xyz[3]) const {
    int64_t I, J, K;
    unpackKey(key_[v], I, J, K);
    double u = s0_ / double(int64_t(1) << lmax_);
    xyz[0] = o_[0] + double(I) * u;
    xyz[1] = o_[1] + double(J) * u;
    xyz[2] = o_[2] + double(K) * u;
}

void Octree::cellMin(const Cell &c, double xyz[3]) const {
    double s = cellSize(c.level);
    xyz[0] = o_[0] + c.i * s;
    xyz[1] = o_[1] + c.j * s;
    xyz[2] = o_[2] + c.k * s;
}

int Octree::evaluatePending(HFunc &h_at) {
    std::vector<double> x, y, z, h;
    std::size_t done = 0;
    while (done < pending_.size()) {
        std::size_t n = std::min(p_.batch, pending_.size() - done);
        x.resize(n); y.resize(n); z.resize(n); h.resize(n);
        for (std::size_t q = 0; q < n; q++) {
            double xyz[3];
            vertexXYZ(pending_[done + q], xyz);
            x[q] = xyz[0]; y[q] = xyz[1]; z[q] = xyz[2];
        }
        if (h_at(n, x.data(), y.data(), z.data(), h.data()) != 0) {
            err_ = "the h callback failed";
            return 1;
        }
        for (std::size_t q = 0; q < n; q++) {
            if (!(h[q] > 0.0)) {
                char b[160];
                std::snprintf(b, sizeof b, "h = %g at (%g, %g, %g): h must be > 0", h[q], x[q], y[q], z[q]);
                err_ = b;
                return 1;
            }
            h_[pending_[done + q]] = h[q];
        }
        stats_.evaluations += n;
        done += n;
    }
    pending_.clear();
    return 0;
}

bool Octree::wantsRule(const Cell &c) const {
    if (c.level >= lmax_) return false;
    double hmin = h_[c.corner[0]];
    for (int q = 1; q < 8; q++) hmin = std::min(hmin, h_[c.corner[q]]);
    return hmin < p_.kfactor * cellSize(c.level);
}

void Octree::split(int32_t ci) {
    // the 27 lattice points of the parent at half its size
    Cell parent = cells_[ci];
    int L = parent.level + 1;
    int64_t m = unitSize(L);
    int64_t I0 = int64_t(parent.i) * 2 * m, J0 = int64_t(parent.j) * 2 * m, K0 = int64_t(parent.k) * 2 * m;
    int32_t v[3][3][3];
    for (int a = 0; a < 3; a++)
        for (int b = 0; b < 3; b++)
            for (int c = 0; c < 3; c++) {
                int corner = (a / 2) + 2 * (b / 2) + 4 * (c / 2);
                if (a != 1 && b != 1 && c != 1) { v[a][b][c] = parent.corner[corner]; continue; }
                bool is_new;
                v[a][b][c] = vertexAt(I0 + a * m, J0 + b * m, K0 + c * m, is_new);
            }
    int32_t first = int32_t(cells_.size());
    cells_[ci].first_child = first;
    for (int dz = 0; dz < 2; dz++)
        for (int dy = 0; dy < 2; dy++)
            for (int dx = 0; dx < 2; dx++) {
                Cell ch;
                ch.first_child = -1;
                ch.level = int8_t(L);
                ch.i = parent.i * 2 + dx;
                ch.j = parent.j * 2 + dy;
                ch.k = parent.k * 2 + dz;
                for (int cz = 0; cz < 2; cz++)
                    for (int cy = 0; cy < 2; cy++)
                        for (int cx = 0; cx < 2; cx++)
                            ch.corner[cx + 2 * cy + 4 * cz] = v[dx + cx][dy + cy][dz + cz];
                cells_.push_back(ch);
            }
}

int32_t Octree::leafAtUnit(int64_t I, int64_t J, int64_t K) const {
    int64_t F = int64_t(1) << lmax_;
    if (I < 0 || J < 0 || K < 0 || I >= n0_[0] * F || J >= n0_[1] * F || K >= n0_[2] * F) return -1;
    int32_t c = int32_t((I / F) + n0_[0] * ((J / F) + n0_[1] * (K / F)));
    while (cells_[c].first_child >= 0) {
        int L = cells_[c].level + 1;
        int64_t m = unitSize(L);
        int dx = int((I / m) & 1), dy = int((J / m) & 1), dz = int((K / m) & 1);
        c = cells_[c].first_child + dx + 2 * dy + 4 * dz;
    }
    return c;
}

int Octree::build(const Params &p, HFunc h_at) {
    p_ = p;
    if (p_.rootsPerSide < 1) p_.rootsPerSide = 4;
    if (p_.batch < 1) p_.batch = 1000000;
    double L[3] = {p.box[1] - p.box[0], p.box[3] - p.box[2], p.box[5] - p.box[4]};
    double Lmax = std::max(L[0], std::max(L[1], L[2]));
    if (!(Lmax > 0) || !(p.kfactor > 0) || !(p.finest > 0)) { err_ = "bad box, K or finest size"; return 1; }
    s0_ = Lmax / p_.rootsPerSide;
    for (int a = 0; a < 3; a++) {
        n0_[a] = std::max(1, int(std::ceil(L[a] / s0_ - 1e-9)));
        o_[a] = p.box[2 * a];
    }
    lmax_ = std::max(0, int(std::floor(std::log2(s0_ / p.finest) + 1e-9)));
    if ((int64_t(n0_[0]) << lmax_) >= (1 << 21) || (int64_t(n0_[1]) << lmax_) >= (1 << 21) ||
        (int64_t(n0_[2]) << lmax_) >= (1 << 21) || lmax_ > 30) {
        err_ = "too many levels for the vertex keys";
        return 1;
    }
    KeyMap map;
    map_ = &map;
    stats_ = Stats();
    cells_.clear(); key_.clear(); h_.clear(); pending_.clear();
    int64_t F = int64_t(1) << lmax_;
    for (int k = 0; k < n0_[2]; k++)
        for (int j = 0; j < n0_[1]; j++)
            for (int i = 0; i < n0_[0]; i++) {
                Cell c;
                c.first_child = -1; c.level = 0; c.i = i; c.j = j; c.k = k;
                for (int q = 0; q < 8; q++) {
                    bool is_new;
                    c.corner[q] = vertexAt((i + (q & 1)) * F, (j + ((q >> 1) & 1)) * F, (k + ((q >> 2) & 1)) * F, is_new);
                }
                cells_.push_back(c);
            }
    int rc = 0;
    for (;;) {
        if ((rc = evaluatePending(h_at)) != 0) break;
        stats_.passes++;
        std::vector<char> want(cells_.size(), 0);
        std::size_t nrule = 0, nbal = 0;
        for (std::size_t c = 0; c < cells_.size(); c++)
            if (cells_[c].first_child < 0 && wantsRule(cells_[c])) { want[c] = 1; nrule++; }
        // 2:1 balance: a leaf whose face neighbour is a leaf two or more
        // levels coarser forces that neighbour to split; a leaf about to
        // split counts one level finer.
        for (std::size_t c = 0; c < cells_.size(); c++) {
            const Cell &cl = cells_[c];
            if (cl.first_child >= 0) continue;
            int Leff = cl.level + (want[c] ? 1 : 0);
            int64_t m = unitSize(cl.level);
            int64_t I0 = int64_t(cl.i) * m, J0 = int64_t(cl.j) * m, K0 = int64_t(cl.k) * m, hm = m / 2;
            int64_t probes[6][3] = {{I0 - 1, J0 + hm, K0 + hm}, {I0 + m, J0 + hm, K0 + hm},
                                    {I0 + hm, J0 - 1, K0 + hm}, {I0 + hm, J0 + m, K0 + hm},
                                    {I0 + hm, J0 + hm, K0 - 1}, {I0 + hm, J0 + hm, K0 + m}};
            for (int f = 0; f < 6; f++) {
                int32_t nb = leafAtUnit(probes[f][0], probes[f][1], probes[f][2]);
                if (nb < 0 || want[nb]) continue;
                if (Leff - cells_[nb].level >= 2) { want[nb] = 1; nbal++; }
            }
        }
        if (nrule + nbal == 0) break;
        stats_.split_rule += nrule;
        stats_.split_balance += nbal;
        std::size_t ncell = cells_.size();
        cells_.reserve(ncell + 8 * (nrule + nbal));
        for (std::size_t c = 0; c < ncell; c++)
            if (want[c]) split(int32_t(c));
    }
    map_ = nullptr;
    if (rc != 0) return rc;
    stats_.cells = cells_.size();
    stats_.vertices = h_.size();
    stats_.leaves_per_level.assign(lmax_ + 1, 0);
    stats_.leaves = 0;
    for (const Cell &c : cells_)
        if (c.first_child < 0) { stats_.leaves++; stats_.leaves_per_level[c.level]++; }
    stats_.levels = lmax_ + 1;
    return 0;
}

int Octree::leafAt(double x, double y, double z) const {
    double u = s0_ / double(int64_t(1) << lmax_);
    int64_t F = int64_t(1) << lmax_;
    double p[3] = {x, y, z};
    int64_t q[3];
    for (int a = 0; a < 3; a++) {
        double f = (p[a] - o_[a]) / u;
        int64_t hi = n0_[a] * F - 1;
        q[a] = f <= 0 ? 0 : (f >= double(hi) ? hi : int64_t(std::floor(f)));
    }
    return leafAtUnit(q[0], q[1], q[2]);
}

double Octree::lookupH(double x, double y, double z) const {
    int32_t c = leafAt(x, y, z);
    if (c < 0) return 0.0;
    const Cell &cl = cells_[c];
    double mn[3];
    cellMin(cl, mn);
    double s = cellSize(cl.level);
    double t[3] = {(x - mn[0]) / s, (y - mn[1]) / s, (z - mn[2]) / s};
    for (int a = 0; a < 3; a++) t[a] = std::min(1.0, std::max(0.0, t[a]));
    double v = 0.0;
    for (int q = 0; q < 8; q++) {
        double w = ((q & 1) ? t[0] : 1 - t[0]) * (((q >> 1) & 1) ? t[1] : 1 - t[1]) *
                   (((q >> 2) & 1) ? t[2] : 1 - t[2]);
        v += w * h_[cl.corner[q]];
    }
    return v;
}

void Octree::leavesInBox(const double lo[3], const double hi[3], std::vector<int32_t> &out) const {
    out.clear();
    // the box in finest-level units, then down from the roots it meets,
    // with integer overlap tests (closed box)
    double u = s0_ / double(int64_t(1) << lmax_);
    int64_t F = int64_t(1) << lmax_, L[3], H[3];
    int r0[3], r1[3];
    for (int a = 0; a < 3; a++) {
        double fl = std::floor((lo[a] - o_[a]) / u), fh = std::floor((hi[a] - o_[a]) / u);
        double top = double(n0_[a] * F);
        if (fh < 0 || fl > top) return;
        L[a] = int64_t(std::max(0.0, fl));
        H[a] = int64_t(std::min(top, fh));
        r0[a] = int(std::min<int64_t>(n0_[a] - 1, std::max<int64_t>(0, (L[a] - 1) / F)));
        r1[a] = int(std::min<int64_t>(n0_[a] - 1, H[a] / F));
    }
    static thread_local std::vector<int32_t> stack;
    stack.clear();
    for (int k = r0[2]; k <= r1[2]; k++)
        for (int j = r0[1]; j <= r1[1]; j++)
            for (int i = r0[0]; i <= r1[0]; i++) stack.push_back(int32_t(i + n0_[0] * (j + n0_[1] * k)));
    while (!stack.empty()) {
        int32_t c = stack.back();
        stack.pop_back();
        const Cell &cl = cells_[c];
        if (cl.first_child < 0) { out.push_back(c); continue; }
        int64_t us = unitSize(cl.level + 1);
        for (int q = 0; q < 8; q++) {
            const Cell &ch = cells_[cl.first_child + q];
            int64_t I = ch.i * us, J = ch.j * us, K = ch.k * us;
            if (I <= H[0] && I + us >= L[0] && J <= H[1] && J + us >= L[1] && K <= H[2] && K + us >= L[2])
                stack.push_back(cl.first_child + q);
        }
    }
}

int Octree::dumpAVS(const std::string &file) const {
    FILE *f = std::fopen(file.c_str(), "w");
    if (!f) return 1;
    std::size_t nleaf = 0;
    for (const Cell &c : cells_) nleaf += c.first_child < 0;
    // only the vertices the leaves use (parents' ones are all leaf corners too)
    std::vector<int32_t> num(h_.size(), 0);
    int32_t nv = 0;
    for (const Cell &c : cells_)
        if (c.first_child < 0)
            for (int q = 0; q < 8; q++)
                if (num[c.corner[q]] == 0) num[c.corner[q]] = -1;
    for (std::size_t v = 0; v < h_.size(); v++)
        if (num[v] == -1) num[v] = ++nv;
    std::fprintf(f, "%10d %10zu %10d %10d %10d\n", nv, nleaf, 1, 1, 0);
    for (std::size_t v = 0; v < h_.size(); v++) {
        if (num[v] <= 0) continue;
        double xyz[3];
        vertexXYZ(int32_t(v), xyz);
        std::fprintf(f, "%010d %21.12E %21.12E %21.12E\n", num[v], xyz[0], xyz[1], xyz[2]);
    }
    // LaGriT hex order: top face (z = 1) first, then the bottom face
    static const int order[8] = {4, 5, 7, 6, 0, 1, 3, 2};
    std::size_t e = 0;
    for (const Cell &c : cells_) {
        if (c.first_child >= 0) continue;
        std::fprintf(f, "%010zu 1 hex", ++e);
        for (int q = 0; q < 8; q++) std::fprintf(f, " %d", num[c.corner[order[q]]]);
        std::fprintf(f, "\n");
    }
    std::fprintf(f, "00001  1\nh_field_att, real\n");
    for (std::size_t v = 0; v < h_.size(); v++)
        if (num[v] > 0) std::fprintf(f, "%010d %21.12E\n", num[v], h_[v]);
    std::fprintf(f, "00001  1\nitetlev, integer\n");
    e = 0;
    for (const Cell &c : cells_)
        if (c.first_child < 0) std::fprintf(f, "%010zu %d\n", ++e, int(c.level));
    std::fclose(f);
    return 0;
}

std::size_t Octree::check(std::string &report) const {
    std::size_t bad_rule = 0, bad_bal = 0, nleaf = 0;
    double vol = 0.0;
    for (const Cell &c : cells_) {
        if (c.first_child >= 0) continue;
        nleaf++;
        double s = cellSize(c.level);
        vol += s * s * s;
        if (wantsRule(c)) bad_rule++;
        int64_t m = unitSize(c.level);
        int64_t I0 = int64_t(c.i) * m, J0 = int64_t(c.j) * m, K0 = int64_t(c.k) * m, hm = std::max<int64_t>(m / 2, 0);
        int64_t probes[6][3] = {{I0 - 1, J0 + hm, K0 + hm}, {I0 + m, J0 + hm, K0 + hm},
                                {I0 + hm, J0 - 1, K0 + hm}, {I0 + hm, J0 + m, K0 + hm},
                                {I0 + hm, J0 + hm, K0 - 1}, {I0 + hm, J0 + hm, K0 + m}};
        for (int f = 0; f < 6; f++) {
            int32_t nb = leafAtUnit(probes[f][0], probes[f][1], probes[f][2]);
            if (nb >= 0 && std::abs(int(cells_[nb].level) - int(c.level)) > 1) { bad_bal++; break; }
        }
    }
    double dom = s0_ * s0_ * s0_ * n0_[0] * n0_[1] * n0_[2];
    std::size_t bad_vol = std::fabs(vol - dom) > 1e-9 * dom ? 1 : 0;
    char b[256];
    std::snprintf(b, sizeof b, "leaves %zu, leaf volume / domain %.12f, leaves breaking the rule %zu, "
                  "leaves with a face neighbour more than one level away %zu", nleaf, vol / dom, bad_rule, bad_bal);
    report = b;
    return bad_rule + bad_bal + bad_vol;
}

std::size_t Octree::bytesUsed() const {
    return cells_.capacity() * sizeof(Cell) + key_.capacity() * sizeof(uint64_t) +
           h_.capacity() * sizeof(double);
}

}  // namespace poi_octree
