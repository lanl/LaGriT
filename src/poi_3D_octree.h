/*
 * poi_3D_octree.h -- an h-adapted octree for createpts / poisson_disk 3D
 * (h_lookup octree). It replaces the uniform h lookup lattice: a leaf is
 * split while the smallest h at its 8 corners is less than K times its
 * size (leaf size <= h / K), down to a finest size of about
 * h_spacing_scalar / 1.5. h at the vertices comes from a callback, in
 * batches; in LaGriT the callback runs the user's h-field .mlgi on a
 * vertex-only mesh object. h must be a pointwise function h(x, y, z)
 * that changes slowly (|grad h| well below K), as Poisson disk sampling
 * needs anyway.
 *
 * Plain arrays; about 52 bytes per cell (leaves and parents) and 16 per
 * vertex once built.
 */
#ifndef POI_3D_OCTREE_H
#define POI_3D_OCTREE_H

#include <cstdint>
#include <functional>
#include <string>
#include <vector>

namespace poi_octree {

struct Params {
    double box[6];          // xmin xmax ymin ymax zmin zmax (the lookup box)
    double kfactor;         // leaf size <= h / kfactor
    double finest;          // no leaf smaller than this (h_spacing_scalar / 1.5)
    int    rootsPerSide;    // level-0 cells along the longest side (default 4)
    std::size_t batch;      // most vertices per callback
};

// h at n points: x, y, z in, h out (all arrays of length n)
typedef std::function<int(std::size_t n, const double *x, const double *y,
                          const double *z, double *h)> HFunc;

struct Cell {
    int32_t first_child;    // -1 for a leaf; children are consecutive
    int8_t  level;
    int32_t i, j, k;        // position at its own level
    int32_t corner[8];      // vertex index, corner c = dx + 2 dy + 4 dz
};

struct Stats {
    std::size_t cells = 0, leaves = 0, vertices = 0, evaluations = 0;
    int passes = 0, levels = 0;
    std::vector<std::size_t> leaves_per_level;
    std::size_t split_rule = 0, split_balance = 0;
};

class Octree {
public:
    // Builds the tree. Returns 0, or nonzero if the callback failed or a
    // value of h was not positive (message in error()).
    int build(const Params &p, HFunc h_at);

    double lookupH(double x, double y, double z) const;
    // the leaves (cell indices) that meet the box lo..hi (closed)
    void leavesInBox(const double lo[3], const double hi[3], std::vector<int32_t> &out) const;
    int    leafAt(double x, double y, double z) const;  // cell index, -1 outside

    // cells, vertices
    const std::vector<Cell> &cells() const { return cells_; }
    double cellSize(int level) const { return s0_ / double(1 << level); }
    void   cellMin(const Cell &c, double xyz[3]) const;
    void   vertexXYZ(int32_t v, double xyz[3]) const;
    double vertexH(int32_t v) const { return h_[v]; }
    std::size_t numVertices() const { return h_.size(); }
    const Stats &stats() const { return stats_; }
    const std::string &error() const { return err_; }
    double origin(int axis) const { return o_[axis]; }
    int    roots(int axis) const { return n0_[axis]; }
    int    maxLevel() const { return lmax_; }

    // The leaves as an AVS UCD hex mesh (LaGriT order: top face first),
    // with h_field_att at the vertices and itetlev at the cells.
    int dumpAVS(const std::string &file) const;

    // Self checks: leaves tile the domain once; every leaf meets the
    // rule or is at the finest level; face neighbours differ by at most
    // one level. Returns the number of problems (0 = all good).
    std::size_t check(std::string &report) const;

    std::size_t bytesUsed() const;

private:
    int32_t vertexAt(int64_t I, int64_t J, int64_t K, bool &is_new);
    int  evaluatePending(HFunc &h_at);
    void split(int32_t c);
    bool wantsRule(const Cell &c) const;
    int32_t leafAtUnit(int64_t I, int64_t J, int64_t K) const;
    int64_t unitSize(int level) const { return int64_t(1) << (lmax_ - level); }

    Params p_;
    double o_[3] = {0, 0, 0}, s0_ = 0;
    int n0_[3] = {0, 0, 0}, lmax_ = 0;
    std::vector<Cell> cells_;          // the roots first, n0x * n0y * n0z
    std::vector<uint64_t> key_;        // vertex position at the finest level
    std::vector<double> h_;
    std::vector<int32_t> pending_;     // vertices still without h
    void *map_ = nullptr;              // key -> vertex, while building
    Stats stats_;
    std::string err_;
};

}  // namespace poi_octree

#endif
