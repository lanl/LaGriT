#ifndef DOMAIN_H_
#define DOMAIN_H_
#include <iostream>
#include <vector>
#include <math.h>
#include <random>

#include "poi_2D_polygon.h"
#include "poi_3D_octree.h"

// /* Point Structure */
// struct Point {
//     double x; // x coord
//     double y; // y coord
//     double z; // z coord
//     double radius; // exclusion radius
//     unsigned int ix; // neighbor grid x-index
//     unsigned int iy; // neighbor grid y-index
//     unsigned int iz; // neighbor grid y-index
//     unsigned int nodeNum;
//     unsigned int face;
//     // std::vector<unsigned int>edges;
// };

/* Edge Structure */
struct Edge {
    unsigned int i; // node 1 index
    unsigned int j; // node 2 index
};


/* Domain Class */
class Domain {
  public:
    // output point name
    const char * mo_poi_pts_out;
    
    // miminum mesh resolution provided by user
    double h;
    // number of sample attempts around an accepted point (cheap)
    unsigned int numSamples;
    
    // number of resample sweeps used to fill in holes (expensive)
    unsigned int resampleSweeps;
    // Number of vertices on the input polygon.
    unsigned int numVertices;
    // Numder of nodes in the point distribution
    unsigned int numNodes;
    // Points in the point distribution
    std::vector<Point> nodes;
    
    // Edges of the bounding cuboid
    std::vector<Edge> edges;
    
    int seed;
    std::mt19937_64 generator;
    
    // Bounding box of the polygon
    double xMax;
    double xMin;
    double yMin;
    double yMax;
    double zMin;
    double zMax;
    
    // Basic polygon functions -> polygon.cpp
    // bool parseCommandLine(int argc, char **argv);
    // void loadVertices();
    void findBoundingBox();
    void initializeVariables();
    void printNodes();
    // void dumpNodes();
    void setBoundary();
    void setEdges();
    
    // Neighborhood Grid Parameters and functions
    // Neighborhood grid cell size (h/sqrt(2))
    double cellSize;
    // 1/cellSize
    double iCellSize;
    // Number of cells in X
    unsigned int numCellsX;
    // Numner of cells in Y
    unsigned int numCellsY;
    // Numner of cells in Y
    unsigned int numCellsZ;
    // Neighbor grid with linear indexing for 2D field
//    std::vector<unsigned int> grid;
    std::vector<int> grid;
    
    // total number of cells in the neighbor grid
    unsigned int totalNGCells;
    
    // Vector of cells that do not contain a point
    std::vector<int> emptyCells;
    
    // Neighborbood grid function -> neighborhoodGrid.cpp
    unsigned int getNeighborGridCellID(double x, double xMin);
    void initializeNeighborGrid();
    void cleanupNBGrid();
    void dumpNBGrid();
    std::vector<int> getNeighborCellsRadius(Point point);
    void findEmptyCells();
    unsigned int fillEmptyCells();
    void tagNeighborCells(Point point);
    unsigned int getNGLinearIndex(unsigned int i, unsigned int j, unsigned int k);
    
    // Distance Field Parameters
    // distance field filename
    
    // Name of distance field mesh object
    const char * mo_dfield_name;
    // max index, used for error detection
    unsigned int dfieldNumNodes;
    // distance field cell size (uniform in x and y)
    double dfCellSize;
    // inverse of distance field size
    double idfCellSize;
    // minimum x-value of distance field
    double dfXMin;
    // minimum y-value of distance field
    double dfYMin;
    // minimum z-value of distance field
    double dfZMin;
    // number of cells in x direction of distance field
    unsigned int dfNumCellsX;
    // number of cells in y direction of distance field
    unsigned int dfNumCellsY;
    // number of cells in y direction of distance field
    unsigned int dfNumCellsZ;
    std::vector<double> distanceField;
    // FIX 1: lattice spacing per axis (dfCellSize above is x)
    double dfCellSizeY;
    double dfCellSizeZ;
    double lookupH(double x, double y, double z);
    // h_lookup octree: when set, lookupH reads h from this octree
    // instead of the lattice (poi_3D_octree.h, poi_3D_octree_lg.cpp)
    poi_octree::Octree *octree = nullptr;
    // h_lookup octree: the neighbour grid on the octree leaves, each leaf
    // split into m x m x m sub-cells of at most the smallest h at its
    // corners; each sub-cell lists the points inside it
    // (poi_3D_octree_sampling.cpp)
    std::vector<int32_t> octOrd;    // octree cell -> leaf number, -1 for a parent
    std::vector<int32_t> octLeaf;   // leaf number -> octree cell
    std::vector<int64_t> octOff;    // leaf number -> its first sub-cell
    std::vector<uint8_t> octM;      // leaf number -> sub-cells per side
    std::vector<int32_t> octHead;   // sub-cell -> first point in it (nodeNum), 0 none
    std::vector<int32_t> octNext;   // nodeNum - 1 -> next point in its sub-cell, 0 last, -1 not listed
    std::vector<int64_t> octEmpty;  // the open sub-cells (findEmptyCells)
    std::vector<int32_t> octScratch;
    std::vector<float> octLeafHmax; // leaf number -> largest h at its corners
    std::vector<float> octLeafReach; // leaf number -> search half side for points in it
    void octInitReach();
    double octEstimateNodes();
    void octInitNeighborGrid();
    std::vector<int> octGetNeighborCellsRadius(const Point &point);
    void octTagNeighborCells(const Point &point);
    void octFindEmptyCells();
    unsigned int octFillEmptyCells();
    void octGather(const double c0[3], const double c1[3], double R, std::vector<int> &out);
    void octSearchLeaves(const double c0[3], const double c1[3], double R);
    void gatherLeaves(const double c0[3], const double c1[3], std::vector<int> &out);
    bool octDiskFree(const Point &x);
    double octSearchR = 0.0;
    void octNearSubCell(int64_t sc, const double mn[3], double u, std::vector<int> &out);
    double octLeafH(int32_t L, double x, double y, double z);
    bool octLocalAccept(Point &x, int32_t L, const std::vector<int> &near);
    bool octBoxCovered(int32_t L, const double lo[3], const double hi[3], const std::vector<int> &near);
    bool octSubCellBox(int64_t sc, double lo[3], double hi[3], int32_t &L);
    bool octOwnCellRejects(const Point &point);
    int64_t octSubCellAt(double x, double y, double z);
    void octSubCellMin(int64_t sc, double mn[3], double &size);
    
    // Distance field -> distancefield.cpp
    void loadDistanceField();
    void dumpDistanceField();
    unsigned int getDFCellID(double x, double xMin);
    void getExclusionRadius(Point &point);
    unsigned int getDFLinearIndex(unsigned int i, unsigned int j, unsigned int k);
    
    // Sampling functions -> sampling.cpp
    double uniformDistribution();
    void initializeRandomGenerator(unsigned int seed);
    void mainSampling(unsigned int startIndex, bool restartFlag);
    void resample();
    Point newCandidate(Point currentPoint);
    bool testCandidate(Point &newPoint, double scaleFactor);
    void acceptCandidate(Point &newPoint);
    bool emptyDiskProperty(Point newPoint);
    void sampleEdges();
    void sampleFaces();
    std::vector<Point> sampleAlongLine(Point x0, Point x1);
    bool inBoundingBox(Point point);
    bool inDomain(Point point);
    Point newCandidateOnFace(Point currentPoint, unsigned int faceID);
    
    // io
    void addNodesToMeshObject();
    // Destructor
    ~Domain();
};

#endif

