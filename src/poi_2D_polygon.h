#ifndef POLYGON_H_
#define POLYGON_H_
#include <vector>
#include <cmath>
#include <iostream>
#include <vector>
#include <random>

// /* Point Structure */
// struct Point {
//     double x = 0; // x coord
//     double y = 0; // y coord
//     double radius = 0; // exclusion radius
//     unsigned int ix = 0; // neighbor grid x-index
//     unsigned int iy = 0; // neighbor grid y-index
//     unsigned int nodeNum = 0;
// };

/* Point Structure */
struct Point {
    double x; // x coord
    double y; // y coord
    double z; // z coord
    double radius; // exclusion radius
    unsigned int ix; // neighbor grid x-index
    unsigned int iy; // neighbor grid y-index
    unsigned int iz; // neighbor grid y-index
    unsigned int nodeNum;
    unsigned int face;
    // std::vector<unsigned int>edges;
};


/* Polygon Class */
class Polygon {
  public:
    // mesh object name for input polygon
    const char * mo_poly_name;
    // mesh object name for output points
    const char * mo_pts_name;
    
    // outputfilename
    std::string outputFilename = "points.xyz";
    // miminum mesh resolution provided by user
    double h;
    // number of sample attempts around an accepted point (cheap)
    unsigned int numSamples = 10;
    // number of resample sweeps used to fill in holes (expensive)
    unsigned int resampleSweeps = 1;
    // Number of vertices on the input polygon.
    unsigned int numVertices;
    // Numder of nodes in the point distribution
    unsigned int numNodes;
    // Points in the point distribution
    std::vector<Point> nodes;
    // Bounding box of the polygon
    double xMax;
    double xMin;
    double yMin;
    double yMax;
    double zValue;
    
    // Random number generator
    unsigned int seed = 0;
    std::mt19937_64 generator;
    
    // Basic polygon functions -> polygon.cpp
    // bool parseCommandLine(int argc, char **argv);
    bool loadVertices();
    void addNodesToMeshObject();
    
    void findBoundingBox();
    void initializeVariables();
    
    void printNodes();
    void dumpNodes();
    
    // LAGRIT added functions
    bool loadVerticesCMO();
    
    
    // Neighborhood Grid Parameters and functions
    // Neighborhood grid cell size (h/sqrt(2))
    double cellSize;
    // 1/cellSize
    double iCellSize;
    // Number of cells in X
    unsigned int numCellsX;
    // Numner of cells in Y
    unsigned int numCellsY;
    // Neighbor grid with linear indexing for 2D field
    std::vector<unsigned int> grid;
    
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
    
    // Name of distance field mesh object
    const char * mo_dfield_name;
    // distance field cell size (uniform in x and y)
    double dfCellSize;
    // inverse of distance field size
    double idfCellSize;
    // minimum x-value of distance field
    double dfXMin;
    // minimum y-value of distance field
    double dfYMin;
    // number of cells in x direction of distance field
    unsigned int dfNumCellsX;
    // number of cells in y direction of distance field
    unsigned int dfNumCellsY;
    std::vector<double> distanceField;
    
    // Distance field -> distancefield.cpp
    void loadDistanceField();
    void loadDistanceFieldCMO();
    void dumpDistanceField();
    unsigned int getDFCellID(double x, double xMin);
    void getExclusionRadius(Point &point);
    
    // Sampling functions -> poi_sampling.cpp
    void mainSampling(unsigned int startIndex, bool restartFlag);
    void resample();
    bool testCandidate(Point &newPoint);
    void acceptCandidate(Point &newPoint);
    bool emptyDiskProperty(Point newPoint);
    void sampleBoundaries();
    std::vector<Point> sampleAlongLine(Point x0, Point x1);
    bool inBoundingBox(Point point);
    bool inDomain(Point point);
    double uniformDistribution();
    void initializeRandomGenerator(unsigned int seed);
    Point newCandidate(Point currentPoint);
    
    // Constructor
    Polygon();
    // Destructor
    ~Polygon();
};

#endif

