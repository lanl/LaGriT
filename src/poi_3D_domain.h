#ifndef DOMAIN_H_
#define DOMAIN_H_
#include <iostream>
#include <vector>
#include <math.h>
#include <random>


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

/* Edge Structure */
struct Edge {
    unsigned int i; // node 1 index
    unsigned int j; // node 2 index
};


/* Domain Class */
class Domain {
  public:
    // input polygon filename
    std::string inputFilename;
    // output point name
    std::string outputFilename;
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
    
    std::mt19937_64 generator;
    
    // Bounding box of the polygon
    double xMax;
    double xMin;
    double yMin;
    double yMax;
    double zMin;
    double zMax;
    
    // Basic polygon functions -> polygon.cpp
    bool parseCommandLine(int argc, char **argv);
    void loadVertices();
    void findBoundingBox();
    void initializeVariables();
    void printNodes();
    void dumpNodes();
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
    // 2D grid
    int ***grid;
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
    
    // Distance Field Parameters
    // distance field filename
    std::string distanceFieldFilename;
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
    double ***distanceField;
    
    // Distance field -> distancefield.cpp
    void loadDistanceField();
    void dumpDistanceField();
    unsigned int getDFCellID(double x, double xMin);
    void getExclusionRadius(Point &point);
    
    // Sampling functions -> sampling.cpp
    double uniformDistribution();
    void initializeRandomGenerator(unsigned int seed);
    void mainSampling(unsigned int startIndex, bool restartFlag);
    void resample();
    Point newCandidate(Point currentPoint);
    bool testCandidate(Point &newPoint);
    void acceptCandidate(Point &newPoint);
    bool emptyDiskProperty(Point newPoint);
    void sampleEdges();
    void sampleFaces();
    std::vector<Point> sampleAlongLine(Point x0, Point x1);
    bool inBoundingBox(Point point);
    bool inDomain(Point point);
    Point newCandidateOnFace(Point currentPoint, unsigned int faceID);
    // Destructor
    ~Domain();
};

#endif

