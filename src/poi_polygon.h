#ifndef POLYGON_H_
#define POLYGON_H_
#include <vector>
#include <cmath>
#include <iostream>
#include <vector>
#include <random>

/* Point Structure */
struct Point {
    double x; // x coord
    double y; // y coord
    double radius; // exclusion radius
    unsigned int ix; // neighbor grid x-index
    unsigned int iy; // neighbor grid y-index
    unsigned int nodeNum;
};

/* Polygon Class */
class Polygon {
  public:
    // input polygon filename
    std::string inputFilename;
    // input polygon filename
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
    
    // Bounding box of the polygon
    double xMax;
    double xMin;
    double yMin;
    double yMax;
    
    // Basic polygon functions -> polygon.cpp
    bool parseCommandLine(int argc, char **argv);
    void loadVertices();
    void findBoundingBox();
    void initializeVariables();
    void printNodes();
    void dumpNodes();

    // LAGRIT added functions
    void loadVerticesCMO();
    
    
    // Neighborhood Grid Parameters and functions
    // Neighborhood grid cell size (h/sqrt(2))
    double cellSize;
    // 1/cellSize
    double iCellSize;
    // Number of cells in X
    unsigned int numCellsX;
    // Numner of cells in Y
    unsigned int numCellsY;
    // 2D grid
    int **grid;
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
    // number of cells in x direction of distance field
    unsigned int dfNumCellsX;
    // number of cells in y direction of distance field
    unsigned int dfNumCellsY;
    double **distanceField;
    
    // Distance field -> distancefield.cpp
    void loadDistanceField();
    void dumpDistanceField();
    unsigned int getDFCellID(double x, double xMin);
    void getExclusionRadius(Point &point);
    
    // Sampling functions -> sampling.cpp
    void mainSampling(unsigned int startIndex, bool restartFlag);
    void resample();
    bool testCandidate(Point &newPoint);
    void acceptCandidate(Point &newPoint);
    bool emptyDiskProperty(Point newPoint);
    void sampleBoundaries();
    std::vector<Point> sampleAlongLine(Point x0, Point x1);
    bool inBoundingBox(Point point);
    bool inDomain(Point point);
    
    // Destructor
    ~Polygon();
};

#endif

