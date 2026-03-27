#include <iostream>
#include <string>
#include <fstream>
#include <random>
#include <vector>
// #include <bits/stdc++.h>
#include <algorithm>
#include <iomanip>

#include "poi_3D_domain.h"
#include "poi_3D_sampling.h"
#include "poi_helperFunctions.h"

using std::cout;
using std::endl;
using std::string;
using std::vector;

/*!
Returns index of neighbor grid
*/
unsigned int Domain::getNGLinearIndex(unsigned int i, unsigned int j, unsigned int k) {
    unsigned int linearIndex = i * (numCellsY * numCellsZ) + j * numCellsZ + k;
    
    if (linearIndex >= totalNGCells) {
        cout << "\nError. Linear index of neighbor grid larger than allocated. Max value : " << totalNGCells << endl;
        cout << "i,j,k,neighborGrid-index: " << i << ", " << j << ", " << k << ", " << linearIndex << endl;
        cout << i*cellSize + xMin << ", " <<  j*cellSize + yMin << ", " << k*cellSize + zMin << ", " << grid[linearIndex] << endl;
        return 0;
    }
    
    return linearIndex;
}

/*! Returns the index of the neighbor grid for a pt coordinate
to get x value
getNeighborGridCellID(x, xMin)
to get the y value
getNeighborGridCellID(y, yMin)
*/
unsigned int Domain::getNeighborGridCellID(double x, double xMin) {
    unsigned int i;
    i = int(floor((x - xMin) * iCellSize));
    return i;
}

/*! Creates the background grid.
The grid is a uniform quad mesh with resolution h/sqrt(2).
This allows for a grid to represent a ball of radius h.
If a cell in the grid is occupied, it's tagged with the node number + 1
0 indicates the cells is empty.
*/
void Domain::initializeNeighborGrid() {
    cout << "Initializing Neighbor Grid" << endl;
    // grid size is h / sqrt(2) to correspond with a circle of radius h
    cellSize = h / sqrt(3);
    cout << "cellSize: " << cellSize << endl;
    iCellSize = 1.0 / cellSize;
    numCellsX = int(ceil((xMax - xMin) * iCellSize)) + 1;
    numCellsY = int(ceil((yMax - yMin) * iCellSize)) + 1;
    numCellsZ = int(ceil((zMax - zMin) * iCellSize)) + 1;
    cout << "Num Cells X " << numCellsX << endl;
    cout << "Num Cells Y " << numCellsY << endl;
    cout << "Num Cells Z " << numCellsZ << endl;
    totalNGCells = numCellsX * numCellsY * numCellsZ;
    cout << "Total Cells " << totalNGCells << endl;
    // Create the background neighbor grid that is numCellX by numCellsY
    // The dynamic memory allocation gets freed in the destructor of polygon.
    cout << "Initializing memory for neighbor grid" << endl;
    grid.reserve(totalNGCells * 3);
    
    for (unsigned int i = 0; i < totalNGCells; i++) {
        grid.push_back(0);
    }
    
    cout << "Initializing memory for neighbor grid: Complete" << endl;
    
    // every occupied cells is labelled with the node-number (start at 1) of the node occupying it. empty cells are 0.
    for (unsigned int i = 0; i < numNodes; i++) {
        nodes[i].ix = getNeighborGridCellID(nodes[i].x, xMin);
        nodes[i].iy = getNeighborGridCellID(nodes[i].y, yMin);
        nodes[i].iz = getNeighborGridCellID(nodes[i].z, zMin);
        nodes[i].nodeNum = i + 1;
        getExclusionRadius(nodes[i]);
        tagNeighborCells(nodes[i]);
    }
    
    cout << "Initializing Grid Complete\n" << endl;
}

/*! dump grid locations to file. Used for debugging */
void Domain::dumpNBGrid() {
    // write nodes out to file
    string filename;
    filename = "output/neighborGrid.dat";
    std::ofstream fp;
    cout << "--> Writing points to file: " << filename << endl;
    fp.open(filename.c_str(), std::ofstream::out | std::ofstream::trunc);
    
    // Write Header
    for (unsigned int k = 0; k < numCellsZ + 1; k++) {
        for (unsigned int j = 0; j < numCellsY + 1; j++) {
            for (unsigned int i = 0; i < numCellsX + 1; i++) {
                auto linearIndex = getNGLinearIndex(i, j, k);
                fp << i*cellSize + xMin << "," <<  j*cellSize + yMin << "," << k*cellSize + zMin << "," << grid[linearIndex] << endl;
            }
        }
    }
    
    fp.close();
}

/*! Walks through the neighborgrid and return indices of all
previously accepted points within the exclusion radius of the current point.
This is done using the values of the neighborgrid
*/
std::vector<int> Domain::getNeighborCellsRadius(Point point) {
    
    std::vector<int> nodeIDs;
    // Get the corners of a square around ix,iy.
    /* Using a square, rather than a circle, because the math is faster.
    * The square has sides of length radius*2, so it contains the circle
    * The box contains all the points of the circle and a few extras.
    * Need to check if it's that big of a deal.
    */
    // determine number of cells in the neighbor grid with the radius
    // of the point
    unsigned int numCells;
    numCells = int( ceil(point.radius / cellSize) );
    unsigned int iMin, iMax;
    // minimum x edge is the current x-index the numCells, or 0.
    iMin = std::max(0, int( point.ix - numCells));
    // maximum x edge is the current x-index plus the numCells, or the edge of the grid.
    iMax = std::min(point.ix + numCells, numCellsX);
    unsigned int jMin, jMax;
    // minimum y edge is the current y-index minus the numCells, or 0.
    jMin = std::max(0, int( point.iy - numCells));
    // maximum y edge is the current y-index plus the numCells, or 0.
    jMax = std::min(point.iy + numCells, numCellsY);
    unsigned int kMin, kMax;
    // minimum y edge is the current y-index minus the numCells, or 0.
    kMin = std::max(0, int( point.iz - numCells));
    // maximum y edge is the current y-index plus the numCells, or 0.
    kMax = std::min(point.iz + numCells, numCellsZ);
    
    // walk through the box, and gather all non-zero entries
    for (unsigned int k = kMin; k < kMax; k++) {
        for (unsigned int j = jMin; j < jMax; j++) {
            for (unsigned int i = iMin; i < iMax; i++) {
                unsigned int linearIndex = getNGLinearIndex(i, j, k);
                
                if (grid[linearIndex] > 0) {
                    // cout << i << " " << j << " " << k << " " << grid[i][j][k] << endl;
                    nodeIDs.push_back(grid[linearIndex]);
                }
            }
        }
    }
    
    // remove duplicates from the vector.
    // duplicates occur because multiple cells in the neighbor grid can
    // fall within the radius of an accepted node.
    std::sort( nodeIDs.begin(), nodeIDs.end() );
    nodeIDs.erase( std::unique( nodeIDs.begin(), nodeIDs.end() ), nodeIDs.end() );
    return nodeIDs;
}

/*! Tag cells in the background neighbor grid as filled if they fall within the
* Exclusion radius of the provided point.
*/
void Domain::tagNeighborCells(Point point) {
//    cout << "tagging neighbor cells" << endl;

    // Get discrete radius. Number of cells covered by the radius
    unsigned int numCells;
    numCells = int( ceil(point.radius / cellSize) );
    // Get the range of x and y in the neighborgrid
    unsigned int iMin, iMax;
    iMin = std::max(0, int( point.ix - numCells));
    iMax = std::min(point.ix + numCells, numCellsX);
    unsigned int jMin, jMax;
    jMin = std::max(0, int( point.iy - numCells));
    jMax = std::min(point.iy + numCells, numCellsY);
    unsigned int kMin, kMax;
    // minimum y edge is the current y-index minus the numCells, or 0.
    kMin = std::max(0, int( point.iz - numCells));
    // maximum y edge is the current y-index plus the numCells, or 0.
    kMax = std::min(point.iz + numCells, numCellsZ);
    // check if the points are within the radius.
    Point tmpPoint;
    double dist;
    
    for (unsigned int k = kMin; k < kMax; k++) {
        for (unsigned int j = jMin; j < jMax; j++) {
            for (unsigned int i = iMin; i < iMax; i++) {
                unsigned int linearIndex = getNGLinearIndex(i, j, k);
                
                // check is cell is already tagged, if not, then check the distance.
                if (grid[linearIndex] == 0) {
                    tmpPoint.x =  i * cellSize + xMin;
                    tmpPoint.y =  j * cellSize + yMin;
                    tmpPoint.z =  k * cellSize + zMin;
                    dist = distance3D(point, tmpPoint);
                    
                    // if the distance is less than the radius, then tag the grid cell as occupied.
                    if (dist <= point.radius) {
                        grid[linearIndex] = point.nodeNum;
                        //cout << point.nodeNum << endl;
                    }
                }
            }
        }
    }
}

/*! Walk through grid and find cells that do not contain a point
*/
void Domain::findEmptyCells() {
    cout << "Finding Empty Cells " << endl;
    emptyCells.erase(emptyCells.begin(), emptyCells.end());
    emptyCells.shrink_to_fit();
    Point tmpPoint;
    
    for (unsigned int k = 0; k < numCellsZ; k++) {
        for (unsigned int j = 0; j < numCellsY; j++) {
            for (unsigned int i = 0; i < numCellsX; i++) {
                unsigned int linearIndex = getNGLinearIndex(i, j, k);
                
                // check if cell is occupied.
                if (grid[linearIndex] == 0) {
                    tmpPoint.x = i * cellSize + xMin;
                    tmpPoint.y = j * cellSize + yMin;
                    tmpPoint.z = k * cellSize + zMin;
                    
                    // Check if point is the domain, neighbor grid can extend
                    // beyond the polygon boundary (edges of the neighbord grid are based on the bounding box).
                    if (inBoundingBox(tmpPoint)) {
                        // The first entry is i the second is j
                        // Example: index 0,1 are i0,j0, 2,3 are i1, j2.
                        emptyCells.push_back(i);
                        emptyCells.push_back(j);
                        emptyCells.push_back(k);
                    }
                }
            }
        }
    }
    
    cout << "There are " << emptyCells.size() / 3 << " empty cells" << endl;
}

/*! Given the list of empty cells, attempt to fill them with a new point.
*/
unsigned int Domain::fillEmptyCells() {
    Point newPoint;
    unsigned int newPoints = 0;
    // cells indices are stored in multiples of 2.
    // The first entry is i the second is j
    // Example: index 0,1 are i0,j0, 2,3 are i1, j2.
    cout << "Filling empty cells in the background grid" << endl;

    cout << "\tCell#\t\tTotal Cells\tPercentage"<<endl;
    double currentPercentage = 0;
    for (unsigned int cellIdx = 0; cellIdx < emptyCells.size(); cellIdx += 3) {
        // if (((cellIdx/3) % 100) == 0){
        //     cout << "\t"<<cellIdx/3 <<  "\t\t" << (emptyCells.size()/3) << "\t\t";
        //     double percentage = 100.0*(double)cellIdx/(double)emptyCells.size();
        //     cout << std::setprecision(3) << percentage << " %"<< endl; 
        // }
        double percentage = 100.0*(double)cellIdx/(double)emptyCells.size();
        if (percentage > currentPercentage){
            currentPercentage += 5;
            cout << "\t"<<cellIdx/3 <<  "\t\t" << (emptyCells.size()/3) << "\t\t";
            cout << std::setprecision(3) << percentage << " %"<< endl; 
        }


        // get the cell index
        unsigned int i = emptyCells[cellIdx];
        unsigned int j = emptyCells[cellIdx + 1];
        unsigned int k = emptyCells[cellIdx + 2];
        // get the the cell boudning boundaries
        double xCellMin = i * cellSize + xMin;
        double yCellMin = j * cellSize + yMin;
        double zCellMin = k * cellSize + zMin;
        // check if the neighbor cell is still empty.
        // Entries get filled in as these points are accetped.
        // Faster to do this check, than update/remove elements from  emptyCells on the fly.
        unsigned int linearIndex = getNGLinearIndex(i, j, k);
        
        if (grid[linearIndex] == 0) {
            for (unsigned int count = 0; count < numSamples; count++) {
                newPoint.x = uniformDistribution() * cellSize + xCellMin;
                newPoint.y = uniformDistribution() * cellSize + yCellMin;
                newPoint.z = uniformDistribution() * cellSize + zCellMin;
                
                if (testCandidate(newPoint, 1)) {
                    numNodes++;
                    newPoints++;
                    newPoint.nodeNum = numNodes;
                    nodes.push_back(newPoint);
                    tagNeighborCells(newPoint);
                    break;
                }
            }
        }
    }
    
    cout << newPoints << " out of " << emptyCells.size() / 3 << " empty cells have been filled" << endl;
    cout << "Filling empty cells in the background grid complete\n" << endl;
    return newPoints;
}
