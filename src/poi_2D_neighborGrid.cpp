#include <iostream>
#include <string>
#include <fstream>
#include <random>
#include <vector>
// #include <bits/stdc++.h>
#include <algorithm>    // std::sort
#include <iomanip>

#include "poi_2D_polygon.h"
#include "poi_2D_sampling.h"
#include "poi_helperFunctions.h"

using std::cout;
using std::endl;
using std::string;
using std::vector;

/*! Returns the index of the neighbor grid for a pt coordinate
to get x value
getNeighborGridCellID(x, xMin)
to get the y value
getNeighborGridCellID(y, yMin)
*/
unsigned int Polygon::getNeighborGridCellID(double x, double xMin) {
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
void Polygon::initializeNeighborGrid() {
    cout << "Initializing Neighbor Grid" << endl;
    // grid size is h / sqrt(2) to correspond with a circle of radius h
    cellSize = h / sqrt(2);
    cout << "cellSize: " << cellSize << endl;
    iCellSize = 1.0 / cellSize;
    numCellsX = int(ceil((xMax - xMin) * iCellSize));
    numCellsY = int(ceil((yMax - yMin) * iCellSize));
    cout << "Num Cells X " << numCellsX << endl;
    cout << "Num Cells Y " << numCellsY << endl;
    cout << "Total Cells " << numCellsX*numCellsY << endl;
    unsigned int totalCells = numCellsX * numCellsY;
    // Create the background neighbor grid that is numCellX by numCellsY
    // The dynamic memory allocation gets freed in the destructor of polygon.
    cout << "Initializing memory for neighbor grid" << endl;
    grid.reserve(totalCells * 2);
    
    // changeg this to linear indexing with a vector, because Linux was being a pita.
    for (unsigned int i = 0; i < numCellsX * numCellsY + 1; i++) {
        grid.push_back(0);
    }
    
    cout << "Initializing memory for neighbor grid: Complete" << endl;
    // every occupied cells is labelled with the node-number (start at 1) of the node occupying it. empty cells are 0.
    cout << "Populating neighbor grid " << endl;
    
    for (unsigned int i = 0; i < numNodes; i++) {
        nodes[i].ix = getNeighborGridCellID(nodes[i].x, xMin);
        nodes[i].iy = getNeighborGridCellID(nodes[i].y, yMin);
        nodes[i].nodeNum = i + 1;
        getExclusionRadius(nodes[i]);
        tagNeighborCells(nodes[i]);
    }
    
    cout << "Populating neighbor grid: complete" << endl;
    cout << "Initializing Grid Complete\n" << endl;
}

/*! dump grid locations to file. Used for debugging */
void Polygon::dumpNBGrid() {
    // write nodes out to file
    string filename;
    filename = "output/neighborGrid.dat";
    std::ofstream fp;
    cout << "--> Writing points to file: " << filename << endl;
    fp.open(filename.c_str(), std::ofstream::out | std::ofstream::trunc);
    
    // Write Header
    for (unsigned int i = 0; i < numCellsX + 1; i++) {
        for (unsigned int j = 0; j < numCellsY + 1; j++) {
            // fp << i*cellSize + xMin << "," <<  j*cellSize + yMin << "," << grid[i][j] << endl;
            fp << i*cellSize + xMin << "," <<  j*cellSize + yMin << "," << grid[i * numCellsY + j] << endl;
        }
    }
    
    fp.close();
}

/*! Walks through the neighborgrid and return indices of all
previously accepted points within the exclusion radius of the current point.
This is done using the values of the neighborgrid
*/
std::vector<int> Polygon::getNeighborCellsRadius(Point point) {
    std::vector<int> nodeIDs;
    nodeIDs.reserve(0);
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
    iMax = std::min(point.ix + numCells, numCellsX + 1);
    unsigned int jMin, jMax;
    // minimum y edge is the current y-index minus the numCells, or 0.
    jMin = std::max(0, int( point.iy - numCells));
    // maximum y edge is the current y-index plus the numCells, or 0.
    jMax = std::min(point.iy + numCells, numCellsY + 1);
    
    // walk through the box, and gather all non-zero entries
    for (unsigned int i = iMin; i < iMax; i++) {
        for (unsigned int j = jMin; j < jMax; j++) {
            //if (grid[i][j] > 0) {
            if (grid[i * numCellsY + j] > 0 && i * numCellsY + j < grid.size()) {
                // nodeIDs.push_back(grid[i][j]);
                //cout << "total cells : " << numCellsX * numCellsY + 1 << endl;
                //cout << "i*NumCellsY + j : " << i * numCellsY + j << endl;
                //cout << "grid[i * numCellsY + j] : " << grid[i * numCellsY + j] << endl;
                nodeIDs.push_back(grid[i * numCellsY + j]);
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
void Polygon::tagNeighborCells(Point point) {
    // cout << "tagNeighborCells start" << endl;
    // Get discrete radius. Number of cells covered by the radius
    unsigned int numCells;
    numCells = int( ceil(point.radius / cellSize) );
    // Get the range of x and y in the neighborgrid
    unsigned int iMin, iMax;
    iMin = std::max(0, int( point.ix - numCells));
    iMax = std::min(point.ix + numCells, numCellsX + 1);
    unsigned int jMin, jMax;
    jMin = std::max(0, int( point.iy - numCells));
    jMax = std::min(point.iy + numCells, numCellsY + 1);
    // check if the points are within the radius.
    Point tmpPoint;
    double dist;
    
    for (unsigned int i = iMin; i < iMax; i++) {
        for (unsigned int j = jMin; j < jMax; j++) {
            // check is cell is already tagged, if not, then check the distance.
            //if (grid[i][j] == 0) {
            if (grid[i * numCellsY + j] == 0) {
                tmpPoint.x =  i * cellSize + xMin;
                tmpPoint.y =  j * cellSize + yMin;
                dist = distance2D(point, tmpPoint);
                
                // if the distance is less than the radius, then tag the grid cell as occupied.
                if (dist < point.radius) {
                    //grid[i][j] = point.nodeNum;
                    grid[i * numCellsY + j] = point.nodeNum;
                }
            }
        }
    }
    
    // cout << "tagNeighborCells end " << endl;
}

/*! Walk through grid and find cells that do not contain a point
*/
void Polygon::findEmptyCells() {
    cout << "Finding Empty Cells " << endl;
    emptyCells.erase(emptyCells.begin(), emptyCells.end());
    emptyCells.shrink_to_fit();
    Point tmpPoint;
    
    for (unsigned int i = 0; i < numCellsX + 1; i++) {
        for (unsigned int j = 0; j < numCellsY + 1; j++) {
            // check if cell is occupied.
            //if (grid[i][j] == 0) {
            if (grid[i * numCellsY + j] == 0) {
                tmpPoint.x = i * cellSize + xMin;
                tmpPoint.y = j * cellSize + yMin;
                
                // Check if point is the domain, neighbor grid can extend
                // beyond the polygon boundary (edges of the neighbord grid are based on the bounding box).
                if (inDomain(tmpPoint)) {
                    // The first entry is i the second is j
                    // Example: index 0,1 are i0,j0, 2,3 are i1, j2.
                    emptyCells.push_back(i);
                    emptyCells.push_back(j);
                }
            }
        }
    }
    
    cout << "There are " << 0.5 * emptyCells.size() << " empty cells" << endl;
}

/*! Given the list of empty cells, attempt to fill them with a new point.
*/
unsigned int Polygon::fillEmptyCells() {
    Point newPoint;
    unsigned int newPoints = 0;
    // cells indices are stored in multiples of 2.
    // The first entry is i the second is j
    // Example: index 0,1 are i0,j0, 2,3 are i1, j2.
    cout << "Filling empty cells in the background grid" << endl;
   
    
    cout << "\tCell#\t\tTotal Cells\tPercentage"<<endl;
    double currentPercentage = 0;
    for (unsigned int cellIdx = 0; cellIdx < emptyCells.size(); cellIdx += 2) {

        double percentage = 100.0*(double)cellIdx/(double)emptyCells.size();
        if (percentage > currentPercentage){
            currentPercentage += 5;
            cout << "\t"<<cellIdx/2 <<  "\t\t" << (emptyCells.size()/2) << "\t\t";
            cout << std::setprecision(3) << percentage << " %"<< endl; 
        }

        // get the cell index
        unsigned int i = emptyCells[cellIdx];
        unsigned int j = emptyCells[cellIdx + 1];
        // get the the cell boudning boundaries
        double xCellMin = i * cellSize + xMin;
        double yCellMin = j * cellSize + yMin;
        
        // check if the neighbor cell is still empty.
        // Entries get filled in as these points are accetped.
        // Faster to do this check, than update/remove elements from  emptyCells on the fly.
        // if (grid[i][j] == 0) {
        if (grid[i * numCellsY + j] == 0) {
            for (unsigned int count = 0; count < numSamples; count++) {
                newPoint.x = uniformDistribution() * cellSize + xCellMin;
                newPoint.y = uniformDistribution() * cellSize + yCellMin;
                
                if (testCandidate(newPoint)) {
                    acceptCandidate(newPoint);
                    newPoints++;
                    break;
                }
            }
        }
    }
    
    cout << newPoints << " out of " << 0.5 * emptyCells.size() << " empty cells have been filled" << endl;
    cout << "Filling empty cells in the background grid complete\n" << endl;
    return newPoints;
}
