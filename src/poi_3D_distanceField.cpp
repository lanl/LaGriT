#include <iostream>
#include <string>
#include <fstream>
#include <limits>
#include <iomanip>
#include <cmath>

#include "poi_3D_domain.h"
#include "poi_3D_helperFunctions.h"

// #define DEBUG 1;

using std::cout;
using std::endl;
using std::string;
using std::vector;
using std::ifstream;

/*! Loads distance field from file.
Header format
nx ny
xMin yMin
h
value_1
value_2
...
value_(nx*ny)
*/
void Domain::loadDistanceField() {
    cout << "Reading distance field from " << distanceFieldFilename << endl;
    string line;
    ifstream distanceFieldFile;
    vector<string> parsedLine;
    distanceFieldFile.open(distanceFieldFilename, ifstream::in);
    // checkIfOpen(inpFile, filename);
    // read header
    getline(distanceFieldFile, line);
    parsedLine = splitOnWhiteSpace(line);
    // get number of cells in x and y
    dfNumCellsX = std::stoi(parsedLine[0]);
    dfNumCellsY = std::stoi(parsedLine[1]);
    dfNumCellsZ = std::stoi(parsedLine[2]);
    cout << "Distance Field number of cells. nx " << dfNumCellsX << " " << "ny " << dfNumCellsY << "nz " << dfNumCellsZ << " " << endl;
    getline(distanceFieldFile, line);
    parsedLine = splitOnWhiteSpace(line);
    // get minimum value for x and y
    dfXMin = std::stod(parsedLine[0]);
    dfYMin = std::stod(parsedLine[1]);
    dfZMin = std::stod(parsedLine[2]);
    cout << "Distance Field Lower Bounds. xMin " << dfXMin << " yMin " << dfYMin << " zMin " << dfZMin << endl;
    getline(distanceFieldFile, line);
    // get distance field cell size
    h = std::stod(line);
    dfCellSize = std::stod(line);
    // inversece cell size
    idfCellSize = 1.0 / dfCellSize;
    cout << "Distance Field Cell Size " << dfCellSize << endl;
    
    // allocate memory for distance field
    try {
        distanceField = new double**[dfNumCellsX];
        
        for (unsigned int i = 0; i < dfNumCellsX + 1; i++) {
            // Initialize all values as 0
            distanceField[i] = new double*[dfNumCellsY + 1];
            
            for (unsigned int j = 0; j < dfNumCellsY + 1; j++) {
                distanceField[i][j] = new double[dfNumCellsZ + 1]();
            }
        }
    } catch (std::bad_alloc& ba) {
        std::cerr << "Bad Allocation for distance Field " << ba.what() << endl;
    }
    
    // read file into the distance field array.
    // Note indexing is set to fortran ()
    for (unsigned int k = 0; k < dfNumCellsZ; k++) {
        for (unsigned int j = 0; j < dfNumCellsY; j++) {
            for (unsigned int i = 0; i < dfNumCellsX; i++) {
                getline(distanceFieldFile, line);
                distanceField[i][j][k] = std::stod(line);
            }
        }
    }
    
    cout << "Loading Distance Field Complete\n" << endl;
}

/*! Write distance field to file. Used for debugging.
Format
x,y,value
*/
void Domain::dumpDistanceField() {
    // write nodes out to file
    string filename;
    filename = "distanceField_out.dat";
    std::ofstream fp;
    cout << "--> Writing points to file: " << filename << endl;
    fp.open(filename.c_str(), std::ofstream::out | std::ofstream::trunc);
    
    // format
    // x y value
    for (unsigned int i = 0; i < dfNumCellsX; i++) {
        for (unsigned int j = 0; j < dfNumCellsX; j++) {
            fp << std::setprecision(12) << i*dfCellSize + dfXMin << "," <<  j*dfCellSize + dfYMin << "," << distanceField[i][j] << endl;
        }
    }
    
    fp.close();
    cout << "Complete " << endl;
}

/*! Returns the cell id of the point x in the distance field.
Used for both x and y. Cell id is defined using the lower left corner
of a quad/hex cell.
*/
unsigned int Domain::getDFCellID(double x, double xMin) {
    unsigned int i;
    i = int(floorf((x - xMin) * idfCellSize));
    return i;
}
/*! Returns the local exclution radius by a look up table in the
distance field array.
*/
void Domain::getExclusionRadius(Point &point) {
#ifdef DEBUG
    cout << "running getExclusionRadius  " << endl;
#endif
    unsigned int i = getDFCellID(point.x, dfXMin);
    unsigned int j = getDFCellID(point.y, dfYMin);
    unsigned int k = getDFCellID(point.z, dfZMin);
#ifdef DEBUG
    cout << "x,y,k: " << point.x << " " << point.y << " " << point.z << endl;
    cout << "i,j,k: " << i << " " << j << " " << k << endl;
#endif
    point.radius = distanceField[i][j][k];
    //point.radius = h;
}
