#include <iostream>
#include <string>
#include <fstream>
#include <limits>
#include <iomanip>

#include "poi_polygon.h"
#include "poi_helperFunctions.h"

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
void Polygon::loadDistanceField() {
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
    cout << "Distance Field number of cells. nx " << dfNumCellsX << " " << "ny " << dfNumCellsY << " " << endl;
    getline(distanceFieldFile, line);
    parsedLine = splitOnWhiteSpace(line);
    // get minimum value for x and y
    dfXMin = std::stod(parsedLine[0]);
    dfYMin = std::stod(parsedLine[1]);
    cout << "Distance Field Lowe Bounds. xMin " << dfXMin << " " << "yMin " << dfYMin << " " << endl;
    getline(distanceFieldFile, line);
    // get distance field cell size
    dfCellSize = std::stod(line);
    // inversece cell size
    idfCellSize = 1.0 / dfCellSize;
    cout << "Distance Field Cell Size " << dfCellSize << endl;
    
    // allocate memory for distance field
    try {
        distanceField = new double*[dfNumCellsX];
        
        for (unsigned int i = 0; i < dfNumCellsX + 1; i++) {
            // Initialize all values as 0
            distanceField[i] = new double[dfNumCellsY + 1]();
        }
    } catch (std::bad_alloc& ba) {
        std::cerr << "Bad Allocation for distance Field " << ba.what() << endl;
    }
    
    // read file into the distance field array.
    // Note indexing is set to fortran ()
    for (unsigned int j = 0; j < dfNumCellsY; j++) {
        for (unsigned int i = 0; i < dfNumCellsX; i++) {
            getline(distanceFieldFile, line);
            distanceField[i][j] = std::stod(line);
        }
    }
    
    cout << "Loading Distance Field Complete\n" << endl;
}

/*! Write distance field to file. Used for debugging.
Format
x,y,value
*/
void Polygon::dumpDistanceField() {
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
unsigned int Polygon::getDFCellID(double x, double xMin) {
    unsigned int i;
    i = int(floorf((x - xMin) * idfCellSize));
    return i;
}
/*! Returns the local exclution radius by a look up table in the
distance field array.
*/
void Polygon::getExclusionRadius(Point &point) {
    unsigned int i = getDFCellID(point.x, dfXMin);
    unsigned int j = getDFCellID(point.y, dfYMin);
    point.radius = distanceField[i][j];
}
