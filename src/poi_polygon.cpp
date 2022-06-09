#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <limits>

#include "poi_polygon.h"
#include "poi_sampling.h"
#include "poi_helperFunctions.h"

/* from lagrit lg_ codes */
#include "lg_c_interface.h"
#include "lg_f_interface.h"

using std::cout;
using std::endl;
using std::string;
using std::vector;
using std::ifstream;


/*  Modified for LAGRIT 
    Parse command line and add variable to the polygon object.
    Expected values:
      1 inputFilename - polygon file or cmo name
      2 outputFilename - output filename or cmo name
      3 h - mininum mesh resolution
      4 distanceFieldFilename - distance field filename
      5 numSamples (default 10) - number of samples initialized
      6 resampleSweeps (default 1) - num resample sweeps

 original: bool Polygon::parseCommandLine(int nargs, char **argv) {
*/

bool Polygon::parseCommandLine(int nargs, char **argstr) {

/*    if (nargs < 5) {
        std::cout << "Error: Incorrect Number of command line arguments provided" << endl;
        std::cout << nargs << " values provided. 4 expected." << endl;
        std::cout << "Expected values: polygon filename (avs format), output filename, mininum mesh resolution, distance field filename" << endl;
        return false;
    }
*/
    std::cout << nargs << " Number values provided." << endl;
    
    // arg 1: input filename
    inputFilename = argstr[1];
    cout << "Reading polygon from " << inputFilename << endl;
    // arg 2: output filename
    outputFilename = argstr[2];
    cout << "Writting points to " << outputFilename << endl;
    // arg 3: h (minimum mesh resolution)
    h = std::stod(argstr[3]);
    cout << "Minium mesh resolution " << h << endl;
    // arg 4: distance field filename
    distanceFieldFilename  = argstr[4];
    cout << "Distance Field Filename " << distanceFieldFilename << endl;
    // Advanced user command line arguments.
    // arg 5: number of samples initialized by accepted point
    // low values are fast but make a point distribution with holes
    // high values are slow due to larger numbers of rejections, but a better distribution
    numSamples = 10;
    cout << "Number of samples " << numSamples << endl;
    // arg 6: number of re-sampling sweeps. 1 is typically sufficent
    // always do 1, subsequent sweeps might help?  depends on numSamples
    resampleSweeps = 1;
    cout << "Number of resample sweeps " << resampleSweeps << endl;
    return true;
}

/*! Computes the 2D bounding box of the polygon */
void Polygon::findBoundingBox() {
    xMax = nodes[0].x;
    xMin = nodes[0].x;
    yMax = nodes[0].y;
    yMin = nodes[0].y;
    
    for (unsigned int i = 1; i < numNodes; i++) {
        xMax = std::max(xMax, nodes[i].x);
        xMin = std::min(xMin, nodes[i].x);
        yMax = std::max(yMax, nodes[i].y);
        yMin = std::min(yMin, nodes[i].y);
    }
    
    cout << "Polygon Bounding Box\nx-min: " << xMin << ", x-max: " << xMax << "\ny-min: " << yMin << ", y-max: " << yMax << "\n" << endl;
}

/*! Initializes variables of the polygon */
void Polygon::initializeVariables() {
    cout << "Initializing variables " << endl;
    // initialize the number of nodes in the point distribution to be the inital number of vertices
    numNodes = numVertices;
    // determine the bounding box of the domain
    findBoundingBox();
    
    // get initial exclusion radius
    for (unsigned int i = 0; i < numNodes; i++) {
        getExclusionRadius(nodes[i]);
    }
}

/*  LAGRIT: new routine will load vertices from polygon cmo
*/
void Polygon::loadVerticesCMO() {

    cout << "Load vertices from cmo: " << inputFilename << endl;

    // error: cannot convert ‘std::__cxx11::string to ‘const char*’ 
    // for argument ‘2’ to ‘int lg_cmo_get_intinfo(const char*, const char*)’
    // int nnodes = lg_cmo_get_intinfo("nnodes", inputFilename);
    // for now just get current cmo

    char cmo_name[32];
    int err = lg_cmo_get_name(cmo_name, 32);

    int nnodes = lg_cmo_get_intinfo("nnodes", cmo_name);
    if (nnodes <= 0) {
        printf("ERROR: No nodes in cmo: '%s'\n", cmo_name);
        return;
    }
    numVertices = nnodes;
    cout << "There are " << numVertices << " nodes on the boundary of the polygon\n";

    cout << "Added vertices from cmo: " << cmo_name <<  endl;

    // get polygon xy minmax from cmo

    long ierr = 0;
    double xval = 0;
    long icmolen = 32; 
    long iattlen = 4;

    fc_cmo_get_double_(cmo_name,"xmin",&xval,&ierr,icmolen,iattlen);
    xMin = xval;
    fc_cmo_get_double_(cmo_name,"xmax",&xval,&ierr,icmolen,iattlen);
    xMax = xval;
    fc_cmo_get_double_(cmo_name,"ymin",&xval,&ierr,icmolen,iattlen);
    yMin = xval;
    fc_cmo_get_double_(cmo_name,"ymax",&xval,&ierr,icmolen,iattlen);
    yMax = xval;

    cout << "Added minmax from cmo: " << cmo_name <<  endl;
    printf("xmin xmax: %f  %f\n", xMin, xMax);
    printf("ymin ymax: %f  %f\n", yMin, yMax);

    cout << "Added vertices from cmo: " << cmo_name << " complete" << endl;
}

/*! Load vertices from the avs file
*/
void Polygon::loadVertices() {

    cout << "Reading vertices from " << inputFilename << endl;
    cout << "Early exit debug version. " << inputFilename << endl;

    string line;
    ifstream inpFile;
    vector<string> parsedLine;
    inpFile.open(inputFilename, ifstream::in);
    // checkIfOpen(inpFile, filename);
    // read header
    getline(inpFile, line);
    parsedLine = splitOnWhiteSpace(line);
    numVertices = std::stoi(parsedLine[0]);
    cout << "There are " << numVertices << " nodes on the boundary of the polygon\n";
    
/* compiler error: poi_polygon.cpp:105:25: error: expected expression
    // read in the node coordinates
    for (unsigned int i = 0; i < numVertices; i++) {
        getline(inpFile, line);
        parsedLine = splitOnWhiteSpace(line);
        // Format Node Number, x-coord, y-coord,
        nodes.push_back({std::stod(parsedLine[1]), std::stod(parsedLine[2]), 0});
    }
*/
    
    inpFile.close();
    cout << "Reading vertices from " << inputFilename << " complete" << endl;
}

/*! Print node information to screen*/
void Polygon::printNodes() {
    for (unsigned int i = 0; i < numNodes; i++) {
        printPoint(nodes[i]);
    }
}
/*! Writes points to file
* format is x, y, flag
*/
void Polygon::dumpNodes() {
    std::ofstream fp;
    cout << "Writing points to file: " << outputFilename << endl;
    cout << "There are " << numNodes << " point in the final distribution" << endl;
    fp.open(outputFilename.c_str(), std::ofstream::out | std::ofstream::trunc);
    
    for (unsigned int i = 0; i < numNodes; i++) {
        fp << std::setprecision(12) << nodes[i].x << " " << nodes[i].y << " " << 0 << endl;
    }
    
    fp.close();
}


/*! Destructor for polygon class.
* Clears the memory allocated for the background grid
* Erase vectors on the polygon class
*/
Polygon::~Polygon() {
    cout << "---------------------------------" << endl;
    cout << "Cleaning up polygon " << endl;
    cout << "Destructor not active at this time. " << endl;

/************** comment out, need to add checks for existance ********    
    try {
        // delete dynamic memory of neighbor grid
        for (unsigned int i = 0; i < numCellsX + 1; i++) {
            delete [] grid[i];
        }
        
        delete [] grid;
    } catch (std::exception &e) {
        cout << e.what() << endl;
    }
    
    try {
        // delete dynamic memory of distance Field
        for (unsigned int i = 0; i < dfNumCellsX + 1; i++) {
            delete [] distanceField[i];
        }
        
        delete [] distanceField;
    } catch (std::exception &e) {
        cout << e.what() << endl;
    }
    
    // Clear vectors
    nodes.erase(nodes.begin(), nodes.end());
    nodes.shrink_to_fit();
    emptyCells.erase(emptyCells.begin(), emptyCells.end());
    emptyCells.shrink_to_fit();
    cout << "Cleaning up polygon complete" << endl;

************** comment out, need to add checks for existance ********/

}
