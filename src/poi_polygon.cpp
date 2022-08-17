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


/*! Computes the 2D bounding box of the polygon */
void Polygon::findBoundingBox() {

    cout << "finding bounding box : Starting " << endl;
    xMax = nodes[0].x;
    xMin = nodes[0].x;
    yMax = nodes[0].y;
    yMin = nodes[0].y;

    cout << "numNodes: " << numNodes << endl; 
    for (unsigned int i = 1; i < numNodes; i++) {
        xMax = std::max(xMax, nodes[i].x);
        xMin = std::min(xMin, nodes[i].x);
        yMax = std::max(yMax, nodes[i].y);
        yMin = std::min(yMin, nodes[i].y);
    }
    
    cout << "Polygon Bounding Box\nx-min: " << xMin << ", x-max: " << xMax << "\ny-min: " << yMin << ", y-max: " << yMax << "\n" << endl;
    cout << "finding bounding box : done " << endl;
}

/*! Initializes variables of the polygon */
void Polygon::initializeVariables() {
    cout << "Initializing variables : Starting " << endl;
    // initialize the number of nodes in the point distribution to be the inital number of vertices
    numNodes = numVertices;
    // determine the bounding box of the domain
    findBoundingBox();
    
    // get initial exclusion radius
    for (unsigned int i = 0; i < numNodes; i++) {
        getExclusionRadius(nodes[i]);
    }
    cout << "Initializing variables : Done" << endl;
}

/*  LAGRIT: new routine will load vertices from polygon cmo
*/
void Polygon::loadVertices() {

    cout << "Adding vertices to polygon objection from " << mo_poly_name << endl;
    double *xptr;
    double *yptr;

    long icmolen;
    long iattlen;
    int ierror = 0;

    long nlen = 0;
    long ierr = 0;
    
    numVertices = lg_cmo_get_intinfo("nnodes", mo_poly_name);
    if (numVertices <= 0) {
        cout << "Error: There are no nodes in cmo:  " <<  mo_poly_name << endl;
        return;
    }
    cout << "There are " << numVertices << " nodes on the boundary of the polygon\n";
    // What are these?

    // get mesh object xic and yic data 
    iattlen = 3;
    fc_cmo_get_vdouble_(mo_poly_name,"xic",&xptr,&nlen,&ierr,icmolen,iattlen);
    if (ierr != 0 || nlen != numVertices){
        cout << "Error: get xic returns length " << nlen << " error: " << ierr << endl;
        return;
    }
    fc_cmo_get_vdouble_(mo_poly_name,"yic",&yptr,&nlen,&ierr,icmolen,iattlen);
    if (ierr != 0 || nlen != numVertices){
        cout << "ERROR: get yic returns length " << nlen << " error: " << ierr << endl;
        return;
    }

    // read in the node coordinates
    for (unsigned int i = 0; i < numVertices; i++) {
        // Format x-coord, y-coord
        nodes.push_back({*(xptr+i), *(yptr+i), 0});
    }
    cout << "Coordinates loaded from mesh object: " << endl;
    for (unsigned int i = 0; i < numVertices; i++) {
        printPoint(nodes[i]);
    } 
    cout << "Added vertices from cmo: " << mo_poly_name << " complete" << endl;
}


/*! Print node information to screen */
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
/*
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
*/
}
