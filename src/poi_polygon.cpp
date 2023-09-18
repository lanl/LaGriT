#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <limits>
#include <stdio.h>
#include <cstring>

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
using std::string;


/*! Computes the 2D bounding box of the polygon */
void Polygon::findBoundingBox() {
    cout << "Finding bounding Box of polygon: Starting " << endl;
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
    
    cout << "Polygon Bounding Box is\n\tx-min: " << xMin << ", x-max: " << xMax << "\n\ty-min: " << yMin << ", y-max: " << yMax << endl;
    cout << "Finding bounding box : Complete" << endl;
}

/*! Initializes variables of the polygon */
void Polygon::initializeVariables() {
    cout << "---------------------------------------" << endl;
    cout << "Initializing variables : Starting " << endl;
    // initialize the number of nodes in the point distribution to be the inital number of vertices
    numNodes = numVertices;
    // determine the bounding box of the domain
    findBoundingBox();
    // Preallocate a little memory
    double deltaX = xMax - xMin;
    double deltaY = yMax - yMin;
    double max_nodes = (1./h)*(1./h)*deltaX * deltaY;
    //cout << "delta X" << deltaX << endl;
    //cout << "delta Y" << deltaY << endl;
    //cout << "h" << h << endl;
    //cout << "max nodes " << max_nodes << endl;
    nodes.reserve(max_nodes);
 
    // get initial exclusion radius
    for (unsigned int i = 0; i < numNodes; i++) {
        getExclusionRadius(nodes[i]);
    }
    
    cout << "Initializing variables : Done\n" << endl;
    cout << "---------------------------------------" << endl;
}

/*  LAGRIT: new routine will load vertices from polygon cmo
*/
bool Polygon::loadVertices() {
    cout << "\nImporting vertices to polygon objection from " << mo_poly_name << endl;
    double *xptr;
    double *yptr;
    double *zptr;
    long icmolen;
    long iattlen;
    int ierror = 0;
    long nlen = 0;
    long ierr = 0;
    LG_ERR err = 0;
    // cout << strlen(mo_poly_name) << endl;
    // cout << "cmo name: " << mo_poly_name << endl;
    numVertices = lg_cmo_get_intinfo("nnodes", mo_poly_name);
    
    if (numVertices <= 0) {
        cout << "Error: There are no nodes in cmo:  " <<  mo_poly_name << endl;
        return false;
    }
    
    //cout << "There are " << numVertices << " nodes on the boundary of the polygon\n";
    int nelements = lg_cmo_get_intinfo("nelements", mo_poly_name);
    int ndim = lg_cmo_get_intinfo("ndimensions_topo", mo_poly_name);
    int ndim_geom = lg_cmo_get_intinfo("ndimensions_geom", mo_poly_name);
    
    if (err == LG_ERR_SUCCESS) {
        cout << "---------------------------------------" << endl;
        cout << "Mesh Data for cmo: '" << mo_poly_name << "'" << endl;
        cout << "nnodes: " << numVertices << endl;
        cout << "nelements: " <<  nelements << endl;
        cout << "ndimensions_topo: " <<  ndim << endl;
        cout << "ndimensions_geom: " <<  ndim_geom << endl;
        cout << "---------------------------------------" << endl;
    }
    
    // get length of
    icmolen = strlen(mo_poly_name);
    // What are these?
    // get mesh object xic and yic data
    iattlen = 3;
    //cout << "Reading in x coords" << endl;
    fc_cmo_get_vdouble_(mo_poly_name, "xic", &xptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numVertices) {
        cout << "Error: get xic returns length " << nlen << " error: " << ierr << endl;
        return false;
    }
    
    //cout << "Reading in y coords" << endl;
    fc_cmo_get_vdouble_(mo_poly_name, "yic", &yptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numVertices) {
        cout << "Error: get yic returns length " << nlen << " error: " << ierr << endl;
        return false;
    }
    //cout << "Reading in z coords" << endl;
    fc_cmo_get_vdouble_(mo_poly_name, "zic", &zptr, &nlen, &ierr, icmolen, iattlen);
    if (ierr != 0 || nlen != numVertices) {
        cout << "Error: get zic returns length " << nlen << " error: " << ierr << endl;
        return false;
    }
    // read in the z value, we assign all nodes the same z value in the end. 
    zValue = *(zptr);
    cout << "Z-coordinate: " << zValue << endl;
    
    // read in the node coordinates
    Point tmpPoint;
    
    for (unsigned int i = 0; i < numVertices; i++) {
        // Format x-coord, y-coord, radius
        tmpPoint.x = *(xptr + i);
        tmpPoint.y = *(yptr + i);
        tmpPoint.radius = 0;
        tmpPoint.ix = 0;
        tmpPoint.iy = 0;
        tmpPoint.nodeNum = i;
        nodes.push_back(tmpPoint);
    }
    
    // cout << "Coordinates loaded from mesh object: " << endl;
    /*
    for (unsigned int i = 0; i < numVertices; i++) {
        printPoint(nodes[i]);
    }
    */
    cout << "Importing vertices from cmo: " << mo_poly_name << " to polygon obejct complete" << endl;
    return true;
}


/* adds nodes from sampling to mo_pts mesh object*/
void Polygon::addNodesToMeshObject() {
    cout << "Adding nodes to " << mo_pts_name << " mesh object" << endl;
    LG_ERR err;
    // Create strings for commands and conver them into chars. Kinda ugly, could be cleaned up.
    // Create new mesh object for points
    string cmd_string = "cmo/create/" + string(mo_pts_name) + "/"  + std::to_string(numNodes) + "/0/triplane";
    // cout << cmd_string << endl;
    // // set the cmo to be the empty point mesh object
    int n = cmd_string.length();
    // // declaring character array
    char *cmd_char;
    cmd_char = new char[n + 1];
    // // copying the contents of the
    // // string to char array
    strcpy(cmd_char, cmd_string.c_str());
    err = lg_dotask(cmd_char);
    cmd_string = "cmo/select/" + string(mo_pts_name);
    // cout << cmd_string << endl;
    // // set the cmo to be the empty point mesh object
    n = cmd_string.length();
    // // declaring character array
    cmd_char = new char[n + 1];
    // // copying the contents of the
    // // string to char array
    strcpy(cmd_char, cmd_string.c_str());
    err = lg_dotask(cmd_char);
    //err = lg_dotask("cmo/status/brief");
    double *xptr;
    double *yptr;
    double *zptr;
    long icmolen;
    long iattlen;
    int ierror = 0;
    long nlen = 0;
    long ierr = 0;
    // get length of mesh object name
    icmolen = strlen(mo_pts_name);
    // What are these?
    // get mesh object xic and yic data
    iattlen = 3;
    fc_cmo_get_vdouble_(mo_pts_name, "xic", &xptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numNodes) {
        cout << "Error: get xic returns length " << nlen << " error: " << ierr << endl;
    }
    
    fc_cmo_get_vdouble_(mo_pts_name, "yic", &yptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numNodes) {
        cout << "Error: get yic returns length " << nlen << " error: " << ierr << endl;
    }
    
    fc_cmo_get_vdouble_(mo_pts_name, "zic", &zptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numNodes) {
        cout << "Error: get zic returns length " << nlen << " error: " << ierr << endl;
    }
    
    // // Copy coordinates from polygon object into the mesh object
    for (unsigned int i = 0; i < numNodes; i++) {
        *(xptr + i) = nodes[i].x;
        *(yptr + i) = nodes[i].y;
        *(zptr + i) = zValue;
    }
    
    err = lg_dotask("cmo/status/brief");
    err = lg_dotask("cmo/printatt/-def-/-xyz-/minmax");
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

/*! Constructor for polygon class.
*/
Polygon::Polygon(){
}

/*! Destructor for polygon class.
* Clears the memory allocated for the background grid
* Erase vectors on the polygon class
*/
Polygon::~Polygon() {
    cout << "---------------------------------" << endl;
    cout << "Cleaning up polygon " << endl;
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
    */
    cout << "Cleaning up polygon complete" << endl;
}
