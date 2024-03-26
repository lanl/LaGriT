#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <limits>
#include <cstring>
#include <stdio.h>

#include "poi_3D_domain.h"
#include "poi_3D_sampling.h"
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

/* Parse command line and add variable to the polygon object.
*/
// bool Domain::parseCommandLine(int argc, char **argv) {
//     distanceFieldFilename  = argv[1];
//     cout << "Distance Field Filename " << distanceFieldFilename << endl;
//     // arg 2: output filename
//     outputFilename = argv[2];
//     cout << "Writting points to " << outputFilename << endl;
//     // Read in boundaries of the box 
//     xMin = atof(argv[3]);
//     xMax = atof(argv[4]);
//     yMin = atof(argv[5]);
//     yMax = atof(argv[6]);
//     zMin = atof(argv[7]);
//     zMax = atof(argv[8]);

//     // arg 5: number of samples initialized by accepted point
//     numSamples = 10;
//     cout << "Number of samples " << numSamples << endl;
//     // arg 6: number of re-sampling sweeps. 1 is typically sufficent
//     resampleSweeps = 1;
//     cout << "Number of resample sweeps " << resampleSweeps << endl;
//     return true;
// }

/*! Computes the 2D bounding box of the polygon */
void Domain::findBoundingBox() {
    xMax = nodes[0].x;
    xMin = nodes[0].x;
    yMax = nodes[0].y;
    yMin = nodes[0].y;
    zMax = nodes[0].z;
    zMin = nodes[0].z;
    
    for (unsigned int i = 1; i < numNodes; i++) {
        xMax = std::max(xMax, nodes[i].x);
        xMin = std::min(xMin, nodes[i].x);
        yMax = std::max(yMax, nodes[i].y);
        yMin = std::min(yMin, nodes[i].y);
        zMax = std::max(zMax, nodes[i].z);
        zMin = std::min(zMin, nodes[i].z);
    }
    
    cout << "Domain Bounding Box\nx-min: " << xMin << ", x-max: " << xMax;
    cout << "\ny-min: " << yMin << ", y-max: " << yMax;
    cout << "\nz-min: " << zMin << ", z-max: " << zMax << endl;
}

void Domain::setBoundary() {
    // set corners of the domain
    nodes.push_back({xMin, yMin, zMin, h});
    nodes.push_back({xMin, yMax, zMin, h});
    nodes.push_back({xMax, yMax, zMin, h});
    nodes.push_back({xMax, yMin, zMin, h});
    nodes.push_back({xMin, yMin, zMax, h});
    nodes.push_back({xMin, yMax, zMax, h});
    nodes.push_back({xMax, yMax, zMax, h});
    nodes.push_back({xMax, yMin, zMax, h});
    // set domain edge connectivity
    setEdges();
}

void Domain::setEdges() {
    // // assign edges
    // // bottom
    edges.push_back({0, 1});
    edges.push_back({0, 3});
    edges.push_back({0, 4});
//    domain.edges.push_back({1,0});
    edges.push_back({1, 2});
    edges.push_back({1, 5});
//    domain.edges.push_back({2,1});
    edges.push_back({2, 3});
    edges.push_back({2, 6});
//    domain.edges.push_back({3,0});
//    domain.edges.push_back({3,2});
    edges.push_back({3, 7});
    // // top
    //domain.edges.push_back({4,0});
    edges.push_back({4, 5});
    edges.push_back({4, 7});
    //domain.edges.push_back({5, 4});
    edges.push_back({5, 6});
    //domain.edges.push_back({5,1});
    //domain.edges.push_back({6,5});
    edges.push_back({6, 7});
    //domain.edges.push_back({6,2});
    //domain.edges.push_back({7,4});
    //domain.edges.push_back({7,6});
    //domain.edges.push_back({7,3});
}
/*! Initializes variables of the polygon */
void Domain::initializeVariables() {
    cout << "Initializing variables " << endl;
    // initialize the number of nodes in the point distribution to be the inital number of vertices
    numNodes = numVertices;
    // determine the bounding box of the domain
    // findBoundingBox();
    double deltaX = xMax - xMin;
    double deltaY = yMax - yMin;
    double deltaZ = zMax - zMin;
    double maxNodes = (1. / h) * (1. / h) * (1. / h) * deltaX * deltaY * deltaZ;
    nodes.reserve(maxNodes);
    
    // get initial exclusion radius
    for (unsigned int i = 0; i < numNodes; i++) {
        getExclusionRadius(nodes[i]);
    }
    
    cout << "Initializing variables complete" << endl;
}

/*! Load vertices from the avs file
*/
// void Domain::loadVertices() {
//     cout << "Reading vertices from " << inputFilename << endl;
//     string line;
//     ifstream inpFile;
//     vector<string> parsedLine;
//     inpFile.open(inputFilename, ifstream::in);
//     // checkIfOpen(inpFile, filename);
//     // read header
//     getline(inpFile, line);
//     parsedLine = splitOnWhiteSpace(line);
//     numVertices = std::stoi(parsedLine[0]);
//     cout << "There are " << numVertices << " nodes on the boundary of the polygon\n";
    
//     // read in the node coordinates
//     for (unsigned int i = 0; i < numVertices; i++) {
//         getline(inpFile, line);
//         parsedLine = splitOnWhiteSpace(line);
//         // Format Node Number, x-coord, y-coord,
//         nodes.push_back({std::stod(parsedLine[1]), std::stod(parsedLine[2]), 0});
//     }
    
//     inpFile.close();
//     cout << "Reading vertices from " << inputFilename << " complete" << endl;
// }

/*! Print node information to screen*/
void Domain::printNodes() {
    for (unsigned int i = 0; i < numNodes; i++) {
        printPoint(nodes[i]);
    }
}

/* adds nodes from sampling to mo_pts mesh object*/
void Domain::addNodesToMeshObject() {
    cout << "Adding nodes to " << mo_poi_pts_out << " mesh object" << endl;
    LG_ERR err;
    // Create strings for commands and conver them into chars. Kinda ugly, could be cleaned up.
    // Create new mesh object for points
    string cmd_string = "cmo/create/" + string(mo_poi_pts_out) + "/" + std::to_string(numNodes) + "/0/tet";
    cout << cmd_string << endl;
    // // set the cmo to be the empty point mesh object
    int n = cmd_string.length();
    // // declaring character array
    char *cmd_char;
    cmd_char = new char[n + 1];
    // // copying the contents of the
    // // string to char array
    strcpy(cmd_char, cmd_string.c_str());
    cout << "calling do task" << endl;
    err = lg_dotask(cmd_char);
    cmd_string = "cmo/select/" + string(mo_poi_pts_out);
    cout << cmd_string << endl;
    // // set the cmo to be the empty point mesh object
    n = cmd_string.length();
    // // declaring character array
    cmd_char = new char[n + 1];
    // // copying the contents of the
    // // string to char array
    strcpy(cmd_char, cmd_string.c_str());
    err = lg_dotask(cmd_char);
    err = lg_dotask("cmo/status/brief");
    double *xptr;
    double *yptr;
    double *zptr;
    long icmolen;
    long iattlen;
    int ierror = 0;
    long nlen = 0;
    long ierr = 0;
    // get length of mesh object name
    icmolen = strlen(mo_poi_pts_out);
    // What are these?
    // get mesh object xic and yic data
    iattlen = 3;
    fc_cmo_get_vdouble_(mo_poi_pts_out, "xic", &xptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numNodes) {
        cout << "Error: get xic returns length " << nlen << " error: " << ierr << endl;
    }
    
    fc_cmo_get_vdouble_(mo_poi_pts_out, "yic", &yptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numNodes) {
        cout << "Error: get yic returns length " << nlen << " error: " << ierr << endl;
    }
    
    fc_cmo_get_vdouble_(mo_poi_pts_out, "zic", &zptr, &nlen, &ierr, icmolen, iattlen);
    
    if (ierr != 0 || nlen != numNodes) {
        cout << "Error: get zic returns length " << nlen << " error: " << ierr << endl;
    }
    
    // // Copy coordinates from polygon object into the mesh object
    for (unsigned int i = 0; i < numNodes; i++) {
        *(xptr + i) = nodes[i].x;
        *(yptr + i) = nodes[i].y;
        *(zptr + i) = nodes[i].z;
    }
    
    err = lg_dotask("cmo/status/brief");
    err = lg_dotask("cmo/printatt/-def-/-xyz-/minmax");
    err = lg_dotask("dump/tmp.inp/mo_out");

}


/*! Writes points to file
* format is x, y, flag
*/
// void Domain::dumpNodes() {
//     std::ofstream fp;
//     cout << "Writing points to file: " << outputFilename << endl;
//     cout << "There are " << numNodes << " point in the final distribution" << endl;
//     fp.open(outputFilename.c_str(), std::ofstream::out | std::ofstream::trunc);
    
//     for (unsigned int i = 0; i < numNodes; i++) {
//         fp << std::setprecision(12) << nodes[i].x << " " << nodes[i].y << " " << nodes[i].z << endl;
//     }
    
//     fp.close();
// }


/*! Destructor for polygon class.
* Clears the memory allocated for the background grid
* Erase vectors on the polygon class
*/
Domain::~Domain() {
    cout << "Cleaning up domain" << endl;
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
    cout << "Cleaning up domain complete" << endl;
}
