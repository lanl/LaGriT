#include <iostream>
#include <string>
#include <fstream>
#include <limits>
#include <iomanip>
#include <cmath>

#include "poi_3D_domain.h"
#include "poi_helperFunctions.h"


/* from lagrit lg_ codes */
#include "lg_c_interface.h"
#include "lg_f_interface.h"
#include <stdio.h>
#include <cstring>


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
    cout << "\n\nLoading Distance field from LaGriT Mesh object : " << mo_dfield_name << endl;

    LG_ERR err = 0;
    double *xptr;
    double *yptr;
    long icmolen;
    long iattlen;
    int ierror = 0;
    long nlen = 0;
    long ierr = 0;
    // select the mo dfield mesh object
    string cmd_string = "cmo/select/" + string(mo_dfield_name);
    // cmd_string = "cmo/select/mo_poi_h_field";
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
    // err = lg_dotask("cmo/select/mo_h_field_pts");
    err = lg_dotask("cmo/status/brief");
    unsigned int dfieldNumNodes = lg_cmo_get_intinfo("nnodes", mo_dfield_name);
    cout << "\nNumber of nodes in the distance field: " << dfieldNumNodes << endl;
    
    if (dfieldNumNodes <= 0) {
        cout << "Error: There are no nodes in cmo:  " <<  mo_dfield_name << endl;
        return;
    }

    icmolen = strlen(mo_dfield_name);
    iattlen = 4;
    double dfXMax, dfYMax, dfZMax;
    double xMin, xMax = 0;
    double yMin, yMax = 0;
    double zMin, zMax = 0;
    fc_cmo_get_double_(mo_dfield_name, "xmin", &xMin, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "ymin", &yMin, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "zmin", &yMin, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "xmax", &xMax, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "ymax", &yMax, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "zmax", &yMax, &ierr, icmolen, iattlen);
    dfXMin = xMin;
    dfYMin = yMin;
    dfZMin = zMin;

    dfXMax = xMax;
    dfYMax = yMax;
    dfZMax = zMax;

    cout << "\nDistance Field number of cells. nx " << dfNumCellsX << " ny " << dfNumCellsY << " ny " << dfNumCellsZ << endl;
    cout << "Distance Field lower Bounds. xMin " << dfXMin << " yMin " << dfYMin << " zMin " << dfZMin  << endl;
    cout << "Distance Field upper Bounds. xMax " << dfYMax << " yMax " << dfYMax << " zMin " << dfZMin << endl;
    double deltaX = xMax - xMin;
    dfCellSize = deltaX / dfNumCellsX;
    // compute inversece cell size
    idfCellSize = 1.0 / dfCellSize;
    cout << "Distance Field Cell Size: " << dfCellSize << endl;
    cout << "Inverse Distance Field Cell Size: " << idfCellSize << endl;
    distanceField.reserve(dfNumCellsX * dfNumCellsY * dfNumCellsZ);

    for (unsigned int i = 0; i < dfNumCellsX * dfNumCellsY * dfNumCellsZ; i++) {
        distanceField.push_back(0);
    }
    
    char att[ ] = "h_field_att";
    double *hptr;
    iattlen = strlen(att);
    fc_cmo_get_vdouble_(mo_dfield_name, att, &hptr, &nlen, &ierr, icmolen, iattlen);
    unsigned int ptIndex = 0;

    for (unsigned int k = 0; k < dfNumCellsZ; k++){
        for (unsigned int j = 0; j < dfNumCellsY; j++) {
            for (unsigned int i = 0; i < dfNumCellsX; i++) {
                unsigned int linearIndex = i * (dfNumCellsY * dfNumCellsZ) + j*dfNumCellsZ + k;
                // distanceField[i][j] = *(hptr + ptIndex);
                distanceField[linearIndex] = *(hptr + ptIndex);
                ptIndex++;
                // cout << "i,j,k,distanceField " << i << " " << j << " " << k << " " << distanceField[linearIndex] << endl;
                if (distanceField[linearIndex] <= 0 ) {
                    cout << "Error. Resolution of 0 or negative number provided. Setting to h" << endl;
                    cout << "i,j,k,ptIndex " << i << " " << j << " " << k << " " << ptIndex << endl;
                    distanceField[linearIndex] = h;
                }
            }
        }
    }

    cout << "Loading Distance Field Complete\n" << endl;



}

//     string line;
//     ifstream distanceFieldFile;
//     vector<string> parsedLine;
//     distanceFieldFile.open(distanceFieldFilename, ifstream::in);
//     // checkIfOpen(inpFile, filename);
//     // read header
//     getline(distanceFieldFile, line);
//     parsedLine = splitOnWhiteSpace(line);
//     // get number of cells in x and y
//     dfNumCellsX = std::stoi(parsedLine[0]);
//     dfNumCellsY = std::stoi(parsedLine[1]);
//     dfNumCellsZ = std::stoi(parsedLine[2]);
//     cout << "Distance Field number of cells. nx " << dfNumCellsX << " " << "ny " << dfNumCellsY << "nz " << dfNumCellsZ << " " << endl;
//     getline(distanceFieldFile, line);
//     parsedLine = splitOnWhiteSpace(line);
//     // get minimum value for x and y
//     dfXMin = std::stod(parsedLine[0]);
//     dfYMin = std::stod(parsedLine[1]);
//     dfZMin = std::stod(parsedLine[2]);
//     cout << "Distance Field Lower Bounds. xMin " << dfXMin << " yMin " << dfYMin << " zMin " << dfZMin << endl;
//     getline(distanceFieldFile, line);
//     // get distance field cell size
//     h = std::stod(line);
//     dfCellSize = std::stod(line);
//     // inversece cell size
//     idfCellSize = 1.0 / dfCellSize;
//     cout << "Distance Field Cell Size " << dfCellSize << endl;
    
//     // allocate memory for distance field
//     try {
//         distanceField = new double**[dfNumCellsX];
        
//         for (unsigned int i = 0; i < dfNumCellsX + 1; i++) {
//             // Initialize all values as 0
//             distanceField[i] = new double*[dfNumCellsY + 1];
            
//             for (unsigned int j = 0; j < dfNumCellsY + 1; j++) {
//                 distanceField[i][j] = new double[dfNumCellsZ + 1]();
//             }
//         }
//     } catch (std::bad_alloc& ba) {
//         std::cerr << "Bad Allocation for distance Field " << ba.what() << endl;
//     }
    
//     // read file into the distance field array.
//     // Note indexing is set to fortran ()
//     for (unsigned int k = 0; k < dfNumCellsZ; k++) {
//         for (unsigned int j = 0; j < dfNumCellsY; j++) {
//             for (unsigned int i = 0; i < dfNumCellsX; i++) {
//                 getline(distanceFieldFile, line);
//                 distanceField[i][j][k] = std::stod(line);
//             }
//         }
//     }
    
//     cout << "Loading Distance Field Complete\n" << endl;
// }

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
    for (unsigned int k = 0; k < dfNumCellsZ; k++){
        for (unsigned int i = 0; i < dfNumCellsX; i++) {
            for (unsigned int j = 0; j < dfNumCellsX; j++) {
                unsigned int linearIndex = i * (dfNumCellsY * dfNumCellsZ) + j*dfNumCellsZ + k;
                fp << std::setprecision(12) << i*dfCellSize + dfXMin << "," <<  j*dfCellSize + dfYMin 
                    << "," << k*dfCellSize + dfZMin <<  distanceField[linearIndex] << endl;
            }
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
    unsigned int linearIndex = i * (dfNumCellsY * dfNumCellsZ) + j*dfNumCellsZ + k;
    point.radius = distanceField[linearIndex];
    //point.radius = h;
}
