#include <iostream>
#include <string>
#include <fstream>
#include <limits>
#include <iomanip>

#include "poi_polygon.h"
#include "poi_helperFunctions.h"

/* from lagrit lg_ codes */
#include "lg_c_interface.h"
#include "lg_f_interface.h"
#include <stdio.h>
#include <cstring>


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
void Polygon::loadDistanceFieldCMO() {
    cout << "\n\nLoading Distance field from LaGriT Mesh object : " << mo_dfield_name << endl;
    LG_ERR err = 0;
    double *xptr;
    double *yptr;
    long icmolen;
    long iattlen;
    int ierror = 0;
    long nlen = 0;
    long ierr = 0;

    // Select the dfield mesh object as the cmo
    err = lg_dotask("cmo/select/mo_h_field_pts");
    err = lg_dotask("cmo/status/brief");

    unsigned int dfieldNumNodes = lg_cmo_get_intinfo("nnodes", mo_dfield_name);
    cout << "number of nodes in the distance field: " << dfieldNumNodes << endl;
    if (dfieldNumNodes <= 0) {
        cout << "Error: There are no nodes in cmo:  " <<  mo_dfield_name << endl;
        return;
    }

    // fc_cmo_get_vdouble_(mo_dfield_name,"xic",&xptr,&nlen,&ierr,icmolen,iattlen);
    // if (ierr != 0 || nlen != dfieldNumNodes){
    //     cout << "Error: get xic returns length " << nlen << " error: " << ierr << endl;
    //     return;
    // }
    // fc_cmo_get_vdouble_(mo_dfield_name,"yic",&yptr,&nlen,&ierr,icmolen,iattlen);
    // if (ierr != 0 || nlen != dfieldNumNodes){
    //     cout << "ERROR: get yic returns length " << nlen << " error: " << ierr << endl;
    //     return;
    // }
    icmolen = strlen(mo_dfield_name);
    iattlen = 4;
    double dfXMax, dfYMax;
    double xMin, xMax = 0;
    double yMin, yMax = 0;
    fc_cmo_get_double_(mo_dfield_name, "xmin", &xMin, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "ymin", &yMin, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "xmax", &xMax, &ierr, icmolen, iattlen);
    fc_cmo_get_double_(mo_dfield_name, "ymax", &yMax, &ierr, icmolen, iattlen);
    dfXMin = xMin;
    dfYMin = yMin;
    dfXMax = xMax;
    dfYMax = yMax;
    cout << "Distance Field number of cells. nx " << dfNumCellsX << " " << "ny " << dfNumCellsY << endl;
    cout << "Distance Field Lower Bounds. xMin " << dfXMin << " " << "yMin " << dfYMin  << endl;
    cout << "Distance Field upper Bounds. xMax " << dfYMax << " " << "yMax " << dfYMax  << endl;
    double deltaX = xMax - xMin;
    dfCellSize = deltaX / dfNumCellsX;
    // compute inversece cell size
    idfCellSize = 1.0 / dfCellSize;
    cout << "Distance Field Cell Size " << dfCellSize << endl;
    cout << "Inverse Distance Field Cell Size " << idfCellSize << endl;

    // allocate memory for distance field
    try {
        distanceField = new double*[dfNumCellsX];
        for (unsigned int i = 0; i < dfNumCellsX + 1; i++) {
            // Initialize all values as 0
            distanceField[i] = new double[dfNumCellsY]();
        }
    } catch (std::bad_alloc& ba) {
        std::cerr << "Bad Allocation for distance Field " << ba.what() << endl;
    }

    /* Get the resolution field from the mesh object.
    // Name of attribute on mesh object is 'h_field_att'
    */
    char att[ ] = "h_field_att";
    double *hptr;
    iattlen = strlen(att);
    fc_cmo_get_vdouble_(mo_dfield_name, att, &hptr, &nlen, &ierr, icmolen, iattlen);
    // cout << "h_field_att nlength: " <<  nlen << endl;
    // for(unsigned int i = 0; i < dfieldNumNodes; i++) {
    //     cout << "i: " << i << " h-att: " << *(hptr + i) << endl;
    // }
    // Stuff the resolution field into the dfield array
    // Need to check index order here, (i,j) vs (j,i)
    // cout << "--> populating resolution field" << endl;
    unsigned int ptIndex = 0;

    for (unsigned int j = 0; j < dfNumCellsY; j++) {
        for (unsigned int i = 0; i < dfNumCellsX; i++) {
            distanceField[i][j] = *(hptr + ptIndex);
            ptIndex++;

//            cout << "i,j,distanceField[i][j] " << i << " " << j << " " << distanceField[i][j] << endl;
            if (distanceField[i][j] <= 0 ) {
                cout << "Error. Resolution of 0 or negative number provided. Setting to h" << endl;
                cout << "i,j,ptIndex " << i << " " << j << " " << ptIndex << endl;
                distanceField[i][j] = h;
            }
        }
    }

    // Use this to take a look at the dfield
    dumpDistanceField();
    cout << "Loading Distance Field Complete\n" << endl;
}

/*
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
*/


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
    unsigned int i = int(floorf((x - xMin) * idfCellSize));
    return i;
}

/*! Returns the local exclution radius by a look up table in the
distance field array.
*/
void Polygon::getExclusionRadius(Point &point) {
    unsigned int i = getDFCellID(point.x, dfXMin);
    unsigned int j = getDFCellID(point.y, dfYMin);
    point.radius = distanceField[i][j];

//    if (i > dfNumCellsX || j > dfNumCellsY){
//        cout << i << " " << j << endl;
//        printPoint(point);
//    }

}
