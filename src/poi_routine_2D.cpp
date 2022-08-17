/* from Jeffrey's main.cpp */

#include <iostream>
#include <fstream>
#include <vector>
#include <chrono>
#include <math.h>
#include <utility>
#include <string>
#include <random>
#include <sstream>
#include "poi_polygon.h"

/* from lagrit lg_ codes */
#include "lg_c_interface.h"
#include "lg_f_interface.h"
#include <stdio.h>
#include <cstring>

using std::cout;
using std::endl;

extern "C" void poisson_2d_(double h);

void poisson_2d_(double h) {
 
    cout << "h: " << h << endl;

    LG_ERR err = 0;
    double *xptr;
    double *yptr;
    long *iptr;
    double xmin = 0;
    double xmax = 0;
    double ymin = 0;
    double ymax = 0;
    double zmin = 0;
    double zmax = 0;

    long nlen = 0;
    long nelem = 0;
    long ierr = 0;

    long icmolen;
    long iattlen;
    int ierror = 0;

    cout << "\n===== Enter Driver for poisson_2d  =========\n\n" << endl;
    cout << "-----------------------------------\n" << endl;
    cout << "Report LaGriT CMO for Polygon:\n\n" << endl;
    // get data from current polygon mesh object
    char cmo_name[32];
    err = lg_cmo_get_name(cmo_name, 32);
    if (err != LG_ERR_SUCCESS) {
        cout << "Failed to get cmo name" << endl;
        return;
    } 
    unsigned int nnodes = lg_cmo_get_intinfo("nnodes", cmo_name);
    if (nnodes <= 0) {
        cout << "ERROR: No nodes in cmo: '" << cmo_name << "'" << endl;
        return;
    }

    int nelements = lg_cmo_get_intinfo("nelements", cmo_name);
    int ndim = lg_cmo_get_intinfo("ndimensions_topo", cmo_name);
    int ndim_geom = lg_cmo_get_intinfo("ndimensions_geom", cmo_name);
    if (err == LG_ERR_SUCCESS) {
        cout << "Mesh Data for cmo: '" << cmo_name << "'"<< endl;
        cout << "nnodes: " << nnodes << endl;
        cout << "nelements: " <<  nelements << endl;
        cout << "ndimensions_topo: " <<  ndim << endl;
        cout << "ndimensions_geom: " <<  ndim_geom << endl;
    }
    // get polygon xy minmax
    icmolen = strlen(cmo_name);
    iattlen = 4;
    fc_cmo_get_double_(cmo_name,"xmin",&xmin,&ierr,icmolen,iattlen);
    fc_cmo_get_double_(cmo_name,"xmax",&xmax,&ierr,icmolen,iattlen);
    fc_cmo_get_double_(cmo_name,"ymin",&ymin,&ierr,icmolen,iattlen);
    fc_cmo_get_double_(cmo_name,"ymax",&ymax,&ierr,icmolen,iattlen);
    fc_cmo_get_double_(cmo_name,"zmin",&zmin,&ierr,icmolen,iattlen);
    fc_cmo_get_double_(cmo_name,"zmax",&zmax,&ierr,icmolen,iattlen);

    cout << "xmin: " << xmin << ", xmax: " << xmax << endl;
    cout << "ymin: " << xmin << ", ymax: " << xmax << endl;
    cout << "zmin: " << xmin << ", zmax: " << xmax << endl;

    // get mesh object xic and yic data 
    iattlen = 3;
    fc_cmo_get_vdouble_(cmo_name,"xic",&xptr,&nlen,&ierr,icmolen,iattlen);
    if (ierr != 0 || nlen != nnodes){
        cout << "ERROR: get xic returns length " << nlen << " error: " << ierr << endl;
        return;
    }
    fc_cmo_get_vdouble_(cmo_name,"yic",&yptr,&nlen,&ierr,icmolen,iattlen);
    if (ierr != 0 || nlen != nnodes){
        cout << "ERROR: get yic returns length " << nlen << " error: " << ierr << endl;
        return;
    }

    // report x,y coordinate data
    cout << "nodes:" << endl;
    for(int i = 0; i < nnodes; i++ ){
        cout << i+1 << " " << *(xptr+i) << " " << *(yptr+i) << endl;
    }

    cout << "===== Begin poisson 2d  =========\n\n" << endl;
    Polygon polygon;
    // This needs to be passed in somehow
    polygon.h = h;
    cout << "h for sampling: " << polygon.h << endl;
    // Defaults are set in poi_polygon.h, should be overloaded at some point in the function call
    polygon.numSamples = 10;
    polygon.resampleSweeps = 1;

    // load polygon vertices (polygon.cpp)
    polygon.loadVerticesCMO();

    // Load distance field from file (distanceField.cpp)
    //polygon.loadDistanceField();
    err = lg_dotask("cmo / status / brief");

    polygon.outputFilename = "points.xyz";
    polygon.dumpNodes();

    return;
}
// #ifdef __cplusplus
// }
// #endif


