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

#ifdef __cplusplus
extern "C" {
#endif
void poisson_2d_();
#ifdef __cplusplus
}
#endif



#ifdef __cplusplus
extern "C" {
#endif
void poisson_2d_ () {
 

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

    int i = 0;
    int ierror = 0;

    printf("\n===== Enter Driver for poisson_2d  =========\n\n");

    printf("-----------------------------------\n");
    printf("Report LaGriT CMO for Polygon:\n\n");
    // get data from current polygon mesh object
    char cmo_name[32];
    err = lg_cmo_get_name(cmo_name, 32);

    if (err != LG_ERR_SUCCESS) {
        printf("Failed to get cmo name\n");
        return;
    } 

    int nnodes = lg_cmo_get_intinfo("nnodes", cmo_name);

    if (nnodes <= 0) {
        printf("ERROR: No nodes in cmo: '%s'\n", cmo_name);
        return;
    }

    int nelements = lg_cmo_get_intinfo("nelements", cmo_name);
    int ndim = lg_cmo_get_intinfo("ndimensions_topo", cmo_name);
    int ndim_geom = lg_cmo_get_intinfo("ndimensions_geom", cmo_name);

    if (err == LG_ERR_SUCCESS) {
        printf("Mesh Data for cmo: '%s'\n", cmo_name);
        printf("nnodes: %d\n", nnodes);
        printf("nelements: %d\n", nelements);
        printf("ndimensions_topo: %d\n", ndim);
        printf("ndimensions_geom: %d\n", ndim_geom);
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

    printf("xmin xmax: %f  %f\n", xmin, xmax);
    printf("ymin ymax: %f  %f\n", ymin, ymax);
    printf("zmin zmax: %f  %f\n", zmin, zmax);

    // get mesh object xic and yic data 
    iattlen = 3;
    fc_cmo_get_vdouble_(cmo_name,"xic",&xptr,&nlen,&ierr,icmolen,iattlen);
    if (ierr != 0 || nlen != nnodes){
        printf("ERROR: get xic returns length %d, error: %4d\n",nlen,ierr);
        return;
    }
    fc_cmo_get_vdouble_(cmo_name,"yic",&yptr,&nlen,&ierr,icmolen,iattlen);
    if (ierr != 0 || nlen != nnodes){
        printf("ERROR: get yic returns length %d, error: %4d\n",nlen,ierr);
        return;
    }

    // report x,y coordinate data
    printf("nodes:\n");
    for( i = 0; i < nnodes; i = i + 1 ){
        printf("%6d %12.4f %12.4f \n",i+1, *(xptr+i), *(yptr+i) );
    }
    printf("\n");

    printf("\n===== Begin poisson 2d  =========\n\n");

    Polygon polygon;

    // read and load command line arguments
    // arg 1: file or cmo name for polygon boundary
    // arg 2: output filename
    // arg 3: h (minimum mesh resolution)
    // arg 4: file or cmo name for distance field

    int nargs = 5; 
    char *buff[5] = {"poisson","poi_poly","poi_out.txt","1","resolution_1.dat"}; 

    printf("-----------------------------------\n");
    if (!polygon.parseCommandLine(nargs, buff)) {
        return;
    };

    printf("-----------------------------------\n");
    // Load distance field from file (distanceField.cpp)
    polygon.loadDistanceField();

    printf("-----------------------------------\n");
    // load polygon vertices (polygon.cpp)
    polygon.loadVerticesCMO();



    return;
}
#ifdef __cplusplus
}
#endif


