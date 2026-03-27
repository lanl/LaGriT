
#include <iostream>
#include <fstream>
#include <vector>
#include <chrono>
#include <math.h>
#include <utility>
#include <string>
#include <random>
#include <sstream>

#include "poi_3D_domain.h"
#include "poi_helperFunctions.h"

using std::cout;
using std::endl;

/* from lagrit lg_ codes */
#include "lg_c_interface.h"
#include "lg_f_interface.h"

#include <stdio.h>
#include <cstring>

using std::cout;
using std::endl;


/* This code runs the near-maximal algorithm for Poisson-Disc Sampling
in two dimensions. Comprehensive detials are found in the published manuscript

Krotz, Johannes, Matthew R. Sweeney, Carl W. Gable, Jeffrey D. Hyman, and Juan M. Restrepo.
"Variable resolution Poisson-disk sampling for meshing discrete fracture networks."
Journal of Computational and Applied Mathematics (2022): 114094.

https://doi.org/10.1016/j.cam.2022.114094

Module author: Jeffrey D. Hyman (EES-16 / LANL)
Last update: 2 April 2024  
*/


extern "C" void poisson_3d_(char mo_poi_pts_out_in[LG_NAME_SIZE], char mo_poi_h_field_in[LG_NAME_SIZE], double *h,
                            double *xMin, double *xMax, double *yMin, double *yMax, double *zMin, double *zMax,
                            unsigned int *dfNumCellsX, unsigned int *dfNumCellsY, unsigned int *dfNumCellsZ,
                            int *seed, int *numSamples, int *resampleSweeps);

void poisson_3d_(char mo_poi_pts_out_in[LG_NAME_SIZE], char mo_poi_h_field_in[LG_NAME_SIZE], double *h,
                 double *xMin, double *xMax, double *yMin, double *yMax, double *zMin, double *zMax,
                 unsigned int *dfNumCellsX, unsigned int *dfNumCellsY, unsigned int *dfNumCellsZ,
                 int *seed, int *numSamples, int *resampleSweeps) {
    // remove white space passed in by LaGriT
    // char mo_poi_poly[LG_NAME_SIZE];
    // process_lagrit_string(mo_poi_poly_in, mo_poi_poly);
    // char mo_poi_pts_out[LG_NAME_SIZE];
    // process_lagrit_string(mo_poi_pts_out_in, mo_poi_pts_out);
    // char mo_poi_h_field[LG_NAME_SIZE];
    // process_lagrit_string(mo_poi_h_field_in, mo_poi_h_field);
    cout << "===== Begin Poisson 3D Sampling =========\n" << endl;
    Domain domain;
    char mo_poi_pts_out[LG_NAME_SIZE];
    process_lagrit_string(mo_poi_pts_out_in, mo_poi_pts_out);
    domain.mo_poi_pts_out = mo_poi_pts_out;
    cout << "Writing points to mesh object " << domain.mo_poi_pts_out << endl;
    char mo_poi_h_field[LG_NAME_SIZE];
    process_lagrit_string(mo_poi_h_field_in, mo_poi_h_field);
    domain.mo_dfield_name = mo_poi_h_field;
    cout << "Loading distance field information from " << domain.mo_dfield_name  << endl;
    domain.h = *h;
    domain.xMin = *xMin;
    domain.xMax = *xMax;
    domain.yMin = *yMin;
    domain.yMax = *yMax;
    domain.zMin = *zMin;
    domain.zMax = *zMax;
    domain.dfNumCellsX = *dfNumCellsX;
    domain.dfNumCellsY = *dfNumCellsY;
    domain.dfNumCellsZ = *dfNumCellsZ;
    domain.numSamples = *numSamples;
    domain.resampleSweeps = *resampleSweeps;
    domain.seed = *seed;

    cout << "---------------------------------------" << endl;
    cout << "Poisson Disc Sampling Parameters:" << endl;
    cout << "h for sampling:\t\t\t\t" << domain.h << "\n" << endl;
    cout << "xMin:\t\t\t\t\t" << domain.xMin << endl;
    cout << "xMax:\t\t\t\t\t" << domain.xMax << endl;
    cout << "yMin:\t\t\t\t\t" << domain.yMin << endl;
    cout << "yMax:\t\t\t\t\t" << domain.yMax << endl;
    cout << "zMin:\t\t\t\t\t" << domain.zMin << endl;
    cout << "zMax:\t\t\t\t\t" << domain.zMax << "\n" << endl;
    cout << "Number of samples:\t\t" << domain.numSamples << endl;
    cout << "Number of resample sweeps:\t" << domain.resampleSweeps << endl;
    cout << "Seed for generator:\t\t" << domain.seed << endl;
    cout << endl;
    cout << "Distance field information:" << endl;
    cout << "npx: " << domain.dfNumCellsX << endl;
    cout << "npy: " << domain.dfNumCellsY << endl;
    cout << "npz: " << domain.dfNumCellsZ << endl;
    cout << "---------------------------------------" << endl;
    domain.initializeRandomGenerator(domain.seed);
    domain.setBoundary();
    // domain.printNodes();
    
    domain.loadDistanceField();
    domain.initializeVariables();
    domain.initializeNeighborGrid();
    domain.sampleEdges();
    // resample over faces a few times
    for (unsigned int i = 0; i < 4; i++){
        domain.sampleFaces();
    }

    domain.mainSampling(0, false);
    domain.findEmptyCells();
    domain.fillEmptyCells();
    domain.resample();
    // // domain.printNodes();
    domain.addNodesToMeshObject();
    cout << "\n===== Finished Poisson 3D Sampling =========\n\n" << endl;
}
