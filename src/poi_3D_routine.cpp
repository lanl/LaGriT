
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

using std::cout;
using std::endl;

/* from lagrit lg_ codes */
#include "lg_c_interface.h"
#include "lg_f_interface.h"

#include <stdio.h>
#include <cstring>

using std::cout;
using std::endl;

const int LG_NAME_SIZE = 32;

/* This code runs the near-maximal algorithm for Poisson-Disc Sampling
in two dimensions. Comprehensive detials are found in the published manuscript

Krotz, Johannes, Matthew R. Sweeney, Carl W. Gable, Jeffrey D. Hyman, and Juan M. Restrepo.
"Variable resolution Poisson-disk sampling for meshing discrete fracture networks."
Journal of Computational and Applied Mathematics (2022): 114094.

https://doi.org/10.1016/j.cam.2022.114094

Module author: Jeffrey D. Hyman (EES-16 / LANL)
Last update; 22 March 2022
*/

// int main (int argc, char **argv) {
//     std::cout << "============================\nPoisson Disc Sampling\n============================\n";
//     std::time_t result = std::time(nullptr);
//     std::cout << "Starting Time Stamp: " << std::asctime(std::localtime(&result)) << "\n\n\n";
//     Domain domain;
//     domain.parseCommandLine(argc, argv);
//     // load nodes
//     // double xMin, xMax, yMin, yMax, zMin, zMax;
//     // xMin = -50;
//     // xMax = 50;
//     // yMin = -50;
//     // yMax = 50;
//     // zMin = -5;
//     // zMax = 5;
//     unsigned int seed;
//     seed = 10;
//     domain.initializeRandomGenerator(seed);
//     domain.setBoundary();
//     domain.numVertices = 8;
//     domain.numSamples = 10;
//     domain.resampleSweeps = 2;
//     domain.loadDistanceField();
//     domain.initializeVariables();
//     domain.initializeNeighborGrid();
//     domain.sampleEdges();
//     cout << "Number of nodes " << domain.numNodes << endl;
//     domain.sampleFaces();
//     domain.mainSampling(0, false);
//     domain.findEmptyCells();
//     domain.fillEmptyCells();
//     domain.resample();
//     // // domain.printNodes();
//     domain.dumpNodes();
//     result = std::time(nullptr);
//     std::cout << "\nFinishing Time Stamp: " << std::asctime(std::localtime(&result)) << endl;
//     return 0;
// }

extern "C" void poisson_3d_(char mo_poi_pts_out[LG_NAME_SIZE]);

void poisson_3d_(char mo_poi_pts_out[LG_NAME_SIZE]) {

    // remove white space passed in by LaGriT
    // char mo_poi_poly[LG_NAME_SIZE];
    // process_lagrit_string(mo_poi_poly_in, mo_poi_poly);
    // char mo_poi_pts_out[LG_NAME_SIZE];
    // process_lagrit_string(mo_poi_pts_out_in, mo_poi_pts_out);
    // char mo_poi_h_field[LG_NAME_SIZE];
    // process_lagrit_string(mo_poi_h_field_in, mo_poi_h_field);
    cout << "===== Begin Poisson 3D Sampling =========\n\n" << endl;

}
