#ifndef _sampling_h_
#define _sampling_h_
#include <fstream>
#include <vector>
#include <random>

#include "poi_polygon.h"

const int LG_NAME_SIZE = 32;

extern "C" void poisson_2d_(const char *mo_poly_name, const char *mo_pts, double *h, unsigned int *dfNumCellsX, unsigned int *dfNumCellsY);

#endif

