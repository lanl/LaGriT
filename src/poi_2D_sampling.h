#ifndef _sampling_h_
#define _sampling_h_
#include <fstream>
#include <vector>
#include <random>

#include "poi_2D_polygon.h"
#include "poi_2D_neighborGrid.h"

double uniformDistribution();
Point newCandidate(Point newPoint);
unsigned int getTimeBasedSeed();

#endif

