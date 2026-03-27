#ifndef _sampling_h_
#define _sampling_h_
#include <fstream>
#include <vector>
#include <random>

#include "poi_3D_domain.h"
#include "poi_3D_neighborGrid.h"

double uniformDistribution();
unsigned int getTimeBasedSeed();
Point newCandidate(Point newPoint);
Point newCandidateOnFace(Point newPoint, unsigned int faceID);

#endif

