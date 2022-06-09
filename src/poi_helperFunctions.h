#ifndef _HELPERFUNCTIONS_h_
#define _HELPERFUNCTIONS_h_
#include <vector>
#include <sstream>

#include "poi_polygon.h"

double distance2D(Point x0, Point x1);
double distance2DSq(Point x0, Point x1);
std::vector<std::string> splitOnWhiteSpace(std::string line);
void printPoint(Point point);

#endif

