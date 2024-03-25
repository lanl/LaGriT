#ifndef _HELPERFUNCTIONS_h_
#define _HELPERFUNCTIONS_h_
#include <vector>
#include <sstream>

#include "poi_3D_domain.h"

const int LG_NAME_SIZE = 32;

double distance2D(Point x0, Point x1);
double distance3D(Point x0, Point x1);
std::vector<std::string> splitOnWhiteSpace(std::string line);
void printPoint(Point point);
void process_lagrit_string(char mo_name[LG_NAME_SIZE], char mo_name_trim[LG_NAME_SIZE]);

#endif

