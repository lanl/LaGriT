#include <iostream>
#include <string>
#include <math.h>
#include <vector>
#include <sstream>
#include <iostream>

#include "poi_helperFunctions.h"
#include "poi_3D_domain.h"

using std::cout;
using std::endl;
using std::string;
using std::vector;

/* computes Euclidean distance between two points */
double distance2D(Point x0, Point x1) {
    return sqrt(pow(x1.x - x0.x, 2) + pow(x1.y - x0.y, 2));
}
/* Computes Euclidean Distance Squared between two points */
double distance2DSq(Point x0, Point x1) {
    return pow(x1.x - x0.x, 2) + pow(x1.y - x0.y, 2);
}

/* computes Euclidean distance between two points */
double distance3D(Point x0, Point x1) {
    return sqrt(pow(x1.x - x0.x, 2) + pow(x1.y - x0.y, 2) + pow(x1.z - x0.z, 2));
}
/* Computes Euclidean Distance Squared between two points */
double distance3DSq(Point x0, Point x1) {
    return pow(x1.x - x0.x, 2) + pow(x1.y - x0.y, 2) + pow(x1.z - x0.z, 2);
}


/*! Splits a line of text on white space and returns
*a of strings with the words in the line
*/
vector<std::string> splitOnWhiteSpace(std::string line) {
    vector<std::string> result;
    std::istringstream line_stream(line);
    
    for (std::string s; line_stream >> s;) {
        result.push_back(s);
    }
    
    return result;
}

/*! Print point information to screen
*/
void printPoint(Point point) {
    cout << "Point - x " << point.x;
    cout << " -y: " << point.y;
    cout << " -z: " << point.z << endl;
    cout << " -ix: " << point.ix;
    cout << " -iy: " << point.iy;
    cout << " -iz: " << point.iz;
    cout << " -r: " << point.radius;
    cout << " -node #: " << point.nodeNum << "\n" << endl;
}

void process_lagrit_string(char mo_name[LG_NAME_SIZE], char mo_name_trim[LG_NAME_SIZE]) {
    // removes white space from LaGriT string. LaGriT passes it in with white space
    // fortran will pads the strings with spaces/
    // This copies each character until we hit a space
    for (size_t len = 0; len < LG_NAME_SIZE - 1; len++) {
        snprintf(mo_name_trim, len + 1, "%s", mo_name);
        
        if (mo_name[len] == ' ') {
            break;
        }
    }
}