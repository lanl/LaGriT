#include <iostream>
#include <string>
#include <math.h>
#include <vector>

#include "poi_helperFunctions.h"
#include "poi_sampling.h"


using std::cout;
using std::endl;
using std::string;
using std::vector;

std::uniform_real_distribution<> uniform01(0, 1);
std::mt19937_64 generator(0);

/*! Returns a number sampled from a uniform distribution on the intervale [0,1]
*/
double uniformDistribution() {
    return uniform01(generator);
}

/*! Sample polygon boundaries
* Discretize the boundaries of the polygon
*/
void Polygon::sampleBoundaries() {
    cout << "Sampling boundaries" << endl;
    vector<Point> boundaryNodes;
    
    for (unsigned int i = 0; i < numVertices; i++) {
        vector<Point> lineNodes;
        lineNodes = sampleAlongLine(nodes[i], nodes[(i + 1) % numVertices]);
        boundaryNodes.insert(boundaryNodes.end(), lineNodes.begin(), lineNodes.end());
    }
    
    nodes.insert(nodes.end(), boundaryNodes.begin(), boundaryNodes.end());
    numNodes = nodes.size();
    cout << "Sampling boundaries complete\n" << endl;
}

/*! Discretizes the edge of the polygon.
* the points are non-uniform spacing along the line
*/
vector<Point> Polygon::sampleAlongLine(Point x0, Point x1) {
    vector<Point> lineNodes;
    double distance;
    double direction[2];
    double increment;
    // initialize previous sample
    Point prevSample;
    prevSample.x = x0.x;
    prevSample.y = x0.y;
    getExclusionRadius(prevSample);
    distance = distance2D(x0, x1);
    direction[0] = (x1.x - x0.x) / distance;
    direction[1] = (x1.y - x0.y) / distance;
    // maximum number of points between vertices
    unsigned int numPoints;
    numPoints = int(floor(distance / h));
    
    for (unsigned int i = 0; i < numPoints; i++) {
        // sample new point frm Uniform distribution on [0.9*local_radius,1.1*local_radius]
        increment = uniformDistribution() * 0.2 * prevSample.radius + 0.9 * prevSample.radius;
        prevSample.x = prevSample.x + direction[0] * increment;
        prevSample.y = prevSample.y + direction[1] * increment;
        
        // make sure the point is in the domain and not too close the end point
        if (inBoundingBox(prevSample) && (distance2D(prevSample, x1) > 0.6 * prevSample.radius)) {
            getExclusionRadius(prevSample);
            prevSample.ix = getNeighborGridCellID(prevSample.x, xMin);
            prevSample.iy = getNeighborGridCellID(prevSample.y, yMin);
            lineNodes.push_back(prevSample);
        } else {
            break;
        }
    }
    
    return lineNodes;
}

/*! main sampling algorithim to fill in the polygon.
* The distribution builds upon the points already accepted in the domain.
* Candidate points are sampled within an annulus around accepted points.
* The candidate point is then tested for acceptance
*/
void Polygon::mainSampling(unsigned int startIndex, bool restartFlag) {
    Point newPoint;
    unsigned int newPointCount = 0;
    
    if (!restartFlag) {
        cout << "Starting Main sampling" << endl;
    } else {
        cout << "Restarting sampling at " << startIndex << endl;
    }
    
    for (unsigned int i = startIndex; i < numNodes; i++) {
        for (unsigned int k = 0; k < numSamples; k++) {
            // Create new points within an anulus around current point
            newPoint = newCandidate(nodes[i]);
            
            // test new point
            if (testCandidate(newPoint)) {
                acceptCandidate(newPoint);
                newPointCount++;
            }
        }
    }
    
    if (!restartFlag) {
        cout << newPointCount << " points added during sampling\n" << endl;
    } else {
        cout << newPointCount << " points added during resampling\n" << endl;
    }
}
void Polygon::resample() {
    // Sweep the neighborgrid and find all cells in the background that are not filled.
    // resample sweeps is a user variable. Default of 1 is typically good enough
    //
    for (unsigned int i = 0; i < resampleSweeps; i++) {
        cout << "Starting resampling sweep " << i + 1 << " out of " << resampleSweeps << endl;
        findEmptyCells();
        unsigned int numNewPoints = fillEmptyCells();
        // Repeat main sampling, but start from index of the new points added to the domain.
        mainSampling(numNodes - numNewPoints, true);
        cout << "Resampling sweep " <<  i + 1 << " out of " << resampleSweeps << " complete\n" << endl;
    }
}

/*! Generate a new candidate point.
*/
Point newCandidate(Point currentPoint) {
    Point newPoint;
    double radius, angle;
    // sample from an annulus with inner radius h and outter 1.5h;
    radius = uniformDistribution() * 1.5 * currentPoint.radius + currentPoint.radius;
    // random rotation within the annulus
    angle = uniformDistribution() * M_PI * 2.0;
    // construct new point
    newPoint.x = currentPoint.x + radius * cosf(angle);
    newPoint.y = currentPoint.y + radius * sinf(angle);
    //cout << "new point " << newPoint.x << " " << newPoint.y << endl;
    return newPoint;
}

void Polygon::acceptCandidate(Point &point) {
    // point is accepted
    // increase global node count
    numNodes++;
    point.nodeNum = numNodes;
    // increaes current point count
    tagNeighborCells(point);
    // add point to the node vector
    nodes.push_back(point);
    //printPoint(newPoint);
}


/*! Test new candidate point for acceptance.
    * Checks if in bounding box
    * Checks if in polygon
    * Checks if grid cell in empty
    * Checks distance to nearby points is greater than local tolerance
*/
bool Polygon::testCandidate(Point &newPoint) {
    // Check 1
    // Check if point is within the bounding box of the polygon.
    // This first check is cheap and easy, so it's done first.
    // Also, neighborhood cells will return a seg fault is the point is outside
    // of the bounding box
    if (!inBoundingBox(newPoint)) {
        // cout << "bounding box fail" << endl;
        return false;
    };
    
    // Check 2
    // Check if point is in the domain. Also cheap.
    if (!inDomain(newPoint)) {
        // cout << "in domain fail" << endl;
        return false;
    }
    
    // Check 3
    // Check if the neighbor grid location is occupied.
    // Another cheap check
    newPoint.ix = getNeighborGridCellID(newPoint.x, xMin);
    newPoint.iy = getNeighborGridCellID(newPoint.y, yMin);
    
    if (grid[newPoint.ix][newPoint.iy] > 0) {
        // cout << "grid space filled" << endl;
        return false;
    }
    
    // Now that we know the point is inside the domain and in an empty-cell
    // we grab the exclusion radius from the distance field.
    getExclusionRadius(newPoint);
    
    // Check 4
    // Check to ensure new point is not within the exclusion radius
    // of accepted points
    if (!emptyDiskProperty(newPoint)) {
        // cout << "empty disk failed" << endl;
        return false;
    }
    
    return true;
}


/*! Checks if a point is within the bounding box of the polygon.
* This is a cheap way to reject a point before any other more
* expensive test.
*/
bool Polygon::inBoundingBox(Point point) {
    // Checks if point is within bounding box of the polygon
    if (point.x < xMin) {
        return false;
    } else if (point.y < yMin) {
        return false;
    } else if (point.x > xMax) {
        return false;
    } else if (point.y > yMax) {
        return false;
    } else {
        return true;
    }
}


/*! Checks if a point is within the polygon domain using a ra casting algorithm.
    1) create a parametric equation for a ray coming from the new Points
    * r(x) = newPoint.x + t * 1
    * r(y) = newPoint.y + t * 1
    * * This ray just goes along the x axis
    * 2) loop through initial vertices of the polygon.
    * Create a parametric equation for each edge
    * l(x) = x + u * mx
    * l(y) = y + u * my
    * 3) find point of intersection between the two rays
    * 4) determine if the point is on the boundary of the polygon.
    * 5) count the number of times the ray emitting from the point crosses the boundary of the polygon
    * if the count is zero or even, the point is outside the domain,
    * if the count is odd, the point is inside the domain
*/
bool Polygon::inDomain(Point newPoint) {
    /*
    for (unsigned int i = 0; i < numVertices; i++) {
        // get the parametric equation values for the edge of the polygon
        point1 = nodes[i];
        point2 =  nodes[(i + 1) % numNodes];
        mx = point2.x - point1.x;
        my = point2.y - point1.y;
        // this is the whole equation, but for our setup, a horizonal ray, it simplifies.
        t = (mx * (newPoint.y - point1.y) + my * (point1.x - newPoint.x)) / (1.0 * my - 1.0 * mx);
        //t = (mx * (newPoint.y - point1.y) + my * (point1.x - newPoint.x)) / my;
    
        if (t > 0) {
            x = newPoint.x + t;
            y = newPoint.y + t;
    
            // Check if intersection point is on the edge
            if (std::min(point1.x, point2.x) <= x && x <= std::max(point1.x, point2.x) && std::min(point1.y, point2.y) <= y && y <= std::max(point1.y, point2.y) ) {
                // if the intersection point is on the line segment, then we count it is as a crossing.
                count++;
            }
        }
    }
    */
    /*
    Ray Casting adapted from
    
    https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html#The%20Inequality%20Tests%20are%20Tricky
    
     License to Use
    
    Copyright (c) 1970-2003, Wm. Randolph Franklin
    
    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
    
    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers.
    Redistributions in binary form must reproduce the above copyright notice in the documentation and/or other materials provided with the distribution.
    The name of W. Randolph Franklin may not be used to endorse or promote products derived from this Software without specific prior written permission.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    
    */
    unsigned i, j;
    int c = 0;
    
    for (i = 0, j = numVertices - 1; i < numVertices; j = i++) {
        if ( ((nodes[i].y > newPoint.y) != (nodes[j].y > newPoint.y)) && (newPoint.x < (nodes[j].x - nodes[i].x) * (newPoint.y - nodes[i].y) / (nodes[j].y - nodes[i].y) + nodes[i].x) ) {
            // flips back and forth between 0 and 1
            // will be 0 for no crossings or an even number of them
            // will be 1 for odd number of crossings
            c = !c;
        }
    }
    
    // If the number crossing is odd, then the point is inside of the domain;
    if (c == 1) {
        return true;
        // if the number crossing is zero or even, then the point is outside of the domain;
    } else {
        return false;
    }
}

/*! Given a potential new point, find all the already accepted points in the neighboring cells,
then directly check the distance between those points to ensure that the new point
does not reside within the disk of the already accepted points.
*/
bool Polygon::emptyDiskProperty(Point newPoint) {
    double dist;
    // Get exclusion radius for new pointw
    std::vector<int> nodeIDs;
    nodeIDs = getNeighborCellsRadius(newPoint);
    
    // Walk through those nodes to check if the new point is too close to those
    for (int i : nodeIDs) {
        // check if the distance between points is less than either point's exclusion radius
        dist = distance2D(newPoint, nodes[i - 1]);
        
        // Ensure distance between points is larger than the maximum of the
        // points' exclusion radii
        if (dist < std::max(newPoint.radius, nodes[i - 1].radius)) {
            return false;
        }
    }
    
    return true;
}

