'''

A better solution using LaGriT refinement is slated for development.
The below functions *will* be deprecated soon!

'''

import math
import numpy as np
import random
#import progressbar
from scipy.spatial import Delaunay
from copy import deepcopy
from tinerator.utilities import *

def isPointOutsidePolygon(vertices,pt1,pt2):
    '''
    Given a line segment from point1 to point2, detect if that line segment
    crosses a boundary.

    Practically, what this means is that if you draw a line from the center 
    of a TIN (which is inside the convex hull) to an arbitrary point, this function
    will return whether that point is inside or outside the convex hull.

    :param vertices: bounding polygon
    :type vertices: np.ndarray
    :param pt1: first vertex defining crossing line
    :type pt1: float
    :param pt2: second vertex defining crossing line
    :type pt2: float
    :returns: flag indicating whether point is in or outside polygon
    '''

    def ccw(A,B,C):
        return (C[1]-A[1]) * (B[0]-A[0]) > (B[1]-A[1]) * (C[0]-A[0])

    # Return true if line segments AB and CD intersect
    def intersect(A,B,C,D):
        return ccw(A,C,D) != ccw(B,C,D) and ccw(A,B,C) != ccw(A,B,D)

    intersection_count = 0

    # Iterate over each line segment in the boundary...
    for i in range(1,len(vertices)):
        A = vertices[i-1]
        B = vertices[i]

        # Return immediately if there is an intersection
        if intersect(A,B,pt1,pt2) == True:
            intersection_count += 1#return True

    # Is the intersection count even or odd?
    # If even (or 0), the point is outside of the boundary.
    if intersection_count % 2 == 0:
        return True
    else:
        return False

def cullTrianglesToAlphaHull(points,triangles,boundary,ncols,cell_size,xll_corner):
    '''
    This function performs a ray casting algorithm to determine if 
    each triangle is within the bounding polygon.

    That is, given the centroids C of each triangle, compute a horizontal 
    line to from c in C to infinity (approximated as the far right of the DEM + 10).
    Count how many times the ray crosses a boundary segment. If even or zero, c 
    is outside of the bounding polygon and should be removed; if odd, c is inside.

    A more rigorous implementation of this algorithm can be found here:
    http://geomalgorithms.com/a03-_inclusion.html
    (see The Winding Number)

    Citations:
    [1] http://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/
    [2] http://geomalgorithms.com/a03-_inclusion.html
    [3] https://en.wikipedia.org/wiki/Point_in_polygon

    :param points: set of points defining mesh
    :type points: np.ndarray
    :param triangles: array of triangle connectivity defining mesh
    :type triangles: np.ndarray
    :param boundary: set of vertices defining mesh boundary
    :type boundary: np.ndarray
    :param ncols: number of columns in DEM
    :type ncols: int
    :param cell_size: DEM cell size
    :type cell_size: float
    :param xll_corner: lower-left corner of DEM (x-component)
    :type xll_corner: float
    :returns: filtered triangles
    '''

    print("\n\nA Culling Triangulation to Bounding Polygon")
    print("C Livingston, D. 2018.\n")

    print("p Finding triangles outside perimeter...")

    triangles_to_remove = []
    reference_point_x = convertXtoProjection(ncols + 10,cell_size,xll_corner)

    #bar = progressbar.ProgressBar(max_value=len(triangles))

    # Iterate over all triangles...
    for i in range(0,len(triangles)):
        #bar.update(i)

        # Capture the currently active triangle...
        tri = triangles[i]

        # Compute the X,Y centroid...
        centroidx = (points[tri[0]][0] + points[tri[1]][0] + points[tri[2]][0]) / 3.0
        centroidy = (points[tri[0]][1] + points[tri[1]][1] + points[tri[2]][1]) / 3.0
        centroid = [centroidx,centroidy]

        # and determine if it is inside or outside the boundary. If so, remove.
        if isPointOutsidePolygon(boundary,centroid,[reference_point_x,centroidy]):
            triangles_to_remove.append(i)

    triangles = np.delete(triangles,triangles_to_remove,axis=0)
    return triangles

def triangulateNodes(nodes,preserve_boundary=False,boundary_vertices=None,
                     qhull_opts="QJ",ncols=None,cell_size=None,xll_corner=None):
    '''

    Triangulates a set of nodes using the Delaunay criterion.

    :param nodes: nodes to triangulate
    :type nodes: np.ndarray
    :param preserve_boundary: flag indicating whether bounding polygon should be respected
    :type preserve_boundary: bool
    :returns: triangulation

    '''

    points = deepcopy(nodes[:,:2]) # Capture only X,Y axes
    points -= points.mean(axis=0)  # Center points around origin to avoid rounding errors

    try:
        tri = Delaunay(points,qhull_options = qhull_opts)
        delaunay_instance = tri
    except:
        return 101
    
    triangles = tri.simplices

    if preserve_boundary == True:
        triangles = cullTrianglesToAlphaHull(nodes,triangles,boundary_vertices,ncols,cell_size,xll_corner)

    return triangles

def warn(string):
    print(string)

def idxToRC(idx,ncols):
    '''
    Returns the [row,column] value of an index
    '''
    return int(math.floor(idx / ncols)),idx%ncols

def CellsInCircle(maxDiameter,cellSize):

    minCells = 1
    maxCells = maxDiameter / cellSize

    arrayCellsInCircle = np.zeros((int(maxCells)+1,3,int(maxCells*maxCells)+1),dtype=int)

    for d in range(int(minCells)-1,int(maxCells)+1):
        upperCell = int(d / 2.0)
        lowerCell = 0 - upperCell

        radius = (float(d) * cellSize) / 2.0

        cellCount = 0

        for y in range(int(lowerCell),int(upperCell)):
            for x in range(int(lowerCell),int(upperCell)):

                skipX2 = False
                if (x == 0 and y == 0): skipX2 = True

                if skipX2 == False:
                    cellCenterDistance = (((float(x) * cellSize) ** 2.0) + ((float(y) * cellSize) ** 2.0)) ** 0.5

                    if cellCenterDistance <= radius:
                        cellCount += 1
                        arrayCellsInCircle[d][1][cellCount] = x
                        arrayCellsInCircle[d][2][cellCount] = y

                        cellCenterDistance = ((((np.abs(float(x)) + 1) * cellSize) ** 2.0) + ((float(y) * cellSize) ** 2.0)) ** 0.5
                        if cellCenterDistance > radius: arrayCellsInCircle[d][0][cellCount] = 1
                        cellCenterDistance = (((float(x) * cellSize) ** 2.0) + (((np.abs(float(y)) + 1.0) * cellSize) ** 2.0)) ** 0.5
                        if cellCenterDistance > radius: arrayCellsInCircle[d][0][cellCount] = 1
        
        arrayCellsInCircle[d][0][0] = cellCount
    
    return arrayCellsInCircle

def PlacePoints(elevation,arrayDistance,boundary,nRows,nCols,cellSize,noDataValue,maxDistance,
                minDistance,maxEdge,minEdge,xllCorner,yllCorner):
    '''
    Circle-packs points with a density proportional to the provided distance map / rasterized gradient field.

    :param elevation: rasterized DEM
    :type elevation: np.ndarray
    :param arrayDistance: rasterized gradient field
    :type arrayDistance: np.ndarray
    :param boundary: set of boundary nodes
    :type boundary: np.ndarray
    :param nRows: rows in DEM
    :type nRows: int
    :param nCols: columns in DEM
    :type nCols: int
    :param cellSize: DEM cell size
    :type cellSize: float
    :param noDataValue: DEM no data value
    :type noDataValue: float
    :param minDistance: threshold value for minimum-edged nodes
    :type minDistance: float
    :param maxDistance: threshold value for maximum-edged nodes
    :type maxDistance: float
    :param minEdge: length of smallest triangle edge
    :type minEdge: float
    :param maxEdge: length of largest triangle edge
    :type maxEdge: float
    :param xllCorner: lower left corner of DEM (x)
    :type xllCorner: float
    :param yllCorner: lower left corner of DEM (y)
    :type yllCorner: float
    :returns: array of nodes
    '''

    print("\nA Variable Circle Packing over a Gradient")
    print("C Middleton, R., et al. 2017.\n")
    print("p Initializing matrices...")

    # Initialize variables
    nPoints = 0
    points = []

    distanceRange = maxDistance - minDistance
    edgeRange = maxEdge - minEdge

    # Initialize arrays
    arrayCovered = np.zeros((nCols,nRows),dtype=bool)
    arrayCellDistance = np.zeros((nCols,nRows),dtype=int)
    arayCellsInCircle = CellsInCircle(maxEdge,cellSize)

    # Get boundary
    if False:#boundary is not None:
        print("p Setting boundary...")
        points.extend(boundary)

        for i in range(0,len(points)):

            row = int(round(convertProjectiontoX(points[i][0],cellSize,xllCorner)))
            col = int(round(convertProjectiontoY(points[i][1],cellSize,yllCorner,nRows)))

            if row >= nRows:
                #warn("Row value {} is out of bounds for DEM with {} rows".format(row,nRows))
                row = nRows - 1
            if col >= nCols:
                #warn("Column value {} is out of bounds for DEM with {} columns".format(col,nCols))
                col = nCols - 1

            if col < nCols - 10:
                arrayCovered[col][row] = True

    for y in range(0,nRows):
        for x in range(0,nCols):

            distance = float(arrayDistance[x][y])*10. - 1

            if int(arrayDistance[x][y]) == int(noDataValue):
                diameterAsCell = noDataValue
            elif distance <= minDistance:
                diameterAsCell = minEdge
            elif distance >= maxDistance:
                diameterAsCell = maxEdge
            else:
                diameterAsCell = int((distance - minDistance) / (distanceRange) * edgeRange) + minEdge

            arrayCellDistance[x][y] = int(round(diameterAsCell))
    
    print("p Circle packing points...\n")
    xvec = random.sample(range(0,nCols),nCols)
    yvec = random.sample(range(0,nRows),nRows)

    #bar = progressbar.ProgressBar(max_value=nCols*nRows-1)

    # Second
    for (iteration,xy) in enumerate(random.sample(range(0,nCols*nRows),nCols*nRows)):
        #bar.update(iteration)
        x,y = idxToRC(xy,nRows)

        diameterAsCell = int(float(arrayCellDistance[x][y]) / float(cellSize))

        skipX = False

        if (float(diameterAsCell)*float(cellSize)) != noDataValue and diameterAsCell != -999:
            diameterAsCell = int(round(float(arrayCellDistance[x][y]) / float(cellSize)))
            for i in range(1,int(arayCellsInCircle[diameterAsCell][0][0])):

                dX = x + arayCellsInCircle[diameterAsCell][1][i]
                dY = y + arayCellsInCircle[diameterAsCell][2][i]

                skipI = False
                if (dX < 0) or (dX > nCols - 1) or (dY < 0) or (dY > nRows - 1):
                    skipI = True

                if skipI == False:
                    if arrayCovered[dX][dY] == True:
                        if arayCellsInCircle[diameterAsCell][0][i] == 0:
                            skipX = True
                            break
        else:
            skipX = True

        if skipX == False:
            nPoints += 1
            arrayCovered[x][y] = True
            points.append([y,x,elevation[x][y]])

    points = np.array(points)
    points[:,0] = xVectorToProjection(points[:,0],cellSize,xllCorner)
    points[:,1] = yVectorToProjection(points[:,1],cellSize,yllCorner,nCols)
    return points

