import numpy as np
import scipy.ndimage as ndimage
from copy import deepcopy
import math
from scipy.spatial.distance import euclidean,cdist

def orderPointsClockwise(points:np.ndarray,opt:str='polar',clockwise:bool=True):
    '''
    Given a 2D array of points, this function reorders points clockwise.
    Available methods are: 'angle', to sort by angle, 'polar', to sort by
    polar coordinates, and 'nearest_neighbor', to sort by nearest neighbor.

    :param points: Array of unsorted points
    :type points: np.ndarray
    :param refvec: Vector pointing in direction of sorted origin
    :type refvec: list
    :param opt: Sorting method
    :type opt: str
    :returns: sorted points
    '''

    origin = np.mean(points,axis=0)
    refvec = [0,1]

    def clockwise_angle_and_distance(point):
        '''
        Returns angle and length from origin.
        Used as a sorting function to order points by angle.
        
        Author credit to MSeifert.
        '''

        vector = [point[0]-origin[0],point[1]-origin[1]]
        lenvector = math.hypot(vector[0],vector[1])

        if lenvector == 0:
            return -math.pi,0.

        normalized = [vector[0]/lenvector,vector[1]/lenvector]
        dotprod = normalized[0]*refvec[0] + normalized[1]*refvec[1]
        diffprod = refvec[1]*normalized[0] - refvec[0]*normalized[1]

        angle = math.atan2(diffprod,dotprod)

        if angle < 0:
            return 2*math.pi+angle,lenvector

        return angle,lenvector

    def polar_sort(point):
        return math.atan2(point[1]-origin[1],point[0]-origin[0])

    def nearest_neighbor_sort(xy:np.ndarray):
        dist_matrix = cdist(xy,xy,'euclidean')
        nil_value = np.max(dist_matrix) + 1000
        mapper = np.empty((np.shape(xy)[0],),dtype=int)

        count = 0; indx = 0
        while count < np.shape(mapper)[0]:
            dist_matrix[indx,:] = nil_value
            indx = np.argmin(dist_matrix[:,indx])
            mapper[count] = indx
            count += 1

        return xy[mapper]

    if opt.lower() == 'polar':
        return np.array(sorted(points,key=clockwise_angle_and_distance))
    elif opt.lower() == 'angle':
        return np.array(sorted(points,key=polar_sort))
    elif opt.lower() == 'nearest_neighbor':
        return nearest_neighbor_sort(points)
    else:
        raise ValueError('Unknown sorting method')

def imageErosionBoundary(A,nil_value,distance,cell_size=None,xll_corner=0,yll_corner=0):
    '''
    Blazing fast way to create an accurate DEM boundary.
    Currently, there is no way to seperate nodes with a delta-x.
    As such, this function is not called anywhere within this package.
    Once that is implemented, this function will depreciate squareTraceBoundary().

    :param A: matrix to perform boundary analysis on
    :type A: np.ndarray
    :param nil_value: value characterizing undefined array values
    :type nil_value: float
    :param distance: spacing between boundary nodes
    :type distance: float
    :returns: boundary nodes
    '''

    use_struct = False
    struct = ndimage.generate_binary_structure(2, 2)

    # -------------
    # First, we are going to erode the image to create a boundary trace.
    # A padded mask is generated so that non-NaN values on the 'walls' of 
    # the image don't create issues.

    # Generate mask and pad to avoid conflicts with matrix edge
    mask = A == nil_value
    mask = np.pad(mask, pad_width=4, mode='constant', constant_values=True)
    
    # Erode image
    if use_struct == True:
        erode = ndimage.binary_erosion(mask, struct)
    else:
        erode = ndimage.binary_erosion(mask)

    # Erosion captures the edge of a matrix. We don't want that.
    erode[:,[0,-1]] = erode[[-1,0],:] = True
    edges = mask ^ erode

    # -------------
    # Next, we are going to post-process the edges matrix.
    # Multiple nodes may be present and these need to be removed.

    xy = np.transpose(np.where(edges == True))
    xy[:, 0], xy[:, 1] = xy[:, 1], xy[:, 0].copy()

    if cell_size is not None:
        from tinerator.unit_conversion import xVectorToProjection,yVectorToProjection
        xy[:,0] = xVectorToProjection(xy[:,0],cell_size,xll_corner)
        xy[:,1] = yVectorToProjection(xy[:,1],cell_size,yll_corner,np.shape(A)[0])

    from matplotlib import pyplot as plt
    plt.scatter(xy[:,0],xy[:,1],c='r')
    plt.show()

    xy = orderPointsClockwise(xy,opt='nearest_neighbor')


    
    plt.scatter(xy[:,0],xy[:,1],c='r')
    plt.plot(xy[:,0],xy[:,1])
    plt.show()

    # Next, remove points within a defined distance from each other.
    mask = np.zeros((np.shape(xy)[0],),dtype=bool)
    mask[0] = True
    reference_point = xy[0]
    
    for (i,p) in enumerate(xy):
        if euclidean(p,reference_point) >= distance:
            mask[i] = True
            reference_point = p

    xy = xy[mask]

    # Finally, our boundary should be light enough to do a full distance matrix
    # point removal.
    from tinerator.visualize import _debugScatterPlot
    _debugScatterPlot(xy)

    #plt.scatter(xy[:,0],xy[:,1],c='black')
    #plt.show()
    sys.exit()


def rectangularBoundary(bbox:list,spacing:float):
    '''
    Generates a rectangular boundary with evenly spaced points.

    bbox should be a list of values in the following format:
        min(x), max(x), min(y), max(y)

    :param bbox: bounding box coordinates
    :type bbox: list<float>
    :param spacing: spacing between adjacent nodes
    :type spacing: float
    :returns: array of interpolated bounding box values
    '''

    x0 = bbox[0]; x1 = bbox[1]; y0 = bbox[2]; y1 = bbox[3]

    N = (x1 - x0) / spacing
    horizontal = np.linspace(x0,x1,N,endpoint=True)

    N = (y1 - y0) / spacing
    vertical = np.linspace(y0,y1,N,endpoint=False)

    if np.size(horizontal) == 0 or np.size(vertical) == 0:
        _err = "Invalid spacing for bounding box\n"
        _err += '   spacing: %f\n' % spacing
        _err += '   bbox:    {}\n'.format(bbox)
        raise ValueError(_err)

    top = np.empty((np.size(horizontal),2))
    top[:,0] = horizontal
    top[:,1] = y0

    bottom = np.empty((np.size(horizontal),2))
    bottom[:,0] = horizontal
    bottom[:,1] = y1

    left = np.empty((np.size(vertical)-1,2))
    left[:,0] = x0
    left[:,1] = vertical[1:]

    right = np.empty((np.size(vertical)-1,2))
    right[:,0] = x1
    right[:,1] = vertical[1:]

    boundary = np.concatenate((top,bottom,left,right),axis=0)
    return orderPointsClockwise(boundary)


def squareTraceBoundary(A,NDV,dist=10.):
    '''
    Uses a square-tracing algorithm to quickly find a set of points
    composing the boundary at the interface of data and "empty" (noDataValue)
    cells in a DEM matrix.

    The optional parameter 'dist' denotes the seperation the points should have between each other.
    A smaller value will result in more points being created.

    For more information:
    http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/square.html

    :param A: matrix to perform boundary analysis on
    :type A: np.ndarray
    :param NDV: value characterizing undefined array values
    :type NDV: float
    :param dist: spacing between boundary nodes
    :type dist: float
    :returns: boundary nodes
    '''

    #A = np.vstack([A,np.repeat(666.,A.shape[1])])
    #A = np.hstack([A,np.resize(np.repeat(66666666.,A.shape[0]),(A.shape[0],1))])

    nRows = np.shape(A)[0] - 1
    nCols = np.shape(A)[1] - 1

    print("\nA Square Tracing method of finding boundary in a psuedo-masked array")
    print("C T. Pavlidis, Algorithms for Graphics and Image Processing, Computer Science Press, Rockville, Maryland, 1982\n")

    global last_point # This point is used to see if boundary is being packed too tightly
    last_point = [-5e4,-5e4]
    bounds = np.zeros((nRows,nCols),dtype=np.uint8) # Array containing visited locations
    maxiters = 1e7 # Maximum times to run through algorithm

    # This point class contains movement and direction functions
    class _Point():
        # Init. point class
        def __init__(self):
            # Constant values
            self.north = 0
            self.east = 1
            self.south = 2
            self.west = 3

            # Dynamic values
            self.x = None
            self.y = None
            self.direction = None
        
        # Turn right
        def right(self):
            self.direction = (self.direction + 1) % 4
        
        # Turn left
        def left(self):
            if (self.direction == 0):
                self.direction = 3
            else:
                self.direction -= 1
        
        def move(self):
            if (self.direction == self.north):
                self.y = self.y - 1
            elif (self.direction == self.east):
                self.x = self.x + 1
            elif (self.direction == self.south):
                self.y = self.y + 1
            elif (self.direction == self.west):
                self.x = self.x - 1
            else:
                print("Error: cannot move. Value {} is not".format(self.direction))
                print("recognized as one of the cardinal directions.")
        
        def moveLeft(self):
            self.left()
            self.move()
        
        def moveRight(self):
            self.right()
            self.move()
        
        def getDir(self):
            return self.direction
        
        def position(self):
            return [self.x,self.y]

    # Update the boundary
    tmp_points = []
    def _updateB(x,y):
        global last_point
        bounds[y][x] = 1 # Mark as visited
        current_point = [x,y,A[y][x]]

        # Check if the current point and last saved point are far enough away
        if (_distance(current_point,last_point) >= dist) or (dist is None):
            tmp_points.append(current_point)
            last_point = current_point

    # Is the pixel you're on a valid pixel to move to?
    def _blackPixel(x,y,Xmax,Ymax):
        if (x >= Xmax) or (y >= Ymax) or (x < 0) or (y < 0):
            return False

        if A[y][x] != NDV:
            return True
        else:
            return False

    # Move across the array until a valid pixel is found.
    # This will be the starting pixel for the trace
    def _getStartingPixel(arr,nrows,ncols):
        s = None
        for y in range(nrows):
            for x in range(ncols):
                if arr[y][x] != NDV:
                    s = [x,y]
                    break
        if s is None:
            error("ERROR: Starting pixel not found.")
        
        return s
    
    # Find the distance between two points
    def _distance(v1,v2):
        x1, y1 = v1[:2]
        x2, y2 = v2[:2]
        return ((x1-x2)**2.+(y1-y2)**2.)**0.5

    print("p Finding starting pixel...")

    # Find starting pixel
    p = _Point()
    s = _getStartingPixel(A,nRows,nCols)

    p.x, p.y = s
    p.direction = p.north
    _updateB(p.x,p.y)
    p.moveLeft()
    c = [p.x,p.y]

    print("p Iterating over array...")

    iters = 0
    while iters < maxiters:
        iters += 1 # Break if no convergence
        
        # Are we back at the origin?
        if [p.x,p.y] == s:
            print("p Found origin...")
            break
        
        if _blackPixel(p.x,p.y,nCols,nRows):
            _updateB(p.x,p.y)
            p.moveLeft()
        else:
            p.moveRight()
    
    print("p Generating array...")
    boundary = np.array(tmp_points,dtype=np.double) + 1.
    return boundary

