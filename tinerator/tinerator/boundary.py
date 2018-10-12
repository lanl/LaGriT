import numpy as np
import scipy.ndimage as ndimage
from copy import deepcopy

def imageErosionBoundary(A,nil_value,distance):
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

    # Generate mask and pad to avoid conflicts with matrix edge
    mask = A == nil_value
    mask = np.pad(mask, pad_width=4, mode='constant', constant_values=True)
    
    # Erode image
    struct = ndimage.generate_binary_structure(2, 2)
    erode = ndimage.binary_erosion(mask, struct)

    # Erosion captures the edge of a matrix. We don't want that.
    erode[:,[0,-1]] = erode[[-1,0],:] = True
    edges = mask ^ erode

    # np.vectorize(lambda x: x + 1)(a)

    plt.imshow(edges)
    plt.show()


def rectangularBoundary(ncols:int,nrows:int,nx:float,ny:float=None):

    point_array = []
    
    ny = nx if ny is None else ny
    
    alpha_x = float(ncols) / float(nx)
    alpha_y = float(nrows) / float(ny)

    for i in range(0,nx+1):
        x = float(i) * alpha_x
        point_array.append((x,0))
        point_array.append((x,nrows))

    for j in range(1,ny):
        y = float(j) * alpha_y
        point_array.append((0,y))
        point_array.append((ncols,y))

    return np.array(point_array)

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

    nRows = np.shape(A)[0]
    nCols = np.shape(A)[1]

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
        if (_distance(current_point,last_point) >= dist) or (dist == None):
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
        for y in range(0,nrows):
            for x in range(0,ncols):
                if arr[y][x] != NDV:
                    s = [x,y]
                    break
        if s == None:
            error("ERROR: Starting pixel not found.")
        
        return s
    
    # Find the distance between two points
    def _distance(v1,v2):
        x1 = v1[0]
        x2 = v2[0]
        y1 = v1[1]
        y2 = v2[1]
        return ((x1-x2)**2.+(y1-y2)**2.)**0.5

    print("p Finding starting pixel...")

    # Find starting pixel
    p = _Point()
    s = _getStartingPixel(A,nRows,nCols)

    p.x = s[0]
    p.y = s[1]
    p.direction = p.north
    _updateB(p.x,p.y)
    p.moveLeft()
    c = [p.x,p.y]

    print("p Iterating over array...")

    iters = 0
    while True or iters < maxiters:
        iters += 1 # Break if no convergence
        
        # Are we back at the origin?
        if (p.x == s[0]) and (p.y == s[1]):
            print("p Found origin...")
            break
        
        if _blackPixel(p.x,p.y,nCols,nRows) == True:
            _updateB(p.x,p.y)
            p.moveLeft()
        else:
            p.moveRight()
    
    print("p Generating array...")
    boundary = np.array(tmp_points)
    return boundary

