from matplotlib import pyplot as plt
import numpy as np
from copy import copy

DEBUG = False

def get_line(start, end):
    """Bresenham's Line Algorithm
    Produces a list of tuples from start and end
 
    >>> points1 = get_line((0, 0), (3, 4))
    >>> points2 = get_line((3, 4), (0, 0))
    >>> assert(set(points1) == set(points2))
    >>> print points1
    [(0, 0), (1, 1), (1, 2), (2, 3), (3, 4)]
    >>> print points2
    [(3, 4), (2, 3), (1, 2), (1, 1), (0, 0)]
    """
    # Setup initial conditions
    x1, y1 = start
    x2, y2 = end
    dx = x2 - x1
    dy = y2 - y1
 
    # Determine how steep the line is
    is_steep = abs(dy) > abs(dx)
 
    # Rotate line
    if is_steep:
        x1, y1 = y1, x1
        x2, y2 = y2, x2
 
    # Swap start and end points if necessary and store swap state
    swapped = False
    if x1 > x2:
        x1, x2 = x2, x1
        y1, y2 = y2, y1
        swapped = True
 
    # Recalculate differentials
    dx = x2 - x1
    dy = y2 - y1
 
    # Calculate error
    error = int(dx / 2.0)
    ystep = 1 if y1 < y2 else -1
 
    # Iterate over bounding box generating points between start and end
    y = y1
    points = []
    for x in range(x1, x2 + 1):
        coord = (y, x) if is_steep else (x, y)
        points.append(coord)
        error -= abs(dy)
        if error < 0:
            y += ystep
            error += dx
 
    # Reverse the list if the coordinates were swapped
    if swapped:
        points.reverse()
    return np.array(points)

def get_perpendicular(w,p_i,p1,p2):
    x1,y1 = p1
    x2,y2 = p2
    x_i,y_i = p_i
    m = (y2 - y1) / (x2 - x1)
    x = x_i + w
    y = -1/m*x + (y_i + x_i / m)

    return int(x),int(y)

def smooth_between(A, w, p1, p2, inplace=False):

    if not inplace:
        A = copy(A)

    a1 = get_perpendicular(w,p1,p1,p2)
    a2 = get_perpendicular(-w,p1,p1,p2)

    pts_a = get_line(a1,a2)

    b1 = get_perpendicular(w,p2,p1,p2)
    b2 = get_perpendicular(-w,p2,p1,p2)

    pts_b = get_line(b1,b2)
    pts_c = copy(pts_b)
    pts_c[:,0] = pts_c[:,0] - 1

    pts_d = copy(pts_a)
    pts_d[:,0] = pts_d[:,0] - 1

    pts_a = np.vstack((pts_d,pts_a))
    pts_b = np.vstack((pts_c,pts_b))

    assert len(pts_a) == len(pts_b)

    for i in range(len(pts_a)):
        p1 = pts_a[i].astype(int)
        p2 = pts_b[i].astype(int)

        pts = get_line(p1,p2)

        a_0 = A[p1[0],p1[1]]
        a_1 = A[p2[0],p2[1]]
        dx = int(abs(p2[0] - p1[0]))

        xvec = [a_0 + i*(a_1 - a_0)/(dx) for i in range(dx+1)]

        for p in pts:
            x, y = p
            A[x,y] = xvec[x-p1[0]]
    
    if DEBUG:
        for p in (a1,a2,b1,b2):
            x,y = p
            A[x,y] = 20

    return A