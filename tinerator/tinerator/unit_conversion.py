'''

Set of functions that convert from matrix (row and column) units to
a DEM projection, and vice versa.

'''

import numpy as np

def convertXtoProjection(x,cellSize,xllCorner):
    return (xllCorner + (float(x) * cellSize) - (cellSize / 2.))

def convertYtoProjection(y,cellSize,yllCorner,nRows):
    return (yllCorner + (float(0. - y + float(nRows)) * cellSize) - (cellSize / 2.))

def convertProjectiontoX(x,cellSize,xllCorner):
    return (cellSize + 2. * float(x) - 2. * xllCorner) / (2. * cellSize)

def convertProjectiontoY(y,cellSize,yllCorner,nRows):
    return ((yllCorner - y) / cellSize + nRows + 1./2.)

def xVectorToProjection(v,cellSize,xllCorner):
    nv = np.vectorize(convertXtoProjection)
    return nv(v,cellSize,xllCorner)

def yVectorToProjection(v,cellSize,yllCorner,nRows):
    nv = np.vectorize(convertYtoProjection)
    return nv(v,cellSize,yllCorner,nRows)

def xProjectionToVector(v,cellSize,xllCorner):
    nv = np.vectorize(convertProjectiontoX)
    return nv(v,cellSize,xllCorner)

def yProjectionToVector(v,cellSize,yllCorner,nRows):
    nv = np.vectorize(convertProjectiontoY)
    return nv(v,cellSize,yllCorner,nRows)

def normalizeMatrix(A):
    return (A-np.min(A))/(np.max(A)-np.min(A))

def filterPoints(points:np.ndarray,eps:float):
    '''
    Removes points that are within eps distance of each other.
    '''
    from scipy.spatial.distance import cdist
    mask = np.ones(np.shape(points)[0],dtype=bool)

    for (i,p) in enumerate(points):
        if mask[i]:
            dst = cdist(points,[p])
            mask[np.argwhere((dst > 0.) & (dst < eps))] = False

    return points[mask]