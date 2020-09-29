import math
import numpy as np
from scipy.ndimage import rotate
from matplotlib import pyplot as plt
import numpy as np
from scipy import ndimage as nd
from scipy.ndimage import rotate
import numpy as np
import tinerator.utilities as util
import numpy as np
from scipy import interpolate
from shapely.geometry import Point
from shapely.geometry.polygon import Polygon
from matplotlib import pyplot as plt
from copy import deepcopy
from math import floor

def fill_nans(arr: np.ndarray):
    '''
    Does a fast nearest-neighbor fill of NaN's in an array.
    '''
    ind = nd.distance_transform_edt(
        np.isnan(arr), 
        return_distances=False, 
        return_indices=True
    )
    return arr[tuple(ind)]

def GRADIENT_FILL(raster_obj, arr, polygon, **kwargs):
    
    try:
        v1 = kwargs["gradient_start"]
        v2 = kwargs["gradient_stop"]
        angle = kwargs["gradient_angle"]
    except KeyError:
        raise KeyError("`gradient_start`, `gradient_stop`, and `gradient_angle` are required")
        
    # Get where the array is NaN
    # Assumes that the only places with NaN 
    # are etched from polygon
    locs = np.argwhere(np.isnan(arr))
    
    minx, miny = np.min(locs[:,1]), np.min(locs[:,0])
    maxx, maxy = np.max(locs[:,1]), np.max(locs[:,0])
    
    height = int(maxy - miny) + 1
    width = int(maxx - minx) + 1
    
    # Create a top-to-bottom gradient
    gradient_arr = np.transpose(np.tile(np.linspace(v1, v2, num=height), (width,1)))
    
    # Rotate the array and fill NaNs
    gradient_arr = fill_nans(rotate(gradient_arr, angle, cval=np.nan))
    
    # Compute centroids
    centroid_A = [(maxx - minx) / 2. + minx, (maxy - miny) / 2. + miny]
    centroid_B = (np.array(gradient_arr.shape[::-1]) - 1) / 2.

    delta_X = (centroid_A[0] - centroid_B[0])
    delta_Y = (centroid_A[1] - centroid_B[1])
    
    locs_adjusted = deepcopy(locs)
    locs_adjusted[:,0] = locs_adjusted[:,0] - floor(delta_Y)
    locs_adjusted[:,1] = locs_adjusted[:,1] - floor(delta_X)

    # Convert to an array-indexing form
    locs = tuple(locs.T)
    locs_adjusted = tuple(locs_adjusted.T)

    print('>>>', locs)
    print('>>>', locs_adjusted)

    # Finally, map the gradient back to NaN's
    arr[locs] = gradient_arr[locs_adjusted]
    
    return arr
    

def vector_angle(p1, p2):
    y = p2[1] - p1[1]
    x = p2[0] - p1[0]
    return math.atan2(y, x)*180/math.pi

def midpoint(p1, p2):
    return ((p1[0] + p2[0])/2., (p1[1] + p2[1])/2.)

def linear_gradient(v1, v2, width, height):
    return np.transpose(np.tile(np.linspace(v1, v2, num=int(height)), (width,1)))

def rotate_matrix(A, p1, p2, no_data=-9999.):
    y = p2[1] - p1[1]
    x = p2[0] - p1[0]
    angle = math.atan2(y, x)*180/math.pi
    
    if angle < 0.:
        angle = 180. + angle
        
    mat = rotate(A, angle, cval=no_data)
    mat[mat == no_data] = np.nan
    return mat

def insert_matrix(parent, child, midpoint):
    parent_rows, parent_cols = parent.shape
    child_rows, child_cols = child.shape
    row_begin = int(round(midpoint[0] - child_rows/2.))
    col_begin = int(round(midpoint[1] - child_cols/2.))
    
    for row in range(child_rows):
        row_i = row + row_begin
        if row_i < 0 or row_i > parent_rows:
            continue
            
        for col in range(child_cols):
            col_i = col + col_begin
            if col_i < 0 or col_i > parent_cols:
                continue
            
            value = child[row][col]
            if np.isnan(value):
                continue
            
            parent[row_i][col_i] = value
    
    return parent

def smooth_between(matrix, width, p1, p2, inplace=True):
    length = ((p2[0]-p1[0])**2 + (p2[1] - p1[1]) ** 2) ** 0.5
    c = linear_gradient(matrix[p1[0],p1[1]], matrix[p2[0],p2[1]], int(width), int(length))
    c = rotate_matrix(c, p1, p2)
    mp = midpoint(p1, p2)
    insert_matrix(matrix, c, mp)