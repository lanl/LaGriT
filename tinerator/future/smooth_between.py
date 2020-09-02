import math
import numpy as np
from scipy.ndimage import rotate

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
        
    mat = rotate(A, 45, cval=no_data)
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