import numpy as np
import sys
import os

def is_polygon_clockwise(pts: np.ndarray, connectivity: np.ndarray = None) -> bool:
    '''
    Given an Nx2 or Nx3 matrix, with points ordered either
    clockwise or counter-clockwise, returns True if ordered
    clockwise.
    '''

    # Find point w/ smallest Y coordinate
    #y_min = np.argwhere(pts[:,1] == np.min(pts[:,1])).T[0]

    # Choose point w/ largest X if tie
    #x_max = np.argwhere(pts[y_min][:,0] == np.max(pts[y_min][:,0])).flatten()[0]

    # Update min Y coord
    #y_min = y_min[x_max]
    
    y_min = 0

    if connectivity is None:
        A = pts[y_min]
        B = pts[y_min + 1]
        C = pts[y_min + 2]
    else:
        AB = connectivity[y_min]
        A, B = pts[AB[0]], pts[AB[1]]
        C = pts[connectivity[connectivity[:,0] == AB[1]].flatten()[1]]
    
    O = np.array([
        [1, A[0], A[1]],
        [1, B[0], B[1]],
        [1, C[0], C[1]]
    ], dtype=float)
    
    determinate = np.linalg.det(O)
    
    if determinate == 0.:
        raise ValueError("Array has co-linear points")
    
    return determinate < 0.

def write_avs(surface_mesh,nodes,tris,cname='tri',matid=None,node_attributes:dict=None):
    with open(surface_mesh,'w') as f:
        node_atts = 0 if node_attributes is None else len(node_attributes.keys())
        f.write('{} {} {} 0 0\n'.format(nodes.shape[0],tris.shape[0],node_atts))

        for (i,node) in enumerate(nodes):
            f.write('{} {} {} {}\n'.format(
                i+1,
                *nodes[i])
            )
        
        if matid is None and len(tris) > 0:
            matid = [i+1 for i in range(len(tris))]

        for (i,cell) in enumerate(tris):
            f.write('{} {} {} {}\n'.format(
                i+1,
                int(matid[i]),
                cname,
                ' '.join([str(x) for x in list(map(int,tris[i]))]))
            )

        if node_attributes is not None:
            f.write('{} {}\n'.format(node_atts, ' '.join(['1']*node_atts)))

            for key in node_attributes:
                f.write('%s, integer\n' % key)

            for i in range(nodes.shape[0]):
                n_atts = []

                for key in node_attributes:
                    n_atts.append(str(node_attributes[key][i]))

                f.write('{} {}\n'.format(i+1,' '.join(n_atts)))
