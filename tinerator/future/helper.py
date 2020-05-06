import numpy as np
import sys
import os

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
