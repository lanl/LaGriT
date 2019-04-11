import os
import subprocess
import numpy as np
import tinerator.facesets as fs
import tinerator.config as cfg

def __get_latest_mesh(dem_object,mesh_object):
    '''
    Given a string describing the mesh type, returns
    the proper DEM class attribute.
    '''

    if mesh_object is None:
        if dem_object._stacked_mesh is not None:
            return dem_object._stacked_mesh,3
        elif dem_object._surface_mesh is not None:
            return dem_object._surface_mesh,2
        else:
            raise ValueError('Could not find a valid mesh')

    if mesh_object.lower() in ['triplane','surface']:
        return dem_object._surface_mesh,2
    elif mesh_object.lower() in ['prism','stacked','full']:
        return dem_object._stacked_mesh,3

    raise ValueError('Could not find a valid mesh')


def to_exodus(dem_object,outfile:str,facesets:list=None,mesh:str=None):
    '''
    Writes a mesh in the Exodus file format.
    Note that to export with facesets, the mesh must
    have depth - that is, `build_layered_mesh()` must have been
    called first.

    # Arguments
    dem_object (tinerator.DEM): a Tinerator DEM object
    outfile (str): path to save Exodus mesh to
    facesets (list): a list containing Faceset objects
    mesh (str): type of mesh to export ('surface' or 'prism')
    '''

    mesh,dims = __get_latest_mesh(dem_object,mesh)
    cmd = 'dump/exo/'+outfile+'/'+mesh.name
    
    if facesets is not None:
        fs_list = fs.write_facesets(dem_object,facesets)
        cmd += '///facesets &\n'
        cmd += ' &\n'.join(fs_list)

    if dims == 2:
        mesh.setatt('ndimensions_geom', 3)
        dem_object.lg.sendline(cmd)
        mesh.setatt('ndimensions_geom', 2)
    else:
        dem_object.lg.sendline(cmd)

    if facesets is not None:
        for file in fs_list:
            os.remove(file)

        faceset_count = len(fs_list)

        cfg.log.info(
            'Wrote Exodus mesh (of type: prism, with facesets: %d) to: \"%s\"' % \
            (faceset_count,outfile))

    else:
        cfg.log.info(
            'Wrote Exodus mesh to: \"%s\"' % outfile)



def to_avs(dem_object,outfile:str,mesh_object:str=None):
    '''
    Writes a mesh in the AVS-UCD file format.
    Note that facesets cannot be exported.

    # Arguments
    dem_object (tinerator.DEM): a Tinerator DEM object
    outfile (str): path to save Exodus mesh to
    mesh (str): type of mesh to export ('surface' or 'prism')
    '''

    mesh,dims = __get_latest_mesh(dem_object,mesh_object)

    if 'inp' not in outfile.lower():
        outfile += '.inp'

    mesh.dump(outfile)
    cfg.log.info('Saved mesh to %s' % outfile)
    


def middleton():
    # medium priority
    pass


