import os
import subprocess
import numpy as np
import tinerator.facesets as fs
import tinerator.config as cfg

def __get_latest_mesh(dem_object,mesh_object=None):
    '''
    Given a string describing the mesh type, returns
    the proper DEM class attribute.
    '''

    if mesh_object.lower() in ['triplane','surface']:
        return dem_object._surface_mesh,2
    elif mesh_object.lower() in ['prism','stacked','full']:
        return dem_object._stacked_mesh,3

    if mesh_object is None:
        if dem_object._stacked_mesh is not None:
            return dem_object._stacked_mesh
        elif dem_object._surface_mesh is not None:
            return dem_object._surface_mesh

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

    # Optional Arguments
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



def avs(dem,mesh_object:str=None):
    __get_latest_mesh(mesh_object=mesh_object)
    pass


def middleton():
    # medium priority
    pass






def deleteFile(filename):
    subprocess.call(("rm %s" % filename),shell=True,stderr=subprocess.STDOUT)

def writeAVS(outfile:str,points:np.ndarray,triangles=None,node_attributes=None,cell_attributes=None):
    '''
    Write out a collection of points and triangles to an AVS mesh file.

    :param outfile:
    :type outfile:
    :param points:
    :type points:
    '''

    nnodes = len(points)
    ntris  = len(triangles) if triangles is not None else 0
    natts  = 0
    ncatts = 0

    # Ensure the user has packed
    if nnodes == 0:
        warn("Mesh object contains no points - aborting `save_avs`.")
        return
    
    # Make sure the file can be written to
    try:
        f = open(outfile,'w')
    except IOError:
        myError = "Could not write to file {}.\nI/O error({}): {}".format(outfile,e.errno,e.strerror)
        error(myError)
    
    # Write out header
    f.write("{:11d}{:11d}{:11d}{:11d}{:11d}\n".format(nnodes,ntris,natts,ncatts,0))

    # Write out nodes
    for i in range(0,nnodes):
        f.write("{:03d}  {:010E}  {:010E}  {:010E}\n".format(i+1,points[i][0],points[i][1],points[i][2]))

    # Write out triangles
    if ntris > 0:
        for i in range(0,ntris):
            f.write("{:03d}        1   tri  {}  {}  {}\n".format(i+1,triangles[i][0]+1,triangles[i][1]+1,triangles[i][2]+1))

    # Add support for attributes here.

    f.close()

def callLaGriT(commands:str,lagrit_path:str="lagrit"):
    with open('_tmp_lagrit_infile.in','w') as f:
        f.write(commands)

    print(("%s < _tmp_lagrit_infile.in" % lagrit_path).split())

    subprocess.call(("%s < _tmp_lagrit_infile.in" % lagrit_path),shell=True,stderr=subprocess.STDOUT)
    deleteFile("_tmp_lagrit_infile.in")    
