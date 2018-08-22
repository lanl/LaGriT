import subprocess
import numpy as np

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
