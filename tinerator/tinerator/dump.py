import subprocess

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

def extrudeTriangularMesh(infile,outfile,layers,lagrit_path="lagrit"):
    '''
    Extrudes a triangular mesh into a layered prism mesh.
    This function will be deprecated when PyLaGriT is updated for Python 3.

    :param infile: triplane mesh infile
    :type infile: string
    :param outfile: output mesh filepath
    :type outfile: string
    :param layers: an ordered list of extrusion depths
    :type outfile: list<float>
    :param lagrit_path: path to LaGriT
    :type lagrit_path: string
    '''

    if not isinstance(layers,list):
        layers = [layers]

    lagrit_string = '''
read/avs/{0}/motmp_top///tri
cmo printatt motmp_top -xyz- minmax
cmo/select/motmp_top
'''.format(infile)

    for (i,layer) in enumerate(layers):
        lagrit_string += '''
cmo/select/motmp_top
math/sub/motmp_top/zic/1,0,0/motmp_top/zic/ %f
dump/layer%d.inp/motmp_top
''' % (layer,i+1)

    lagrit_string += '\ncmo/delete/motmp_top'
    lagrit_string += '\ncmo/create/CMO_STACK\ncmo/select/CMO_STACK\nstack/layers/avs/ &\n'
    lagrit_string += '\n'.join(["  layer{0}.inp {0}  &".format(len(layers)-i) for i in range(len(layers))])
    lagrit_string += '  %s.inp 1  / flip\n' % infile
    lagrit_string += '''
stack/fill/CMO_PRISM / CMO_STACK                                                              
cmo select CMO_PRISM                                                                  
resetpts/itp

cmo/addatt/CMO_PRISM/ volume / cell_vol
cmo/printatt/CMO_PRISM/ volume minmax
dump avs %s CMO_PRISM
cmo printatt CMO_PRISM -all- minmax

finish

''' % outfile

    with open('_tmp_lagrit_infile.in','w') as f:
        f.write(lagrit_string)

    print(("%s < _tmp_lagrit_infile.in" % lagrit_path).split())

    subprocess.call(("%s < _tmp_lagrit_infile.in" % lagrit_path),shell=True,stderr=subprocess.STDOUT)
    deleteFile(infile)
    deleteFile("_tmp_lagrit_infile.in")

