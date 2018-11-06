'''

Meshing related functions that rely on an interface with LaGriT.

'''

import os
import numpy as np
from numpy import genfromtxt, sqrt, cos, arcsin
from tinerator.dump import callLaGriT
import pylagrit
from copy import deepcopy
from scipy import interpolate

def generateLineConnectivity(nodes:np.ndarray,connect_ends:bool=False):
    '''
    Simple function to define a closed or open polyline for a set of 
    nodes. Assumes adjacency in array == implicit connection.
    That is, requires a clockwise- or counter-clockwise set of nodes.
    '''

    delta = 0 if connect_ends else -1
    size = np.shape(nodes)[0]
    connectivity = np.empty((size+delta,2),dtype=np.int)
    for i in range(size-1):
        connectivity[i] = np.array((i+1,i+2))
    if connect_ends:
        connectivity[-1] = np.array((size,1))
    return connectivity

def _writeLineAVS(boundary,outfile:str,connections=None,material_id=None,
                  node_atts:dict=None,cell_atts:dict=None):

    nnodes = np.shape(boundary)[0]
    nlines = np.shape(connections)[0] if connections is not None else 0
    natts = len(node_atts.keys()) if node_atts is not None else 0
    catts = len(cell_atts.keys()) if cell_atts is not None else 0

    if material_id is not None:
        assert np.shape(material_id)[0] >= nlines, \
        'Mismatch count between material ID and cells'

    with open(outfile,'w') as f:
        f.write("{} {} {} {} 0\n".format(nnodes,nlines,natts,catts))

        for i in range(nnodes):
            f.write("{} {} {} 0.0\n".format(i+1,boundary[i][0],boundary[i][1]))

        for i in range(nlines):
            mat_id = material_id[i] if material_id is not None else 1
            f.write("{} {} line {} {}\n".format(i+1,mat_id,connections[i][0],connections[i][1]))

        if natts:

            for key in node_atts.keys():
                assert np.shape(node_atts[key])[0] >= nnodes, \
                'Length of node attribute %s does not match length of points array' % key

            # 00007  1  1  1  1  1  1  1
            f.write(str(natts) + ' 1'*natts + '\n')

            # imt1, integer
            # itp1, integer
            _t = '\n'.join([key + ', ' + 'integer' if node_atts[key].dtype == int else 'real' for key in node_atts.keys()])
            f.write(_t + '\n')

            for i in range(nnodes):
                _att_str = '%d' % (i+1)
                for key in node_atts.keys():
                    _att_str += ' ' + str(node_atts[key][i])
                _att_str += '\n'
                f.write(_att_str)

        if catts:

            for key in cell_atts.keys():
                assert np.shape(cell_atts[key])[0] >= nlines, \
                'Length of cell attribute %s does not match length of elem array' % key

            # 00007  1  1  1  1  1  1  1
            f.write(str(catts) + ' 1'*catts + '\n')

            # imt1, integer
            # itp1, integer
            _t = '\n'.join([key + ', ' + 'integer' if cell_atts[key].dtype == int else 'real' for key in cell_atts.keys()])
            f.write(_t + '\n')

            for i in range(nlines):
                _att_str = '%d' % (i+1)
                for key in cell_atts.keys():
                    _att_str += ' ' + str(cell_atts[key][i])
                _att_str += '\n'
                f.write(_att_str)

        f.write("\n")

def addElevation(lg:pylagrit.PyLaGriT,dem,triplane_path:str,flip:str='y',fileout=None):
    '''

    Given a triplane mesh and a tinerator.DEM instance, this function will 
    map elevation data from the array to the triplane mesh.

    :param pylagrit.PyLaGriT lg:
    :param str triplane: filepath to mesh to add 
    '''

    # Generate sheet metadata
    dem_dimensions = [dem.ncols,dem.nrows]
    lower_left_corner = [dem.xll_corner, dem.yll_corner]
    cell_size = [dem.cell_size,dem.cell_size]

    # Overwrite original mesh if a path isn't provided
    if fileout is None:
        fileout = triplane_path

    # Interpolate no data values on the DEM
    # This is to prevent a noise effect on LaGriT interpolation 
    _dem = deepcopy(dem.dem).astype(float)
    _dem[_dem == dem.no_data_value] = np.nan

    # Mask invalid values
    _dem = np.ma.masked_invalid(_dem)
    xx, yy = np.meshgrid(np.arange(0,_dem.shape[1]),np.arange(0,_dem.shape[0]))

    #get only the valid values
    x1 = xx[~_dem.mask]
    y1 = yy[~_dem.mask]
    newarr = _dem[~_dem.mask]

    _dem = interpolate.griddata((x1, y1), newarr.ravel(), (xx, yy), method='nearest')
    _dem[_dem == np.nan] = dem.no_data_value

    # Dump DEM data
    _array_out = "_temp_elev_array.dat"
    _dem.filled().tofile(_array_out,sep=' ',format='%.3f')

    # Read DEM as a quad mesh
    tmp_sheet = lg.read_sheetij('surfacemesh', _array_out, dem_dimensions,lower_left_corner, cell_size, flip=flip)

    # Remove no_data_value elements
    comp = 'le' if dem.no_data_value <= 0. else 'ge'
    dud_value = dem.no_data_value - np.sign(dem.no_data_value)
    pduds = tmp_sheet.pset_attribute('zic',dud_value,comparison=comp,name='pduds')
    eduds = pduds.eltset(membership='inclusive',name='eduds')
    tmp_sheet.rmpoint_eltset(eduds,compress=True,resetpts_itp=True)

    # Copy elevation to new variable (priming for interpolation)
    tmp_sheet.addatt('z_elev')
    tmp_sheet.copyatt('zic','z_elev',tmp_sheet)
    tmp_sheet.setatt('zic',0.)

    # Load triplane && interpolate z-values onto mesh
    triplane = lg.read(triplane_path,name='triplanemesh')
    triplane.addatt('z_new')
    triplane.interpolate('continuous', 'z_new', tmp_sheet, 'z_elev')
    triplane.copyatt('z_new','zic')
    triplane.delatt('z_new')
    tmp_sheet.delete()
    triplane.dump(fileout)
    os.remove(_array_out)

def stackLayers(lg:pylagrit.PyLaGriT,infile:str,outfile:str,layers:list,
                matids=None,xy_subset=None,nlayers=None):
    '''
    Created a stacked mesh with layer thickness described by the variable
    layers.

    :param lg: Instantiation of PyLaGriT() class
    :type lg: pylagrit.PyLaGriT()
    :param outfile: path to save mesh
    :type outfile: str
    :param layers: sequential list of layer thickness
    :type layers: list<float>
    :param matids: material ids for layers
    :type matids: list
    '''

    stack_files = ['layer%d.inp' % (len(layers)-i) for i in range(len(layers))]
    if nlayers is None:
        nlayers=['']*(len(stack_files)-1)
    
    motmp_top = lg.read(infile)

    for (i,offset) in enumerate(layers):
        motmp_top.math('sub','zic',value=offset)
        motmp_top.dump('layer%d.inp' % (i+1))

    motmp_top.delete()
    stack = lg.create()
    stack.stack_layers(stack_files,flip_opt=True,nlayers=nlayers,matids=matids,xy_subset=xy_subset)
    stack.dump('tmp_layers.inp')

    cmo_prism = stack.stack_fill(name='CMO_PRISM')
    cmo_prism.resetpts_itp()

    cmo_prism.addatt('cell_vol',keyword='volume')
    cmo_prism.dump(outfile)

def generateSingleColumnPrism(lg:pylagrit.PyLaGriT,infile:str,outfile:str,layers:list,
                matids=None,xy_subset=None,nlayers=None):
    '''
    Created a stacked mesh with layer thickness described by the variable
    layers.

    :param lg: Instantiation of PyLaGriT() class
    :type lg: pylagrit.PyLaGriT()
    :param outfile: path to save mesh
    :type outfile: str
    :param layers: sequential list of layer thickness
    :type layers: list<float>
    :param matids: material ids for layers
    :type matids: list
    '''

    stack_files = ['layer%d.inp' % (len(layers)-i) for i in range(len(layers))]
    if nlayers is None:
        nlayers=['']*(len(stack_files)-1)
    
    motmp_top = lg.read(infile)
    motmp_top.setatt('itetclr',2)
    motmp_top.setatt('itetclr','1,1,0 1')

    for (i,offset) in enumerate(layers):
        motmp_top.math('sub','zic',value=offset)
        motmp_top.dump('layer%d.inp' % (i+1))

    motmp_top.delete()
    stack = lg.create()
    stack.stack_layers(stack_files,flip_opt=True,nlayers=nlayers,matids=matids,xy_subset=xy_subset)
    stack.dump('tmp_layers.inp')

    cmo_prism = stack.stack_fill(name='CMO_PRISM')
    cmo_prism.resetpts_itp()

    cmo_prism.addatt('cell_vol',keyword='volume')
    cmo_prism.dump(outfile)

def generateFaceSetsNaive(lg:pylagrit.PyLaGriT,stacked_mesh:str,outfile:str,material_names=None,face_names=None):

    #stack_hex = lg.read(inmesh)
    #lg.createpts()

    cmd = '''define CMO_PRISM motmp2
read/avs/{}/CMO_PRISM
infile infile_get_facesets3.mlgi

# WRITE 3 faceset version of mesh
dump/exo/{}/CMO_PRISM///facesets &
fs1_bottom.avs fs2_top.avs fs3_sides_all.avs

finish
'''.format(stacked_mesh,outfile)

    callLaGriT(cmd,lagrit_path=lg.lagrit_exe)


def generateFaceSets(lg:pylagrit.PyLaGriT,inmesh:str,outfile:str,material_names=None,face_names=None):
    '''
    Generate boundary face sets according to normal vectors and layer ID.

    :param lg: running instance of PyLaGriT
    :type lg: pylagrit.PyLaGriT
    :param outfile: filepath to save Exodus facesets
    :type outfile: str
    :param material_names: material_id,material_name key/value pairs
    :type material_names: dict
    :param face_names: faceset_id/faceset_name key/value pairs
    :type face_names: dict

    '''

    stack_hex = lg.read(inmesh)

    if material_names is None:
        material_names = {10000:'soil: top layer',
                          20000:'soil: middle layer',
                          30000:'soil: bottom layer',
                          40000:'geology layer',
                          50000:'bottom layer'}

    if face_names is None:
        face_names = {1:'bottom face',
                     2:'surface',
                     3:'front',
                     4:'right',
                     5:'back',
                     6:'left'}

    if '.' in outfile:
        xml_outfile = '.'.join(outfile.split('.')[:-1])+'.xml'
    else:
        xml_outfile = outfile+'.xml'

    lg.sendline('boundary_components')

    # Automatically create face sets based on normal vectors and layer id
    fs = stack_hex.create_boundary_facesets(base_name='faceset_bounds',stacked_layers=True)

    # Write exo file with boundary facesets
    stack_hex.dump_exo(outfile,facesets=fs.values())

    # Dump ats style xml for mesh, can provide options for other schemas easily also
    stack_hex.dump_ats_xml(xml_outfile,outfile,matnames=material_names,facenames=face_names)    

def buildUniformTriplane(lg:pylagrit.PyLaGriT,boundary:np.ndarray,outfile:str,
                         counterclockwise:bool=False,connectivity:bool=None,
                         min_edge:float=16.):

    if connectivity is None:
        connectivity = generateLineConnectivity(boundary)

    _writeLineAVS(boundary,"poly_1.inp",connections=connectivity)

    cmd = '# UNIFORM TRIPLANE \n '

    cmd += 'define / ID / 1\n'
    cmd += 'define / OUTFILE_GMV / mesh_1.gmv\n'
    cmd += 'define / OUTFILE_AVS / %s\n' % outfile
    cmd += 'define / OUTFILE_LG / mesh_1.lg\n'

    cmd += 'define / H_PRIME / ' + str(0.8*min_edge) + '\n'
    cmd += 'define / H_SCALE / ' + str(min_edge) + '\n'
    cmd += 'define / H_SCALE2 / ' + str(2*min_edge) + '\n'
    cmd += 'define / H_SCALE4 / ' + str(4*min_edge) + '\n'
    cmd += 'define / H_SCALE8 / ' + str(8*min_edge) + '\n'
    cmd += 'define / H_SCALE16 / ' + str(16*min_edge) + '\n'
    cmd += 'define / H_SCALE32 / ' + str(32*min_edge) + '\n'
    cmd += 'define / H_SCALE64 / ' + str(64*min_edge) + '\n'

    cmd += 'define / POLY_FILE / poly_1.inp\n'

    cmd += 'read / POLY_FILE / motmp\n'
    
    cmd += 'cmo create motri///triplane\n'
    cmd += 'cmo setatt motri ipolydat no\n'
    cmd += 'copypts motri motmp\n'
    cmd += '  cmo setatt motri imt 1\n'
    cmd += '  cmo delete motmp\n'

    cmd += 'cmo select motri\n'

    if counterclockwise:
        cmd += 'triangulate/counterclockwise\n'
    else:
        cmd += 'triangulate/clockwise\n'
        
    cmd += 'cmo setatt motri itetclr 1\n'
    cmd += 'cmo setatt motri imt 1\n'
    cmd += 'resetpts itp\n'

    cmd += 'quality edge_max y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'
    cmd += 'quality\n'
  #  cmd += 'dump gmv tmp_tri.gmv motri\n'
    cmd += 'cmo copy mo motri \n'

    cmd += '# break up very large triangles \n'
    cmd += 'cmo select motri\n'

    cmd += 'refine rivara///edge/1,0,0/H_SCALE64///inclusive\n'
    cmd += 'recon 0 \n'
    cmd += 'smooth\n'
    cmd += 'recon 0 \n'
    cmd += 'smooth\n'
    cmd += 'recon 0 \n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'
    cmd += 'quality edge_max y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'
   
    cmd += 'refine rivara///edge/1,0,0/H_SCALE32///inclusive\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'
    cmd += 'quality edge_max y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'

    cmd += 'refine rivara///edge/1,0,0/H_SCALE16///inclusive\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'
    cmd += 'quality edge_max y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'

    cmd += 'define EDGELEN H_SCALE8\n'
    cmd += 'refine rivara///edge/1,0,0/H_SCALE8///inclusive\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'
    cmd += 'quality edge_max y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'

    cmd += 'refine rivara///edge/1,0,0/H_SCALE4///inclusive\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'
    cmd += 'quality edge_max y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'
    cmd += 'dump gmv tmp3.gmv motri\n'

    cmd += 'refine rivara///edge/1,0,0/H_SCALE2///inclusive\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'
    
    cmd += 'refine rivara///edge/1,0,0/H_SCALE///inclusive\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'rmpoint compress\n'

    cmd += '### should be close to target edge length here ###\n'

    cmd += 'quality\n'
    cmd += 'quality edge_max y\n'
    cmd += 'quality edge_min y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'
    cmd += 'cmo printatt motri edgemin minmax\n'

    cmd += 'define DAMAGE  1. \n'
    cmd += 'define MAXEDGE  1.e+20 \n'
   
    cmd += '# massage/MAXEDGE H_PRIME DAMAGE /1,0,0/\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'rmpoint compress\n'
    cmd += 'resetpts itp\n'

    # edge lengths good, try to improve aspect toward 1
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'rmpoint compress\n'
    cmd += 'resetpts itp\n'
    cmd += 'quality aspect y\n'
    cmd += 'pset/pgood/attribute aratio/1,0,0/ gt .8\n'
    cmd += 'pset/prest/ not pgood\n'

    ## connect delaunay
    cmd += 'rmpoint compress\n'
    cmd += 'recon 1\n'
    cmd += 'smooth\n'
    cmd += 'recon 0\n'
    cmd += 'recon 1\n'

    cmd += 'quality edge_max y\n'
    cmd += 'quality edge_min y\n'
    cmd += 'quality aspect y\n'
    cmd += 'cmo printatt motri edgemax minmax\n'
    cmd += 'cmo printatt motri edgemin minmax\n'
    cmd += 'cmo printatt motri aratio minmax\n'
    cmd += 'pset/pgood/attribute aratio/1,0,0/ gt .8\n'
    cmd += 'pset/prest/ not pgood\n'

    cmd += 'quality\n'
    cmd += 'cmo setatt motri ipolydat yes\n'
    cmd += 'cmo addatt motri vor_volume vorvol\n'
    cmd += 'dump gmv OUTFILE_GMV motri\n'
    cmd += 'dump avs OUTFILE_AVS motri\n'

    cmd += 'finish\n'

    callLaGriT(cmd,lagrit_path=lg.lagrit_exe)

def buildRefinedTriplane(lg:pylagrit.PyLaGriT,boundary:np.ndarray,feature:np.ndarray,outfile:str,
                         h:float,connectivity:bool=None,delta:float=0.75,
                         slope:float=2.,refine_dist:float=0.5):
    '''
    Constructs a triplane mesh using LaGriT as a backend.
    Requires an Nx2 np.ndarray as a boundary input, and an Nx2 np.ndarray as a 
    feature input.

    :param boundary: captured DEM boundary
    :type boundary: np.ndarray
    :param feature: feature to refine around, found through deliniation
    :type feature: np.ndarray
    :param outfile: path to save mesh
    :type outfile: str
    :returns: nodes,connectivity
    '''

    #TODO: convert to pylagrit

    if connectivity is None:
        connectivity = generateLineConnectivity(boundary)

    _writeLineAVS(boundary,"poly_1.inp",connections=connectivity)
    _writeLineAVS(feature,"intersections_1.inp")

    cmd = ''

    h_extrude = 0.5*h # upper limit on spacing of points on interssction line
    h_radius = sqrt((0.5*h_extrude)**2 + (0.5*h_extrude)**2)
    h_trans = -0.5*h_extrude + h_radius*cos(arcsin(delta))
    counterclockwise = False

    cmd += 'define / ID / 1\n'
    cmd += 'define / OUTFILE_GMV / mesh_1.gmv\n'
    cmd += 'define / OUTFILE_AVS / %s\n' % outfile
    cmd += 'define / OUTFILE_LG / mesh_1.lg\n'

    cmd += 'define / POLY_FILE / poly_1.inp\n'
    cmd += 'define / LINE_FILE / intersections_1.inp \n'

    cmd += 'define / QUAD_FILE / tmp_quad_1.inp\n'
    cmd += 'define / EXCAVATE_FILE / tmp_excavate_1.inp\n'
    cmd += 'define / PRE_FINAL_FILE / tmp_pre_final_1.inp\n'
    cmd += 'define / PRE_FINAL_MASSAGE / tmp_pre_final_massage_1.gmv\n'
    cmd += 'define / H_SCALE / ' + str(h) + '\n'
    cmd += 'define / H_EPS / ' + str(h*10**-7) + '\n'
    cmd += 'define / H_SCALE2 / ' + str(1.5*h) + '\n'
    cmd += 'define / H_EXTRUDE / ' + str(h_extrude) + '\n'
    cmd += 'define / H_TRANS / ' + str(h_trans) + '\n'
    cmd += 'define / H_PRIME / ' + str(0.8*h) + '\n'
    cmd += 'define / H_PRIME2 / ' + str(0.3*h) + '\n'
    cmd += 'define / H_SCALE3 / ' + str(3*h) + '\n'
    cmd += 'define / H_SCALE8 / ' + str(8*h) + '\n'
    cmd += 'define / H_SCALE16 / ' + str(16*h) + '\n'
    cmd += 'define / H_SCALE32 / ' + str(32*h) + '\n'
    cmd += 'define / H_SCALE64 / ' + str(64*h) + '\n'
    cmd += 'define / PURTURB8 / ' + str(8*0.05*h) + '\n'
    cmd += 'define / PURTURB16 / ' + str(16*0.05*h) + '\n'
    cmd += 'define / PURTURB32 / ' + str(32*0.05*h) + '\n'
    cmd += 'define / PURTURB64 / ' + str(64*0.05*h) + '\n'
    cmd += 'define / PARAM_A / '+str(slope)+'\n'
    cmd += 'define / PARAM_B / '+str(h*(1-slope*refine_dist))+'\n'
    cmd += 'define / PARAM_A2 / '+str(0.5*slope)+'\n'
    cmd += 'define / PARAM_B2 / '+str(h*(1 - 0.5*slope*refine_dist))+'\n'
    cmd += 'define / THETA  / 0.000000000000 \n'
    cmd += 'define / X1 /  -0.000000000000 \n'
    cmd += 'define / Y1 / -0.000000000000 \n'
    cmd += 'define / Z1 / -0.000000000000 \n'
    cmd += 'define / X2 / 0.000000000000 \n'
    cmd += 'define / Y2 / 0.000000000000 \n'
    cmd += 'define / Z2 / 0.000000000000 \n'
    cmd += 'define / FAMILY / 2 \n'

    #cmd += 'define / POLY_FILE / poly_1.inp \n'
    cmd += 'define / OUTPUT_INTER_ID_SSINT / id_tri_node_1.list\n'

    cmd += 'read / POLY_FILE / mo_poly_work\n'
    cmd += 'read / LINE_FILE / mo_line_work \n'

    ## Triangulate Fracture without point addition
    cmd += 'cmo / create / mo_pts / / / triplane \n'
    cmd += 'copypts / mo_pts / mo_poly_work \n'
    cmd += 'cmo / select / mo_pts \n'

    if counterclockwise:
        cmd += 'triangulate / counterclockwise # change to clockwise based on bndry\n'
    else:
        cmd += 'triangulate / clockwise # change to clockwise based on bndry\n'

    cmd += 'cmo / setatt / mo_pts / imt / 1 0 0 / ID \n'
    cmd += 'cmo / setatt / mo_pts / itetclr / 1 0 0 / ID \n'
    cmd += 'resetpts / itp \n'
    cmd += 'cmo / delete / mo_poly_work \n'
    cmd += 'cmo / select / mo_pts \n'

    # Creates a Coarse Mesh and then refines it using the distance field from intersections
    cmd += 'massage / H_SCALE64 / H_EPS  / H_EPS\n'
    cmd += 'recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0\n'
    cmd += 'resetpts / itp\n'
    cmd += 'pset / p_move / attribute / itp / 1 0 0 / 0 / eq\n'
    cmd += 'perturb / pset get p_move / PERTURB64 PERTURB64 0.0\n'
    cmd += 'recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0\n'
    cmd += 'smooth;recon 0;smooth;recon 0;smooth;recon 0\n'

    cmd += 'massage / H_SCALE32 / H_EPS / H_EPS\n'
    cmd += 'resetpts / itp\n'
    cmd += 'pset / p_move / attribute / itp / 1 0 0 / 0 / eq\n'
    cmd += 'perturb / pset get p_move / PERTURB32 PERTURB32 0.0\n'
    cmd += 'recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0\n'
    cmd += 'smooth;recon 0;smooth;recon 0;smooth;recon 0\n'

    cmd += 'massage / H_SCALE16 / H_EPS  / H_EPS\n'
    cmd += 'resetpts / itp\n'
    cmd += 'pset / p_move / attribute / itp / 1 0 0 / 0 / eq\n'
    cmd += 'perturb / pset get p_move / PERTURB16 PERTURB16 0.0\n'
    cmd += 'recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0\n'
    cmd += 'smooth;recon 0;smooth;recon 0;smooth;recon 0\n'

    cmd += 'massage / H_SCALE8 / H_EPS / H_EPS\n'
    cmd += 'resetpts / itp\n'
    cmd += 'pset / p_move / attribute / itp / 1 0 0 / 0 / eq\n'
    cmd += 'perturb / pset get p_move / PERTURB8 PERTURB8 0.0\n'
    cmd += 'recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0\n'
    cmd += 'smooth;recon 0;smooth;recon 0;smooth;recon 0\n'

    cmd += 'cmo/addatt/ mo_pts /x_four/vdouble/scalar/nnodes \n'
    cmd += 'cmo/addatt/ mo_pts /fac_n/vdouble/scalar/nnodes \n'

    # Massage points based on linear function down to h_prime
    cmd += 'massage2/user_function2.lgi/H_PRIME/fac_n/1.e-5/1.e-5/1 0 0/strictmergelength \n'

    cmd += 'assign///maxiter_sm/1 \n'
    cmd += 'smooth;recon 0;smooth;recon 0;smooth;recon 0\n'

    cmd += 'assign///maxiter_sm/10\n'

    cmd += 'massage2/user_function.lgi/H_PRIME/fac_n/1.e-5/1.e-5/1 0 0/strictmergelength \n'
    cmd += 'cmo / DELATT / mo_pts / rf_field_name \n'

    ################################################
    cmd += 'dump / %s / mo_pts\n' % outfile
    cmd += 'finish\n'
    ################################################

    callLaGriT(cmd,lagrit_path=lg.lagrit_exe)

def generateComplexFacesets(lg:pylagrit.PyLaGriT,outfile:str,mesh_file:str,
                            boundary:np.ndarray,facesets:np.ndarray):
    '''
    A new technique for generating Exodus facesets from the material ID of
    boundary line segments.

    By providing the array `boundary_attributes`, of equal length to `boundary`,
    the line segment material IDs are used as indentifiers for unique facesets.

    The top and bottom facesets will always be generated; there will be a
    minimum of one side facesets if boundary_attributes is set to a uniform
    value, or up to length(boundary) number of facesets, if each value in
    boundary_attributes is unique.

    LaGriT methodology developed by Terry Ann Miller, Los Alamos Natl. Lab.

    :param lg: Instance of PyLaGriT
    :type lg: pylagrit.PyLaGriT
    :param outfile: Exodus file to write out to
    :type outfile: str
    :param mesh_file: Stacked mesh file to read
    :type mesh_file: str
    :param boundary: A boundary containing the convex hull of the mesh
    :type boundary: np.ndarray
    :param boundary_attributes: An array containing integer values
    :type boundary_attributes: np.ndarray
    '''

    _cleanup = []

    cell_atts = None
    has_top = False
    if isinstance(facesets,dict):
        boundary_attributes = facesets['all']
        if 'top' in facesets.keys():
            cell_atts = { 'ioutlet': facesets['top'] }
            has_top = True
    else:
        boundary_attributes = facesets

    boundary_attributes = deepcopy(boundary_attributes) - np.min(boundary_attributes) + 1

    # Test that the array does not have values that 'skip' an integer,
    # i.e., [1,4,3] instead of [1,3,2]
    assert np.all(np.unique(boundary_attributes) == \
           np.array(range(1,np.size(np.unique(boundary_attributes))+1))),\
           'boundary_attributes cannot contain non-sequential values'

    boundary_file = "_boundary_line_colors.inp"
    _writeLineAVS(boundary,boundary_file,
                  connections=generateLineConnectivity(boundary,connect_ends=True),
                  material_id=boundary_attributes,cell_atts=cell_atts)

    _cleanup.append(boundary_file)

    cmo_in = lg.read(mesh_file)
    cmo_in.resetpts_itp()
    
    cmo_bndry = lg.read(boundary_file)
    cmo_bndry.resetpts_itp()

    # Extract surface w/ cell & face attributes to get the outside face to element relationships
    mo_surf = lg.extract_surfmesh(cmo_in=cmo_in,stride=[1,0,0],external=True,resetpts_itp=True)
    mo_surf.addatt('id_side',vtype='vint',rank='scalar',length='nelements')
    mo_surf.select()
    mo_surf.settets_normal()
    mo_surf.copyatt('itetclr',attname_sink='id_side',mo_src=mo_surf)

    for att in ['itetclr0','idnode0','idelem0','facecol','itetclr1','idface0',\
                'nlayers','nnperlayer','neperlayer','ikey_utr']:
        mo_surf.delatt(att)

    # use stack attribute layertyp to set top and bottom
    # set all sides to default 3 all
    mo_surf.select()
    mo_surf.setatt('id_side',3)

    ptop = mo_surf.pset_attribute('layertyp',-2,comparison='eq',stride=[1,0,0])
    pbot = mo_surf.pset_attribute('layertyp',-1,comparison='eq',stride=[1,0,0])

    etop = ptop.eltset(membership='exclusive')
    ebot = pbot.eltset(membership='exclusive')

    mo_surf.setatt('id_side',2,stride=['eltset','get',etop.name])
    mo_surf.setatt('id_side',1,stride=['eltset','get',ebot.name])

    mo_surf.copyatt('id_side',attname_sink='itetclr',mo_src=mo_surf)

    # Set default node imt based on top, bottom, sides
    # NOTE nodes at top/side edge are set to 3 side
    # change order of setatt to overwrite differently

    mo_surf.select()
    mo_surf.setatt('imt',1)
    esides = mo_surf.eltset_attribute('id_side',3)
    psides = esides.pset()

    mo_surf.setatt('imt',2,stride=['pset','get',ptop.name])
    mo_surf.setatt('imt',1,stride=['pset','get',pbot.name])
    mo_surf.setatt('imt',3,stride=['pset','get',psides.name])

    mo_surf.select()
    mo_surf.addatt('zsave',vtype='vdouble',rank='scalar',length='nnodes')
    mo_surf.copyatt('zic',mo_src=mo_surf,attname_sink='zsave')
    mo_surf.setatt('zic',0.)
    cmo_bndry.setatt('zic',0.)

    # INTERPOLATE boundary faces to side faces
    # and set numbering so 1 and 2 are top and bottom

    cmo_bndry.math('add','itetclr',value=2,stride=[1,0,0],cmosrc=cmo_bndry,attsrc='itetclr')
    mo_surf.interpolate('map','id_side',cmo_bndry,'itetclr',stride=['eltset','get',esides.name])

    if has_top:
        mo_surf.addatt('ioutlet',vtype='vint',rank='scalar',length='nelements')
        mo_surf.addatt('ilayer',vtype='vint',rank='scalar',length='nelements')
        mo_surf.interpolate('map','ioutlet',cmo_bndry,'ioutlet',stride=['eltset','get',esides.name])

    mo_surf.copyatt('zsave',mo_src=mo_surf,attname_sink='zic')
    mo_surf.delatt('zsave')

    mo_surf.setatt('id_side',2,stride=['eltset','get',etop.name])
    mo_surf.setatt('id_side',1,stride=['eltset','get',ebot.name])

    # check material numbers, must be greater than 0
    # id_side is now ready for faceset selections
    mo_surf.copyatt('id_side',attname_sink='itetclr',mo_src=mo_surf)

    _all_facesets = []
    faceset_count = np.size(np.unique(boundary_attributes)) + 2

    # This creates a top-layer boundary faceset by setting the area defined in 
    # ioutlet to a unique value *only* on the top layer.
    if has_top:

        mo_surf.select()
        mo_surf.setatt('ilayer',0.)

        elay_inc = ptop.eltset(membership='inclusive')
        mo_surf.setatt('ilayer',1,stride=['eltset','get',elay_inc.name])
        mo_surf.setatt('ilayer',0,stride=['eltset','get',etop.name]) # ????

        e1 = mo_surf.eltset_attribute('ilayer',1,boolstr='eq')
        e2 = mo_surf.eltset_attribute('ioutlet',2,boolstr='eq')

        faceset_count += 1

        e_out1 = mo_surf.eltset_inter([e1,e2])
        mo_surf.setatt('id_side',faceset_count,stride=['eltset','get',e_out1])

        mo_surf.delatt('ioutlet')
        mo_surf.delatt('ilayer')

    # Capture and write all facesets
    for ss_id in range(1,faceset_count+1):
        fname = 'ss%d_fs.faceset' % ss_id

        mo_tmp = mo_surf.copy()
        mo_tmp.select()
        e_keep = mo_tmp.eltset_attribute('id_side',ss_id,boolstr='eq')
        e_delete = mo_tmp.eltset_bool(boolstr='not',eset_list=[e_keep])
        mo_tmp.rmpoint_eltset(e_delete,compress=True,resetpts_itp=False)

        mo_tmp.delatt('layertyp')
        mo_tmp.delatt('id_side')

        lg.sendline('dump / avs2 / '+fname+'/'+mo_tmp.name+'/ 0 0 0 2')
        mo_tmp.delete()
        _all_facesets.append(fname)

    # Write exodus with faceset files
    cmd = 'dump/exo/'+outfile+'/'+cmo_in.name+'///facesets &\n'
    cmd += ' &\n'.join(_all_facesets)

    lg.sendline(cmd)
    _cleanup.extend(_all_facesets)

def getFacesetsFromCoordinates(coords:dict,boundary:np.ndarray):
    '''
    Given an array of points on or near the boundary, generate the material_id
    array required for a facesets function.

    The points *must* be ordered clockwise: that is, the algorithm will generate
    facesets under the assumption that the intermediate space between two
    adjacent points is where a new faceset should be placed.

    Example:

        _coords = np.array([[3352.82,7284.46],[7936.85,4870.53],\
                            [1798.4,256.502],[1182.73,1030.19]])
        fs = getFacesetsFromCoordinates(_coords,my_dem.boundary)

    :param coords: ordered array of coordinates between which to add facesets
    :type coords: np.ndarray
    :param boundary: tinerator.DEM boundary
    :type boundary: np.ndarray
    :returns: faceset array for 
    '''

    from scipy.spatial import distance

    facesets = {}

    if isinstance(coords,(list,np.ndarray)):
        coords = {'all':coords}

    for key in coords:
        mat_ids = np.full((np.shape(boundary)[0],),1,dtype=int)
        fs = []

        # Iterate over given coordinates and find the closest boundary point...
        for c in coords[key]:
            ind = distance.cdist([c], boundary[:,:2]).argmin()
            fs.append(ind)

        fs.sort(reverse=True)

        # Map the interim space as a new faceset.
        # 'Unmarked' facesets have a default filled value of 1
        for i in range(len(fs)):
            mat_ids[fs[-1]:fs[i]] = i+2

        facesets[key] = mat_ids

    # TODO: what to do in case of 'all' being undefined?

    return facesets
