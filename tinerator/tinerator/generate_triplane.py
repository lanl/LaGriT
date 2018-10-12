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

def generateLineConnectivity(nodes,connect_ends=False):
    connectivity = np.empty((np.shape(nodes)[0]-1,2),dtype=np.int)
    for i in range(np.shape(connectivity)[0]):
        connectivity[i] = np.array((i+1,i+2))
    return connectivity

def _writeLineAVS(boundary,outfile,connections=None):

    nnodes = np.shape(boundary)[0]
    nlines = np.shape(connections)[0] if connections is not None else 0

    with open(outfile,'w') as f:
        f.write("{} {} 0 0 0\n".format(nnodes,nlines))

        for i in range(nnodes):
            f.write("{} {} {} 0.0\n".format(i+1,boundary[i][0],boundary[i][1]))

        for i in range(nlines):
            f.write("{} 1 line {} {}\n".format(i+1,connections[i][0],connections[i][1]))

        f.write("\n")

def addElevation(lg:pylagrit.PyLaGriT,dem,triplane_path:str,flip:str='y',fileout=None):
    '''

    Given a triplane mesh and a tinerator.DEM instance, this function will 
    map elevation data from the array to the triplane mesh.

    :param pylagrit.PyLaGriT lg:
    :param str triplane: filepath to mesh to add 
    :param str att_name:
    :param str att_filepath:
    :param str fileout: 
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

    from matplotlib import pyplot as plt
    plt.imshow(_dem)
    plt.show()

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


def generateFaceSets_2(lg:pylagrit.PyLaGriT,inmesh:str,outfile:str,material_names=None,face_names=None,
                     north_line=None,flow_line=None):
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

    # 1. generate river line
    # 2. generate north_line
    # 3. read mesh as CMO_PRISM

    center = [4297.67,3925.68,0.]
    upper_left = [3956.31,7400.28,0.]
    upper_right = [7912.47,4961.96,0.]

    inlet = [5516.83,6059.2,0.]
    outlet = [859.651,280.406,0.]

    north_line = '''3 2 0 0 0
1 {} {} {}
2 {} {} {}
3 {} {} {}
1 1 line 1 2
2 1 line 2 3
'''.format(upper_left[0],upper_left[1],upper_left[2],
           center[0],center[1],center[2],
           upper_right[0],upper_right[1],upper_right[2])

    with open('north_line.inp','w') as f:
        f.write(north_line)

    river_line = '''2 1 0 0 0
1 {} {} {}
2 {} {} {}
1 1 line 1 2
'''.format(inlet[0],inlet[1],inlet[2],
           outlet[0],outlet[1],outlet[2])

    with open('river_line.inp','w') as f:
        f.write(river_line)

    #
    #_writeLineAVS(boundary,outfile,connections=None)

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

    a = '''
cmo create motri///triplane                                                    
cmo setatt motri ipolydat no
copypts motri motmp
  cmo setatt motri imt 1  
  cmo delete motmp

cmo select motri
triangulate/clockwise                                                    
#triangulate/counterclockwise
cmo setatt motri itetclr 1
cmo setatt motri imt 1
resetpts itp

quality edge_max y
cmo printatt motri edgemax minmax
quality
dump gmv tmp_tri.gmv motri
cmo copy mo motri 

# break up very large triangles 
cmo select motri

# edgemax  7.075320352E+01  5.180963546E+03
define EDGELEN 1000.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0 
smooth
recon 0 
smooth
recon 0 
smooth
rmpoint compress
quality edge_max y
cmo printatt motri edgemax minmax
dump gmv tmp1.gmv motri

define EDGELEN 500.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0
smooth
recon 0
smooth
recon 0
smooth
rmpoint compress
quality edge_max y
cmo printatt motri edgemax minmax
dump gmv tmp2.gmv motri

define EDGELEN 200.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0
smooth
recon 0
smooth
recon 0
smooth
rmpoint compress
quality edge_max y
cmo printatt motri edgemax minmax
dump gmv tmp2.gmv motri

define EDGELEN 100.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0
smooth
recon 0
smooth
recon 0
smooth
rmpoint compress
quality edge_max y
cmo printatt motri edgemax minmax
dump gmv tmp2.gmv motri

define EDGELEN 60.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0
smooth
recon 0
smooth
recon 0
smooth
rmpoint compress
quality edge_max y
cmo printatt motri edgemax minmax
dump gmv tmp3.gmv motri

define EDGELEN 30.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0
smooth
recon 0
smooth
rmpoint compress

# crop skinny tip
pset/py/attribute yic/1,0,0/ lt 4.314867400E+06
rmpoint pset,get,py
rmpoint compress
resetpts itp

define EDGELEN 15.
refine rivara///edge/1,0,0/EDGELEN///inclusive
recon 0
smooth
recon 0
smooth
rmpoint compress



### should be close to target edge length here ###

quality
quality edge_max y
quality edge_min y
cmo printatt motri edgemax minmax
cmo printatt motri edgemin minmax
dump gmv tmp4.gmv motri

define DAMAGE  1. 
define MAXEDGE  1.e+20 
define MINEDGE  10. 

# massage/MAXEDGE MINEDGE DAMAGE /1,0,0/                                               
smooth
recon 0
rmpoint compress
resetpts itp

# edge lengths good, try to improve aspect toward 1
smooth
recon 0
smooth
recon 0
smooth
recon 0
smooth
recon 0
smooth
recon 0
rmpoint compress
resetpts itp
quality aspect y
pset/pgood/attribute aratio/1,0,0/ gt .8
pset/prest/ not pgood
dump gmv tri_ref02.gmv motri   

## connect delaunay #############
rmpoint compress
recon 1
smooth
recon 0
recon 1

quality edge_max y
quality edge_min y
quality aspect y
cmo printatt motri edgemax minmax
cmo printatt motri edgemin minmax
cmo printatt motri aratio minmax
pset/pgood/attribute aratio/1,0,0/ gt .8
pset/prest/ not pgood

quality
cmo setatt motri ipolydat yes
cmo addatt motri vor_volume vorvol
dump gmv tri_recon.gmv motri
dump avs tri_recon.inp motri'''

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

    cmd += '## Triangulate Fracture without point addition \n'
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

    cmd += '# Creates a Coarse Mesh and then refines it using the distance field from intersections\n'
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

    cmd += '# Massage points based on linear function down to h_prime\n'
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