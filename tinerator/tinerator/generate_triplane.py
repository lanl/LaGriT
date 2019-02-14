'''

Meshing related functions that rely on an interface with LaGriT.

'''

import os
import numpy as np
from numpy import genfromtxt, sqrt, cos, arcsin
from tinerator.dump import callLaGriT
from tinerator.lg_infiles import Infiles
from tinerator.unit_conversion import cleanup
import tinerator.config as cfg
import pylagrit
from copy import deepcopy
from scipy import interpolate
from scipy import ndimage as nd
from scipy.misc import imresize
import string
import random
import logging

def smoothRasterBoundary(raster:np.ndarray,width:int,no_data_value:float=np.nan):
    raster[raster == no_data_value] = np.nan
    width += width % 2 # Keep even for padding

    mask = np.array(~np.isnan(raster),dtype=int)
    dims = mask.shape
    edges = imresize(mask,(dims[0]-width,dims[1]-width),interp='nearest')
    edges = np.pad(edges,pad_width=int(width/2),mode='constant',constant_values=True)
    edges = ~edges & mask

    raster[edges == True] = no_data_value
    return raster

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

def addElevation(lg:pylagrit.PyLaGriT,dem,triplane_path:str,flip:str='y',fileout=None,
                 smooth_boundary:bool=False):
    '''

    Given a triplane mesh and a tinerator.DEM instance, this function will 
    map elevation data from the array to the triplane mesh.

    :param pylagrit.PyLaGriT lg:
    :param str triplane: filepath to mesh to add 
    '''

    # Generate sheet metadata
    dem_dimensions = [dem.ncols,dem.nrows]
    lower_left_corner = [dem.xll_corner,dem.yll_corner]
    cell_size = [dem.cell_size,dem.cell_size]

    # Overwrite original mesh if a path isn't provided
    if fileout is None:
        fileout = triplane_path


    # Interpolate no data values on the DEM
    # This is to prevent a noise effect on LaGriT interpolation 
    _dem = deepcopy(dem.dem).astype(float)
    _dem[_dem == dem.no_data_value] = np.nan

    if smooth_boundary:
        _dem = smoothRasterBoundary(_dem,width=4,no_data_value=np.nan)

    ind = nd.distance_transform_edt(np.isnan(_dem), return_distances=False, return_indices=True)
    _dem = _dem[tuple(ind)]

    # Dump DEM data
    _array_out = "_temp_elev_array.dat"
    _dem.tofile(_array_out,sep=' ',format='%.3f')

    # Read DEM as a quad mesh
    tmp_sheet = lg.read_sheetij('surfacemesh',_array_out,dem_dimensions,
                                lower_left_corner,cell_size,flip=flip)

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

    try:
        triplane.interpolate('continuous','z_new',tmp_sheet,'z_elev')
    except Exception as e:
        _err = 'Caught an unknown exception. Most likely, this is related to'+\
        'floating point underflow / overflow.\n\nTry setting `xll_corner` and'+\
        '`yll_corner` to 0.\n\nORIGINAL EXCEPTION: ' + e.msg
        raise Exception(_err)

    triplane.copyatt('z_new','zic')
    triplane.delatt('z_new')
    tmp_sheet.delete()
    triplane.dump(fileout)
    os.remove(_array_out)

def mapFunctionToAttribute(lg:pylagrit.PyLaGriT,mesh:str,
                           number_of_layers:int,layers=None,
                           attribute_name:str=None,operator:str='+',
                           fn=lambda layer: layer*100,outfile:str=None):
    '''

    Maps a function and on operator onto mesh data.
    The function fn should take one parameter: the current layer
    number. The operator will perform on the data and function result.

    In other words, the new attribute data will be a result of:

       attribute_data(layer) = attribute_data [operation] fn(layer)

    For fn = lambda layer: layer*100 and operator '+',

       attribute_data(layer) = attribute_data + layer*100

    meaning that if a selection of attribute data is

        [1,3,5,10,12...]

    then, with operator '+' and lambda layer: layer*100,

        layer 1: [101,103,105,110,112...]
        layer 2: [201,203,205,210,212...]
        layer 3: [301,103,305,310,312...]
        ... 

    '''

    # Sanity check to ensure that function is valid
    try:
        fn(1)
    except TypeError:
        print(fn)
        raise TypeError('Function fn is not a valid function.')

    # Remap operators to LaGriT syntax
    operator = operator.lower()
    if operator in ['+','add']:
        operator = 'add'
    elif operator in ['-','sub','subtract']:
        operator = 'subtract'
    elif operator in ['/','div','divide']:
        operator = 'divide'
    elif operator in ['*','x','mul','multiply']:
        operator = 'multiply'

    stacked_mesh = lg.read(mesh)

    info = stacked_mesh.information()
    v = info['elements'] // number_of_layers

    # Conform scalar to type(list<int>)
    if isinstance(layers,(int,float)):
        layers = [int(layers)]
    elif layers is None:
        layers = list(range(1,number_of_layers+1))

    # Sanity check - ensure that all layers are numbers
    if not all(isinstance(x,int) for x in layers):
        raise ValueError('layers contains non-conforming data')

    if attribute_name is None:
        attribute_name = 'itetclr'

    stacked_mesh.addatt('elttmp',vtype='VINT',length='nelements')
    for layer in layers:

        # Set att to 1 for all layers
        stacked_mesh.setatt('elttmp',1)

        # Set att to 2 *only* for the current layer we're on
        stacked_mesh.setatt('elttmp',2,stride=[v*(layer-1),v*layer])

        # Capture the current layer elements
        current_layer = stacked_mesh.eltset_attribute('elttmp',2)

        # Apply function to layer data
        stacked_mesh.math(operator,attribute_name,value=fn(layer),
                          stride=['eltset','get',current_layer.name],
                          attsrc=attribute_name)

    stacked_mesh.delatt('elttmp')

    if outfile is not None:
        stacked_mesh.dump(outfile)

    return stacked_mesh

def addAttribute(lg:pylagrit.PyLaGriT,data:np.ndarray,stacked_mesh_infile:str,
                 outfile:str,dem_dimensions:list,number_of_layers:int,extent:list,
                 attribute_name:str=None,layers:int=None,dtype=None):
    '''
    Adds an attribute to the stacked mesh, over one or more layers. Default is all.
    Data must be an NxM matrix - it does not necessarily have to be the same size at the DEM,
    but is recommended as it will be streched to span the domain of it.

    attribute_name will be the element-based attribute the data is written into.
    The default is 'material ID', but can be changed to any
    [a-z][A-Z][0-9] string (outside of reserved LaGriT keywords).

    :param data: NxM matrix of data to be written as matrix
    :type data: np.ndarray
    :param layers: Layer IDs to write attributes to. Defaults to 'all'.
    :type layers: list<int>
    :param attribute_name: Attribute name to store data in. Defaults to material ID
    :type attribute_name: str
    :param outfile: Filename to write mesh to
    :type outfile: str
    :param dtype: Data type of elements in data. Defaults to float
    :type dtype: type
    :returns: Prism mesh with new attribute
    '''

    if attribute_name is not None:
        cfg.log.info('Adding attribute '+attribute_name+' to mesh')
    else:
        cfg.log.info('Adding attribute MATERIAL_ID to mesh')

    if dtype is None:
        dtype = type(data[0,0])

    cfg.log.debug('Raster data type: ' + str(dtype))
    
    dtype = 'VINT' if dtype in [int,np.int64,np.int] else 'VDOUBLE'
    data = imresize(data,(dem_dimensions[1],dem_dimensions[0]),interp='nearest')

    # Dump attribute data
    _array_out = "_temp_attrib_array.dat"
    data = np.flip(data,1).flatten(order='C')[::-1]

    # TODO: Streamline this
    with open(_array_out,'w') as f:
        for i in range(dem_dimensions[1]*dem_dimensions[0]):
            f.write('%d\n' % (data[i]))

    cfg.log.debug('Wrote raster data to ' + _array_out)

    # Adjusted matrix dimensions for LaGriT
    NX, NY = (dem_dimensions[0] + 1), (dem_dimensions[1] + 1)

    # Below, we are:
    #   1. Reading in an attribute 
    #   2. Creating a quad mesh of the same (rows,cols) as the data
    #   3. Copying that data to an element-based attribute
    #   4. Extruding the quad mesh such that the z-axis spans the stacked mesh
    #   5. Interpolating the data onto the stacked mesh

    mo_pts = lg.read_att(_array_out,'imt',operation=(1,0,0))
    mo_quad = lg.create(elem_type='quad')
    mo_quad.quadxy([NX,NY,1],[[extent[0],extent[2],0.],
                              [extent[1],extent[2],0.],
                              [extent[1],extent[3],0.],
                              [extent[0],extent[3],0.]])

    cfg.log.debug('Raster dimensions: [' + str(NX) + ',' + str(NY) + ',1]')

    mo_quad.rzbrick([NX,NY,1],stride=(1,0,0),connect=True)
    mo_quad.addatt('id_strat',vtype=dtype,length='nelements')

    mo_quad.copyatt('imt',mo_src=mo_pts,attname_sink='id_strat')
    mo_extrude = mo_quad.extrude(10000.)
    mo_extrude.addatt('id_strat',vtype=dtype,length='nelements')
    mo_extrude.copyatt('id_strat',mo_src=mo_quad,attname_sink='id_strat')

    stacked_mesh = lg.read(stacked_mesh_infile)

    # Default to material_id - 'itetclr'
    if attribute_name is not None:
        stacked_mesh.addatt(attribute_name,length='nelements')
    else:
        attribute_name = 'itetclr'

    info = stacked_mesh.information()
    v = info['elements'] // number_of_layers

    if layers is None:
        cfg.log.info('Interpolating to full mesh')
        stacked_mesh.interpolate('map',attribute_name,mo_extrude,'id_strat',stride=[1,0,0])
    else:
        # Conform scalar to type(list<int>)
        if isinstance(layers,(int,float)):
            layers = [int(layers)]

        # Sanity check - ensure that all layers are numbers
        if not all(isinstance(x,int) for x in layers):
            raise ValueError('layers contains non-conforming data')

        stacked_mesh.addatt('elttmp',vtype='VINT',length='nelements')
        for layer in layers:
            cfg.log.info('Interpolating to layer %d' % layer)

            # Set att to 1 for all layers
            stacked_mesh.setatt('elttmp',1)

            # Set att to 2 *only* for the current layer we're on
            stacked_mesh.setatt('elttmp',2,stride=[v*(layer-1),v*layer])

            # Capture the current layer elements
            current_layer = stacked_mesh.eltset_attribute('elttmp',2)

            # Iterpolate matrix onto layer
            stacked_mesh.interpolate('map',attribute_name,mo_extrude,'id_strat',
                                     stride=['eltset','get',current_layer.name])

        stacked_mesh.delatt('elttmp')


    # Remove temporary meshes
    for mesh in [mo_pts,mo_extrude,mo_quad]:
        cfg.log.debug('Deleting %s' % mesh.name)
        mesh.delete()

    os.remove(_array_out)

    if outfile is not None:
        cfg.log.info('Writing mesh to %s' % outfile)
        stacked_mesh.dump(outfile)

    return stacked_mesh

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

    if layers[0] != 0.:
        layers = [0.0] + layers
        matids = [0] + matids
        assert len(layers) == len(matids)

    stack_files = ['layer%d.inp' % (len(layers)-i) for i in range(len(layers))]
    if nlayers is None:
        nlayers=['']*(len(stack_files)-1)

    cfg.log.info('Stacking %d layers' % nlayers)
    
    motmp_top = lg.read(infile)

    for (i,offset) in enumerate(layers):
        motmp_top.math('sub','zic',value=offset)
        motmp_top.dump('layer%d.inp' % (i+1))

    motmp_top.delete()
    stack = lg.create()
    stack.stack_layers(stack_files,flip_opt=True,nlayers=nlayers,matids=matids,xy_subset=xy_subset)
    #stack.dump('tmp_layers.inp')

    cmo_prism = stack.stack_fill(name='CMO_PRISM')
    cmo_prism.resetpts_itp()

    cmo_prism.addatt('cell_vol',keyword='volume')
    cmo_prism.dump(outfile)

    if not cfg.DEBUG_MODE:
        cleanup(stack_files)

    return cmo_prism


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


def generateFaceSetsNaive(lg:pylagrit.PyLaGriT,stacked_mesh,outfile:str):
    '''

    Generates an Exodus mesh with three facesets:

    - Top, bottom, and sides

    This can be done easily without any further input from the user - i.e.
    explicitly delimiting what determines a 'top' and a 'side'.

    '''

    _all_facesets = ['fs1_bottom.avs','fs2_top.avs','fs3_sides_all.avs']

    if not isinstance(stacked_mesh,pylagrit.MO):
        raise ValueError('stacked_mesh must be a PyLaGriT mesh object')

    tmp_infile = 'infile_tmp_get_facesets3.mlgi'

    with open(tmp_infile,'w') as f:
        f.write(Infiles.get_facesets3)

    lg.sendline('define CMO_PRISM %s' % stacked_mesh.name)
    lg.sendline('infile %s' % tmp_infile)

    cmd = 'dump/exo/'+outfile+'/CMO_PRISM///facesets &\n'
    cmd += ' &\n'.join(_all_facesets)

    lg.sendline(cmd)

    _all_facesets.extend(tmp_infile)
    cleanup(_all_facesets)
   

def buildUniformTriplane(lg:pylagrit.PyLaGriT,boundary:np.ndarray,outfile:str,
                         counterclockwise:bool=False,connectivity:bool=None,
                         min_edge:float=16.):
    '''

    Generate a uniform triplane mesh from a boundary polygon.

    The algorithm works by triangulating only the boundary, and
    then iterating through each triangle, breaking edges in half
    where they exceed the given edge length.

    Consequently, this means that the final length scale will have
    a minimum edge length of min_edge / 2, and a maximum edge
    length of min_edge.

    :param lg: Instance of PyLaGriT
    :type lg: pylagrit.PyLaGriT
    :param boundary: Boundary nodes with, at least, x and y columns
    :type boundary: np.ndarray
    :param outfile: outfile to save triangulation to (set to None to skip)
    :type outfile: str
    :param min_edge: approximate minimum triangle edge length
    :type min_edge: float
    :param counterclockwise: flag to indicate connectivity ordering
    :type counterclockwise: bool
    :param connectivity: optional array declaring node connectivity
    :type connectivity: np.ndarray
    '''

    # Generate the boundary polygon
    if connectivity is None:
        connectivity = generateLineConnectivity(boundary)

    _writeLineAVS(boundary,"poly_1.inp",connections=connectivity)

    # Compute length scales to break triangles down into
    # See below for a more in-depth explanation
    length_scales = [min_edge*i for i in [1,2,4,8,16,32,64]][::-1]

    mo_tmp = lg.read("poly_1.inp")

    motri = lg.create(elem_type='triplane')
    motri.setatt('ipolydat','no')
    lg.sendline('copypts / %s / %s' % (motri.name,mo_tmp.name))
    motri.setatt('imt',1)
    mo_tmp.delete()

    # Triangulate the boundary
    motri.select()
    if counterclockwise:
        motri.triangulate(order='counterclockwise')
    else:
        motri.triangulate(order='clockwise')

    # Set material ID to 1        
    motri.setatt('itetclr',1)
    motri.setatt('motri',1)
    motri.resetpts_itp()

    lg.sendline('cmo/copy/mo/%s' % motri.name)

    # Move through each length scale, breaking down edges less than the value 'ln'
    # Eventually this converges on triangles with edges in the range [0.5*min_edge,min_edge]
    motri.select()
    for ln in length_scales:
        motri.refine(refine_option='rivara',refine_type='edge',values=[ln],inclusive_flag='inclusive')

        for _ in range(3):
            motri.recon(0)
            motri.smooth()
        motri.rmpoint_compress(resetpts_itp=False)

    # Smooth and reconnect the triangulation
    for _ in range(6):
        motri.smooth()
        motri.recon(0)
        motri.rmpoint_compress(resetpts_itp=True)

    motri.rmpoint_compress(resetpts_itp=False)
    motri.recon(1); motri.smooth(); motri.recon(0); motri.recon(1)

    #pgood = motri.pset_attribute('aratio',0.8,comparison='gt')
    #prest = motri.pset_not([pgood])

    motri.setatt('ipolydat','yes')
    #motri.addatt('vorvol',keyword='vor_volume')

    if outfile:
        motri.dump(outfile)

    return motri


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

    # Define initial parameters
    counterclockwise = False
    h_eps = h*10**-7
    PARAM_A = slope
    PARAM_B = h*(1-slope*refine_dist)
    PARAM_A2 = 0.5*slope
    PARAM_B2 = h*(1 - 0.5*slope*refine_dist)

    if connectivity is None:
        connectivity = generateLineConnectivity(boundary)

    _writeLineAVS(boundary,"poly_1.inp",connections=connectivity)
    _writeLineAVS(feature,"intersections_1.inp")

    # Write massage macros
    with open('user_function.lgi','w') as f:
        f.write(Infiles.distance_field)

    with open('user_function2.lgi','w') as f:
        f.write(Infiles.distance_field_2)

    # Read boundary and feature
    mo_poly_work = lg.read('poly_1.inp',name='mo_poly_work')
    mo_line_work = lg.read('intersections_1.inp',name='mo_line_work')

    # Triangulate Fracture without point addition
    mo_pts = mo_poly_work.copypts(elem_type='triplane')
    mo_pts.select()

    if counterclockwise:
        mo_pts.triangulate(order='counterclockwise')
    else:
        mo_pts.triangulate(order='clockwise')

    # Set element attributes for later use
    mo_pts.setatt('imt',1,stride=(1,0,0))
    mo_pts.setatt('itetclr',1,stride=(1,0,0))
    mo_pts.resetpts_itp()
    
    mo_pts.select()

    # Refine at increasingly smaller distances, approaching h
    for (i,ln) in enumerate([8,16,32,64][::-1]):
        h_scale = ln*h
        perturb = h_scale*0.05

        mo_pts.massage(h_scale,h_eps,h_eps)
        
        # Do a bit of smoothing on the first pass
        if (i == 0):
            for _ in range(3):
                mo_pts.recon(0)
                mo_pts.smooth()
            mo_pts.recon(0)

        mo_pts.resetpts_itp()

        #p_move = mo_pts.pset_attribute('itp',0,comparison='eq',stride=(1,0,0),name='p_move')
        #p_move.perturb(perturb,perturb.format(ln),0.0)

        # Smooth and reconnect
        for _ in range(6):
            mo_pts.recon(0)
            mo_pts.smooth()
        mo_pts.recon(0)

    # Define attributes to be used for massage functions
    mo_pts.addatt('x_four',vtype='vdouble',rank='scalar',length='nnodes')
    mo_pts.addatt('fac_n',vtype='vdouble',rank='scalar',length='nnodes')

    # Define internal variables for user_functions
    lg.define(mo_pts=mo_pts.name,PARAM_A=PARAM_A,PARAM_A2=PARAM_A2,PARAM_B=PARAM_B,PARAM_B2=PARAM_B2)

    # Run massage2
    mo_pts.massage2('user_function2.lgi',0.8*h,'fac_n',0.00001,0.00001,stride=(1,0,0),strictmergelength=True)
    lg.sendline('assign///maxiter_sm/1')

    for _ in range(3):
        mo_pts.smooth()
        mo_pts.recon(0)

    # Massage once more, cleanup, and return
    lg.sendline('assign///maxiter_sm/10')
    mo_pts.massage2('user_function.lgi',0.8*h,'fac_n',0.00001,0.00001,stride=(1,0,0),strictmergelength=True)
    mo_pts.delatt('rf_field_name')

    mo_line_work.delete()
    mo_poly_work.delete()

    if outfile is not None:
        mo_pts.dump(outfile)

    cleanup(['user_function.lgi','user_function2.lgi'])

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

    # TODO: Fix this.
    # Band-aid approach! Causes segfault if too many
    # meshes are present in memory.
    for mesh in lg.mo:
        lg.sendline('cmo/delete/%s' % mesh)

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
    cleanup(_cleanup)

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
