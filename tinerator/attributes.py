import numpy as np
from copy import deepcopy
from scipy import interpolate
from scipy import ndimage as nd
from scipy.misc import imresize

import tinerator.utilities as util
import tinerator.config as cfg

def _remove_attribute(mesh,attribute_name):
    mesh.delatt(attribute_name)

def mapFunctionToAttribute(lg,mesh:str,
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

def _add_attribute(lg,
                   data:np.ndarray,
                   stacked_mesh,
                   dem_dimensions:list,
                   number_of_layers:int,
                   extent:list,
                   attribute_name:str=None,
                   outfile:str=None,
                   layers:int=None,
                   dtype=None):
    '''
    Adds an attribute to the stacked mesh, over one or more layers. Default is all.
    Data must be an NxM matrix - it does not necessarily have to be the same size at the DEM,
    but is recommended as it will be streched to span the domain of it.

    attribute_name will be the element-based attribute the data is written into.
    The default is 'material ID', but can be changed to any
    [a-z][A-Z][0-9] string (outside of reserved LaGriT keywords).
    '''

    if dtype is None:
        dtype = type(data[0,0])

    cfg.log.debug('Raster data type: ' + str(dtype))
    
    dtype = 'VINT' if dtype in [int,np.int64,np.int] else 'VDOUBLE'

    if attribute_name is not None:
        cfg.log.info('Adding attribute \"'+attribute_name+'\" to mesh (type: %s)' % dtype)
    else:
        cfg.log.info('Adding attribute \"MATERIAL_ID\" to mesh (type: %s)' % dtype)

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

    stacked_mesh = stacked_mesh.copy()

    # Default to material_id - 'itetclr'
    if attribute_name is not None:
        stacked_mesh.addatt(attribute_name,length='nelements')
    else:
        attribute_name = 'itetclr'

    info = stacked_mesh.information()
    v = info['elements'] // number_of_layers

    if layers is None:
        cfg.log.info('Interpolating attribute to full mesh')
        stacked_mesh.interpolate('map',attribute_name,mo_extrude,'id_strat',stride=[1,0,0])
    else:
        # Conform scalar to type(list<int>)
        if isinstance(layers,(int,float)):
            layers = [int(layers)]

        # Sanity check - ensure that all layers are numbers
        if not all(isinstance(x,int) for x in layers):
            raise ValueError('layers contains non-conforming data')

        # Should vtype be float?
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

    util.cleanup([_array_out])

    if outfile is not None:
        cfg.log.info('Writing mesh to %s' % outfile)
        stacked_mesh.dump(outfile)

    return stacked_mesh