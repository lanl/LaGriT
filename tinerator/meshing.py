import numpy as np
import tinerator.utilities as util
import tinerator.config as cfg
from tinerator.lg_infiles import Infiles
from copy import deepcopy
from scipy import ndimage as nd

def _intrp_elevation_to_surface(lg,
                                dem_object,
                                triplane,
                                flip:str='y',
                                fileout=None,
                                smooth_boundary:bool=False,
                                outfile:str=None):
    '''

    Given a triplane mesh and a tinerator.DEM instance, this function will 
    map elevation data from the array to the triplane mesh.

    '''

    # Generate sheet metadata
    dem_dimensions = [dem_object.ncols,dem_object.nrows]
    lower_left_corner = [dem_object.xll_corner,dem_object.yll_corner,0.]
    cell_size = [dem_object.cell_size,dem_object.cell_size]

    # Interpolate no data values on the DEM
    # This is to prevent a noise effect on LaGriT interpolation 
    _dem = deepcopy(dem_object.dem).astype(float)
    _dem[_dem == dem_object.no_data_value] = np.nan

    if smooth_boundary:
        _dem = util._smooth_raster_boundary(_dem,width=4,no_data_value=np.nan)

    ind = nd.distance_transform_edt(np.isnan(_dem),return_distances=False,return_indices=True)
    _dem = _dem[tuple(ind)]

    cfg.log.info('Parsing elevation data')

    # Dump DEM data
    _array_out = "_temp_elev_array.dat"
    _dem.tofile(_array_out,sep=' ',format='%.3f')

    # Read DEM as a quad mesh at 0., 0.
    tmp_sheet = lg.read_sheetij('surfacemesh',
                                _array_out,
                                dem_dimensions,
                                (0.,0.),
                                cell_size,
                                flip=flip)

    # Move mesh to proper position
    # TODO: Figure out why this is necessary. Overflow problem in readsheet?
    tmp_sheet.trans((0,0,0),lower_left_corner)

    cfg.log.info('Applying elevation to surface mesh')

    # Remove no_data_value elements
    comp = 'le' if dem_object.no_data_value <= 0. else 'ge'
    dud_value = dem_object.no_data_value - np.sign(dem_object.no_data_value)
    pduds = tmp_sheet.pset_attribute('zic',dud_value,comparison=comp,name='pduds')
    eduds = pduds.eltset(membership='inclusive',name='eduds')
    tmp_sheet.rmpoint_eltset(eduds,compress=True,resetpts_itp=True)

    # Copy elevation to new variable (priming for interpolation)
    tmp_sheet.addatt('z_elev')
    tmp_sheet.copyatt('zic','z_elev',tmp_sheet)
    tmp_sheet.setatt('zic',0.)

    # Interpolate z-values onto triplane
    #triplane = lg.read(triplane_path,name='triplanemesh')
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

    if outfile is not None:
        cfg.log.debug('Writing mesh to %s' % outfile)
        triplane.dump(outfile)

    if not cfg.DEBUG:
        os.remove(_array_out)


def _uniform_surface_mesh(lg,
                          boundary:np.ndarray,
                          min_edge:float,
                          outfile:str=None,
                          counterclockwise:bool=False,
                          connectivity:bool=None):
    '''
    Generate a uniform triplane mesh from a boundary polygon.
    This function should, in the standard workflow, be called
    exclusively from `tinerator.DEM.build_uniform_triplane`.

    The algorithm works by triangulating only the boundary, and
    then iterating through each triangle, breaking edges in half
    where they exceed the given edge length.

    Consequently, this means that the final length scale will have
    a minimum edge length of min_edge / 2, and a maximum edge
    length of min_edge.

    # Arguments
    lg (pylagrit.PyLaGriT): Instance of PyLaGriT
    boundary (np.ndarray): Boundary nodes with, at least, x and y columns
    min_edge (float): approximate minimum triangle edge length

    # Optional Arguments
    outfile (str): outfile to save triangulation to (set to None to skip)
    counterclockwise (bool): flag to indicate connectivity ordering
    connectivity (np.ndarray): optional array declaring boundary node connectivity

    # Returns
    PyLaGriT mesh object
    '''

    cfg.log.info('[ UNIFORM TRIPLANE GENERATION ]')

    # Generate the boundary polygon
    if connectivity is None:
        connectivity = util._line_connectivity(boundary)

    cfg.log.debug('Writing boundary to poly_1.inp')
    util._write_line(boundary,"poly_1.inp",connections=connectivity)

    # Compute length scales to break triangles down into
    # See below for a more in-depth explanation
    length_scales = [min_edge*i for i in [1,2,4,8,16,32,64]][::-1]

    cfg.log.info('Preparing boundary')

    mo_tmp = lg.read("poly_1.inp")

    motri = lg.create(elem_type='triplane')
    motri.setatt('ipolydat','no')
    lg.sendline('copypts / %s / %s' % (motri.name,mo_tmp.name))
    motri.setatt('imt',1)
    mo_tmp.delete()

    cfg.log.info('First pass triangulation')

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
        cfg.log.info('Refining at length %s' % str(ln))

        motri.refine(refine_option='rivara',
                     refine_type='edge',
                     values=[ln],
                     inclusive_flag='inclusive')

        for _ in range(3):
            motri.recon(0)
            motri.smooth()
        motri.rmpoint_compress(resetpts_itp=False)

    cfg.log.info('Smoothing mesh')

    # Smooth and reconnect the triangulation
    for _ in range(6):
        motri.smooth()
        motri.recon(0)
        motri.rmpoint_compress(resetpts_itp=True)

    motri.rmpoint_compress(resetpts_itp=False)
    motri.recon(1); motri.smooth(); motri.recon(0); motri.recon(1)

    motri.setatt('ipolydat','yes')

    if outfile:
        motri.dump(outfile)

    return motri



def _refined_surface_mesh(lg,
                          boundary:np.ndarray,
                          feature:np.ndarray,
                          h:float,
                          connectivity:bool=None,
                          delta:float=0.75,
                          slope:float=2.,
                          refine_dist:float=0.5,
                          outfile:str=None):
    '''
    Constructs a triplane mesh refined around a feature using LaGriT
    as a backend.
    
    Requires an Nx2 np.ndarray as a boundary input, and an Nx2 np.ndarray
    as a feature input.
    '''

    # Define initial parameters
    counterclockwise = False
    h_eps = h*10**-7
    PARAM_A = slope
    PARAM_B = h*(1-slope*refine_dist)
    PARAM_A2 = 0.5*slope
    PARAM_B2 = h*(1 - 0.5*slope*refine_dist)

    if connectivity is None:
        connectivity = util._line_connectivity(boundary)

    cfg.log.debug('Writing boundary to poly_1.inp')
    util._write_line(boundary,"poly_1.inp",connections=connectivity)

    cfg.log.debug('Writing feature to intersections_1.inp')
    util._write_line(feature,"intersections_1.inp")

    # Write massage macros
    with open('user_function.lgi','w') as f:
        f.write(Infiles.distance_field)

    with open('user_function2.lgi','w') as f:
        f.write(Infiles.distance_field_2)

    cfg.log.info('Preparing feature and boundary')

    # Read boundary and feature
    mo_poly_work = lg.read('poly_1.inp',name='mo_poly_work')
    mo_line_work = lg.read('intersections_1.inp',name='mo_line_work')

    # Triangulate Fracture without point addition
    mo_pts = mo_poly_work.copypts(elem_type='triplane')
    mo_pts.select()

    cfg.log.info('First pass triangulation')

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
        cfg.log.info('Refining at length %s' % str(ln))

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
    mo_pts.addatt('fac_n', vtype='vdouble',rank='scalar',length='nnodes')

    # Define internal variables for user_functions
    lg.define(mo_pts=mo_pts.name,
              PARAM_A=PARAM_A,
              PARAM_A2=PARAM_A2,
              PARAM_B=PARAM_B,
              PARAM_B2=PARAM_B2)

    cfg.log.info('Smoothing mesh (1/2)')

    # Run massage2
    mo_pts.massage2('user_function2.lgi',
                     0.8*h,
                     'fac_n',
                     0.00001,
                     0.00001,
                     stride=(1,0,0),
                     strictmergelength=True)

    lg.sendline('assign///maxiter_sm/1')

    for _ in range(3):
        mo_pts.smooth()
        mo_pts.recon(0)

    cfg.log.info('Smoothing mesh (2/2)')

    # Massage once more, cleanup, and return
    lg.sendline('assign///maxiter_sm/10')
    mo_pts.massage2('user_function.lgi',
                     0.8*h,
                     'fac_n',
                     0.00001,
                     0.00001,
                     stride=(1,0,0),
                     strictmergelength=True)

    mo_pts.delatt('rf_field_name')

    mo_line_work.delete()
    mo_poly_work.delete()

    if outfile is not None:
        mo_pts.dump(outfile)

    util.cleanup(['user_function.lgi','user_function2.lgi'])

    return mo_pts


def _stacked_mesh(lg,
                  surface_mesh,
                  outfile:str,
                  layers:list,
                  matids=None,
                  xy_subset=None,
                  nlayers=None):
    '''
    Created a stacked mesh with layer thickness described by the variable
    layers.
    '''

    if layers[0] != 0.:
        layers = [0.0] + layers
        matids = [0] + matids
        assert len(layers) == len(matids)

    stack_files = ['layer%d.inp' % (len(layers)-i) for i in range(len(layers))]
    if nlayers is None:
        nlayers=['']*(len(stack_files)-1)

    cfg.log.info('Stacking %d layers' % len(nlayers))
    
    motmp_top = surface_mesh.copy()#lg.read(infile)

    for (i,offset) in enumerate(layers):
        cfg.log.info('Adding layer %d with thickness %s' % (i+1,str(offset)))
        motmp_top.math('sub','zic',value=offset)
        motmp_top.dump('layer%d.inp' % (i+1))

    lg.sendline('cmo/status/brief')

    motmp_top.delete()

    cfg.log.info('Adding volume to layers')
    stack = lg.create()
    stack.stack_layers(stack_files,
                       flip_opt=True,
                       nlayers=nlayers,
                       matids=matids,
                       xy_subset=xy_subset)

    cmo_prism = stack.stack_fill()
    cmo_prism.resetpts_itp()

    stack.delete()

    if outfile is not None:
        cmo_prism.dump(outfile)

    if not cfg.DEBUG_MODE:
        util.cleanup(stack_files)

    return cmo_prism

def __generateSingleColumnPrism(lg:pylagrit.PyLaGriT,infile:str,outfile:str,layers:list,
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


