import os
import richdem as rd
import numpy as np
from pylagrit import PyLaGriT
from copy import deepcopy
import logging
import warnings
import tinerator.config as cfg
import tinerator.utilities as util
import tinerator.boundary as boundary
import tinerator.plot as tinplot
import tinerator.watershed_deliniation as delin
import tinerator.meshing as mesh
import tinerator.attributes as attrib
import tinerator.future as future

class DEM():
    '''
    This is the 'main' class of TINerator, and stores all DEM and GIS data
    related to a particular project.

    # Attributes
    filepath (str): Filepath to DEM raster
    lagrit_exe (str,None): Optional filepath to LaGriT binary. If PyLaGriT is
    configured correctly, this should be unnecessary.
    '''
    def __init__(self,filepath:str,lagrit_exe:str=None):
        self.dem = rd.LoadGDAL(filepath)
        self.lg = PyLaGriT(lagrit_exe=lagrit_exe,verbose=False)
        self.distance_field = None
        self.triangles = None
        self.feature = None
        self.boundary = None
        self.points = None
        self.stacked_mesh = None
        self.accumulation_matrix = None

        self.no_data_value = self.dem.no_data
        self.ncols = np.shape(self.dem)[1]
        self.nrows = np.shape(self.dem)[0]
        self.cell_size = (self.dem.geotransform[1], self.dem.geotransform[5])
        self.xll_corner = self.dem.geotransform[0]
        self.yll_corner = self.dem.geotransform[3]
        self.mask = self.dem == self.dem.no_data if self.no_data_value in self.dem else None

        self._surface_mesh = None
        self._stacked_mesh = None

        # Mesh characteristics
        self.number_of_layers = 0
        self.metadata = {
            'filename': filepath
        }

        self.__replace_infs_with_nans()

    def __repr__(self):
        # DEM information
        display = "Name: {0}\nPath: {1}\nCRS: {2}\nExtent: {3}\n"\
                  "Dimensions: {4}\nOrigin: {5}\nCell size: {6}"\
                  "Maximum elevation: {7}\nMinimum elevation: {8}"\
                    .format(None,None,None,self.extent,
                            'X: %d, Y: %d' % (self.ncols,self.nrows),
                            self.origin,self.cell_size,self.min_z,
                            self.max_z)
        return display

    def __del__(self):
        if not cfg.KEEP_LAGRIT_LOGS:
            util.cleanup(['logx3dgen','outx3dgen',
                          'lagrit.log','lagrit.out'])

    @property
    def origin(self):
        return (self.xll_corner,self.yll_corner)

    @property
    def extent(self):
        return self.getBoundingBox()

    @property
    def ratio(self):
        return self.nrows / self.ncols

    @property
    def min_z(self):
        return np.nanmin(self.dem)

    @property
    def max_z(self):
        return np.nanmax(self.dem)

    @property
    def top_layer(self):
        return 0

    @property
    def bottom_layer(self):
        return 0

    # Replace +/-float('inf') with np.nans which are more managable
    def __replace_infs_with_nans(self):
        for _invalid in [float('inf'),float('-inf')]:
            self.dem[self.dem == _invalid] = cfg.GLOBAL_NDV
            if self.no_data_value == _invalid:
                cfg.log.debug('Found %s ' % _invalid)
                self.change_ndv(GLOBAL_NDV)


    # Resets the DEM mask
    def __reset_mask(self):
        if self.no_data_value in self.dem:
            self.mask = self.dem == self.dem.no_data
        else:
            self.mask = None

    def change_ndv(self,ndv:float):
        '''
        Changes `no_data_value` of the DEM object.
        This should be used instead of resetting `dem.no_data_value`
        manually, as it makes deeper changes.

        # Example
        ```python
        dem.change_ndv(-9999.)
        print(dem.no_data_value) # -9999.0
        ```

        # Arguments
        ndv (float): New `no_data_value`
        '''

        self.no_data_value = ndv
        self.dem.no_data = ndv
        self.dem.wrap()
        self.__reset_mask()

    def set_to_ndv(self,value:float):
        '''
        Changes all occurances of `value` in the DEM data 
        to `no_data_value`.

        # Example
        ```
        dem.set_to_ndv(dem.dem[0][0])
        print(dem.dem == dem.no_data_value) # True
        ```

        # Arguments
        value (float): raster value to replace
        '''
        self.dem[self.dem == value] = self.no_data_value
        self.__reset_mask()

    def set_verbosity(self,verb_level:int,filename:str=None):
        '''
        Set the verbosity level of printed output.

        * `NOTHING` : Nothing (except warnings and related)
        * `INFO` : Log output
        * `FULL` : Log output and LaGriT output
        * `DEBUG` : Log output, LaGriT output, and turns on debug mode

        Each of these verbosity levels are variables in `tinerator.config`.

        # Example

        ```python
        dem.set_verbosity(tinerator.config.FULL)
        ```

        # Arguments
        verb_level (int): verbosity level 
        filename (str): file to write log output to
        '''

        cfg.DEBUG_MODE = False
        if verb_level == cfg.NOTHING:
            self.lg.verbose = False
            cfg.log.setLevel(logging.NOTSET)
        elif verb_level == cfg.DEBUG:
            self.lg.verbose = True
            cfg.log.setLevel(logging.DEBUG)
            cfg.DEBUG_MODE = True
        elif verb_level == cfg.INFO:
            self.lg.verbose = False
            cfg.log.setLevel(logging.INFO)
        elif verb_level == cfg.FULL:
            self.lg.verbose = False
            cfg.log.setLevel(logging.INFO)

        if filename is not None:
            cfg.log.warning('Dynamic file logging isn\'t enabled yet')

    def fill_depressions(self,
                         fill_depressions:bool=True,
                         fill_flats:bool=True):
        '''
        Fills flats and depressions on DEM. On meshes intended to be high-
        resolution, leaving flats and depressions untouched may cause solver
        issues. This method should be called before generating a triplane.

        # Arguments
        fill_depressions (bool): fill pits and depressions on DEM
        fill_flats (bool): fill flats on DEM

        # Example
        ```python
        dem1 = tin.load.from_file("example.asc")
        dem2 = tin.load.from_file("example.asc")

        dem1.fill_depressions()

        plt.imshow(dem1.dem - dem2.dem)
        plt.show()
        ```

        This example shows the different in topology between
        a post-processed and unprocessed DEM.
        '''

        if fill_depressions:
            cfg.log.info('Filling depressions')
            rd.FillDepressions(self.dem,epsilon=False,in_place=True)

        if fill_flats:
            cfg.log.info('Filling flats')
            rd.ResolveFlats(self.dem,in_place=True)

    def smooth_between(self, shapefile_path: str, with_radius=25.):
        '''
        Given a shapefile containing one or more line objects,
        this will smooth the DEM across the line(s) at a radius of
        `with_radius`.

        # Arguments
        shapefile_path (str): path to shapefile
        '''
        lines = util.get_geometry(shapefile_path)

        if all([x['type'].lower().strip() != 'linestring' for x in lines]):
            raise ValueError(f"Requested LineString geometry. Got: {lines}")

        for c in [lines[0]]:
            cc = c['coordinates']
            print('1>>', cc)
            cc = np.flip(
                util.ProjectedVectorToXY(
                    cc, 
                    self.xll_corner, 
                    self.yll_corner, 
                    self.cell_size, 
                    self.nrows
                ),
                axis = 1,
            )
            print('2>>', cc)
            future.smooth_between(
                self.dem.data, 
                with_radius, 
                cc[1], 
                cc[0],
                inplace=True
            )

    def watershed_delineation(self,
                              threshold:float,
                              method:str='D8',
                              exponent:float=None,
                              interactive:bool=False):
        '''
        Performs watershed delineation on a DEM and returns a set of points
        corresponding to the feature.

        Available methods are:

        * D8
        * D4
        * Rho8
        * Rho4
        * Dinf
        * Quinn
        * Holmgren
        * Freeman

        # Arguments
        threshold (float): threshold for determining feature from noise
        method (str): Flow calculation method
        interactive (bool): if True and function is called within a
                            Jupyter notebook, then function params
                            can be controlled with sliders
        
        # Returns
        Polyline of feature as ordered (x,y) pairs
        '''

        if threshold is None:
            threshold = 1.0

        if interactive:
            if cfg.IN_NOTEBOOK:
                return self.__watershed_delin_as_interactive(threshold,method)
            else:
                cfg.log.warn('Cannot init Jupyter notebook functionality')

        if (exponent is not None) and (method not in ['Holmgren', 'Freeman']):
            cfg.log.warn("Ignoring exponent: exponent is only valid " + \
                         "for Holmgren/Freeman methods")
        
        self.accumulation_matrix = delin.watershedDelineation(self.dem,
                                                              method=method,
                                                              exponent=exponent)

        if threshold is None:
            _thresh = np.unique(accumulation)
            threshold = _thresh[int(0.1*len(_thresh))]

        self.feature = delin.getFeatureTrace(self.accumulation_matrix,feature_threshold=threshold).astype(float)

        if np.size(self.feature) == 0:
            raise ValueError("Feature trace is empty. " + \
                             "Try setting a lower threshold or " + \
                             "using a different method.")

        self.feature = util.xyVectorToProjection(self.feature,
                                                 self.cell_size,
                                                 self.xll_corner,
                                                 self.yll_corner,
                                                 self.nrows)


        return self.feature


    def set_river_network_from_raster(self,raster:np.ndarray,threshold:float) -> None:
        '''
        Sets the refinement feature from a polyline defined by the
        `x` and `y` vectors. 

        # Arguments
        x (list): x-coord list of river network nodes
        y (list): y-coord list of river network nodes

        # Returns
        Nothing.
        '''
        
        self.feature = delin.getFeatureTrace(raster,
                                             feature_threshold=threshold)

        if np.size(self.feature) == 0:
            raise ValueError("Feature trace is empty. " + \
                             "Try setting a lower threshold or " + \
                             "using a different method.")

        self.feature = util.xyVectorToProjection(self.feature,
                                                 self.cell_size,
                                                 self.xll_corner,
                                                 self.yll_corner,
                                                 self.nrows)


        return self.feature

    def import_refinement_features(
        self, shp_paths: list
    ) -> None:
        """
        Imports one or more shapefiles for creating a refined surface mesh.

        # Arguments
        shp_paths (list<str>): list of paths to shapefiles
        """
    
        if isinstance(shp_paths, str):
            shp_paths = [shp_paths]
    
        master_arr = None
    
        for shp_path in shp_paths:
            if not os.path.exists(shp_path):
                raise FileNotFoundError(
                    f'Shapefile doesn\'t exist at path "{shp_path}"'
                )
    
            arr = util.rasterize_shapefile_like(shp_path, self.metadata['filename'])
    
            if master_arr is None:
                master_arr = arr
            else:
                master_arr[arr == True] = True
    
        self.set_river_network_from_raster(master_arr, 0.5)

    def import_river_network(self,shp_filepath:str) -> None:
        '''
        Loads a river network shapefile as a feature to be used
        for mesh refinement. Use instead of `self.watershed_delineation`.

        # Example
        ```python
        my_dem.import_river_network('data/River/river.shp')
        my_dem.build_refined_triplane()
        ```

        # Arguments
        shp_filepath (str): Path to `.shp` file.
        crs (str): Proj4 CRS to re-project into (optional).

        # Returns
        Nothing.
        '''

        warnings.warn(
            'This method is deprecated. Please use `self.import_refinement_features`.', 
            DeprecationWarning, 
            stacklevel=1
        )
        self.import_refinement_features(shp_filepath)

    def __watershed_delin_as_interactive(self,threshold,method):
        '''
        Driver for handling interactive watershed delineation.
        '''

        try:
            from ipywidgets import interact,interactive,fixed,interact_manual
            from ipywidgets import FloatSlider
            import ipywidgets as widgets
        except NameError:
            cfg.IN_NOTEBOOK = False
            raise NameError('Could not init Jupyter notebook functionality')

        def __fnc_driver(threshold,method):
            self.watershed_delineation(threshold,method=method,interactive=False)
            tinplot.plot_feature(self)

        accum = delin.watershedDelineation(self.dem,method=method)
        max_accum = np.nanmax(accum)
        min_accum = min(1,np.nanmin(accum))
        step = (max_accum - min_accum) / 100.

        threshold = max(min_accum,min(max_accum,threshold))

        interact_manual(__fnc_driver,
                        threshold=widgets.FloatSlider(min=min_accum,
                                                      max=max_accum,
                                                      step=step,
                                                      value=threshold),
                        method=['D8','D4','Rho8','Rho4','Dinf','Quinn']);
                        # 'Holmgren','Freeman' require exponents


    def _generate_boundary(self,distance:float,rectangular:bool=False):
        '''
        Generates a set of spaced nodes corresponding to the boundary of the DEM,
        where the boundary is defined as the intersection of no_data_values
        (nil) and elevation (non-nil) data.

        # Arguments
        distance (float): Euclidean distance between adjacent boundary nodes
        rectangular (bool): if True, generates a boundary around the extent of the DEM

        # Returns
        vertices of boundary
        '''

        if rectangular:
            self.boundary = boundary.rectangularBoundary(self.extent,distance)
            return self.boundary

        distance /= (0.5*(abs(self.cell_size[0]) + abs(self.cell_size[1])))
        self.boundary = boundary.squareTraceBoundary(self.dem,
                                                     self.no_data_value,
                                                     dist=distance)

        self.boundary = util.xyVectorToProjection(self.boundary,
                                                  self.cell_size,
                                                  self.xll_corner,
                                                  self.yll_corner,
                                                  self.nrows)
        return self.boundary


    def build_uniform_triplane(self,
                               edge_length:float,
                               smooth_boundary:bool=False,
                               flip:str='y',
                               apply_elevation:bool=True,
                               outfile:str=None,
                               rectangular_boundary:bool=False,
                               boundary_distance:float=None,
                               interactive:bool=False):
        '''
        Generates a triplane with uniformly sized elements.

        # Attributes
        edge_length (float): desired lengths for triangle edges
        flip (str): flips array of the elevation raster along a given axis (x,y,xy)
        smooth_boundary (bool): If True, smooth the DEM boundary for better interpolation
        apply_elevation (bool): If True, interpolate DEM elevations onto surface mesh
        outfile (str): filepath to save generated mesh to
        rectangular_boundary (bool): set to true if the DEM domain is rectangular
        boundary_distance (float): Overrides edge length and manually sets
                                   spacing between boundary nodes
        interactive (bool): if True and function is called within a
                            Jupyter notebook, then function params
                            can be controlled with sliders

        # Returns
        PyLaGriT mesh object
        '''

        if interactive:
            if cfg.IN_NOTEBOOK:
                return self.__interactive_triplane_uniform_driver(edge_length,
                                                                  smooth_boundary,
                                                                  flip,
                                                                  apply_elevation,
                                                                  outfile,
                                                                  rectangular_boundary)
            else:
                cfg.log.warn('Cannot init Jupyter notebook functionality')

        if self._surface_mesh is not None:
            self._surface_mesh.delete()

        if boundary_distance is None:
            boundary_distance = edge_length

        self._generate_boundary(boundary_distance,rectangular=rectangular_boundary)
        self._surface_mesh = mesh._uniform_surface_mesh(self.lg,
                                                        self.boundary,
                                                        outfile=outfile,
                                                        min_edge=edge_length*1.75)

        if apply_elevation:
            mesh._intrp_elevation_to_surface(self.lg,
                                             self,
                                             self._surface_mesh,
                                             outfile=outfile,
                                             flip=flip,
                                             smooth_boundary=smooth_boundary)

        if outfile is not None:
            cfg.log.info('Saved mesh to \"%s\"' % outfile)

        return self._surface_mesh


    def build_refined_triplane(self,
                               min_edge_length:float,
                               max_edge_length:float,
                               outfile:str=None,
                               apply_elevation:bool=True,
                               slope:float=2.,
                               refine_dist:float=0.5,
                               flip:str='y',
                               smooth_boundary:bool=False,
                               rectangular_boundary:bool=False,
                               boundary_distance:float=None,
                               interactive:bool=False):
        '''
        Generates a refined triangular mesh, with a minimum refinement length 
        defined by h.

        # Attributes
        min_edge_length (float): minimum triangle edge lengths
        max_edge_length (float): maximum triangle edge lengths
        outfile (str): Filepath to save mesh to
        apply_elevation (bool): If True, interpolate DEM elevations onto surface mesh
        slope (float): slope of refine function
        refine_dist (float): Threshold for minimum distance in distance map
        flip (str): flips array of the elevation raster along a given axis (`'x','y','xy'`)
        smooth_boundary (bool): If True, smooth the DEM boundary for better interpolation
        rectangular_boundary (bool): set to true if the DEM domain is rectangular
        boundary_distance (float): Overrides edge length and manually sets spacing between boundary nodes
        interactive (bool): if True and function is called within a
                            Jupyter notebook, then function params
                            can be controlled with sliders

        # Returns
        PyLaGriT mesh object
        '''

        if interactive:
            if cfg.IN_NOTEBOOK:
                return self.__interactive_triplane_refined_driver(min_edge_length,
                                                                  max_edge_length,
                                                                  smooth_boundary,
                                                                  flip,
                                                                  apply_elevation,
                                                                  outfile,
                                                                  rectangular_boundary,
                                                                  slope)
            else:
                cfg.log.warn('Cannot init Jupyter notebook functionality')        

        if self.feature is None:
            raise ValueError("Feature selection must be performed first")

        if boundary_distance is None:
            boundary_distance = max_edge_length

        # Generate boundary at max edge length
        self._generate_boundary(boundary_distance,rectangular=rectangular_boundary)

        # Filter feature to min edge length
        feature = deepcopy(self.feature)
        feature = util.filter_points(feature,min_edge_length*1.75)

        self._surface_mesh = mesh._refined_surface_mesh(self.lg,
                                                        self.boundary,
                                                        feature,
                                                        min_edge_length*1.75,
                                                        refine_dist=refine_dist,
                                                        slope=slope,
                                                        delta=0.75,
                                                        outfile=outfile)

        if apply_elevation:
            mesh._intrp_elevation_to_surface(self.lg,
                                             self,
                                             self._surface_mesh,
                                             outfile=outfile,
                                             flip=flip,
                                             smooth_boundary=smooth_boundary)

        if outfile is not None:
            cfg.log.info('Saved mesh to \"%s\"' % outfile)

        return self._surface_mesh


    def construct_surface_mesh(self,nodes:np.ndarray,tris:np.ndarray,zero_indexed:bool = False):
        '''
        Function that allows a user to explicitly set the nodes and elements
        of the surface mesh. For example, one could construct or load in a mesh 
        externally and pass the values here.

        # Arguments
        nodes (np.ndarray): Nx3 Numpy array with x,y,z values
        tris (np.ndarray): Nx3 Numpy array with triangle connectivity
        '''

        if not isinstance(tris, np.ndarray):
            tris = np.array(tris)

        if not isinstance(nodes, np.ndarray):
            nodes = np.ndarray

        if zero_indexed:
            tris += 1

        surface_mesh = '_surface_temp.inp'

        with open(surface_mesh,'w') as f:
            f.write('{} {} 0 0 0\n'.format(nodes.shape[0],tris.shape[0]))

            for (i,node) in enumerate(nodes):
                f.write('{} {} {} {}\n'.format(
                    i+1,
                    *nodes[i])
                )

            for (i,cell) in enumerate(tris):
                f.write('{} 1 tri {} {} {}\n'.format(
                    i+1,
                    *list(map(int,tris[i])))
                )

        self._surface_mesh = self.lg.read('_surface_temp.inp')

        try:
            os.remove(surface_mesh)
        except OSError as e:
            print("Could not clean up temporary file: ",e)

        # Handle the boundary
        _, bedges = boundary.get_alpha_shape(tris)
        bedges -= 1
        self.boundary = nodes[boundary.order_boundary_nodes(bedges)]

        return self._surface_mesh


    def build_layered_mesh(self,
                           layers,
                           matids=None,
                           outfile:str=None):
        '''
        Builds a layered mesh from a triplane.

        # Arguments
        layers (list<float>): List of sequential layer thicknesses
        matids (list<int>): List of material IDs to set each respective layer to
        outfile (str): Filepath to save mesh to

        # Example
        ```python
        layers = [1.,1.,3.,10.,2.]
        matids = [1,1,2,1,3]

        dem.build_layered_mesh(layers,matids=matids)
        ```

        '''

        if self._surface_mesh is None:
            raise ValueError("A surface mesh is required - run triplane generation")

        self._stacked_mesh = mesh._stacked_mesh(self.lg,
                                                self._surface_mesh,
                                                outfile,
                                                layers,
                                                matids=matids)

        self.number_of_layers = len(layers)
        
        return self._stacked_mesh

    def proportional_sublayering(self,layers:list):

        f_temp = 'temp.inp'
        self._surface_mesh.dump(f_temp)

        # Parse AVS surface mesh into a data structure
        with open(f_temp,'r') as f:
            header = f.readline().strip().split()
            nnodes = int(header[0])
            nelems = int(header[1])

            nodes = np.zeros((nnodes,3),dtype=float)
            elems = np.zeros((nelems,3),dtype=int)

            for i in range(nnodes):
                node = list(map(float,f.readline().split()[1:]))
                nodes[i,:] = node

            for i in range(nelems):
                elem = list(map(float,f.readline().split()[3:]))
                elems[i,:] = elem

        os.remove(f_temp)

        m = future.mesh.Mesh()
        m.nodes = nodes
        m.elements = elems
        m.element_type = future.mesh.ElementType.TRIANGLE

        stacked = future.layering.stack(m,layers)

        # Create the `layertyp` node attribute to mimic what LaGriT does
        layertyp = np.zeros((stacked.n_nodes,1),dtype=int)
        nodes_per_layer = stacked.metadata['layering']['nodes_per_layer']
        layertyp[:nodes_per_layer] = -2
        layertyp[-nodes_per_layer:] = -1
        stacked.add_attribute('layertyp',layertyp,attrb_type='node')

        future.helper.write_avs(
            f_temp,
            stacked.nodes,
            stacked.elements,
            cname='prism',
            matid=stacked.material_id,
            node_attributes={ 'layertyp': stacked.get_attribute('layertyp') }
        )

        self._stacked_mesh = self.lg.read(f_temp)
        self.number_of_layers = stacked.metadata['layering']['num_layers']

        os.remove(f_temp)
        
        return self._stacked_mesh


    def add_attribute(self,
                      data,
                      layers=None,
                      attribute_name=None,
                      outfile=None,
                      dtype=None):
        '''
        Adds an attribute to the stacked mesh, over one or more layers. Default is all.
        Data must be an NxM matrix - it does not necessarily have to be the same size at the DEM,
        but is recommended as it will be streched to span the domain of it.

        `attribute_name` will be the element-based attribute the data is written into.
        The default is 'material ID' (`itetclr`), but can be changed to any
        [a-z][A-Z][0-9] string (outside of reserved LaGriT keywords).

        # Arguments
        data (np.ndarray): NxM matrix of data to be written as matrix
        layers (list<int>): Layer IDs to write attributes to. Defaults to 'all'.
        attribute_name (str): Attribute name to store data in. Defaults to material ID
        outfile (str): Filename to write mesh to
        dtype (type): Data type of elements in data (`float` or `int`)
        '''

        # Check for the case that a user passes in a string
        # instead of a type
        if isinstance(dtype,str):
            if 'int' in dtype.lower():
                dtype = int
        
        self._stacked_mesh = attrib._add_attribute(self.lg,
                                                   data,
                                                   self._stacked_mesh,
                                                   [self.ncols,self.nrows],
                                                   self.number_of_layers,
                                                   self.extent,
                                                   attribute_name=attribute_name,
                                                   layers=layers,
                                                   outfile=outfile,
                                                   dtype=dtype)

        return self._stacked_mesh
        


    def remove_attribute(self,attribute_name,mesh=None):
        if mesh is None:
            mesh = self._stacked_mesh

        attrib.remove_attribute(mesh,attribute_name)

    def add_attribute_from_vector(
            self,
            v: np.ndarray, 
            attrb_type: str = 'element', 
            attrb_name:str = 'vector',
            apply_to_stacked_mesh=True
        ):
        '''
        Directly assign a node or element attribute from 
        an array of length n_nodes or n_elements.
        
        `attrb_type` should be either 'element' or 'node'.
        '''

        assert np.max(v.shape) == v.size, 'Vector is not 1-D'
        
        if apply_to_stacked_mesh:
            mesh = self._stacked_mesh
        else:
            mesh = self._surface_mesh

        np.savetxt('tmp_attrb.dat', v, delimiter='\n')
        mo1 = self.lg.read_att('tmp_attrb.dat', 'fattrb', operation = '1 0 0')
        
        if attrb_type.lower()[:4] == 'node':
            mesh.add_node_attribute(attrb_name)
        else:
            mesh.add_element_attribute(attrb_name)
            
        mesh.copyatt('fattrb', attname_sink=attrb_name, mo_src=mo1)
            
        mo1.delete()
        os.remove('tmp_attrb.dat')


    def node_to_element_attribute(self, node_attribute: str, element_attribute: str, apply_to_stacked_mesh=True):

        if apply_to_stacked_mesh:
            mesh = self._stacked_mesh
        else:
            mesh = self._surface_mesh

        mesh.interpolate('voronoi',element_attribute,mesh,node_attribute)

    def map_function_to_attribute(self,
                                  operator='+',
                                  layers=None,
                                  attribute_name=None,
                                  outfile=None,
                                  fn=lambda layer: layer*100):
        '''

        Maps a function and on operator onto mesh data.
        The function fn should take one parameter: the current layer
        number. The operator will perform on the data and function result.

        In other words, the new attribute data will be a result of:

             attribute_data(layer) = attribute_data [operation] fn(layer)

        For `fn = lambda layer: layer*100` and operator `+`,

             attribute_data(layer) = attribute_data + layer*100

        meaning that if a selection of attribute data is

             [1,3,5,10,12...]

        then, with operator '+' and lambda layer: layer*100,

              layer 1: [101,103,105,110,112...]
              layer 2: [201,203,205,210,212...]
              layer 3: [301,103,305,310,312...]
              ... 

        '''

        attrib._map_function_to_attribute(self.lg,
                                          self._stacked_mesh,
                                          self.number_of_layers,
                                          layers=layers,
                                          attribute_name=attribute_name,
                                          operator=operator,
                                          fn=fn,
                                          outfile=outfile)


    def meshStatistics(self):
        return self.stacked_mesh.information()

    def getBoundingBox(self,mpl_style:bool=True):
        '''
        Returns the bounding box (or extent) of the DEM domain.

        By default, the format of the extent returned is:

            (x_min,x_max,y_min,y_max)

        By setting `mpl_style=False`, the format changes to:

            (x_min,y_min,x_max,y_max)

        Extent units are relative to the parent DEM coordinate system.

        # Arguments
        mpl_style (bool): Change the format of the returned extent

        # Returns
        DEM domain bounding box
        '''

        if mpl_style:
            return (self.xll_corner,
                    self.ncols*self.cell_size[0]+self.xll_corner,
                    self.yll_corner,
                    self.nrows*self.cell_size[1]+self.yll_corner)
        else:
            return (self.xll_corner,
                    self.yll_corner,
                    self.ncols*self.cell_size[0]+self.xll_corner,
                    self.nrows*self.cell_size[1]+self.yll_corner)


    def plot_dem(self,hillshade:bool=False,plot_out:str=None):
        '''
        Plots the loaded DEM.
        '''
        tinplot.plot_dem(self,hillshade=hillshade,plot_out=plot_out)


    def plot_boundary(self):
        '''
        Plots the DEM domain boundary (if available).
        '''
        tinplot.plot_boundary(self)


    def plot_feature(self):
        '''
        Displays the feature captured by performing watershed delination
        (if available).
        '''
        tinplot.plot_feature(self)


    # =====================================
    # Hidden class methods
    # =====================================
    def __interactive_triplane_uniform_driver(self,
                                              edge_length,
                                              smooth_boundary,
                                              flip,
                                              apply_elevation,
                                              outfile,
                                              rectangular_boundary):
        '''
        Driver for Jupyter uniform triplane interactivity.
        '''
        def __fnc_driver(edge_length,smooth_boundary,flip,
                         apply_elevation,outfile,rectangular_boundary):
            self.build_uniform_triplane(edge_length,
                                        smooth_boundary=smooth_boundary,
                                        flip=flip,
                                        apply_elevation=apply_elevation,
                                        outfile=outfile,
                                        rectangular_boundary=rectangular_boundary,
                                        interactive=False)
            #tinplot.plot_triplane(self)
            cfg.log.info('Done.')


        try:
            from ipywidgets import interact,interactive,fixed,interact_manual
            from ipywidgets import FloatSlider
            import ipywidgets as widgets
        except NameError:
            cfg.IN_NOTEBOOK = False
            raise NameError('Could not init Jupyter notebook functionality')

        max_edge = abs(self.extent[1]-self.extent[0]) / 20.0
        min_edge = 0.0
        step = (max_edge - min_edge) / 100.

        if edge_length is None:
            edge_length = max_edge / 2.

        edge_length = max(min_edge,min(max_edge,edge_length))

        interact_manual(__fnc_driver,
                        edge_length=widgets.FloatSlider(min=min_edge,
                                                        max=max_edge,
                                                        step=step,
                                                        value=edge_length),
                        smooth_boundary=[('No', False), ('Yes', True)],
                        flip=['y','x','xy'],
                        apply_elevation=fixed(apply_elevation),
                        outfile=fixed(outfile),
                        rectangular_boundary=fixed(rectangular_boundary));



    def __interactive_triplane_refined_driver(self,
                                              min_edge,
                                              max_edge,
                                              smooth_boundary,
                                              flip,
                                              apply_elevation,
                                              outfile,
                                              rectangular_boundary,
                                              slope):
        '''
        Driver for Jupyter refined triplane interactivity.
        '''
        def __fnc_driver(edge_range,smooth_boundary,flip,
                         apply_elevation,outfile,rectangular_boundary,slope):

            min_edge_length = edge_range[0]
            max_edge_length = edge_range[1]

            self.build_refined_triplane(min_edge_length,
                                        max_edge_length,
                                        smooth_boundary=smooth_boundary,
                                        flip=flip,
                                        apply_elevation=apply_elevation,
                                        outfile=outfile,
                                        rectangular_boundary=rectangular_boundary,
                                        interactive=False,
                                        slope=slope)
            #tinplot.plot_triplane(self)
            cfg.log.info('Done.')

        try:
            from ipywidgets import interact,interactive,fixed,interact_manual
            from ipywidgets import FloatSlider
            import ipywidgets as widgets
        except NameError:
            cfg.IN_NOTEBOOK = False
            raise NameError('Could not init Jupyter notebook functionality')

        _max_edge = abs(self.extent[1]-self.extent[0]) / 50.0
        _min_edge = 0.0

        if min_edge is None:
            min_edge = _min_edge

        if max_edge is None:
            max_edge = _max_edge

        step = (max_edge - min_edge) / 100.

        interact_manual(__fnc_driver,
                        edge_range = widgets.FloatRangeSlider(
                                                value=[min_edge,max_edge],
                                                min=_min_edge,
                                                max=_max_edge,
                                                step=step,
                                                description='Edge Lengths:',
                                                disabled=False,
                                                continuous_update=False,
                                                orientation='horizontal',
                                                readout=True,
                                                readout_format='.1f'
                                            ),

                        slope =      widgets.FloatSlider(
                                                min=0.0,
                                                max=2.5,
                                                step=0.1,
                                                value=0.5
                                            ),

                        smooth_boundary = widgets.Checkbox(
                                                value=False,
                                                description='Smooth DEM boundary',
                                                disabled=False
                                            ),

                        flip=['y','x','xy'],
                        apply_elevation=fixed(apply_elevation),
                        outfile=fixed(outfile),
                        rectangular_boundary=fixed(rectangular_boundary));


