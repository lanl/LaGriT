import richdem as rd
import matplotlib.pyplot as plt
from copy import deepcopy
from pylagrit import PyLaGriT
from tinerator.watershed_deliniation import *
from tinerator.boundary import *
from tinerator.unit_conversion import *
from tinerator.pack_points import *
from tinerator.dump import *
from tinerator.visualize import *
from tinerator.downloader import *
from tinerator.generate_triplane import *
import tinerator.config as cfg

GLOBAL_NDV = -9999.
MATERIAL_ID = 'itetclr'

def loadDEM(filepath:str,lagrit_exe:str=None):
    '''
    Loads a DEM raster from a local filepath and returns 
    a `tinerator.DEM` instance.

    # Attributes
    filepath (str): Filepath to DEM raster
    lagrit_exe (str,None): Optional filepath to LaGriT binary. If PyLaGriT is
                           configured correctly, this should be unnecessary.
    '''

    return DEM(filepath,lagrit_exe=lagrit_exe)


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

        self.no_data_value = self.dem.no_data
        self.ncols = np.shape(self.dem)[1]
        self.nrows = np.shape(self.dem)[0]
        self.cell_size = self.dem.geotransform[1]
        self.xll_corner = self.dem.geotransform[0]
        self.yll_corner = self.dem.geotransform[3] - self.nrows * self.cell_size
        self.mask = self.dem == self.dem.no_data if self.no_data_value in self.dem else None

        # The below values are defaults and should be changed based on DEM
        self.max_distance = 20.
        self.min_distance = 0.3
        self.max_edge = 10.
        self.min_edge = 0.1

        # Mesh characteristics
        self.number_of_layers = 0

        self.__replace_infs_with_nans()

    def __repr__(self):
        return 'DEM: %d rows, %d cols; cell size: %f' % \
        (self.nrows,self.ncols,self.cell_size)

    # Replace +/-float('inf') with np.nans which are more managable
    def __replace_infs_with_nans(self):
        for _invalid in [float('inf'),float('-inf')]:
            self.dem[self.dem == _invalid] = GLOBAL_NDV
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

        # Optional Arguments
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

    def plot(self,plot_out=None):
        '''
        Draws the DEM and distance map.

        # Attributes
        plot_out (str,None): file path to save image (optional)
        '''

        dem = deepcopy(self.dem)
        distance = deepcopy(self.distance_field)

        if self.mask is not None:
            try:
                dem[self.mask] = np.nan
            except ValueError:
                dem = dem.astype(float)
                dem[self.mask] = np.nan

        extent = (self.xll_corner,self.ncols*self.cell_size+self.xll_corner,
                  self.yll_corner,self.nrows*self.cell_size+self.yll_corner)

        plotDEM(dem,plot_out=plot_out,title="DEM",extent=extent,xlabel="latitude",ylabel="longitude")

    def fillDepressions(self,fill_depressions:bool=True,fill_flats:bool=True):
        '''
        Fills flats and depressions on DEM. On meshes intended to be high-
        resolution, leaving flats and depressions untouched may cause solver
        issues. This method should be called before generating a triplane.

        # Arguments
        fill_depressions (bool): fill pits and depressions on DEM
        fill_flats (bool): fill flats on DEM

        # Example
        ```python
        dem1 = loadDEM("example.asc")
        dem2 = loadDEM("example.asc")

        dem1.fillDepressions()

        plt.imshow(dem1.dem - dem2.dem)
        plt.show()
        '''

        if fill_depressions:
            cfg.log.info('Filling depressions')
            rd.FillDepressions(self.dem,epsilon=False,in_place=True)

        if fill_flats:
            cfg.log.info('Filling flats')
            rd.ResolveFlats(self.dem,in_place=True)


    def watershedDelineation(self,threshold:float=None,plot:bool=False,spacing:float=None,method:str='D8'):
        '''
        Performs watershed delineation on a DEM and returns a set of points
        corresponding to the feature.

        # Attributes
        threshold (float,None): threshold for determining feature from noise
        plot (bool,None): plot the watershed delineation and captured feature
        spacing (float,None): the 'resolution' of the feature polygon
        
        # Returns
        Polyline of feature as ordered (x,y) pairs
        '''

        accumulation = watershedDelineation(self.dem,method=method)

        if threshold is None:
            _thresh = np.unique(accumulation)
            threshold = _thresh[int(0.1*len(_thresh))]

        self.feature = getFeatureTrace(accumulation,feature_threshold=threshold)
        assert np.size(self.feature) != 0,"Feature trace is empty. Try setting a lower threshold."

        self.feature[:,0] = xVectorToProjection(self.feature[:,0],self.cell_size,self.yll_corner)
        self.feature[:,1] = yVectorToProjection(self.feature[:,1],self.cell_size,self.xll_corner,self.nrows)

        if spacing is not None:
            self.feature = filterPoints(self.feature,spacing)

        if plot:
            from mpl_toolkits.axes_grid1 import make_axes_locatable
            from matplotlib.colors import LogNorm

            f, (ax1, ax2) = plt.subplots(1, 2, figsize=(14,6))

            f.suptitle('Watershed delineation (threshold: %2.3e)\nMethod: %s' % (threshold,method))
            divider = make_axes_locatable(ax1)
            cax = divider.append_axes('right', size='5%', pad=0.05)

            im = ax1.imshow(accumulation,norm=LogNorm(vmin=0.01, vmax=np.max(accumulation)))
            f.colorbar(im, cax=cax, orientation='vertical')
            
            ax2.scatter(self.feature[:,0],self.feature[:,1],c=np.array([[0.,0.,0.]]),s=0.2)
            plt.show()

        return self.feature

    def __setRefinementSettings(self,min_edge=None,max_edge=None,min_distance=None,max_distance=None):
        self.min_edge = min_edge if min_edge is not None else self.min_edge
        self.max_edge = max_edge if max_edge is not None else self.max_edge
        self.min_distance = min_distance if min_distance is not None else self.min_distance
        self.max_distance = max_distance if max_distance is not None else self.max_distance

    def generateBoundary(self,distance:float):
        '''
        Generates a set of spaced nodes corresponding to the boundary of the DEM,
        where the boundary is defined as the intersection of noDataValue and elevation data.

        # Attributes
        distance (float): Euclidean distance between adjacent boundary nodes

        # Returns
        vertices of boundary
        '''

        distance /= self.cell_size

        self.boundary = squareTraceBoundary(self.dem,self.no_data_value,dist=distance)
        self.boundary[:,0] = xVectorToProjection(self.boundary[:,0],self.cell_size,self.xll_corner)
        self.boundary[:,1] = yVectorToProjection(self.boundary[:,1],self.cell_size,self.yll_corner,self.nrows)
        return self.boundary

    def __rectangularBoundary(self,distance):
        self.boundary = rectangularBoundary(self.getBoundingBox(),distance)
        self.boundary[:,0] = xVectorToProjection(self.boundary[:,0],self.cell_size,self.xll_corner)
        self.boundary[:,1] = yVectorToProjection(self.boundary[:,1],self.cell_size,self.yll_corner,self.nrows)
        return self.boundary

    def buildTriplane(self,min_edge:float,delta:float=0.75,outfile:str=None,
                           slope:float=2.,refine_dist:float=0.5,
                           apply_elevation:bool=True,flip:str='y',
                           plot:bool=False,smooth_boundary:bool=False):
        '''
        Generates a refined triangular mesh, with a minimum refinement length 
        defined by h. Then, extrudes the mesh with user-defined layer thickness
        and material ids.

        # Attributes
        outfile (str): filename to save mesh
        layers (list<float>): heights of each extruded layer
        h (float): meshing length scale
        delta (float): buffer zone, amount of h/2 removed around feature
        slope (float): slope of coarsening function
        refine_dist (float): distance used in coarsing function
        matids (list<int>): material ids for layers
        plot (bool): plot the surface mesh
        '''

        if min_edge is None:
            min_edge = 50.

        if self.feature is None:
            buildUniformTriplane(self.lg,self.boundary,"_trimesh.inp",min_edge=min_edge)
        else:
            buildRefinedTriplane(self.lg,self.boundary,self.feature,
                                 "_trimesh.inp",min_edge,
                                 refine_dist=refine_dist,
                                 slope=slope,delta=delta)

        if apply_elevation:
            addElevation(self.lg,self,"_trimesh.inp",fileout="_trimesh.inp",flip=flip,smooth_boundary=smooth_boundary)

        if plot:
            points = None
            triangles = None

            with open('_trimesh.inp','r') as f:
                header = f.readline().strip().split()
                n_pts = int(header[0])
                n_elems = int(header[1])

                points = np.empty((n_pts,3),dtype=float)
                triangles = np.empty((n_elems,3),dtype=int)

                for pt in range(n_pts):
                    line = f.readline().strip().split()
                    points[pt,:] = np.array([float(x) for x in line[1:]])

                for el in range(n_elems):
                    line = f.readline().strip().split()
                    triangles[el,:] = np.array([int(x) for x in line[3:]])

            from mayavi import mlab
            mlab.triangular_mesh(points[:,0],points[:,1],points[:,2],triangles-1,representation='wireframe')
            mlab.show()


    def layeredMesh(self,layers,matids=None,xy_subset=None,outfile=None):
        '''
        Builds a layered mesh from a triplane.

        # Arguments
        layers (list<float>): List of sequential layer thicknesses

        # Optional Arguments
        matids (list<int): List of numbers to set as material ID for a list
        outfile (str): Filepath to save mesh to
        xy_subset ():


        # Example
        ```python
        layers = [1.,1.,3.,10.,2.]
        matids = [1,1,2,1,3]

        dem.layeredMesh(layers,matids=matids)
        ```

        '''
        if outfile is None:
            outfile = '_tin_stacked_mo.inp'

        mo = stackLayers(self.lg,"_trimesh.inp",outfile,layers,matids=matids,
                         xy_subset=xy_subset,nlayers=nlayers)
        self.stacked_mesh = outfile
        self.number_of_layers = len(layers)


    def addAttribute(self,data,layers=None,attribute_name=None,outfile=None,dtype=float):
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

        '''
        outfile = self.stacked_mesh if outfile is None else outfile
        _tmp = addAttribute(self.lg,data,self.stacked_mesh,
                     outfile,[self.ncols,self.nrows],
                     self.number_of_layers,self.getBoundingBox(),
                     attribute_name=attribute_name,layers=layers,
                     dtype=dtype)
        _tmp.delete()


    def mapFunctionToAttribute(self,operator='+',layers=None,attribute_name=None,
                               outfile=None,fn=lambda layer: layer*100):
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

        outfile = self.stacked_mesh if outfile is None else outfile
        mapFunctionToAttribute(self.lg,self.stacked_mesh,
                               self.number_of_layers,layers=layers,
                               attribute_name=attribute_name,operator=operator,
                               fn=fn)

    def meshStatistics(self):
        return self.stacked_mesh.information()

    def generateFacesets(self,outfile,facesets=None,naive=False):
        '''
        Generate boundary face sets according to normal vectors and layer ID.

        :param lg: running instance of PyLaGriT
        :type lg: pylagrit.PyLaGriT
        :param outfile: filepath to save Exodus facesets
        :type outfile: str
        :param facesets: generated facesets integer array with length equal to boundary
        :type facesets: np.ndarray
        :param naive: flag to generate Exodus mesh with 3 facesets: top, bottom, sides
        :type naive: bool

        '''

        if self.stacked_mesh is None:
            raise ValueError("A stacked mesh must be generated before calling this function")

        if naive:
            mo = self.lg.read(self.stacked_mesh)
            generateFaceSetsNaive(self.lg,mo,outfile)
        else:
            assert facesets is not None, 'Function requires facesets array'
            generateComplexFacesets(self.lg,outfile,self.stacked_mesh,self.boundary,facesets)

    def generateSingleColumnPrism(self,outfile:str,layers:list,matids=None,nlayers=None,xy_subset=None):
        generateSingleColumnPrism(self.lg,"_trimesh.inp",outfile,layers,matids=matids,xy_subset=xy_subset,nlayers=nlayers)

    def calculateDistanceField(self,accumulation_threshold:float=75.,mask:bool=True,normalize:bool=True):
        '''
        Calculates the distance field for a DEM.
        To adjust the visibility of features, you may have to 
        tweak the accumulation threshold.

        :param accumulation_threshold: feature threshold 
        :type accumulation_threshold: float
        :param mask: flag to mask distance map similar to DEM
        :type mask: bool
        :returns: generated distance field
        '''

        if self.mask is None:
            mask = False

        accumulation = watershedDelineation(self.dem)
        self.distance_field = calculateDistanceField(accumulation,accumulation_threshold=accumulation_threshold)

        if normalize:
            if mask:
                self.distance_field[self.mask] = 0.
            self.distance_field = normalizeMatrix(self.distance_field) * 100.

        if mask:
            self.distance_field[self.mask] = self.no_data_value

        return self.distance_field

    def plotBoundary(self):

        dem = deepcopy(self.dem)

        if self.mask is not None:
            try:
                dem[self.mask] = np.nan
            except ValueError:
                dem = dem.astype(float)
                dem[self.mask] = np.nan

        fig,ax = plt.subplots()

        extent = (self.xll_corner,self.ncols*self.cell_size+self.xll_corner,
                  self.yll_corner,self.nrows*self.cell_size+self.yll_corner)
        im = ax.imshow(dem,zorder=1,extent=extent,cmap=plt.cm.cubehelix)
        fig.colorbar(im)
        plt.title('DEM with generated boundary')
        ax.scatter(self.boundary[:,0],self.boundary[:,1],zorder=2,s=1.,c='red')
        plt.show()

    def getBoundingBox(self,mpl_style:bool=True):
        '''
        Returns the bounding box (or extent) of the DEM domain.

        By default, the format of the extent returned is:

            (x_min,x_max,y_min,y_max)

        By setting `mpl_style=False`, the format changes to:

            (x_min,y_min,x_max,y_max)

        Extent units are relative to the parent DEM coordinate system.

        # Optional Arguments
        mpl_style (bool): Change the format of the returned extent

        # Returns
        DEM domain bounding box
        '''

        if mpl_style:
            return (self.xll_corner,
                    self.ncols*self.cell_size+self.xll_corner,
                    self.yll_corner,
                    self.nrows*self.cell_size+self.yll_corner)
        else:
            return (self.xll_corner,
                    self.yll_corner,
                    self.ncols*self.cell_size+self.xll_corner,
                    self.nrows*self.cell_size+self.yll_corner)


