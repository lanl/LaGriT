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

# Global TINerator settings
params = {
    'remove_temporary_files': True,
    'quiet_lagrit': False,
    'enable_logging': True,
    'log_file': '_tin_log.out'
}

def loadDEM(filepath:str,lagrit_exe:str=None):
    return DEM(filepath,lagrit_exe=lagrit_exe)

class DEM():
    def __init__(self,filepath:str,lagrit_exe:str=None):
        self.dem = rd.LoadGDAL(filepath)
        self.lg = PyLaGriT(lagrit_exe=lagrit_exe)
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

    def plot(self,plot_out=None):
        '''
        Draws the DEM and distance map.

        :param plot_out: file path to save image (optional)
        :type plot_out: string
        '''

        dem = deepcopy(self.dem)
        distance = deepcopy(self.distance_field)

        if self.mask is not None:
            try:
                dem[self.mask] = np.nan
            except ValueError:
                dem = dem.astype(float)
                dem[self.mask] = np.nan
            #distance[self.mask] = np.nan

        extent = (self.xll_corner,self.ncols*self.cell_size+self.xll_corner,
                  self.yll_corner,self.nrows*self.cell_size+self.yll_corner)

        plotDEM(dem,plot_out=plot_out,title="DEM",extent=extent,xlabel="latitude",ylabel="longitude")
        #plotDEM(distance,plot_out=plot_out,extent=extent,title="Distance Field",xlabel="latitude",ylabel="longitude",hillshade_image=False)

    def watershedDelineation(self,threshold:float=None,plot:bool=False,spacing:float=None):
        '''

        Performs watershed delineation on a DEM and returns a set of points
        corresponding to the feature.

        :param threshold: threshold for determining feature from noise
        :type threshold: float
        :returns: (x,y) pairs of feature
        '''

        accumulation = watershedDelineation(self.dem)

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

            f.suptitle('Watershed delineation (threshold: %2.3e)' % threshold)            
            divider = make_axes_locatable(ax1)
            cax = divider.append_axes('right', size='5%', pad=0.05)

            im = ax1.imshow(accumulation,norm=LogNorm(vmin=0.01, vmax=np.max(accumulation)))
            f.colorbar(im, cax=cax, orientation='vertical')
            
            ax2.scatter(self.feature[:,0],self.feature[:,1],c=np.array([[0.,0.,0.]]),s=0.2)
            plt.show()

        return self.feature

    def writeAVS(self,outfile:str):
        '''
        Write out mesh in the UCD-AVS file format.

        :param outfile: path to save mesh
        :type outfile: string
        '''
        writeAVS(outfile,self.points,triangles=self.triangles)

    def setRefinementSettings(self,min_edge=None,max_edge=None,min_distance=None,max_distance=None):
        self.min_edge = min_edge if min_edge is not None else self.min_edge
        self.max_edge = max_edge if max_edge is not None else self.max_edge
        self.min_distance = min_distance if min_distance is not None else self.min_distance
        self.max_distance = max_distance if max_distance is not None else self.max_distance

    def generateBoundary(self,distance):
        '''
        Generates a set of spaced nodes corresponding to the boundary of the DEM,
        where the boundary is defined as the intersection of noDataValue and elevation data.

        :param distance: Euclidean distance between adjacent boundary nodes
        :type distance: float
        :returns: vertices of boundary
        '''

        distance /= self.cell_size

        self.boundary = squareTraceBoundary(self.dem,self.no_data_value,dist=distance)
        self.boundary[:,0] = xVectorToProjection(self.boundary[:,0],self.cell_size,self.xll_corner)
        self.boundary[:,1] = yVectorToProjection(self.boundary[:,1],self.cell_size,self.yll_corner,self.nrows)
        return self.boundary

    def rectangularBoundary(self,distance):
        self.boundary = rectangularBoundary(self.getBoundingBox(),distance)
        self.boundary[:,0] = xVectorToProjection(self.boundary[:,0],self.cell_size,self.xll_corner)
        self.boundary[:,1] = yVectorToProjection(self.boundary[:,1],self.cell_size,self.yll_corner,self.nrows)
        return self.boundary

    def generateStackedTIN(self,outfile:str,layers:list,h=None,delta:float=0.75,
                           slope:float=2.,refine_dist:float=0.5,matids=None,
                           nlayers=None,xy_subset=None,apply_elevation=True,flip='y',
                           plot=False):
        '''
        Generates a refined triangular mesh, with a minimum refinement length 
        defined by h. Then, extrudes the mesh with user-defined layer thickness
        and material ids.

        :param outfile: filename to save mesh
        :type outfile: str
        :param layers: heights of each extruded layer
        :type layers: list<float>
        :param h: meshing length scale
        :type h: float
        :param delta: buffer zone, amount of h/2 removed around feature
        :type delta: float
        :param slope: slope of coarsening function
        :type slope: float
        :param refine_dist: distance used in coarsing function
        :type refine_dist: float
        :param matids: material ids for layers
        :type matids: list<int>
        '''

        if h is None:
            h = 50.

        if self.feature is None:
            buildUniformTriplane(self.lg, self.boundary, "_trimesh.inp", min_edge=h)
        else:
            buildRefinedTriplane(self.lg,self.boundary,self.feature,"_trimesh.inp",h,refine_dist=refine_dist,slope=slope,delta=delta)

        if apply_elevation:
            addElevation(self.lg,self,"_trimesh.inp",fileout="_trimesh.inp",flip=flip)

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

        mo = stackLayers(self.lg,"_trimesh.inp",outfile,layers,matids=matids,xy_subset=xy_subset,nlayers=nlayers)
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
        addAttribute(self.lg,data,self.stacked_mesh,
                     outfile,[self.ncols,self.nrows],
                     self.number_of_layers,self.getBoundingBox(),
                     attribute_name=attribute_name,layers=layers,
                     dtype=dtype)

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
            generateFaceSetsNaive(self.lg,self.stacked_mesh,outfile)
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

    def getBoundingBox(self):
        '''
        Returns the bounding box (or extent) of the DEM domain.

        Format of return is:

           (x_min,x_max,y_min,y_max)

        '''

        return (self.xll_corner,self.ncols*self.cell_size+self.xll_corner,
                self.yll_corner,self.nrows*self.cell_size+self.yll_corner)


