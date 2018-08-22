import richdem as rd
import matplotlib.pyplot as plt
from copy import deepcopy
from tinerator.watershed_deliniation import *
from tinerator.boundary import *
from tinerator.unit_conversion import *
from tinerator.pack_points import *
from tinerator.dump import *
from tinerator.visualize import *
from tinerator.downloader import *
from tinerator.generate_triplane import *

def loadDEM(filepath):
    return DEM(filepath)

class DEM():
    def __init__(self,filepath):
        self.dem = rd.LoadGDAL(filepath)
        self.distance_field = None
        self.triangles = None
        self.feature = None
        self.boundary = None
        self.points = None
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

        print(self.__dict__)

    def plot(self,plot_out=None):
        '''
        Draws the DEM and distance map.

        :param plot_out: file path to save image (optional)
        :type plot_out: string
        '''

        dem = deepcopy(self.dem)
        distance = deepcopy(self.distance_field)

        if self.mask is not None:
            dem[self.mask] = np.nan
            distance[self.mask] = np.nan

        extent = (self.xll_corner,self.ncols*self.cell_size+self.xll_corner,
                  self.yll_corner,self.nrows*self.cell_size+self.yll_corner)

        plotDEM(dem,plot_out=plot_out,title="DEM",extent=extent,xlabel="latitude",ylabel="longitude")
        plotDEM(distance,plot_out=plot_out,extent=extent,title="Distance Field",xlabel="latitude",ylabel="longitude",hillshade_image=False)

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

        self.boundary = squareTraceBoundary(self.dem,self.no_data_value,dist=distance)
        self.boundary[:,0] = xVectorToProjection(self.boundary[:,0],self.cell_size,self.xll_corner)
        self.boundary[:,1] = yVectorToProjection(self.boundary[:,1],self.cell_size,self.yll_corner,self.nrows)
        return self.boundary

    def _generateTIN(self,outfile,offset=10.,lagrit_path="lagrit"):
        '''

        ~~~~~~DEPRECATED~~~~~

        Generates an extruded TIN.

        This function will soon be refactored (though the syntax will stay approximately
        the same) in favor of a LaGriT-based algorithm.

        '''
        self.points = PlacePoints(self.dem,self.distance_field,self.boundary,self.ncols,self.nrows,
                                  self.cell_size,self.no_data_value,self.max_distance,
                                  self.min_distance,self.max_edge,self.min_edge,
                                  self.xll_corner,self.yll_corner)

        self.triangles = triangulateNodes(self.points,preserve_boundary=True,ncols=self.ncols,
                                          cell_size=self.cell_size,xll_corner=self.xll_corner,
                                          boundary_vertices=self.boundary)

        self.writeAVS("_trimesh.inp")
        extrudeTriangularMesh("_trimesh.inp",outfile,offset=offset,lagrit_path=lagrit_path)


    def generateTIN(self,outfile,offset=10.,lagrit_path="lagrit"):
        '''
        Generates an extruded TIN.

        '''
        buildRefinedTriplane(self.boundary,self.feature,"_trimesh.inp")
        #extrudeTriangularMesh("_trimesh.inp",outfile,offset=offset,lagrit_path=lagrit_path)

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

        accumulation = watershedDeliniation(self.dem)
        self.distance_field = calculateDistanceField(accumulation,accumulation_threshold=accumulation_threshold)

        if normalize:
            if mask:
                self.distance_field[self.mask] = 0.
            self.distance_field = normalizeMatrix(self.distance_field) * 100.

        if mask:
            self.distance_field[self.mask] = self.no_data_value

        return self.distance_field


