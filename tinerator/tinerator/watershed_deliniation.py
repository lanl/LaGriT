import richdem as rd
import skfmm
from matplotlib import pyplot as plt
import numpy as np
from copy import deepcopy
import skimage.draw as draw

def __getPolygonFromFeature(feature,feature_threshold=750.):
    '''
    docs
    '''

    threshold_matrix = feature > feature_threshold
    #plt.imshow(threshold_matrix); plt.show()
    shapes = rasterio.features.shapes(threshold_matrix.astype(np.int32))
    for shape in shapes:
        xy = shape[0]['coordinates'][0]
        xy = np.array(xy)
        plt.scatter(xy[:,0],xy[:,1])
    plt.show()
    
    #print(shapes.__dict__)
    #polygons = [shapely.geometry.Polygon(shape[0]["coordinates"][0]) for shape in shapes if shape[1] == 1]
    #print(polygons[0].exterior.coords.xy) # POLYGON ((369 24, 369 25, 372 25, 372 24, 369 24))

def getFeatureTrace(feature: np.ndarray, distance: int, feature_threshold: float=750.):
    threshold_matrix = (feature > feature_threshold)
    captured_areas = np.zeros(np.shape(threshold_matrix),dtype=bool)

    from tinerator.unit_conversion import xVectorToProjection,yVectorToProjection
    from tinerator.generate_triplane import generateLineConnectivity,_writeLineAVS

    xllCorner = 0.0
    yllCorner = 0.0
    nRows = 751
    cellSize = 10.0

    xy = np.transpose(np.where(threshold_matrix == True))
    xy[:,0] = xVectorToProjection(xy[:,0],cellSize,xllCorner)
    xy[:,1] = yVectorToProjection(xy[:,1],cellSize,yllCorner,nRows)

    connectivity = generateLineConnectivity(xy)

    _writeLineAVS(xy,"tmp_boundary.inp",connections=connectivity)

    #import scipy.spatial.distance as distance
    #dst = distance.cdist(xy,xy)


def calculateDistanceField(accum: np.ndarray,accumulation_threshold:float=750.):
    '''
    Given flow accumulation, creates a distance field.

    :param accum: flow accumulation matrix
    :type accum: np.ndarray
    :param accumulation_threshold: feature threshold (lower value will
    show more detail, higher value will show less)
    :type accumulation_threshold: float

    Returns:
    :param distance_field: distance field of shape(accum)
    :type distance_field: np.ndarray
    '''

    distance_field = np.ones(np.shape(accum))
    thres = accum > accumulation_threshold
    distance_field[thres] = -1

    distance_field = skfmm.distance(distance_field,dx=0.25)
    return distance_field

def watershedDeliniation(dem,fill_depressions:bool=True,fill_flats:bool=True,method:str='D8'):
    '''
    Performs watershed delination on a DEM.
    Optionally, fills DEM pits and flats.

    :param dem: richdem array
    :type dem: richdem.rdarray
    :param fill_depressions: flag to fill DEM pits / holes
    :type fill_depressions: bool
    :param fill_flats: flag to fill DEM flats
    :type fill_flats: bool
    :param method: flow direction algorithm
    :type method: string

    Returns:
    :param accum: flow accumulation matrix
    :type accum: np.ndarray
    '''

    #dem_original = deepcopy(dem)

    if fill_depressions:
        rd.FillDepressions(dem,epsilon=False,in_place=True)

    if fill_flats:
        rd.ResolveFlats(dem,in_place=True)

    accum_d8 = rd.FlowAccumulation(dem, method=method)
    return accum_d8