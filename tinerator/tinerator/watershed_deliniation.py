import richdem as rd
import skfmm
from matplotlib import pyplot as plt
import numpy as np
from copy import deepcopy
import skimage.draw as draw

def getFeatureTrace(feature:np.ndarray,feature_threshold:float=750.):
    '''
    Returns an array of (x,y) pairs corresponding to values over a given
    threshold in a feature array.

    :param feature: 
    :type feature:
    :param distance:
    :type distance:
    :param feature_threshold:
    :type feature_threshold:
    :returns: 
    '''

    threshold_matrix = (feature > feature_threshold)
    captured_areas = np.zeros(np.shape(threshold_matrix),dtype=bool)
    xy = np.transpose(np.where(threshold_matrix == True))
    xy[:, 0], xy[:, 1] = xy[:, 1], xy[:, 0].copy()

    return xy


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

def watershedDelineation(dem,fill_depressions:bool=True,fill_flats:bool=True,method:str='D8',exponent=None):
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

    dem = deepcopy(dem)

    if fill_depressions:
        rd.FillDepressions(dem,epsilon=False,in_place=True)

    if fill_flats:
        rd.ResolveFlats(dem,in_place=True)

    accum_d8 = rd.FlowAccumulation(dem,method=method,exponent=exponent)
    return accum_d8
