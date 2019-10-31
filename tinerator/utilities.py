'''

Set of functions that convert from matrix (row and column) units to
a DEM projection, and vice versa.

'''

import numpy as np
import skimage
import os
from osgeo import ogr, gdal, gdal_array

'''

POINT CONVERSION UTILITIES

'''

def convertXtoProjection(x,cellSize,xllCorner):
    return (xllCorner + (float(x) * cellSize) - (cellSize / 2.))

def convertYtoProjection(y,cellSize,yllCorner,nRows):
    return (yllCorner + (float(0. - y + float(nRows)) * cellSize) - (cellSize / 2.))

def convertProjectiontoX(x,cellSize,xllCorner):
    return (cellSize + 2. * float(x) - 2. * xllCorner) / (2. * cellSize)

def convertProjectiontoY(y,cellSize,yllCorner,nRows):
    return ((yllCorner - y) / cellSize + nRows + 1./2.)

def xVectorToProjection(v,cellSize,xllCorner):
    nv = np.vectorize(convertXtoProjection)
    return nv(v,cellSize,xllCorner)

def yVectorToProjection(v,cellSize,yllCorner,nRows):
    nv = np.vectorize(convertYtoProjection)
    return nv(v,cellSize,yllCorner,nRows)

def xProjectionToVector(v,cellSize,xllCorner):
    nv = np.vectorize(convertProjectiontoX)
    return nv(v,cellSize,xllCorner)

def yProjectionToVector(v,cellSize,yllCorner,nRows):
    nv = np.vectorize(convertProjectiontoY)
    return nv(v,cellSize,yllCorner,nRows)

def xyVectorToProjection(v,cellSize,xllCorner,yllCorner,nRows):

    for row in range(v.shape[0]):
        v[row][0] = convertXtoProjection(v[row][0],cellSize,xllCorner)
        v[row][1] = convertYtoProjection(v[row][1],cellSize,yllCorner,nRows)

    return v

def normalizeMatrix(A):
    return (A-np.min(A))/(np.max(A)-np.min(A))

def filter_points(points:np.ndarray,eps:float):
    '''
    Removes points that are within `eps` distance of each other.

    # Arguments
    points (np.ndarray): point array to filter
    eps (float): remove adjacent points within this distance of each other

    # Returns
    Filtered points
    '''
    from scipy.spatial.distance import cdist
    mask = np.ones(np.shape(points)[0],dtype=bool)

    for (i,p) in enumerate(points):
        if mask[i]:
            dst = cdist(points,[p])
            mask[np.argwhere((dst > 0.) & (dst < eps))] = False

    return points[mask]

def cleanup(files,silent=True):
    if not isinstance(files,list):
        files = [files]

    for file in files:
        try:
            os.remove(file)
        except Exception as e:
            if not silent:
                print('Could not remove %s' % file)
                print(e.message)

'''

RASTER RELATED UTILITIES

'''

def _smooth_raster_boundary(raster:np.ndarray,width:int,no_data_value:float=np.nan):
    '''
    Smooths the boundary of a raster to circumvent cropping artifacts.
    '''
    raster[raster == no_data_value] = np.nan
    width += width % 2 # Keep even for padding

    mask = np.array(~np.isnan(raster),dtype=int)
    dims = mask.shape

    edges = skimage.transform.resize(mask,
                                    (dims[0]-width,dims[1]-width),
                                    mode='edge',
                                    anti_aliasing=False,
                                    anti_aliasing_sigma=None,
                                    preserve_range=True,
                                    order=0)

    edges = np.pad(edges,pad_width=int(width/2),mode='constant',constant_values=True)
    edges = ~edges & mask

    raster[edges == True] = no_data_value
    return raster

def rasterize_shapefile_like(shpfile:str, model_raster_fname:str, nodata_val:float = 0):
    """
    Given a shapefile, rasterizes it so it has
    the exact same extent as the given model_raster

    Taken from [0].

    [0]: https://github.com/terrai/rastercube/blob/master/rastercube/datasources/shputils.py
    """
    
    dtype = gdal.GDT_Float64
    print(dtype)
    
    model_dataset = gdal.Open(model_raster_fname)
    shape_dataset = ogr.Open(shpfile)
    shape_layer = shape_dataset.GetLayer()
    mem_drv = gdal.GetDriverByName('MEM')
    mem_raster = mem_drv.Create(
        '',
        model_dataset.RasterXSize,
        model_dataset.RasterYSize,
        1,
        dtype
    )
    mem_raster.SetProjection(model_dataset.GetProjection())
    mem_raster.SetGeoTransform(model_dataset.GetGeoTransform())
    mem_band = mem_raster.GetRasterBand(1)
    mem_band.Fill(nodata_val)
    mem_band.SetNoDataValue(nodata_val)

    # http://gdal.org/gdal__alg_8h.html#adfe5e5d287d6c184aab03acbfa567cb1
    # http://gis.stackexchange.com/questions/31568/gdal-rasterizelayer-doesnt-burn-all-polygons-to-raster
    err = gdal.RasterizeLayer(
        mem_raster,
        [1],
        shape_layer,
        None,
        None,
        [1]
    )
    assert err == gdal.CE_None, 'Could not rasterize layer'
    return mem_raster.ReadAsArray()

'''

MESHING RELATED UTILITIES

'''

def _line_connectivity(nodes:np.ndarray,connect_ends:bool=False):
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


def _write_line(boundary,outfile:str,connections=None,material_id=None,
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
