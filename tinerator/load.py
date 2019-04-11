import subprocess
import rasterio
import fiona
import os
from rasterio.mask import mask
from tinerator.dem_class import DEM
import tinerator.config as cfg
import numpy as np


def __download_dem(bounds=None,outfile=None,shapefile=None,margin=None,SRTM_30=True,crop=False):
    '''

    '''

    cmd = 'eio '
    delete_raster = False
    if not SRTM_30:
        cmd += '--product SRTM3 '
    if outfile is None:
        outfile = '_temp_dem.tif'
        delete_raster = True
    cmd += 'clip -o %s ' % (outfile)

    if shapefile is None:
        if bounds is None:
            raise RuntimeError('Parameters bounds or shapefile must be defined!')

        left, bottom, right, top = bounds

        if margin is not None:
            if isinstance(margin,str):
                margin_percent = float(margin[:-1]) / 100.

            margin_lon = (right - left) * margin_percent
            margin_lat = (top - bottom) * margin_percent
            left, bottom, right, top = (left - margin_lon,
                                        bottom - margin_lat,
                                        right + margin_lon,
                                        top + margin_lat)

        cmd += '--bounds %.2f %.2f %.2f %.2f' % (left,bottom,right,top)
    else:
        cmd += '--reference %s' % (shapefile)

    process = subprocess.call(cmd,shell=True,stderr=subprocess.STDOUT)

    if shapefile is not None and crop:

        # Capture the shapefile geometry
        with fiona.open(shapefile, "r") as _shapefile:
            _poly = [feature["geometry"] for feature in _shapefile]

        # Open the DEM in rasterio && mask && update metadata with mask
        with rasterio.open(outfile) as _dem:
            out_image, out_transform = mask(_dem,_poly,crop=True,invert=False)
            out_meta = _dem.meta.copy()

        out_meta.update({   "driver":    "GTiff",
                            "height":    out_image.shape[1],
                            "width":     out_image.shape[2],
                            "transform": out_transform
                        })

        # Write out DEM and import into a TINerator class
        with rasterio.open("_temp_cropped_dem.tif", "w", **out_meta) as dest:
            dest.write(out_image)
            
        _dem = DEM("_temp_cropped_dem.tif")
        os.remove("_temp_cropped_dem.tif")
        return _dem
    
    _dem = DEM(outfile)
    if delete_raster:
        os.remove(outfile)
    return _dem


def from_file(filepath:str,lagrit_exe=None):
    '''
    Loads a DEM raster from a local filepath and returns 
    a `tinerator.DEM` instance.

    # Arguments
    filepath (str): Filepath to DEM raster
    '''

    cfg.log.info('Reading DEM: %s' % filepath)

    if not os.path.isfile(filepath):
        raise FileNotFoundError('Could not find DEM: %s' % filepath)

    return DEM(filepath,lagrit_exe=lagrit_exe)


def from_matrix(array:np.ndarray,
                xllcorner=0.0,
                yllcorner=0.0,
                cell_size=10.0,
                no_data_value=-9999):
    '''
    Creates a TINerator DEM instance from a Numpy 2D matrix.

    # Arguments
    array (np.ndarray): NxM Numpy matrix
    * xllcorner (int,float): lower-left raster x-coordinate
    * yllcorner (int,float): lower-left raster y-coordinate
    * cell_size (int,float): raster cell size
    * no_data_value (int,float): raster null value
    
    **Note: no_data_value cannot be np.nan.**

    # Returns
    `tinerator.DEM` instance
    '''
    shp = np.shape(array)
    hdr = 'ncols {0}\nnrows {1}\nxllcorner {2}\nyllcorner {3}\ncellsize {4}\nnodata_value {5}'\
          .format(shp[1],shp[0],xllcorner,yllcorner,cell_size,no_data_value)
    np.savetxt('tmp_raster.asc',array,header=hdr,comments='')
    return DEM('tmp_raster.asc')


def from_coordinates(bounding_box,outfile=None,SRTM_30=True):
    '''
    Downloads and returns a DEM from a geodetic coordinate bounding box.

    This function is powered by the [elevation](https://pypi.org/project/elevation/)
    package, and sources rasters from either SRTM 30m Global 1 arc second V003
    elaborated by NASA and NGA,
    or SRTM 90m Digital Elevation Database v4.1 elaborated by CGIAR-CSI.

    Note that note bounding box values must be latitude and longitude values
    given as: `(left, bottom, right, top)`.

    (Specifically, geodetic coordinates in the WGS84 reference system EPSG:4326.)

    Note: if this method fails with error 'Too many tiles', try again with
    `SRTM_30=False`.

    # Arguments
    bounding_box (tuple): Lat/long bounding box values
    outfile (str): File path to save downloaded raster
    SRTM_30 (bool): If True, use SRTM 30m. If False, SRTM 90m.

    # Returns
    `tinerator.DEM` instance
    '''
    return __download_dem(bounds=bounding_box,outfile=outfile,SRTM_30=SRTM_30)


def from_shapefile(shapefile,outfile=None,SRTM_30=True):
    '''
    Downloads and returns the DEM enclosed by a shapefile.

    This function is powered by the [elevation](https://pypi.org/project/elevation/)
    package, and sources
    rasters from either SRTM 30m Global 1 arc second V003 elaborated by
    NASA and NGA, or SRTM 90m Digital Elevation Database v4.1 elaborated
    by CGIAR-CSI.

    Note: if this method fails with error 'Too many tiles', try again with
    `SRTM_30=False`.

    # Arguments
    shapefile (str): Path to ESRI Shapefile
    outfile (str): File path to save downloaded raster
    SRTM_30 (bool): If True, use SRTM 30m. If False, SRTM 90m.

    # Returns
    `tinerator.DEM` instance
    '''
    return __download_dem(shapefile=shapefile,outfile=outfile,SRTM_30=SRTM_30,crop=True)


