'''

(c) 2019. Triad National Security, LLC. All rights reserved.
 
This program was produced under U.S. Government contract 89233218CNA000001
for Los Alamos National Laboratory (LANL), which is operated by Triad National
Security, LLC for the U.S. Department of Energy/National Nuclear Security
Administration.
 
All rights in the program are reserved by Triad National Security, LLC,
and the U.S. Department of Energy/National Nuclear Security Administration.
The Government is granted for itself and others acting on its behalf a
nonexclusive, paid-up, irrevocable worldwide license in this material to
reproduce, prepare derivative works, distribute copies to the public,
perform publicly and display publicly, and to permit others to do so.

'''

import rasterio
import fiona
import geopandas
import os
import shutil
import richdem as rd
import matplotlib.pyplot as plt
from copy import deepcopy
from pylagrit import PyLaGriT

import tinerator.load as load
import tinerator.dump as dump
import tinerator.facesets as facesets
import tinerator.config as cfg
import tinerator.boundary as boundary
import tinerator.utilities as util
import tinerator.plot as plot
from tinerator.dem_class import DEM

def reprojectShapefile(shapefile:str,outfile:str,projection:str):
    '''
    Re-projects a shapefile from one coordinate space to
    another.

    # Arguments
    shapefile (str): filepath to the shapefile
    outfile (str): file to write re-projected shapefile to
    projection (str): string with new projection; i.e. 'epsg:3413'

    '''
    shp = geopandas.read_file(shapefile)
    shp = shp.to_crs({'init': projection})
    log.info('Shapefile re-projected to %s' % (projection))
    shp.to_file(outfile,driver='ESRI Shapefile')


def maskRasterWithShapefile(raster_filename:str,
                            shapefile_filename:str,
                            shapefile_reprojection:str=None,
                            raster_outfile:str=None,
                            return_dem:bool=True):
    '''

    # Arguments
    raster_filename (str): Raster file to be cropped
    shapefile_filename (str): Shapefile to crop raster with

    # Optional Arguments
    shapefile_reprojection (str): string with new projection; i.e. 'epsg:3413'
    raster_outfile (str): Filepath to save cropped raster
    return_dem (bool): if true, returns a tinerator.DEM object
    '''

    temp_shp_name = '_temp_shapefile'
    temp_dem_name = '_temp_raster'

    should_delete_new_raster = False

    if raster_outfile is None:
        should_delete_new_raster = True
        raster_outfile = temp_dem_name

    if shapefile_reprojection is not None:
        reprojectShapefile(shapefile_filename,temp_shp_name,shapefile_reprojection)
        shapefile_filename = temp_shp_name

    # Capture the shapefile geometry
    with fiona.open(shapefile_filename, 'r') as _shapefile:
        _poly = [feature['geometry'] for feature in _shapefile]

    # Open the DEM && mask && update metadata with mask
    with rasterio.open(raster_filename,'r') as _dem:
        out_image, out_transform = rasterio.mask.mask(_dem,
                                                      _poly,
                                                      crop=True,
                                                      invert=False)
        out_meta = _dem.meta.copy()

    # Update raster metadata with new changes
    out_meta.update({
                        "driver":    "GTiff",
                        "height":    out_image.shape[1],
                        "width":     out_image.shape[2],
                        "transform": out_transform
                    })

    # Write out DEM and import into a TINerator class
    with rasterio.open(raster_outfile, "w", **out_meta) as dest:
        dest.write(out_image)
    
    if return_dem:
        _dem = DEM(raster_outfile)

    if should_delete_new_raster:
        os.remove(raster_outfile)

    # Remove shapefile (as a folder or a file)
    if os.path.isfile(temp_shp_name):
        os.remove(temp_shp_name)
    else:
        shutil.rmtree(temp_shp_name)

    if return_dem:
        return _dem


