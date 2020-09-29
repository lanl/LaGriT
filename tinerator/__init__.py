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

import vtk
import pyvista
import rasterio
import fiona
import geopandas
import os
import shutil
import richdem as rd
import matplotlib.pyplot as plt
from copy import deepcopy
from pylagrit import PyLaGriT

import tinerator.config as cfg
import tinerator.load as load
import tinerator.dump as dump
import tinerator.facesets as facesets
import tinerator.boundary as boundary
import tinerator.utilities as util
import tinerator.plot as plot
from tinerator.dem_class import DEM
import tinerator.future as future

from tinerator.plot import plot_triplane
from tinerator.plot import plot_full_mesh

_MAJOR = 0
_MINOR = 4
_PATCH = 3
VERSION = 'v{0}.{1}.{2}'.format(_MAJOR,_MINOR,_PATCH)

def test():
    future.mesh.test_mesh()
    future.layering.test_layering()

def reprojectShapefile(shapefile_in:str,shapefile_out:str,projection:str) -> None:
    '''
    Re-projects a shapefile and writes it to `shapefile_out`.

    # Arguments
    shapefile_in (str): filepath to the shapefile
    shapefile_out (str): file to write re-projected shapefile to
    projection (str): Proj4 string with new projection; i.e. '+init=epsg:3413'

    '''
    shp = geopandas.read_file(shapefile_in)
    shp = shp.to_crs(projection)
    cfg.log.info('Shapefile re-projected to: %s' % (projection))
    shp.to_file(shapefile_out,driver='ESRI Shapefile')

def reprojectRaster(raster_in:str,raster_out:str,dst_crs:str) -> None:
    '''
    Re-projects a raster and writes it to `raster_out`.

    # Example
    ```python
    reprojectRaster('dem_in.asc','dem_out.tif','EPSG:2856')
    ```

    # Arguments
    raster_in (str): Filepath to input raster
    raster_out (str): Filepath to save reprojected raster
    dst_crs (str): Desired CRS
    '''

    from rasterio.warp import calculate_default_transform, reproject, Resampling

    with rasterio.open(raster_in) as src:
        transform, width, height = calculate_default_transform(
            src.crs, dst_crs, src.width, src.height, *src.bounds)
        kwargs = src.meta.copy()
        kwargs.update({
            'crs': dst_crs,
            'transform': transform,
            'width': width,
            'height': height
        })
    
        with rasterio.open(raster_out, 'w', **kwargs) as dst:
            for i in range(1, src.count + 1):
                reproject(
                    source=rasterio.band(src, i),
                    destination=rasterio.band(dst, i),
                    src_transform=src.transform,
                    src_crs=src.crs,
                    dst_transform=transform,
                    dst_crs=dst_crs,
                    resampling=Resampling.nearest)

    cfg.log.info('Raster re-projected to: %s' % (dst_crs))


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
    if shapefile_reprojection is not None:
        if os.path.isfile(temp_shp_name):
            os.remove(temp_shp_name)
        else:
            shutil.rmtree(temp_shp_name)

    if return_dem:
        return _dem
