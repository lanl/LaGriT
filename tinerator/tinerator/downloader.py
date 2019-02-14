import subprocess
import tinerator
import rasterio
import fiona
import os
from rasterio.mask import mask

def downloadDEM(bounds=None,outfile=None,shapefile=None,margin=None,low_res=True,crop=False):
    '''
    Downloads a DEM within a specified bounding box or shapefile.
    The DEM will be at a SRTM3 (90m resolution) or SRTM1 (30m resolution).

    A simple bounding box or a shapefile may be passed in to define the area of
    the DEM to be downloaded. If using the bounding box method, note that
    values must be geodetic coordinates in the WGS84 reference system EPSG:4326.

    To download DEMs using a shapefile as a reference, you must run:
    
    ```bash
    pip install fiona
    ```

    # Example
    ```python
    my_dem = downloadDEM(bounds=(12.35,41.8,12.65,42),
                         outfile='Rome-90m-DEM.tif'))
    plt.imshow(my_dem)
    plt.show()
    ```

    # Arguments
    bounds (tuple): Bounding box values in geodetic coordinates
                    (left, bottom, right, top)
    outfile (str): File path to save DEM
    shapefile (str): File path to bounding shapefile
    margin (str): Percentage margin to extend bounding box
    low_res (bool): A value of True will recieve a 90m DEM, and False
                    recieves a 30m DEM

    # Returns
    `tinerator.DEM` instance
    '''

    cmd = 'eio '
    delete_raster = False
    if low_res:
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
            
        _dem = tinerator.DEM("_temp_cropped_dem.tif")
        os.remove("_temp_cropped_dem.tif")
        return _dem
    
    _dem = tinerator.DEM(outfile)
    if delete_raster:
        os.remove(outfile)
    return _dem
        
