# tinerator


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


## reprojectShapefile
```python
reprojectShapefile(shapefile:str, outfile:str, projection:str)
```

Re-projects a shapefile from one coordinate space to
another.

__Arguments__

- __shapefile (str)__: filepath to the shapefile
- __outfile (str)__: file to write re-projected shapefile to
- __projection (str)__: string with new projection; i.e. 'epsg:3413'


## maskRasterWithShapefile
```python
maskRasterWithShapefile(raster_filename:str, shapefile_filename:str, shapefile_reprojection:str=None, raster_outfile:str=None, return_dem:bool=True)
```


__Arguments__

- __raster_filename (str)__: Raster file to be cropped
- __shapefile_filename (str)__: Shapefile to crop raster with

__Optional Arguments__

shapefile_reprojection (str): string with new projection; i.e. 'epsg:3413'
raster_outfile (str): Filepath to save cropped raster
return_dem (bool): if true, returns a tinerator.DEM object

