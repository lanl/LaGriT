# TINerator API


## loadDEM
```python
loadDEM(filepath:str, lagrit_exe:str=None)
```

Loads a DEM raster from a local filepath and returns
a `tinerator.DEM` instance.

__Arguments__

- `filepath (str)`: Filepath to DEM raster
- `lagrit_exe (str,None)`: Optional filepath to LaGriT binary. If PyLaGriT is
     configured correctly, this should be unnecessary.

__Optional Arguments__

- `filepath (str)`: Filepath to DEM raster
- `lagrit_exe (str,None)`: Optional filepath to LaGriT binary. If PyLaGriT is
     configured correctly, this should be unnecessary.