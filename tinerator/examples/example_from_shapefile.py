from downloader import *
from matplotlib import pyplot as plt

example_shapefile = "/Users/livingston/playground/code_and_shapefiles/boundary.shp"

# Download a DEM based on a shapefile.
my_dem = downloadDEM(shapefile=example_shapefile)

# Calculate the distance field.
my_dem.calculateDistanceField()

# Plot them!
my_dem.plot()