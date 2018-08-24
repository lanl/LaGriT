from tinerator import *

# You can use any one of the three methods of importing a DEM:
my_dem = loadDEM("data/dem.asc") # by file
my_dem = downloadDEM(bounds=(12.35,41.8,12.65,42)) # by bounding lat/long box
my_dem = downloadDEM(shapefile="data/shapefile/example_shapefile.shp",crop=True) # or by shapefile

my_dem.plot() # view the DEM

# Perform watershed delineation to capture features
my_dem.watershedDelineation(threshold=4500.)

# Define the layers and corresponding material ids
layers = (0.1*50.,0.3*50.,0.6*50.,8.0*50.,21.0*50.)
matids = (1,2,3,4,5)

# Generate a perimeter around the DEM, spaced at 10 meters.
my_dem.generateBoundary(10.)
my_dem.generateStackedTIN("test_extruded_mesh.inp",layers,matids=matids)
