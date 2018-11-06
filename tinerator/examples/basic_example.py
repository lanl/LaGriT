from tinerator import *

# You can use any one of the three methods of importing a DEM:
my_dem = loadDEM("data/dem.asc") # by file
#my_dem = downloadDEM(bounds=(12.35,41.8,12.65,42)) # by bounding lat/long box
#my_dem = downloadDEM(shapefile="data/shapefile/example_shapefile.shp",crop=True) # or by shapefile

#my_dem.plot() # view the DEM
#from sys import exit
#exit(1)

# Perform watershed delineation to capture features
my_dem.watershedDelineation(threshold=4500.,plot=False)

# Generate a perimeter around the DEM, spaced at 10 meters.
my_dem.generateBoundary(10.)
#my_dem.plotBoundary()

# Define the layers and corresponding material ids
layers = (0.1*50.,0.3*50.,0.6*50.,8.0*50.,21.0*50.)
matids = (1,2,3,4,5)

my_dem.generateStackedTIN("test_extruded_mesh.inp",layers,matids=matids,plot=False)

# Now we can generate facesets in one of three ways:
option = 3 # change me!

# The 'naive' approach: only generate top, bottom, and sides
if option == 1:
    my_dem.generateFacesets('facesets_example.exo',naive=True)

# The GUI approach: manually select facesets from boundary
elif option == 2:
    fs_sides = selectFacesetsFromBoundary(my_dem) # Select sidesets
    fs_outlet = selectFacesetsFromBoundary(my_dem) # Select outlet - you should only choose one faceset here!

    my_dem.generateFacesets('facesets_example.exo',facesets={'all': fs_sides, 'top': fs_outlet})

# The programmatic approach: define facesets using stateplane coordinates
elif option == 3:

    _coords = {'all': np.array([[3352.82,7284.46],[7936.85,4870.53],[1798.4,256.502],[1182.73,1030.19]]),
               'top': np.array([[780.41,304.79],[567.05,524.24]])}

    fs = getFacesetsFromCoordinates(_coords,my_dem.boundary)
    my_dem.generateFacesets('facesets_example.exo',facesets=fs)

# Additional note: you can simply pass in a numpy array for the facesets argument, 
# and side facesets will be generated, though no outlet will be recognized.