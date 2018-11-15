from tinerator import *

'''

1. Loading the DEM.

'''

# You can use any one of the three methods of importing a DEM:
my_dem = loadDEM("data/dem.asc") # by file
#my_dem = downloadDEM(bounds=(12.35,41.8,12.65,42)) # by bounding lat/long box
#my_dem = downloadDEM(shapefile="data/shapefile/example_shapefile.shp",crop=True) # or by shapefile

my_dem.plot() # view the DEM


'''

2. Preparing the mesh.

'''

# Perform watershed delineation to capture features
# spacing is distance between adjacent nodes
# in DEM units (i.e. meters), and will be represented in the mesh
my_dem.watershedDelineation(threshold=4500.,plot=True,spacing=30.)

# Generate a perimeter around the DEM, spaced at 10 meters.
my_dem.generateBoundary(100.)
my_dem.plotBoundary()


'''

3. Generating the mesh.

In generateStackedTIN, `h` is a proxy for minimum edge length.
Other optional parameters for adjusting minimum and maximum edge lengths, and
their transistion, are:

    delta: buffer zone, amount of h/2 removed around feature
    slope: slope of coarsening function
    refine_dist: distance used in coarsing function

'''

# Define the layers and corresponding material ids
layers = [0.1*50.,0.3*50.,0.6*50.,8.0*50.,21.0*50.]
matids = [1,2,3,4,5]

my_dem.generateStackedTIN("test_extruded_mesh.inp",layers,matids=matids,plot=True,h=50.)

'''

4. Adding attributes

Next, we are going to make a set of attributes and apply them to discrete
layers in the mesh.

Here, we're generating a standard 2D Gaussian for our sample dataset.
'Real' datasets should be imported using the proper library,
and then passed into the function.

'''
def makeGaussian(size, fwhm = 600, center=None):
    """ Make a square gaussian kernel.

    size is the length of a side of the square
    fwhm is full-width-half-maximum, which
    can be thought of as an effective radius.
    """

    import numpy as np

    x = np.arange(0, size, 1, float)
    y = x[:,np.newaxis]

    if center is None:
        x0 = y0 = size // 2
    else:
        x0 = center[0]
        y0 = center[1]

    return np.exp(-4*np.log(2) * ((x-x0)**2 + (y-y0)**2) / fwhm**2)

data = makeGaussian(500)

my_dem.addAttribute(data,1)
my_dem.addAttribute(1.-data,4)


'''

5. Generating facesets

'''

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
