from tinerator import *
from matplotlib import pyplot as plt

lagrit_path = "/Users/livingston/.bin/lagrit"

# First, load the DEM from a physical file.
my_dem = loadDEM("data/dem.asc")

print(my_dem.__dict__)

delin = watershedDeliniation(my_dem.dem)
poly = getFeatureTrace(delin,100)

# Then, calculate the distance field.
#my_dem.calculateDistanceField(accumulation_threshold=4500.)

# Let's visualize it.
#my_dem.plot()

# Generate a perimeter around the DEM, spaced at 10 meters.
my_dem.generateBoundary(10.)

'''
# Set the refinement settings for triangulation.
my_dem.setRefinementSettings(min_edge=50.,max_edge=300.,min_distance=0.,max_distance=160.0)

# Finally, pack, triangulate, and extrude.
my_dem.generateTIN("my_new_tin.inp",offset=10.,lagrit_path=lagrit_path)
'''