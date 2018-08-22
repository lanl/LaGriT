from tinerator import *
from matplotlib import pyplot as plt

# First, load the DEM from a physical file.
my_dem = loadDEM("data/dem.asc")

my_dem.watershedDeliniation(threshold=4500.)

# Then, calculate the distance field.
#my_dem.calculateDistanceField(accumulation_threshold=4500.)

# Let's visualize it.
#my_dem.plot()

# Define the layers and corresponding material ids
layers = (0.1*50.,0.3*50.,0.6*50.,8.0*50.,21.0*50.)
matids = (1,2,3,4,5)

# Generate a perimeter around the DEM, spaced at 10 meters.
my_dem.generateBoundary(10.)
my_dem.generateStackedTIN("test_extruded_mesh.inp",layers,matids=matids)