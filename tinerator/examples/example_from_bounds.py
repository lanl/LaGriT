from downloader import *
from matplotlib import pyplot as plt

lagrit_path = "/Users/livingston/.bin/lagrit"

# Define a bounding box of lat, long coordinates. This one is Rome!
my_dem = downloadDEM(bounds=(12.35,41.8,12.65,42))

# Calculate the distance field.
my_dem.calculateDistanceField()

# Plot!
my_dem.plot()