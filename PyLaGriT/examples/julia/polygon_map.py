from pylagrit import PyLaGriT
import numpy as np

# User parameters #######################
# Discretization
nx = 61; ny = 61
# Polygon delineation filename
polygon_file = '2d_mesh.avs'
# User parameters #######################

l = PyLaGriT()

# Read in polygon delineation mesh
mopoly = l.read(polygon_file)

# Create quad using polygon min and max coordinates
xs = np.linspace(mopoly.xmin,mopoly.xmax,nx)
ys = np.linspace(mopoly.ymin,mopoly.ymax,ny)
moquad = l.gridder(xs,ys,elem_type='quad',connect=True)
mopoly.setatt('itetclr',1)

# Create extruded quad to tet and center at z=0
mopoly.extrude(1.0)
moprism = mopoly.extrude(1.0)
moprism.trans((0.,0.,0.),(0.,0.,0.5))
motet = moprism.grid2grid_prismtotet3()

# Clean up a little
mopoly.delete()
moprism.delete()

# Add attribute to moquad to store if in polygon or not
moquad.addatt('if_in_poly',vtype='vint')
moquad.setatt('if_in_poly', 0) # Make sure attr starts as zero

# Interpolate motet's itetclr into moquad's if_in_poly
moquad.interpolate_map('if_in_poly',motet,'itetclr')
moquad.intersect_elements(motet,attr_name='if_inter')

moquad.dump('polygon_map_python.inp')
