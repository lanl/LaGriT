from pylagrit import PyLaGriT
import numpy as np

lg = PyLaGriT()

# Create base layer from x=10 to x=21 to match bert02 mesh
x = np.linspace(10.,21,(21.-10.)/0.25+1)
y = [0.,0.25]
top = lg.gridder(x,y,elem_type='quad',connect=True)

# Create top of mesh
# Collapse y values
top.addatt('y_save',type='vdouble',rank='scalar')
top.copyatt('yic','y_save')
top.setatt('yic',0.)

# Read in top elevations
d = np.genfromtxt("./Topo_Profile_NS_ERT.csv", delimiter=",", names=True)
surf_pts = lg.points(x=d['Distance_m'],z=d['Z'],elem_type='quad')
surf_pts.addatt('z_save',type='vdouble',rank='scalar')
surf_pts.copyatt('zic','z_save')
surf_pts.setatt('zic',0.)

# Interpolate surface elevations to top
top.addatt('z_val',type='vdouble',rank='scalar')
top.interpolate_voronoi('z_val',surf_pts,'z_save')
top.copyatt('y_save','yic')
top.copyatt('z_val','zic')

# Save top
top.setatt('imt',1)
top.setatt('itetclr',1)
top.dump('tmp_lay_peat_top.inp')
surf_pts.delete()

# Copy top to create intermediate layers
layer = top.copy()

# Begin to construct stacked layer arrays
# Names of layer files
stack_files = ['tmp_lay_peat_top.inp']
# Material id, should be same length as length of stack_files
matids = [1]

# Add (15) 2 cm thick layers
layer.math('sub',0.02*15,'zic')
layer.dump('tmp_lay1.inp')
stack_files.append('tmp_lay1.inp')
# Number of layers in between surfaces, should be length of stack_files - 1
nlayers = [14]
matids.append(1)

# Add (15) 5 cm thick layers
layer.math('sub',0.05*15,'zic')
layer.dump('tmp_lay2.inp')
stack_files.append('tmp_lay2.inp')
# Number of layers in between surfaces, should be length of stack_files - 1
nlayers.append(14)
matids.append(2)

# Add (15) 10 cm thick layers
layer.math('sub',0.1*15,'zic')
layer.dump('tmp_lay3.inp')
stack_files.append('tmp_lay3.inp')
# Number of layers in between surfaces, should be length of stack_files - 1
nlayers.append(14)
matids.append(2)

# Add (15) 1 c thick layers
layer.math('sub',1*15,'zic')
layer.dump('tmp_lay3.inp')
stack_files.append('tmp_lay3.inp')
# Number of layers in between surfaces, should be length of stack_files - 1
nlayers.append(14)
matids.append(2)

layer.math('sub',2.*15.,'zic')
layer.dump('tmp_lay4.inp')
stack_files.append('tmp_lay4.inp')
nlayers.append(14)
matids.append(2)
 
layer.setatt('zic',-45.)
layer.dump('tmp_lay_bot.inp')
stack_files.append('tmp_lay_bot.inp 2')
nlayers.append(1)
matids.append(2)

# Create stacked layer mesh and fill
# Reverse arrays so that order is from bottom to top!!!
stack_files.reverse()
nlayers.reverse()
matids.reverse()
stack = lg.create()
stack.stack_layers(stack_files,nlayers=nlayers,matids=matids,flip_opt=True)
stack_hex = stack.stack_fill()

# Create boundary facesets, dictionary of PyLaGriT faceset objects is returned
fs = stack_hex.create_boundary_facesets(base_name='faceset_bounds',stacked_layers=True,reorder=True)

# Should add this to PyLaGriT, but I'm feeling lazy ;-)
stack_hex.sendline('quality volume itetclr')

# Write exo file with boundary facesets
stack_hex.dump_exo('arctic_siteb_2d.exo',facesets=fs.values())

# Write region and faceset identifier file for ats_xml 
matnames = {10000:'computational domain peat',
            20000:'computational domain upper mineral'}
facenames = {1:'bottom face',
             2:'surface',
             3:'front',
             4:'right',
             5:'back',
             6:'left'}

stack_hex.dump_ats_xml('arctic_siteb_2d_mesh.xml','/scratch/tundra/dharp/arctic/geophysics/mesh/from_lucia/arctic_siteb_2d.exo',matnames=matnames,facenames=facenames)
stack_hex.dump_ats_xml('arctic_siteb_2d_par4_mesh.xml','/scratch/tundra/dharp/arctic/geophysics/mesh/from_lucia/arctic_siteb_2d.par',matnames=matnames,facenames=facenames)
