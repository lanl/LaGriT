from pylagrit import PyLaGriT
import numpy

# Variables
nx = 50 # Number of points in x direction
dy = 0.25 # width of cells in y direction
distance = 12.34364319 # distance in x direction

l = PyLaGriT()

# Create base layer
layer = l.create()
layer.createpts_xyz((nx,2,1),[0.,0.,0.],[distance,dy,0.],rz_switch=[1,1,1],connect=True)
layer.setatt('itetclr',12)
layer.minmax_xyz()

# Create top of mesh
# Collapse y values
layer.addatt('y_save',type='vdouble',rank='scalar')
layer.copyatt('yic','y_save')
layer.setatt('yic',0.)

# Read in lidar top elevations
peat_surf_pts = l.read('surface_coords2.avs')
peat_surf_pts.addatt('z_save',type='vdouble',rank='scalar')
peat_surf_pts.copyatt('zic','z_save')
peat_surf_pts.setatt('zic',0.)

# Interpolate surface elevations to layer mo
layer.addatt('z_val',type='vdouble',rank='scalar')
layer.interpolate_voronoi('z_val',peat_surf_pts,'z_save')
layer.copyatt('y_save','yic')
layer.copyatt('z_val','zic')

# Save peat top
layer.setatt('imt',1)
layer.setatt('itetclr',1)
layer.dump('tmp_lay_peat_top.inp')
peat_surf_pts.delete()

# Read in peat bottom elevations
peat_bot_pts = l.read('bottom_peat2.avs')
peat_bot_pts.addatt('z_save',type='vdouble',rank='scalar')
peat_bot_pts.copyatt('zic','z_save')
peat_bot_pts.setatt('zic',0.)

# Interpolate peat bottom elevations to layer mo
layer.setatt('yic',0.)
layer.addatt('z_val',type='vdouble',rank='scalar')
layer.interpolate_voronoi('z_val',peat_bot_pts,'z_save')
layer.copyatt('y_save','yic')
layer.delatt('y_save')
layer.copyatt('z_val','zic')
layer.delatt('z_val')

# Save peat bot
layer.dump('tmp_lay_peat_bot.inp')

# Copy layer to peat_bot for use in generating additional layers
peat_bot = layer.copy()

stack_files = ['tmp_lay_peat_top.inp 1,5']
stack_files.append('tmp_lay_peat_bot.inp 1,33')

# Layer depths?
#           1   2   3    4    5    6    7   8    9   10
layers = [ 1.0, 0.16, 0.32, 0.64, 1.28, 3.2, 6.4, 8.0]
addnum = [   3,    3,    3,    3,    4,   4,   3,   5]
matnum = [2]*len(layers) 

layer_interfaces = numpy.cumsum(layers)
i = 1
for li,m,a in zip(layer_interfaces,matnum,addnum):
    layer.math('sub',li,'zic',cmosrc=peat_bot)
    stack_files.append('tmp_lay'+str(i)+'.inp '+str(int(m))+', '+str(a))
    layer.dump('tmp_lay'+str(i)+'.inp')
    i += 1

layer.setatt('zic',-45.)
layer.dump('tmp_lay_bot.inp')
stack_files.append('tmp_lay_bot.inp 2')
stack_files.reverse()

# Create stacked layer mesh and fill
stack = l.create()
stack.stack_layers('avs',stack_files,flip_opt=True)
stack_hex = stack.stack_fill()

# Create boundary facesets, dictionary of PyLaGriT faceset objects is returned
fs = stack_hex.create_boundary_facesets(base_name='faceset_bounds',stacked_layers=True)

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
