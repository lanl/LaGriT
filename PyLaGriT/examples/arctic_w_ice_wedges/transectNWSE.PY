from pylagrit import PyLaGriT
import numpy as np

lg = PyLaGriT()

# Create base layer, with x matching s from the csv file 
x = np.linspace(0.,29.75,(29.75-0.)/0.25+1)
y = [0.,0.25]
top = lg.gridder(x,y,elem_type='quad',connect=True)

# Create top of mesh
# Collapse y values
top.addatt('y_save',vtype='vdouble',rank='scalar')
top.copyatt('yic','y_save')
top.setatt('yic',0.)

# Read in top elevations
d = np.genfromtxt("transectNWSE.csv", delimiter=",", names=True)
coords = np.column_stack([d['s'],np.zeros_like(d['s']),d['z']])
surf_pts = lg.points(coords,elem_type='quad')
surf_pts.addatt('z_save',vtype='vdouble',rank='scalar')
surf_pts.copyatt('zic','z_save')
surf_pts.setatt('zic',0.)

# Interpolate surface elevations to top
top.addatt('z_val',vtype='vdouble',rank='scalar')
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

# Add (2) 1 cm thick layers
layer.math('sub',0.01*2,'zic')
layer.dump('tmp_lay1.inp')
stack_files.append('tmp_lay1.inp')
nlayers = [1]
matids.append(1)

# Add (6) 2 cm thick layers
layer.math('sub',0.02*6,'zic')
layer.dump('tmp_lay2.inp')
stack_files.append('tmp_lay2.inp')
nlayers.append(5)
matids.append(2)

# Add (8) 2 cm thick layers
layer.math('sub',0.02*8,'zic')
layer.dump('tmp_lay3.inp')
stack_files.append('tmp_lay3.inp')
nlayers.append(7)
matids.append(3)

# Add (15) 5 cm thick layers
layer.math('sub',0.05*15,'zic')
layer.dump('tmp_lay4.inp')
stack_files.append('tmp_lay4.inp')
nlayers.append(14)
matids.append(3)

# Add (15) 10 cm thick layers
layer.math('sub',0.1*15,'zic')
layer.dump('tmp_lay5.inp')
stack_files.append('tmp_lay5.inp')
nlayers.append(14)
matids.append(3)

# Add (15) 1 m thick layers
layer.math('sub',1*15,'zic')
layer.dump('tmp_lay6.inp')
stack_files.append('tmp_lay6.inp')
nlayers.append(14)
matids.append(3)

# Add (15) 2 m thick layers
layer.math('sub',2.*15.,'zic')
layer.dump('tmp_lay7.inp')
stack_files.append('tmp_lay7.inp')
nlayers.append(14)
matids.append(3)

# Add the bottom layer, and make the bottom boundary flat 
layer.setatt('zic',29.)
layer.dump('tmp_lay_bot.inp')
stack_files.append('tmp_lay_bot.inp')
nlayers.append(0)
matids.append(3)

# Create stacked layer mesh and fill
# Reverse arrays so that order is from bottom to top!!!
stack_files.reverse()
nlayers.reverse()
matids.reverse()
stack = lg.create()
stack.stack_layers(stack_files,nlayers=nlayers,matids=matids,flip_opt=True)
stack_hex = stack.stack_fill()

# Define ice wedges
def iceWedgePoints( vtxL, vtxB, vtxR, dx, dz ):
    xnodes = np.arange( vtxL[0], vtxR[0], dx )
    znodes = np.arange( vtxB[1], vtxR[1], dz )
    xg, zg = np.meshgrid(xnodes, znodes)
    xg = xg.flatten(); zg = zg.flatten()
    m1 = (vtxB[1] - vtxL[1])/(vtxB[0] - vtxL[0])
    b1 = vtxL[1] - m1*vtxL[0]
    m2 = (vtxR[1] - vtxB[1])/(vtxR[0] - vtxB[0])
    b2 = vtxR[1] - m2*vtxR[0]
    idx = [ (zg[i] > m1*xg[i]+b1) & (zg[i] > m2*xg[i]+b2)  for i in range(len(zg)) ]
    iwx = xg[np.array(idx)]; iwz = zg[np.array(idx)]
    iwy = np.concatenate( (np.ones(iwx.size)*0.05, np.ones(iwx.size)*0.2) )    
    iwx = np.tile(iwx, 2); iwz = np.tile(iwz, 2)
   
    return iwx, iwy, iwz

iw1x, iw1y, iw1z = iceWedgePoints( [3.25, 78.3], [3.875, 75.0], [4.5, 78.3], 0.125, 0.01 )
iw1pts = lg.points(np.column_stack([iw1x, iw1y, iw1z]), connect=True)
iw1 = stack_hex.eltset_object(iw1pts)
iw1.setatt('itetclr', 4)

iw2x, iw2y, iw2z = iceWedgePoints( [18.5, 78.4], [19.125, 75.1], [19.75,78.4], 0.125, 0.01 )
iw2pts = lg.points(np.column_stack([iw2x, iw2y, iw2z]), connect=True)
iw2 = stack_hex.eltset_object(iw2pts)
iw2.setatt('itetclr', 4)

# Create boundary facesets, dictionary of PyLaGriT faceset objects is returned
fs = stack_hex.create_boundary_facesets(base_name='faceset_bounds',stacked_layers=True,reorder=True)

# Should add this to PyLaGriT, but I'm feeling lazy ;-)
stack_hex.sendline('quality volume itetclr')

# Write exo file with boundary facesets
stack_hex.dump_exo('transectNWSE.exo',facesets=fs.values())

# Write region and faceset identifier file for ats_xml 
matnames = {10000:'computational domain moss',
            20000:'computational domain peat',
            30000:'computational domain mineral',
            40000:'computational domain ice wedge'}
facenames = {1:'bottom face',
             2:'surface',
             3:'front',
             4:'right',
             5:'back',
             6:'left'}

stack_hex.dump_ats_xml('transectNWSE_mesh.xml','../../mesh/transectNWSE.exo',matnames=matnames,facenames=facenames)
stack_hex.dump_ats_xml('transectNWSE_parmesh.xml','../../mesh/4/transectNWSE.par',matnames=matnames,facenames=facenames)
