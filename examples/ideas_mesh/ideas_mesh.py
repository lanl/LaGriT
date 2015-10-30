import numpy
import matplotlib.pyplot as plt
from pylagrit import PyLaGriT

# Get data in and take a look
d = numpy.genfromtxt("dem1m.dat",skip_header=6)
#plt.matshow(d)

# Create mesh based on ncols and nrows in file, could automatically read
x = numpy.arange(80)
y = numpy.arange(16)
xx, yy = numpy.meshgrid(x,y)

# Write avs file
fh = open('dem1m.avs','w')
# Find number of data values and write avs header
N = numpy.where(numpy.isnan(d)==False)[0].shape[0]
fh.write(str(N)+' 0 0 0 0\n')
# Collect data into avs format
davs = numpy.zeros([80*60,4])
ind = 0
for i,row in enumerate(d):
    for j,v in enumerate(row):
        davs[ind,:] = [ind + 1, xx[i,j], yy[i,j], v]
        ind += 1
# Write data to avs file 
numpy.savetxt(fh,davs,fmt=['%4d','%.1lf','%.1lf','%.8lf'])   
fh.close()

# Crank up pylagrit
lg = PyLaGriT()
# Read in elevations in avs fromat
m = lg.read('dem1m.avs')
# Take a look, change paraview exe
# Will need to use glyph filter to view points
#m.paraview(exe='paraview')

# Copy z values over to attribute
m.addatt('z_save',type='vdouble',rank='scalar')
m.copyatt('zic','z_save')
# Not sure why this has to be done, but not data elements don't get removed otherwise
m.setatt('zic',0.)

# Create connected quad mesh surface
m2 = lg.create()
m2.createpts_xyz((80,16,1),[0.,0.,0.],[80,16,0],rz_switch=[1,1,1],connect=True)
# Create temporary z value attribute
m2.addatt('z_save',type='vdouble',rank='scalar')
# Interpolate elevations to z_save
m2.interpolate_voronoi('z_save',m,'z_save')

# Find nodes associated with nodata
pdel = m2.pset_attribute('z_save',-9999)
# Create element set from these nodes and remove elements
edel = pdel.eltset()
m2.rmpoint_eltset(edel)
# Copy temp z values over to actual a values
m2.copyatt('z_save','zic')
# Take a look to make sure everything is ok
#m2.paraview(exe='paraview')

# Create top surface avs
m2.dump('top.inp')
# Subtract 1 from z values of top and dump into bottom surface avs
m2.math('sub',1,'zic',cmosrc=m2)
m2.dump('bot.inp')

# Stack the layers in a new mesh object
stack = lg.create()
stack.stack_layers('avs',['bot.inp 1','top.inp 1,0'],flip_opt=True)
stack_hex = stack.stack_fill()
# Automatically create face sets based on normal vectors and layer id
fs = stack_hex.create_boundary_facesets(base_name='faceset_bounds',stacked_layers=True)
# Diagnostic
#stack_hex.sendline('quality volume itetclr')

# Write exo file with boundary facesets
stack_hex.dump_exo('ideas.exo',facesets=fs.values())

# Write region and faceset identifier file for ats_xml
matnames = {10000:'computational domain peat'}
facenames = {1:'bottom face',
             2:'surface',
             3:'front',
             4:'right',
             5:'back',
             6:'left'}

# Dump ats style xml for mesh, can provide options for other schemas easily also
stack_hex.dump_ats_xml('ideas.xml','ideas.exo',matnames=matnames,facenames=facenames)

# Take a look
stack_hex.paraview(exe='paraview')








