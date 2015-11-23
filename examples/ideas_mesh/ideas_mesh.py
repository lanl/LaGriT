import optparse
import shutil, distutils.dir_util, os, sys, glob

import numpy
import matplotlib.pyplot as plt
from pylagrit import PyLaGriT

#
#  Create parser and options
#
p = optparse.OptionParser()
p.add_option('--dem-file', help='DEM data file name including path', type='string', dest='dem_filename' )
p.add_option('--exo-file', help='Exodus II mesh output including path', type='string', dest='exo_filename')
p.add_option('--dry-run', help='Show commands and diagnostics but no execution', default=False, dest='dry_run', action='store_true')
p.add_option('--plot', help='Show commands and diagnostics but no execution', default=False, dest='plot', action='store_true')
#
(opts,args) = p.parse_args()

print opts.dem_filename
print opts.dry_run

# Input: DEM data
dem_file_fullname=opts.dem_filename
dem_file_noext, dem_file_ext=os.path.splitext(opts.dem_filename)
dem_file_base=os.path.basename(opts.dem_filename).rstrip(dem_file_ext)
dem_file_path=os.path.dirname(opts.dem_filename)

# Output: Exodus II mesh
exo_file_fullname=opts.exo_filename
exo_file_noext, exo_file_ext=os.path.splitext(opts.exo_filename)
exo_file_base=os.path.basename(opts.exo_filename).rstrip(exo_file_ext)
exo_file_path=os.path.dirname(opts.exo_filename)

# Output: XML region specifications for the new mesh
xml_file_fullname=exo_file_path+exo_file_base+'.xml'

# DEM parsing
# 
with open(dem_file_fullname) as dem_header:
    for line in dem_header:
        if ("ncols" in line):
            nx=int(line.split()[1])
        elif ("nrows" in line):
            ny=int(line.split()[1])
        elif ("xllcorner" in line):
            xll_corner=float(line.split()[1])
        elif ("yllcorner" in line):
            yll_corner=float(line.split()[1])
        elif ("cellsize" in line):
            dx=float(line.split()[1])
            dy=dx
        elif ("NODATA_value" in line):
            no_data_value=int(line.split()[1])
            break

print nx, ny, xll_corner, yll_corner, dx, dy, no_data_value
dem_header.close()

d = numpy.flipud(numpy.genfromtxt(dem_file_fullname,skip_header=6))
#plt.matshow(d)

# Create mesh based on ncols and nrows in file, could automatically read
x = numpy.linspace(xll_corner,dx*(nx-1),nx)
y = numpy.linspace(yll_corner,dy*(ny-1),ny)

x=numpy.arange(xll_corner,dx*nx,dx)
y=numpy.arange(yll_corner,dy*ny,dy)

xx, yy = numpy.meshgrid(x,y)

# Write avs file
fh = open(dem_file_base+'.avs','w')
# Find number of data values and write avs header
N = numpy.where(numpy.isnan(d)==False)[0].shape[0]
fh.write(str(N)+' 0 0 0 0\n')
# Collect data into avs format
# JDM:  Not sure if this should be nx*ny? 
davs = numpy.zeros([nx*ny,4])
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
m = lg.read(dem_file_base+'.avs')
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
m2.createpts_xyz((nx,ny,1),[xx.min(),yy.min(),0.],[xx.max(),yy.max(),0],rz_switch=[1,1,1],connect=True)

#m3 = m2.grid2grid_quadtotri2()
m3=m2
# Create temporary z value attribute
m3.addatt('z_save',type='vdouble',rank='scalar')
# Interpolate elevations to z_save
m3.interpolate_voronoi('z_save',m,'z_save')

# Find nodes associated with nodata
pdel = m3.pset_attribute('z_save',-9999)
# Create element set from these nodes and remove elements
edel = pdel.eltset()
m3.rmpoint_eltset(edel)
# Copy temp z values over to actual a values
m3.copyatt('z_save','zic')
# Take a look to make sure everything is ok
#m2.paraview(exe='paraview')

# Create top surface avs
m3.dump('top.inp')
## Subtract 1 from z values of top and dump into bottom surface avs
m3.math('sub',1,'zic',cmosrc=m3)
m3.dump('mid.inp')
m3.setatt('zic',0.0)
m3.dump('bot.inp')

# Stack the layers in a new mesh object
stack = lg.create()
stack.stack_layers('avs',['bot.inp 1','mid.inp 1,3','top.inp 1,0'],flip_opt=True)
stack_hex = stack.stack_fill()
# Automatically create face sets based on normal vectors and layer id
fs = stack_hex.create_boundary_facesets(base_name='faceset_bounds',stacked_layers=True)
# Diagnostic
#stack_hex.sendline('quality volume itetclr')

# Write exo file with boundary facesets
stack_hex.dump_exo(exo_file_fullname,facesets=fs.values())

# Write region and faceset identifier file for ats_xml
matnames = {10000:'computational domain peat'}
facenames = {1:'bottom face',
             2:'surface',
             3:'front',
             4:'right',
             5:'back',
             6:'left'}

# Dump ats style xml for mesh, can provide options for other schemas easily also
stack_hex.dump_ats_xml(xml_file_fullname,exo_file_fullname,matnames=matnames,facenames=facenames)

# Take a look
if opts.plot: 
    stack_hex.paraview(exe='/Applications/paraview.app/Contents/MacOS/paraview')



