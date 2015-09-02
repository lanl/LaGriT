# Set up a five spot co2 injection and brine production problem
import os,sys
from fdata import*
from fpost import*
import zipfile
import time
from glob import glob
import pylagrit

batch = False
dip = 90. # Degrees, dip of fault, angle is measured from horizontal
pars = {'k_cap':-18,
    'k_res':-12.,
    'k_aq':-14,
    'k_leak':-12,
    'zcap1':100.,
    'zcap2':100.,       
    'fx':3000.,
    'fw':380,
    'fl':1.e3,
    'injection_rate':10.,
    'specific_storage':1.e-4,
    'failure_pressure':3.,
    }

# dimensions
xmin, xmax = -2000., 6000.
zmin, zmax = -1000.,0.
print 'zmin = '+str(zmin)+' zmax '+str(zmax)
zmid = -1.e3        # 1 km depth

fx = pars['fx']         # distance between fault and injector
fw = pars['fw']
fl = pars['fl']

############################################## generate grid

# ndivisions
nxbase = 11
nzbase = int((zmax-zmin)/25.)

xmm = np.linspace(xmin,xmax,nxbase)
ymm = [0.,25.]
zmm = np.linspace(zmin,zmax,nzbase+1)

N = len(xmm)*len(ymm)*len(zmm)
print 'Grid will contain '+str(N) +' nodes'
print str(len(zmm))+' in the z direction'

# assemble parameters and root names
dat = fdata()
dat.work_dir='.'

dat.grid.make(dat.work_dir+os.sep+'GRID.inp',x=xmm,y=ymm,z=zmm)
dat.grid.write('AVS.inp', format='avs')
l = pylagrit.PyLaGriT(batch=batch)
m = l.read('AVS.inp')
z_inj = dat.grid.node_nearest_point((0,0,zmid)).position[2]
#m_l = l.create_line(2, (0.1,0.1,zmin), (0.1,0.1,zmax))
#m_l.connect_delaunay()
#m.refine_to_object(m_l,level=3)
#m_l.delete()

#for i in range(4):
#    prefine = m.pset_geom( (-.1,-.1,zmin), (.1,.1,zmax))
#    prefine.refine('constant','element',inclusive_flag='inclusive',prd_choice=12)
#    prefine.delete()

m_l = l.create_line(2, [fx+fw/2.,-fl/2.,zmax+.1], [fx+fw/2.,fl/2.,zmax+.1])
m_h = m_l.extrude(fw, direction=[1,0,0])
m_l.delete()
m_c = m_h.extrude((zmax-zmin)*2, direction=[np.tan(np.radians(90.-dip)),0.,-1])
m_h.delete()
m_c.connect_delaunay()
for i in range(4): m.refine_to_object(m_c,prd_choice=1)

m2 = m.grid2grid_tree_to_fe()
#m2.gmv()
m2.connect_delaunay()

# Set imt at injector
p_inj = m.pset_geom( (-.1,-.1,z_inj-0.1), (.1,.1,z_inj+0.1))
p_inj.setatt('imt',10)
 
# set imt of fault
attr_name = m2.intersect_elements(m_c)
e_fault = m2.eltset_attribute(attr_name,0,boolstr='gt')
p_fault = e_fault.pset()
p_fault.setatt('imt',20)

if batch: l.run_batch()
else: m2.paraview()

