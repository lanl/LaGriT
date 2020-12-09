from pylagrit import PyLaGriT
import numpy
import sys

df = 0.0005 # Fault half aperture
lr = 7 # Levels of refinement
nx = 4 # Number of base mesh blocks in x direction
nz = 20 # Number of base mesh blocks in z direction
d_base = df*2**(lr+1) # Calculated dimension of base block
w = d_base*nx # Calculated width of model
d = d_base*nz # Calculated depth of model

lg = PyLaGriT()

# Create discrete fracture mesh
dxyz = numpy.array([d_base,d_base,0.])
mins = numpy.array([0.,-d,0.])
maxs = numpy.array([w,0,0])
mqua = lg.createpts_dxyz(dxyz,mins,maxs,'quad',hard_bound=('min','max','min'),connect=True)

for i in range(lr):
    prefine = mqua.pset_geom_xyz(mins-0.1,(0.0001,0.1,0))
    erefine = prefine.eltset()
    erefine.refine()
    prefine.delete()
    erefine.delete()

mtri = mqua.copypts('triplane')
mtri.connect()
# Make sure that not nodes are lost during connect
if 'The mesh is complete but could not include all points.' in str(lg.before):
    print("Error: Lost some points during connect, not completing mesh and exiting workflow!\n")
    sys.exit()
mtri.tri_mesh_output_prep()
mtri.reorder_nodes(cycle='xic yic zic')
pfault = mtri.pset_geom_xyz(mins-0.1,(0.0001,0.1,0))
psource = mtri.pset_geom_xyz(mins-0.1,mins+0.0001)
mtri.setatt('imt',1)
pfault.setatt('imt',10)
psource.setatt('imt',20)

mtri.paraview(filename='discrete_fracture.inp')
