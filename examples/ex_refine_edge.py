from pylagrit import PyLaGriT
import numpy

lg = PyLaGriT()
#m = lg.create()
dxyz = numpy.array([0.256,0.256,0.])
#mins = numpy.array([0.,0,0.])
#maxs = numpy.array([5.,99.84,0])
mins = numpy.array([0.,-10.24,0.])
maxs = numpy.array([5.,0,0])
m = lg.createpts_dxyz(dxyz,mins,maxs,hard_bound=('min','max','min'),connect=True)
#m.trans((0.,m.ymax,0.),(0.,0.,0.))

for i in range(8):
    prefine = m.pset_geom_xyz(mins-0.1,(0.0001,0.1,0))
    erefine = prefine.eltset()
    erefine.refine()
    prefine.delete()
    erefine.delete()

m.paraview(filename='m_refine.inp')
