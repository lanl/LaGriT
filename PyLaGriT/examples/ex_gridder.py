from pylagrit import PyLaGriT
import numpy

lg = PyLaGriT()

x0 = -numpy.logspace(1,2,15,endpoint=True)
x1 = numpy.arange(-10,10,1)
x2 = -x0
x = numpy.concatenate([x0,x1,x2])
y = x
mqua = lg.gridder(x,y,elem_type='quad',connect=True)

mqua.paraview()
