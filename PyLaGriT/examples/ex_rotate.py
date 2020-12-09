from pylagrit import PyLaGriT
import numpy

x = numpy.arange(0,10.1,1)
y = x
z = [0,1]

lg = PyLaGriT()
mqua = lg.gridder(x,y,z,elem_type='hex',connect=True)

mqua.rotateln([mqua.xmin-0.1,0,0],[mqua.xmax+0.1,0,0],25)

mqua.dump_exo('rotated.exo')
mqua.dump_ats_xml('rotated.xml','rotated.exo')

mqua.paraview()
