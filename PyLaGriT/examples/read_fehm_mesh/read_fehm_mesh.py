from pylagrit import PyLaGriT
l = PyLaGriT()
mhex = l.read_fehm('fehm.grid')
#mhex.paraview()

mtet = l.create(elem_type='tet')
mtet = mhex.copypts()
mtet.connect()
mtet.paraview()


