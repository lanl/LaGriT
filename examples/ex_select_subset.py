#To use pylagrit, import the module.
import pylagrit

#Start the lagrit session.
lg = pylagrit.PyLaGriT()

#Create a mesh object.
mo = lg.create()
mo.createpts_brick_xyz((5,5,5), (0,0,0), (5,5,5))

#Take the subset from (3,3,3)
mo.subset((3,3,3),(5,5,5))



