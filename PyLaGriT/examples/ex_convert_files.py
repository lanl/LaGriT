#To use pylagrit, import the module.
import pylagrit

#Create your pylagrit session.
lg = pylagrit.PyLaGriT()

#Create a mesh object and dump it to a gmv file 'test.gmv'.
mo = lg.create(name='test')
mo.createpts_brick_xyz((5,5,5), (0,0,0), (5,5,5,))
mo.dump('gmv', 'test.gmv')

#Convert test.gmv to exoduce and contour files.
lg.convert('test.gmv', 'exo')
lg.convert('test.gmv', 'avs')



