#To use pylagrit, import the module.
import pylagrit

#Create your pylagrit session.
lg = pylagrit.PyLaGriT()

#Create a mesh object and dump it to a gmv file 'test.gmv'.
mo = lg.create(name='test')
mo.createpts_brick_xyz((5,5,5), (0,0,0), (5,5,5,))
mo.dump('test.gmv')
mo.dump('test.avs')
mo.dump('test.lg')

mo1 = lg.read('test.gmv')
mo2 = lg.read('test.avs')
mo3 = lg.read('test.lg',name='test')


