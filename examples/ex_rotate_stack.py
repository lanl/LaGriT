from pylagrit import PyLaGriT
import numpy

x = numpy.arange(0,10.1,1)
y = [0,1]
#z = [0,1]

lg = PyLaGriT()
layer = lg.gridder(x=x,y=y,elem_type='quad',connect=True)
layer.rotateln([0,layer.ymin-0.10,0],[0,layer.ymax+0.1,0],25)
layer.dump('tmp_lay_top.inp')

# Layer depths?
#           1   2   3    4    5    6    7   8    9   10
layers = [ .1, 1.]
addnum = [  4, 2]
#matnum = [2]*len(layers)
matnum = [2, 1]
layer_interfaces = numpy.cumsum(layers)

mtop = layer.copy()
stack_files = ['tmp_lay_top.inp 1,9']
#stack_files.append('tmp_lay_peat_bot.inp 1,33')

i = 1
for li,m,a in zip(layer_interfaces,matnum,addnum):
    layer.math('sub',li,'zic',cmosrc=mtop)
    stack_files.append('tmp_lay'+str(i)+'.inp '+str(int(m))+', '+str(a))
    layer.dump('tmp_lay'+str(i)+'.inp')
    i += 1

layer.math('sub',2,'zic',cmosrc=mtop)
#layer.setatt('zic',-2.)
layer.dump('tmp_lay_bot.inp')
stack_files.append('tmp_lay_bot.inp 2')
stack_files.reverse()

# Create stacked layer mesh and fill
stack = lg.create()
stack.stack_layers('avs',stack_files,flip_opt=True)
stack_hex = stack.stack_fill()


stack_hex.dump_exo('rotated.exo')
stack_hex.dump_ats_xml('rotated.xml','rotated.exo')

stack_hex.paraview()
