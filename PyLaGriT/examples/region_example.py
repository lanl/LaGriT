from pylagrit import PyLaGriT
lg = PyLaGriT()
mesh = lg.create()
mins = (0,0,0)
maxs = (5,5,5)
eighth = mesh.surface_box(mins,maxs)
boolstr1 = 'le '+eighth.name
boolstr2 = 'gt '+eighth.name
reg1 = mesh.region(boolstr1)
reg2 = mesh.region(boolstr2)
mreg1 = mesh.mregion(boolstr1)
mreg2 = mesh.mregion(boolstr2)
mesh.createpts_brick_xyz((10,10,10), (0,0,0), (10,10,10))
mesh.rmregion(reg1)
mesh.dump('reg_test.gmv')






