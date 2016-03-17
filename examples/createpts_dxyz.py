from pylagrit import PyLaGriT
l = PyLaGriT()

# Create 2x2x2 cell mesh
m = l.create()
m.createpts_dxyz((0.5,0.5,0.5),(0.,0.,0.),(1.,1.,1.),rz_switch=[1,1,1],connect=True)
m.paraview()
#m.gmv()

# Create 2x2x2 mesh where maxs will be truncated to nearest value under given maxs
m_under = l.create()
m_under.createpts_dxyz((0.4,0.4,0.4),(0.,0.,0.),(1.,1.,1.),rz_switch=[1,1,1],connect=True)
m_under.paraview()
#m_under.gmv()

# Create 3x3x3 mesh where maxs will be truncated to nearest value over given maxs
m_over = l.create()
m_over.createpts_dxyz((0.4,0.4,0.4),(0.,0.,0.),(1.,1.,1.),bound='over',rz_switch=[1,1,1],connect=True)
m_over.paraview()
#m_over.gmv()

