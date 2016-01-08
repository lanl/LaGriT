from pylagrit import PyLaGriT

# Create polyline avs file of outline of mesh
# Probably should create a pylagrit function/method to do this...
mstr = '''\
4 4 0 0 0
1 0. 0. 0.
2 2200. 0. 0.
3 2200. 200. 0.
4 0. 1000. 0.
1 1 line 1 2
2 1 line 2 3
3 1 line 3 4
4 1 line 4 1

'''
with open('polyline.inp','w') as fh: fh.write(mstr)

# Create pylagrit object
lg = PyLaGriT()

# Read in polyline and create tri mesh
mop = lg.read('polyline.inp')
motri = mop.copypts(mesh_type='tri')
mop.delete()

# Triangulate polygon
motri.triangulate(order='counterclockwise')
motri.setatt('imt',1)
motri.setatt('itetclr',1)

# refine mesh with successively smaller edge length constraints
edge_length = [1000,500,250,125,75,40,20,15]
for i,l in enumerate(edge_length):
    motri.resetpts_itp()
    motri.refine(refine_option='rivara',refine_type='edge',values=[l],inclusive_flag='inclusive')
    motri.smooth()
    motri.recon(0)

# provide additional smoothing after the last refine
for i in range(5):
    motri.smooth()
    motri.recon(0)

# create delaunay mesh and clean up
motri.clean()
motri.recon(1)
motri.clean()

# dump fehm files
motri.dump_fehm('nk_mesh00')

# view results
motri.paraview()


