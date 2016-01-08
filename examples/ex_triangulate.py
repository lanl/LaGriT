from pylagrit import PyLaGriT

# Create pylagrit object
lg = PyLaGriT()

# Define polygon points in clockwise direction
# and create tri mesh object
coords = [[0.0, 0.0, 0.0], 
          [0.0, 1000.0, 0.0], 
          [2200.0, 200.0, 0.0], 
          [2200.0, 0.0, 0.0]]
motri = lg.tri_mo_from_polyline(coords)

# Triangulate polygon
motri.triangulate()
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
motri.tri_mesh_output_prep()

# dump fehm files
motri.dump_fehm('nk_mesh00')

# view results
motri.paraview()


