
 1. hex_add_volumes.mlgi
    Reduce octree to tet mesh and add volume attributes


# Create cell and node volume attributes
# for resolution decisions based on volumes
# define MOVOL as the hex mesh for added attributes
#        MOVOL is a standard mesh and not octree
#        grid2grid/tree_to_fe will convert to standard form

cmo/status/MOVOL/brief
cmo/printatt/MOVOL/itetlev/minmax

# create tet voronoi attribute to put on hex nodes
cmo/create/motet
copypts/motet/MOVOL
  cmo/select/motet
  cmo/setatt/motet/imt/1
  filter/1,0,0
  rmpoint compress
  connect
  resetpts/itp
  quality
  dump avs tmp_connect.inp motet
  cmo/addatt/ motet / voronoi_volume / vor_vol
  dump avs tmp_vor_vol.inp motet

# interpolate from tet to hex mesh
cmo/addatt/MOVOL/vor_vol/VDOUBLE/scalar/nnodes/linear/permanent/afgx/0.0/
interpolate/voronoi/MOVOL vor_vol/1,0,0/motet vor_vol
cmo/delete/motet

# add more attributes
  cmo/addatt/MOVOL/ volume / cell_vol
  quality/edge_min/ y
  quality/edge_max/ y
  dump avs tmp_attributes.inp MOVOL

# minmax of added attributes
  cmo/printatt/MOVOL/-all- minmax

# check hex arrays
  cmo/printatt/MOVOL/itetlev/ minmax
  cmo/printatt/MOVOL/vor_vol/ minmax
  cmo/printatt/MOVOL/vor_vol/1,5,1
  cmo/printatt/MOVOL/cell_vol/ minmax
  cmo/printatt/MOVOL/cell_vol/1,5,1
  cmo/status/MOVOL/brief

# end infile macro hex_add_volumes.mlgi
finish

