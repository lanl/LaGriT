# reset imt values of truncated mesh

Source imt values are the truncated imt values minus the node values that need to be reset.

This example removes materials greater than MAX_MAT.
The removal can leave behind node values that need to be reset.
We do not want to remove the nodes as we want to keep these cells.

Main command file with the truncation calls:

```

# truncate top of mesh, remove materials gt 8

define CMO cmotet
define MAX_MAT 8

eltset/edel/ itetclr / gt MAX_MAT 
rmpoint element eltset,get,edel
rmpoint/compress
resetpts/itp

# FIX imt of nodes left behind after removal of cells

infile reset_truncated_nodes_imt.mlgi

```

Macro File reset_truncated_nodes_imt.mlgi:

```
# macro to fix node imt values left over from truncation
# must define CMO when calling this macro
# must define MAX_MAT

# create set of points with good values
# fix using nearest interpolation from good set

cmo/create/cmotmp
copypts/cmotmp/CMO
cmo/select/cmotmp
pset/pbad/attribute imt/1,0,0/gt MAX_MAT
rmpoint/pset,get,pbad
rmpoint/compress

cmo/select/CMO
pset/pfix/attribute imt/1,0,0/gt MAX_MAT
interpolate/voronoi/CMO imt/pset,get,pfix/ cmotmp imt

cmo/delete/cmotmp
cmo/printatt/CMO/imt minmax

finish

```
