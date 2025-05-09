# extract hex cells intersected by fault
# input fault surface MOS and hex mesh MOHEX
# write FILE_FLT

# Extract intersected fault cells
cmo/copy/mo2/MOHEX
cmo / select / mo2
intersect_elements / mo2 / MOS / if_inter
eltset / e_inter / if_inter / gt / 0

eltset/eduds/ not / e_inter
rmpoint element eltset,get,eduds
  rmpoint compress
  resetpts itp
  boundary_components/ node
  dump/avs/FILE_FLT mo2

# report minmax values for hex fault
cmo/printatt/mo2/imt/minmax
cmo/printatt/mo2/itetlev/minmax
cmo/printatt/mo2/cell_vol/minmax
cmo/printatt/mo2/vor_vol/minmax

cmo/delete/mo2

finish
