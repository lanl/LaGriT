# input hex fault read file FILE_FLT 
# input fault surface mesh object MOS
# write files FILE_NFLT and FILE_PFLT
# assign itetclr with MATNO

read/FILE_FLT/ moin
cmo/status/moin/ brief

cmo/setatt/moin/itetclr 1
cmo/setatt/moin/imt 1
resetpts/itp

# extract outside surface of the fault 
extract/surfmesh/1,0,0/cmosurf/moin/external
cmo/select/cmosurf

# define the negative and positive sides of the fault
cmo/select/cmosurf
surface/ surf_flt / reflect / sheet / MOS
region / r_fault_p / ge surf_flt
region / r_fault_n / lt surf_flt
pset / p_fault_p / region / r_fault_p
pset / p_fault_n / region / r_fault_n

cmo/addatt/cmosurf/n_flt/vint/scalar/nnodes/linear/permanent/agx/0/
cmo / setatt / cmosurf / n_flt / pset get p_fault_p / 2
cmo / setatt / cmosurf / n_flt / pset get p_fault_n / 3
cmo/setatt/cmosurf/ imt / 1
cmo/setatt/cmosurf/ imt / pset,get,p_fault_p / 2
cmo/setatt/cmosurf/ imt / pset,get,p_fault_n / 3

# check for continuous connectivity
boundary_components/ node / 2
boundary_components/ node / 3

# color faces of each side
cmo/setatt/cmosurf/itetclr/ 1

# inclusive will include extra associated nodes
eltset/epi/ inclusive pset,get,p_fault_p
eltset/eni/ inclusive pset,get,p_fault_n

# exclusive are only those attached to cells
eltset/epe/ exclusive pset,get,p_fault_p
eltset/ene/ exclusive pset,get,p_fault_n

# remove extra attributes and subset side surfs
define MO cmosurf
infile remove_attributes.mlgi

# create the thin sides of the fault mesh
cmo/setatt/cmosurf/itetclr/eltset,get,epe/ 2
cmo/setatt/cmosurf/itetclr/eltset,get,ene/ 3
cmo/addatt/cmosurf/volume/cell_vol
dump/avs/tmp_surf_sides.inp/ cmosurf

# negative side of fault
cmo/copy/moside/cmosurf
cmo/select/moside
eltset/eduds/ itetclr ne 2
rmpoint/element/eltset,get,eduds
rmpoint/compress
cmo/setatt/moside/imt/MATNO
cmo/setatt/moside/itetclr/MATNO
dump/avs/FILE_PFLT/moside
# report minmax values for thin fault pos
  cmo/printatt/moside/imt/minmax
  cmo/printatt/moside/itetlev/minmax
  cmo/printatt/moside/cell_vol/minmax
  cmo/printatt/moside/vor_vol/minmax
  cmo/delete/moside

# positive side of fault
cmo/copy/moside/cmosurf
cmo/select/moside
eltset/eduds/ itetclr ne 3
rmpoint/element/eltset,get,eduds
rmpoint/compress
cmo/setatt/moside/imt/MATNO
cmo/setatt/moside/itetclr/MATNO
dump/avs/FILE_NFLT/moside
# report minmax values for thin fault neg
  cmo/printatt/moside/imt/minmax
  cmo/printatt/moside/itetlev/minmax
  cmo/printatt/moside/cell_vol/minmax
  cmo/printatt/moside/vor_vol/minmax
  cmo/delete/moside


surface / surf_flt / remove
region / r_fault_p / remove
region / r_fault_n / remove
cmo/delete/surf_flt
cmo/delete/cmosurf
cmo/delete/moin

finish
