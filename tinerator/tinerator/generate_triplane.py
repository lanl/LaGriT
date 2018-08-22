'''

Meshing related functions that rely on an interface with LaGriT.

'''

import numpy as np
from tinerator.dump import callLaGriT

def stackLayers(lg,outfile,layers):
    '''
    Created a stacked mesh with layer thickness described by the variable
    layers.

    :param lg: Instantiation of PyLaGriT() class
    :type lg: pylagrit.PyLaGriT()
    :param outfile:
    :type outfile: str
    :param layers:
    :type layers: list<float>
    '''

    #layers = (0.1,0.3,0.6,8.0,21.0)
    stack_files = ['layer%d.inp' % (len(layers)-i) for i in range(len(layers))]

    #lg = PyLaGriT()
    motmp_top = lg.read(TOP_FILE)

    for (i,offset) in enumerate(layers):
        motmp_top.math('sub','zic',value=offset)
        motmp_top.dump('layer%d.inp' % (i+1))

    motmp_top.delete()
    stack = lg.create()
    stack.stack_layers('avs',stack_files,flip_opt=True)
    stack.dump('tmp_layers.inp')

    cmo_prism = stack.stack_fill(name='CMO_PRISM')
    cmo_prism.resetpts_itp()

    cmo_prism.addatt('cell_vol',keyword='volume')
    cmo_prism.dump('tmp_stack_volume.gmv')



def generateLineConnectivity(nodes,connect_ends=False):
    connectivity = np.empty((np.shape(nodes)[0]-1,2),dtype=np.int)
    for i in range(np.shape(connectivity)[0]):
        connectivity[i] = np.array((i+1,i+2))
    return connectivity

def _writeLineAVS(boundary,outfile,connections=None):

    nnodes = np.shape(boundary)[0]
    nlines = np.shape(connections)[0] if connections is not None else 0

    with open(outfile,'w') as f:
        f.write("{} {} 0 0 0\n".format(nnodes,nlines))

        for i in range(nnodes):
            f.write("{} {} {} 0.0\n".format(i+1,boundary[i][0],boundary[i][1]))

        for i in range(nlines):
            f.write("{} 1 line {} {}\n".format(i+1,connections[i][0],connections[i][1]))

        f.write("\n")

def buildRefinedTriplane(boundary,feature,outfile,connectivity=None):
    '''
    Constructs a triplane mesh using LaGriT as a backend.
    Requires an Nx2 np.ndarray as a boundary input, and an Nx2 np.ndarray as a 
    feature input.

    The boundary input must be ordered clockwise.

    :param boundary:
    :type boundary:
    :param feature:
    :type feature:
    :param outfile:
    :type outfile:
    :return mesh:
    '''

    if connectivity is None:
        connectivity = generateLineConnectivity(boundary)

    _writeLineAVS(boundary,"poly_1.inp",connections=connectivity)
    print("WRITING")

    outfile_gmv = "test.gmv"
    outfile_avs = outfile
    outfile_lg = "lg_out.lg"
    poly_file = "poly_1.inp"
    quad_file = "ANYBODY_HERE"
    excavate_file = "ANYBODY_HERE_ex"
    pre_final_massage = "PRE_FINAL_MASSAGE"
    line_file = "tmp_boundary.inp"
    output_iter_id_ssint = "output_inter"
    h = 10.0

    from numpy import genfromtxt, sqrt, cos, arcsin

    delta = 0.75
    h_extrude = 0.5*h # upper limit on spacing of points on interssction line
    h_radius = sqrt((0.5*h_extrude)**2 + (0.5*h_extrude)**2)
    h_trans = -0.5*h_extrude + h_radius*cos(arcsin(delta))


    lagrit_input = '''
define / ID / 1
define / OUTFILE_GMV / {0}
define / OUTFILE_AVS / {1}
define / OUTFILE_LG / {2}
define / POLY_FILE / {3}
define / QUAD_FILE / {4}
define / EXCAVATE_FILE / {5}
define / PRE_FINAL_FILE / {6}
define / PRE_FINAL_MASSAGE / {7}
define / H_SCALE / {10}
define / H_EPS / {11}
define / H_SCALE2 / {12}
define / H_EXTRUDE / {13}
define / H_TRANS / {14}
define / H_PRIME / {15}
define / H_PRIME2 / {16}
define / H_SCALE3 / {17}
define / H_SCALE8 / {18}
define / H_SCALE16 / {19}
define / H_SCALE32 / {20}
define / H_SCALE64 / {21}
define / PERTURB8 / {22}
define / PERTURB16 / {23}
define / PERTURB32 / {24}
define / PERTURB64 / {25}
define / PARAM_A / 2.000000 
define / PARAM_B / 0.000000 
define / PARAM_A2 / 1.000000 
define / PARAM_B2 / 0.025000 
define / THETA  / 0.000000000000 
define / X1 /  -0.000000000000 
define / Y1 / -0.000000000000 
define / Z1 / -0.000000000000 
define / X2 / 0.000000000000 
define / Y2 / 0.000000000000 
define / Z2 / 0.000000000000 
define / FAMILY / 2 

#LaGriT Script
# Name the input files that contain the polygons 
# and lines of intersection. 

define / POLY_FILE / {3}
define / LINE_FILE / {8}
define / OUTPUT_INTER_ID_SSINT / {9}

# Define parameters such as: 
# length scale to refine triangular mesh  
# purturbation distance to break symmetry of refined mesh# 

# Read in line and polygon files  
read / POLY_FILE / mo_poly_work

read / LINE_FILE / mo_line_work 
filterkd / 1,0,0 / 10.0
resetpts/itp

## Triangulate Fracture without point addition 
cmo / create / mo_pts / / / triplane 
copypts / mo_pts / mo_poly_work 
cmo / select / mo_pts 

# change to clockwise based on bndry
triangulate / clockwise 

cmo / setatt / mo_pts / imt / 1 0 0 / ID 
cmo / setatt / mo_pts / itetclr / 1 0 0 / ID 
resetpts / itp 
cmo / delete / mo_poly_work 
cmo / select / mo_pts 


# Creates a Coarse Mesh and then refines it using the distance field from intersections
massage / H_SCALE64 / H_EPS  / H_EPS
recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0
resetpts / itp
pset / p_move / attribute / itp / 1 0 0 / 0 / eq
perturb / pset get p_move / PERTURB64 PERTURB64 0.0
recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0
smooth;recon 0;smooth;recon 0;smooth;recon 0

massage / H_SCALE32 / H_EPS / H_EPS
resetpts / itp
pset / p_move / attribute / itp / 1 0 0 / 0 / eq
perturb / pset get p_move / PERTURB32 PERTURB32 0.0
recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0
smooth;recon 0;smooth;recon 0;smooth;recon 0

massage / H_SCALE16 / H_EPS  / H_EPS
resetpts / itp
pset / p_move / attribute / itp / 1 0 0 / 0 / eq
perturb / pset get p_move / PERTURB16 PERTURB16 0.0
recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0
smooth;recon 0;smooth;recon 0;smooth;recon 0

massage / H_SCALE8 / H_EPS / H_EPS
resetpts / itp
pset / p_move / attribute / itp / 1 0 0 / 0 / eq
perturb / pset get p_move / PERTURB8 PERTURB8 0.0
recon 0; smooth;recon 0;smooth;recon 0;smooth;recon 0
smooth;recon 0;smooth;recon 0;smooth;recon 0

cmo/addatt/ mo_pts /x_four/vdouble/scalar/nnodes 
cmo/addatt/ mo_pts /fac_n/vdouble/scalar/nnodes 

# Massage points based on linear function down to h_prime
massage2/user_function2.lgi/H_PRIME/fac_n/1.e-5/1.e-5/1 0 0/strictmergelength 

assign///maxiter_sm/1 
smooth;recon 0;smooth;recon 0;smooth;recon 0

assign///maxiter_sm/10

massage2/user_function.lgi/H_PRIME/fac_n/1.e-5/1.e-5/1 0 0/strictmergelength 
cmo / DELATT / mo_pts / rf_field_name 

# Extrude and excavate the lines of intersection
cmo / select / mo_line_work 

extrude / mo_quad / mo_line_work / const / H_EXTRUDE / volume / 0. 0. 1. 
cmo / select / mo_quad 

# Translate extruced lines of intersectino down slightly to excavate 
# nearby points from the mesh 

trans / 1 0 0 / 0. 0. 0. / 0. 0. H_TRANS
hextotet / 2 / mo_tri / mo_quad 
cmo / delete / mo_quad 
addmesh / excavate / mo_excavate / mo_pts / mo_tri

##### DEBUG #####
# If meshing fails, uncomment and rerun the script to get tmp meshes, 
# which are otherwise not output 
#dump / avs2 / tmp_tri.inp / mo_tri / 1 1 1 0
#dump / avs2 / tmp_pts.inp / mo_pts / 1 1 1 0
#dump / avs2 / tmp_excavate.inp / mo_excavate / 1 1 1 0
#finish
#####
 
cmo / delete / mo_tri 
cmo / delete / mo_pts 

# recompute dfield 
cmo / create / mo_final / / / triplane 
copypts / mo_final / mo_excavate  
compute / distance_field / mo_final / mo_line_work / dfield 
cmo / printatt / mo_final / dfield / minmax 
pset / pdel / attribute dfield / 1,0,0 / lt H_PRIME2 
rmpoint / pset,get,pdel / inclusive  
rmpoint / compress  

copypts / mo_final / mo_line_work  

cmo / select / mo_final 
cmo / setatt / mo_final / imt / 1 0 0 / ID 
cmo / setatt / mo_final / itp / 1 0 0 / 0 
cmo / setatt / mo_final / itetclr / 1 0 0 / ID 
# cmo / printatt / mo_final / -xyz- / minmax 
trans/ 1 0 0 / zero / xyz 
cmo / setatt / mo_final / zic / 1 0 0 / 0.0 
cmo / printatt / mo_final / -xyz- / minmax 
connect 

trans / 1 0 0 / original / xyz 
cmo / printatt / mo_final / -xyz- / minmax 

#cmo / delete / mo_line_work 
cmo / delete / mo_excavate
cmo / select / mo_final 
resetpts / itp 


## Massage Mesh Away from Intersection 
pset / pref / attribute / dfield / 1,0,0 / lt / H_EPS 
pset / pregion / attribute / dfield / 1,0,0 / gt / H_SCALE2 
pset / pboundary / attribute / itp / 1,0,0 / eq / 10 
pset / psmooth / not / pregion pref pboundary 
#massage / H_SCALE / 1.e-5 / 1.e-5 / pset get pref / & 
#nosmooth / strictmergelenth

assign///maxiter_sm/1 

smooth / position / esug / pset get psmooth; recon 0; 
smooth / position / esug / pset get psmooth; recon 0; 
smooth / position / esug / pset get psmooth; recon 0; 

assign///maxiter_sm/10
###########################################
# nodes for Intersection / Mesh Connectivity Check 
cmo / copy / mo_final_check / mo_final
#
# Define variables that are hard wired for this part of the workflow
define / MO_TRI_MESH_SSINT / mo_tri_tmp_subset
define / MO_LINE_MESH_SSINT / mo_line_tmp_subset
define / ATT_ID_INTERSECTION_SSINT / b_a
define / ATT_ID_SOURCE_SSINT / id_node_global
define / ATT_ID_SINK_SSINT / id_node_tri
#
# Before subsetting the mesh reate a node attribute containing the integer global node number
cmo / set_id / mo_final_check / node / ATT_ID_SOURCE_SSINT
#
# Subset the triangle mesh based on b_a node attribute ne 0
#
cmo / select / mo_final_check
pset / pkeep / attribute / ATT_ID_INTERSECTION_SSINT / 1 0 0 / ne / 0
pset / pall / seq / 1 0 0
pset / pdel / not pall pkeep
rmpoint / pset get pdel / exclusive
rmpoint / compress
#
# Create an integer node attribute in the line mesh to interpolate the triangle node number onto
# 
cmo / addatt / mo_line_work / ATT_ID_SINK_SSINT / vint / scalar / nnodes
interpolate / voronoi / mo_line_work ATT_ID_SINK_SSINT / 1 0 0 / &
                        mo_final_check  ATT_ID_SOURCE_SSINT
#
# Supress AVS output of a bunch of node attributes
#
cmo / modatt / mo_line_work / imt / ioflag / l
cmo / modatt / mo_line_work / itp / ioflag / l
cmo / modatt / mo_line_work / isn / ioflag / l
cmo / modatt / mo_line_work / icr / ioflag / l
cmo / modatt / mo_line_work / a_b / ioflag / l
cmo / modatt / mo_line_work / b_a / ioflag / l
#
# Output list of intersection nodes with the corrosponding node id number from the triangle mesh

dump / avs2 / OUTPUT_INTER_ID_SSINT / mo_line_work / 0 0 2 0
cmo / delete / mo_line_work

cmo / delete / mo_final_check
# nodes for intersection check over

cmo / select / mo_final 

##### DEBUG
# write out mesh before it is rotate back into its final location
# Useful to compare with meshing workflow if something crashes
#dump / avs2 / tmp_mesh_2D.inp / mo_final / 1 1 1 0 
##### DEBUG
# Rotate facture back into original plane 
rotateln / 1 0 0 / nocopy / X1, Y1, Z1 / X2, Y2, Z2 / THETA / 0.,0.,0.,/  
cmo / printatt / mo_final / -xyz- / minmax 
recon 1 

resetpts / itp 

cmo / addatt / mo_final / unit_area_normal / xyz / vnorm 
cmo / addatt / mo_final / scalar / xnorm ynorm znorm / vnorm 
cmo / DELATT / mo_final / vnorm 


cmo / DELATT / mo_final / x_four 
cmo / DELATT / mo_final / fac_n 
cmo / DELATT / mo_final / rf_field_name 
cmo / DELATT / mo_final / xnorm 
cmo / DELATT / mo_final / ynorm 
cmo / DELATT / mo_final / znorm 
cmo / DELATT / mo_final / a_b 
cmo / setatt / mo_final / ipolydat / no 
cmo / modatt / mo_final / icr1 / ioflag / l 
cmo / modatt / mo_final / isn1 / ioflag / l 
    
# Create Family element set
cmo / addatt / mo_final / family_id / vint / scalar / nelements 
cmo / setatt / mo_final / family_id / 1 0 0 / FAMILY
    

dump / OUTFILE_AVS / mo_final
dump / lagrit / OUTFILE_LG / mo_final

quality 
cmo / delete / mo_final 
cmo / status / brief 
finish

    '''.format(outfile_gmv,outfile_avs,outfile_lg,poly_file,quad_file,excavate_file,\
        pre_final_massage,pre_final_massage,line_file,output_iter_id_ssint,h,\
        h*10**-7,1.5*h,h_extrude,h_trans,0.8*h,0.3*h,3*h,8*h,16*h,32*h,64*h,\
        8*0.05*h,16*0.05*h,32*0.05*h,64*0.05*h)


    callLaGriT(lagrit_input,lagrit_path="/Users/livingston/.bin/lagrit")