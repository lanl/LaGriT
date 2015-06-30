/scratch/nts/gable/grid_gen/ascem/test_prism

on bash, convert to ascii
  for f in *.exo ; do echo $f; ncdump $f > $f.ascii; done

See included images

prism_one - One Prism 

prism_one_layer - xy min/max 0:1, z min/max 0:0.1
                - 200 prisms

prism_two_layer - same as above but 2 layers thick, z min/max 0:0.2
                - bottom layer 200 prisms material 1
                - top layer 200 prisms mater 2

prism_one_layer_perturb_z - random perturbation of z coordinate so that the top
                            of prism_one_layer is not flat

prism_two_layer_perturb_z - random perturbation of z coordinate so that the
                            interior surface is not flat


prism_one.exo
prism_one.gmv
prism_one.inp
prism_one.png

prism_one_layer.exo
prism_one_layer.gmv
prism_one_layer.inp
prism_one_layer.png

prism_one_layer_perturb_z.exo
prism_one_layer_perturb_z.gmv
prism_one_layer_perturb_z.inp
prism_one_layer_perturb_z.png

prism_two_layer.exo
prism_two_layer.gmv
prism_two_layer.inp
prism_two_layer.png

prism_two_layer_perturb_z.exo
prism_two_layer_perturb_z.gmv
prism_two_layer_perturb_z.inp
prism_two_layer_perturb_z.png

added mesh with facesets defined in 6 directions to last 2 material prism mesh

-rw-r----- 1 tam sft 137208 May  2 13:02 prism_outside_faces.gmv
-rw-r----- 1 tam sft  60978 May  2 13:02 prism_outside_faces.inp
-rw-r----- 1 tam sft   1903 May  2 13:02 fs_1_bottom.faceset
-rw-r----- 1 tam sft  52144 May  2 13:02 fs_1_bottom.gmv
-rw-r----- 1 tam sft   2103 May  2 13:02 fs_2_top.faceset
-rw-r----- 1 tam sft  52168 May  2 13:02 fs_2_top.gmv
-rw-r----- 1 tam sft    323 May  2 13:02 fs_3_right.faceset
-rw-r----- 1 tam sft   5856 May  2 13:02 fs_3_right.gmv
-rw-r----- 1 tam sft    323 May  2 13:02 fs_4_back.faceset
-rw-r----- 1 tam sft   5880 May  2 13:02 fs_4_back.gmv
-rw-r----- 1 tam sft    323 May  2 13:02 fs_5_left.faceset
-rw-r----- 1 tam sft   5904 May  2 13:02 fs_5_left.gmv
-rw-r----- 1 tam sft    323 May  2 13:02 fs_6_front.faceset
-rw-r----- 1 tam sft   5928 May  2 13:02 fs_6_front.gmv

-rw-r----- 1 tam sft  28204 May  2 13:02 mesh_fsets.exo
-rw-r----- 1 tam sft  55290 May  2 13:02 mesh_fsets.inp
-rw-r----- 1 tam sft  70636 May  2 13:02 mesh_fsets.gmv

Single material version
-rw-r----- 1 tam sft  27876 May  2 13:02 mesh_fsets_single_mat.exo
-rw-r----- 1 tam sft  55290 May  2 13:02 mesh_fsets_single_mat.inp
-rw-r----- 1 tam sft  53020 May  2 13:02 mesh_fsets_single_mat.gmv

README.method.txt has method detail 



