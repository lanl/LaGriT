## 5. Constructing a Fault

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/05_hex_01_fault_imt_itetclr.png)

### 5.1 Creating a Fault and Subsurface Layers

Next, we are going to map a fault and surfaces to our mesh. The objects created
will be:

![](images_new/05_fault_objects.png)

For all five of these surfaces, we will:

1. Define the X,Y,Z extent
2. Use `quadxy` to generate the point distribution
3. Connect the points into a quad mesh using `createpts/brick`.

For the main fault mesh, this process looks like:

```
cmo / create / mosurf_fault
cmo / select / mosurf_fault

define / X0S /  -20.0
define / X1S / 4020.0

define / Y0S /  -20.0
define / Y1S / 4020.0

define / Z1 / -1.e4
define / Z2 / -1.e4
define / Z3 /  1.e4
define / Z4 /  1.e4

quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
cmo / printatt / mosurf / -xyz- / minmax
```

For the remaining four surfaces this process is repeated, with `Z1,Z2,Z3,Z4`
altered independently.

The created surfaces have the names `mosurf1_fminus`, `mosurf2_fminus`,
`mosurf1_fplus`, `mosurf2_fplus`, and `mosurf_fault`.

### 5.2 Define Geometry of Hydrostratigraphic Model

Recall in step 4 how we used two surface meshes to alter `imt` and `itetclr`
values: first, by defining `surfaces` from the planar meshes; second, using the
`surface` objects to define `region` objects; third, creating `psets` and
`eltsets` from the `regions`; and finally, by modifying `itetclr` and `imt`
through the defined `psets` and `eltsets`.

This process is replicated here. First, by creating the `surfaces`:

```
surface / s_1_fm / reflect / sheet / mosurf1_fminus
surface / s_2_fm / reflect / sheet / mosurf2_fminus
surface / s_1_fp / reflect / sheet / mosurf1_fplus
surface / s_2_fp / reflect / sheet / mosurf2_fplus
surface / s_f    / reflect / sheet / mosurf_fault
```

Then, by mapping the surfaces to regions (and deleting the planar meshes to
free up memory):

```
region / r_1_fm / le s_1_fm and               le s_f
region / r_2_fm / gt s_1_fm and le s_2_fm and le s_f
region / r_3_fm / gt s_2_fm and               le s_f
region / r_1_fp / le s_1_fp and               gt s_f
region / r_2_fp / gt s_1_fp and le s_2_fp and gt s_f
region / r_3_fp / gt s_2_fp and               gt s_f

cmo / delete / mosurf1_fminus
cmo / delete / mosurf2_fminus
cmo / delete / mosurf1_fplus
cmo / delete / mosurf2_fplus
cmo / delete / mosurf_fault
```

And finally, by creating `psets` and `eltsets` and using them to modify material
attributes of the parent mesh:

```
pset   / p_r_1_fm / region / r_1_fm / 1 0 0
pset   / p_r_2_fm / region / r_2_fm / 1 0 0
pset   / p_r_3_fm / region / r_3_fm / 1 0 0
pset   / p_r_1_fp / region / r_1_fp / 1 0 0
pset   / p_r_2_fp / region / r_2_fp / 1 0 0
pset   / p_r_3_fp / region / r_3_fp / 1 0 0

eltset / e_r_1_fm / region / r_1_fm
eltset / e_r_2_fm / region / r_2_fm
eltset / e_r_3_fm / region / r_3_fm
eltset / e_r_1_fp / region / r_1_fp
eltset / e_r_2_fp / region / r_2_fp
eltset / e_r_3_fp / region / r_3_fp

cmo / setatt / MONAME / imt / 1 0 0 / 7
cmo / setatt / MONAME / itetclr / 1 0 0 / 7

cmo / setatt / MONAME / imt     / pset   get p_r_1_fm / 1
cmo / setatt / MONAME / imt     / pset   get p_r_2_fm / 2
cmo / setatt / MONAME / imt     / pset   get p_r_3_fm / 3
cmo / setatt / MONAME / imt     / pset   get p_r_1_fp / 4
cmo / setatt / MONAME / imt     / pset   get p_r_2_fp / 5
cmo / setatt / MONAME / imt     / pset   get p_r_3_fp / 6

cmo / setatt / MONAME / itetclr / eltset get e_r_1_fm / 1
cmo / setatt / MONAME / itetclr / eltset get e_r_2_fm / 2
cmo / setatt / MONAME / itetclr / eltset get e_r_3_fm / 3
cmo / setatt / MONAME / itetclr / eltset get e_r_1_fp / 4
cmo / setatt / MONAME / itetclr / eltset get e_r_2_fp / 5
cmo / setatt / MONAME / itetclr / eltset get e_r_3_fp / 6
```

The six distinct regions can now be seen by viewing itetclr on the
parent mesh:

![](Tutorial_Hex_Mesh/images/05_hex_01_fault_imt_itetclr.png)