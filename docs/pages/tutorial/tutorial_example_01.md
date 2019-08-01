---
title: "LaGriT Tutorial #10"
---

<link href="https://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css" rel="stylesheet">

<div class="lgtutorial" style="display: flex;">
    <div style="flex: 1;">
        <!-- Title -->
        <h1>LaGriT Tutorial #10</h1>
        <h2>Mesh basics, materials, and regions</h2>
        
        <!-- Subtitle -->
        <p>
            Welcome to the first tutorial on LaGriT mesh generation.
            Lorem ipsum dolor sit amet, consecetur adipsicing elit. Nullam ut lobortis erat.
            Nunc egestas id ante scelerisque vehicula.
        </p>
        
        <!-- Date, difficulty, etc. -->
        <div class="about1">
            <div class="about2">
                <div>
                    <span class="about">
                        Oct 3 2018 &middot; Beginner &middot; 30 mins
                    </span>
                </div>
            </div>
        </div>

        <!-- Download Materials button -->
        <a href="https://lanl.github.io/LaGriT/pages/docs/lagrit_input_2dconnect" download="lg_tutorial_10.lgi" class="lgtutorial btn left">
            <span class="left icon icon-download"></span>
            <span class="right title"><span class="arrow-right"></span>Download (2 KB)</span>
        </a>

        <!-- Requirements list -->
        <div style="display: block; margin-top: 24px; box-sizing: inherit;">
            <h4 style="font-size: 17px;">Requires:</h4>
            <div style="margin-top: 5px;">
                  <input type="checkbox" name="checkbox" id="checkbox" />
                  <label for="checkbox">LaGriT 3.3+</label>
                  <br>
                  <input type="checkbox" name="checkbox" id="checkbox" />
                  <label for="checkbox">Exodus 7.0+</label>
            </div>
        </div>
    </div>

    <!-- Example image -->
    <div style="width: 50%;">
        <figure>
            <img alt="" src="http://dev-g.lanl.gov/orgs/ees/lagrit/_assets/img/graphic-3.png" width="400px" style="margin: 10px 20px 0 20px;">
        </figure>
    </div>
</div>


-----------------------

# Creating a Basic Stratigraphic Mesh

In this tutorial, we are going to ____.

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/19_hex_01_to_tet.png)

## 1. Building a Hex Mesh

First, we are going to construct a structured hex mesh. The hex mesh will span
from 0 meters to 4000 m, 4000 m, and 3000 m, for the x/y/z coordinates
respectively.

For both consistency and rapid parameter manipulation, these can be stored in
variables. In LaGriT, variables are assigned using the `define` keyword. 

```
define / X0 / 0.
define / X1 / 4000.
define / Y0 / 0.
define / Y1 / 4000.
define / Z0 / 0.
define / Z1 / 3000.

define / NX / 51
define / NY / 51
define / NZ / 26

define / MONAME / mohex
```

Above, the spatial domain (`X0,X1,Y0,...`), element density (`NX/NY/NZ`), and
mesh name (`MONAME`) have been defined.

Next, we will create an empty mesh object, with element type `hex`, using the
[`cmo / create`](null.md) command:

```
cmo / create / MONAME / / / hex
```

Due to the variable assignment of `MONAME <- mohex` above, this command is
translated internally as:

```
cmo / create / mohex / / / hex
```

This empty object can then be populated with nodes and elements. 
The [`createpts / brick`](null.md) command will generate a defined number of
hex elements across a defined domain. 

```
createpts / brick / xyz / NX NY NZ / X0 Y0 Z0 / X1 Y1 Z1 / 1 1 1
```

`NX` number of hex elements, along with their corresponding vertices, have been
created in the spatial domain spanning `X0->X1`, along with `NY` elements in
the Y domain and `NZ` elements in the Z domain.

Optionally, [save the mesh object](null.md):

```
dump / avs / tmp_hex_01.inp / MONAME
```

This file can be rendered in certain scientific 3D visualization applications,
such as [ParaView](https://www.paraview.org).

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/01_hex_01.png)

### Full script:

```
define / X0 / 0.
define / X1 / 4000.
define / Y0 / 0.
define / Y1 / 4000.
define / Z0 / 0.
define / Z1 / 3000.

define / NX / 51
define / NY / 51
define / NZ / 26

define / MONAME / mohex

cmo / create / MONAME / / / hex
createpts / brick / xyz / NX NY NZ / X0 Y0 Z0 / X1 Y1 Z1 / 1 1 1
cmo / status / brief
quality
finish
```

## 2. Define Boundaries Using Point Sets 

In LaGriT, a pset (or *point-set*) is a collection of points (nodes) within a 
mesh object. Similarly, an eltset (*element-set*) is a collection of mesh
elements. By capturing points and elements, discrete manipulations can be
performed on them, such as translation, removal, or attribute functions. 

In this example, point sets are used to create a boundary on the top surface of 
the created hex mesh. The boundary will be the intersection of three circles on
the top layer of the mesh.

```
# Set vertices (imt) and cells (itetlcr) to 1
cmo / setatt / mohex / imt / 1 0 0 / 1
cmo / setatt / mohex / itetclr / 1 0 0 / 1

resetpts / itp
```

Capturing a `pset` is done through the command

```
pset / pset_name / select_type / select_type_options
```

where `pset_name` is an arbitrary variable name to store the pset into, 
`select_type` is the method of pset selection, and `select_type_options` are
parameters specific to the chosen `select_type` for configuring the subset
selection (see the [documentation]() for more information).

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/02_hex_01_top_region.png)

### 2.1 PSet Definitions

As the boundary will live only in the top layer, nodes belonging to the top 
layer will be identified first.

```
pset / p_top / attribute / zic / 1 0 0 / ge / Z1
```

Here, a pset named `p_top` is created. This pset contains all nodes 
(stride = `1 0 0`) where the node's Z value (`zic`) is greater than or equal to
(`ge`) the top of the mesh (`Z1`). Remember that we defined `Z1` above for the
initial creation of the mesh - and that by simply changing `Z1` and re-running
the script, this pset capture will still be valid for any value of `Z1`
(where `Z1 > Z0`).

Now that the top is defined, we will move to defining three cylindrical
objects.

This is done through the command

```
pset / pset_name / geom / rtz / ifirst,ilast,istride / r1,t1,z1 / r2,t2,z2 / xcen,ycen,zcen
```

which forms a pset of nodes within the cylinder or cylindrical shell given by 
radius r1 to r2, angle theta t1 to t2 and height z1 to z2.

```
pset / p_circle1 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 1500. 1500. 0.
pset / p_circle2 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 2500. 2500. 0.
pset / p_circle3 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 2500. 1500. 0.
```

Above, any points within a full circle (theta = `360`) of radius `1100.` and 
within `Z={-1.0,1.e4}` are captured. Three different cylinders are created,
where only the centroids change.

Finally, all four psets are intersected such that all points belonging to the
union of all given sets are preserved into the point seet `p_region`:

```
pset / p_region / inter / p_top p_circle1 p_circle2 p_circle3
```

This creates a point-set where all points (i) live on the top layer, and (ii)
live within the intersection of the three cylinders.

### 2.2 Map PSets to an Attribute

As a simple sanity check during meshing, it can be helpful to map mesh
operations to an attribute. These can be visualized at intermediate steps in
the meshing process to provide a form of verification or debugging.

First, create an attribute using the `cmo / addatt` command:

```
cmo / addatt / MONAME / id_top_region / vint / scalar / nnodes
```

Here, an attribute named `id_top_region` is created within the mesh `MONAME`,
and has type integer (`vint`), is of scalar dimensions, and is a node-based
attribute (`nnodes`).

The attribute can be progressively filled in with different values based on
the psets:

```
# Fill the entire attribute with 1
cmo / setatt / MONAME / id_top_region / 1 0 0 / 1

# Color all nodes in the pset p_circle1 with the value 2
cmo / setatt / MONAME / id_top_region / pset get p_circle1 / 2

# Color all nodes in the pset p_circle2 with the value 2
cmo / setatt / MONAME / id_top_region / pset get p_circle2 / 3

# And so on...
cmo / setatt / MONAME / id_top_region / pset get p_circle3 / 4
cmo / setatt / MONAME / id_top_region / pset get p_region / 5
```

Cutting through the mesh, the defined psets and their intersections become
obvious:

![test.png](Cut plane mesh)

Finally, release the psets from memory:

```
pset / p_top / release
pset / p_circle1 / release
pset / p_circle2 / release
pset / p_circle3 / release
pset / p_region / release
```

### Full script:

```
#
# Identify a region on the top surface that is the intersection of two circular regions
# Defined previously: MONAME Z1
#
pset / p_top / attribute / zic / 1 0 0 / ge / Z1
pset / p_circle1 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 1500. 1500. 0.
pset / p_circle2 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 2500. 2500. 0.
pset / p_circle3 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 2500. 1500. 0.

pset / p_region / inter / p_top p_circle1 p_circle2 p_circle3
#
# Create a new integer attribute and fill values using the various pset's
#
cmo / addatt / MONAME / id_top_region / vint / scalar / nnodes
cmo / setatt / MONAME / id_top_region / 1 0 0 / 1
cmo / setatt / MONAME / id_top_region / pset get p_circle1 / 2
cmo / setatt / MONAME / id_top_region / pset get p_circle2 / 3
cmo / setatt / MONAME / id_top_region / pset get p_circle3 / 4
cmo / setatt / MONAME / id_top_region / pset get p_region / 5
#
# Output a 'zone' file with a list of the vertex numbers in the top region.
#
pset / p_region / zone / tmp_zone_top_region.zone
#
# Get rid of pset's that are not needed any more.
#
pset / p_top / release
pset / p_circle1 / release
pset / p_circle2 / release
pset / p_circle3 / release
pset / p_region / release
```

## 3. Constructing Stratigraphy

In the next step of this tutorial, we will build some surfaces to define
stratigraphy.
In a real model, the surfaces would come from some geologic framework model
and would define geologic or hydro-geologic horizons and topography.

These surfaces will be planar quad meshes that cut through a defined section of
the hex mesh. Later, we will map the intersections of the surfaces to the hex
mesh.

#### Create the top surface:

```
cmo / create / mosurf1
cmo / select / mosurf1

define / X0S /  -20.0
define / X1S / 4020.0
define / Y0S /  -20.0
define / Y1S / 4020.0

define / Z1 / 1000.
define / Z2 / 1500.
define / Z3 / 2500.
define / Z4 /  500.

quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
```

Note that the X and Y domains of the quad mesh exceed that of the hex mesh.
This serves two purposes. First, it serves as a helpful visualization aid,
allowing one to easily see how the surfaces cut the hex mesh without adjusting
opacity. Second, and more importantly, it ensures that all elements cut by the
surfaces will be properly recognized as such. Rounding errors may affect
elements at the perimeter of the cutting planes from being properly labeled.

#### Create the bottom surface:

```
cmo / create / mosurf2
cmo / select / mosurf2

define / Z1 / 1800.
define / Z2 / 2100.
define / Z3 / 2800.
define / Z4 /  800.
quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
```

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/03_hex_01_set_imt_itetclr.png)

### Full Script

```
#
# Create surfaces
#
cmo / create / mosurf1
cmo / create / mosurf2
#
# For visualization, make the surface a bit bigger than the 0-4000m of the mesh.
#
define / X0S /  -20.0
define / X1S / 4020.0
define / Y0S /  -20.0
define / Y1S / 4020.0
cmo / select / mosurf1
#
define / Z1 / 1000.
define / Z2 / 1500.
define / Z3 / 2500.
define / Z4 /  500.
quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
cmo / printatt / mosurf / -xyz- / minmax

cmo / select / mosurf2
#
define / Z1 / 1800.
define / Z2 / 2100.
define / Z3 / 2800.
define / Z4 /  800.
quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
cmo / printatt / mosurf / -xyz- / minmax
```

## 4. Map Surfaces to Mesh

Now that the straitipgrahy has been modeled and we are comfortable with our
results, we will map their spanning domain to the parent mesh. As done with the
psets above, this process will be driven via attributes.

We now have two planes spanning the X,Y domain of the mesh. These planes can be
leveraged to create different material IDs at different regions of the
subsurface.

For example,

* For all nodes/cells above plane 1, set their material ID to 1
* For all nodes/cells between plane 1 and plane 2, set their material ID to 2
* For all nodes/cells below plane 2, set their material ID to 3

This process can be accomplished by:

1. Defining the above regions using the `region` keyword
2. Capturing the relevant nodes and elements that fall within the defined regions
3. Setting the node and element material IDs based on the `psets` and `eltsets`

### 4.1 Defining Regions

The syntax for `region` is:

     region / region_name / region_definition

where `region_definition` is a string composed of boolean operators and 
instantiated `surface` objects.

Recall that we have defined two planes, `mosurf1` and `mosurf2`. We would like
to generate the `region` objects from these planes, but the arguments for
`region` require `surface` objects.

Fortunately, we can map the planes to a `surface` very easily. The syntax for
generating a `surface` object from a quad or triangle mesh is:

     surface / surface_name / reflect / sheet / input_mesh

The two planes can then be mapped to surface objects:

```
surface / s_1 / reflect / sheet / mosurf1
surface / s_2 / reflect / sheet / mosurf2
```

And finally, we can remove the plane meshes and define regions:

```
cmo / delete / mosurf1
cmo / delete / mosurf2
cmo / select / MONAME

region / r_1 / le s_1
region / r_2 / gt s_1 and le s_2
region / r_3 / gt s_2
```

### 4.2 Creating Eltsets and PSets from Regions

Point sets and element sets can easily be created through region objects.
The syntax is:

     pset / pset_name / region / region_object / 1,0,0
     eltset / eltset_name / region / region_object

Applying this to our `region` objects yields:

```
pset   / p_r_1 / region / r_1 / 1 0 0
pset   / p_r_2 / region / r_2 / 1 0 0
pset   / p_r_3 / region / r_3 / 1 0 0

eltset / e_r_1 / region / r_1
eltset / e_r_2 / region / r_2
eltset / e_r_3 / region / r_3
```

### 4.3 Setting Attributes from Eltsets and PSets

Recall that the node attribute `imt` holds the 'node colors' of the mesh, and
cell attribute `itetclr` stores the 'cell colors' (or material ID).

Let's use the defined `pset`s and `eltset`s to change these:

```
cmo / setatt / MONAME / imt     / pset   get p_r_1 / 1
cmo / setatt / MONAME / imt     / pset   get p_r_2 / 2
cmo / setatt / MONAME / imt     / pset   get p_r_3 / 3

cmo / setatt / MONAME / itetclr / eltset get e_r_1 / 1
cmo / setatt / MONAME / itetclr / eltset get e_r_2 / 2
cmo / setatt / MONAME / itetclr / eltset get e_r_3 / 3
```

Our mesh's cells and nodes now store information about their intersections
with the cut-planes. Visualizing `itetclr`, we can see that this has behaved
as expected:

![](images/ch4.png)

### Full Script

```
#
# Set Materials for node imt and element itetclr
# Previously defined: MONAME mosurf1 mosurf2
#
cmo / select / MONAME
#
surface / s_1 / reflect / sheet / mosurf1
surface / s_2 / reflect / sheet / mosurf2
#
cmo / delete / mosurf1
cmo / delete / mosurf2
cmo / select / MONAME
#
region / r_1 / le s_1
region / r_2 / gt s_1 and le s_2
region / r_3 / gt s_2
#
pset   / p_r_1 / region / r_1 / 1 0 0
pset   / p_r_2 / region / r_2 / 1 0 0
pset   / p_r_3 / region / r_3 / 1 0 0
#
eltset / e_r_1 / region / r_1
eltset / e_r_2 / region / r_2
eltset / e_r_3 / region / r_3
#
cmo / setatt / MONAME / imt     / pset   get p_r_1 / 1
cmo / setatt / MONAME / imt     / pset   get p_r_2 / 2
cmo / setatt / MONAME / imt     / pset   get p_r_3 / 3
#
cmo / setatt / MONAME / itetclr / eltset get e_r_1 / 1
cmo / setatt / MONAME / itetclr / eltset get e_r_2 / 2
cmo / setatt / MONAME / itetclr / eltset get e_r_3 / 3
#
finish
```

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

## 6. Truncate with Polyline

We now have a mesh with complex stratigraphy encoded in its material ID. 
However, the domain of this mesh is a rather boring cuboid and doesn't
accurately reflect the geospatial domain that we are trying to model.

By importing a polyline, the exterior boundary of the mesh can be truncated.
As in previous steps, we will use a mesh to define a `region` that will be used
for element-wise operations.

However, the boundary is a line object. In order to use it as a surface/region,
it must be a surface. A polyline can be turned into a vertical surface by
'extruding' it in the vertical (0,0,1) direction:

```
read / avs / basin_bnd_ply_rescale.inp / mo_bndry
extrude / mo_fence / mo_bndry / const / 3200. / volume / 0. 0. -1.
```

We will also translate the extrusion so that it covers the vertical extent
of the hex mesh:

```
trans / 1 0 0 / 0. 0. -3200. / 0. 0. 0.
```

Next, we use the boundary to truncate (remove) cells outside the boundary.

There are three ways to define 'outside':

1. Only remove a cell if ALL vertices are outside
2. Remove a cell if the centroid (average of all vertices) is outside
3. Remove a cell if one or more vertices are outside

```
cmo / select / MONAME
surface / s_bndry / reflect / sheet / mo_fence
cmo / select / MONAME
region / r_bndry / ge s_bndry
pset / p_bndry / region r_bndry
```
**Method 1:**

    eltset / e_delete1 / exclusive / pset get p_bndry

**Method 2:**

    eltset / e_delete2 / region r_bndry

**Method 3:**

    eltset / e_delete3 / inclusive / pset get p_bndry

## TODO: add text here

```
cmo / addatt / MONAME / id_in_out_bndry / vint / scalar / nelements
cmo / setatt / MONAME / id_in_out_bndry / 1 0 0 / 4
cmo / setatt / MONAME / id_in_out_bndry / eltset get e_delete3 / 3
cmo / setatt / MONAME / id_in_out_bndry / eltset get e_delete2 / 2
cmo / setatt / MONAME / id_in_out_bndry / eltset get e_delete1 / 1
eltset / e_delete4 /    id_in_out_bndry / eq / 4
eltset / e_delete3 /    id_in_out_bndry / eq / 3
eltset / e_delete2 /    id_in_out_bndry / eq / 2
eltset / e_delete1 /    id_in_out_bndry / eq / 1

rmpoint / element / eltset get e_delete4
rmpoint / element / eltset get e_delete3
rmpoint / compress
resetpts / itp
```

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/06_boundary_truncate.png)

## 7. Refine Fault

In step 5, we defined a fault surface (named `s_f`) intersecting the mesh. In 
this section, we are going to refine the mesh where the fault intersects it.

The LaGriT command `intersect_elements` takes two meshes and creates an
element-based attribute in mesh1 that contains the number of elements in mesh2
that intersected the respective element in mesh1.

Performing the intersection between the mesh `MONAME` and fault `s_f`, storing
the intersection count in attribute `if_inter`:

```
cmo / select / MONAME
intersect_elements / MONAME / s_f / if_inter
```

The attribute `if_inter` will be non-zero everywhere there is an intersection,
and zero where there was not intersection. Taking advantage of this fact,
we can create an eltset to refine:

```
eltset / e_refine / if_inter / gt / 0
refine / eltset / eltset get e_refine
cmo / DELATT / MONAME / if_inter
```

### TODO: add 'repeat this process'

## 8. Insert Well

## 9. Convert Hex Mesh to Tet

![](/Users/livingston/playground/tutorial/Tutorial_Hex_Mesh/images/19_hex_01_to_tet.png)