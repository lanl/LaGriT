---
title: HFIELD
tags: hfield, poisson_disk, variable resolution, h field, spacing
---

# HFIELD

----------------------

**hfield** builds a named description of the spacing h, the length the
edges of a mesh should have, from a few *sources*: things the mesh
should be fine near, or inside. [createpts / poisson_disk](createpts/createpts_poisson.md)
takes the name (`hfield, hf_name`) where it takes a control file, so a
variable resolution mesh needs no control file. **hfield / eval**
writes h at the vertices of any mesh object, to check a field or to
use it for something else.

## SYNTAX

<pre>
<b>hfield / create</b> / hf_name / <b>h_floor</b>, F / <b>h_ceiling</b>, C [/ <b>h_growth</b>, G]

<b>hfield / add</b> / hf_name / <b>near</b>, mo_src [/ <b>h_value</b>, H | <b>h_attribute</b>, ATT [/ <b>h_factor</b>, A]] [/ <b>h_band</b>, B] [/ <b>h_growth</b>, G]

<b>hfield / add</b> / hf_name / <b>inside</b>, <b>surface</b> / mo_surf [/ <b>h_value</b>, H] [/ <b>h_band</b>, B] [/ <b>h_growth</b>, G]

<b>hfield / list</b> / hf_name

<b>hfield / eval</b> / hf_name / mo / att

<b>hfield / dump</b> / hf_name / file.inp [/ n]

<b>hfield / delete</b> / hf_name
</pre>

Commas and slashes both separate words, so `h_floor, 0.05` is the
keyword and its value. Keywords after the source can come in any order.
A number can be a `define` name. An hfield name has at most 28
characters; names like `hf_dfn` (as `mo_dfn` for mesh objects) keep
them apart from mesh objects.

## THE RULE

For each source i, with d the distance from a point to that source:

```
h_i = H_i + h_growth_i * max(0, d - h_band_i)
h   = min(h_ceiling, max(h_floor, the smallest h_i))
```

- **H_i** is the spacing at the source: **h_value**, or **h_factor**
  times the source's vertex attribute **h_attribute** at its vertex
  nearest the point, or, with neither, **h_floor**.
- **near:** d is the distance to the source: to its triangles and
  lines, or its quads, when it has cells (the exact distance of
  [compute / distance_field / ... / facet](COMPUTE.md)), and to its
  vertices when it has none.
- **inside, surface:** d is 0 inside the surface and the distance to
  it outside, so the whole inside gets H and h grows away from the
  boundary.
- **h_band** keeps h = H out to that distance from the source.
- **h_growth** is how fast h grows with distance (dh/dx, no units).
  [createpts / poisson_disk](createpts/createpts_poisson.md) makes good
  triangles and tets with 0.2 to 0.3.
- Far from every source h is **h_ceiling**. With no source at all, h is
  h_ceiling everywhere.

Every h_i grows by at most its h_growth per unit of distance, so h does
too: the field cannot jump, whatever the sources.

| keyword | what | default |
|---|---|---|
| **h_floor** | the smallest h anywhere | required |
| **h_ceiling** | the largest h, and h far from every source | required |
| **h_growth** | dh/dx away from a source; on create, the default for every source; on add, for that source only | 0.2 |
| **h_value** | H, a number | h_floor |
| **h_attribute** | H from a VDOUBLE vertex attribute of the source (for example [edge_lengths_mean](cmo/cmo_addatt.md)) | |
| **h_factor** | multiplies h_attribute | 1.0 |
| **h_band** | the distance out to which h stays H | 0 |

## ACTIONS

**create** makes an empty hfield. **add** adds one source; each add is
one more h_i, and the smallest wins.

**near**, mo_src: any mesh object with vertices: a set of points, a
line mesh, a triangle surface or a DFN (triangles and lines in any
mix), or quads only.

**inside, surface**, mo_surf: in 3D a closed (watertight) surface of
triangles, or of quads (each cut into 4 triangles at its center); in 2D
a closed loop of lines, or a mesh object of vertices only, taken in
order as a closed polygon (as for `createpts / poisson_disk /
2d_polygon`). Closed means every edge is shared by exactly two cells
(every vertex of a loop is on exactly two lines). The cells need not
be oriented the same way: hfield orients them first, then decides
inside from the winding number, with the even-odd rule, so a hollow
shape (a surface inside a surface) has its hole outside.

**list** writes the rule in words, one line per source.

**eval** writes h at every vertex of mo into att, a VDOUBLE vertex
attribute (made if it is not there).

**dump** writes h on a lattice of n x n x n vertices (n x n when the
sources are flat in z), n 21 by default, over the bounding box of the
sources' vertices, as an AVS file with the attribute **h_field**, for
ParaView.

**delete** removes the hfield.

The sources are read when h is evaluated, so a source changed after
add is used as it is then.

## ERRORS

Each of these is an ERROR, and nothing is changed:

- h_floor not greater than 0, h_ceiling less than h_floor, h_growth,
  h_value or h_factor not greater than 0, h_band negative;
- create without h_floor or h_ceiling, or of a name that exists;
- an hfield name not made with create;
- a source mesh object that does not exist or has no vertices;
- near: a source with cells other than triangles and lines, or quads
  only;
- h_attribute that is not a VDOUBLE scalar vertex attribute of the
  source; h_value with h_attribute; h_factor without h_attribute;
  h_attribute with inside;
- inside, surface: a surface that is not closed, cannot be oriented
  (a one-sided surface), or mixes cell types; a mesh object with no
  cells and fewer than 3 vertices;
- eval: att exists and is not a VDOUBLE scalar vertex attribute.

## WITH CREATEPTS / POISSON_DISK

```
createpts / poisson_disk / 3d_box / mo_pts / 0.05 / connect / hfield, hf_dfn
```

`hfield, hf_name` goes where the control file goes; give one or the
other. It works in 2D and 3D, with every h_lookup and face_lattice
option, exactly as a control file that sets the same h. When h_floor
is below h_spacing_scalar, LaGriT writes a WARNING and uses
h_spacing_scalar as the floor.

## EXAMPLES

A DFN: tets next to the fractures as long as the fractures' own edges,
growing away from them:

```
cmo / addatt / mo_dfn / edge_lengths_mean / h_dfn
hfield / create / hf_dfn / h_floor, 0.05 / h_ceiling, 1.0 / h_growth, 0.2
hfield / add / hf_dfn / near, mo_dfn / h_attribute, h_dfn / h_factor, 0.74 / h_band, 0.15
hfield / add / hf_dfn / near, mo_well / h_value, 0.02
hfield / add / hf_dfn / inside, surface / mo_lens / h_value, 0.1
hfield / list / hf_dfn
createpts / poisson_disk / 3d_box / mo_pts / 0.05 / connect / hfield, hf_dfn
```

The first line gives each DFN vertex the mean length of its edges; the
first source makes h near the fractures 0.74 times that length, the
same out to 0.15 from them, then growing by 0.2 per unit of distance up
to 1.0. The well gets 0.02, and the whole lens 0.1.

A control file that grows h with the distance from a line, in an
hfield. The control file:

```
cmo / create / mo_dfield
createpts / line / 400 / / / 0.5 0.5 POI_ZMIN / 0.5 0.5 POI_ZMAX / 1 1 1 /
compute / distance_field / mo_poi_h_field / mo_dfield / dfield
math / multiply / mo_poi_h_field / h_field_att / 1,0,0 / mo_poi_h_field / dfield / SLOPE /
math / add / mo_poi_h_field / h_field_att / 1,0,0 / mo_poi_h_field / h_field_att / INTERCEPT /
math / floor / mo_poi_h_field / h_field_att / 1 0 0 / mo_poi_h_field / h_field_att / H_FLOOR
math / ceiling / mo_poi_h_field / h_field_att / 1 0 0 / mo_poi_h_field / h_field_att / H_CEILING
cmo / delete / mo_dfield
```

The same h as an hfield, made once before createpts:

```
cmo / create / mo_axis
createpts / line / 400 / / / 0.5 0.5 0. / 0.5 0.5 1. / 1 1 1 /
hfield / create / hf_axis / h_floor, H_FLOOR / h_ceiling, H_CEILING / h_growth, SLOPE
hfield / add / hf_axis / near, mo_axis / h_value, INTERCEPT
createpts / poisson_disk / 3d_box / mo_pts / H_FLOOR / connect / hfield, hf_axis
```

To look at a field before meshing:

```
hfield / dump / hf_dfn / hf_dfn.inp / 41
```

A field the rule cannot express still needs a control file.

Test: test/suite/test_hfield (the rule checked at query vertices, and
the same mesh from a control file and from an hfield).
