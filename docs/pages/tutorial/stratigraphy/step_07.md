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