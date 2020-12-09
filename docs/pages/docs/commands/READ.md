---
title: READ
---
 
# READ

This command reads data or a mesh into the active Mesh Object.
  AVS, LaGriT, and GMV formats are supported.  The other formats may
  be used, but no guarantees are made about their capabilities.

 
  **[read/ avs](../read_avs.md)** read AVS UCD format files

  **[read/ lagrit](../read_lagrit.md)** read lagrit restart file with mesh object defined

  **[read/ gmv](../read_gmv.md)** read General Mesh Viewer GMV format files

  **[read/ gmvfreeformat](../read_freeformat.md)**  ascii gmv files to be read with read(*)  

  **[read/ sheetij](../read_sheetij.md)** read elevations into quad surface

  **[cmo / readatt/...](cmo/cmo_readatt.md)** read tabular data spreadsheet style into node attributes 

  **[read/ zone or zonn or zone_element](../read_fehm_zone.md)** read node numbers from zone or zonn file

  **[read/ gocad](../read_gocad.md)** read GOCAD file, single instance of mesh only

  **[read/ iges_grid](../read_iges_grid.md)** IGES file with surfaces, curves, points in NURBS format 

  **[read/ ngp](../read_ngp.md)** 

  **[read/ vrml](../read_vrml.md)**

  **[read/ datex](../read_datex.md)**



## EXAMPLES

```
read / avs / myfile / mesh_object_name

read / myfile.inp / mesh_object_name
    
read / mesh.gmv / mesh_object_name

```
  
Short form syntax does not require file type as the second token. This is supported for the suffixes **`avs inp gmv lg lagrit`**
  
See links to various formats for more detailed explanations and examples.
