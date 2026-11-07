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
  
The file is given by its name, without a path: LaGriT runs in one folder, and a command is split into words at each `/`, so `read / avs / ../dir/file.inp / mo` would give read the file name `..`. A file name that is a folder, or a command with more words than the format takes (avs 7, lagrit 5, gmv 5, gocad 4), is an ERROR and nothing is read. Copy or link the file into the working folder. See [Syntax Conventions](../conventions.md).

Short form syntax does not require file type as the second token. This is supported for the suffixes **`avs inp gmv lg lagrit`**
  
See links to various formats for more detailed explanations and examples.
