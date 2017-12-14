---
title: READ
tags: ok
---
 
**READ**

  This command reads data or a mesh into the active Mesh Object.

 **FORMAT:**

  AVS, LaGriT, and GMV formats are supported.  The other formats may
  be used, but no guarantees are made about their capabilities. goCad
  format is supported only for reading TSURF files.

 
  **[read/ avs](../read_avs.md)**

  **[read/ lagrit](../read_lagrit.md)**

  **[read/ gmv](../read_gmv.md)**

  **[read/ gocad](../read_gocad.md)**

  **[read/ iges\_grid](../read_iges_grid.md)**

  **[read/ ngp](../read_ngp.md)**

  **[read/ vrml](../read_vrml.md)**

  **[read/ datex](../read_datex.md)**

  **[read/ sheetij](../read_sheetij.md)**

  **[read/ gmvfreeformat](../read_freeformat.md)**

  **[read/ zone|zonn](../read_fehm_zone.md)**
 
  Note: To read tabular data (spreadsheet style x,y,z nodes or
  attributes) see: [cmo/readatt/...](cmo/cmo_readatt.md)

 **EXAMPLES:**

```
read / avs / myfile / mesh_object_name
    
read / mesh.gmv / mesh_object_name

```
  
  Short form syntax does not require file type as the second token.
  This is supported for the suffixes avs, inp, gmv, lg, and lagrit.
  
  See links to various formats for more detailed explanations and
  examples.
