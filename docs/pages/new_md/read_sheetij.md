---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
---

 **read/sheetij**

  ** **creates a quad mesh from an elevation file using syntax similar
  to avs type .fld header file. Note the input file is assumed to
  contain elevation values and are read as Z(i,j) into the cmo
  attribute "zic". For node attribute values of f(i,j), use addatt and
  copyatt as shown in the examples below.

 **FORMAT:**

  

  **read****/sheetij**/ file\_name /nx,ny/minx,miny/dx,dy [options]

  **read****/sheetij**/ file\_name /nx,ny/minx,miny/dx,dy/ skip n /
  [options]

  **read****/sheetij**/ file\_name
  /nx,ny/minx,miny/dx,dy/istart,jstart,istop,jstop/[options]

  **read****/sheetij**/ file\_name/-header-/[options]

   
    --------------------------- ------------------------------------------------------
    nx                          columns in x direction
    ny                          rows in y direction
    min, miny                   location of lower left corner
    xinc, yinc                  cell size in x and y direction
    -header-                    keyword meaning read nx, ny,miny,xinc,yinc,from file
    skip n                      skip n number of header lines
    istart,istop,jstart,jstop   gives indices of ij subset  
    --------------------------- ------------------------------------------------------
 
 options are:

   
    --------------------------- ---------------------------------------------------------------
    **ascii** or **binary**     file-type (default-**ascii**)
    **center**                  assume elevation at cell center (default = lower left corner)
    **connect** or **points**   **connect** causes quad grid to be formed and is default.
                                **points** keeps data as points only.
    **float** or **double**     size of data (default = **float**)
    **xflip** or **yflip**      reflect along x or y axis (default = no reflection) 
    --------------------------- ---------------------------------------------------------------
 
 **EXAMPLES:**

   for data file with header - test\_hdr.dat:          showing nx, ny,
  min x, min y, dx, dy

           
#  5    

           
#  4    

           
#         0.0000000  

           
#         0.0000000  

           
#         5.000000   

           
#         5.000000   

           
#
 
         to read elevations using information in the file header

         cmo create cmohdr

         read sheetij test\_hdr.dat / -header- / ascii
 
         to subset read ascii file with header and subset from i=2,5
  and j=3,4

           read sheetij test\_hdr.dat / -header- / 2,5, 3,4 / ascii
 
         for binary files with no headers where nx=5, ny=4,
  minx=miny=0., xinc=yinc=5.

           read sheetij test.bin /5,4 /0.0,0.0 / 5.0,5.0 / binary
 
         to flip along the x-axis

           read sheetij test.bin /5,4 /0.0,0.0 / 5.0,5.0 / xflip,
  binary
 
         read temperature values from file instead of elevation

          cmo create cmot

          read sheetij temp.dat / 150,183 /1602850.,1727950. / &

          500.0,500.0 / skip 6

          cmo / addatt / cmot / tempval / VDOUBLE / scalar / nnodes

          cmo / copyatt / cmot / cmot / tempval / zic

          cmo / setatt / cmot / zic / 0.



TEST **EXAMPLES:**



Test file for sheetij: [lagrit\_input00](lagrit_input00)

Test data for sheetij: [test\_data](test_data)

![Quad sheet](image/zall_200w.GIF)"200"


* read ascii elevations into quad sheet

cmo create cmo1

read sheetij test\_data /5,4 /0.0,0.0 &

/ 5.0,5.0 / ascii

![Quad sheet with xflip](image/zall_xflip_200w.GIF)"200"


* read ascii elevations and flip x

cmo create cmo1

read sheetij test\_data /5,4 /0.0,0.0 &

/ 5.0,5.0 / xflip, ascii

![Quad sheet with xy flip](image/zall_xyflip_200w.GIF)"200"


* read ascii elevations and flip xy

cmo create cmo1

read sheetij test\_data /5,4 /0.0,0.0 &

/ 5.0,5.0 / xflip, yflip, ascii



Test file for binary sheetij: [lagrit\_input01](lagrit_input01)

Binary test file not included.

![Quad sheet from binary file](image/binsurf_200w.GIF)"200"


* read binary elevations

cmo create cmo1

read sheetij qbog50\_l.float /31,21 &

/ 0.0, 0.0 / 50.0,50.0 / binary / float

![Quad sheet from binary
file](image/view_binsurf_subset_200w.GIF)"200"


* read binary elevations and subset along ij

cmo create cmo1a

read sheetij qbog50\_l.float /31,21/ 0. 0./ 50.0,50.0 &

/10,25 1,10/ binary / float


* combine grids for viewing

math/sub/cmo1a/zic/1,0,0/cmo1a/zic/500.

addmesh merge cmov cmo1 cmo1a

dump gmv view\_binsurf\_subset.gmv cmov
