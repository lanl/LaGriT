---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
---

 **read/sheetij**

  ** **creates a quad mesh from an elevation file using syntax similar
  to avs type .fld header file.

 **FORMAT:**

  **read****/sheetij**/file\_name/-header-/[options]/nx,ny/minx,miny/xinc,yinc/[istart,istop,jstart,jstop]/[options]

  **read****/sheetij**/file\_name/-header-/[options]

   
    --------------------------- ------------------------------------------------------
    nx                          columns in x direction
    ny                          rows in y direction
    min, miny                   location of lower left corner
    xinc, yinc                  cell size in x and y direction
    - header -                  keyword meaning read nx, ny,miny,xinc,yinc,from file
    istart,istop,jstart,jstop   gives indices of subset of data 
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

   for data file with header - test\_hdr.dat:

           
#  5     nx

           
#  4     ny

           
#         0.0000000   min x

           
#         0.0000000   min y

           
#         5.000000    dx

           
#         5.000000    dy

           
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

   

 
