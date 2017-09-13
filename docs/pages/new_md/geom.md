---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 8
---

 ** Geometry Information**

  
 Much geometry information is maintained in the **geom** common block
 which is defined in the include file **geom\_lg.h.**
   ------------- ------------------------------------------------------------
   nsurf         is the number of surfaces
   nregs         is the number of regions
   nmregs        is the number of mregions
   maxdef        is the number of tokens in the longest region definition
   maxmdef       is the number of tokens in the longest mregion definition.
   lastregdef    end of regdef array
   lastmregdef   end of regmdef array
   lastsparam    end of sparam array
   ------------- ------------------------------------------------------------

  Geometry information is contained in a set of memory managed arrays
  whose partition name is the geometry name.
 
  Retrieve the geometry name by:

             call cmo\_get\_attinfo('geom\_name, cmo\_name, iout,
  rout, **geom\_name**, ipout, itype,ierror)
 
  Surface information includes:

   
 
    ----------- ---------------------------------------------------------------
    csall       surface name
    istype      surface type (e.g. 'plane', cylinder, .....)
    ibtype      boundary type (e.g. 'reflect;, 'interface', ...)
    sheetnm     sheetname (for sheet surface types only)
    surfparam   data describing the surface, varies depending on surface type
    offsparam   offset into surfparam for a given surface
    ----------- ---------------------------------------------------------------
 
  Region/mregion information includes:
 
    ------------------------ -----------------------------------------------------------
    cregs (cmregs)           region (mregions) names
    ndetregs(ndefmregs)      number of tokens in region(mregion) definition
    regdef (mregdef)         region (mregion) definition
    offregdef (offmregdef)   offset into regdef (mregdef) for a given region (mregion)
    matregs                  material number fo a material region
    ------------------------ -----------------------------------------------------------
 
  subroutine get\_material\_number(mregion\_name,number,ierror)

           returns the material number associated with the material
  region called mregion\_name.
 
  Any of the memory managed geometry arrays can be accessed using
  [mmfindbk,](memmang.md) for example:

            character
*32 pointer (ipcsall, csall)

            call mmfindbk('csall', geom\_name,ipcsall,length,ierror)

            where geom\_name has been retrieved from the mesh object
  using cmo\_get\_attinfo.

 
