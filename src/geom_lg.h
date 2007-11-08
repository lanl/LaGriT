C ######################################################################
C
C     PURPOSE -
C
C       Declarations for geometry information
C
C     CHANGE HISTORY -
C
C  $Log: geom_lg.h,v $
C  Revision 2.00  2007/11/05 19:45:56  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Thu Feb 03 13:28:44 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.4   Tue Oct 26 14:07:52 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Thu Jan 21 20:56:28 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS    
CPVCS       Rev 1.2   Tue Mar 17 12:59:30 1998   dcg
CPVCS    add comments for integer*8 declarations needed for
CPVCS    64 bit address version
CPVCS
CPVCS       Rev 1.1   Wed Nov 26 11:05:00 1997   dcg
CPVCS    change to 4 byte integers
CPVCS
CPVCS       Rev 1.0   Mon Nov 24 14:46:50 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C  region information
C  cregs contains names of regions
C  ndefregs contains the length of the region definition
C  offregs is the offset into the regdef array of the region
C  definition
C  regdef contains all the region definitions
      pointer (ipcregs,cregs)
      character*32 cregs(*)
      pointer (ipndefregs,ndefregs)
      integer ndefregs(*)
      pointer (ipoffregdef,offregdef)
      integer offregdef(*)
      pointer (ipregdef,regdef)
      character*32 regdef(*)
C  mregion information
C  cmregs contains names of mregions
C  nmdefregs contains the length of the mregion definition
C  matregs contains the material number for this mregion
C  moffregs is the offset into the mregdef array of the mregion
C  definition
C  mregdef contains all the mregion definitions
      pointer (ipcmregs,cmregs)
      character*32 cmregs(*)
      pointer (ipndefmregs,ndefmregs)
      integer ndefmregs(*)
      pointer (ipmatregs,matregs)
      integer matregs(*)
      pointer (ipoffmregdef,offmregdef)
      integer offmregdef(*)
      pointer (ipmregdef,mregdef)
      character*32 mregdef(*)
C  surface information
C  csall contains names of surfaces
C  istype contains surface type
C  ibtype contains boundary type
C  sheetnm contains the name of the sheet cmo for sheet surfaces
c  or the geometry type for tabular surfaces
C  surfparam contains the surface definition parameter for non-sheet surfaces
C  offsparam contains the offset to surfparam for each surface
      pointer (ipcsall,csall)
      character*32 csall(*)
      pointer (ipistype,istype)
      character*32 istype(*)
      pointer (ipibtype,ibtype)
      character*32 ibtype(*)
      pointer (ipsheetnm,sheetnm)
      character*32 sheetnm(*)
      pointer (ipsurfparam,surfparam)
      real*8 surfparam(*)
      pointer (ipoffsparam,offsparam)
      integer offsparam(*)
c
c  names and information for geometries
c  geom_info has number of surfaces(nsurf), number of regions(nregs),
c  number of mregions(nmregs), length of longest region definition(maxdef),
c  length of longest mregion definition(maxmdef), last word in region
c  definition array(lasregdef), 
c  last word in mregion definition array(lastmregdef),
c  last word in surface parameter array(lastparam)
c
      pointer(ipgeom_names,geom_names)
      character*32 geom_names(*)
      pointer (ipgeom_info,geom_info)
      integer geom_info(8,*)
c
C  number of surfaces and number of regions - these are saved in
C  geom_info_lg for all geometries - active values are in common
c
      integer nsurf,nregs,nmregs,maxdef,maxmdef,lastregdef,lastmregdef,
     *   lastsparam,number_of_geometries
      common /geom/ nsurf, nregs, nmregs,maxdef,maxmdef
     *   ,lastregdef,lastmregdef, lastsparam,number_of_geometries
      common /cgeom/ current_geom_name
      character*32  current_geom_name
      save /geom/ 
      save /cgeom/
C
C ######################################################################
