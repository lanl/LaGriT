      subroutine dumpstl(ifile,cmonam)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR STL.
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C         $Log: dumpstl.f,v $
C         Revision 2.00  2007/11/05 19:45:53  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   30 Sep 2005 15:40:30   gable
CPVCS    
CPVCS    
CPVCS    Change floating point number formats from 14.6 to 20.12.
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 16:35:32   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.2   Wed Oct 08 16:55:16 1997   dcg
CPVCS    add missing character declaration
CPVCS
CPVCS       Rev 1.1   Thu Aug 21 14:20:28 1997   gable
CPVCS    Initial Revision
CPVCS    Added option for dump / stl which will output a triangular
CPVCS    sheet CMO in aformat appropriate for Stereo Lithography
CPVCS    as done by Pete Smith (ESA-WMM). Code written by
CPVCS    L. Lundquist.
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
      include "local_element.h"
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet1, itet1)
      integer itet1(10000000)
      integer itettyp(1000000), itetoff(1000000)
 
      integer ilen, itype, ier, lenitetoff
      integer lenitet1, lenitettyp
      integer it, in
      integer nnodes, numtet, mbndry
      character*(*) cmonam
      character*32 isubname, title
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      character*132 logmess
      integer iunit, ierror, lenfile
      real*8 xnorm, ynorm, znorm
      real*8 unit_xnorm, unit_ynorm, unit_znorm
      real*8 x(8), y(8), z(8)
      real*8 x12,y12,z12,x13,y13,z13,mag
      integer lenxic, lenyic, lenzic
      character*32 ifile
C
      dimension xic(1000000), yic(1000000), zic(1000000)
C
C
C ######################################################################
C
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C begin
C
      isubname="dumpstl"
C
C
C    Get Mesh Object Information
C
      call cmo_select(cmonam,ier)
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      call cmo_get_info('nelements',cmonam,numtet,ilen,itype,ier)
      call cmo_get_info('itet',cmonam,ipitet1,lenitet1,itype,ier)
      call cmo_get_info('itettyp',cmonam,ipitettyp,lenitettyp,itype,ier)
      call cmo_get_info('itetoff',cmonam,ipitetoff,lenitetoff,itype,ier)
      call cmo_get_info('ndimensions_topo',cmonam,
     *                  nsdtopo1,ilen,itype,ier)
      call cmo_get_info('ndimensions_geom',cmonam,
     *                  nsdgeom1,ilen,itype,ier)
      call cmo_get_info('nodes_per_element',cmonam,
     *                  nen1,ilen,itype,ier)
      call cmo_get_info('xic' ,cmonam,ipxic,lenxic,itype,ier)
      call cmo_get_info('yic' ,cmonam,ipyic,lenyic,itype,ier)
      call cmo_get_info('zic' ,cmonam,ipzic,lenzic,itype,ier)
C
C    Error check: make sure that the incoming mesh object is a 2-d
C      surface in 3-d space.
C
      write (logmess, *) "topo dimensions:", nsdtopo1
      call writloga('default',0,logmess,0,ier)
      write (logmess, *) "geom dimensions:", nsdgeom1
      call writloga('default',0,logmess,0,ier)
      write (logmess, *) "nodes per element:", nen1
      call writloga('default',0,logmess,0,ier)
      if (nsdtopo1.ne.2) then
          write(logmess,'(a)')
     *     'ERROR: Topographical dimensions not equal to 2 '
          call writloga('default',0,logmess,0,ier)
          return
      endif
      if (nsdgeom1.ne.3) then
          write(logmess,'(a)')
     *     'ERROR: Geometrical dimensions not equal to 3 '
          call writloga('default',0,logmess,0,ier)
CC	   write (iunit, *), "nsdgeom1"
	  return
      endif
C
C    Write file header
C
      title = cmonam(1:icharlnf(cmonam))
      iunit = -1
      call hassign(iunit,ifile,ier)
      if (iunit.lt.0 .or. ier.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif

      write(iunit,*) "solid ", title
C
C    Get node information for each element
C
      do it = 1, numtet
          do in = 1, nelmnef(itettyp(it))
              x(in) = xic(itet1(itetoff(it) + in))
              y(in) = yic(itet1(itetoff(it) + in))
              z(in) = zic(itet1(itetoff(it) + in))
          enddo
c
c    Calculate the normal to the two dimensional element.
c
          x12 = x(1)-x(2)
          y12 = y(1)-y(2)
          z12 = z(1)-z(2)
          x13 = x(1)-x(3)
          y13 = y(1)-y(3)
          z13 = z(1)-z(3)
c
c    Take the cross product of xyz12 and xyz13
c
          xnorm = y12*z13 - y13*z12
          ynorm = z12*x13 - z13*x12
          znorm = x12*y13 - x13*y12
c
c    Find the magnitude of the normal.
c
          mag = sqrt((xnorm*xnorm) + (ynorm*ynorm) + (znorm*znorm))
c
c    Find the unit normal.
c
          unit_xnorm = xnorm/mag
          unit_ynorm = ynorm/mag
          unit_znorm = znorm/mag
C
C    Output information in stl format.
C
          write(iunit, 9030) unit_xnorm, unit_ynorm, unit_znorm
 9030     format('  facet normal', 1x, 3(1pe20.12))
          write(iunit, '(a)') "    outer loop"
          do in = 1, nelmnef(itettyp(it))
              write(iunit, 9040) x(in), y(in), z(in)
 9040         format('      vertex', 1x, 3(1pe20.12))
          enddo
          write(iunit, '(a)') "    endloop"
          write(iunit, '(a)') "  endfacet"
      enddo
      write(iunit,*) "endsolid ", title
c
      close(iunit)

 9999 continue
C
C      call mmrelprt(isubname,icscode)
C
      return
      end
