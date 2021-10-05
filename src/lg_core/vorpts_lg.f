c
      subroutine vorpts_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
 
C#######################################################################
C
C     PURPOSE -
C       This routine is used to add voronoi nodes and their coordinates
c       to the mesh.  Three new attributes are added xvor,yvor,zvor
c       which contain the x,y,z coordinates of the voronoi nodes.
c       These attributes are of type vdouble and length nelements and
c       rank scalar.
C
C     INPUT ARGUMENTS -
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C
C     FORMAT -
C
C       createpts/vorornoi/ 
C

C        $Log: vorpts_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   02 Nov 2000 10:06:58   dcg
CPVCS    Initial revision.
C
C#######################################################################
c ........................................................................
 
      implicit none
      include 'local_element.h'
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*132 logmess
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
      pointer (ipitettyp,itettyp),(ipitetoff,itetoff),(ipitet,itet)
      integer itettyp(*),itetoff(*),itet(*)
      pointer (ipxvor, xvor)
      pointer (ipyvor, yvor)
      pointer (ipzvor, zvor)
      real*8 xvor(*), yvor(*), zvor(*),r
      character*32 cmo,isubname
      integer ierr,icscode,nelements,ilen,ityp,it,ioff
 
C#######################################################################
C get some initial info
 
      isubname='vorpts_lg'
 
      ierr=0
 
      call cmo_get_name(cmo,ierr)
      if (ierr.ne.0) then
         goto 9999
      endif
 
      call cmo_get_intinfo('nelements',cmo,nelements,ilen,ityp,ierr)
      if (ierr.ne.0.or.nelements.eq.0) then
         goto 9999
      endif
 
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)

c
c  add attributes if don't exist
c
      call cmo_get_info('xvor',cmo,ipxvor,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//xvor/VDOUBLE/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('xvor',cmo,ipxvor,ilen,ityp,ierr)
      endif
      call cmo_get_info('yvor',cmo,ipyvor,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//yvor/VDOUBLE/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('yvor',cmo,ipyvor,ilen,ityp,ierr)
      endif
      call cmo_get_info('zvor',cmo,ipzvor,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//zvor/VDOUBLE/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('zvor',cmo,ipzvor,ilen,ityp,ierr)
      endif
 
c
c  calculate voronoi points
c
       do it=1,nelements
          ioff=itetoff(it)
          ityp=itettyp(it)
          if (ityp.eq.ifelmtet) then
             call voronoi_center(
     *        xic(itet(ioff+1)),yic(itet(ioff+1)),zic(itet(ioff+1)),
     *        xic(itet(ioff+2)),yic(itet(ioff+2)),zic(itet(ioff+2)),
     *        xic(itet(ioff+3)),yic(itet(ioff+3)),zic(itet(ioff+3)),
     *        xic(itet(ioff+4)),yic(itet(ioff+4)),zic(itet(ioff+4)),
     *        xvor(it),yvor(it),zvor(it),r)
          elseif(ityp.eq.ifelmtri) then
             call voronoi_center_2d(
     *        xic(itet(ioff+1)),yic(itet(ioff+1)),zic(itet(ioff+1)),
     *        xic(itet(ioff+2)),yic(itet(ioff+2)),zic(itet(ioff+2)),
     *        xic(itet(ioff+3)),yic(itet(ioff+3)),zic(itet(ioff+3)),
     *        xvor(it),yvor(it),zvor(it) )
          else
             logmess='element not triangle or tetrahedron'
             call writloga('default',0,logmess,0,ierr)
             go to 9999
          endif
       enddo
       go to 9998
            
 9999 continue
      write(logmess,*) '  VORPTS ERROR - '
      call writloga('default',0,logmess,0,icscode)
 9998 continue
      return
      end

 
 
