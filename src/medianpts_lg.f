      subroutine medianpts_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
 
C#######################################################################
C
C     PURPOSE -
C       This routine is used to add median node coordinates
c       to the mesh.  Three new attributes are added xmed,ymed,zmed
c       which contain the x,y,z coordinates of the median nodes.
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
C       createpts / median
C

C        $Log:   /pvcs.config/t3d/src/medianpts_lg.f_a  $
CPVCS    
CPVCS       Rev 1.0   01 Nov 2002 13:05:22   gable
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
      pointer (ipxmed, xmed)
      pointer (ipymed, ymed)
      pointer (ipzmed, zmed)
      real*8 xmed(*), ymed(*), zmed(*),r
      character*32 cmo,isubname
      integer ierr,icscode,nelements,ilen,ityp,it,ioff
      integer inode, knode
      real*8 xave,yave,zave,xsum,ysum,zsum
 
C#######################################################################
C get some initial info
 
      isubname='medianpts_lg'
 
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
      call cmo_get_info('xmed',cmo,ipxmed,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//xmed/VDOUBLE/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('xmed',cmo,ipxmed,ilen,ityp,ierr)
      endif
      call cmo_get_info('ymed',cmo,ipymed,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//ymed/VDOUBLE/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('ymed',cmo,ipymed,ilen,ityp,ierr)
      endif
      call cmo_get_info('zmed',cmo,ipzmed,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//zmed/VDOUBLE/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('zmed',cmo,ipzmed,ilen,ityp,ierr)
      endif

c
c  calculate voronoi points
c
       do it=1,nelements
          ioff=itetoff(it)
          ityp=itettyp(it)
          xsum = 0.0
          ysum = 0.0
          zsum = 0.0
          do inode = 1, nelmnen(ityp)
             knode = itet(ioff + inode)
             xsum = xsum + xic(knode)
             ysum = ysum + yic(knode)
             zsum = zsum + zic(knode)
          enddo
          xave = xsum/float(nelmnen(ityp))
          yave = ysum/float(nelmnen(ityp))
          zave = zsum/float(nelmnen(ityp))
          xmed(it) = xave
          ymed(it) = yave
          zmed(it) = zave
       enddo

 9999 continue
      return
      end

 
 
