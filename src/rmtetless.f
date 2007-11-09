*dk,rmtetless
      subroutine rmtetless(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        RMTETLESS removes nodes with no attached tetrahedra.
C        
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C     $Log:   /pvcs.config/t3d/src/rmtetless.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Dec 21 13:54:00 1998   kuprat
CPVCS    We now print out number of nodes dudded.
CPVCS    
CPVCS       Rev 1.0   Fri Apr 10 14:46:40 1998   kuprat
CPVCS    Initial revision.
C
C #####################################################################
c
      implicit none

      include 'local_element.h'
C
      integer lenptr
      parameter (lenptr=1000000)

      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror,ierrw
      
      character*132 logmess
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      pointer (ipitettyp,itettyp)
      pointer (ipitp1,itp1)
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      integer itet(lenptr),itetoff(lenptr),jtet(lenptr),
     &   jtetoff(lenptr),itettyp(lenptr),itp1(lenptr)
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)

      pointer (iplhastet,lhastet)
      logical lhastet(lenptr)
      pointer (ipireal1,ireal1)
      integer ireal1(lenptr)

      character*32 cmo,isubname
      integer length,icmotype,icscode,nnodes,i,j,nelements,mbndry,
     &   node,ierrdum,ndud

      isubname = 'rmtetless'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.

      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'RMTETLESS: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      
         call cmo_get_info('nnodes',cmo,
     *      nnodes,length,icmotype,icscode)
         call cmo_get_info('nelements',cmo,
     *      nelements,length,icmotype,icscode)
         call cmo_get_info('itet',cmo,
     *      ipitet,length,icmotype,icscode)
         call cmo_get_info('itetoff',cmo,
     *      ipitetoff,length,icmotype,icscode)
         call cmo_get_info('jtet',cmo,
     *      ipjtet,length,icmotype,icscode)
         call cmo_get_info('jtetoff',cmo,
     *      ipjtetoff,length,icmotype,icscode)
         call cmo_get_info('xic',cmo,
     *      ipxic,length,icmotype,icscode)
         call cmo_get_info('yic',cmo,
     *      ipyic,length,icmotype,icscode)
         call cmo_get_info('zic',cmo,
     *      ipzic,length,icmotype,icscode)
         call cmo_get_info('itettyp',cmo,
     *      ipitettyp,length,icmotype,icscode)
         call cmo_get_info('itp1',cmo,
     *      ipitp1,length,icmotype,icscode)
         call cmo_get_info('mbndry',cmo,
     *      mbndry,length,icmotype,icscode)


c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
      call mmgetblk('ireal1',isubname,ipireal1,nnodes,1,icscode)
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)

      call mmgetblk('lhastet',isubname,iplhastet,nnodes,1,icscode)

      do i=1,nnodes
         lhastet(i)=.false.
      enddo
      do i=1,nelements
         do j=1,nelmnen(itettyp(i))
            node=itet(itetoff(i)+j)
            lhastet(node)=.true.
         enddo
      enddo
      ndud=0
      do i=1,nnodes
         if (ireal1(i).eq.1.and..not.lhastet(i)) then
            ndud=ndud+1
            itp1(i)=21
         endif
      enddo
      if(ndud.ne.0) then
         write(logmess,'(a)')
     *      'RMTETLESS:  dudded ',ndud,' unattached nodes.'
         call writloga('default',0,logmess,0,ierrw)
      endif

 9999 continue 
      call mmrelprt(isubname,icscode)

      return
      end

