*dk,rankvolume
      subroutine rankvolume(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        RANKVOLUME prints data on the smallest volume elements
C        (smallest 100 by default).
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
C        $Log: rankvolume.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   30 Jan 2001 08:12:42   dcg
CPVCS    change format to allow for larger element numbers
CPVCS
CPVCS       Rev 1.0   Tue Dec 02 19:28:58 1997   kuprat
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.0   Tue Jun 03 12:44:34 1997   kuprat
CPVCS    Initial revision.
C
C #####################################################################
c
c   RANKVOLUME prints out the lowest volume elements from a mesh, ranked
c   in increasing order.  The default is to print out the 100 lowest
c   volume elements, but this number can be changed by specifying it as an
c   optional second argument to the command.
c   Also printed are the contents of the ITET and JTET arrays
c   corresponding to these elements.
c
c   FORMAT:
c
c      RANKVOLUME / number to rank
c
C #####################################################################
 
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
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      integer itet(lenptr),itetoff(lenptr),jtet(lenptr),
     &   jtetoff(lenptr),itettyp(lenptr)
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)
 
      pointer (ipvol,vol),(ipieltlist,ieltlist)
      real*8 vol(lenptr)
      integer ieltlist(lenptr)
 
      real*8 xicvol(maxnen),yicvol(maxnen),zicvol(maxnen)
C
      character*32 cmo,isubname
      integer length,icmotype,icscode,nnodes,i,j,nelements,mbndry,
     &   nrank,ierr,ielt,nbf,nif
      real*8 ascend
 
      isubname = 'rankvolume'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.
 
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'RANKVOLUME: ',cmo,' not a valid mesh object'
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
         call cmo_get_info('mbndry',cmo,
     *      mbndry,length,icmotype,icscode)
 
      if (nwds.ge.2) then
         call test_argument_type(1,1,2,imsgin,xmsgin,cmsgin,msgtype,
     *      nwds)
         nrank=min(nelements,imsgin(2))
      else
         nrank=min(nelements,100)
      endif
      call mmgetblk('ieltlist',isubname,ipieltlist,nelements,1,icscode)
      call mmgetblk('vol',isubname,ipvol,nelements,2,icscode)
      do i=1,nelements
         do j=1,nelmnen(itettyp(i))
            xicvol(j)=xic(itet(itetoff(i)+j))
            yicvol(j)=yic(itet(itetoff(i)+j))
            zicvol(j)=zic(itet(itetoff(i)+j))
         enddo
         call volume_element(itettyp(i),xicvol,yicvol,zicvol,vol(i))
         ieltlist(i)=i
      enddo
 
      ascend=1.
      call hpsort1(nelements,vol,ascend,ieltlist)
 
      write(logmess,9000)
 9000 format('elt. no.       volume     ',
     &   '#ext.bound.faces #int.bound.faces')
      call writloga('default',0,logmess,0,ierr)
 
      do i=1,nrank
         ielt=ieltlist(i)
         nbf=0
         nif=0
         do j=1,nelmnef(itettyp(ielt))
            if (jtet(jtetoff(ielt)+j).eq.mbndry) then
               nbf=nbf+1
            elseif (jtet(jtetoff(ielt)+j).gt.mbndry) then
               nif=nif+1
            endif
         enddo
         write(logmess,9010) ielt,vol(ielt),nbf,nif
 9010    format(i10,2x,e12.6,2x,i5,6x,i5)
         call writloga('default',0,logmess,0,ierr)
      enddo
 
 9999 continue
      call mmrelprt(isubname,icscode)
 
      return
      end
 
