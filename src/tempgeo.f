C#####################################################################
C
C      FILE -
C
C     Source code for geological applications using x3dgen
C     Written by EES5 grid team members and students
C     Many of these routines may be obsolete or nearly so as
C     they are replaced
C
C      CHANGE HISTORY -
C
C
C#####################################################################
 
C
      subroutine reset_imt(imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C
C#####################################################################
C
C      PURPOSE -
C
C     Reset node colors based on element colors. Loop through
C     element colors istart to iend and assign all nodes of a
C     element to the element color.
C
C     The default is to loop through all element colors.
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
c        T. Cherry    - added imat and screen output
C        Carl Gable   - initial version
C
C        $Log: tempgeo.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#####################################################################
C
c      implicit real*8 (a-h, o-z)
      implicit none
C
c      character*132 logmess
C
C####################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include "chydro.h"
      include "local_element.h"
c
      pointer (ipimt1, imt1)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet1, itet1)
      integer imt1(1000000)
      integer itet1(10000000)
      integer itetclr(1000000), itettyp(1000000), itetoff(1000000)
 
      integer ilen, itype, ier, lenimt1, lenitetclr, lenitetoff
      integer lenitet1, lenitettyp
      integer istart, iend, iinc
      integer i, it, in, ic, ics
      integer nnodes, numtet, mbndry
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
      character*32 isubname, cmonam
      character*132 logmess
      integer imat(1000)
      integer nmat, maxmat
      data maxmat /1000/
C
C
C#####################################################################
C
C
      isubname='reset_imt'
      nmat=0
c
      call cmo_get_name(cmonam,ier)
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      call cmo_get_info('nelements',cmonam,numtet,ilen,itype,ier)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,itype,ier)
C
      call cmo_get_info('imt1',cmonam,ipimt1,lenimt1,itype,ier)
      call cmo_get_info('itet',cmonam,ipitet1,lenitet1,itype,ier)
      call cmo_get_info('itettyp',cmonam,ipitettyp,lenitettyp,itype,ier)
      call cmo_get_info('itetclr',cmonam,ipitetclr,lenitetclr,itype,ier)
      call cmo_get_info('itetoff',cmonam,ipitetoff,lenitetoff,itype,ier)
C
c
c     quick fix to reassign nodes based on element color
c
c     Loop through all colors (max - min)
c     Loop through all elements
c     Reset all nodes of all elements based on element color
c
C READ PARSER VALUES
      if (nwds .eq. 1)then
c
          iinc   = 1
          istart = 1
          iend   = 0
 
          do it = 1, numtet
            istart = min(istart, itetclr(it))
            iend   = max(iend  , itetclr(it))
          enddo
c      use usr input
       elseif (nwds .eq. 3)then
         istart = imsgin(2)
         iend   = imsgin(3)
 
       elseif (nwds .eq. 4)then
         istart = imsgin(2)
         iend   = imsgin(3)
         iinc   = imsgin(4)
       else
         goto 9999
      endif
c
c INITIALIZE imat
      do i = 1,maxmat
        imat(i)=0
      enddo
c
      do ic = istart, iend, iinc
      do it = 1, numtet
        if(itetclr(it) .eq. ic)then
           do in = 1, nelmnef(itettyp(it))
                  imt1(itet1(itetoff(it)+in))=itetclr(it)
                  imat(itetclr(it)) = 1
           enddo
        endif
      enddo
      enddo
      do i = 1,maxmat
        if (imat(i) .ne. 0) nmat=nmat+1
      enddo
c
9999  write(logmess,'(i12,a)') nmat,' total materials reset.'
      call writloga('default',0,logmess,0,ics)
c
      return
      end
C
      subroutine setvels
      real*8 vels
      pointer(ipvel,vels(3,100))
      character*32 cmo,cname,cvel,sbname,defname
      pointer(ipout,out)
      real*8 out(*),rout
      integer iout,ilen,ityp
      call cmo_get_name(cmo,ier)
      cname='vels'
      call cmo_get_info(cname,cmo,ipvel,lenvel,itvel,ierror)
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierror)
      sbname='sbcmoprm'
      defname='default'
      call cmo_get_attinfo('velname',cmo,iout,rout,cvel,ipout,
     *    ilen,ityp,ier)
      print *,cvel
      do i=1,npoints
        do j=1,3
        vels(j,i)=i
      enddo
      enddo
      return
      end
C
C
      subroutine attrib
      character*32 cmo,name
      cmo='3dmesh'
      call cmo_get_info('number_of_attributes',cmo,num,len,itp,icscode)
      print *,num
      call cmo_get_attribute_name(cmo,10,name,icscode)
      print *,name
      call cmo_get_attribute_name(cmo,40,name,icscode)
      print *,name
      call cmo_get_attribute_name(cmo,39,name,icscode)
      print *,name
      return
      end
 
C
 
