*dk,fnd2to3i
      subroutine fnd2to3i(it1,ipos1,it2,it3,ibdytet,id,jd,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C
C ######################################################################
C
C      PURPOSE -
C
C         This routine first takes two tets into three tets:
C            (i1,i2,i3,i4) - (j1,j2,j3,xx)
C            (i1,i3,i2,i5) - (k1,k2,k3,xx)
C            -----------------------------------------
C            (i1,i2,i5,i4) - { (2,it2),(1,it3),j3,k2 }
C            (i2,i3,i5,i4) - { (2,it3),(1,it1),j1,k1 }
C            (i3,i1,i5,i4) - { (2,it1),(1,it2),j2,k3 }
C
C        It then flips a connection on the material interface.  This is
C        accomplished by first changing the original boundary connection
C        into an interior connection.  The new connection defined by
C        i4 and i5 then becomes the boundary connection.  The tetra-
C        hedron defined by "ibdytet" is changed to the material type
C        across the interface, and the jtet boundary pointers are reset
C        to indicate the new shape of the interface.
C
C      INPUT ARGUMENTS -
C
C         it1     - the first tet
C         ipos1   - the position of the common face in it1
C         ibdytet - indicates which of the three new tets is on the
C                    boundary
C         it2     - the second tet
C
C      OUTPUT ARGUMENTS -
C
C         it3     - the new tet number(=ntets+1)
C         id      - the "itet" values of the three new tets
C         jd      - the "jtet" values of the three new tets
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/fnd2to3i.f_a  $
CPVCS    
CPVCS       Rev 1.7   30 Sep 2004 10:01:42   dcg
CPVCS    use iand in place of .and. for integer variables
CPVCS    
CPVCS       Rev 1.6   Tue Sep 22 11:48:20 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.5   Thu Jul 03 15:34:36 1997   dcg
CPVCS    comment out call to rwdmpw
CPVCS
CPVCS       Rev 1.4   Thu Apr 17 16:15:58 1997   dcg
CPVCS    check for matching materials for nodes on
CPVCS    elements to be flipped
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:00   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:04   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:52:48   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:13:56   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      parameter (nmulti = 200)
      dimension ichain1(nmulti),imt1a(nmulti),id(12),jd(12)
C
C ######################################################################
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      ipos2=iand((jtet(ipos1,it1)-1),maskface)+1
C
      i1=itet(iflist(3*ipos1-2),it1)
      i3=itet(iflist(3*ipos1-1),it1)
      i2=itet(iflist(3*ipos1  ),it1)
      i4=itet(ipos1,it1)
      i5=itet(ipos2,it2)
      j1=jtet(iflist(3*ipos1-2),it1)
      j3=jtet(iflist(3*ipos1-1),it1)
      j2=jtet(iflist(3*ipos1  ),it1)
      l1=iflist(3*ipos2-2)
      l2=iflist(3*ipos2-1)
      l3=iflist(3*ipos2)
                             k1=jtet(l1,it2)
      if(itet(l2,it2).eq.i1) k1=jtet(l2,it2)
      if(itet(l3,it2).eq.i1) k1=jtet(l3,it2)
                             k2=jtet(l1,it2)
      if(itet(l2,it2).eq.i3) k2=jtet(l2,it2)
      if(itet(l3,it2).eq.i3) k2=jtet(l3,it2)
                             k3=jtet(l1,it2)
      if(itet(l2,it2).eq.i2) k3=jtet(l2,it2)
      if(itet(l3,it2).eq.i2) k3=jtet(l3,it2)
C
C     ******************************************************************
C     MAKE THE THREE NEW TETRAHEDRON ASSIGNMENTS
C
      it3=ntets+1
C
      id(1)=i1
      id(2)=i2
      id(3)=i5
      id(4)=i4
      jd(1)=4*(it2-1)+2
      jd(2)=4*(it3-1)+1
      jd(3)=j3
      jd(4)=k2
C
      id(5)=i2
      id(6)=i3
      id(7)=i5
      id(8)=i4
      jd(5)=4*(it3-1)+2
      jd(6)=4*(it1-1)+1
      jd(7)=j1
      jd(8)=k1
C
      id(9) =i3
      id(10)=i1
      id(11)=i5
      id(12)=i4
      jd(9) =4*(it1-1)+2
      jd(10)=4*(it2-1)+1
      jd(11)=j2
      jd(12)=k3
C
C     ******************************************************************
C
C     FLIP THE BOUNDARY CONNECTION BY CHANGING THE "ibdytet" TO THE
C     MATERIAL ACROSS THE INTERFACE.  REASSIGN THE CORRECT CHILD
C     POINTS FOR THE NEW MATERIAL TYPE.
C
      indx=4*(ibdytet-1)
      jtemp=jd(indx+3)-mbndry
      if (jtemp .le. 0) call termcode(1)
      imtx = imt1(itet1(jtemp))
      do 250 k=1,4
         kndx=indx+k
         kpt=id(kndx)
         call getchain(kpt,ichain1,imt1a,nmulti,ict,ipar)
         if (ict .eq. 0) then
            if(imt1a(1).ne.imtx) then
               write(logdan,260) it1,it2,it3,kpt
 260           format ('in fnd2to3i for tets ',3i8,
     *               ' point ',i10, ' not interface point')
               call writloga('default',0,logdan,0,icscode)
               call termcode(1)
            endif
         else
            do  m=1,ict
               if (imt1a(m) .eq. imtx) then
                 id(kndx)=ichain1(m)
                 goto 250
               endif
            enddo
C
C   If can't find matching material nodes use tet color
C
            write(logdan,243) it1,it2,it3
 243        format ('material mismatch fnd2to3i - tets ',
     *            3i10)
            call writloga('default',0,logdan,0,icscode)
            go to 9999
         endif
 250  continue
C
C     ******************************************************************
C
C     RESET THE "jtet" INTERFACE POINTERS TO REFLECT THE NEW POSITION
C     OF THE INTERFACE.
C
      jd(indx+1)=jd(indx+1)+mbndry
      jd(indx+2)=jd(indx+2)+mbndry
      jd(indx+3)=jd(indx+3)-mbndry
      jd(indx+4)=jd(indx+4)-mbndry
      if (ibdytet .eq. 1) then
         n1=6
         n2=9
      elseif (ibdytet .eq. 2) then
         n1=1
         n2=10
      else
         n1=2
         n2=5
      endif
      jd(n1)=jd(n1)+mbndry
      jd(n2)=jd(n2)+mbndry
C
C     ******************************************************************
C
C     DEBUG SECTION.
C
      if(idebug.gt.0) then
         ict123=0
         ict45=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            if(isum1.eq.0.and.isum2.eq.0.and.isum3.eq.0) ict123=ict123+1
 100     continue
         do 110 it=1,ntets
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum4.eq.0.and.isum5.eq.0) ict45=ict45+1
 110     continue
         if(ict123.ne.2) then
            write(logdan,9020) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2,i3
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("fnd2to3i - FACE EXISTS MORE THAN TWICE: it1=",i10,
     *             " it2=",i10)
 9030       format("                face:     ",i10,"     ",i10,
     *             "     ",i10)
c            call rwdmpw
         endif
         if(ict45.ne.0) then
            write(logdan,9000) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i4,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("fnd2to3i - LINE EXISTS: it1=",i10," it2=",i10)
 9010       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
      endif
      goto 9999
 9999 continue
      return
      end
