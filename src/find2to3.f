*dk,find2to3
      subroutine find2to3(it1,ipos1,it2,ipos2,it3,id,jd,
     *                    npoints,ntets,ierflg)
C
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C      PURPOSE -
C
C         This routine takes two tets into three tets:
C            (i1,i2,i3,i4) - (j1,j2,j3,xx)
C            (i1,i3,i2,i5) - (k1,k2,k3,xx)
C            -----------------------------------------
C            (i1,i2,i5,i4) - { (2,it2),(1,it3),j3,k2 }
C            (i2,i3,i5,i4) - { (2,it3),(1,it1),j1,k1 }
C            (i3,i1,i5,i4) - { (2,it1),(1,it2),j2,k3 }
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         ipos1 - the position of the common face in it1
C
C      OUTPUT ARGUMENTS -
C
C         it2   - the second tet
C         ipos2 - the position of the common face in it2
C         it3   - the new tet number(=ntets+1)
C         id    - the "itet" values of the three new tets
C         jd    - the "jtet" values of the three new tets
c         ierflg - error return (0 flip okay - 1 flip bad)ifalg
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/find2to3.f_a  $
CPVCS    
CPVCS       Rev 1.9   15 Nov 2004 10:31:12   dcg
CPVCS    replace .and. with function iand
CPVCS
CPVCS       Rev 1.8   05 Jan 2001 12:56:02   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.7   Tue Sep 22 11:48:10 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.6   Wed Jul 09 09:11:14 1997   dcg
CPVCS    make error checking more rigorous
CPVCS    add better diagnostics if idebug set
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:48:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:04:58   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:36   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:50:44   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:04   pvcs
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
      dimension id(12),jd(12)
C
C ######################################################################
C
C
C     ******************************************************************
      ierflg=0
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      it2=0.25*dble(jtet(ipos1,it1))+0.9
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
            if(isum1.eq.0.and.isum2.eq.0.and.isum3.eq.0.and.
     *          itet(1,it).gt.0) ict123=ict123+1
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum4.eq.0.and.isum5.eq.0.and.itet(1,it).gt.0)
     *         ict45=ict45+1
 100     continue
         if(ict123.ne.2) then
            write(logdan,9020) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            do it=1,ntets
               isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
               isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
               isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
               if(isum1.eq.0.and.isum2.eq.0.and.isum3.eq.0) then
                  write(logdan,9001) i1,i2,i3,it
                  call writloga("default",0,logdan,0,ierrwrt)
               endif
            enddo
 9001       format(' face ',3i10,' in tet ',i10)
 9020       format('find2to3 - FACE EXISTS MORE THAN TWICE: it1=',i10,
     *             ' it2=',i10)
 
c            call rwdmpw
            ierflg=1
         endif
         if(ict45.ne.0) then
            write(logdan,9000) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            do it=1,ntets
               isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
               isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
               if(isum4.eq.0.and.isum5.eq.0) then
                  write(logdan,9010) i4,i5,it
                  call writloga("default",0,logdan,0,ierrwrt)
               endif
            enddo
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format('find2to3 - LINE EXISTS: it1=',i10,' it2=',i10)
 9010       format('          connection:     ',2i10,' in tet ',i10)
c            call rwdmpw
            ierflg=1
         endif
      endif
      goto 9999
 9999 continue
      return
      end
