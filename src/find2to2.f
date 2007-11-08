*dk,find2to2
      subroutine find2to2(it1,it2,i1,i2,i3,i4,i5,id,jd,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        This routine finds connection flips on boundaries. This routine
C        closely parallels the routine "find4to4".
C
C        i1-i2   - IS THE OLD CONNECTION (EXISTS ONLY ONCE).
C        i4-i5   - IS THE NEW CONNECTION (CAN'T EXIST BEFORE THIS FLIP).
C
C     INPUT ARGUMENTS -
C
C        it1     - the first tet
C        it2     - the second tet
C        i1-i5   - the five point numbers
C
C     OUTPUT ARGUMENTS -
C
C        id      - the "itet" coordinates of the two new tets
C        jd      - the "jtet" coordinates of the two new tets
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/find2to2.f_a  $
CPVCS    
CPVCS       Rev 1.6   Tue Sep 22 11:48:08 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.5   Thu Jul 03 15:34:08 1997   dcg
CPVCS    comment out call to rwdmpw
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:47:56 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:04:54   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:34   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:50:40   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:00   pvcs
CPVCS    Original version.
C
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      dimension id(8),jd(8),ipos(2,2)
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
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
      endif
C
C     ******************************************************************
C
C
      do 10 i=1,4
         if(itet(i,it1).eq.i1) ipos(1,1)=i
         if(itet(i,it2).eq.i1) ipos(1,2)=i
         if(itet(i,it1).eq.i2) ipos(2,1)=i
         if(itet(i,it2).eq.i2) ipos(2,2)=i
 10   continue
      id(1)=i1
      id(2)=i5
      id(3)=i3
      id(4)=i4
      id(5)=i2
      id(6)=i3
      id(7)=i5
      id(8)=i4
      jd(1)=4*(it2-1)+1
      jd(2)=jtet(ipos(2,1),it1)
      jd(3)=mbndry
      jd(4)=jtet(ipos(2,2),it2)
      jd(5)=4*(it1-1)+1
      jd(6)=mbndry
      jd(7)=jtet(ipos(1,1),it1)
      jd(8)=jtet(ipos(1,2),it2)
C
      if(idebug.gt.0) then
         ict12=0
         ict45=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            if(isum1.eq.0.and.isum2.eq.0) ict12=ict12+1
 100     continue
         do 110 it=1,ntets
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum4.eq.0.and.isum5.eq.0) ict45=ict45+1
 110     continue
         if(ict12.ne.2) then
            write(logdan,9020) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("find2to2 - LINE EXISTS MORE THAN TWICE: it1=",i10,
     *             " it2=",i10)
 9030       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
         if(ict45.ne.0) then
            write(logdan,9000) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i4,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("find2to2 - LINE EXISTS: it1=",i10," it2=",i10)
 9010       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
      endif
      goto 9999
 9999 continue
      return
      end
