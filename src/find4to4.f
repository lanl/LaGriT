*dk,find4to4
      subroutine find4to4(it1,iepos,it2,it3,it4,i1,i2,i3,i4,i5,i6,
     *                    iopt,id,jd,id2,jd2,
     *                    npoints,ntets,ierflg)
C
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C      PURPOSE -
C
C         This routine finds the two 4-to-4 configurations corresponding
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         iepos - the position of the initial common edge (1 - 6)
C         it2   - the tet number of the second member of the quartet
C         it3   - the tet number of the third member of the quartet
C         it4   - the tet number of the fourth member of the quartet
C         i1-i6 - the six point numbers
C         iopt  - the configuration option:
C                    0 => find both configurations
C                    1 => find the i3-i5 (common edge) configuration
C                    2 => find the i4-i6 (common edge) configuration
C
C      OUTPUT ARGUMENTS -
C
C         id    - the "itet" coordinates of the first four new tets
C         jd    - the "jtet" coordinates of the first four new tets
C         id2   - the "itet" coordinates of the second four new tets
C         jd2   - the "jtet" coordinates of the second four new tets
C         ierflg- return flag 0=> flip okay, 1=> not okay
c
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/find4to4.f_a  $
CPVCS    
CPVCS       Rev 1.8   18 Jun 2001 11:03:20   kuprat
CPVCS    Made sure expensive diagnostic requires IDEBUG>=10.
CPVCS    
CPVCS       Rev 1.7   Tue Sep 22 11:48:14 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.6   Wed Jul 09 09:12:18 1997   dcg
CPVCS    make error checking more rigorous
CPVCS    add better diagnostics if idebug set
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:48:06 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:02   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:44   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:00   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:14   pvcs
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
      dimension id(16),jd(16),id2(16),jd2(16),ipos(2,4)
C
C ######################################################################
C
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
C
      ipos(1,1)=ielist(4*(iepos-1)+1)
      ipos(2,1)=ielist(4*(iepos-1)+2)
      do 10 i=1,4
         if(itet(i,it2).eq.i1) ipos(1,2)=i
         if(itet(i,it3).eq.i1) ipos(1,3)=i
         if(itet(i,it4).eq.i1) ipos(1,4)=i
         if(itet(i,it2).eq.i2) ipos(2,2)=i
         if(itet(i,it3).eq.i2) ipos(2,3)=i
         if(itet(i,it4).eq.i2) ipos(2,4)=i
 10   continue
C
      if(iopt.eq.0.or.iopt.eq.1) then
         id(1)=i1
         id(2)=i5
         id(3)=i3
         id(4)=i4
         id(5)=i2
         id(6)=i3
         id(7)=i5
         id(8)=i4
         id(9)=i2
         id(10)=i5
         id(11)=i3
         id(12)=i6
         id(13)=i1
         id(14)=i3
         id(15)=i5
         id(16)=i6
C
         jd(1) =4*(it2-1)+1
         jd(2) =jtet(ipos(2,1),it1)
         jd(3) =jtet(ipos(2,2),it2)
         jd(4) =4*(it4-1)+4
         jd(5) =4*(it1-1)+1
         jd(6) =jtet(ipos(1,2),it2)
         jd(7) =jtet(ipos(1,1),it1)
         jd(8) =4*(it3-1)+4
         jd(9) =4*(it4-1)+1
         jd(10)=jtet(ipos(1,3),it3)
         jd(11)=jtet(ipos(1,4),it4)
         jd(12)=4*(it2-1)+4
         jd(13)=4*(it3-1)+1
         jd(14)=jtet(ipos(2,4),it4)
         jd(15)=jtet(ipos(2,3),it3)
         jd(16)=4*(it1-1)+4
      endif
      if(iopt.eq.0.or.iopt.eq.2) then
         id2(1)=i1
         id2(2)=i6
         id2(3)=i3
         id2(4)=i4
         id2(5)=i2
         id2(6)=i6
         id2(7)=i5
         id2(8)=i4
         id2(9)=i2
         id2(10)=i4
         id2(11)=i3
         id2(12)=i6
         id2(13)=i1
         id2(14)=i4
         id2(15)=i5
         id2(16)=i6
C
         jd2(1) =4*(it3-1)+1
         jd2(2) =jtet(ipos(2,1),it1)
         jd2(3) =4*(it4-1)+3
         jd2(4) =jtet(ipos(2,3),it3)
         jd2(5) =4*(it4-1)+1
         jd2(6) =jtet(ipos(1,2),it2)
         jd2(7) =4*(it3-1)+3
         jd2(8) =jtet(ipos(1,4),it4)
         jd2(9) =4*(it1-1)+1
         jd2(10)=jtet(ipos(1,3),it3)
         jd2(11)=4*(it2-1)+3
         jd2(12)=jtet(ipos(1,1),it1)
         jd2(13)=4*(it2-1)+1
         jd2(14)=jtet(ipos(2,4),it4)
         jd2(15)=4*(it1-1)+3
         jd2(16)=jtet(ipos(2,2),it2)
      endif
C
c.... This diagnostic is so expensive, we require idebug>=10.
      if(idebug.ge.10) then
         ict12=0
         ict35=0
         ict46=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            if(isum1.eq.0.and.isum2.eq.0.and.itet(1,it).gt.0)
     *         ict12=ict12+1
 100     continue
         do 110 it=1,ntets
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum3.eq.0.and.isum5.eq.0.and.itet(1,it).gt.0)
     *         ict35=ict35+1
 110     continue
         do 120 it=1,ntets
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum6=(itet(1,it)-i6)*(itet(2,it)-i6)*
     *            (itet(3,it)-i6)*(itet(4,it)-i6)
            if(isum4.eq.0.and.isum6.eq.0.and.itet(1,it).gt.0)
     *         ict46=ict46+1
 120     continue
         if(ict12.ne.4) then
            write(logdan,9020) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("find4to4 - LINE EXISTS MORE THAN FOUR TIMES:",
     *             " it1=",i10," it2=",i10," it3=",i10," it4=",i10)
 9030       format("          connection:     ",i10,"     ",i10)
            ierflg=1
c            call rwdmpw
         endif
         if((iopt.eq.0.or.iopt.eq.1).and.ict35.ne.0) then
            write(logdan,9000) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i3,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("find4to4 - LINE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10," it4=",i10)
 9010       format("          connection:     ",i10,"     ",i10)
            ierflg=1
c            call rwdmpw
         endif
         if((iopt.eq.0.or.iopt.eq.2).and.ict46.ne.0) then
            write(logdan,9040) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9050) i4,i6
            call writloga("default",0,logdan,0,ierrwrt)
 9040       format("find4to4 - LINE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10," it4=",i10)
 9050       format("          connection:     ",i10,"     ",i10)
            ierflg=1
c            call rwdmpw
         endif
      endif
      goto 9999
 9999 continue
      return
      end
