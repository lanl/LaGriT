*dk,fnd4to4i
      subroutine fnd4to4i(it1,it2,it3,it4,i1,i2,i3,i3b,i4,i5,i6,
     *                    id,jd,
     *                    npoints,ntets)
       implicit none
C
C ######################################################################
C
C      PURPOSE -
C
C         This routine finds the 4-to-4i configuration which flips
C         connections on a material interface.
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         it2   - the tet number of the second member of the quartet
C         it3   - the tet number of the third member of the quartet
C         it4   - the tet number of the fourth member of the quartet
C         i1-i6 - the six point numbers
C         i3b   - a child point corresponding to point i3
C
C      OUTPUT ARGUMENTS -
C
C         id    - the "itet" coordinates of the four new tets
C         jd    - the "jtet" coordinates of the four new tets
C
C      CHANGE HISTORY -
C
C        $Log: fnd4to4i.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.11   Tue Sep 22 11:48:28 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.10   Wed Jun 17 11:56:20 1998   dcg
CPVCS    fix node order so that all tets are oriented with
CPVCS    postive volumes
CPVCS
CPVCS       Rev 1.9   Tue Jun 16 13:26:36 1998   dcg
CPVCS    more changes for ivoronoi = -2
CPVCS
CPVCS       Rev 1.7   Wed Jun 10 16:58:18 1998   dcg
CPVCS    add subroutine to handle other interface orientation
CPVCS
CPVCS       Rev 1.6   Fri Jul 11 09:16:32 1997   dcg
CPVCS    clean up warning messages
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:49:24 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:06   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:12   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:53:10   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:08   pvcs
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
      integer nmulti
      parameter (nmulti = 200)
      integer kpts(3),ichain1(nmulti),imt1a(nmulti),id(16),jd(16),
     *          ipos(2,4)
      integer m,ierror,length,icmotype,kpt,imtx,k,ict,ipar,i0,i1,i2,i3,
     *  i4,i5,i6,i1b,i2b,ict12,isum1,isum2,isum3,npoints,ntets,
     *  it1,it2,it3,it4,ierrwrt,isum6,isum5,isum4,it,ict35,ict46,i5b,
     *  i3b
      integer iposfnd,min0
C
C ######################################################################
C
C     MACROS.
C
      iposfnd(i0,i1,i2,i3,i4)=min0(iabs(i1-i0)*4+1,iabs(i2-i0)*4+2,
     *                             iabs(i3-i0)*4+3,iabs(i4-i0)*4+4)
C
C ######################################################################
C
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
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C     FIND CHILDREN FOR BOUNDARY POINTS.
C
      imtx=imt1(itet(1,it3))
      kpts(1)=i1
      kpts(2)=i2
      kpts(3)=i5
      do 250 k=1,3
         kpt=kpts(k)
         call getchain(kpt,ichain1,imt1a,nmulti,ict,ipar)
         if (ict .eq. 1) then
            if (imt1a(1) .ne. imtx) call termcode
            kpts(k)=ichain1(1)
         else
            do 200 m=1,ict
               if (imt1a(m) .eq. imtx) then
                  kpts(k)=ichain1(m)
                  goto 250
               endif
 200        continue
            call termcode(1)
         endif
 250  continue
      i1b=kpts(1)
      i2b=kpts(2)
      i5b=kpts(3)
C
C     ******************************************************************
C
C     FIND THE POSITIONS IN THE FOUR ORIGINAL TETRAHEDRA WHICH POINT TO
C     OUTSIDE FACES.
C
      ipos(1,1)=iposfnd(i1,itet(1,it1),itet(2,it1),itet(3,it1),
     *                  itet(4,it1))
      ipos(1,2)=iposfnd(i1,itet(1,it2),itet(2,it2),itet(3,it2),
     *                  itet(4,it2))
      ipos(1,3)=iposfnd(i1b,itet(1,it3),itet(2,it3),itet(3,it3),
     *                  itet(4,it3))
      ipos(1,4)=iposfnd(i1b,itet(1,it4),itet(2,it4),itet(3,it4),
     *                  itet(4,it4))
      ipos(2,1)=iposfnd(i2,itet(1,it1),itet(2,it1),itet(3,it1),
     *                  itet(4,it1))
      ipos(2,2)=iposfnd(i2,itet(1,it2),itet(2,it2),itet(3,it2),
     *                  itet(4,it2))
      ipos(2,3)=iposfnd(i2b,itet(1,it3),itet(2,it3),itet(3,it3),
     *                  itet(4,it3))
      ipos(2,4)=iposfnd(i2b,itet(1,it4),itet(2,it4),itet(3,it4),
     *                  itet(4,it4))
C
C     ******************************************************************
C
C     MAKE THE FOUR NEW TETRAHEDRON ASSIGNMENTS
C
      id(1)=i1
      id(2)=i5
      id(3)=i3
      id(4)=i4
      id(5)=i2
      id(6)=i3
      id(7)=i5
      id(8)=i4
      id(9)=i2b
      id(10)=i5b
      id(11)=i3b
      id(12)=i6
      id(13)=i1b
      id(14)=i3b
      id(15)=i5b
      id(16)=i6
C
      jd(1) =4*(it2-1)+1
      jd(2) =jtet(ipos(2,1),it1)
      jd(3) =jtet(ipos(2,2),it2)
      jd(4) =4*(it4-1)+4+mbndry
      jd(5) =4*(it1-1)+1
      jd(6) =jtet(ipos(1,2),it2)
      jd(7) =jtet(ipos(1,1),it1)
      jd(8) =4*(it3-1)+4+mbndry
      jd(9) =4*(it4-1)+1
      jd(10)=jtet(ipos(1,3),it3)
      jd(11)=jtet(ipos(1,4),it4)
      jd(12)=4*(it2-1)+4+mbndry
      jd(13)=4*(it3-1)+1
      jd(14)=jtet(ipos(2,4),it4)
      jd(15)=jtet(ipos(2,3),it3)
      jd(16)=4*(it1-1)+4+mbndry
C
C     ******************************************************************
C     DEBUG SECTION.
C
      if(idebug.gt.0) then
         ict12=0
         ict35=0
         ict46=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            if(isum1.eq.0.and.isum2.eq.0.and.itet(1,it).gt.0)
     *            ict12=ict12+1
 100     continue
         do 110 it=1,ntets
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum3.eq.0.and.isum5.eq.0.and.itet(1,it).gt.0)
     *             ict35=ict35+1
 110     continue
         do 120 it=1,ntets
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum6=(itet(1,it)-i6)*(itet(2,it)-i6)*
     *            (itet(3,it)-i6)*(itet(4,it)-i6)
            if(isum4.eq.0.and.isum6.eq.0.and.itet(1,it).gt.0)
     *            ict46=ict46+1
 120     continue
C
C  note we are checking child points not parents
C  so we are likely to find two connections only
C
         if(ict12.ne.2) then
            write(logdan,9020) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("fnd4to4i - LINE EXISTS MORE THAN FOUR TIMES:",
     *             " it1=",i10," it2=",i10," it3=",i10," it4=",i10)
 9030       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
         if(ict35.ne.0) then
            write(logdan,9000) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i3,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("fnd4to4i - LINE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10," it4=",i10)
 9010       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
         if(ict46.ne.0) then
            write(logdan,9040) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9050) i4,i6
            call writloga("default",0,logdan,0,ierrwrt)
 9040       format("fnd4to4i - LINE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10," it4=",i10)
 9050       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
      endif
C
      goto 9999
 9999 continue
      return
      end
      subroutine fnd4to4ix(it1,it2,it3,it4,i1,i2,i3,i4b,i4,i5,i6,
     *                    id,jd,
     *                    npoints,ntets)
       implicit none
C
C ######################################################################
C
C      PURPOSE -
C
C         This routine finds the 4-to-4i configuration which flips
C         connections on a material interface.
C         note this version of the routine is called when
c           it1  |  it3
c            ____|_________
c                |
c           it2  |   it4
C         where the material interface is between it1/it3 and
c         it2/it4
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         it2   - the tet number of the second member of the quartet
C         it3   - the tet number of the third member of the quartet
C         it4   - the tet number of the fourth member of the quartet
C         i1-i6 - the six point numbers
C         i4b   - a child point corresponding to point i4
C
C      OUTPUT ARGUMENTS -
C
C         id    - the "itet" coordinates of the four new tets
C         jd    - the "jtet" coordinates of the four new tets
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      integer nmulti
      parameter (nmulti = 200)
      integer kpts(3),ichain1(nmulti),imt1a(nmulti),id(16),jd(16),
     *          ipos(2,4)
      integer m,ierror,length,icmotype,kpt,imtx,k,ict,ipar,i0,i1,i2,i3,
     *  i4,i5,i6,i1b,i2b,i4b,i6b,ict12,isum1,isum2,isum3,npoints,ntets,
     *  it1,it2,it3,it4,ierrwrt,isum6,isum5,isum4,it,ict35,ict46
      integer iposfnd,min0
C
C ######################################################################
C
C     MACROS.
C
      iposfnd(i0,i1,i2,i3,i4)=min0(iabs(i1-i0)*4+1,iabs(i2-i0)*4+2,
     *                             iabs(i3-i0)*4+3,iabs(i4-i0)*4+4)
C
C ######################################################################
C
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
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C     FIND CHILDREN FOR BOUNDARY POINTS.
C
      imtx=imt1(itet(1,it2))
      kpts(1)=i1
      kpts(2)=i2
      kpts(3)=i6
      do 250 k=1,3
         kpt=kpts(k)
         call getchain(kpt,ichain1,imt1a,nmulti,ict,ipar)
         if (ict .eq. 1) then
            if (imt1a(1) .ne. imtx) call termcode
            kpts(k)=ichain1(1)
         else
            do 200 m=1,ict
               if (imt1a(m) .eq. imtx) then
                  kpts(k)=ichain1(m)
                  goto 250
               endif
 200        continue
            call termcode(1)
         endif
 250  continue
      i1b=kpts(1)
      i2b=kpts(2)
      i6b=kpts(3)
C
C     ******************************************************************
C
C     FIND THE POSITIONS IN THE FOUR ORIGINAL TETRAHEDRA WHICH POINT TO
C     OUTSIDE FACES.
C
      ipos(1,1)=iposfnd(i1,itet(1,it1),itet(2,it1),itet(3,it1),
     *                  itet(4,it1))
      ipos(1,2)=iposfnd(i1b,itet(1,it2),itet(2,it2),itet(3,it2),
     *                  itet(4,it2))
      ipos(1,3)=iposfnd(i1,itet(1,it3),itet(2,it3),itet(3,it3),
     *                  itet(4,it3))
      ipos(1,4)=iposfnd(i1b,itet(1,it4),itet(2,it4),itet(3,it4),
     *                  itet(4,it4))
      ipos(2,1)=iposfnd(i2,itet(1,it1),itet(2,it1),itet(3,it1),
     *                  itet(4,it1))
      ipos(2,2)=iposfnd(i2b,itet(1,it2),itet(2,it2),itet(3,it2),
     *                  itet(4,it2))
      ipos(2,3)=iposfnd(i2,itet(1,it3),itet(2,it3),itet(3,it3),
     *                  itet(4,it3))
      ipos(2,4)=iposfnd(i2b,itet(1,it4),itet(2,it4),itet(3,it4),
     *                  itet(4,it4))
C
C     ******************************************************************
C
C     MAKE THE FOUR NEW TETRAHEDRON ASSIGNMENTS
C
      id(1)=i1
      id(2)=i3
      id(3)=i4
      id(4)=i6
      id(5)=i2
      id(6)=i4
      id(7)=i3
      id(8)=i6
      id(9)=i2b
      id(10)=i5
      id(11)=i4b
      id(12)=i6b
      id(13)=i1b
      id(14)=i4b
      id(15)=i5
      id(16)=i6b
C
      jd(1) =4*(it2-1)+1
      jd(2) =4*(it4-1)+3+mbndry
      jd(3) =jtet(ipos(2,3),it3)
      jd(4) =jtet(ipos(2,1),it1)
      jd(5) =4*(it1-1)+1
      jd(6) =jtet(ipos(1,3),it3)
      jd(7) =4*(it3-1)+2+mbndry
      jd(8) =jtet(ipos(1,1),it1)
      jd(9) =4*(it4-1)+1
      jd(10)=4*(it2-1)+3+mbndry
      jd(11)=jtet(ipos(1,4),it4)
      jd(12)=jtet(ipos(1,2),it2)
      jd(13)=4*(it3-1)+1
      jd(14)=jtet(ipos(2,4),it4)
      jd(15)=4*(it1-1)+2+mbndry
      jd(16)=jtet(ipos(2,2),it2)
C
C     ******************************************************************
C     DEBUG SECTION.
C
      if(idebug.gt.0) then
         ict12=0
         ict35=0
         ict46=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            if(isum1.eq.0.and.isum2.eq.0.and.itet(1,it).gt.0)
     *            ict12=ict12+1
 100     continue
         do 110 it=1,ntets
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum3.eq.0.and.isum5.eq.0.and.itet(1,it).gt.0)
     *             ict35=ict35+1
 110     continue
         do 120 it=1,ntets
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum6=(itet(1,it)-i6)*(itet(2,it)-i6)*
     *            (itet(3,it)-i6)*(itet(4,it)-i6)
            if(isum4.eq.0.and.isum6.eq.0.and.itet(1,it).gt.0)
     *            ict46=ict46+1
 120     continue
C
C  note we are checking child points not parents
C  so we are likely to find two connections only
C
         if(ict12.ne.2) then
            write(logdan,9020) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("fnd4to4i - LINE EXISTS MORE THAN FOUR TIMES:",
     *             " it1=",i10," it2=",i10," it3=",i10," it4=",i10)
 9030       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
         if(ict35.ne.0) then
            write(logdan,9000) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i3,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("fnd4to4i - LINE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10," it4=",i10)
 9010       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
         if(ict46.ne.0) then
            write(logdan,9040) it1,it2,it3,it4
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9050) i4,i6
            call writloga("default",0,logdan,0,ierrwrt)
 9040       format("fnd4to4i - LINE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10," it4=",i10)
 9050       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
      endif
C
      goto 9999
 9999 continue
      return
      end
