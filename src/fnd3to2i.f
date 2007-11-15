*dk,fnd3to2i
      subroutine fnd3to2i(it1,ipos,it2,it3,id,jd,iflip,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C
C ######################################################################
C
C      PURPOSE -
C
C        This routine flips a connection on the material interface
C        if that connection is shared by exactly three tetrahedra.
C        Two of the tetrahedra (it2 and it3) must be of the same
C        material, and the other tetrahedron (it1) must be of a
C        different material.  Tetrahedron "it1," which has two
C        boundary faces, is then converted to the material type of
C        tetrahedra "it2" and "it3."  The two boundary faces of "it1"
C        become interior faces, and the other two faces of "it2" become
C        boundary faces.  The three tetrahedra are then taken into
C        two tetrahedra, and the jtet pointers are reset to indicate
C        the new shape of the interface.
C
C           (i1,i2,i3,i4) - (j1,j2,xx,xx)
C           (i1,i2,i4,i5) - (k1,k2,xx,xx)
C           (i1,i2,i5,i3) - (m1,m2,xx,xx)
C           -------------------------------------
C           (i3,i4,i1,i5) - {k2,m2,(3,it2),j2+mbndry}
C           (i4,i3,i2,i5) - {m1,k1,(3,it1),j1+mbndry}
C
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         ipos  - the position of the common edge (1 - 6)
C
C      OUTPUT ARGUMENTS -
C
C         it2   - the tet number of the second member of the triplet
C         it3   - the tet number of the third member of the triplet
C         id    - the "itet" coordinates of the two new tets
C         jd    - the "jtet" coordinates of the two new tets
C         iflip - +1 ==>  flip is possible
C                  0 ==>  flip is not-possible
C
C     CHANGE HISTORY -
C
C        $Log: fnd3to2i.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   05 Jan 2001 12:59:32   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.8   Tue Sep 22 11:48:24 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.7   Fri Jul 11 09:16:18 1997   dcg
CPVCS    clean up warning messages
CPVCS
CPVCS       Rev 1.5   Thu Apr 17 16:14:46 1997   dcg
CPVCS    set iflip to zero and return if cant find
CPVCS    matching materials - do not call termcode
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:02   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:08   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:53:00   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:02   pvcs
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
      dimension kpts(4),ichain1(nmulti),imt1a(nmulti),id(12),jd(12)
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
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
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
      iflip=0
C
C     ******************************************************************
C
C     FIND ALL THREE TETRAHEDRA AROUND THE CONNECTION.
C
      ipos1=ielist(4*ipos-3)
      ipos2=ielist(4*ipos-2)
      ipos3=ielist(4*ipos-1)
      ipos4=ielist(4*ipos  )
      if (jtet(ipos3,it1).le.mbndry.or.jtet(ipos4,it1).le.mbndry)
     *   call termcode(1)
      jtemp2=jtet(ipos3,it1)-mbndry
      jtemp3=jtet(ipos4,it1)-mbndry
      it2=0.25*dble(jtemp2)+0.9
      it3=0.25*dble(jtemp3)+0.9
C
C     ******************************************************************
C
C     GET THE CORRECT CHILD POINTS TO CONVERT TET "it1" TO THE MATERIAL
C     OF TETS "it2" and "it3."
C
C
      kpts(1)=itet(ipos1,it1)
      kpts(2)=itet(ipos2,it1)
      kpts(3)=itet(ipos3,it1)
      kpts(4)=itet(ipos4,it1)
C
      i5=itet1(jtemp3)
      imtx=imt1(i5)
      do 250 k=1,4
         kpt=kpts(k)
         call getchain(kpt,ichain1,imt1a,nmulti,ict,ipar)
         if (ict .eq. 1) then
            if (imt1a(1) .ne. imtx) then
               iflip=0
               go to 9999
            endif
            kpts(k)=ichain1(1)
         else
            do 200 m=1,ict
               if (imt1a(m) .eq. imtx) then
                   kpts(k)=ichain1(m)
                  goto 250
               endif
 200        continue
            iflip=0
            go to 9999
         endif
 250  continue
C
      i1=kpts(1)
      i2=kpts(2)
      i3=kpts(3)
      i4=kpts(4)
C
C     ******************************************************************
C
C     MAKE THE TWO NEW TETRAHEDRON ASSIGNMENTS.
C
      id(1)=i3
      id(2)=i4
      id(3)=i1
      id(4)=i5
      id(5)=i4
      id(6)=i3
      id(7)=i2
      id(8)=i5
C
      jd(3)=4*(it2-1)+3
      jd(7)=4*(it1-1)+3
C        "id(3)" and "id(7)" denote the previously common edge
      do 300 i=1,4
         if(itet(i,it2).eq.id(3)) jd(6)=jtet(i,it2)
         if(itet(i,it2).eq.id(7)) jd(1)=jtet(i,it2)
         if(itet(i,it3).eq.id(3)) jd(5)=jtet(i,it3)
         if(itet(i,it3).eq.id(7)) jd(2)=jtet(i,it3)
 300   continue
C
C     ..................................................................
C     CHANGE THE JTET BOUNDARY POINTERS TO REFLECT THE NEW SHAPE OF THE
C     INTERFACE.
C
      jtemp4=jtet(ipos2,it1)
      if (jtemp4 .lt. mbndry) then
         jtemp4=jtemp4+mbndry
      elseif (jtemp4 .gt. mbndry) then
         if (imt1(itet1(jtemp4-mbndry)) .eq. imtx) jtemp4=jtemp4-mbndry
      endif
      jd(4)=jtemp4
      jtemp8=jtet(ipos1,it1)
      if (jtemp8 .lt. mbndry) then
         jtemp8=jtemp8+mbndry
      elseif (jtemp8 .gt. mbndry) then
         if (imt1(itet1(jtemp8-mbndry)) .eq. imtx) jtemp8=jtemp8-mbndry
      endif
      jd(8)=jtemp8
C
C
C     ******************************************************************
C     DETERMINE IF A 3-TO-2 FLIP IS POSSIBLE BY TESTING WHETHER THE
C     POINTS ON THE PREVIOUS COMMON EDGE (id(3)-id(7)) ARE ON OPPOSITE
C     SIDES OF THE POTENTIAL NEW FACE (id(1)-id(2)-id(4))
C
      xst=1.0e-9
      xn=crosx1(id(1),id(2),id(4))
      yn=crosy1(id(1),id(2),id(4))
      zn=crosz1(id(1),id(2),id(4))
      xa=xic(id(1))
      ya=yic(id(1))
      za=zic(id(1))
      x3=xic(id(3))-xa
      y3=yic(id(3))-ya
      z3=zic(id(3))-za
      x7=xic(id(7))-xa
      y7=yic(id(7))-ya
      z7=zic(id(7))-za
      sn=sqrt(xn*xn+yn*yn+zn*zn)
      s3=sqrt(x3*x3+y3*y3+z3*z3)
      s7=sqrt(x7*x7+y7*y7+z7*z7)
      dot1=xn*x3+yn*y3+zn*z3
      dot2=xn*x7+yn*y7+zn*z7
      if(dot1*dot2.lt.0.0.and.abs(dot1).ge.xst*sn*s3.and.
     *                        abs(dot2).ge.xst*sn*s7) iflip=1
C
C     ******************************************************************
C     DEBUG SECTION.
C
      if(idebug.gt.0) then
         ict12=0
         ict345=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            if(isum1.eq.0.and.isum2.eq.0.and.itet(1,it).gt.0)
     *             ict12=ict12+1
 100     continue
         do 110 it=1,ntets
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum3.eq.0.and.isum4.eq.0.and.isum5.eq.0.and.
     *            itet(1,it).gt.0) ict345=ict345+1
 110     continue
C
C note: checking for child points - not parents
C  so number of tets containing edge is not very relevant
C
         if(ict12.gt.3) then
            write(logdan,9020) it1,it2,it3
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("fnd3to2i - LINE EXISTS MORE THAN THREE TIMES:"
     *             " it1=",i10," it2=",i10," it3=",i10)
 9030       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
         if(ict345.ne.0) then
            write(logdan,9000) it1,it2,it3
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i3,i4,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("fnd3to2i - FACE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10)
 9010       format("                face:     ",i10,"     ",i10,
     *             "     ",i10)
c            call rwdmpw
         endif
      endif
      goto 9999
 9999 continue
      return
      end
