*dk,find3to2
      subroutine find3to2(it1,ipos,it2,it3,id,jd,flag,
     *                    npoints,ntets)
C
C ######################################################################
C
C      PURPOSE -
C
C         This routine finds the tet pair corresponding to the
C         tet triplet with a common edge:
C            (i1,i2,i3,i4)
C            -------------
C            (i3,i4,i1,nodd)
C            (i4,i3,i2,nodd)   where i1-i2 denotes the common edge.
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         ipos  - the position fo the common edge (1 - 6)
C
C      OUTPUT ARGUMENTS -
C
C         it2   - the tet number of the second member of the triplet
C         it3   - the tet number of the third member of the triplet
C         id    - the "itet" coordinates of the two new tets
C         jd    - the "jtet" coordinates of the two new tets
C         flag  - +1 ==>  flip is possible
C                  0 ==>  flip is not-possible
C
C     CHANGE HISTORY -
C
C        $Log: find3to2.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   05 Jan 2001 12:56:04   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.7   Tue Aug 18 16:26:04 1998   kuprat
CPVCS     Reduced diagnostic output if idebug<2.
CPVCS
CPVCS       Rev 1.6   Wed Jul 09 09:12:08 1997   dcg
CPVCS    make error checking more rigorous
CPVCS    add better diagnostics if idebug set
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:48:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   08/30/95 21:09:18   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:00   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:40   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:50:50   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:08   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
C arguments it1,ipos,it2,it3,id,jd,flag,npoints,ntets 
      integer it1,ipos,it2,it3,flag,npoints,ntets
      integer id(12),jd(12)

C variables
 
      integer i1,i2,i3,i4,i5
      integer ierror,ierrwrt,lenitet,icmotype,lenjtet,i,it,
     *        isum1,isum2,isum3,isum4,isum5,ict12,
     *        j,k,length,lenxic, lenyic,lenzic,ipos1,ipos2,ipos3,
     *        ipos4,ict345

      real*8 xst,xn,yn,zn,xa,ya,za,x3,y3,z3,x7,y7,z7,sn,s3,s7,
     *       dot1,dot2 

    
C statement functions

      real*8 crosx1,crosy1,crosz1
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
C
C ######################################################################
C BEGIN begin
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
 
      ipos1=ielist(4*ipos-3)
      ipos2=ielist(4*ipos-2)
      ipos3=ielist(4*ipos-1)
      ipos4=ielist(4*ipos  )
      it2=0.25*dble(jtet(ipos3,it1))+0.9
      it3=0.25*dble(jtet(ipos4,it1))+0.9
C
      i1=itet(ipos1,it1)
      i2=itet(ipos2,it1)
      i3=itet(ipos3,it1)
      i4=itet(ipos4,it1)
      i5=itet1(jtet(ipos4,it1))
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
      jd(4)=jtet(ipos2,it1)
      jd(7)=4*(it1-1)+3
      jd(8)=jtet(ipos1,it1)
C        "id(3)" and "id(7)" denote the previously common edge
      do 10 i=1,4
         if(itet(i,it2).eq.id(3)) jd(6)=jtet(i,it2)
         if(itet(i,it2).eq.id(7)) jd(1)=jtet(i,it2)
         if(itet(i,it3).eq.id(3)) jd(5)=jtet(i,it3)
         if(itet(i,it3).eq.id(7)) jd(2)=jtet(i,it3)
 10   continue
C
C     ******************************************************************
C
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
      flag=0.0
      if(dot1*dot2.lt.0.0.and.abs(dot1).ge.xst*sn*s3.and.
     *                        abs(dot2).ge.xst*sn*s7) flag=1.0
C
      if(idebug.ge.2) then
         ict12=0
         ict345=0
         do  it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            if(isum1.eq.0.and.isum2.eq.0.and.itet(1,it).gt.0)
     *               ict12=ict12+1
C
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
            if(isum3.eq.0.and.isum4.eq.0.and.isum5.eq.0.and.
     *           itet(1,it).gt.0) ict345=ict345+1
         enddo
         if(ict12.ne.3) then
            write(logdan,9020) it1,it2,it3
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("find3to2 - LINE EXISTS MORE THAN THREE TIMES:"
     *             " it1=",i10," it2=",i10," it3=",i10)
 9030       format("          connection:     ",i10,"     ",i10)
 9001       format(' in tet ',i10)
            do  it=1,ntets
               isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
               isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
               if(isum1.eq.0.and.isum2.eq.0) then
                  write(logdan,9001) it
                  call writloga('default',0,logdan,0,ierrwrt)
               endif
            enddo
            flag=0.0
C*****      if(flag.ne.0) call rwdmpw
         endif
         if(ict345.ne.0) then
            write(logdan,9000) it1,it2,it3
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i3,i4,i5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("find3to2 - FACE EXISTS: it1=",i10," it2=",i10,
     *             " it3=",i10)
 9010       format("                face:     ",i10,"     ",i10,
     *             "     ",i10)
            do  it=1,ntets
               isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
               isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
               isum5=(itet(1,it)-i5)*(itet(2,it)-i5)*
     *            (itet(3,it)-i5)*(itet(4,it)-i5)
               if(isum3.eq.0.and.isum4.eq.0.and.isum5.eq.0) then
                  write(logdan,9001) it
                  call writloga('default',0,logdan,0,ierrwrt)
               endif
            enddo
            flag=0.0
C*****      if(flag.ne.0) call rwdmpw
         endif
      endif
      goto 9999
 9999 continue
      return
      end
