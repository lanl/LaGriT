*dk,try4to4x
      subroutine try4to4x(iepos,it,iflag,nface2,i,
     *                    npoints,ntets)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine attempts flipping 4-to-4 type connections.
C
C     INPUT ARGUMENTS -
C
C        iepos    - the edge position
C        it       - the tet number
C
C     OUTPUT ARGUMENTS -
C
C        iflag    - the success flag:
C                      0 => flip was not performed
C                      1 => flip was performed
C
C     CHANGE HISTORY -
C
C        $Log: try4to4x.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   05 Jan 2001 12:58:10   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.8   Tue Sep 01 08:35:50 1998   dcg
CPVCS    get rid of single precision constants
CPVCS
CPVCS       Rev 1.7   Mon May 25 00:52:00 1998   kuprat
CPVCS    Dropped passing of last two (unused) arguments to TEST4TO4.
CPVCS
CPVCS       Rev 1.6   Wed Jul 09 09:12:14 1997   dcg
CPVCS    make error checking more rigorous
CPVCS    add better diagnostics if idebug set
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 17:05:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   Tue Apr 16 14:38:46 1996   dcg
CPVCS    replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:12   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:46   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:42   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:56   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none

      include "cmo.h"
      include "chydro.h"
      include "neibor.h"

C arguments (iepos,it,iflag,nface2,i,npoints,ntets)
      integer iepos,it,iflag,nface2,i,npoints,ntets

C variables

      pointer( ipiopen , lst(*) )
      integer lst
      integer id(16,2),jd(16,2),itets(4)

      real*8 alargenumber
      parameter (alargenumber= 1.0d+30)

      real*8 test1,test2,test3,test4,test5,test6,test7,test8,
     *  vol1,vol2,vol3,xxlarge,val1,val2,valinit,vol4,distsq,
     *  xv,yv,zv

      integer ip,itv,nflips,ierflg,ifindopt,np1,np2,np3,jf1,jf2,
     *  itx,i5,i6,it5,iofs,iflg1,iflg2,ilen,icmotype,ierror,
     *  ier,ifour,it2,it4,ipos2,it3,ipos3,idum,
     *  j,k,i1,i2,i3,i4
C
      real*8 crosx1,crosy1,crosz1,volume
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C ######################################################################
C BEGIN begin
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,ilen,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      xxlarge=alargenumber
      val1=xxlarge
      val2=xxlarge
      valinit=1.0e-14
      iflag=0
      ifour=4
c     ipitets=loc(itets(1))
C
C     ***************************************************************
C
C     DETERMINE WHETHER OR NOT A 4-TO-4 FLIP IS POSSIBLE
C
      iofs=4*(iepos-1)
      if(jtet(ielist(iofs+3),it).ge.mbndry.or.
     *   jtet(ielist(iofs+4),it).ge.mbndry) then
            goto 9999
      endif
      it2=0.25*dble(jtet(ielist(iofs+3),it))+0.9
      it3=0.25*dble(jtet(ielist(iofs+4),it))+0.9
      i1=itet(ielist(iofs+1),it)
      i2=itet(ielist(iofs+2),it)
      i3=itet(ielist(iofs+3),it)
      i4=itet(ielist(iofs+4),it)
      i5=itet1(jtet(ielist(iofs+3),it))
      i6=itet1(jtet(ielist(iofs+4),it))
C
      ipos2=4
      if(itet(1,it2).eq.i4) ipos2=1
      if(itet(2,it2).eq.i4) ipos2=2
      if(itet(3,it2).eq.i4) ipos2=3
      if(jtet(ipos2,it2).ge.mbndry) then
         goto 9999
      endif
      it4=0.25*dble(jtet(ipos2,it2))+0.9
C
      ipos3=4
      if(itet(1,it3).eq.i3) ipos3=1
      if(itet(2,it3).eq.i3) ipos3=2
      if(itet(3,it3).eq.i3) ipos3=3
      if(jtet(ipos3,it3).ge.mbndry) then
         goto 9999
      endif
      it5=0.25*dble(jtet(ipos3,it3))+0.9
C
      if(it4.ne.it5) then
         goto 9999
      endif
      if(kfix(1,it2).ge.1.or.kfix(1,it3).ge.1.or.kfix(1,it4).ge.1) then
         goto 9999
      endif
      vol1=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
      vol2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
      vol3=volume(itet(1,it3),itet(2,it3),itet(3,it3),itet(4,it3))
      vol4=volume(itet(1,it4),itet(2,it4),itet(3,it4),itet(4,it4))
      if(  min(vol1,vol2,vol3,vol4).le.0) goto 9999
C
C     _________________________________________________________
C
C     EVALUATE THE TWO 4-TO-4 POSSIBILITIES
C
      call test4to4(i3,i1,i6,i2,i4,i5,iflg1)
      call test4to4(i4,i1,i3,i2,i5,i6,iflg2)
      if(i.gt.nface2) then
         do 750 j=1,nface2
            jf1=lst(j)
            if(jtet1(jf1).ge.mbndry) goto 750
            jf2=jtet1(jf1)
            call face(jf2,idum,itx,np1,np2,np3)
            if(itet(1,itx).le.0) goto 750
            if((np1.eq.i3.and.np2.eq.i5).or.
     *         (np1.eq.i5.and.np2.eq.i3).or.
     *         (np2.eq.i3.and.np3.eq.i5).or.
     *         (np2.eq.i5.and.np3.eq.i3).or.
     *         (np3.eq.i3.and.np1.eq.i5).or.
     *         (np3.eq.i5.and.np1.eq.i3)) iflg1=0
            if((np1.eq.i4.and.np2.eq.i6).or.
     *         (np1.eq.i6.and.np2.eq.i4).or.
     *         (np2.eq.i4.and.np3.eq.i6).or.
     *         (np2.eq.i6.and.np3.eq.i4).or.
     *         (np3.eq.i4.and.np1.eq.i6).or.
     *         (np3.eq.i6.and.np1.eq.i4)) iflg2=0
 750     continue
      endif
      if(iflg1.eq.1.and.iflg2.eq.1) then
         ifindopt=0
      elseif(iflg1.eq.1) then
         ifindopt=1
      elseif(iflg2.eq.1) then
         ifindopt=2
      else
         goto 9999
      endif
      call find4to4(it,iepos,it2,it3,it4,i1,i2,i3,i4,i5,i6,
     *              ifindopt,id(1,1),jd(1,1),id(1,2),jd(1,2),
     *              npoints,ntets,ierflg)
      if(ierflg.ne.0) then
         iflag=0
         go to 9999
      endif
      itets(1)=it
      itets(2)=it2
      itets(3)=it3
      itets(4)=it4
      call try2to0(itets,ifour,nflips,itv,
     *             npoints,ntets)
      if(nflips.ge.1) goto 9999
C
      if(iflg1.eq.1) then
         call vorpoint(id(1,1),id(2,1),id(3,1),id(4,1),
     *                 xv,yv,zv,distsq)
         test1=distsq-(xic(id(9,1))-xv)**2-
     *                (yic(id(9,1))-yv)**2-
     *                (zic(id(9,1))-zv)**2
         test2=distsq-(xic(id(12,1))-xv)**2-
     *                (yic(id(12,1))-yv)**2-
     *                (zic(id(12,1))-zv)**2
         call vorpoint(id(9,1),id(10,1),id(11,1),id(12,1),
     *                 xv,yv,zv,distsq)
         test3=distsq-(xic(id(1,1))-xv)**2-
     *                (yic(id(1,1))-yv)**2-
     *                (zic(id(1,1))-zv)**2
         test4=distsq-(xic(id(4,1))-xv)**2-
     *                (yic(id(4,1))-yv)**2-
     *                (zic(id(4,1))-zv)**2
         val1=  max(test1,test2,test3,test4)
      endif
 90   continue
      if(iflg2.eq.1) then
C
         call vorpoint(id(1,2),id(2,2),id(3,2),id(4,2),
     *                 xv,yv,zv,distsq)
         test5=distsq-(xic(id(5,2))-xv)**2-
     *                (yic(id(5,2))-yv)**2-
     *                (zic(id(5,2))-zv)**2
         test6=distsq-(xic(id(7,2))-xv)**2-
     *                (yic(id(7,2))-yv)**2-
     *                (zic(id(7,2))-zv)**2
         call vorpoint(id(5,2),id(6,2),id(7,2),id(8,2),
     *                 xv,yv,zv,distsq)
         test7=distsq-(xic(id(1,2))-xv)**2-
     *                (yic(id(1,2))-yv)**2-
     *                (zic(id(1,2))-zv)**2
         test8=distsq-(xic(id(3,2))-xv)**2-
     *                (yic(id(3,2))-yv)**2-
     *                (zic(id(3,2))-zv)**2
         val2=  max(test5,test6,test7,test8)
      endif
 91   continue
C
C     ____________________________________________________________
C
C     SELECT THE BEST FLIPPING POSSIBILITY AND FLIP IT!
C
      if(val1.lt.valinit) then
         ip=1
      elseif(val2.lt.valinit) then
         ip=2
      else
         goto 9999
      endif
      iflag=1
      call flip4to4(it,it2,it3,it4,id(1,ip),jd(1,ip),
     *              npoints,ntets)
      kfix(1,it)=it
      kfix(1,it2)=it2
      kfix(1,it3)=it3
      kfix(1,it4)=it4
C
      goto 9999
 9999 continue
      return
      end
