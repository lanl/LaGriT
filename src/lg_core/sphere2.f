      subroutine sphere2(itype,icount,nrt,radius,xc,yc,zc,uradius)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE CONTROLS THE GENERATION OF POINTS ON A
C        SPHERE USING PAUL FREDRECKSON AND JOHN BAUMGARDNER ALGORITHM
C        WHICH GENERATES A SPHERE OF POINTS, AT A SPECIFIED
C        RADIUS, FROM THE GRIDDING OF AN ICOSAHEDRON ( OR 10 DIAMONDS
C        PLACED ON THE SURFACE OF THE SPHERE ).
C
C
C        FORMAT: SPHERE/itype=2/npt/radius/xcen/ycen/zcen/velocity
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log: sphere2.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Wed Apr 05 13:35:06 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.0   Mon Jan 31 11:14:20 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.7   Wed Feb 18 09:21:20 1998   dcg
CPVCS    clarify number of nodes arguments
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 17:02:06 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Tue Mar 05 12:35:54 1996   het
CPVCS    Correct an error with the zero point.
CPVCS
CPVCS       Rev 1.4   Thu Feb 08 15:55:56 1996   dcg
CPVCS    replace uic,vic,wic with user defined velocity vector
CPVCS
CPVCS       Rev 1.3   11/07/95 17:26:48   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.2   10/04/95 07:42:16   het
CPVCS    Correct the calling sequence arguments
CPVCS
CPVCS       Rev 1.1   08/15/95 18:19:54   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.0   01/17/95 16:39:38   pvcs
CPVCS     Original version
C
C
C#######################################################################
C
      implicit none
      character*132 logmess
C
C#######################################################################
C
      include "cmo.h"
      include "chydro.h"
C
      pointer ( ipt1 , dista(1) )
      pointer ( ipt2 , idista(1) )
      pointer ( ipt3 , xn(1) )
      real*8 xn,dista
      integer idista
      pointer(ipvels,vels)
      real*8 vels(3,1000000)
C
      character*32 isubname
      character*32 cvelnm
      integer   icts,icount,i,i1,nrt1,nrt,itype,npts,lenmm1,lenmm2,
     * ics,lenxic,icmotype,ierror,length,iout,lout,ityp,lin,
     *  itin,ier,ict1,i4,ierr,icount1
      pointer(ipout,out)
      real*8 out(*)
      real*8 radius,xc,yc,zc,uradius,xdiff,ydiff,zdiff,radxyz,rout,
     * xb ,yb,zb,xa,ya,za,cvmgt
C
C#######################################################################
C
C
      isubname='sphere2'
 
C
      icts=icount+1
C
C     THE LOGICAL SIDE LENGTH OF A PATCH MUST in the form
C     1+2**n
C
      do  i=0,100
         i1=2**i+1
         if(10*((i1-2)**2+2*(i1-1)-2)+12.gt.nrt) then
            nrt1=2**(i-1)+1
            write(logmess,9000) nrt1
            call writloga('default',0,logmess,0,ierr)
 9000       format('Sphere2: each patch will be ',i5,'**2 points')
            nrt1=nrt1-1
            goto 11
         endif
      enddo
 11   continue
C
C
C       CHECK TO SEE IF THERE IS ENOUGH WORK SPACE.
C
      if(itype.gt.0) then
         npts=nrt+12
      else
         npts=(4*nrt1)+(nrt1+1)+2*nrt1*nrt1
      endif
C
C     ******************************************************************
C
C     GET MEMORY FOR LOCAL VARIABLES.
C
      lenmm1 = 10*npts+2
      lenmm2 = 3*npts+1000
      call mmgetblk('dista', isubname, ipt1, lenmm1,2, ics)
      call mmgetblk('idista',isubname, ipt2, lenmm1,2, ics)
      call mmgetblk('xn', isubname, ipt3, lenmm2,2, ics)
C
C
C.......................................................................
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
C
      length=icount+10*npts+2
      if(length.gt.lenxic) then
         call cmo_set_info('nnodes',cmo,length,1,1,ierror)
         call cmo_newlen(cmo,ierror)
      endif
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
C
C
      if(radius.le.1.0e-10) then
         icount=icount+1
         xic(icount)=0.0+xc
         yic(icount)=0.0+yc
         zic(icount)=0.0+zc
         goto 9998
      endif
C
C
      icount=icount+1
      xic(icount)=0.0
      yic(icount)=0.0
      zic(icount)=radius
      icount=icount+1
      xic(icount)=0.0
      yic(icount)=0.0
      zic(icount)=-radius
C
      do 50 i4=1,10
      call ocgrid(xn,i4,nrt1)
      call occonv(itype,icount,xn,i4,nrt1,
     *            xic,yic,zic)
 50   continue
C
      do 100 i1=icts,icount
         radxyz=sqrt(xic(i1)*xic(i1)+yic(i1)*yic(i1)+
     *            zic(i1)*zic(i1))
         xic(i1)=xic(i1)*radius/radxyz+xc
         yic(i1)=yic(i1)*radius/radxyz+yc
         zic(i1)=zic(i1)*radius/radxyz+zc
 100  continue
C
C
C     SORT COORDINATES AND THROW OUT DUPLICATES.
C
      do 300 i1=icts,icount
         dista(i1-icts+1)=0.0
         idista(i1-icts+1)=0
 300  continue
      ict1=icts-1
 290  continue
         ict1=ict1+1
         if(ict1.ge.icount) goto 400
            if(idista(ict1-icts+1).ne.0) goto 290
            xa=xic(ict1)
            ya=yic(ict1)
            za=zic(ict1)
            do 310 i1=ict1+1,icount
               xb=xic(i1)
               yb=yic(i1)
               zb=zic(i1)
               dista(i1-icts+1)=(xa-xb)*(xa-xb)+(ya-yb)*(ya-yb)+
     *            (za-zb)*(za-zb)
 310        continue
            do 320 i1=ict1+1,icount
               idista(i1-icts+1)=cvmgt(1,idista(i1-icts+1),
     *           dista(i1-icts+1).lt.1.0e-10)
 320        continue
         goto 290
 400  continue
      icount1=icts
      do 410 i1=icts+1,icount
         if(idista(i1-icts+1).eq.0) then
            icount1=icount1+1
            xic(icount1)=xic(i1)
            yic(icount1)=yic(i1)
            zic(icount1)=zic(i1)
         endif
 410  continue
      icount=icount1
C
C
      if(abs(uradius).lt.1.0e-15) goto 9998
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *   ipout,lout,ityp,ierror)
      call cmo_get_info(cvelnm,cmo,ipvels,lin,itin,ier)
      if(ier.ne.0) go to 9998
      do 200 i1=icts,icount
         xdiff=xic(i1)-xc
         ydiff=yic(i1)-yc
         zdiff=zic(i1)-zc
         radxyz=sqrt(xdiff*xdiff+ydiff*ydiff+zdiff*zdiff)
C
         vels(1,i1)=uradius*xdiff/radxyz
         vels(2,i1)=uradius*ydiff/radxyz
         vels(3,i1)=uradius*zdiff/radxyz
 200  continue
C
C     ******************************************************************
C
C     RELEASE MEMORY FOR LOCAL VARIABLES IN THE PARTITION
C
 9998 call mmrelprt(isubname, ics)
C
 9999 return
      end
