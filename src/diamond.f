*DK diamond
      subroutine diamond(itype,icount,nrt,npt,ri,rf,xcn,ycn,zcn,
     *  irz,irratio,rrz)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE  GENERATIONS the POINTS ON one diamond of A
C        SPHERE USING PAUL FREDRECKSON AND JOHN BAUMGARDNER ALGORITHM
C        WHICH GENERATES A SPHERE OF POINTS, AT A SPECIFIED
C        RADIUS, FROM THE GRIDDING OF AN ICOSAHEDRON ( OR 10 DIAMONDS
C        PLACED ON THE SURFACE OF THE SPHERE ).
C
C
C        FORMAT: diamond/NR,NPT,RI,RF/XCN,YCN,ZCN/IRZ,IRRATIO,RRZ
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
C        $Log:   /pvcs.config/t3d/src/diamond.f_a  $
CPVCS    
CPVCS       Rev 1.3   28 Jan 2000 16:40:10   dcg
CPVCS    remove sbcmoprm
CPVCS
CPVCS       Rev 1.2   Fri Jun 19 09:40:10 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.1   Wed Feb 18 09:21:40 1998   dcg
CPVCS    clarify number of nodes arguments
CPVCS
CPVCS       Rev 1.0   Wed Feb 11 09:46:42 1998   dcg
CPVCS    Initial revision.
 
 
C
C
C#######################################################################
C
      implicit none
      character*256 logmess
C
C#######################################################################
C
      include 'cmo.h'
      include 'chydro.h'
C
      pointer ( ipt1 , dista )
      pointer ( ipt2 , idista )
      pointer ( ipt3 , xn )
      real*8 xn(*), dista(*)
      integer idista(*)
C
      character*8 isubname
      integer icntin,icount,nr1,nrt,i1,ir,itype,npt,irratio,
     *  icts,ierr,lenmm1,lenmm2,ierror,ics,leni,icmotype,i4,
     *  icount1,ict1,length,npts,nrt1,irz,i
      real*8 rsum,rsumr,drrel1,rf,ri,dr1,r11,drabs1,
     *   xcn,ycn,zcn,dr,radxyz,xa,ya,za,xb,yb,zb,cvmgt,
     *   cvmgtr,r1,rrz
C
C#######################################################################
C
C
      isubname='diamond'
      itype=2
C
      icntin=icount
      if(irz.ne.0.and.irz.ne.1) irz=0
      if(rrz.le.0.0) rrz=1.0
      if(irz.eq.0) then
         nr1=nrt
         rsum=0.0
         if(nr1.eq.1) goto 31
         do 30 i1=1,nr1-1
            rsum=rsum+rrz**float(i1)
 30      continue
 31      continue
         rsumr=0.5+rsum+0.5*rrz**float(nr1+1)
         drrel1=(rf-ri)/rsumr
         dr1=drrel1
         r11=ri-0.5*dr1
      else
         rsum=0.0
         nr1=nrt
         if(nr1.lt.2) nr1=2
         if(nr1.eq.2) goto 41
         do 40 i1=1,nr1-2
            rsum=rsum+rrz**float(i1)
 40      continue
 41      continue
         rsumr=1.0+rsum
         drabs1=(rf-ri)/rsumr
         dr1=drabs1/rrz
         r11=ri-dr1
      endif
      r1=r11
      dr=dr1
      do 110 ir=1,nr1
C
      r1=r1+dr
      icts=icount+1
C
C     SINCE THE LOGICAL SIDE LENGTH OF A PATCH MUST BE
C       A POWER OF 2 + 1 -- ENFORCE THIS RESTRICTION.
C
      do  i=1,100
         i1=2**i+1
         if(i1*i1.gt.npt) then
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
         npts=(nrt1+1)*(nrt1+1)
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
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
C
      length=icount+10*npts+2
      if(length.gt.leni) then
         call cmo_set_info('nnodes',cmo,length,1,1,ierror)
         call cmo_newlen(cmo,ierror)
      endif
C
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
C
C
      if(r1.le.1.0e-10) then
         icount=icount+1
         xic(icount)=0.0+xcn
         yic(icount)=0.0+ycn
         zic(icount)=0.0+zcn
         goto 9997
      endif
C
C
      icount=icount+1
      xic(icount)=0.0
      yic(icount)=0.0
      zic(icount)=r1
      icount=icount+1
      xic(icount)=0.0
      yic(icount)=0.0
      zic(icount)=-r1
C
      i4=1
      call ocgrid(xn,i4,nrt1)
      call occonv(itype,icount,xn,i4,nrt1,
     *            xic,yic,zic)
 
C
      do 100 i1=icts,icount
         radxyz=sqrt(xic(i1)*xic(i1)+yic(i1)*yic(i1)+
     *            zic(i1)*zic(i1))
         xic(i1)=xic(i1)*r1/radxyz+xcn
         yic(i1)=yic(i1)*r1/radxyz+ycn
         zic(i1)=zic(i1)*r1/radxyz+zcn
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
      dr=cvmgtr(dr*rrz,dr,irratio.eq.1)
9997  call mmrelprt(isubname, ics)
  110 continue
C
C  get rid of some extra points
C
      logmess=' '
      write(logmess,9996) xcn-.001,ycn-.001,zcn-(rf-ri)-1.,
     *   xcn+.001,ycn+.001,zcn-.001
9996  format ('pset,diamondp,geom/xyz/1,0,0/',6(e12.5,','),
     * ' ; rmpoint/pset/get/diamondp ; pset/diamondp/delete ; '
     * ,'finish')
      call dotaskx3d(logmess,ierr)
 
 
C
C     ******************************************************************
C
C     RELEASE MEMORY FOR LOCAL VARIABLES IN THE PARTITION
C
 9998 continue
C
      goto 9999
 9999 continue
      return
      end
