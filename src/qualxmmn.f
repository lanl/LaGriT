*dk,qualxmmn
      subroutine qualxmmn(cgeom,mpary,mpno,npts,xic,yic,zic,
     *                    xmn,ymn,zmn,xmx,ymx,zmx,
     *                    xcen,ycen,zcen,xscl,yscl,zscl,
     *                    nmaspnt,maspnt,ierr)
C
C ######################################################################
C
C        $Log: qualxmmn.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
C
      implicit real*8 (a-h, o-z)
C
      character*132 logmess
C
C
C#######################################################################
C
C     PURPOSE -
C
C
C        THIS ROUTINE QUALIFIES A SET OF MASS POINTS ACCORDING TO
C           SOME GEOMETRY CONSTRAINTS.
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     HISTORY -
C
C        HT0103AA-91
C
C#######################################################################
C
      include 'consts.h'
      character*32 cgeom
      dimension xic(*), yic(*), zic(*), mpary(*)
      dimension maspnt(*)
C
C#######################################################################
C
      do 50 i1=1,npts
         maspnt(i1)=0
 50   continue
      if(cgeom(1:3).eq.'xyz') then
         xscale=xscl
         yscale=yscl
         zscale=zscl
         xmn1=xscale*(xmn-xcen)+xcen
         ymn1=yscale*(ymn-ycen)+ycen
         zmn1=zscale*(zmn-zcen)+zcen
         xmx1=xscale*(xmx-xcen)+xcen
         ymx1=yscale*(ymx-ycen)+ycen
         zmx1=zscale*(zmx-zcen)+zcen
         do 100 ii=1,mpno
            i1=mpary(ii)
            it1x=cvmgt(1,0,xic(i1).gt.xmn1)
            it2x=cvmgt(1,0,xic(i1).lt.xmx1)
            it1y=cvmgt(1,0,yic(i1).gt.ymn1)
            it2y=cvmgt(1,0,yic(i1).lt.ymx1)
            it1z=cvmgt(1,0,zic(i1).gt.zmn1)
            it2z=cvmgt(1,0,zic(i1).lt.zmx1)
            maspnt(i1)=i1*iand(iand(iand(iand(iand(it1z,it2z),
     *             it2y),it1y),it2x),it1x)
 100     continue
      endif
      if(cgeom(1:3).eq.'rtz') then
         rscale=xscl
         tscale=yscl
         zscale=zscl
         radmin=rscale*xmn
         radmax=rscale*xmx
         thimin=tscale*ymn
         thimax=tscale*ymx
         zmn1=zscale*(zmn-zcen)+zcen
         zmx1=zscale*(zmx-zcen)+zcen
         do 200 ii=1,mpno
            i1=mpary(ii)
            xp=xic(i1)-xcen
            yp=yic(i1)-ycen
            zp=zic(i1)
            radius=sqrt(xp**2+yp**2)
            call angle3v(zero,zero,zero,xp,yp,zp,thi,dummy)
            maspnt(i1)=0
            if(radius.ge.radmin.and.radius.le.radmax) then
               if(thi.ge.thimin.and.thi.le.thimax) then
                  if(zp.ge.zmn1.and.zp.le.zmx1) then
                     maspnt(i1)=i1
                  endif
               endif
            endif
 200     continue
      endif
      if(cgeom(1:3).eq.'rtp') then
         rscale=xscl
         tscale=yscl
         pscale=zscl
         radmin=rscale*xmn
         radmax=rscale*xmx
         thimin=tscale*ymn
         thimax=tscale*ymx
         phimin=pscale*zmn
         phimax=pscale*zmx
         do 300 ii=1,mpno
         i1=mpary(ii)
         xp=xic(i1)-xcen
         yp=yic(i1)-ycen
         zp=zic(i1)-zcen
         radius=sqrt(xp**2+yp**2+zp**2)
         call angle3v(zero,zero,zero,xp,yp,zp,phi,thi)
         maspnt(i1)=0
         if(radius.ge.radmin.and.radius.le.radmax) then
            if(phi.ge.phimin.and.phi.le.phimax) then
               if(thi.ge.thimin.and.thi.le.thimax) then
                  maspnt(i1)=i1
               endif
            endif
         endif
 300     continue
      endif
      n=npts
      call kmprsn(n,maspnt,1,maspnt,1,maspnt,1,nmaspnt)
C
C
      goto 9999
 9999 continue
      return
      end
