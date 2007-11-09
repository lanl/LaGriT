*dk,rotatelo
      subroutine rotatelo(xc,yc,zc,xcp,ycp,zcp,xa,ya,za,xb,yb,zb,th1)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/rotatelo.f_a  $
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:00:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Mon Jun 03 15:10:50 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.2   Tue Apr 30 11:24:58 1996   dcg
CPVCS    replace literal in argument lists with consts variables
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:10   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:22   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
      include 'consts.h'
C
c
c
c rotate a point, (xc,yc,zc),  about a line defined by the line
c (xa,ya,za) to (xb,yb,zb)c through an angle theta to find the
c new point (xcp,ycp,zcp).
c
c
c
c find the point (x1,y1,z1) to be point on (a,b) where
c (a,x) is the projection of (a,c) onto (a,b).
c
      distab=(xb-xa)**2+(yb-ya)**2+(zb-za)**2
      if(distab.eq.zero) then
         xcp=xc
         ycp=yc
         zcp=zc
         goto 9999
      else
         dotacab=(xc-xa)*(xb-xa)+(yc-ya)*(yb-ya)+(zc-za)*(zb-za)
         x1=xa+(xb-xa)*dotacab/distab
         y1=ya+(yb-ya)*dotacab/distab
         z1=za+(zb-za)*dotacab/distab
      endif
c
c now use the point (x1,y1,z1), (xc,yc,zc), and (xb,yb,zb) to
c rotate the coordinate system such that to new z-axis points
c from (x1,y1,z1) to (xb,yb,zb). then use this local cylinderical
c to rotate through an angle theta for (xc,yc,zc) to (xcp,ycp,zcp)
c
c first rotate the coordinate system by using a transformation
c matrix subroutine
c
      bx1=xb-x1
      by1=yb-y1
      bz1=zb-z1
      distbx=bx1**2+by1**2+bz1**2
      if(distbx.eq.zero) then
         bx1=xa-x1
         by1=ya-y1
         bz1=za-z1
         th1=-th1
      endif
      cx1=xc-x1
      cy1=yc-y1
      cz1=zc-z1
      call eullag3(1,zero,zero,zero,bx1,by1,bz1,cx1,cy1,cz1,ap1,bp1,cp1)
      distab=sqrt(ap1**2+bp1**2)
      if(distab.eq.zero) then
         a1=zero
         b1=zero
         c1=cp1
      else
         distxc=sqrt(cx1**2+cy1**2+cz1**2)
         cosab=ap1/distab
         sinab=bp1/distab
         a1=distxc*(cos(th1)*cosab-sin(th1)*sinab)
         b1=distxc*(sin(th1)*cosab+cos(th1)*sinab)
         c1=cp1
      endif
      call eullag3(2,zero,zero,zero,bx1,by1,bz1,a1,b1,c1,xc1,yc1,zc1)
      xcp=xc1+x1
      ycp=yc1+y1
      zcp=zc1+z1
      goto 9999
 9999 continue
      return
      end
