*dk,voron2d
      subroutine voron2d(k1,k2,n1,n2,ichoice)
       implicit none
      real*8 alargenumber,atolerance,asmallnumber
       parameter (asmallnumber=1.0d-30)
      parameter (alargenumber=1.0d+30)
      parameter (atolerance=1.0d-13)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine determines which of two circles has the
C        minimum radius.  The two circles are defined by k1,k2,n1
C        and by k1,k2,n2.
C
C     INPUT ARGUMENTS -
C
C        k1,k2    - two of the three points defining a circle
C        n1,n2    - the points which will be taken one at a time to
C                   be the third point defining a circle.
C
C     OUTPUT ARGUMENTS -
C
C        ichoice  - 1 => minimum radius circle uses n1
C                   2 => minimum radius circle uses n2
C
C     CHANGE HISTORY -
C
C        JM1121AA-89
C
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      integer k1,k2,n1,n2,ichoice,ilen,icmotype,ierror
      real*8 a,b,g,d,e,f,crosx,crosy,crosz,xa,ya,za,
     *  fac,xb,yb,zb,xl,yl,zl,xc,yc,zc,x1,y1,z1,ztmp,
     *  dlen,x2,y2,z2,dist1,xd,yd,zd,x3,y3,z3,x4,y4,z4,
     *   dist2,dmin
      real*8 epsilon
      data epsilon /1.0d-50/
C
C ######################################################################
C
      crosx(a,b,g,d,e,f)=b*f-g*e
      crosy(a,b,g,d,e,f)=g*d-a*f
      crosz(a,b,g,d,e,f)=a*e-b*d
C
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
      call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
C
C ######################################################################
C
C     ******************************************************************
C
C     NORMALIZE THE VECTOR k2-k1.
C
      xa=xic(k1)
      ya=yic(k1)
      za=zic(k1)
      dlen=sqrt((xic(k2)-xa)**2+(yic(k2)-ya)**2+(zic(k2)-za)**2)
      fac=1.0/(dlen + epsilon)
      xb=(xic(k2)-xa)*fac
      yb=(yic(k2)-ya)*fac
      zb=(zic(k2)-za)*fac
      xl=0.5*xb
      yl=0.5*yb
      zl=0.5*zb
C
C     ******************************************************************
C
C     MULTIPLY OTHER PERTINENT VECTORS BY THE NORMALIZING FACTOR AND
C     FIND THE DISTANCE TO THE CENTER OF EACH CIRCLE ALONG THE
C     PERPENDICULAR BISECTOR OF k2-k1.
C
      xc=(xic(n1)-xa)*fac
      yc=(yic(n1)-ya)*fac
      zc=(zic(n1)-za)*fac
      x1=crosx(xb,yb,zb,xc,yc,zc)
      y1=crosy(xb,yb,zb,xc,yc,zc)
      z1=crosz(xb,yb,zb,xc,yc,zc)
      if((x1**2+y1**2+z1**2).le.atolerance) then
         ztmp=1.0
      else
         ztmp=0.0
      endif
      x2=crosx(x1,y1,z1,xb,yb,zb)
      y2=crosy(x1,y1,z1,xb,yb,zb)
      z2=crosz(x1,y1,z1,xb,yb,zb)
      dist1=(0.5*(xc**2+yc**2+zc**2)-(xc*xl+yc*yl+zc*zl))/
     *      (xc*x2+yc*y2+zc*z2+asmallnumber)
      dist1=abs(dist1)*sqrt(x2*x2+y2*y2+z2*z2)+ztmp*alargenumber
      xd=(xic(n2)-xa)*fac
      yd=(yic(n2)-ya)*fac
      zd=(zic(n2)-za)*fac
      x3=crosx(xb,yb,zb,xd,yd,zd)
      y3=crosy(xb,yb,zb,xd,yd,zd)
      z3=crosz(xb,yb,zb,xd,yd,zd)
      if((x3**2+y3**2+z3**2).le.atolerance) then
         ztmp=1.0
      else
         ztmp=0.0
      endif
      x4=crosx(x3,y3,z3,xb,yb,zb)
      y4=crosy(x3,y3,z3,xb,yb,zb)
      z4=crosz(x3,y3,z3,xb,yb,zb)
      dist2=(0.5*(xd**2+yd**2+zd**2)-(xd*xl+yd*yl+zd*zl))/
     *      (xd*x4+yd*y4+zd*z4+asmallnumber)
      dist2=abs(dist2)*sqrt(x4*x4+y4*y4+z4*z4)+ztmp*alargenumber
C
      dmin=  min(dist1,dist2)
      ichoice=1
      if (dmin .eq. dist2) ichoice=2
C
      goto 9999
 9999 continue
      return
      end
