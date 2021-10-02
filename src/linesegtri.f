C
C     $Log: linesegtri.f,v $
C     Revision 2.00  2007/11/05 19:46:00  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   17 Jul 2000 15:19:10   bap
CPVCS    Modified the code to more accurately report the intersection point in the 
CPVCS    case when one of the points is within the triangle.
CPVCS    
CPVCS       Rev 1.5   10 Jul 2000 11:09:30   bap
CPVCS    Changed return value to 2 if the line intersects one of the 
CPVCS    edges; 1 if one of the endpoints of the line is inside the 
CPVCS    triangle.
CPVCS    
CPVCS       Rev 1.4   21 Jun 2000 11:41:30   bap
CPVCS    Modified to be more robust, changed line definition to be local to 
CPVCS    the triangle, not the origin. Miscellaneous error checking added.
CPVCS    
CPVCS       Rev 1.3   22 Mar 2000 08:44:32   dcg
CPVCS    use local_eps in place of epsilon
CPVCS
CPVCS       Rev 1.2   06 Jan 2000 14:28:48   bap
CPVCS    Corrected errors regarding intersection of a large line with a tri.
CPVCS    Corrected errors regarding unitization of directional line
CPVCS    Removed dependence on inv3x3.f
CPVCS
CPVCS       Rev 1.1   Wed Sep 01 12:40:38 1999   dcg
CPVCS    use implicit none - fix undefined use of eps
CPVCS
CPVCS       Rev 1.0   Wed Aug 04 10:13:36 1999   bap
CPVCS    Initial revision.
C
*dk,linesegtri
      subroutine lineseg_tri(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                      xa,ya,za,xb,yb,zb,
     *                      x,y,z,iflag)
     &    bind (C, name="lineseg_tri_")
C
C ######################################################################
C
      use, intrinsic :: ISO_C_BINDING
      implicit none
      real*8 local_eps,releps,xa,xb,ya,yb,za,zb,dist1,dist2,det,
     *  xsum,area31,ax31,ay31,az31,area23,ax23,ay23,az23,area12,
     *  area123,ax12,ay12,az12,ax123,ay123,az123,dsum,dsb,dsa,
     *  xunit,yunit,zunit,dsab,b3,a31,a32,a33,b2,a21,a22,a23,
     *  b1,a13,a12,a11,cx,cy,cz,x,y,z,x1,y1,z1,x2,y2,z2,x3,y3,z3
      real*8 a1norm, a2norm, a3norm

      integer iflag, i, ierror
      include "local_element.h"
      real*8 xnodes(3),ynodes(3),znodes(3)
      real*8 lennorm
      real*8 triarea
      character*132 warnmess
C
C ######################################################################
C
      local_eps = 1e-8
C
C
C     Calculate the plane defined by the triangle
      cx=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      cy=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
      cz=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
      triarea=0.5*sqrt(cx**2+cy**2+cz**2)
C
C     Releps = local_eps*.5*(length of line+area of triangle)
      releps = local_eps*(sqrt((xa-xb)**2+(ya-yb)**2+(za-zb)**2)+
     &         triarea)
 
C
C     Create a matrix with the first row the equation of the plane
C     of the triangle, the second and third rows the equations of
C     two planes that make up the line.
C
      a11=cx
      a12=cy
      a13=cz
C     Normalize the first row of the matrix to length 1
      a1norm=sqrt(a11**2+a12**2+a13**2)
      a11=a11/a1norm
      a12=a12/a1norm
      a13=a13/a1norm

C     Do the point on the plane
      b1=(cx*x1+cy*y1+cz*z1)/a1norm

      a21=0.0
      a22=0.0
      a23=0.0
      b2=0.0
      a31=0.0
      a32=0.0
      a33=0.0
      b3=0.0
      dsab=sqrt((xa-xb)**2+(ya-yb)**2+(za-zb)**2)
      if(abs(xa-xb).gt.local_eps*dsab) then
         a21=-(ya-yb)
         a22=  xa-xb
         a23=0.0
         b2=-xb*(ya-yb)+yb*(xa-xb)
         a31=-(za-zb)
         a32=0.0
         a33=  xa-xb
         b3=-xb*(za-zb)+zb*(xa-xb)
      elseif(abs(ya-yb).gt.local_eps*dsab) then
         a21=-(ya-yb)
         a22=  xa-xb
         a23=0.0
         b2=-xb*(ya-yb)+yb*(xa-xb)
         a31=0.0
         a32=-(za-zb)
         a33=  ya-yb
         b3=-yb*(za-zb)+zb*(ya-yb)
      elseif(abs(za-zb).gt.local_eps*dsab) then
         a21=-(za-zb)
         a22=0.0
         a23=  xa-xb
         b2=-xb*(za-zb)+zb*(xa-xb)
         a31=0.0
         a32=-(za-zb)
         a33=  ya-yb
         b3=-yb*(za-zb)+zb*(ya-yb)
      endif
C
C     Normalize the bottom two rows of a and apply the corresponding 
C     transformations to b
      a2norm=sqrt(a21**2+a22**2+a23**2)
      a3norm=sqrt(a31**2+a32**2+a33**2)
C     Row 2
      a21=a21/a2norm
      a22=a22/a2norm
      a23=a23/a2norm
      b2 =b2/a2norm
C     Row 3
      a31=a31/a3norm
      a32=a32/a3norm
      a33=a33/a3norm
      b3 =b3/a3norm
C
C     Make sure the matrix isn't singular, if it is, figure out how far away
C     from the plane the parallel line is. If the line is within releps
C     it's close enough to be considered in the plane
C
C     Calculate the determinant of the line-plane matrix
C
      det = a11*(a22*a33-a23*a32)-a12*(a21*a33-a23*a31)+
     &      a13*(a21*a32-a22*a31)
      if(abs(det).le.local_eps) then
C
C     Find the unit normal to the plane
         lennorm = sqrt(cx**2+cy**2+cz**2)
         xunit = cx/lennorm
         yunit = cy/lennorm
         zunit = cz/lennorm
C     We think we're parallel, make sure...
C     Projection of the line between a point on the plane and a
C     point on the line onto the normal gives us the distance
         dist1 = xunit*(xa-x1)+yunit*(ya-y1)+zunit*(za-z1)
         dist2 = xunit*(xb-x1)+yunit*(yb-y1)+zunit*(zb-z1)
         if(((abs(dist1).le.releps).OR.(abs(dist2).le.releps))
     $        .AND.(abs(dist1-dist2).le.releps)) then
C     Figure out if either of the two points are within the triangle
C     or not.
            xnodes(1)=x1
            ynodes(1)=y1
            znodes(1)=z1
            xnodes(2)=x2
            ynodes(2)=y2
            znodes(2)=z2
            xnodes(3)=x3
            ynodes(3)=y3
            znodes(3)=z3
            call inside_element(ifelmtri, xnodes, ynodes, znodes,
     $           xa, ya, za, iflag)
            if(iflag.ge.0) then
               x = xa
               y = ya
               z = za
               iflag = 1
               goto 9999
            endif
            call inside_element(ifelmtri, xnodes, ynodes, znodes,
     $           xb, yb, zb, iflag)
            if(iflag.ge.0) then
               x = xb
               y = yb
               z = zb
               iflag = 1
               goto 9999
            endif
C           Ok, the points are not inside the triangle, let's see if the
C           line intersects any of the sides of the triangle
            do i = 0,2
               call lineseg_lineseg(xnodes(mod(i,3)+1),
     &                              ynodes(mod(i,3)+1),
     &                              znodes(mod(i,3)+1),
     &                              xnodes(mod(i+1,3)+1),
     &                              ynodes(mod(i+1,3)+1),
     &                              znodes(mod(i+1,3)+1),
     &                              xa,ya,za,
     &                              xb,yb,zb,
     &                              iflag)
               if(iflag.ge.0) then
                  iflag = 2
                  goto 9999
               endif
            enddo
         elseif(abs(dist1-dist2).lt.releps) then
            iflag = -1
         else
C           Alright, who's making the pathological case?
C           Try and do the best you can with it & warn the user...
            write(warnmess, '(a)')
     $      'Warning! You may have a pathological case in your problem.'
            call writloga('default',0,warnmess,0,ierror)
            write(warnmess, '(a)')
     $           'Examine your output for inconsistencies.'
            call writloga('default',0,warnmess,0,ierror)
            iflag = -1
         endif
         goto 9999
      endif
      iflag=0
C
C     The matrix is not singular, now solve for x, y, z (i.e.,
C     where the plane intersects the line)
C
C     Solve for x (replace a{1,2,3}1 with b{1,2,3})
      x = (b1*(a22*a33-a23*a32)-a12*(b2*a33-a23*b3)+
     &      a13*(b2*a32-a22*b3))/det
C     Solve for y (replace a{1,2,3}2 with b{1,2,3})
      y = (a11*(b2*a33-a23*b3)-b1*(a21*a33-a23*a31)+
     &      a13*(a21*b3-b2*a31))/det
C     Solve for z (replace a{1,2,3}3 with b{1,2,3})
      z = (a11*(a22*b3-b2*a32)-a12*(a21*b3-b2*a31)+
     &      b1*(a21*a32-a22*a31))/det
      if(iflag.eq.0) then
         dsa=sqrt((x-xa)**2+(y-ya)**2+(z-za)**2)
         dsb=sqrt((x-xb)**2+(y-yb)**2+(z-zb)**2)
         dsum=dsa+dsb
         ax123=  (y2-y1)*(z3-z1)-(y3-y1)*(z2-z1)
         ay123=-((x2-x1)*(z3-z1)-(x3-x1)*(z2-z1))
         az123=  (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)
         area123=0.5*sqrt(ax123**2+ay123**2+az123**2)
         ax12=  (y2-y1)*(z-z1)-(y-y1)*(z2-z1)
         ay12=-((x2-x1)*(z-z1)-(x-x1)*(z2-z1))
         az12=  (x2-x1)*(y-y1)-(x-x1)*(y2-y1)
         area12=0.5*sqrt(ax12**2+ay12**2+az12**2)
         ax23=  (y3-y2)*(z-z2)-(y-y2)*(z3-z2)
         ay23=-((x3-x2)*(z-z2)-(x-x2)*(z3-z2))
         az23=  (x3-x2)*(y-y2)-(y3-y2)*(x-x2)
         area23=0.5*sqrt(ax23**2+ay23**2+az23**2)
         ax31=  (y1-y3)*(z-z3)-(y-y3)*(z1-z3)
         ay31=-((x1-x3)*(z-z3)-(x-x3)*(z1-z3))
         az31=  (x1-x3)*(y-y3)-(x-x3)*(y1-y3)
         area31=0.5*sqrt(ax31**2+ay31**2+az31**2)
         xsum=area12+area23+area31
         if(abs(dsab-dsum).le.local_eps*dsab .and.
     *      abs(area123-xsum).le.local_eps*area123) then
            iflag=0
         else
            iflag=-1
         endif
      endif
      goto 9999
 9999 continue
      return
      end
 
 
