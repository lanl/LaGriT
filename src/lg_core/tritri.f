C
C     $Log: tritri.f,v $
C     Revision 2.00  2007/11/09 20:04:05  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   03 Jan 2007 12:31:56   tam
CPVCS    save iflag and pass itmp to routines instead
CPVCS    
CPVCS       Rev 1.5   12 Oct 2005 14:41:14   gable
CPVCS    Added implicit none and variable declarations.
CPVCS    Minor changes to log messages.
CPVCS    
CPVCS       Rev 1.4   22 Mar 2000 08:30:48   dcg
CPVCS    replace epsilon with local_epsilon
CPVCS
CPVCS       Rev 1.3   Tue Nov 30 09:13:34 1999   tam
CPVCS    changed ztri index from 2 to i (bap changes)
CPVCS
CPVCS       Rev 1.2   Tue Aug 10 14:34:12 1999   dcg
CPVCS    same as previous change
CPVCS
CPVCS       Rev 1.1   Tue Aug 10 14:29:50 1999   dcg
CPVCS    get rid of code that changes value of do loop index
CPVCS    look for goto 50 and 60 to find loops that are changed
CPVCS
CPVCS       Rev 1.0   Wed Aug 04 10:38:32 1999   bap
CPVCS    Initial revision.
C
*dk,tritri
      subroutine tri_tri(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                   xa,ya,za,xb,yb,zb,xc,yc,zc,
     *                   iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        This subroutine checks for the intersection of two triangles.
C        It calculates the planes for the triangles, calculates the
C        line of intersection, and then figures out whether or not
C        the triangles actually intersect.
C
C     NOTES -
C
C        The equations of the planes will be stored in the following
C        format:
C        a1(x)+b1(y)+c1(z) = d1      Plane 1
C        a2(x)+b2(y)+c2(z) = d2      Plane 2
C        The equation of the line will be stored in the following
C        format
C        <xdir, ydir, zdir>          vector || to the line
C        (xpt, ypt, zpt)             Point on the line
C
C
C     INPUT ARGUMENTS -
C        x1,y1,z1; x2,y2,z2; x3,y3,z3  Coordinates of Triangle 1
C        xa,ya,za; xb,yb,zb; xc,yc,zc  Coordinates of Triangle 2
C
C     OUTPUT ARGUMENTS -
C        iflag returns -1 if the triangles do not intersect, 0 if
C        they do.
C
C######################################################################
C
C     Variable Declarations
C
      implicit none
      include "local_element.h"
C
C     Plane coefficients and values:
      real*8 a1,b1,c1,d1
      real*8 a2,b2,c2,d2
C
C     Line direction and point values:
      real*8 xdir, ydir, zdir
      real*8 mag, lennorm, local_epsilon,eps
      real*8 xpt, ypt, zpt
C
C     Triangle intersection points
C     Triangle 1:
      real*8 xendpt1(3), yendpt1(3), zendpt1(3)
C
C     Triangle 2:
      real*8 xendpt2(3), yendpt2(3), zendpt2(3)
C
C     Triangle arrays
      real*8 xtri1(3),xtri2(3)
      real*8 ytri1(3),ytri2(3)
      real*8 ztri1(3),ztri2(3)
C
      integer iflag,ierror,itmp,i,j, idxtri1, idxtri2
      
      real*8 dist, dist_ab, dist_atestpt, dist_btestpt
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3
      real*8 xa,ya,za,xb,yb,zb,xc,yc,zc
      
      character*132 logmess
C
C######################################################################
C
C     Initialize variables
       local_epsilon = 1.0d-10 
C      parameter(local_epsilon=1.0d-10)
C
C
C
C     X-coordinates
      xtri1(1)=x1
      xtri1(2)=x2
      xtri1(3)=x3
      xtri2(1)=xa
      xtri2(2)=xb
      xtri2(3)=xc
C     Y-coordinates
      ytri1(1)=y1
      ytri1(2)=y2
      ytri1(3)=y3
      ytri2(1)=ya
      ytri2(2)=yb
      ytri2(3)=yc
C     Z-coordinates
      ztri1(1)=z1
      ztri1(2)=z2
      ztri1(3)=z3
      ztri2(1)=za
      ztri2(2)=zb
      ztri2(3)=zc
C
      iflag=0
C
C######################################################################
C
C     Figure out the coefficients of the planes by finding the cross
C     product of the two vectors defined by the three points of the
C     triangles. This uses points 1 and a as the tail of the vectors.
C     Then use them to figure out d.
C
C     *****************************************************************
C     Plane 1
      a1=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      b1=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
      c1=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
C     Unitize the directions, ensuring that the triangle has some
C     volume...
      lennorm = sqrt(a1**2+b1**2+c1**2)

C     Check for size compared to local epsilon value
C     epsilon comparisons will not work for extremely small objects
      if (lennorm.lt.local_epsilon) then
         write(logmess,'(a)')
     &  'Warning in tri_tri: Triangle area less than local epsilon'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a,1pe14.5e3,a,1pe14.5e3)')
     &  'local epsilon : ',local_epsilon,' lennorm: ',lennorm
         call writloga('default',0,logmess,0,ierror)

         iflag = -1
         goto 9999
      endif
      a1=a1/lennorm
      b1=b1/lennorm
      c1=c1/lennorm
C
      d1=  a1*x1+b1*y1+c1*z1
C
C     *****************************************************************
C     Plane 2
      a2=  (yb-ya)*(zc-za)-(zb-za)*(yc-ya)
      b2=-((xb-xa)*(zc-za)-(zb-za)*(xc-xa))
      c2=  (xb-xa)*(yc-ya)-(yb-ya)*(xc-xa)
C     Unitize the directions, ensuring that the triangle has some
C     volume...
      lennorm = sqrt(a2**2+b2**2+c2**2)
      if (lennorm.lt.local_epsilon) then
         write(logmess,'(a)')
     &  'Warning in tri_tri: Triangle 2 area less than local epsilon'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a,1pe14.5e3,a,1pe14.5e3)')
     &  'local epsilon : ',local_epsilon,' lennorm: ',lennorm
         call writloga('default',0,logmess,0,ierror)
         iflag = -1
         goto 9999
      endif
      a2=a2/lennorm
      b2=b2/lennorm
      c2=c2/lennorm
C
      d2=  a2*xa+b2*ya+c2*za
C
C     *****************************************************************
C     Now that we have the equations of the planes, figure out the
C     direction of the line that they create by taking their cross
C     product.
C
      xdir=   b1*c2-c1*b2
      ydir= -(a1*c2-c1*a2)
      zdir=   a1*b2-b1*a2
 
C     If the magnitude of the cross product of the two vectors is
C     zero, then the planes are the same, or they are parallel.
C     If this is the case, find the distance between a point on one
C     of them with the other one. If the distance is zero, they are
C     coplanar, find out if any of the points in one of the triangles
C     are in the other, if so return 0, otherwise return -1
C
      mag = sqrt(xdir**2+ydir**2+zdir**2)
      if (mag.lt.local_epsilon) then
C
C     Distance between a plane and a point is the projection of
C     a line between a point on one plane and a point on the other
C     onto the unit normal of one of the planes. Since our coefficients
C     are already in unit vectors, it is just the dot product of the
C     line with the normal of one of the planes.
         dist = a1*(xa-x1)+b1*(ya-y1)+c1*(za-z1)
         if(abs(dist).le.local_epsilon) then
C           Find out whether or not any of the points of either
C           triangle are inside the other one.
            do i = 1,3
               itmp=0
               call inside_element(ifelmtri, xtri1,ytri1,ztri1,
     &                        xtri2(i),ytri2(i),ztri2(i), itmp)
               if(itmp.ge.0) then
                  iflag = 0
                  goto 9999
               endif
            enddo
            do i = 1,3
               itmp=0
               call inside_element(ifelmtri, xtri2,ytri2,ztri2,
     &                        xtri1(i),ytri1(i),ztri1(i), itmp)
               if(itmp.ge.0) then
                  iflag = 0
                  goto 9999
               endif
            enddo
            iflag = -1
            goto 9999
         else
            iflag = -1
            goto 9999
         endif
      endif
C     Unitize the line vector, that way things are easier to deal with
      xdir=xdir/mag
      ydir=ydir/mag
      zdir=zdir/mag
C
C     At this point, we know that the planes intersect (someplace) and
C     we know the direction of the line that defines their intersection
C     Now, all we need is a point on that line.
C
C     First ensure there isn't a problem with dividing by zero. (Silly
C     singular determinants)
C
      if(abs(b1*a2-a1*b2).gt.local_epsilon) then
         zpt = 0.0
         ypt = (d1*a2-a1*d2)/(b1*a2-a1*b2)
         if(abs(a1).gt.local_epsilon) then
            xpt = (d1-b1*ypt)/a1
         else
            xpt = (d2-b2*ypt)/a2
         endif
      elseif(abs(c1*b2-b1*c2).gt.local_epsilon) then
         xpt = 0.0
         zpt = (d1*b2-b1*d2)/(c1*b2-b1*c2)
         if(abs(b1).gt.local_epsilon) then
            ypt = (d1-c1*zpt)/b1
         else
            ypt = (d2-c2*zpt)/b2
         endif
      elseif(abs(a1*c2-c1*a2).gt.local_epsilon) then
         ypt = 0.0
         xpt = (d1*c2-c1*d2)/(a1*c2-c1*a2)
         if(abs(c1).gt.local_epsilon) then
            zpt = (d1-a1*xpt)/c1
         else
            zpt = (d2-a2*xpt)/c2
         endif
      else
C     We have a problem!!!
         write(logmess,'(a)')
     &        'Error in subroutine tri_tri: at least one plane is '
     &        // 'degenerate'
         call writloga('default',0,logmess,0,ierror)
         iflag = -1
         goto 9999
      endif
C
C     Now we have a line that is guaranteed to intersect at least
C     two of the lines of the sides of each of the triangles.
C     Triangle 1
      idxtri1 = 1
      do i = 0,2
         call lineseg_line_info(xtri1(mod(i,3)+1),ytri1(mod(i,3)+1),
     &                          ztri1(mod(i,3)+1),xtri1(mod(i+1,3)+1),
     &                          ytri1(mod(i+1,3)+1),ztri1(mod(i+1,3)+1),
     &                          xpt,ypt,zpt,xpt+xdir,ypt+ydir,zpt+zdir,
     &                          xendpt1(idxtri1), yendpt1(idxtri1),
     &                          zendpt1(idxtri1), iflag)
         if (iflag.eq.0) then
            idxtri1 = idxtri1 + 1
         elseif (iflag.gt.0) then
            xendpt1(1) = xtri1(mod(i,3)+1)
            xendpt1(2) = xtri1(mod(i+1,3)+1)
            yendpt1(1) = ytri1(mod(i,3)+1)
            yendpt1(2) = ytri1(mod(i+1,3)+1)
            zendpt1(1) = ztri1(mod(i,3)+1)
            zendpt1(2) = ztri1(mod(i+1,3)+1)
c           i=2
            idxtri1=3
            goto 50
         endif
      enddo
 50   idxtri1=idxtri1-1
C
C     Triangle 2
      idxtri2 = 1
      do i = 0,2
         call lineseg_line_info(xtri2(mod(i,3)+1),ytri2(mod(i,3)+1),
     &                          ztri2(mod(i,3)+1),xtri2(mod(i+1,3)+1),
     &                          ytri2(mod(i+1,3)+1),ztri2(mod(i+1,3)+1),
     &                          xpt,ypt,zpt,xpt+xdir,ypt+ydir,zpt+zdir,
     &                          xendpt2(idxtri2), yendpt2(idxtri2),
     &                          zendpt2(idxtri2), iflag)
         if (iflag.eq.0) then
            idxtri2 = idxtri2 + 1
         elseif (iflag.gt.0) then
            xendpt2(1) = xtri2(mod(i,3)+1)
            xendpt2(2) = xtri2(mod(i+1,3)+1)
            yendpt2(1) = ytri2(mod(i,3)+1)
            yendpt2(2) = ytri2(mod(i+1,3)+1)
            zendpt2(1) = ztri2(mod(i,3)+1)
            zendpt2(2) = ztri2(mod(i+1,3)+1)
c           i=2
            idxtri2=3
            goto 60
         endif
      enddo
 60   idxtri2=idxtri2-1
      if((idxtri2.gt.3).OR.(idxtri1.gt.3)) then
         write(logmess,'(a)')
     &        'Warning in tri_tri: more than three intersection'
     &        // ' points, unpredicatable results possible'
         call writloga('default',0,logmess,0,ierror)
      endif
C
C
C     Are any of the intersection points from triangle two between
C     the intersection points of triangle 1?
C
      do i=1,idxtri1
         dist_ab = sqrt((xendpt1(mod(i-1,idxtri1)+1)-
     &                   xendpt1(mod(i,idxtri1)+1))**2+
     &                  (yendpt1(mod(i-1,idxtri1)+1)-
     &                   yendpt1(mod(i,idxtri1)+1))**2+
     &                  (zendpt1(mod(i-1,idxtri1)+1)-
     &                   zendpt1(mod(i,idxtri1)+1))**2)
         do j=1,idxtri2
            dist_atestpt = sqrt((xendpt1(mod(i-1,idxtri1)+1)-
     &                           xendpt2(j))**2+
     &                          (yendpt1(mod(i-1,idxtri1)+1)-
     &                           yendpt2(j))**2+
     &                          (zendpt1(mod(i-1,idxtri1)+1)-
     &                           zendpt2(j))**2)
            dist_btestpt = sqrt((xendpt1(mod(i,idxtri1)+1)-
     &                           xendpt2(j))**2+
     &                          (yendpt1(mod(i,idxtri1)+1)-
     &                           yendpt2(j))**2+
     &                          (zendpt1(mod(i,idxtri1)+1)-
     &                           zendpt2(j))**2)
            if(abs(dist_ab-(dist_atestpt+dist_btestpt)).lt.
     &           (local_epsilon*dist_ab)) then
               iflag = 0
               goto 9999
            endif
         enddo
      enddo
C
C     Are any of the intersection points from triangle one between
C     the intersection points of triangle 2?
C
      do i=1,idxtri2
         dist_ab = sqrt((xendpt2(mod(i-1,idxtri2)+1)-
     &                   xendpt2(mod(i,idxtri2)+1))**2+
     &                  (yendpt2(mod(i-1,idxtri2)+1)-
     &                   yendpt2(mod(i,idxtri2)+1))**2+
     &                  (zendpt2(mod(i-1,idxtri2)+1)-
     &                   zendpt2(mod(i,idxtri2)+1))**2)
         do j=1,idxtri1
            dist_atestpt = sqrt((xendpt2(mod(i-1,idxtri2)+1)-
     &                           xendpt1(j))**2+
     &                          (yendpt2(mod(i-1,idxtri2)+1)-
     &                           yendpt1(j))**2+
     &                          (zendpt2(mod(i-1,idxtri2)+1)-
     &                           zendpt1(j))**2)
            dist_btestpt = sqrt((xendpt2(mod(i,idxtri2)+1)-
     &                           xendpt1(j))**2+
     &                          (yendpt2(mod(i,idxtri2)+1)-
     &                           yendpt1(j))**2+
     &                          (zendpt2(mod(i,idxtri2)+1)-
     &                           zendpt1(j))**2)
            if(abs(dist_ab-(dist_atestpt+dist_btestpt)).lt.
     &           (local_epsilon*dist_ab)) then
               iflag = 0
               goto 9999
            endif
         enddo
      enddo
C     Otherwise, no dice...
      iflag = -1
C     We're Finished
 9999 continue
      return
      end
