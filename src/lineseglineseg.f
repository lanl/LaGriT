C
C     $Log:   /pvcs.config/t3d/src/lineseglineseg.f_a  $
CPVCS    
CPVCS       Rev 1.1   22 Mar 2000 08:42:54   dcg
CPVCS    remove unused epsilon variable
CPVCS
CPVCS       Rev 1.0   Wed Aug 04 10:10:02 1999   bap
CPVCS    Initial revision.
C
*DK,lineseglineseg
      subroutine lineseg_lineseg(x1,y1,z1,x2,y2,z2,
     &                           xa,ya,za,xb,yb,zb,
     &                           iflag)
C
C######################################################################
C
C     PURPOSE -
C        Wrapper for lineseg_lineseg_info
C
C     NOTES -
C        There are times you wish that fortran had the ability to
C        distinguish functions based on the number of arguments...
C
C######################################################################
C
C
      implicit real*8 (a-h, o-z)
      call lineseg_lineseg_info(x1,y1,z1,x2,y2,z2,
     &                          xa,ya,za,xb,yb,zb,
     &                          xjunk,yjunk,zjunk,
     &                          iflag)
      return
      end
C
C######################################################################
C
*DK,lineseglineseginfo
      subroutine lineseg_lineseg_info(x1,y1,z1,x2,y2,z2,
     &                                xa,ya,za,xb,yb,zb,
     &                                xcommon,ycommon,zcommon,
     &                                iflag)
C
C######################################################################
C
C     PURPOSE -
C        This routine finds out if two line segments intersect, and
C        if they do, finds their point of intersection.
C
C     NOTES -
C        This code calls line_line_info to find out if the lines
C        the segments form intersect, and if so, what the point of
C        intersection is.
C
C     INPUT ARGUMENTS -
C        x1,y1,z1; x2,y2,z2  Coordinates of the ends of Line Segment 1
C        xa,ya,za; xb,yb,zb  Coordinates of the ends of Line Segment 2
C
C     OUTPUT ARGUMENTS -
C        xcommon, ycommon, zcommon
C                            Coordinates of the point of intersection
C        iflag               Returns 0 if the line segments intersect
C                            at a point; 1 if the line segments are
C                            colinear and overlap; and -1 if the lines
C                            are skew, parallel and not colinear, or
C                            colinear but do not overlap.
C
C######################################################################
C
C     Variable Declaration
C
      implicit real*8 (a-h, o-z)
      include "local_element.h"
C
C     Distances used to find out if the line segments overlap or if
C     they are mutually exclusive.
C
      real*8 xendpt(2), yendpt(2), zendpt(2)
C
C     *****************************************************************
C
C     Initialize Variables
C
      iflag = 0
C
C######################################################################
C
C     Find out if the lines that the line segments form intersect at
C     all.
C
      call line_line_info(x1,y1,z1,x2,y2,z2,
     &                    xa,ya,za,xb,yb,zb,
     &                    xcommon,ycommon,zcommon,
     &                    iflag)
C
C     If iflag is zero, the lines intersect only in one place.
C     find out if that place is inside both of the line segments.
      if(iflag.eq.0) then
         xendpt(1) = x1
         xendpt(2) = x2
         yendpt(1) = y1
         yendpt(2) = y2
         zendpt(1) = z1
         zendpt(2) = z2
         call inside_element(ifelmlin, xendpt, yendpt, zendpt,
     &                       xcommon, ycommon, zcommon, iflag)
         if(iflag.ge.0) then
            xendpt(1) = xa
            xendpt(2) = xb
            yendpt(1) = ya
            yendpt(2) = yb
            zendpt(1) = za
            zendpt(2) = zb
            call inside_element(ifelmlin, xendpt, yendpt, zendpt,
     &                          xcommon, ycommon, zcommon, iflag)
            if(iflag.ge.0) then
               iflag = 0
               goto 9999
            endif
         endif
         iflag = -1
         goto 9999
      elseif (iflag.gt.0) then
C        Now, the lines are colinear, but who knows if they overlap
C        See if either of the points of line 1
         xendpt(1) = x1
         xendpt(2) = x2
         yendpt(1) = y1
         yendpt(2) = y2
         zendpt(1) = z1
         zendpt(2) = z2
         call inside_element(ifelmlin, xendpt, yendpt, zendpt,
     &                       xa, ya, za, iflag)
         if(iflag.ge.0) then
            iflag = 1
            goto 9999
         endif
         call inside_element(ifelmlin, xendpt, yendpt, zendpt,
     &                       xb, yb, zb, iflag)
         if(iflag.ge.0) then
            iflag = 1
            goto 9999
         endif
         xendpt(1) = xa
         xendpt(2) = xb
         yendpt(1) = ya
         yendpt(2) = yb
         zendpt(1) = za
         zendpt(2) = zb
         call inside_element(ifelmlin, xendpt, yendpt, zendpt,
     &                       x1, y1, z1, iflag)
         if(iflag.ge.0) then
            iflag = 1
            goto 9999
         endif
         iflag = -1
      endif
C
C     We're Done
 9999 continue
      return
      end
C
C######################################################################
C
*DK,lineseglineinfo
      subroutine lineseg_line_info(x1,y1,z1,x2,y2,z2,
     &                                xa,ya,za,xb,yb,zb,
     &                                xcommon,ycommon,zcommon,
     &                                iflag)
C
C######################################################################
C
C     PURPOSE -
C        This routine finds out if a line segment intersects with an
C        infinite line, and if it does, finds the point of
C        intersection.
C
C     NOTES -
C        This code calls line_line_info to find out if the lines
C        the segments form intersect, and if so, what the point of
C        intersection is.
C
C     INPUT ARGUMENTS -
C        x1,y1,z1; x2,y2,z2  Coordinates of the ends of Line Segment 1
C        xa,ya,za; xb,yb,zb  Coordinates of the defining points of
C                            the line
C
C     OUTPUT ARGUMENTS -
C        xcommon, ycommon, zcommon
C                            Coordinates of the point of intersection
C        iflag               Returns 0 if the line segments intersect
C                            at a point; 1 if the line segments is
C                            colinear with the line; and -1 if the
C                            lines are skew, or do not intersect.
C
C######################################################################
C
C     Variable Declaration
C
      implicit real*8 (a-h, o-z)
      include "local_element.h"
C
C     Points used to find out if the line segments overlap or if
C     they are mutually exclusive.
C
      real*8 xendpt(2), yendpt(2), zendpt(2)
C
C     *****************************************************************
C
C     Initialize Variables
C
      iflag = 0
C
C######################################################################
C
C     Find out if the lines that the line segments form intersect at
C     all.
C
      call line_line_info(x1,y1,z1,x2,y2,z2,
     &                    xa,ya,za,xb,yb,zb,
     &                    xcommon,ycommon,zcommon,
     &                    iflag)
C
C     If iflag is zero, the lines intersect only in one place.
C     find out if that place is the line segment.
      if(iflag.eq.0) then
         xendpt(1) = x1
         xendpt(2) = x2
         yendpt(1) = y1
         yendpt(2) = y2
         zendpt(1) = z1
         zendpt(2) = z2
         call inside_element(ifelmlin, xendpt, yendpt, zendpt,
     &                       xcommon, ycommon, zcommon, iflag)
      endif
C
C     Otherwise, the rest of the cases are properly taken care of in
C     line_line_info()
C
      return
      end
 
