C
C    $Log:   /pvcs.config/t3d/src/lineline.f_a  $
CPVCS    
CPVCS       Rev 1.4   23 Jun 2006 08:33:34   tam
CPVCS    used implicit none
CPVCS    changed local_epsilon from 1.0d-08 to 1.0d-10
CPVCS    iflag returns neg values unique to place in code
CPVCS    where the error occurs, at end returns -1
CPVCS    
CPVCS       Rev 1.3   22 Mar 2000 08:41:46   dcg
CPVCS     use local_epsilon in place of epsilon
CPVCS
CPVCS       Rev 1.2   06 Jan 2000 14:26:00   bap
CPVCS    Corrected errors regarding intersections, fixed problem with
CPVCS    singular determinants.
CPVCS
CPVCS       Rev 1.1   Wed Aug 04 10:07:32 1999   bap
CPVCS    Added PVCS header
C
*DK,lineline
      subroutine line_line(x1,y1,z1,x2,y2,z2,
     &                     xa,ya,za,xb,yb,zb,
     &                     iflag)
C
C######################################################################
C
C     PURPOSE -
C        Wrapper for line_line_info
C
C     NOTES -
C        There are times you wish that fortran had the ability to
C        distinguish functions based on the number of arguments...
C
C######################################################################
C
C
C     implicit real*8 (a-h, o-z)
      implicit none

      integer iflag
      real*8 x1,y1,z1,x2,y2,z2
      real*8 xa,ya,za,xb,yb,zb,xjunk,yjunk,zjunk

      call line_line_info(x1,y1,z1,x2,y2,z2,
     &                    xa,ya,za,xb,yb,zb,
     &                    xjunk,yjunk,zjunk,
     &                    iflag)
      return
      end
C
C######################################################################
C
*DK,linelineinfo
      subroutine line_line_info(x1,y1,z1,x2,y2,z2,
     &                          xa,ya,za,xb,yb,zb,
     &                          xcommon,ycommon,zcommon,
     &                          iflag)
C
C######################################################################
C
C     PURPOSE -
C        This routine finds out if two lines in space intersect, and
C        if they do, finds their point of intersection.
C
C     NOTES -
C
C     INPUT ARGUMENTS -
C        x1,y1,z1; x2,y2,z2  Defining points of line 1
C        xa,ya,za; xb,yb,zb  Defining points of line 2
C
C     OUTPUT ARGUMENTS -
C        xcommon, ycommon, zcommon
C                            Coordinates of the point of intersection
C        iflag               Returns 0 if the lines intersect at a
C                            point; 1 if the lines are colinear and
C                            overlap; and -1 if the lines are skew or
C                            parallel and not colinear.
C
C
C######################################################################
C
C     Variable Declaration
C
C     implicit real*8 (a-h, o-z)
      implicit none

      integer ierror, iflag

      real*8 x1,y1,z1,x2,y2,z2,a1dot21
      real*8 xa,ya,za,xb,yb,zb,xcommon,ycommon,zcommon
C
C     Variables used to store three necessary unit vectors: Two
C     describing the lines and one describing the perpendicular needed
C     to translate line 21 to line ab.
C     Also necessary is a magnitude for the displacement.
C
      real*8 xu21, yu21, zu21
      real*8 mag21
      real*8 xuba, yuba, zuba
      real*8 magba
      real*8 xudisp, yudisp, zudisp
      real*8 dispmag, disphalf
C
C     In order to get the displacement vector's length, and to
C     find the point of intersection between these lines (if it
C     exists) we need to do some calculations that involve vectors
C     from point 1 to points a and b
      real*8 xa1, ya1, za1
      real*8 xb1, yb1, zb1
C
C     Other useful things in the event that lines ab and 12 are
C     parallel.
      real*8 xua1, yua1, zua1
      real*8 maga1
C
C     After this, we need to find a unit perpendicular to line 21
      real*8 xuperp21, yuperp21, zuperp21
      real*8 perp21mag
C
C     Finally, we need to find the ratio of similar triangles used
C     calculate the intersection point. (This is really a fractional
C     distance.)
      real*8 P1,P2,ratio

      real*8 local_epsilon
      parameter(local_epsilon=1.0d-10)

      character*132 logmess
C
C######################################################################
C
C     Initialize variables
C
      iflag = 0
C
C     *****************************************************************
C     Get the unit vectors for the lines squared away, as well as the
C     other vectors that rely solely on the points
C
C     Unit vector 21
      xu21 = x2-x1
      yu21 = y2-y1
      zu21 = z2-z1
      mag21 = sqrt(xu21**2+yu21**2+zu21**2)
      if (mag21.le.local_epsilon) then
         write(logmess,'(a)')
     &     'Error in subroutine line_line: line 21 is indeterminate!'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
      xu21 = xu21/mag21
      yu21 = yu21/mag21
      zu21 = zu21/mag21
C
C     Unit Vector ba
      xuba = xb-xa
      yuba = yb-ya
      zuba = zb-za
      magba = sqrt(xuba**2+yuba**2+zuba**2)
      if (magba.le.local_epsilon) then
         write(logmess,'(a)')
     &     'Error in subroutine line_line: line ba is indeterminate!'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
      xuba = xuba/magba
      yuba = yuba/magba
      zuba = zuba/magba
C
C     Vectors from point 1 to point a and from point 1 to point b
      xa1 = xa-x1
      ya1 = ya-y1
      za1 = za-z1
      xb1 = xb-x1
      yb1 = yb-y1
      zb1 = zb-z1
C
C     *****************************************************************
C     Now comes the fun part... Find out if the lines are skew by
C     finding the cross product of the two unit vectors describing them
C     and dotting it with the vector from point a to point 1. If the
C     magnitude of the cross product is zero, then the lines are
C     parallel.  Then one needs to find whether or not they are
C     colinear. If they are, then the vector a1 will be C*vector 21
C
      xudisp =  (yu21*zuba)-(zu21*yuba)
      yudisp =-((xu21*zuba)-(zu21*xuba))
      zudisp =  (xu21*yuba)-(yu21*xuba)
      dispmag = sqrt(xudisp**2+yudisp**2+zudisp**2)
      if (dispmag.le.local_epsilon) then
C        the lines are parallel
C        See if the line 1a is parallel to line 12, if they are, then
C        we have colinear lines
         maga1=sqrt(xa1**2+ya1**2+za1**2)
         if(maga1.le.local_epsilon) then
C           the lines are colinear
            iflag = 1
            goto 9999
         endif
C        Otherwise, we're still not sure...
         xua1=xa1/maga1
         yua1=ya1/maga1
         zua1=za1/maga1
C        Test for parallel or anti-parallel vectors
         if(((abs(xua1-xu21).le.local_epsilon).AND.
     &       (abs(yua1-yu21).le.local_epsilon).AND.
     &       (abs(zua1-zu21).le.local_epsilon)).OR.
     &      ((abs(xua1+xu21).le.local_epsilon).AND.
     &       (abs(yua1+yu21).le.local_epsilon).AND.
     &       (abs(zua1+zu21).le.local_epsilon))) then
C           the lines are colinear
            iflag = 1
            goto 9999
         else
C           the lines are parallel, but not colinear
            iflag = -2
            goto 9999
         endif
      endif
C
C     Now we know we have non-parallel lines.
      xudisp = xudisp/dispmag
      yudisp = yudisp/dispmag
      zudisp = zudisp/dispmag
C
C     Dot vector a1 with the displacement vector to find out if the
C     lines are skew or not. A non-zero result implies that the lines
C     are skew and lines are outside
      dispmag = xudisp*xa1+yudisp*ya1+zudisp*za1
      disphalf = (mag21+magba)*.50d+00
      if(abs(dispmag).gt.(local_epsilon*disphalf)) then
C        the lines are skew
C        this test may return skew lines for some elements located
C        at coordinates with 7 digits or more
C        translating to zero seems to avoid this problem
C        but would like a different or additional test before
C        throwing this set of lines out of consideration  -tam
         iflag = -3
         goto 9999
      endif
C
C     *****************************************************************
C     At this point we have determined that the lines are not skew or
C     parallel, so we can go on to find their point of intersection.
C
C     Find the perpendicular part of vector a1 with respect to vector
C     21 by subtracting the dot product of vector a1 with the unit
C     vector 21 from vector a1
      a1dot21 = (xa1*xu21+ya1*yu21+za1*zu21)
      xuperp21 = xa1 - a1dot21*xu21
      yuperp21 = ya1 - a1dot21*yu21
      zuperp21 = za1 - a1dot21*zu21
      perp21mag = sqrt(xuperp21**2+yuperp21**2+zuperp21**2)
      if (perp21mag.le.local_epsilon) then
C        the lines intersect at "point a and point 1"
         xcommon = x1
         ycommon = y1
         zcommon = z1
         iflag = 0
         goto 9999
      endif
      xuperp21 = xuperp21/perp21mag
      yuperp21 = yuperp21/perp21mag
      zuperp21 = zuperp21/perp21mag
      P1 = perp21mag
      P2 = xb1*xuperp21+yb1*yuperp21+zb1*zuperp21
      if(abs(P1-P2).lt.local_epsilon) then
         if(abs(P1).lt.local_epsilon) then
            iflag = 1
            goto 9999
         else
            iflag = -4
            goto 9999
         endif
      endif
      ratio = P1/(P1-P2)
      xcommon = xa+magba*ratio*xuba
      ycommon = ya+magba*ratio*yuba
      zcommon = za+magba*ratio*zuba
      iflag = 0
 9999 continue
      if (iflag.lt.0) iflag = -1
      return
      end
 
 
