       subroutine identify_dot(dot,epsilon,id,ierror)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         Copyright, 1996                              C
C This program was prepared by the Regents of the University of        C
C California at Los Alamos National Laboratory (the University) under  C
C Contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). C
C The University has certain rights in the program pursuant to the     C
C contract and the program should not be copied or distributed outside C
C your organization. All rights in the program are reserved by the DOE C
C and the University. Neither the U.S. Government nor the University   C
C makes any warranty, express or implied, or assumes and liability or  C
C responsibility for the use of this software.                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C #####################################################################
C
C     PURPOSE -
C
C         Assign an integer flag value depending on the 6 values passed
C         in the DOT vector. The input is assumed to be the dot product
C         of a vector with the normal to 6 planes. Depending on the values
C         in DOT, determine which direction the vector was pointing.
C
C     INPUT ARGUMENTS -
C
C        dot     - a 6 component vector with the dot product of a
C                  vector with 6 planes.
C                  use routines vector_area() and dot_product_6_planes() to
C                  find vector and values for dot(1:6)
C        epsilon - mininum acceptable value for zero
C
C     OUTPUT ARGUMENTS -
C
C        id   -    integer 1-26 indicating orientation
C                  -1 indicates orientation was not found - shouldn't happen
C        ierror -  integer 0 if nothing wrong, else -1 
C
C       26 possible exterior node types
c
c   1   bottom             1, 4, 3, 2
c   2   top                5, 6, 7, 8
c   3   front (south)      1, 2, 6, 5
c   4   right (east)       2, 3, 7, 6
c   5   back  (north)      3, 4, 8, 7
c   6   left  (west)       1, 5, 8, 4
c   7   bottom front       1,2
c   8   bottom left        1,4
c   9   front  left        1,5
c  10   bottom right       2,3
c  11   front right        2,6
c  12   bottom back        3,4
c  13   right back         3,7
c  14   back left          4,8
c  15   top front          5,6
c  16   top left           5,8
c  17   top right          6,7
c  18   top back           7,8
c  19   bottom front left  1
c  20   bottom front right 2
c  21   bottom back  right 3
c  22   bottom back  left  4
c  23   top    front left  5
c  24   top    front right 6
c  25   top    back  right 7
c  26   top    back  left  8
C
C        IERROR   - error flag (returns zero if no errors)
C
C     AUTHOR
C
C         Carl W. Gable  gable@lanl.gov (Los Alamos National Laboratory)
C
C     CHANGE HISTORY -
C
C        $Log: identify_dot.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Tue Mar 10 14:52:02 1998   tam
CPVCS    moved edge logic after faces, used epsilon for face checks
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:51:32 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Wed May 08 12:38:54 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
      real*8 dot(6), epsilon
      real*8 zero
      real*8 dotl(6)
      integer id
      integer ierror
c      data zero / 0.0d0 / 
 
c BEGIN
      id = 0
      ierror = 0
      zero = epsilon 

c     use epsilon and distance from 0 for comparisons
c     to find very small values that should be zero
      dotl(1) = abs(0.0 - abs(dot(1)))
      dotl(2) = abs(0.0 - abs(dot(2)))
      dotl(3) = abs(0.0 - abs(dot(3)))
      dotl(4) = abs(0.0 - abs(dot(4)))
      dotl(5) = abs(0.0 - abs(dot(5)))
      dotl(6) = abs(0.0 - abs(dot(6)))
      
c      write(*,'(3(2x,e14.6))')dot(1),dot(2),dot(3)
c      write(*,'(3(2x,e14.6))')dot(4),dot(5),dot(6)

c.....find the 12 side facing edges, id's 7-18

c     7   bottom front  1=2=5=6
c     18   top back 
c      if((dotl(3) .le. zero).and.
c     *              (abs((abs(dot(1)) - abs(dot(2)))) .le. zero) .and.
c     *              (abs((abs(dot(2)) - abs(dot(5)))) .le. zero) .and.
c     *              (abs((abs(dot(5)) - abs(dot(6)))) .le. zero) ) then
c         if(dot(4) .lt. 0)id=7
c         if(dot(4) .gt. 0)id=18
c
cc     8   bottom left   1=2=3=4
cc     17   top right
c      elseif((dotl(6) .le. zero).and.
c     *              (abs((abs(dot(1)) - abs(dot(2)))) .le. zero) .and.
c     *              (abs((abs(dot(2)) - abs(dot(3)))) .le. zero) .and.
c     *              (abs((abs(dot(3)) - abs(dot(4)))) .le. zero) ) then
c         if(dot(5) .lt. 0)id=8
c         if(dot(5) .gt. 0)id=17
c
cc     9   front  left
cc     13   right back   3=4=5=6
c      elseif((dotl(1) .le. zero).and.
c     *              (abs((abs(dot(3)) - abs(dot(4)))) .le. zero) .and.
c     *              (abs((abs(dot(4)) - abs(dot(5)))) .le. zero) .and.
c     *              (abs((abs(dot(5)) - abs(dot(6)))) .le. zero) ) then
c         if(dot(2) .lt. 0)id=9
c         if(dot(2) .gt. 0)id=13
c
cc     10   bottom right 1=2=3=4
cc     16   top left
c      elseif((dotl(5) .le. zero) .and.
c     *              (abs((abs(dot(1)) - abs(dot(2)))) .le. zero) .and.
c     *              (abs((abs(dot(2)) - abs(dot(3)))) .le. zero) .and.
c     *              (abs((abs(dot(3)) - abs(dot(4)))) .le. zero)) then
c         if(dot(6) .lt. 0)id=10
c         if(dot(6) .gt. 0)id=16
c
cc     14   back left    3=4=5=6
cc     11   front right
c      elseif((dotl(2) .le. zero).and.
c     *              (abs((abs(dot(3)) - abs(dot(4)))) .le. zero) .and.
c     *              (abs((abs(dot(4)) - abs(dot(5)))) .le. zero) .and.
c     *              (abs((abs(dot(5)) - abs(dot(6)))) .le. zero)) then
c         if(dot(1) .lt. 0)id=14
c         if(dot(1) .gt. 0)id=11
c
cc     12   bottom back  1=2=5=6
cc     15   top front
c      elseif((dotl(4) .le. zero).and.
c     *              (abs((abs(dot(1)) - abs(dot(2)))) .le. zero) .and.
c     *              (abs((abs(dot(2)) - abs(dot(5)))) .le. zero) .and.
c     *              (abs((abs(dot(5)) - abs(dot(6)))) .le. zero) ) then
c         if(dot(3) .lt. 0)id=12
c         if(dot(3) .gt. 0)id=15
c
c
cc.....find the 6 faces, id's 1-6

c     1   bottom
      if((dot(3).lt.0 .and. dotl(3).gt.zero)  .and.
     *       (dot(4).lt.0 .and. dotl(4).gt.zero)  .and.
     *       (dot(5).lt.0 .and. dotl(5).gt.zero)  .and.
     *       (dot(6).lt.0 .and. dotl(6).gt.zero)  )then
         id = 1
c     2   top
      elseif((dot(3).gt.0 .and. dotl(3).gt.zero)  .and.
     *       (dot(4).gt.0 .and. dotl(4).gt.zero)  .and.
     *       (dot(5).gt.0 .and. dotl(5).gt.zero)  .and.
     *       (dot(6).gt.0 .and. dotl(6).gt.zero)  )then
         id = 2
c     3   front (south)
      elseif((dot(1).gt.0 .and. dotl(1).gt.zero)  .and.
     *       (dot(2).lt.0 .and. dotl(2).gt.zero)  .and.
     *       (dot(3).gt.0 .and. dotl(3).gt.zero)  .and.
     *       (dot(4).lt.0 .and. dotl(4).gt.zero)  )then
         id = 3
c     4   right (east)
      elseif((dot(1).gt.0 .and. dotl(1).gt.zero)  .and.
     *       (dot(2).gt.0 .and. dotl(2).gt.zero)  .and.
     *       (dot(5).gt.0 .and. dotl(5).gt.zero)  .and.
     *       (dot(6).lt.0 .and. dotl(6).gt.zero)  )then
         id = 4
c     5   back  (north)
      elseif((dot(1).lt.0 .and. dotl(1).gt.zero)  .and.
     *       (dot(2).gt.0 .and. dotl(2).gt.zero)  .and.
     *       (dot(3).lt.0 .and. dotl(3).gt.zero)  .and.
     *       (dot(4).gt.0 .and. dotl(4).gt.zero)  )then
         id = 5
c     6   left  (west)
      elseif((dot(1).lt.0 .and. dotl(1).gt.zero)  .and.
     *       (dot(2).lt.0 .and. dotl(2).gt.zero)  .and.
     *       (dot(5).lt.0 .and. dotl(5).gt.zero)  .and.
     *       (dot(6).gt.0 .and. dotl(6).gt.zero)  )then
         id = 6

 
c.....find the 8 corners, id's 19-26
 
c     25   top    back  right
c     19   bottom front left
      elseif(  (dotl(1) .le. zero).and.
     *         (dotl(3) .le. zero).and.(dotl(6) .le. zero) .and.
     *         (abs((abs(dot(2)) - abs(dot(4))) ) .le. zero) .and.
     *         (abs((abs(dot(4)) - abs(dot(5))) ) .le. zero)  ) then 
         if(dot(2) .gt. 0)id=25
         if(dot(2) .lt. 0)id=19

c     21   bottom back  right
c     23   top    front left
      elseif( (dotl(1).le. zero).and.
     *        (dotl(4).le. zero).and.(dotl(5).le. zero).and.
     *        (abs((abs(dot(2)) - abs(dot(3))) ) .le. zero)  .and.
     *        (abs((abs(dot(3)) - abs(dot(6))) ) .le. zero)  ) then
          if(dot(2) .gt. 0)id=21
          if(dot(2) .lt. 0)id=23

c     24   top    front right
c     22   bottom back  left
      elseif( (dotl(2).le. zero).and.
     *        (dotl(4).le. zero).and.(dotl(6).le. zero).and.
     *        (abs((abs(dot(1)) - abs(dot(3))) ) .le. zero)  .and.
     *        (abs((abs(dot(3)) - abs(dot(5))) ) .le. zero)  ) then
           if(dot(1) .gt. 0)id=24
           if(dot(1) .lt. 0)id=22

c     20   bottom front right
c     26   top    back  left
      elseif( (dotl(2).le. zero).and.
     *        (dotl(3).le. zero).and.(dotl(5).le. zero).and.
     *        (abs((abs(dot(1)) - abs(dot(4))) ) .le. zero)  .and.
     *        (abs((abs(dot(4)) - abs(dot(6))) ) .le. zero)  ) then
          if(dot(1) .gt. 0)id=20
          if(dot(1) .lt. 0)id=26
 
c.....find the 12 edges between side facing edge and corners, id's 7-18
 
c     7   bottom front
c     18   top back 
      elseif((dotl(3) .le. zero).and.
     *              (abs((abs(dot(2)) - abs(dot(5)))) .le. zero) .and.
     *              (abs((abs(dot(1)) - abs(dot(6)))) .le. zero) ) then
         if(dot(4) .lt. 0)id=7
         if(dot(4) .gt. 0)id=18

c     8   bottom left
c     17   top right
      elseif((dotl(6) .le. zero).and.
     *              (abs((abs(dot(1)) - abs(dot(3)))) .le. zero) .and.
     *              (abs((abs(dot(2)) - abs(dot(4)))) .le. zero) ) then
         if(dot(5) .lt. 0)id=8
         if(dot(5) .gt. 0)id=17

c     9   front  left
c     13   right back
      elseif((dotl(1) .le. zero).and.
     *              (abs((abs(dot(3)) - abs(dot(6)))) .le. zero) .and.
     *              (abs((abs(dot(5)) - abs(dot(4)))) .le. zero) ) then
         if(dot(2) .lt. 0)id=9
         if(dot(2) .gt. 0)id=13

c     10   bottom right
c     16   top left 
      elseif((dotl(5) .le. zero) .and.
     *              (abs((abs(dot(1)) - abs(dot(4)))) .le. zero) .and.
     *              (abs((abs(dot(3)) - abs(dot(2)))) .le. zero)) then
         if(dot(6) .lt. 0)id=10
         if(dot(6) .gt. 0)id=16

c     14   back left
c     11   front right
      elseif((dotl(2) .le. zero).and.
     *              (abs((abs(dot(3)) - abs(dot(5)))) .le. zero) .and.
     *              (abs((abs(dot(4)) - abs(dot(6)))) .le. zero)) then
         if(dot(1) .lt. 0)id=14
         if(dot(1) .gt. 0)id=11

c     12   bottom back
c     15   top front 
      elseif((dotl(4) .le. zero).and.
     *              (abs((abs(dot(1)) - abs(dot(5)))) .le. zero) .and.
     *              (abs((abs(dot(2)) - abs(dot(6)))) .le. zero) ) then
         if(dot(3) .lt. 0)id=12
         if(dot(3) .gt. 0)id=15

      endif
 
 
      if(id .eq. 0)then
c       shouldn't reach this point
        ierror = -1
        id = -1
      endif

c     if (id.eq.1) print*,'  1  bottom'
c     if (id.eq.2) print*,'  2  top'
c     if (id.eq.3) print*,'  3  front'
c     if (id.eq.4) print*,'  4   right'
c     if (id.eq.5) print*,'  5   back'
c     if (id.eq.6) print*,'  6   left'
c     if (id.eq.7) print*,'  7   bottom front'
c     if (id.eq.8) print*,'  8   bottom left'
c     if (id.eq.9) print*,'  9   front  left'
c     if (id.eq.10) print*,' 10   bottom right'
c     if (id.eq.11) print*,' 11   front right'
c     if (id.eq.12) print*,' 12   bottom back'
c     if (id.eq.13) print*,' 13   right back'
c     if (id.eq.14) print*,' 14   left  back'
c     if (id.eq.15) print*,' 15   top front'
c     if (id.eq.16) print*,' 16   top left'
c     if (id.eq.17) print*,' 17   top right '
c     if (id.eq.18) print*,' 18   top back '
c     if (id.eq.19) print*,' 19   bottom front left'
c     if (id.eq.20) print*,' 20   bottom front right'
c     if (id.eq.21) print*,' 21   bottom back  right'
c     if (id.eq.22) print*,' 22   bottom back  left'
c     if (id.eq.23) print*,' 23   top    front left'
c     if (id.eq.24) print*,' 24   top    front right'
c     if (id.eq.25) print*,' 25   top    back  right'
c     if (id.eq.26) print*,' 26   top    back  left'
c     if (id.eq.-1) print*,' -1   unknown'
c     print*,'-------------------------------------------------'

      return
      end
