      subroutine assign_quadrant_id(id,vector,epsilon,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C          Determing the which of 6 quadrents VECTOR points into
C
C     INPUT ARGUMENTS -
C
C        vector - 3 component vector
C
C     OUTPUT ARGUMENTS -
C
C        id       - integer flag indicating which direction vector points
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
C        $Log: assign_quadrant_id.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Mar 02 08:27:24 1998   tam
CPVCS    added epsilon to call identify_dot()
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:39:02 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Wed May 08 12:35:26 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
      real*8 dot(6)
      real*8 epsilon 
      real*8 vector(3)
      integer id
      integer ierror
c
      call dot_product_6_planes(vector,dot)
      call identify_dot(dot,epsilon,id,ierror)
c
      return
      end
