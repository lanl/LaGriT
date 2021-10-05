      subroutine aratio_element_edge(ielmtyp,
     *                               x,y,z,
     *                               id_edge_min, id_edge_max,
     *                               edge_min, edge_max,
     *                               edge_min_edge_max_ratio)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        For a single element finds the ratio:
C                (shortest element edge/longest element edge)
C
C     INPUT ARGUMENTS -
C
C        ielmtyp            : The element type.
C        (x(),y(),z()       : The coordinates of the element.
C
C     OUTPUT ARGUMENTS -
C
C        id_edge_min             : The local edge number of the shortest edge
C        id_edge_max             : The local edge number of the longest edge
C        edge_min                : The length of the longest edge
C        edge_max                : The length of the shortest edge
C        edge_min_edge_max_ratio : The computed edge min to edge max ratio.
C
C     CHANGE HISTORY -
C
C $Log: aratio_element_edge.f,v $
C Revision 2.00  2007/11/05 19:45:46  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   20 Jun 2006 18:25:14   gable
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "local_element.h"
C
      real*8 edge_min_edge_max_ratio, dist, edge_min, edge_max
      real*8 x(1000000), y(1000000), z(1000000)
      integer i, i1, i2, ielmtyp
      integer id_edge_min, id_edge_max
C
C ######################################################################
C
C
C   Find min and max edge length. Save the square root calculation
C   until the min/max have been found.
C
      edge_max = -1.e20
      edge_min =  1.e20
      do i=1,nelmnee(ielmtyp)
         i1 = ielmedge1(1,i,ielmtyp)
         i2 = ielmedge1(2,i,ielmtyp)
         dist = (x(i2)-x(i1))**2 + 
     1          (y(i2)-y(i1))**2 +
     2          (z(i2)-z(i1))**2

         if(dist .gt. edge_max)then
            edge_max = dist
            id_edge_max = i
         endif
         if(dist .lt. edge_min)then
            edge_min = dist
            id_edge_min = i
         endif
      enddo

      edge_max = sqrt(edge_max)
      edge_min = sqrt(edge_min)
      edge_min_edge_max_ratio = edge_min/edge_max

      return
      end
