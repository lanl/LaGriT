*dk volume_element
      subroutine volume_element(ielmtyp,
     *                          xicvol,yicvol,zicvol,
     *                          volelm)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF AN ELEMENT
C         BY BREAKING IT INTO TRIANGLES AND TETS IF NOT
C         A TRIANGLE OR TET, USING THE MIDPOINT.
C
C         Compare volume_element_voronoi and volume_element_alt_lg below
C
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        ielmtyp                     - THE ELEMENT TYPE.
C        (xicvol(),yicvol(),zicvol() - THE COORDINATES OF THE TET.
C
C     OUTPUT ARGUMENTS -
C
C        volelm - THE VOLUME OF THE ELEMENT.
C
C     CHANGE HISTORY -
C
C        $Log: volume_element.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Tue Oct 19 13:20:20 1999   jtg
CPVCS    alternate way of calculating volume using "minimum area
CPVCS    point" (instead of midpoint) for quad faces or elements added.
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 17:05:34 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   Mon Nov 11 20:56:18 1996   het
CPVCS    
CPVCS       Rev 1.1   Thu Oct 10 08:43:28 1996   het
CPVCS    Calculate Voronoi volumes.
CPVCS    
CPVCS       Rev 1.0   Mon Jul 29 15:42:46 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      integer ielmtyp
      real*8 xicvol(1000000), yicvol(1000000), zicvol(1000000)
      real*8 volelm
C
C$$  integer ierwrt
C
C#######################################################################
C
C
      if(ielmtyp.eq.ifelmpnt) then
         volelm=0.0
      elseif(ielmtyp.eq.ifelmlin) then
         volelm=sqrt((xicvol(2)-xicvol(1))**2+
     *               (yicvol(2)-yicvol(1))**2+
     *               (zicvol(2)-zicvol(1))**2)
      elseif(ielmtyp.eq.ifelmtri) then
         call volume_tri(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmqud) then
         call volume_qud(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmtet) then
         call volume_tet(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmpyr) then
         call volume_hex(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmpri) then
         call volume_hex(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmhex) then
         call volume_hex(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   xicvol(7),yicvol(7),zicvol(7),
     *                   xicvol(8),yicvol(8),zicvol(8),
     *                   volelm)
      else
         volelm=0.0
         write(logmess,9010) ielmtyp
 9010    format('Invalid element volume type: ',i10)
      endif
C
      return
      end
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
c ======================================================================
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
*dk volume_element_alt_lg
      subroutine volume_element_alt_lg(ielmtyp,
     *                          xicvol,yicvol,zicvol,
     *                          volelm)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF AN ELEMENT
C         BY BREAKING IT INTO TRIANGLES AND TETS IF NOT
C         A TRIANGLE OR TET, USING THE POINT WHICH
C         MINIMIZES THE SUM OF THE AREAS FOR QUDS (AND QUD FACES).
C
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        ielmtyp                     - THE ELEMENT TYPE.
C        (xicvol(),yicvol(),zicvol() - THE COORDINATES OF THE TET.
C
C     OUTPUT ARGUMENTS -
C
C        volelm - THE VOLUME OF THE ELEMENT.
C
C     CHANGE HISTORY -
C
C        $Log
C
C#######################################################################
C
C
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicvol(1000000), yicvol(1000000), zicvol(1000000)
C
C$$   integer ierwrt
C
C#######################################################################
C
C
      if(ielmtyp.eq.ifelmpnt) then
         volelm=0.0
      elseif(ielmtyp.eq.ifelmlin) then
         volelm=sqrt((xicvol(2)-xicvol(1))**2+
     *               (yicvol(2)-yicvol(1))**2+
     *               (zicvol(2)-zicvol(1))**2)
      elseif(ielmtyp.eq.ifelmtri) then
         call volume_tri(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmqud) then
         call volume_qud_alt_lg(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   volelm,x1,y1,z1)
      elseif(ielmtyp.eq.ifelmtet) then
         call volume_tet(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmpyr) then
         ! break into 4 tets using area-minimizing point of 4-sided face
         call volume_qud_alt_lg(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   vvv,x1,y1,z1)
         call volume_tet(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   x1,y1,z1,
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   volelm)
         call volume_tet(xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   x1,y1,z1,
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   x1,y1,z1,
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(1),yicvol(1),zicvol(1),
     *                   x1,y1,z1,
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   vvv)
         volelm=volelm+vvv
      elseif(ielmtyp.eq.ifelmpri) then
         ! find the area-minimizing points of 4-sided faces
         ! face 1254 midpoint
         call volume_qud_alt_lg(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   vvv,x1,y1,z1)
         ! face 2365 midpoint
         call volume_qud_alt_lg(xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   vvv,x2,y2,z2)
         ! face 3146 midpoint
         call volume_qud_alt_lg(xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   vvv,x3,y3,z3)
         ! break into 11 tets using area-minimizing points of 4-sided faces
         ! 3 tets using edges which connect top tri to bottom tri
         ! and face midpoints of the adjoining quad faces
         call volume_tet(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   x1,y1,z1, x3,y3,z3, volelm)
         call volume_tet(xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   x2,y2,z2, x1,y1,z1, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   x3,y3,z3, x2,y2,z2, vvv)
         volelm=volelm+vvv
         ! 4 tets resulting from connection of face-midpoint tri to top tri
         call volume_tet(xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   x1,y1,z1, x3,y3,z3, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(5),yicvol(5),zicvol(5),
     *                   x2,y2,z2, x1,y1,z1, x3,y3,z3, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   x2,y2,z2, x3,y3,z3, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   x3,y3,z3, vvv)
         volelm=volelm+vvv
         ! 4 tets resulting from connection of face-midpoint tri to bottom tri
         call volume_tet(xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(1),yicvol(1),zicvol(1),
     *                   x1,y1,z1, x3,y3,z3, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(2),yicvol(2),zicvol(2),
     *                   x1,y1,z1, x2,y2,z2, x3,y3,z3, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   x2,y2,z2, x3,y3,z3, vvv)
         volelm=volelm+vvv
         call volume_tet(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   x3,y3,z3, vvv)
         volelm=volelm+vvv
      elseif(ielmtyp.eq.ifelmhex) then
         call volume_hex_alt_lg(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   xicvol(7),yicvol(7),zicvol(7),
     *                   xicvol(8),yicvol(8),zicvol(8),
     *                   volelm)
      else
         volelm=0.0
         write(logmess,9010) ielmtyp
 9010    format('Invalid element volume type: ',i10)
      endif
C
      return
      end
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C=======================================================================
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
*dk volume_element_voronoi
      subroutine volume_element_voronoi(ielmtyp,
     *                                  xicvol,yicvol,zicvol,
     *                                  volelm,voledge)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF AN ELEMENT.
C         BY BREAKING IT INTO TRIANGLES AND TETS IF NOT
C         A TRIANGLE OR TET, USING THE MIDPOINT.
C         for a tet, volelm->volelm(4), voledge->voledge(6)
C         and the appropriate voronoi info is calculated
C
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        ielmtyp                     - THE ELEMENT TYPE.
C        (xicvol(),yicvol(),zicvol() - THE COORDINATES OF THE ELEMENT.
C
C     OUTPUT ARGUMENTS -
C
C        volelm - THE VOLUME OF THE ELEMENT.
C        voledge - THE EDGE VOLUMES, returned only for tets
C
C     CHANGE HISTORY -
C
C        $Log: volume_element.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
C
C#######################################################################
C
C
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicvol(1000000), yicvol(1000000), zicvol(1000000)
C
C#######################################################################
C
C
      if(ielmtyp.eq.ifelmpnt) then
         volelm=0.0
      elseif(ielmtyp.eq.ifelmlin) then
         volelm=sqrt((xicvol(2)-xicvol(1))**2+
     *               (yicvol(2)-yicvol(1))**2+
     *               (zicvol(2)-zicvol(1))**2)
      elseif(ielmtyp.eq.ifelmtri) then
         call volume_tri(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmqud) then
         call volume_qud(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   volelm)
      elseif(ielmtyp.eq.ifelmtet) then
         call volume_tet_voronoi(xicvol(1),yicvol(1),zicvol(1),
     *                           xicvol(2),yicvol(2),zicvol(2),
     *                           xicvol(3),yicvol(3),zicvol(3),
     *                           xicvol(4),yicvol(4),zicvol(4),
     *                           volelm,voledge)
      elseif(ielmtyp.eq.ifelmpyr) then
         write(logmess,9000) ielmtyp,celmnames(ielmtyp)
 9000    format('Element volume not supported: ',i10,a32)
      elseif(ielmtyp.eq.ifelmpri) then
         write(logmess,9000) ielmtyp,celmnames(ielmtyp)
      elseif(ielmtyp.eq.ifelmhex) then
         call volume_hex(xicvol(1),yicvol(1),zicvol(1),
     *                   xicvol(2),yicvol(2),zicvol(2),
     *                   xicvol(3),yicvol(3),zicvol(3),
     *                   xicvol(4),yicvol(4),zicvol(4),
     *                   xicvol(5),yicvol(5),zicvol(5),
     *                   xicvol(6),yicvol(6),zicvol(6),
     *                   xicvol(7),yicvol(7),zicvol(7),
     *                   xicvol(8),yicvol(8),zicvol(8),
     *                   volelm)
      else
         volelm=0.0
         write(logmess,9010) ielmtyp
 9010    format('Invalid element volume type: ',i10)
      endif
C
      return
      end
C#######################################################################
