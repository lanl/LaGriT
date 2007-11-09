*dk volume_tri
      subroutine volume_tri(x1,y1,z1,
     *                      x2,y2,z2,
     *                      x3,y3,z3,
     *                      voltri)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A TRI-ELEMENT DEFINDED BY
C            3 COORDINATE NODES. THE VOLUME IS FOUND BY TAKING THE
C            CROSS PRODUCT OF TWO VECTORS.
C
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC TRIANGLE.
C
C            i3  *- - - - - -*  i2
C                 \         /
C                  \       /
C                   \     /
C                    \   /
C                     \ /
C                      *
C                     i1
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x3,y3,z3) - THE COORDINATES OF THE TRI.
C
C     OUTPUT ARGUMENTS -
C
C        voltri - THE AREA OF THE TRI.
C
C     CHANGE HISTORY -
C
C        $Log: volume_tri.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Tue Oct 19 13:16:10 1999   jtg
CPVCS    changed comment lines to actually correspond to a tri
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 17:05:44 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Mon Jul 29 15:42:52 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
C
      implicit real*8 (a-h,o-z)
C
C#######################################################################
C
C
C     ..................................................................
C     TAKE THE CROSS PRODUCT OF THE 21 AND 31 VECTORS TO THE AN AREA
C        VECTOR.
C
      dx= (y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
      dy= (z2-z1)*(x3-x1) - (z3-z1)*(x2-x1)
      dz= (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
C
C     ..................................................................
C     THEN TAKE 1/2 THE MAGNITUDE OF THIS AREA VECTOR TO GET THE TRI VOLUME
C
      voltri=0.5d+00*sqrt(dx**2+dy**2+dz**2)
C
      return
      end
