*dk volume_hex
      subroutine volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                      volhex)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A HEX-ELEMENT DEFINDED BY
C            8 COORDINATE NODES. THE VOLUME OF THE HEX IS DETERMINED
C            BY SUBDIVIDING IT INTO 24 TETRAHEDRAL ELEMENTS AND THEN
C            SUMMING THE VOLUME OF THE TETRAHEDRA.
C
C            THIS VERSION USES THE MIDPOINTS
C            see volume_hex_alt_lg below for version using "min-area-points"
C
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC HEXRAHEDRON.
C
C 
C          8$ * * * * * * $7             Face 1: 1 2 3 4  (bottom)
C           * *           - *                 2: 5 8 7 6  (top)
C           *   *         -   *               3: 1 5 6 2  (front)
C           *     *       -     *             4: 2 6 7 3  (back)
C           *       *     -       *           5: 3 7 8 4  (right)
C           *        5$ * * * * * * $6        6: 1 4 8 5  (left)
C           *         *   -         *    
C          4$ - - - - - - $3        *    Edge 1: 1 2
C             *       *     -       *            2 3 
C               *     *       -     *            3 4
C                 *   *         -   *            4 1
C                   * *           - *            5 6
C                    1$ * * * * * * $2           6 7 
C                                                7 8 
C                                                8 5
C                                                1 5
C                                                2 6
C                                                3 7 
C                                                4 8
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x8,y8,z8) - THE COORDINATES OF THE HEX WHERE
C                                    1,2,3,4 FORM THE BOTTOM FACE AND 
C                                    5,6,7,8 FORM THE TOP FACE.
C
C     OUTPUT ARGUMENTS -
C
C        volhex - THE VOLUME OF THE HEX.
C
C     CHANGE HISTORY -
C
C        $Log: volume_hex.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   Tue Oct 19 13:18:02 1999   jtg
CPVCS    alternate way of calculating volume using "minimum area
CPVCS    point" (instead of midpoint) for each faces added.
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:05:36 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Thu Nov 21 19:07:46 1996   het
CPVCS    Correct an error in the calculation of the first tet of the hex.
CPVCS    
CPVCS       Rev 1.2   Mon Nov 11 20:56:14 1996   het
CPVCS    Remove the absolute value for volume.
CPVCS    
CPVCS       Rev 1.1   08/15/95 18:29:36   het
CPVCS    Modify some comment lines
C
C
C#######################################################################
C
      implicit real*8 (a-h,o-z)
C
C#######################################################################
C
C
C     ..................................................................
C     FIND THE MEDIAN POINT FOR EACH OF THE SIX FACES AND THE HEX.
C
      x1432=0.25d+00*(x1+x4+x3+x2)
      y1432=0.25d+00*(y1+y4+y3+y2)
      z1432=0.25d+00*(z1+z4+z3+z2)
      x5678=0.25d+00*(x5+x6+x7+x8)
      y5678=0.25d+00*(y5+y6+y7+y8)
      z5678=0.25d+00*(z5+z6+z7+z8)
      x2376=0.25d+00*(x2+x3+x7+x6)
      y2376=0.25d+00*(y2+y3+y7+y6)
      z2376=0.25d+00*(z2+z3+z7+z6)
      x1584=0.25d+00*(x1+x5+x8+x4)
      y1584=0.25d+00*(y1+y5+y8+y4)
      z1584=0.25d+00*(z1+z5+z8+z4)
      x1265=0.25d+00*(x1+x2+x6+x5)
      y1265=0.25d+00*(y1+y2+y6+y5)
      z1265=0.25d+00*(z1+z2+z6+z5)
      x4873=0.25d+00*(x4+x8+x7+x3)
      y4873=0.25d+00*(y4+y8+y7+y3)
      z4873=0.25d+00*(z4+z8+z7+z3)
      x12345678=(x1+x2+x3+x4+x5+x6+x7+x8)/8.0d+00
      y12345678=(y1+y2+y3+y4+y5+y6+y7+y8)/8.0d+00
      z12345678=(z1+z2+z3+z4+z5+z6+z7+z8)/8.0d+00
C
C
C     ..................................................................
C     FIND THE VOLUME OF THE 24-TET HEX.
C
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x4,y4,z4,
     *            voltet11)
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x3,y3,z3,
     *            voltet12)
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x2,y2,z2,
     *            voltet13)
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x1,y1,z1,
     *            voltet14)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x6,y6,z6,
     *            voltet21)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x7,y7,z7,
     *            voltet22)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x8,y8,z8,
     *            voltet23)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x5,y5,z5,
     *            voltet24)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x3,y3,z3,
     *            voltet31)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x7,y7,z7,
     *            voltet32)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x6,y6,z6,
     *            voltet33)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x2,y2,z2,
     *            voltet34)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x5,y5,z5,
     *            voltet41)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x8,y8,z8,
     *            voltet42)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x4,y4,z4,
     *            voltet43)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x1,y1,z1,
     *            voltet44)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x2,y2,z2,
     *            voltet51)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x6,y6,z6,
     *            voltet52)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x5,y5,z5,
     *            voltet53)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x1,y1,z1,
     *            voltet54)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x8,y8,z8,
     *            voltet61)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x7,y7,z7,
     *            voltet62)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x3,y3,z3,
     *            voltet63)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x4,y4,z4,
     *            voltet64)
C
C
C     ..................................................................
C     SUM THE 24 TET VOLUMES TO THE VOLUME OF THE HEX.
C
      volhex=-voltet11-voltet12-voltet13-voltet14-
     *        voltet21-voltet22-voltet23-voltet24-
     *        voltet31-voltet32-voltet33-voltet34-
     *        voltet41-voltet42-voltet43-voltet44-
     *        voltet51-voltet52-voltet53-voltet54-
     *        voltet61-voltet62-voltet63-voltet64
C
      return
      end
C#######################################################################
C=======================================================================
C#######################################################################
*dk volume_hex
      subroutine volume_hex_alt_lg(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                      volhex)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A HEX-ELEMENT DEFINDED BY
C            8 COORDINATE NODES. THE VOLUME OF THE HEX IS DETERMINED
C            BY SUBDIVIDING IT INTO 24 TETRAHEDRAL ELEMENTS AND THEN
C            SUMMING THE VOLUME OF THE TETRAHEDRA.
C
C            THIS VERSION USES THE AREA-MINMIZING POINTS OF THE QUD FACES
C            see volume_hex above for version using midpoints, and conventions
C
C
C#######################################################################
C
      implicit real*8 (a-h,o-z)
C
C#######################################################################
C
C
C     ..................................................................
C     FIND THE AREA_MINIMIZING POINT FOR EACH OF THE SIX FACES AND THE HEX.
C
      call volume_qud_alt_lg(x1,y1,z1, x4,y4,z4, x3,y3,z3, x2,y2,z2,
     *                       vvv, x1432,y1432,z1432 )
      call volume_qud_alt_lg(x5,y5,z5, x6,y6,z6, x7,y7,z7, x8,y8,z8,
     *                       vvv, x5678,y5678,z5678 )
      call volume_qud_alt_lg(x2,y2,z2, x3,y3,z3, x7,y7,z7, x6,y6,z6,
     *                       vvv, x2376,y2376,z2376 )
      call volume_qud_alt_lg(x1,y1,z1, x5,y5,z5, x8,y8,z8, x4,y4,z4,
     *                       vvv, x1584,y1584,z1584 )
      call volume_qud_alt_lg(x1,y1,z1, x2,y2,z2, x6,y6,z6, x5,y5,z5,
     *                       vvv, x1265,y1265,z1265 )
      call volume_qud_alt_lg(x4,y4,z4, x8,y8,z8, x7,y7,z7, x3,y3,z3,
     *                       vvv, x4873,y4873,z4873 )

      ! OK to use midpoint for center, as +/- contributions should cancel...
      x12345678=(x1432+x5678+x2376+x1584+x1265+x4873)/6.0d+00
      y12345678=(y1432+y5678+y2376+y1584+y1265+y4873)/6.0d+00
      z12345678=(z1432+z5678+z2376+z1584+z1265+z4873)/6.0d+00

      ! if actually need volume-minimizing-point for breaking into tets: 
      ! calculate as point which minimizes [ sum_{1=1,24} V(i)^2 ]
      ! where V(i) is the volume of the 24 sub-tets
      ! (similar to min-area-point calculation in volume_qud_alt_lg)
C
C     ..................................................................
C     FIND THE VOLUME OF THE 24-TET HEX (same as above).
C
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x4,y4,z4,
     *            voltet11)
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x3,y3,z3,
     *            voltet12)
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x2,y2,z2,
     *            voltet13)
      call volume_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x1,y1,z1,
     *            voltet14)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x6,y6,z6,
     *            voltet21)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x7,y7,z7,
     *            voltet22)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x8,y8,z8,
     *            voltet23)
      call volume_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x5,y5,z5,
     *            voltet24)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x3,y3,z3,
     *            voltet31)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x7,y7,z7,
     *            voltet32)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x6,y6,z6,
     *            voltet33)
      call volume_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x2,y2,z2,
     *            voltet34)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x5,y5,z5,
     *            voltet41)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x8,y8,z8,
     *            voltet42)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x4,y4,z4,
     *            voltet43)
      call volume_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x1,y1,z1,
     *            voltet44)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x2,y2,z2,
     *            voltet51)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x6,y6,z6,
     *            voltet52)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x5,y5,z5,
     *            voltet53)
      call volume_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x1,y1,z1,
     *            voltet54)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x8,y8,z8,
     *            voltet61)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x7,y7,z7,
     *            voltet62)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x3,y3,z3,
     *            voltet63)
      call volume_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x4,y4,z4,
     *            voltet64)
C
C
C     ..................................................................
C     SUM THE 24 TET VOLUMES TO THE VOLUME OF THE HEX.
C
      volhex=-voltet11-voltet12-voltet13-voltet14-
     *        voltet21-voltet22-voltet23-voltet24-
     *        voltet31-voltet32-voltet33-voltet34-
     *        voltet41-voltet42-voltet43-voltet44-
     *        voltet51-voltet52-voltet53-voltet54-
     *        voltet61-voltet62-voltet63-voltet64
C
      return
      end
