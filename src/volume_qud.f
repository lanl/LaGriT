*dk volume_qud
      subroutine volume_qud(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      volqud)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A QUD-ELEMENT DEFINDED BY
C            4 COORDINATE NODES. THE VOLUME OF THE QUD IS DETERMINED
C            BY SUBDIVIDING IT INTO 4 TRIANGULAR ELEMENTS
C            USING THE MIDPOINT
C            AND THEN SUMMING THE VOLUME OF THE TRIANGLES.
C
C            compare volume_qud_alt_lg below
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC QUD.
C
C
C          4- - - -3    Edge 1: 1 2
C          |       |         2: 2 3
C          |       |         3: 3 4
C          1- - - -2         4: 4 1
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x4,y4,z4) - THE COORDINATES OF THE QUD.
C
C     OUTPUT ARGUMENTS -
C
C        volqud - THE VOLUME OF THE QUD.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/volume_qud.f_a  $
CPVCS    
CPVCS       Rev 1.4   Tue Oct 19 15:13:20 1999   jtg
CPVCS    fixed form of inv3x3 call
CPVCS    
CPVCS       Rev 1.3   Tue Oct 19 13:16:54 1999   jtg
CPVCS    alternate way of caluculating volume using "minimum area
CPVCS    point" (instead of midpoint) added.
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:05:40 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Mon Nov 11 20:55:52 1996   het
CPVCS    Remove the absolute value for volume.
CPVCS    
CPVCS       Rev 1.0   Mon Jul 29 15:42:48 1996   dcg
CPVCS    Initial revision.
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
      x1234=(x1+x2+x3+x4)/4.0d+00
      y1234=(y1+y2+y3+y4)/4.0d+00
      z1234=(z1+z2+z3+z4)/4.0d+00

      !local_debug=0
      !if (local_debug.gt.0) then
      !  write(*,'(a,3f12.8)') 'using mid ',x1234,y1234,z1234
      !endif
C
C
C     ..................................................................
C     FIND THE VOLUME OF THE 4-TRI QUD.
C
      call volume_tri(x1234,y1234,z1234,
     *                x1,y1,z1,
     *                x2,y2,z2,
     *                voltri1)
      call volume_tri(x1234,y1234,z1234,
     *                x2,y2,z2,
     *                x3,y3,z3,
     *                voltri2)
      call volume_tri(x1234,y1234,z1234,
     *                x3,y3,z3,
     *                x4,y4,z4,
     *                voltri3)
      call volume_tri(x1234,y1234,z1234,
     *                x4,y4,z4,
     *                x1,y1,z1,
     *                voltri4)
C
C
C     ..................................................................
C     SUM THE 4 TRI VOLUMES TO THE VOLUME OF THE QUD.
C
      volqud=voltri1+voltri2+voltri3+voltri4
C
      return
      end
C#######################################################################
C=======================================================================
C#######################################################################
*dk volume_qud_alt
      subroutine volume_qud_alt_lg(x1,y1,z1,x2,y2,z2
     *                      ,x3,y3,z3,x4,y4,z4,volqud
     *                      ,x1234,y1234,z1234)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A QUD-ELEMENT DEFINDED BY
C            4 COORDINATE NODES. THE VOLUME OF THE QUD IS DETERMINED
C            BY SUBDIVIDING IT INTO 4 TRIANGULAR ELEMENTS
C            USING THE POINT WHICH MINIMIZES THE SUM OF THE AREAS
C            AND THEN SUMMING THE VOLUME OF THE TRIANGLES.
C
C            structure and arguments same as above
C            except call also returns "min-area-point" (x1234,y1234,z1234)
C
C#######################################################################
C
      implicit none
      real*8 x1,y1,z1,x2,y2,z2 ,x3,y3,z3,x4,y4,z4,volqud
     *      ,x1234,y1234,z1234

      real*8 mat(3,3),vec(3),d(3,4),s(3,4),dd(4),ds(4),ddd
     *      ,voltri
      integer i,j,local_debug,iflag
C
C#######################################################################
C
C     ..................................................................
C     FIND THE POINT WHICH MINIMIZES THE SUM OF THE AREAS
C

      d(1,1)=x2-x1
      d(2,1)=y2-y1
      d(3,1)=z2-z1
      d(1,2)=x3-x2
      d(2,2)=y3-y2
      d(3,2)=z3-z2
      d(1,3)=x4-x3
      d(2,3)=y4-y3
      d(3,3)=z4-z3
      d(1,4)=x1-x4
      d(2,4)=y1-y4
      d(3,4)=z1-z4

      s(1,1)=x2+x1
      s(2,1)=y2+y1
      s(3,1)=z2+z1
      s(1,2)=x3+x2
      s(2,2)=y3+y2
      s(3,2)=z3+z2
      s(1,3)=x4+x3
      s(2,3)=y4+y3
      s(3,3)=z4+z3
      s(1,4)=x1+x4
      s(2,4)=y1+y4
      s(3,4)=z1+z4

      ddd=0.0d+0
      do j=1,4
         dd(j)=0.0d+0
         ds(j)=0.0d+0
         do i=1,3
            dd(j)=dd(j)+d(i,j)*d(i,j)
            ds(j)=ds(j)+d(i,j)*s(i,j)
         enddo
         ddd=ddd+dd(j)
      enddo

      do i=1,3
         vec(i)=d(i,1)*ds(1)+d(i,2)*ds(2)+d(i,3)*ds(3)+d(i,4)*ds(4)
     &         -s(i,1)*dd(1)-s(i,2)*dd(2)-s(i,3)*dd(3)-s(i,4)*dd(4)
         vec(i)=vec(i)*0.5d+00
         do j=1,3
            mat(i,j)=d(i,1)*d(j,1)+d(i,2)*d(j,2)
     &              +d(i,3)*d(j,3)+d(i,4)*d(j,4)
         enddo
         mat(i,i)=mat(i,i)-ddd
      enddo

      call inv3x3(mat(1,1),mat(1,2),mat(1,3)
     &           ,mat(2,1),mat(2,2),mat(2,3)
     &           ,mat(3,1),mat(3,2),mat(3,3)
     &           ,vec(1),vec(2),vec(3)
     &           ,x1234,y1234,z1234 ,iflag)

      !local_debug=0
      !if (local_debug.gt.0) then
      !  write(*,'(a,3f12.8)') 'using alt ',x1234,y1234,z1234
      !endif

C     ..................................................................
C     NOW FIND THE VOLUME OF THE 4-TRI QUD.
C
      call volume_tri(x1234,y1234,z1234,
     &                x1,y1,z1,
     &                x2,y2,z2,
     &                volqud)
      call volume_tri(x1234,y1234,z1234,
     &                x2,y2,z2,
     &                x3,y3,z3,
     &                voltri)
      volqud=volqud+voltri
      call volume_tri(x1234,y1234,z1234,
     &                x3,y3,z3,
     &                x4,y4,z4,
     &                voltri)
      volqud=volqud+voltri
      call volume_tri(x1234,y1234,z1234,
     &                x4,y4,z4,
     &                x1,y1,z1,
     &                voltri)
      volqud=volqud+voltri
C
      return
      end
C#######################################################################
