      subroutine voronoi_center (
     1      pts11,pts12,pts13,
     2      pts21,pts22,pts23,
     3      pts31,pts32,pts33,
     4      pts41,pts42,pts43,
     5      center1,  center2,  center3, radius  )
c
c     This code takes pts as input and outputs the
c     Voronoi point of the tetrahedra formed by the 4 pts.
c     This is the translation to Fortran of the C code of 
c     Mike Murphy called compute_center.
C
C      CHANGE HISTORY -
C
C      $Log:   /pvcs.config/t3d/src/voronoi_center.f_a  $
CPVCS    
CPVCS       Rev 1.1   08 Feb 2006 14:38:08   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.0   Thu Jun 19 17:33:08 1997   gable
CPVCS    Initial revision.
c
      implicit none
      
      real*8 A(3,3)
      real*8 B(3)
      real*8 f11,f12,f13,f21,f22,f23,f31,f32,f33
      real*8 pts11,pts12,pts13,pts21,pts22,pts23,
     x pts31,pts32,pts33,pts41,pts42,pts43,
     x center1,  center2,  center3, radius
      real*8 deta,detai
      real*8 det3
      det3(f11,f12,f13,f21,f22,f23,f31,f32,f33) =
     1    ((f31*((f12*f23)-(f22*f13)))-
     2     (f32*((f11*f23)-(f13*f21)))+
     3     (f33*((f11*f22)-(f21*f12))))
      
      A(1,1) = pts41 - pts11
      A(1,2) = pts42 - pts12
      A(1,3) = pts43 - pts13

      A(2,1) = pts41 - pts21
      A(2,2) = pts42 - pts22
      A(2,3) = pts43 - pts23

      A(3,1) = pts41 - pts31
      A(3,2) = pts42 - pts32
      A(3,3) = pts43 - pts33
      
      B(1) = 0.5d0*(A(1,1)*(pts11+pts41)+
     1              A(1,2)*(pts12+pts42)+
     2              A(1,3)*(pts13+pts43))


      B(2) = 0.5d0*(A(2,1)*(pts21+pts41)+
     1              A(2,2)*(pts22+pts42)+
     2              A(2,3)*(pts23+pts43))


      B(3) = 0.5d0*(A(3,1)*(pts31+pts41)+
     1              A(3,2)*(pts32+pts42)+
     2              A(3,3)*(pts33+pts43))
            
      deta = det3(A(1,1),A(1,2),A(1,3),
     1       A(2,1),A(2,2),A(2,3),           
     2       A(3,1),A(3,2),A(3,3))
     
      detai = 1.0d0/(deta + 1.e-30)
            
      center1 = det3(B(1),A(1,2),A(1,3),
     1            B(2),A(2,2),A(2,3),           
     2            B(3),A(3,2),A(3,3))*detai

      center2 = det3(A(1,1),B(1),A(1,3),
     1            A(2,1),B(2),A(2,3),           
     2            A(3,1),B(3),A(3,3))*detai

      center3 = det3(A(1,1),A(1,2),B(1),
     1            A(2,1),A(2,2),B(2),
     2            A(3,1),A(3,2),B(3))*detai


      radius = sqrt((center1-pts11)**2 +
     1              (center2-pts12)**2 +
     2              (center3-pts13)**2 )
      return
      end
