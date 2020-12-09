C     voronoi_center and routines using voronoi center

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
C      $Log: voronoi_center.f,v $
C      Revision 2.00  2007/11/09 20:04:06  spchu
C      Import to CVS
C
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

C####################

      subroutine voronoi_vector_area (
     1      x1,y1,z1,
     1      x2,y2,z2,
     1      x3,y3,z3,
     5      xarea,  yarea,  zarea  )
c
c     This code takes 3 pts of triangle as input 
c     and outputs the Voronoi area associated with first point 
c     which is the sum of 2 triangles formed with 
c     point 1, edge midpoints, voronoi center point
c
c     note convention has faces pointing inward

      implicit none
  
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,xarea,yarea,zarea 
      real*8 xvor,yvor,zvor,xm,ym,zm,ax1,ay1,az1,ax2,ay2,az2

      xarea=0.0d+00
      yarea=0.0d+00
      zarea=0.0d+00

      call voronoi_center_2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *           xvor,yvor,zvor)

C           area of right side triangle first (pt,midpt,vorpt)
            xm=(x2+x1)*0.5d+00
            ym=(y2+y1)*0.5d+00
            zm=(z2+z1)*0.5d+00

            ax1=  0.5d+00*((y1-ym)*(zvor-zm)-(yvor-ym)*(z1-zm))
            ay1= -0.5d+00*((x1-xm)*(zvor-zm)-(xvor-xm)*(z1-zm))
            az1=  0.5d+00*((x1-xm)*(yvor-ym)-(xvor-xm)*(y1-ym))

C           area of left side triangle (pt,vorpt,midpt)
            xm=(x3+x1)*0.5d+00
            ym=(y3+y1)*0.5d+00
            zm=(z3+z1)*0.5d+00

            ax2=  0.5d+00*((y1-yvor)*(zm-zvor)-(ym-yvor)*(z1-zvor))
            ay2= -0.5d+00*((x1-xvor)*(zm-zvor)-(xm-xvor)*(z1-zvor))
            az2=  0.5d+00*((x1-xvor)*(ym-yvor)-(xm-xvor)*(y1-yvor))

            xarea =  ax1 + ax2
            yarea =  ay1 + ay2
            zarea =  az1 + az2

c           print*,"area 1:   ",ax1,ay1,az1
c           print*,"area 2:   ",ax2,ay2,az2
c           print*,"area sum: ",xarea,yarea,zarea

      return
      end
