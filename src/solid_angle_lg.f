*dk solid_angle_lg
C #####################################################################
C     PURPOSE -
C       find solid angle at 4 of the triangle (1,2,3)
C
C     INPUT ARGUMENTS -
C       x,y,z positions of the 4 points
C       ordering convention: (1,2,3) cyclic points at 4
C
C     OUTPUT ARGUMENTS -
C       solid angle
C       (units: stereradians or however you spell it: circle is 4pi)
C
C     CHANGE HISTORY -
C$Log:   /pvcs.config/t3d/src/solid_angle_lg.f_a  
CPVCS    
CPVCS       Rev 1.0   Fri Sep 17 17:27:30 1999   jtg
CPVCS    Initial revision.
C #####################################################################

        subroutine solid_angle_lg(solid_angle
     &          ,x1,y1,z1, x2,y2,z2, x3,y3,z3, x4,y4,z4)

        implicit none

        integer MXN,MXEL
        parameter (MXN=3000,MXEL=10000)
        real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4
     &          ,solid_angle
     &          ,x(4),y(4),z(4)
     &          ,dx1,dy1,dz1,dx2,dy2,dz2,d11,d12,d22,cos_ang
        integer ii(4),k,i1,i2,i3,i4

c -----------------------------------------------------------------
c statement functions (may want to convert to in-line ...)

        real*8 crosx,crosy,crosz
        real*8 cx1,cy1,cz1,cx2,cy2,cz2,cx3,cy3,cz3

        crosx(cx1,cy1,cz1,cx2,cy2,cz2,cx3,cy3,cz3)
     &          =(cy2-cy1)*(cz3-cz1)-(cz2-cz1)*(cy3-cy1)
        crosy(cx1,cy1,cz1,cx2,cy2,cz2,cx3,cy3,cz3)
     &          =(cz2-cz1)*(cx3-cx1)-(cx2-cx1)*(cz3-cz1)
        crosz(cx1,cy1,cz1,cx2,cy2,cz2,cx3,cy3,cz3)
     &          =(cx2-cx1)*(cy3-cy1)-(cy2-cy1)*(cx3-cx1)

c -----------------------------------------------------------------

c solid angle from "area of spherical triangle" formula:
c for triangle on sphere of radius r, area of triangle is
c   area = (a_1+a_2+a_3-pi)*r*r
c where a_j=angle of j-th corner in tangent plane at that corner
c [I don't guarantee this is the most efficient coding]

c note: the sum of the solid angles of a tetrahedron should
c lie in range pi to 2*pi (? I think...),
c sum of solid angles around an interior point = 4*pi (tested)

        ! ..........................................
        ! convert to vectors for convenience
        ! and so code below could be inserted elsewhere
        ! with, eg, ii(j)=itet(j,iel) from itet relation
        ! (per coding commented out with "c|el|" below)

        x(1)=x1
        x(2)=x2
        x(3)=x3
        x(4)=x4
        y(1)=y1
        y(2)=y2
        y(3)=y3
        y(4)=y4
        z(1)=z1
        z(2)=z2
        z(3)=z3
        z(4)=z4
        do k=1,4
         ii(k)=k
        enddo

        ! ..........................................
        ! calc solid angle

c|el|   do iel=1,nelements
c|el|    do j=1,4
c|el|     if (mod(j,2).eq.0) then
c|el|      ii(1)=itet(j,iel)
c|el|      ii(2)=itet(1+mod(j,4),iel)
c|el|     else
c|el|      ii(2)=itet(j,i)
c|el|      ii(1)=itet(1+mod(j,4),i)
c|el|     endif
c|el|     ii(3)=itet(1+mod(j+1,4),i)
c|el|     ii(4)=itet(1+mod(j+2,4),i)
c|el|    enddo

        ! may want to change to pi from data statement....
        solid_angle=-3.14159265358d0

        i4=ii(4)
        do k=1,3

          i1=ii(k)
          i2=ii(1+mod(k,3))
          i3=ii(1+mod(k+1,3))

          dx1=crosx(x(i4),y(i4),z(i4)
     &                  ,x(i3),y(i3),z(i3),x(i1),y(i1),z(i1))
          dy1=crosy(x(i4),y(i4),z(i4)
     &                  ,x(i3),y(i3),z(i3),x(i1),y(i1),z(i1))
          dz1=crosz(x(i4),y(i4),z(i4)
     &                  ,x(i3),y(i3),z(i3),x(i1),y(i1),z(i1))
          dx2=crosx(x(i4),y(i4),z(i4)
     &                  ,x(i3),y(i3),z(i3),x(i2),y(i2),z(i2))
          dy2=crosy(x(i4),y(i4),z(i4)
     &                  ,x(i3),y(i3),z(i3),x(i2),y(i2),z(i2))
          dz2=crosz(x(i4),y(i4),z(i4)
     &                  ,x(i3),y(i3),z(i3),x(i2),y(i2),z(i2))

          d11=(dx1*dx1+dy1*dy1+dz1*dz1)
          d22=(dx2*dx2+dy2*dy2+dz2*dz2)
          d12=(dx1*dx2+dy1*dy2+dz1*dz2)

          cos_ang=d12/dsqrt(d11*d22)
          solid_angle=solid_angle + dacos(cos_ang)

          ! note the assumption has been made that this angle is
          ! between 0-180 degrees (ie, the triangle was correctly oriented)
          ! should be tested in general by also calculating the sine

        enddo
        ! write(*,*) solid_angle/(4.d0*3.14159265358d0)

c|el|    s_ang(1+mod(j+2,4),iel)=solid_angle
c|el|   enddo

        ! ..........................................
        return
        end
