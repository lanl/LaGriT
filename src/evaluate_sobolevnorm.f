 
      subroutine evaluate_sobolevnorm(ieltary,ieltno,itet
     &   ,itetoff,x,y,z,hxx,hxy,hxz,hyy,hyz,hzz,sob)
 
C
C #####################################################################
C
C     PURPOSE -
C
C     EVALUATE_SOBOLEVNORM evaluates the Sobolev seminorm estimate
C     which is the integrated squared norm of the difference
C     between the solution gradient and the numerical solution
C     gradient.  This is approximated to leading order using
C     the Hessian (assumed to be in partition ISUBNAME).
C     See literature on Minimum Error Gradient Adaption for more info.
C
C     INPUT ARGUMENTS -
C
C         ieltary - array of elements
C         ieltno - length of IELTARY
C         itet - element-node relation
C         itetoff - offset for ITET
C         x,y,z - node positions
C         hxx,hxy,etc. - Hessian components
C
C     OUTPUT ARGUMENTS -
C
C         sob - estimate of Sobolev seminorm
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/evaluate_sobolevnorm.f_a  $
CPVCS    
CPVCS       Rev 1.2   28 May 2002 16:01:32   dcg
CPVCS    access hessian matrix with correct indirect addressing
CPVCS    
CPVCS       Rev 1.1   07 Jan 2002 20:25:00   kuprat
CPVCS    Put Hessian into parameter list.
CPVCS    
CPVCS       Rev 1.0   19 Dec 2001 14:29:52   kuprat
CPVCS    Initial revision.
C
      implicit none
 
      include 'consts.h'
 
      integer ii,j,ieltno,ieltary(*),ihyb,i1,i2,i3,i4,itet(*),itetoff(*)
     &   ,k,length,icscode
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*)
      real*8 sob,afac(3,4),x(*),y(*),z(*),vol,aa(4,4),volfac,b(6,6),v(6)
     &   ,bv(6),ex,ey,ez,sobtet
 
      real*8 crosx,crosy,crosz,x1,y1,z1,x2,y2,z2,
     &   x3,y3,z3
 
c...  Statement functions for the components of the cross product
c...  ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
 
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
      sob=0.d0
 
      do ii=1,ieltno
         ihyb=ieltary(ii)
c.... This code is meant only for IHYB being a tetrahedron.
         i1=itet(itetoff(ihyb)+1)
         i2=itet(itetoff(ihyb)+2)
         i3=itet(itetoff(ihyb)+3)
         i4=itet(itetoff(ihyb)+4)
 
c.... Compute contribution of each tet to the
c.... polyhedral functional.
 
c...  Compute area vector of face opposite NODE
 
         afac(1,1)=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &      x(i4),y(i4),z(i4))*0.5d0
         afac(2,1)=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &      x(i4),y(i4),z(i4))*0.5d0
         afac(3,1)=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &      x(i4),y(i4),z(i4))*0.5d0
 
c...  Compute area vectors of faces containing NODE.
 
         afac(1,2)=crosx(x(i1),y(i1),z(i1),x(i4),y(i4),z(i4),x(i3)
     &      ,y(i3),z(i3))*0.5d0
         afac(2,2)=crosy(x(i1),y(i1),z(i1),x(i4),y(i4),z(i4),x(i3)
     &      ,y(i3),z(i3))*0.5d0
         afac(3,2)=crosz(x(i1),y(i1),z(i1),x(i4),y(i4),z(i4),x(i3)
     &      ,y(i3),z(i3))*0.5d0
         afac(1,3)=crosx(x(i1),y(i1),z(i1),x(i2),y(i2),z(i2),x(i4)
     &      ,y(i4),z(i4))*0.5d0
         afac(2,3)=crosy(x(i1),y(i1),z(i1),x(i2),y(i2),z(i2),x(i4)
     &      ,y(i4),z(i4))*0.5d0
         afac(3,3)=crosz(x(i1),y(i1),z(i1),x(i2),y(i2),z(i2),x(i4)
     &      ,y(i4),z(i4))*0.5d0
         afac(1,4)=crosx(x(i1),y(i1),z(i1),x(i3),y(i3),z(i3),x(i2)
     &      ,y(i2),z(i2))*0.5d0
         afac(2,4)=crosy(x(i1),y(i1),z(i1),x(i3),y(i3),z(i3),x(i2)
     &      ,y(i2),z(i2))*0.5d0
         afac(3,4)=crosz(x(i1),y(i1),z(i1),x(i3),y(i3),z(i3),x(i2)
     &      ,y(i2),z(i2))*0.5d0
 
c...  Compute volume of tetrahedron.
 
         vol=((x(i2)-x(i1))*afac(1,1)+(y(i2)-y(i1))*afac(2,1)+(z(i2
     &      )-z(i1))*afac(3,1))*one3rd
 
c.... Compute matrix of dot products of area vectors
 
         do j=1,4
            aa(j,j)=afac(1,j)**2+afac(2,j)**2+afac(3,j)**2
            do k=j+1,4
               aa(j,k)=afac(1,j)*afac(1,k)+afac(2,j)*afac(2,k)
     &            +afac(3,j)*afac(3,k)
c$$$                        aa(k,j)=aa(j,k)
            enddo
         enddo
 
c.... Compute matrix of dot products of gradients of quadratic basis
c.... functions.
 
         volfac=1.d0/(180.d0*vol)
 
c.... Diagonal terms
         b(1,1)=2*volfac*(aa(1,1)+aa(1,2)+aa(2,2))
         b(2,2)=2*volfac*(aa(1,1)+aa(1,3)+aa(3,3))
         b(3,3)=2*volfac*(aa(1,1)+aa(1,4)+aa(4,4))
         b(4,4)=2*volfac*(aa(2,2)+aa(2,3)+aa(3,3))
         b(5,5)=2*volfac*(aa(2,2)+aa(2,4)+aa(4,4))
         b(6,6)=2*volfac*(aa(3,3)+aa(3,4)+aa(4,4))
 
c.... Skew-Edge Interactions
         b(1,6)=volfac*(aa(1,3)+aa(1,4)+aa(2,3)+aa(2,4))
         b(2,5)=volfac*(aa(1,2)+aa(1,4)+aa(2,3)+aa(3,4))
         b(3,4)=volfac*(aa(1,2)+aa(1,3)+aa(2,4)+aa(3,4))
 
c.... Adjacent Edge Interactions
 
         b(1,2)=volfac*(2*aa(2,3)-aa(1,4))
         b(5,6)=b(1,2)
 
         b(1,3)=volfac*(2*aa(2,4)-aa(1,3))
         b(4,6)=b(1,3)
 
         b(1,4)=volfac*(2*aa(1,3)-aa(2,4))
         b(3,6)=b(1,4)
 
         b(1,5)=volfac*(2*aa(1,4)-aa(2,3))
         b(2,6)=b(1,5)
 
         b(2,3)=volfac*(2*aa(3,4)-aa(1,2))
         b(4,5)=b(2,3)
 
         b(2,4)=volfac*(2*aa(1,2)-aa(3,4))
         b(3,5)=b(2,4)
 
c...  Compute the edge errors over the edges {1,...,6}.
 
         ex=x(i2)-x(i1)
         ey=y(i2)-y(i1)
         ez=z(i2)-z(i1)
         v(1)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)
     &      *ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez)
 
         ex=x(i3)-x(i1)
         ey=y(i3)-y(i1)
         ez=z(i3)-z(i1)
         v(2)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)
     &      *ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez)
 
         ex=x(i4)-x(i1)
         ey=y(i4)-y(i1)
         ez=z(i4)-z(i1)
         v(3)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)
     &      *ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez)
 
         ex=x(i2)-x(i3)
         ey=y(i2)-y(i3)
         ez=z(i2)-z(i3)
         v(4)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)
     &      *ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez)
 
         ex=x(i2)-x(i4)
         ey=y(i2)-y(i4)
         ez=z(i2)-z(i4)
         v(5)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)
     &      *ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez)
 
         ex=x(i3)-x(i4)
         ey=y(i3)-y(i4)
         ez=z(i3)-z(i4)
         v(6)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)
     &      *ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez)
 
c.... Compute the products B * V.
 
         do j=1,6
            bv(j)=0.d0
         enddo
         do j=1,6
            bv(j)=bv(j)+b(j,j)*v(j)
            do k=j+1,6
               bv(j)=bv(j)+b(j,k)*v(k)
               bv(k)=bv(k)+b(j,k)*v(j)
            enddo
         enddo
 
c...  Compute SOBTET=V * BV.
         sobtet=0.d0
         do j=1,6
            sobtet=sobtet+v(j)*bv(j)
         enddo
 
C...  Increment the H1 seminorm functional by the tet contribution.
         sob=sob+sobtet
 
      enddo
 
      return
      end
