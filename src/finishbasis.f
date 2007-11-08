*dk,finishbasis
      subroutine finishbasis(u,v,w)
C #####################################################################
C
C     PURPOSE -
C
C        Give a unit vector U, FINISHBASIS selects unit vectors
C        V,W so that U-V-W are a righthanded orthonormal basis for R^3.
C
C     INPUT ARGUMENTS -
C
C        U - Unit vector in R^3.
C
C     OUTPUT ARGUMENTS -
C
C        V,W ...  Orthonormal vectors to complete a righthanded
C            orthonormal basis for R^3.
C
C     CHANGE HISTORY -
C $Log:   /pvcs.config/t3d/src/finishbasis.f_a  $
CPVCS    
CPVCS       Rev 1.0   31 Jan 2000 17:11:52   kuprat
CPVCS    Initial revision.
C
C ######################################################################

      implicit none
      include 'consts.h'
 
      real*8 u(3),v(3),w(3),au1,au2,au3,vlen
 
C Our choice of V and W depend on which is the smallest component of U.
 
      au1=abs(u(1))
      au2=abs(u(2))
      au3=abs(u(3))
 
      if ((au3.le.au1).and.(au3.le.au2)) then
         v(1)=-u(2)
         v(2)=u(1)
         v(3)=zero
         w(1)=-u(1)*u(3)
         w(2)=-u(2)*u(3)
         w(3)=u(1)**2+u(2)**2
 
      elseif (au2.le.au1) then
 
         v(3)=-u(1)
         v(1)=u(3)
         v(2)=zero
         w(3)=-u(3)*u(2)
         w(1)=-u(1)*u(2)
         w(2)=u(3)**2+u(1)**2
 
      else
 
         v(2)=-u(3)
         v(3)=u(2)
         v(1)=zero
         w(2)=-u(2)*u(1)
         w(3)=-u(3)*u(1)
         w(1)=u(2)**2+u(3)**2
 
      endif
 
c   Normalize V and W.
 
      vlen=sqrt(v(1)**2+v(2)**2+v(3)**2)
      v(1)=v(1)/vlen
      v(2)=v(2)/vlen
      v(3)=v(3)/vlen
 
      vlen=sqrt(w(1)**2+w(2)**2+w(3)**2)
      w(1)=w(1)/vlen
      w(2)=w(2)/vlen
      w(3)=w(3)/vlen
 
9999  continue
      return
      end
 
