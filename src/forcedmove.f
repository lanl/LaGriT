*dk,forcedmove
      subroutine forcedmove(nexact,ncurv,curverr,curvdir,conbasis,
     &   dx,dy,dz)
C #####################################################################
C
C     PURPOSE -
C
C     INPUT ARGUMENTS -
C
C     OUTPUT ARGUMENTS -
C
C     CHANGE HISTORY -
C $Log:   /pvcs.config/t3d/src/forcedmove.f_a  $
CPVCS    
CPVCS       Rev 1.0   31 Jan 2000 17:12:16   kuprat
CPVCS    Initial revision.
C
C ######################################################################

      implicit none
 
      integer nexact,ncurv,indcurv
      real*8 conbasis(3,3),dx,dy,dz,curverr(3),curvdir(3,3),dxnew(3)
 
      if (ncurv.ne.1) goto 9999
 
c Compute forced movement.
 
      indcurv=3-nexact
 
      dxnew(1)=-curverr(1)/(curvdir(1,1)*conbasis(1,indcurv)+
     &   curvdir(2,1)*conbasis(2,indcurv)+
     &   curvdir(3,1)*conbasis(3,indcurv))
 
      dx=conbasis(1,indcurv)*dxnew(1)
      dy=conbasis(2,indcurv)*dxnew(1)
      dz=conbasis(3,indcurv)*dxnew(1)
 
 9999 continue
      return
      end
 
