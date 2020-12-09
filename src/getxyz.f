*dk getxyz
      subroutine getxyz(ndeg,xin,yin,zin,du,dv,eu,ev,u,v,xnew,ynew,
     &   znew,areamin)
C #####################################################################
C
C     PURPOSE -
C
C        Given the (u,v) position of a node and its neighbours, and
C        given the proposed change DU, DV of that node, limit that
C        change to prevent triangle inversion, and output the safe new
C        (x,y,z) position of the node.
C
C     INPUT ARGUMENTS -
C
C        NDEG      --  Number of neighbours.
C        XIN,YIN,  --  Original x,y,z position of the node.
C        ZIN
C        DU,DV     --  Proposed change in position of node in (u,v) space.
C        EU(3),    --  Unit vectors in (x,y,z) space that correspond to the
C        EV(3)         U and V directions.
C        U(0:NDEG),--  Input (u,v) coordinates of the node and its neighbours,
C        V(0:NDEG)     with '0' corresponding to the node, and >0 corresponding
C                      to the neighbours.
C
C     OUTPUT ARGUMENTS -
C
C        XNEW,YNEW,  -- Safe new positions for the node.
C        ZNEW
C
C     CHANGE HISTORY -
C$Log: getxyz.f,v $
CRevision 2.00  2007/11/05 19:45:58  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.7   30 Sep 2004 11:17:54   dcg
CPVCS    make factionlost double precision
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 16:50:08 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Sun Dec 31 17:41:58 1995   kuprat
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   10/18/95 11:49:28   kuprat
CPVCS    Fixed up declaration.
CPVCS
CPVCS       Rev 1.3   10/17/95 17:04:14   dcg
CPVCS    fix subroutine statement continuation
CPVCS
CPVCS       Rev 1.2   10/13/95 13:00:54   kuprat
CPVCS    Added user tolerance for min area triangle
CPVCS
CPVCS       Rev 1.1   06/02/95 21:03:00   kuprat
CPVCS    Changed the triangle area collapse limit from something hardwired to
CPVCS    a variable parameter.
CPVCS
CPVCS
CPVCS       Rev 1.0   02/15/95 13:37:40   dcg
CPVCS    Original version
C
C ######################################################################
      implicit none
      include 'consts.h'
      real*8 fractionlost
      parameter (fractionlost=0.25d0)
      integer ndeg
      real*8 xin,yin,zin,du,dv,eu(3),ev(3),u(0:10000000),v(0:10000000),
     &  xnew,ynew,znew,areamin,allowedloss
 
      real*8 t,det,rate
      integer i,i1,i2
 
      t=1.
      do i=1,ndeg
         i1=i
         i2=mod(i,ndeg)+1
         det=(u(i2)-u(i1))*(v(0)-v(i1))-(v(i2)-v(i1))*(u(0)-u(i1))
         rate=(u(i2)-u(i1))*dv-(v(i2)-v(i1))*du
c
c We allow a loss of determinant equal to the lesser of
c (1) the difference of the current determinant and the
c     ABSOLUTE minimum possible determinant, and
c (2) the maximum fractional determinant lossrate times the
c     current determinant.
c
         allowedloss=min(det-2.*areamin,fractionlost*det)
c
c Only possibly limit the movement if the determinant is SHRINKING.
c
         if (rate.lt.zero) then
c
c We want actual loss <= ALLOWEDLOSS.
c That is, -RATE*T <= ALLOWEDLOSS.
c So       T <= -ALLOWEDLOSS/RATE.
c Also, note that we are rejecting movement in the OPPOSITE direction
c (T<0) as a way of fixing the problem.
c
            t=max(zero,min(t,-allowedloss/rate))
         endif
      enddo
 
      du=t*du
      dv=t*dv
      xnew=xin+du*eu(1)+dv*ev(1)
      ynew=yin+du*eu(2)+dv*ev(2)
      znew=zin+du*eu(3)+dv*ev(3)
      return
      end
