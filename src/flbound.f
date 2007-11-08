*dk flbound
      subroutine flbound(iop,ibd,ibtp,x1,y1,z1,x2,y2,z2,x3,y3,z3)
C
C #####################################################################
C
C     PURPOSE -
C
ccht
ccht
ccht  this routine files reflective boundary information
ccht
ccht  FORMAT: FLBOUND/iop/ib#/ibtp/x1/y1/z1/x2/y2/z2/x3/y3/z3
ccht
c
c iop - an option flag used to determine the function of this call
c
c       iop = 1 ==> file the reflective boundary type (ibtp) and the
c                   three coordinate triplets for the 'ibd' reflective
c                   plane.
c                      x1 = x-coordinate of the 1st reflective corner
c                      y1 = y-coordinate of the 1st reflective corner
c                      z1 = z-coordinate of the 1st reflective corner
c                      x2 = x-coordinate of the 2nd reflective corner
c                      y2 = y-coordinate of the 2nd reflective corner
c                      z2 = z-coordinate of the 2nd reflective corner
c                      x3 = x-coordinate of the 3rd reflective corner
c                      y3 = y-coordinate of the 3rd reflective corner
c                      z3 = z-coordinate of the 3rd reflective corner
c       iop = 2 ==> file the reflective boundary type (ibtp) and the
c                   three velocity triplets for the 'ibd' reflective
c                   plane.
c                      x1 = x-velocity of the 1st reflective corner
c                      y1 = y-velocity of the 1st reflective corner
c                      z1 = z-velocity of the 1st reflective corner
c                      x2 = x-velocity of the 2nd reflective corner
c                      y2 = y-velocity of the 2nd reflective corner
c                      z2 = z-velocity of the 2nd reflective corner
c                      x3 = x-velocity of the 3rd reflective corner
c                      y3 = y-velocity of the 3rd reflective corner
c                      z3 = z-velocity of the 3rd reflective corner
c       iop = 3 ==> file the reflective boundary type (ibtp) and the
c                   pressure, density, internal energy and temperature
c                   for the 'ibd' reflective plane.
c                      x1 = pressure
c                      y1 = density
c                      z1 = internal energy
c                      x2 = temperature
c
c
c      ibtp = 2 ==> moving reflective boundary
c      ibtp = 3 ==> infinite, reflective boundary
c      ibtp = 4 ==>   finite, reflective boundary
cC
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/flbound.f_a  $
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:48:16 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   12/26/94 10:35:26   het
CPVCS    Corrected errors with the header format.
CPVCS    
CPVCS
CPVCS       Rev 1.1   12/24/94 10:52:14   het
CPVCS    Add include files for chydro.h and comdict.h.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:26   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
      include 'chydro.h'
C
C
C#######################################################################
C
C
ccht
ccht
      goto (100,200,300) iop
100   continue
ccht  nb=nb+1
      ib(ibd)=ibtp
      xbb(1,ibd)=x1
      ybb(1,ibd)=y1
      zbb(1,ibd)=z1
      xbb(2,ibd)=x2
      ybb(2,ibd)=y2
      zbb(2,ibd)=z2
      xbb(3,ibd)=x3
      ybb(3,ibd)=y3
      zbb(3,ibd)=z3
      goto 9999
ccht
ccht
ccht
200   continue
ccht
ccht
ccht if a boundary velocity is specified then the type is set here
ccht
      ib(ibd)=ibtp
      ubb(1,ibd)=x1
      vbb(1,ibd)=y1
      wbb(1,ibd)=z1
      ubb(2,ibd)=x2
      vbb(2,ibd)=y2
      wbb(2,ibd)=z2
      ubb(3,ibd)=x3
      vbb(3,ibd)=y3
      wbb(3,ibd)=z3
      goto 9999
ccht
ccht
ccht
 300  continue
ccht
ccht if boundary condition are set then the boundary type
ccht is set here
ccht
      ib(ibd)=ibtp
ccht
      pbb(ibd)=x1
      rbb(ibd)=y1
      ebb(ibd)=z1
      tbb(ibd)=x2
      goto 9999
9999  continue
      return
      end
