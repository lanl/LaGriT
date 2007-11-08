*dk getuv
      subroutine getuv(node,list,ndeg,x,y,z,eu,ev,planar,u,v)
C #####################################################################
C
C     PURPOSE -
C
C        Convert the position of NODE and its neighbour nodes from
C        (x,y,z) to (u,v) for 2D mesh smoothing.
C
C     INPUT ARGUMENTS -
C
C        NODE   --   Node whose position is to be converted.
C        LIST   --   List of neighbours of NODE.
C        NDEG   --   Number of neighbours.
C        X,Y,Z  --   The xic,yic,zic arrays for the current mesh.
C        EU(3), --   Unit vectors in (x,y,z) space that correspond to the
C        EV(3)       U and V directions in 2D planar case.
C        PLANAR --   If .TRUE. 2D mesh is planar; if .FALSE. mesh is
C                    nonplanar and EU, EV vectors must be computed.
C
C     OUTPUT ARGUMENTS -
C
C        U(0:NDEG), -- Output (u,v) coordinates of NODE and its neighbours,
C        V(0:NDEG)     with '0' corresponding to NODE, and >0 corresponding
C                      to the neighbours.
C
C     CHANGE HISTORY -
C$Log: getuv.f,v $
CRevision 2.00  2007/11/05 19:45:57  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:50:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   08/10/95 16:49:04   dcg
CPVCS    replace print * with writloga
CPVCS
CPVCS       Rev 1.0   02/15/95 13:37:38   dcg
CPVCS    Original version
C
C ######################################################################
      implicit none
      integer node,list(10000000),ndeg
      real*8 x(10000000),y(10000000),z(10000000),eu(3),ev(3),
     &   u(0:10000000),v(0:10000000)
      logical planar
      character*132 logmess
 
      integer i, ierrw
 
      if (.not.planar) then
         write (logmess,'(a)') 'Error: 2D non-planar unimplemented'
         call writloga('default',0,logmess,0,ierrw)
         stop
      endif
      u(0)=eu(1)*x(node)+eu(2)*y(node)+eu(3)*z(node)
      v(0)=ev(1)*x(node)+ev(2)*y(node)+ev(3)*z(node)
      do i=1,ndeg
         u(i)=eu(1)*x(list(i))+eu(2)*y(list(i))+eu(3)*z(list(i))
         v(i)=ev(1)*x(list(i))+ev(2)*y(list(i))+ev(3)*z(list(i))
      enddo
      return
      end
