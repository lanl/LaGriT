*DK coordsys
      subroutine coordsys(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE DEFINES A LOCAL COORDINATE SYSTEM TO BE IN EFFECT
C     UNTIL ANOTHER COORDINATE SYSTEM IS DEFINED OR THE NORMAL COORD-
C     INATE SYSTEM IS RESET.  THE NEW COORDINATE SYSTEM IS DEFINED BY
C     SPECIFYING AN ORIGIN, A POINT ON THE NEW X-Z PLANE AND A POINT
C     ON THE NEW Z-AXIS.  THESE POINTS ARE SPECIFIED IN THE NORMAL
C     COORDINATE SYSTEM.  THE OPTIONS AVALIABLE IN iopt ARE:
C       define - DEFINE A NEW LOCAL COORDINATE SYSTEM
C       normal - RETURN TO THE NORMAL COORDINATE SYSTEM
C       save - SAVE THE CURRENT COORDINATE SYSTEM FOR RECALL
C       restore - RECALL THE LAST SAVED COORDINATE SYSTEM
C
C
C     FORMAT: COORDSYS/IOPT/X0/Y0/Z0/XX/XY/XZ/ZX/ZY/ZZ
C          WHERE X0,Y0,Z0 IS THE LOCATION OF THE NEW ORIGIN,
C                XX,XY,XZ IS A POINT ON THE NEW X-Z PLANE AND
C                ZX,ZY,ZZ IS A POINT ON THE NEW Z-AXIS.
C                THESE POINTS ARE DEFINED WITH THE NORMAL
C                COORDINATE SYSTEM, AND USED ONLY WITH THE
C                define OPTION.
C
C
C     INPUT ARGUMENTS -
C
C        xmsgin - REAL ARRAY OF COMMAND INPUT VALUES
C        msgin  - INTEGER ARRAY OF COMMAND INPUT VALUES
C        imsgin - INTEGER ARRAY OF COMMAND INPUT VALUES
C        nwds   - NO. OF WORDS OF COMMAND INPUT VALUES
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/coordsys.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:42:32 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   09/19/95 13:11:06   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.0   07/17/95 16:44:48   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      character*132 logmess
C
      include "chydro.h"
      include "consts.h"
C
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      character*32 copt
C
      double precision xx,xy,xz,zx,zy,zz,xdist,zdist,axz,bxz,cxz,
     &                 ay1,by1,cy1,ax,bx,cx,ay,by,cy,az,bz,cz
C
C#######################################################################
C
C
C#######################################################################
C
C     ******************************************************************
C     GET iopt OPTION
C
      copt=cmsgin(2)
      lenopt=icharlnf(copt)
C
C     ******************************************************************
C     define OPTION - DEFINE A NEW COORDINATE SYSTEM
C
      if (copt(1:lenopt) .eq. 'define') then
         call test_argument_type(9,2,3,imsgin,xmsgin,cmsgin,msgtype,
     *                       nwds)
 
         x0=xmsgin(3)
         y0=xmsgin(4)
         z0=xmsgin(5)
         xx=xmsgin(6)
         xy=xmsgin(7)
         xz=xmsgin(8)
         zx=xmsgin(9)
         zy=xmsgin(10)
         zz=xmsgin(11)
C
C        ---------------------------------------------------------------
C        DETERMINE THE UNIT VECTOR ALONG THE NEW X-Z PLANE
C
         xdist=sqrt((xx-x0)**2 + (xy-y0)**2 + (xz-z0)**2)
         axz=(xx-x0)/xdist
         bxz=(xy-y0)/xdist
         cxz=(xz-z0)/xdist
C
C        ---------------------------------------------------------------
C        DETERMINE THE UNIT VECTOR ALONG THE NEW Z-AXIS
C
         zdist=sqrt((zx-x0)**2 + (zy-y0)**2 + (zz-z0)**2)
         az=(zx-x0)/zdist
         bz=(zy-y0)/zdist
         cz=(zz-z0)/zdist
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE NEW Y AXIS WHICH LIES
C        PERPENDICULAR TO THE NEW Z AXIS AND THE POINT ON THE
C        NEW X-Z PLANE (Z CROSS X).
C
         ay1=bz*cxz-bxz*cz
         by1=cz*axz-cxz*az
         cy1=az*bxz-axz*bz
         ay=ay1/sqrt(ay1*ay1 + by1*by1 + cy1*cy1)
         by=by1/sqrt(ay1*ay1 + by1*by1 + cy1*cy1)
         cy=cy1/sqrt(ay1*ay1 + by1*by1 + cy1*cy1)
C
C        ...............................................................
C        DETERMINE THE UNIT VECTOR ALONG THE NEW X AXIS WHICH LIES
C        PERPENDICULAR TO THE NEW Y AND Z AXIS (Y CROSS Z)
C
         ax=by*cz-bz*cy
         bx=cy*az-cz*ay
         cx=ay*bz-az*by
C
C        ---------------------------------------------------------------
C        SAVE THE UNIT VECTORS IN THE CURRENT ROTATION MATRIX.
C
         rotatc(1,1)=ax
         rotatc(1,2)=bx
         rotatc(1,3)=cx
         rotatc(2,1)=ay
         rotatc(2,2)=by
         rotatc(2,3)=cy
         rotatc(3,1)=az
         rotatc(3,2)=bz
         rotatc(3,3)=cz
C
C        ---------------------------------------------------------------
C        SAVE THE NEW ORIGIN IN THE CURRENT ORIGIN ARRAY.
C        SET THE NORMAL FLAG OFF.
C
         origc(1)=x0
         origc(2)=y0
         origc(3)=z0
         normflgc=1
C
      endif
C
C     ******************************************************************
C     normal OPTION - RESET NORMAL COORDINATE SYSTEM
C
      if (copt(1:lenopt) .eq. 'normal') then
C
C        ---------------------------------------------------------------
C        PLACE NORMAL UNIT VECTORS IN THE CURRENT ROTATION MATRIX.
C
         rotatc(1,1)=1.
         rotatc(1,2)=0.
         rotatc(1,3)=0.
         rotatc(2,1)=0.
         rotatc(2,2)=1.
         rotatc(2,3)=0.
         rotatc(3,1)=0.
         rotatc(3,2)=0.
         rotatc(3,3)=1.
C
C        ---------------------------------------------------------------
C        SAVE THE NORMAL ORIGIN IN THE CURRENT ORIGIN ARRAY.
C        SET THE NORMAL FLAG ON.
C
         origc(1)=0.
         origc(2)=0.
         origc(3)=0.
         normflgc=0
C
      endif
C
C     ******************************************************************
C     save OPTION - SAVE THE CURRENT COORDINATE SYSTEM FOR RECALL
C
      if (copt(1:lenopt) .eq. 'save') then
C
C        ---------------------------------------------------------------
C        PLACE CURRENT UNIT VECTORS IN THE SAVED ROTATION MATRIX.
C
         do 10 i=1,3
            do 10 j=1,3
               rotats(i,j)=rotatc(i,j)
   10    continue
C
C        ---------------------------------------------------------------
C        SAVE THE CURRENT ORIGIN IN THE SAVED ORIGIN ARRAY.
C        SAVE THE CURRENT NORMAL FLAG.
C
         origs(1)=origc(1)
         origs(2)=origc(2)
         origs(3)=origc(3)
         normflgs=normflgc
C
      endif
C
C     ******************************************************************
C     restore OPTION - RECALL THE LAST SAVED COORDINATE SYSTEM
C
      if (copt(1:lenopt) .eq. 'restore') then
C
C        ---------------------------------------------------------------
C        PLACE SAVED UNIT VECTORS IN THE CURRENT ROTATION MATRIX.
C
         do 20 i=1,3
            do 20 j=1,3
               rotatc(i,j)=rotats(i,j)
   20    continue
C
C        ---------------------------------------------------------------
C        RESTORE THE SAVED ORIGIN IN THE CURRENT ORIGIN ARRAY.
C        RESTORE THE SAVED NORMAL FLAG.
C
         origc(1)=origs(1)
         origc(2)=origs(2)
         origc(3)=origs(3)
         normflgc=normflgs
C
      endif
C
C     ******************************************************************
C     PRINT ORIGIN AND VECTOR DATA FOR CURRENT COORDINATE SYSTEM.
C
      write(logmess,6000) origc(1),origc(2),origc(3)
 6000 format('  The origin is now (',
     &       e14.7,', ',e14.7,', ',e14.7,')')
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,6001) rotatc(1,1),rotatc(1,2),rotatc(1,3)
 6001 format('  The unit vector for the x-axis is ',
     &       f10.7,'i  ',f10.7,'j  ',f10.7,'k')
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,6002) rotatc(2,1),rotatc(2,2),rotatc(2,3)
 6002 format('  The unit vector for the y-axis is ',
     &       f10.7,'i  ',f10.7,'j  ',f10.7,'k')
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,6003) rotatc(3,1),rotatc(3,2),rotatc(3,3)
 6003 format('  The unit vector for the z-axis is ',
     &       f10.7,'i  ',f10.7,'j  ',f10.7,'k')
      call writloga('default',0,logmess,0,ierrw)
C
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
      return
      end
