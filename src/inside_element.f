*dk inside_element
      subroutine inside_element(ielmtyp,
     &                          xicelm,yicelm,zicelm,
     &                          xtestpt,ytestpt,ztestpt,
     &                          iflag)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS WHETHER OR NOT THE TEST POINT (xtestpt,
C         ytestpt, ztestpt) IS WITHIN OR ON A FACE OF THE ELEMENT
C         DEFINED BY ielmtyp AND THE VERTICES xicelm(),yicelm(),zicelm()
C
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        ielmtyp                     - THE ELEMENT TYPE.
C        xicelm(),yicelm(),zicelm()  - THE COORDINATES OF THE TET.
C        xtestpt,ytestpt,ztestpt     - THE COORDINATES OF THE POINT
C                                      IN QUESTION.
C
C     OUTPUT ARGUMENTS -
C
C        iflag  - RETURNS -1 IF THE TEST POINT IS OUTSIDE THE TET,
C                 0 IF IT IS INSIDE THE TET, OR A POSITIVE NUMBER
C                 CORRESPONDING TO THE FACE OF THE TET IT IS ON.
C
C
C     $Log:   /pvcs.config/t3d/src/inside_element.f_a  $
CPVCS    
CPVCS       Rev 1.2   03 Jan 2007 10:59:38   tam
CPVCS    make implicit none, write value of iflag if debug
CPVCS    pass itmp instead of iflag to maintain iflag value
CPVCS    
CPVCS       Rev 1.1   22 Mar 2000 08:40:06   dcg
CPVCS     use local_epsilon in place of epsilon
CPVCS
CPVCS       Rev 1.0   Tue Aug 03 15:27:50 1999   bap
CPVCS    Initial revision.
C
C#######################################################################
C
C     Standard Declarations for this module.
C
C#######################################################################
C
C
C     implicit real*8 (a-h,o-z)
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicelm(1000000), yicelm(1000000), zicelm(1000000)
      real*8 local_epsilon
      real*8 dist_ab, dist_atestpt, dist_btestpt
      real*8 xtestpt, ytestpt, ztestpt
C
      integer ielmtyp
      integer iflag, itmp, ierror, idebug
C
C     ******************************************************************
C     Initialize Variables
C
      local_epsilon = 1.0e-10
C
C#######################################################################
C
C     Start finding out what is what
C
C#######################################################################
C
C
      idebug = iflag
      iflag = 0
      if(ielmtyp.eq.ifelmpnt) then
         if((abs(xicelm(1)-xtestpt).lt.local_epsilon).AND.
     &      (abs(yicelm(1)-ytestpt).lt.local_epsilon).AND.
     &      (abs(zicelm(1)-ztestpt).lt.local_epsilon)) then
            iflag=0
         else
            iflag=-1
         endif
      elseif(ielmtyp.eq.ifelmlin) then
         dist_ab = sqrt((xicelm(1)-xicelm(2))**2+
     &                  (yicelm(1)-yicelm(2))**2+
     &                  (zicelm(1)-zicelm(2))**2)
         dist_atestpt = sqrt((xicelm(1)-xtestpt)**2+
     &                       (yicelm(1)-ytestpt)**2+
     &                       (zicelm(1)-ztestpt)**2)
         dist_btestpt = sqrt((xicelm(2)-xtestpt)**2+
     &                       (yicelm(2)-ytestpt)**2+
     &                       (zicelm(2)-ztestpt)**2)
         if(abs(dist_ab-(dist_atestpt+dist_btestpt)).lt.
     &      (local_epsilon*dist_ab)) then
            iflag = 0
         else
            iflag = -1
         endif
C
C     Definition of inside a triangle or a quad exists such that
C     the point must be in the same plane as the triangle in question.
C     So, the first call ensures that the point is in the plane,
C
      elseif(ielmtyp.eq.ifelmtri) then
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_tri(xicelm(1),yicelm(1),zicelm(1),
     &                     xicelm(2),yicelm(2),zicelm(2),
     &                     xicelm(3),yicelm(3),zicelm(3),
     &                     xtestpt,ytestpt,ztestpt,itmp)
         if(itmp.le.0) then
            iflag = -1
            goto 9999
         endif
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_tri2d(xicelm(1),yicelm(1),zicelm(1),
     &                     xicelm(2),yicelm(2),zicelm(2),
     &                     xicelm(3),yicelm(3),zicelm(3),
     &                     xtestpt,ytestpt,ztestpt,itmp)
         iflag=itmp
      elseif(ielmtyp.eq.ifelmqud) then
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_quad(xicelm(1),yicelm(1),zicelm(1),
     &                      xicelm(2),yicelm(2),zicelm(2),
     &                      xicelm(3),yicelm(3),zicelm(3),
     &                      xicelm(4),yicelm(4),zicelm(4),
     &                      xtestpt,ytestpt,ztestpt,itmp)
         if(itmp.le.0) then
            iflag = -1
            goto 9999
         endif
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_quad2d(xicelm(1),yicelm(1),zicelm(1),
     &                      xicelm(2),yicelm(2),zicelm(2),
     &                      xicelm(3),yicelm(3),zicelm(3),
     &                      xicelm(4),yicelm(4),zicelm(4),
     &                      xtestpt,ytestpt,ztestpt,itmp)
         iflag=itmp
      elseif(ielmtyp.eq.ifelmtet) then
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_tet(xicelm(1),yicelm(1),zicelm(1),
     &                   xicelm(2),yicelm(2),zicelm(2),
     &                   xicelm(3),yicelm(3),zicelm(3),
     &                   xicelm(4),yicelm(4),zicelm(4),
     &                   xtestpt,ytestpt,ztestpt,itmp)
         iflag=itmp
      elseif(ielmtyp.eq.ifelmpyr) then
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_pyr(xicelm(1),yicelm(1),zicelm(1),
     &                   xicelm(2),yicelm(2),zicelm(2),
     &                   xicelm(3),yicelm(3),zicelm(3),
     &                   xicelm(4),yicelm(4),zicelm(4),
     &                   xicelm(5),yicelm(5),zicelm(5),
     &                   xtestpt,ytestpt,ztestpt,itmp)
         iflag=itmp
      elseif(ielmtyp.eq.ifelmpri) then
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_pri(xicelm(1),yicelm(1),zicelm(1),
     &                   xicelm(2),yicelm(2),zicelm(2),
     &                   xicelm(3),yicelm(3),zicelm(3),
     &                   xicelm(4),yicelm(4),zicelm(4),
     &                   xicelm(5),yicelm(5),zicelm(5),
     &                   xicelm(6),yicelm(6),zicelm(6),
     &                   xtestpt,ytestpt,ztestpt,itmp)
         iflag=itmp
      elseif(ielmtyp.eq.ifelmhex) then
         itmp=idebug
         if (idebug.eq.1) itmp=0
         call inside_hex(xicelm(1),yicelm(1),zicelm(1),
     &                   xicelm(2),yicelm(2),zicelm(2),
     &                   xicelm(3),yicelm(3),zicelm(3),
     &                   xicelm(4),yicelm(4),zicelm(4),
     &                   xicelm(5),yicelm(5),zicelm(5),
     &                   xicelm(6),yicelm(6),zicelm(6),
     &                   xicelm(7),yicelm(7),zicelm(7),
     &                   xicelm(8),yicelm(8),zicelm(8),
     &                   xtestpt,ytestpt,ztestpt,itmp)
         iflag=itmp
      else
         iflag=0
         write(logmess,9010) ielmtyp
 9010    format('Invalid element in inside_element, type: ',i10)
         call writloga('default',0,logmess,0,ierror)
      endif
C
 9999 continue
      if (idebug.gt.0) then
         write(logmess,'(a,i5)')
     *   "Exit inside_element iflag: ",iflag
         call writloga('default',0,logmess,0,ierror)
      endif
      return
      end
 
