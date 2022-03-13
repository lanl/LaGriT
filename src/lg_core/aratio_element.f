*deck aratio_element
      subroutine aratio_element(ielmtyp,
     *                          xicasp,yicasp,zicasp,
     *                          aratelm, epsilon)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the aspect ratio of an element
C
C     INPUT ARGUMENTS -
C
C        ielmtyp                      : The element type.
C        (xicasp(),yicasp(),zicasp()  : The coordinates of the element.
C        epsilon                      : epsilon to use in denominators
C
C     OUTPUT ARGUMENTS -
C
C        aratelm : The aspect ratio of the element.
C
C     CHANGE HISTORY -
C
C $Log: aratio_element.f,v $
C Revision 2.00  2007/11/05 19:45:46  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:13:02 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      integer ielmtyp
      real*8 aratelm
      real*8 xicasp(1000000), yicasp(1000000), zicasp(1000000)
      real*8 epsilon
      integer icscode
C
C ######################################################################
C
C
 
      if (ielmtyp .eq. ifelmtet) then
         call aratio_tet(xicasp(1),yicasp(1),zicasp(1),
     *                   xicasp(2),yicasp(2),zicasp(2),
     *                   xicasp(3),yicasp(3),zicasp(3),
     *                   xicasp(4),yicasp(4),zicasp(4),
     *                   aratelm,epsilon)
      elseif (ielmtyp .eq. ifelmqud) then
         call aratio_qud(xicasp(1),yicasp(1),zicasp(1),
     *                   xicasp(2),yicasp(2),zicasp(2),
     *                   xicasp(3),yicasp(3),zicasp(3),
     *                   xicasp(4),yicasp(4),zicasp(4),
     *                   aratelm)
      elseif (ielmtyp .eq. ifelmhex) then
         call aratio_hex(xicasp(1),yicasp(1),zicasp(1),
     *                   xicasp(2),yicasp(2),zicasp(2),
     *                   xicasp(3),yicasp(3),zicasp(3),
     *                   xicasp(4),yicasp(4),zicasp(4),
     *                   xicasp(5),yicasp(5),zicasp(5),
     *                   xicasp(6),yicasp(6),zicasp(6),
     *                   xicasp(7),yicasp(7),zicasp(7),
     *                   xicasp(8),yicasp(8),zicasp(8),
     *                   aratelm)
      elseif (ielmtyp .eq. ifelmtri) then
         call aratio_tri(xicasp(1),yicasp(1),zicasp(1),
     *                   xicasp(2),yicasp(2),zicasp(2),
     *                   xicasp(3),yicasp(3),zicasp(3),
     *                   aratelm)
      else
         aratelm = 0.0
         write(logmess,1000) ielmtyp
 1000    format('Invalid element aratio type: ',i10)
         call writloga('default',0,logmess,0,icscode)
      endif
 
      return
      end
