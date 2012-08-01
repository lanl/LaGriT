*dk,tst2to3i
      subroutine tst2to3i(n1,n2,n3,n4,n5,iflag1,iflag2,
     *                    npoints,ntets)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine first determines whether or not a 2-to-3 flip
C        connecting points "n4" to "n5" is geometrically consistent.
C        To be consistent, the line segment n4-n5 must pass through
C        the face n1-n2-n3.  This condition assures that all tetrahedra
C        produced by a 2-to-3 flip will have positive volume.  If the
C        structure is not geometrically consistent, the routine then
C        determines whether the tetrahedron on the interface will be
C        the only inverted tetrahedron produced by a 2-to-3i flip.
C
C     INPUT ARGUMENTS -
C
C        n1-n3    - the points that specify the common face between the
C                   two tets.
C        n4,n5    - the two points to be connected
C
C     OUTPUT ARGUMENTS -
C
C        iflag1   - 1 => geometrically consistent (the three new
C                        tetrahedra will all have positive volume)
C        iflag2   - 1 => only the boundary tetrahedron will be inverted
C                        in a 2-to-3 flip.
C
C     CHANGE HISTORY -
C
C        $Log: tst2to3i.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:05:22 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   12/02/94 15:07:14   het
CPVCS    Added an option for the "cmo" access functions
CPVCS    
CPVCS    
CPVCS       Rev 1.2   12/01/94 18:49:50   het
CPVCS    Added a data type to the "cmo" calles 
CPVCS       and added the "cmo.h" include file.
CPVCS    
CPVCS       Rev 1.1   11/17/94 21:55:48   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:04   pvcs
CPVCS    Original version.
C
C ######################################################################
      implicit none
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "cmerge.h"

C arguments (n1,n2,n3,n4,n5,iflag1,iflag2,npoints,ntets)
      integer n1,n2,n3,n4,n5,iflag1,iflag2,npoints,ntets

C variables
      integer i,j,k,ierror,lenxic,lenyic,lenzic,icmotype

      real*8 xst,xn1,yn1,zn1,sn1,xnorm1,ynorm1,znorm1,
     *       xnorm2,ynorm2,znorm2,xnorm3,ynorm3,znorm3,
     *       snorm1,snorm2,snorm3,dot1,dot2,dot3,
     *       test1,test2,test3

      real*8 crosx1,crosy1,crosz1
C
C ######################################################################
C
C     MACROS.
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
C
C ######################################################################
C BEGIN begin
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
      xst=1.0e-11
C
C     ******************************************************************
C
C     DETERMINE IN WHICH HALF-SPACES THE POINT "n5" LIES.
C
      xn1=xic(n5)-xic(n4)
      yn1=yic(n5)-yic(n4)
      zn1=zic(n5)-zic(n4)
      sn1=sqrt(xn1*xn1+yn1*yn1+zn1*zn1)
      xnorm1=crosx1(n4,n1,n2)
      ynorm1=crosy1(n4,n1,n2)
      znorm1=crosz1(n4,n1,n2)
      snorm1=sqrt(xnorm1**2+ynorm1**2+znorm1**2)
      xnorm2=crosx1(n4,n3,n1)
      ynorm2=crosy1(n4,n3,n1)
      znorm2=crosz1(n4,n3,n1)
      snorm2=sqrt(xnorm2**2+ynorm2**2+znorm2**2)
      xnorm3=crosx1(n4,n2,n3)
      ynorm3=crosy1(n4,n2,n3)
      znorm3=crosz1(n4,n2,n3)
      snorm3=sqrt(xnorm3**2+ynorm3**2+znorm3**2)
      dot1=(xn1*xnorm1+yn1*ynorm1+zn1*znorm1)
      dot2=(xn1*xnorm2+yn1*ynorm2+zn1*znorm2)
      dot3=(xn1*xnorm3+yn1*ynorm3+zn1*znorm3)
      iflag1=0
      iflag2=0
      test1=xst*sn1*snorm1
      test2=xst*sn1*snorm2
      test3=xst*sn1*snorm3
      if (dot1.ge.test1.and.dot2.ge.test2.and.dot3.ge.test3) then
         iflag1=1
      elseif (dot1.lt.0.and.dot2.ge.test2.and.dot3.ge.test3) then
         iflag2=1
      endif
C
      goto 9999
 9999 continue
      return
      end
