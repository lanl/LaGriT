      subroutine xsectelements(ielmtyp1,
     &                         xicelm1,yicelm1,zicelm1,
     &                         ielmtyp2,
     &                         xicelm2,yicelm2,zicelm2,
     &                         iflag)
c
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find call xsectelm
C        with the correctly ordered elements - ielmtyp1 < ielmtyp2
C
C     INPUT ARGUMENTS -
C        ielmtyp1                       The element type of element 1
C        xicelm1(),yicelm1(),zicelm1()  The coordinates of the nodes of
C                                       element 1
C        ielmtyp2                       The element type of element 2
C        xicelm2(),yicelm2(),zicelm2()  The coordinates of the nodes of
C                                       element 2
C
C     OUTPUT ARGUMENTS -
C        iflag                    Returns 0 if the elements intersect,
C
C######################################################################
C
C    $Log: xsectelm.f,v $
C    Revision 2.00  2007/11/09 20:04:06  spchu
C    Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   03 Jan 2007 13:02:22   tam
CPVCS    add debug using iflag value
CPVCS    Adjust coordinates only if numbers are very small
CPVCS    or if comparison of distance against size is large
CPVCS    This MAY not be needed since inside_element is better
CPVCS    these changes should not impact the called routines
CPVCS    
CPVCS       Rev 1.6   30 Sep 2004 11:25:52   dcg
CPVCS    make local_epsilon double precision
CPVCS
CPVCS       Rev 1.5   10 Apr 2001 14:20:42   dcg
CPVCS    change declaration of hexvertex to integer
CPVCS
CPVCS       Rev 1.4   22 Mar 2000 08:46:24   dcg
CPVCS    use local_epsilon in place of epsilon
CPVCS
CPVCS       Rev 1.3   Tue Feb 08 15:07:34 2000   dcg
CPVCS    get rid of comdict.h
CPVCS
CPVCS       Rev 1.2   06 Jan 2000 14:31:22   bap
CPVCS    Added support for hex/any element intersection, and tet/tet intersection
CPVCS    Corrected errors in some of the intersect_X_element routines.
CPVCS    Added more robust support for quads.
CPVCS
CPVCS       Rev 1.1   Thu Aug 19 09:48:26 1999   dcg
CPVCS    get rid of recursion
CPVCS    implement as implicit none
CPVCS
CPVCS       Rev 1.0   Wed Aug 04 10:50:36 1999   bap
CPVCS    Initial revision.
C
C######################################################################
      implicit none
      include 'local_element.h'
      integer ielmtyp1,ielmtyp2,iflag
      real*8 xicelm1(*),yicelm1(*),zicelm1(*),
     *       xicelm2(*),yicelm2(*),zicelm2(*)
C######################################################################


C
C     If element type 1 is greater than element type 2
C
      if (ielmtyp1.gt.ielmtyp2) then
         call xsectelm(ielmtyp2, xicelm2,yicelm2,zicelm2,
     &                      ielmtyp1, xicelm1,yicelm1,zicelm1,
     &                      iflag)
      else
         call xsectelm(ielmtyp1, xicelm1,yicelm1,zicelm1,
     &                      ielmtyp2, xicelm2,yicelm2,zicelm2,
     &                      iflag)
      endif
      return
      end
C
C
      subroutine xsectelm    ( ielmtyp1, xic1,yic1,zic1,
     &                         ielmtyp2, xic2,yic2,zic2,
     &                         iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find whether element1
C        and element2 intersect.
C
C     NOTES -
C        This code is currently FUNCTIONAL.
C        Code has been modified to translate both elements
C        to put xmin,ymin,zmin at zero
C        This avoids errors when working with coordinates
C        with large numbers
C        Also copy into local arrays to avoid changing originals
C
C
C     INPUT ARGUMENTS -
C        ielmtyp1                       The element type of element 1
C        xic1(),yic1(),zic1()           The coordinates of the nodes of
C                                       element 1
C        ielmtyp2                       The element type of element 2
C        xic2(),yic2(),zic2()           The coordinates of the nodes of
C                                       element 2
C
C     OUTPUT ARGUMENTS -
C        iflag                    Returns 0 if the elements intersect,
C                                 -1 if they don't.
C
C######################################################################
C
C     Variable declarations
C
      implicit none
      include "machine.h"
      include "chydro.h"
      include "local_element.h"

C     arguments
      integer ielmtyp1,ielmtyp2,iflag
      real*8 xic1(*), yic1(*), zic1(*)
      real*8 xic2(*), yic2(*), zic2(*)
    
      integer i,ierror
      real*8 xicelm1(8), yicelm1(8), zicelm1(8)
      real*8 xicelm2(8), yicelm2(8), zicelm2(8)
      real*8 xtrans,ytrans,ztrans,
     *       xmin,ymin,zmin,xmax,ymax,zmax
      character*132 logmess

C     Get idebug set from iflag then intialize to zero
C     Restrict to idebug greater than 1 so a setting of 1
C     does not generate output from all the lower calls
C     which can really slow down program speed
      idebug = iflag
      if (idebug .eq. 1) idebug=0
      iflag = 0

C     translate and scale coordinates as needed
C     icelm arrays will have the adjusted coordinates
C     the original coordinates in ic are unchanged
      call adjust_element_pair(
     &     xic1,yic1,zic1,xic2,yic2,zic2,
     &     xicelm1,yicelm1,zicelm1,
     &     xicelm2,yicelm2,zicelm2,
     &     ielmtyp1,ielmtyp2, iflag)

      if (iflag .lt. 0) then
        write(logmess,"(a)")
     &  "Warning: Precision errors may occur with very small values:"
        call writloga('default',0,logmess,0,ierror)
      endif
      if (iflag .gt. 0) then
        write(logmess,"(a)")
     &  "Warning: Precision errors may occur with very large values:"
        call writloga('default',0,logmess,0,ierror)
      endif
      if (iflag.ne.0) then
        write(logmess,"(a,1pe14.5e3,1pe14.5e3,1pe14.5e3)")
     &  "Element 1 xyz:  ", xic1(1),yic1(1),zic1(1)
        call writloga('default',0,logmess,0,ierror)
        write(logmess,"(a,1pe14.5e3,1pe14.5e3,1pe14.5e3)")
     &  "Element 2 xyz:  ", xic2(1),yic2(1),zic2(1)
        call writloga('default',0,logmess,0,ierror)
        iflag = 0
      endif


      if (ielmtyp1.eq.ifelmpnt) then
         call inside_element(ielmtyp2, xicelm2, yicelm2,zicelm2,
     &                       xicelm1(1),yicelm1(1),zicelm1(1),
     &                       iflag)
      elseif (ielmtyp1.eq.ifelmlin) then
         call intersect_line_element(ielmtyp2,xicelm2,yicelm2,zicelm2,
     &                               xicelm1(1),yicelm1(1),zicelm1(1),
     &                               xicelm1(2),yicelm1(2),zicelm1(2),
     &                               iflag)
      elseif (ielmtyp1.eq.ifelmtri) then
         call intersect_tri_element(ielmtyp2,xicelm2,yicelm2,zicelm2,
     &                              xicelm1(1),yicelm1(1),zicelm1(1),
     &                              xicelm1(2),yicelm1(2),zicelm1(2),
     &                              xicelm1(3),yicelm1(3),zicelm1(3),
     &                              iflag)
      elseif (ielmtyp1.eq.ifelmqud) then
         call intersect_quad_element(ielmtyp2,xicelm2,yicelm2,zicelm2,
     &                               xicelm1(1),yicelm1(1),zicelm1(1),
     &                               xicelm1(2),yicelm1(2),zicelm1(2),
     &                               xicelm1(3),yicelm1(3),zicelm1(3),
     &                               xicelm1(4),yicelm1(4),zicelm1(4),
     &                               iflag)
      elseif (ielmtyp1.eq.ifelmtet) then
         call intersect_tet_element(ielmtyp2,xicelm2,yicelm2,zicelm2,
     &                              xicelm1,yicelm1,zicelm1,iflag)
      elseif (ielmtyp1.eq.ifelmhex) then
         call intersect_hex_element(ielmtyp2,xicelm2,yicelm2,zicelm2,
     &                              xicelm1,yicelm1,zicelm1,iflag)
      else
         write(logmess,9010) ielmtyp1
 9010    format('Invalid element in xsecteelements, type: ',i10)
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     &        'Element types supported are: points, lines, tris,'
     &        // ' quads, tets, and hexes.'
         call writloga('default',0,logmess,0,ierror)
         iflag = -1
      endif

C     done with xsectelements 
      return
      end
C
C
      subroutine intersect_line_element(ielmtyp,
     &                                  xicelm,yicelm,zicelm,
     &                                  x1,y1,z1, x2,y2,z2,
     &                                  iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find out whether the
C        element fed to this routine intersects with the line defined
C        by (x1,y1,z1) and (x2,y2,z2).
C
C     ****************************************************************
C
C     NOTES -
C        Currently only for hexes, tets, quads, tris, and lines.
C
C     INPUT ARGUMENTS -
C        ielmtyp                     - THE ELEMENT TYPE.
C        xicelm(),yicelm(),zicelm()  - THE COORDINATES OF THE ELEMENT.
C        x1,y1,z1                    - THE 1st COORDINATES OF THE LINE.
C        x2,y2,z2                    - THE 2nd COORDINATES OF THE LINE.
C
C     OUTPUT ARGUMENTS -
C        iflag  - Returns -1 if the line does not intersect the
C                 element, O if it does, or a positive number
C                 corresponding to the face it is in.
C
C######################################################################
C
C     Standard Declarations for this module.
C
C######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicelm(*), yicelm(*), zicelm(*)
      real*8 xtemp,ytemp,ztemp,x1,x2,y1,y2,z1,z2,xmidpt,ymidpt,
     *    zmidpt,local_epsilon
C
C     set up a vertex array for the hex faces
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
      integer hexvertex(6,4)
C
      integer iflag, ielmtyp, i, j, ierror
      parameter(local_epsilon=1.0d-10)
         data (hexvertex(1,j), j=1,4)/1,2,3,4/
         data (hexvertex(2,j), j=1,4)/5,6,7,8/
         data (hexvertex(3,j), j=1,4)/5,6,2,1/
         data (hexvertex(4,j), j=1,4)/6,7,3,2/
         data (hexvertex(5,j), j=1,4)/7,8,4,3/
         data (hexvertex(6,j), j=1,4)/8,5,1,4/
C
C######################################################################
C
C     Start finding out what is what
C
C######################################################################
C
C
C     *****************************************************************
C     Hexes (Not the voodoo kind)
C
      if(ielmtyp.eq.ifelmhex) then
         call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                       x1,y1,z1,iflag)
         if(iflag.ge.0) return
         call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                       x2,y2,z2,iflag)

         if(iflag.ge.0) return
C
C     Ok... Now we know that the two end points are _NOT_ inside
C     the Hex, figure out whether or not they intersect any of the
C     sides.
C     The sides are based on the following six vertex quads:
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
 
C     Call the requisite additional functions
C
         do i=1,6
C     Find the midpoint of each face.
            xmidpt = 0.0
            ymidpt = 0.0
            zmidpt = 0.0
            do j = 1,4
               xmidpt = xmidpt + xicelm(hexvertex(i,j))/4
               ymidpt = ymidpt + yicelm(hexvertex(i,j))/4
               zmidpt = zmidpt + zicelm(hexvertex(i,j))/4
            enddo
C     Call the interesection routines on the proper two vertices and
C     the midpoint
            do j = 0,3
               iflag = 0
               call lineseg_tri(xicelm(hexvertex(i,mod(j,4)+1)),
     &                          yicelm(hexvertex(i,mod(j,4)+1)),
     &                          zicelm(hexvertex(i,mod(j,4)+1)),
     &                          xicelm(hexvertex(i,mod(j+1,4)+1)),
     &                          yicelm(hexvertex(i,mod(j+1,4)+1)),
     &                          zicelm(hexvertex(i,mod(j+1,4)+1)),
     &                          xmidpt, ymidpt, zmidpt,
     &                          x1,y1,z1,x2,y2,z2,
     &                          xtemp,ytemp,ztemp,iflag)
               if(iflag.ge.0) goto 9999
            enddo
         enddo
C        Sorry, you lose.
         goto 9999
 
C     *****************************************************************
C     Tets
C
      elseif(ielmtyp.eq.ifelmtet) then
         call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                       x1,y1,z1,iflag)
         if(iflag.ge.0) goto 9999
         call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                       x2,y2,z2,iflag)
         if(iflag.ge.0) goto 9999
         do i=0,3
            iflag = 0
            call lineseg_tri(xicelm(mod(i,4)+1),yicelm(mod(i,4)+1),
     &                      zicelm(mod(i,4)+1),
     &                      xicelm(mod(i+1,4)+1),yicelm(mod(i+1,4)+1),
     &                      zicelm(mod(i+1,4)+1),
     &                      xicelm(mod(i+2,4)+1),yicelm(mod(i+2,4)+1),
     &                      zicelm(mod(i+2,4)+1),
     &                      x1,y1,z1,x2,y2,z2,
     &                      xtemp,ytemp,ztemp,iflag)
 
            if(iflag.ge.0) goto 9999
         enddo
C
C     *****************************************************************
C     Quads
C
      elseif(ielmtyp.eq.ifelmqud) then
         xmidpt = 0.0
         ymidpt = 0.0
         zmidpt = 0.0
         do i = 1,4
            xmidpt = xmidpt + xicelm(i)/4
            ymidpt = ymidpt + yicelm(i)/4
            zmidpt = zmidpt + zicelm(i)/4
         enddo
         do i = 0,3
            call lineseg_tri(xicelm(mod(i,4)+1),yicelm(mod(i,4)+1),
     &                      zicelm(mod(i,4)+1),
     &                      xicelm(mod(i+1,4)+1),yicelm(mod(i+1,4)+1),
     &                      zicelm(mod(i+1,4)+1),
     &                      xmidpt, ymidpt, zmidpt,
     &                      x1,y1,z1,x2,y2,z2,
     &                      xtemp,ytemp,ztemp,iflag)
 
            if(iflag.ge.0) goto 9999
         enddo
C
C     *****************************************************************
C     Tris
C
      elseif(ielmtyp.eq.ifelmtri) then
         call lineseg_tri(xicelm(1),yicelm(1),zicelm(1),
     &                    xicelm(2),yicelm(2),zicelm(2),
     &                    xicelm(3),yicelm(3),zicelm(3),
     &                    x1,y1,z1, x2,y2,z2,
     &                    xtemp, ytemp, ztemp, iflag)
 
         if (iflag.ge.0) goto 9999
C
C     *****************************************************************
C     Line Segments
C
      elseif(ielmtyp.eq.ifelmlin) then
         call lineseg_lineseg(xicelm(1),yicelm(1),zicelm(1),
     &                        xicelm(2),yicelm(2),zicelm(2),
     &                        x1,y1,z1, x2,y2,z2, iflag)
 
         if(iflag.ge.0) goto 9999
C
C
C     *****************************************************************
C     Anything Else
C
      else
         iflag = -1
         write(logmess,9010) ielmtyp
 9010    format('Invalid element in intersect_line_element, type: ',
     &          i10)
         call writloga('default',0,logmess,0,ierror)
      endif
 9999 continue
      return
      end
C
C
      subroutine intersect_tri_element(ielmtyp,
     &                                 xicelm,yicelm,zicelm,
     &                                 x1,y1,z1, x2,y2,z2,
     &                                 x3,y3,z3,
     &                                 iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find out whether the
C        element fed to this routine intersects with the tri defined
C        by (x1,y1,z1), (x2,y2,z2), and (x3,y3,z3).
C
C     ****************************************************************
C
C     NOTES -
C        Currently only for hexes, tets, quads, and tris.
C
C     INPUT ARGUMENTS -
C        ielmtyp                     - THE ELEMENT TYPE.
C        xicelm(),yicelm(),zicelm()  - THE COORDINATES OF THE TET.
C        x1,y1,z1                    - THE 1st COORDINATES OF THE TRI.
C        x2,y2,z2                    - THE 2nd COORDINATES OF THE TRI.
C        x3,y3,z3                    - THE 3rd COORDINATES OF THE TRI.
C
C     OUTPUT ARGUMENTS -
C        iflag  - Returns -1 if the tri does not intersect the
C                 element, O if it does, or a positive number
C                 corresponding to the face it is in.
C
C######################################################################
C
C     Standard Declarations for this module.
C
C######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicelm(*), yicelm(*), zicelm(*), local_epsilon
      real*8 x1,y1,z1,x2,y2,z2,xmidpt,ymidpt,zmidpt,x3,y3,z3
C
C     set up a vertex array for the hex faces
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
      integer hexvertex(6,4)
C
      integer iflag,ielmtyp,i,j,ierror
      parameter(local_epsilon=1.0d-10)
C
C
C######################################################################
C
C     Start finding out what is what
C
C######################################################################
C
C     *****************************************************************
C     Hexes (Not the voodoo kind)
C
      if(ielmtyp.eq.ifelmhex) then
         call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                       x1,y1,z1,iflag)
         if(iflag.ge.0) return
         call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                       x2,y2,z2,iflag)
         if(iflag.ge.0) return
         call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                       x3,y3,z3,iflag)
         if(iflag.ge.0) return
C
C     Ok... Now we know that the three vertices are _NOT_ inside
C     the Hex, figure out whether or not they intersect any of the
C     sides.
C     The sides are based on the following six vertex quads:
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
         data (hexvertex(1,j), j=1,4)/1,2,3,4/
         data (hexvertex(2,j), j=1,4)/5,6,7,8/
         data (hexvertex(3,j), j=1,4)/5,6,2,1/
         data (hexvertex(4,j), j=1,4)/6,7,3,2/
         data (hexvertex(5,j), j=1,4)/7,8,4,3/
         data (hexvertex(6,j), j=1,4)/8,5,1,4/
C     Call the requisite additional functions
C
         do i=1,6
C     Find the midpoint of each face.
            xmidpt = 0.0
            ymidpt = 0.0
            zmidpt = 0.0
            do j = 1,4
               xmidpt = xmidpt + xicelm(hexvertex(i,j))/4.0d+00
               ymidpt = ymidpt + yicelm(hexvertex(i,j))/4.0d+00
               zmidpt = zmidpt + zicelm(hexvertex(i,j))/4.0d+00
            enddo

C     Call the interesection routines on the proper two vertices and
C     the midpoint
            do j = 0,3
               iflag = 0
               call tri_tri(xicelm(hexvertex(i,mod(j,4)+1)),
     &                          yicelm(hexvertex(i,mod(j,4)+1)),
     &                          zicelm(hexvertex(i,mod(j,4)+1)),
     &                          xicelm(hexvertex(i,mod(j+1,4)+1)),
     &                          yicelm(hexvertex(i,mod(j+1,4)+1)),
     &                          zicelm(hexvertex(i,mod(j+1,4)+1)),
     &                          xmidpt, ymidpt, zmidpt,
     &                          x1,y1,z1,
     &                          x2,y2,z2,
     &                          x3,y3,z3,
     &                          iflag)

               if(iflag.ge.0) goto 9999
            enddo
         enddo
C        Sorry, you lose.
         goto 9999
 
C
C     *****************************************************************
C     Tets
C
      elseif(ielmtyp.eq.ifelmtet) then
         call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                       x1,y1,z1,iflag)
         if(iflag.ge.0) goto 9999
         call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                       x2,y2,z2,iflag)
         if(iflag.ge.0) goto 9999
         call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                       x3,y3,z3,iflag)
         if(iflag.ge.0) goto 9999
         do i=0,3
            iflag = 0
            call tri_tri(xicelm(mod(i,4)+1),yicelm(mod(i,4)+1),
     &                   zicelm(mod(i,4)+1),
     &                   xicelm(mod(i+1,4)+1),yicelm(mod(i+1,4)+1),
     &                   zicelm(mod(i+1,4)+1),
     &                   xicelm(mod(i+2,4)+1),yicelm(mod(i+2,4)+1),
     &                   zicelm(mod(i+2,4)+1),
     &                   x1,y1,z1,x2,y2,z2,x3,y3,z3, iflag)
 
            if(iflag.ge.0) goto 9999
         enddo
C
C     *****************************************************************
C     Quads
C
      elseif(ielmtyp.eq.ifelmqud) then
         xmidpt = 0.0
         ymidpt = 0.0
         zmidpt = 0.0
         do i = 1,4
            xmidpt = xmidpt + xicelm(i)/4
            ymidpt = ymidpt + yicelm(i)/4
            zmidpt = zmidpt + zicelm(i)/4
         enddo
         do i = 0,3
            call tri_tri(xicelm(mod(i,4)+1),yicelm(mod(i,4)+1),
     &                   zicelm(mod(i,4)+1),
     &                   xicelm(mod(i+1,4)+1),yicelm(mod(i+1,4)+1),
     &                   zicelm(mod(i+1,4)+1),
     &                   xmidpt, ymidpt, zmidpt,
     &                   x1,y1,z1,x2,y2,z2,x3,y3,z3,iflag)
 
            if(iflag.ge.0) goto 9999
         enddo
C
C     *****************************************************************
C     Tris
C
      elseif(ielmtyp.eq.ifelmtri) then
         call tri_tri(xicelm(1),yicelm(1),zicelm(1),
     &                xicelm(2),yicelm(2),zicelm(2),
     &                xicelm(3),yicelm(3),zicelm(3),
     &                x1,y1,z1, x2,y2,z2,x3,y3,z3,iflag)
 
         if (iflag.ge.0) goto 9999
C
C     *****************************************************************
C     Anything Else
C
      else
         iflag = -1
         write(logmess,9010) ielmtyp
 9010    format('Invalid element in intersect_tri_element, type: ',i10)
         call writloga('default',0,logmess,0,ierror)
      endif
 9999 continue
      return
      end
C
C
      subroutine intersect_quad_element(ielmtyp,
     &                                  xicelm,yicelm,zicelm,
     &                                  x1,y1,z1, x2,y2,z2,
     &                                  x3,y3,z3, x4,y4,z4,
     &                                  iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find out whether the
C        element fed to this routine intersects with the quad defined
C        by (x1,y1,z1), (x2,y2,z2), (x3,y3,z3), and (x4,y4,z4).
C
C     ****************************************************************
C
C     NOTES -
C        Currently only for hexes, tets, and quads.
C
C     INPUT ARGUMENTS -
C        ielmtyp                     - THE ELEMENT TYPE.
C        xicelm(),yicelm(),zicelm()  - THE COORDINATES OF THE TET.
C        x1,y1,z1                    - THE 1st COORDINATES OF THE QUAD.
C        x2,y2,z2                    - THE 2nd COORDINATES OF THE QUAD.
C        x3,y3,z3                    - THE 3rd COORDINATES OF THE QUAD.
C        x4,y4,z4                    - THE 4th COORDINATES OF THE QUAD.
C
C     OUTPUT ARGUMENTS -
C        iflag  - Returns -1 if the quad does not intersect the
C                 element, O if it does, or a positive number
C                 corresponding to the face it is in.
C
C######################################################################
C
C     Standard Declarations for this module.
C
C######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicelm(*), yicelm(*), zicelm(*)
      real*8 xquad(4), yquad(4), zquad(4)
      real*8 xmidpt1, ymidpt1, zmidpt1, x3,y3,z3, x4,y4,z4,
     *   x1,y1,z1, x2,y2,z2, local_epsilon
      real*8 xmidpt2, ymidpt2, zmidpt2
C
C     set up a vertex array for the hex faces
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
      integer hexvertex(6,4)
C
C
      integer iflag,ielmtyp,i,j,k,ierror
      parameter(local_epsilon=1.0d-10)
C
C######################################################################
C
C     Initialize a few variables...
C
C######################################################################
C
      xquad(1)=x1
      xquad(2)=x2
      xquad(3)=x3
      xquad(4)=x4
      yquad(1)=y1
      yquad(2)=y2
      yquad(3)=y3
      yquad(4)=y4
      zquad(1)=z1
      zquad(2)=z2
      zquad(3)=z3
      zquad(4)=z4
      xmidpt1=(x1+x2+x3+x4)/4
      ymidpt1=(y1+y2+y3+y4)/4
      zmidpt1=(z1+z2+z3+z4)/4
C
C######################################################################
C
C     Start finding out what is what
C
C######################################################################
C
C
C     *****************************************************************
C     Hexes (Not the voodoo kind)
C
      if(ielmtyp.eq.ifelmhex) then
         do i=1,4
            call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                          xquad(i),yquad(i),zquad(i),iflag)
         if(iflag.ge.0) return
         enddo
C
C     Ok... Now we know that the actual points are _NOT_ inside
C     the Hex, figure out whether or not they intersect any of the
C     sides.
C     The sides are based on the following six vertex quads:
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
         data (hexvertex(1,j), j=1,4)/1,2,3,4/
         data (hexvertex(2,j), j=1,4)/5,6,7,8/
         data (hexvertex(3,j), j=1,4)/5,6,2,1/
         data (hexvertex(4,j), j=1,4)/6,7,3,2/
         data (hexvertex(5,j), j=1,4)/7,8,4,3/
         data (hexvertex(6,j), j=1,4)/8,5,1,4/
C     Call the requisite additional functions
C
         do i=1,6
C     Find the midpoint of each hex face.
            xmidpt2 = 0.0
            ymidpt2 = 0.0
            zmidpt2 = 0.0
            do j = 1,4
               xmidpt2 = xmidpt2 + xicelm(hexvertex(i,j))/4
               ymidpt2 = ymidpt2 + yicelm(hexvertex(i,j))/4
               zmidpt2 = zmidpt2 + zicelm(hexvertex(i,j))/4
            enddo
C     Call the interesection routines on the proper vertices and
C     the midpoint of both the hex and the quad.
C     [xyz]midpt1 is the midpoint of the quad
C     [xyz]midpt2 is the midpoint of the hex face
C
C           Cycle through the hex face vertices
            do j = 0,3
C              Cycle through the quad vertices
               do k = 0,3
                  iflag = 0
                  call tri_tri(xicelm(hexvertex(i,mod(j,4)+1)),
     &                         yicelm(hexvertex(i,mod(j,4)+1)),
     &                         zicelm(hexvertex(i,mod(j,4)+1)),
     &                         xicelm(hexvertex(i,mod(j+1,4)+1)),
     &                         yicelm(hexvertex(i,mod(j+1,4)+1)),
     &                         zicelm(hexvertex(i,mod(j+1,4)+1)),
     &                         xmidpt2, ymidpt2, zmidpt2,
     &                         xquad(mod(k,4)+1),
     &                         yquad(mod(k,4)+1),
     &                         zquad(mod(k,4)+1),
     &                         xquad(mod(k+1,4)+1),
     &                         yquad(mod(k+1,4)+1),
     &                         zquad(mod(k+1,4)+1),
     &                         xmidpt1,ymidpt1,zmidpt1,
     &                         iflag)
                  if(iflag.ge.0) goto 9999
               enddo
            enddo
         enddo
C        Sorry, you lose.
         goto 9999
 
C     *****************************************************************
C     Tets
C
      elseif(ielmtyp.eq.ifelmtet) then
         do i=1,4
            call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                          xquad(i),yquad(i),zquad(i),iflag)
            if(iflag.ge.0) goto 9999
         enddo
         do i=0,3
            do j=0,3
               iflag = 0
               call tri_tri(xicelm(mod(i,4)+1),yicelm(mod(i,4)+1),
     &                      zicelm(mod(i,4)+1),
     &                      xicelm(mod(i+1,4)+1),yicelm(mod(i+1,4)+1),
     &                      zicelm(mod(i+1,4)+1),
     &                      xicelm(mod(i+2,4)+1),yicelm(mod(i+2,4)+1),
     &                      zicelm(mod(i+2,4)+1),
     &                      xquad(mod(j,4)+1),yquad(mod(j,4)+1),
     &                      zquad(mod(j,4)+1),
     &                      xquad(mod(j+1,4)+1),yquad(mod(j+1,4)+1),
     &                      zquad(mod(j+1,4)+1),
     &                      xmidpt1,ymidpt1,zmidpt1,iflag)
               if(iflag.ge.0) goto 9999
            enddo
         enddo
C
C     *****************************************************************
C     Quads
C
      elseif(ielmtyp.eq.ifelmqud) then
         xmidpt2 = 0.0
         ymidpt2 = 0.0
         zmidpt2 = 0.0
         do i = 1,4
            xmidpt2 = xmidpt2 + xicelm(i)/4
            ymidpt2 = ymidpt2 + yicelm(i)/4
            zmidpt2 = zmidpt2 + zicelm(i)/4
         enddo
         do i = 0,3
            do j = 0,3
               call tri_tri(xicelm(mod(i,4)+1),yicelm(mod(i,4)+1),
     &                      zicelm(mod(i,4)+1),
     &                      xicelm(mod(i+1,4)+1),yicelm(mod(i+1,4)+1),
     &                      zicelm(mod(i+1,4)+1),
     &                      xmidpt2, ymidpt2, zmidpt2,
     &                      xquad(mod(j,4)+1),yquad(mod(j,4)+1),
     &                      zquad(mod(j,4)+1),
     &                      xquad(mod(j+1,4)+1),yquad(mod(j+1,4)+1),
     &                      zquad(mod(j+1,4)+1),
     &                      xmidpt1,ymidpt1,zmidpt1,iflag)
 
               if(iflag.ge.0) goto 9999
            enddo
         enddo
C
C     *****************************************************************
C     Anything Else
C
      else
         iflag = -1
         write(logmess,9010) ielmtyp
 9010    format('Invalid element in intersect_quad_element, type: ',
     &          i10)
         call writloga('default',0,logmess,0,ierror)
      endif
 9999 continue
      return
      end
 
      subroutine intersect_tet_element(ielmtyp,
     &                         xicelm,yicelm,zicelm,
     &                         xtet,ytet,ztet,
     &                         iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find out whether the
C        element fed to this routine intersects with the tet defined
C        by the xtet(i),ytet(i),ztet(i).
C
C     ****************************************************************
C
C     NOTES -
C        Currently only for hexes and tets
C
C     INPUT ARGUMENTS -
C        ielmtyp                     - THE ELEMENT TYPE.
C        xicelm(),yicelm(),zicelm()  - THE COORDINATES OF THE element.
C        xhex(),yhex(),zhex()        - THE COORDINATES OF THE TET.
C
C     OUTPUT ARGUMENTS -
C        iflag  - Returns -1 if the tet does not intersect the
C                 element, O if it does
C
C######################################################################
C
C     Standard Declarations for this module.
C
C######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicelm(*), yicelm(*), zicelm(*)
      real*8 xtet(*), ytet(*), ztet(*)
C
      integer iflag, initialtetvertex, i, ielmtyp, ierror
C
C
C
C######################################################################
C
C     Start finding out what is what
C
C######################################################################
C
C
C     *****************************************************************
C     Hexes (Not the voodoo kind)
C
      if(ielmtyp.eq.ifelmhex) then
C        Start the show by finding out if any of the points of the hex
C        are in the tet or vice versa.
C        Is the tet inside the hex
         do i=1,4
            call inside_element(ifelmhex, xicelm,yicelm,zicelm,
     &                          xtet(i),ytet(i),ztet(i),iflag)
            if(iflag.ge.0) return
         enddo
C        Is the hex inside the tet?
         do i=1,8
            call inside_element(ifelmtet,xtet,ytet,ztet,
     &                          xicelm(i),yicelm(i),zicelm(i),iflag)
            if(iflag.ge.0) return
         enddo
C
C        Ok, we now know that these two volumes are oddly placed so
C        they intersect, but don't have any points inside one another,
C        this means that their sides must intersect. Set up the loop
C        loops to do this.
         do initialtetvertex=0,3
            call intersect_tri_element(ifelmhex,xicelm,yicelm,zicelm,
     &           xtet(mod(initialtetvertex,4)+1),
     &           ytet(mod(initialtetvertex,4)+1),
     &           ztet(mod(initialtetvertex,4)+1),
     &           xtet(mod(initialtetvertex+1,4)+1),
     &           ytet(mod(initialtetvertex+1,4)+1),
     &           ztet(mod(initialtetvertex+1,4)+1),
     &           xtet(mod(initialtetvertex+2,4)+1),
     &           ytet(mod(initialtetvertex+2,4)+1),
     &           ztet(mod(initialtetvertex+2,4)+1),
     &           iflag)
            if(iflag.ge.0) return
         enddo
C     *****************************************************************
C     Tets
C
      elseif(ielmtyp.eq.ifelmtet) then
C        Start the show by finding out if any of the points of either
C        tet are in the other one.
C        Is the parent tet inside the other tet?
         do i=1,4
            call inside_element(ifelmtet,xicelm,yicelm,zicelm,
     &                          xtet(i),ytet(i),ztet(i),iflag)
            if(iflag.ge.0) return
         enddo
C
C        Is tet #2 inside the parent tet?
         do i=1,4
            call inside_element(ifelmtet,xtet,ytet,ztet,
     &                          xicelm(i),yicelm(i),zicelm(i),iflag)
            if(iflag.ge.0) return
         enddo
C
C        Ok, we now know that these two volumes are oddly placed so
C        they intersect, but don't have any points inside one another,
C        this means that their sides must intersect. Set up the loop
C        loops to do this.
         do initialtetvertex=0,3
            call intersect_tri_element(ifelmtet,xicelm,yicelm,zicelm,
     &           xtet(mod(initialtetvertex,4)+1),
     &           ytet(mod(initialtetvertex,4)+1),
     &           ztet(mod(initialtetvertex,4)+1),
     &           xtet(mod(initialtetvertex+1,4)+1),
     &           ytet(mod(initialtetvertex+1,4)+1),
     &           ztet(mod(initialtetvertex+1,4)+1),
     &           xtet(mod(initialtetvertex+2,4)+1),
     &           ytet(mod(initialtetvertex+2,4)+1),
     &           ztet(mod(initialtetvertex+2,4)+1),
     &           iflag)
            if(iflag.ge.0) return
         enddo
C
C     *****************************************************************
C     Anything Else
C
      else
         iflag = -1
         write(logmess,9010) ielmtyp
 9010    format('Invalid element in intersect_tet_element, type: ',
     &          i10)
         call writloga('default',0,logmess,0,ierror)
      endif
 9999 continue
      return
      end
 
 
 
 
 
      subroutine intersect_hex_element(ielmtyp,
     &                         xicelm,yicelm,zicelm,
     &                         xhex,yhex,zhex,
     &                         iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to find out whether the
C        element fed to this routine intersects with the hex defined
C        by the xhex(i),yhex(i),zhex(i).
C
C     ****************************************************************
C
C     NOTES -
C        Currently only for hexes.
C
C     INPUT ARGUMENTS -
C        ielmtyp                     - THE ELEMENT TYPE.
C        xicelm(),yicelm(),zicelm()  - THE COORDINATES OF THE TET.
C        xhex(),yhex(),zhex()        - THE COORDINATES OF THE HEX.
C
C     OUTPUT ARGUMENTS -
C        iflag  - Returns -1 if the hex does not intersect the
C                 element, O if it does
C
C######################################################################
C
C     Standard Declarations for this module.
C
C######################################################################
C
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
      real*8 xicelm(*), yicelm(*), zicelm(*)
      real*8 xhex(*), yhex(*), zhex(*)
C
C     set up a vertex array for the hex faces
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
      integer hexvertex(6,4)
      integer iflag, hexface, i, ielmtyp, ierror
C
C
C     Set up the data for the hex vertex array
C     The sides are based on the following six vertex quads:
C     <1,2,3,4>,<5,6,7,8>,<5,6,2,1>,<6,7,3,2>,<7,8,4,3>,<8,5,1,4>
         data (hexvertex(1,i), i=1,4)/1,2,3,4/
         data (hexvertex(2,i), i=1,4)/5,6,7,8/
         data (hexvertex(3,i), i=1,4)/5,6,2,1/
         data (hexvertex(4,i), i=1,4)/6,7,3,2/
         data (hexvertex(5,i), i=1,4)/7,8,4,3/
         data (hexvertex(6,i), i=1,4)/8,5,1,4/
C
C
C
C######################################################################
C
C     Start finding out what is what
C
C######################################################################
C
C
C     *****************************************************************
C     Hexes (Not the voodoo kind)
C
      if(ielmtyp.eq.ifelmhex) then
C        Start the show by finding out if any of the points of either
C        hex are inside the other.
C        hex #1
         do i=1,8
            call inside_element(ifelmhex,xicelm,yicelm,zicelm,
     &                          xhex(i),yhex(i),zhex(i),iflag)
         if(iflag.ge.0) return
         enddo
C        hex #2
         do i=1,8
            call inside_element(ifelmhex,xhex,yhex,zhex,
     &                          xicelm(i),yicelm(i),zicelm(i),iflag)
         if(iflag.ge.0) return
         enddo
C
C     Ok... Now we know that the actual points are _NOT_ inside
C     the Hex, figure out whether or not they intersect any of the
C     sides.
C
C     We will use the "parent" hex as the outside loop
C     Parent hexes sides
         do hexface=1,6
C           For each of the faces (quads), we need to check whether the
C           hex intersects that quad, if it does, we are done.
            call intersect_quad_element(ifelmhex,xicelm,yicelm,zicelm,
     &           xhex(hexvertex(hexface,1)),
     &           yhex(hexvertex(hexface,1)),
     &           zhex(hexvertex(hexface,1)),
     &           xhex(hexvertex(hexface,2)),
     &           yhex(hexvertex(hexface,2)),
     &           zhex(hexvertex(hexface,2)),
     &           xhex(hexvertex(hexface,3)),
     &           yhex(hexvertex(hexface,3)),
     &           zhex(hexvertex(hexface,3)),
     &           xhex(hexvertex(hexface,4)),
     &           yhex(hexvertex(hexface,4)),
     &           zhex(hexvertex(hexface,4)),
     &           iflag)
            if(iflag.ge.0) return
         enddo
C
C     *****************************************************************
C     Anything Else
C
      else
         iflag = -1
         write(logmess,9010) ielmtyp
 9010    format('Invalid element in intersect_hex_element, type: ',
     &          i10)
         call writloga('default',0,logmess,0,ierror)
      endif
 9999 continue
      return
      end
 
 
      subroutine adjust_element_pair(
     &    xic1,yic1,zic1,    xic2,yic2,zic2,
     &    xnew1,ynew1,znew1, xnew2,ynew2,znew2,
     &    ielmtyp1,ielmtyp2, iflag)

C
C######################################################################
C
C     PURPOSE -
C
C        The purpose of this subroutine is to create a copy of element
C        coordinates and adjust with translate and or scale as 
C        needed to keep the problem adjusted to epsilon_local
C        which is used in the inside and intersect routines.
C        It is a constant parameter set to 1.0e-10
C
C     ****************************************************************
C
C     NOTES -
C
C     INPUT ARGUMENTS -
C     xic1(),yic1(),zic1()  - COORDINATES OF 1ST ELEMENT 
C     xic2(),yic2(),zic2()  - COORDINATES OF 2nd ELEMENT 
C     xnew1(),ynew1(),znew1()  - ADJUSTED COORDINATES OF 1ST ELEMENT 
C     xnew2(),ynew2(),znew2()  - ADJUSTED COORDINATES OF 2nd ELEMENT 
C     ielmtyp1,ielmtyp2     - ELEMENT TYPES FOR 1ST and 2ND ELEMENT
C     iflag
C
C     OUTPUT ARGUMENTS -
C        iflag  - Returns -1 if numbers may be too small for precision 
C                          1 if numbers may be too big for precision 
C                          O if no error 
C
C######################################################################
C
C     Standard Declarations for this module.
C
C######################################################################

      implicit none
      include 'consts.h'
      include "local_element.h"

C     arguments 
      integer ielmtyp1,ielmtyp2,iflag
      real*8 xic1(8), yic1(8), zic1(8)
      real*8 xic2(8), yic2(8), zic2(8)
      real*8 xnew1(8), ynew1(8), znew1(8)
      real*8 xnew2(8), ynew2(8), znew2(8)

      real*8 xtrans,ytrans,ztrans, 
     *       xdist,ydist,zdist, xcntr,ycntr,zcntr, xfac,
     *       xmin,ymin,zmin,xmax,ymax,zmax, eps_scale
      real*8 BS_ratio, maxdist, releps, local_epsilon

      integer i,ierror,idebug,nwrite,xzero,yzero,zzero

      character*132 logmess

      parameter(local_epsilon=1.0d-10)

C     assign variables
      idebug=iflag
      iflag = 0
      nwrite = 0
      xtrans = 0.
      ytrans = 0.
      ztrans = 0.
      xzero = 0
      yzero = 0
      zzero = 0
      eps_scale = .10d+01
      releps = 0.0 
      maxdist = 0.0 

C     Find x,y,z min of both objects 
      xmin = xic1(1)
      ymin = yic1(1) 
      zmin = zic1(1)
      xmax = xic1(1)
      ymax = yic1(1)
      zmax = zic1(1)     
      do i = 1,nelmnen(ielmtyp1)
        if (xic1(i) .lt. xmin)  xmin = xic1(i)
        if (yic1(i) .lt. ymin)  ymin = yic1(i)
        if (zic1(i) .lt. zmin)  zmin = zic1(i)
      enddo                 
      do i = 1,nelmnen(ielmtyp2)
        if (xic2(i) .lt. xmin)  xmin = xic2(i)
        if (yic2(i) .lt. ymin)  ymin = yic2(i)
        if (zic2(i) .lt. zmin)  zmin = zic2(i)
      enddo

      do i = 1,nelmnen(ielmtyp1)
        if (xic1(i) .gt. xmax)  xmax = xic1(i)
        if (yic1(i) .gt. ymax)  ymax = yic1(i)
        if (zic1(i) .gt. zmax)  zmax = zic1(i)
      enddo
      do i = 1,nelmnen(ielmtyp2)
        if (xic2(i) .gt. xmax)  xmax = xic2(i)
        if (yic2(i) .gt. ymax)  ymax = yic2(i)
        if (zic2(i) .gt. zmax)  zmax = zic2(i)
      enddo

C     Find maxdist - max distance bounding the two objects
C     This will be used to scale up small object
C     And allow checks against coordinate values that
C     may be too large or small for arithmetic precision
C
      xdist = max(abs(xmax-xmin),maxdist)
      ydist = max(abs(ymax-ymin),maxdist)
      zdist = max(abs(zmax-zmin),maxdist)
      maxdist = max(xdist,ydist)
      maxdist = max(zdist,maxdist)

C     Find distance vector
C     to compare size against distance from zero


C ----TRANSLATION WORK
C     Translate and copy into new coordinates
C     move coordinates so zero is at centroid of first object
C     first object is usually an element to find point inside
C     alternative is to move by xyx mins, but this does not
C     help enough for elements with extreme lengths 
C     this also avoids subtracting numbers that are nearly equal 
C     Original translation using xyz mins:
C       xtrans = xmin
C       ytrans = ymin
C       ztrans = zmin
C       avoid subtracting near zero from near zero
c       if (abs(xmin-xtrans) .le. (abs(xmin)*local_epsilon)) xzero=1
c       if (abs(ymin-ytrans) .le. (abs(ymin)*local_epsilon)) yzero=1
c       if (abs(zmin-ztrans) .le. (abs(zmin)*local_epsilon)) zzero=1

      xcntr = zero
      ycntr = zero
      zcntr = zero
      do i = 1,nelmnen(ielmtyp1)
        xcntr = xcntr+xic1(i)
        ycntr = ycntr+yic1(i)
        zcntr = zcntr+zic1(i)
      enddo
      xfac= one /dble(nelmnen(ielmtyp1))
      xtrans=xfac*xcntr
      ytrans=xfac*ycntr
      ztrans=xfac*zcntr

C     Compare distance against size 
C     to see if translation is worth doing
      BS_ratio = 1.

      if (BS_ratio .lt. 1.0d+1) then
        do i = 1,nelmnen(ielmtyp1)
          xnew1(i) = xic1(i)-xtrans 
          ynew1(i) = yic1(i)-ytrans 
          znew1(i) = zic1(i)-ztrans 
        enddo
        do i = 1,nelmnen(ielmtyp2)
          xnew2(i) = xic2(i)-xtrans 
          ynew2(i) = yic2(i)-ytrans 
          znew2(i) = zic2(i)-ztrans 
        enddo
      else
C       print*,"No translate, BS ratio= ",BS_ratio
        do i = 1,nelmnen(ielmtyp1)
          xnew1(i) = xic1(i)
          ynew1(i) = yic1(i)
          znew1(i) = zic1(i)
        enddo
        do i = 1,nelmnen(ielmtyp2)
          xnew2(i) = xic2(i)
          ynew2(i) = yic2(i)
          znew2(i) = zic2(i)
        enddo
      endif


C ----SCALE WORK (if needed for very small numbers)
C     elpsilonr is machine epsilon
C     local_epsilon is used locally in lower routines, use same here
C     eps_scale is the multiple of 10 to scale small numbers

      releps = maxdist
      if (releps.eq.0) then
         releps = local_epsilon 
       else
         releps = local_epsilon*releps
      endif

      do while (releps .lt. local_epsilon*100.d0)
          eps_scale = eps_scale * 1.0d+1 
          releps = releps * eps_scale
      enddo

C     Scale and copy into new coordinates
C     scale first to avoid arithmetic errors that can occur 
C     when working with small numbers less than 1.0e-6
      if (eps_scale .gt. .10d+1) then
C       print*,"Scale, releps= ",releps
        do i = 1,nelmnen(ielmtyp1)
          xnew1(i) = xnew1(i) * eps_scale
          ynew1(i) = ynew1(i) * eps_scale
          znew1(i) = znew1(i) * eps_scale
        enddo
        do i = 1,nelmnen(ielmtyp2)
          xnew2(i) = xnew2(i) * eps_scale
          ynew2(i) = ynew2(i) * eps_scale
          znew2(i) = znew2(i) * eps_scale
        enddo
      endif

C     bounds and error checking 
C     return 1 if possibly too large
C     return -1 if possibly too small

      iflag = 0
      if (maxdist .lt. epsilonr*100.d0 ) then
        iflag = -1
C       print*,"maxdist:  ", maxdist," smaller than: ",epsilonr*100.d0
      else if (maxdist .gt. .10d0/epsilonr ) then
        iflag = 1
C       print*,"maxdist:  ", maxdist," larger than: ",.10d0/epsilonr
      endif
     
 9999 continue
      return
      end

