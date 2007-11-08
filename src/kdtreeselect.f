*DK kDtreeselect
      subroutine kDtreeselect(ielmtyp, xicelm,yicelm,zicelm, linktree,
     &                        safebox, numfound, itfound, iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The Purpose of this subroutine is to take a k-D-R+ tree found
C        in linktree and provide an entry point into the routines that
C        find candidates based on the type of element (currently only
C        two)
C
C     NOTES -
C
C
C     INPUT ARGUMENTS -
C        ielmtyp                    The type of intersecting element,
C                                   used to decide what routine to use.
C        xicelm(),yicelm(),zicelm() Coordinates of the intersecting
C                                   element.
C        linktree()                 k-D-R+ tree structure
C        safebox(2,3,*)             bounding boxes corresponding to
C                                   elements in the k-D-R+ tree
C
C     OUTPUT ARGUMENTS -
C        numfound                   number of candidate elements found
C        itfound                    array of candidate elements
C        iflag                      error flag, 0 if successful.
C
C
C     $Log: kdtreeselect.f,v $
C     Revision 2.00  2007/11/05 19:45:59  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   28 Mar 2000 14:09:20   dcg
CPVCS    remove include 'machine.h'
CPVCS
CPVCS       Rev 1.2   22 Mar 2000 08:39:04   dcg
CPVCS    use local_epsilon in place of epsilon
CPVCS
CPVCS       Rev 1.1   08 Feb 2000 08:38:24   dcg
CPVCS    remove comdict
CPVCS
CPVCS       Rev 1.0   Wed Aug 04 10:00:34 1999   bap
CPVCS    Initial revision.
C
C
C######################################################################
C
C     Variable Declarations
C
      implicit none
      include "local_element.h"
C
C     Arguments
      integer ielmtyp
      real*8 xicelm(1000000), yicelm(1000000), zicelm(1000000)
      integer linktree(1000000)
      real*8 safebox(2,3,1000000)
      integer numfound
      integer itfound(1000000)
      integer iflag
C
C     Indices
      integer i
C
C     Bounds
      real*8 xboundhi, yboundhi, zboundhi
      real*8 xboundlo, yboundlo, zboundlo
C
C     Other stuff
      real*8 local_epsilon
      real*8 releps
 
C
C######################################################################
C
C     Initialize the variables
C
      local_epsilon = 1.0e-10
C
C######################################################################
C
      if (ielmtyp.eq.ifelmlin) then
         releps = sqrt((xicelm(2)-xicelm(1))**2+
     &                 (yicelm(2)-yicelm(1))**2+
     &                 (zicelm(2)-zicelm(1))**2)*local_epsilon
         call lineseg_inter(xicelm(1),yicelm(1),zicelm(1),
     &                      xicelm(2),yicelm(2),zicelm(2),
     &                      linktree, safebox, releps,
     &                      numfound, itfound, iflag)
      else
         xboundhi = xicelm(1)
         xboundlo = xicelm(1)
         yboundhi = yicelm(1)
         yboundlo = yicelm(1)
         zboundhi = zicelm(1)
         zboundlo = zicelm(1)
         do i = 2,nelmnen(ielmtyp)
            xboundhi = max(xicelm(i),xboundhi)
            xboundlo = min(xicelm(i),xboundlo)
            yboundhi = max(yicelm(i),yboundhi)
            yboundlo = min(yicelm(i),yboundlo)
            zboundhi = max(zicelm(i),zboundhi)
            zboundlo = min(zicelm(i),zboundlo)
         enddo
         call bbox_inter(xboundlo, yboundlo, zboundlo,
     &                   xboundhi, yboundhi, zboundhi,
     &                   linktree, safebox,
     &                   numfound, itfound, iflag)
      endif
      return
      end
 
*DK bbox_inter
      subroutine bbox_inter(xboundlo, yboundlo, zboundlo,
     &                      xboundhi, yboundhi, zboundhi,
     &                      linktree, safebox,
     &                      numfound, itfound, iflag)
C
C######################################################################
C
C     PURPOSE -
C
C        The Purpose of this subroutine is to take a k-D-R+ tree found
C        in linktree and find candidates based on the bounding boxes
C        provided in safebox and defined by <boundlo> and <boundhi>
C
C     NOTES -
C
C        This code is currently NONFUNCTIONAL
C
C     INPUT ARGUMENTS -
C        xboundlo,yboundlo,zboundlo Lower Bounding Box Coordinates of
C                                   the intersecting element.
C        xboundhi,yboundhi,zboundhi Upper Bounding Box Coordinates of
C                                   the intersecting element.
C        linktree()                 k-D-R+ tree structure
C        safebox(2,3,*)             bounding boxes corresponding to
C                                   elements in the k-D-R+ tree
C
C     OUTPUT ARGUMENTS -
C        numfound                   number of candidate elements found
C        itfound                    array of candidate elements
C        iflag                      error flag, 0 if successful.
C
C######################################################################
C
C     Variable Declarations
C
      implicit none
C
C     Arguments
      real*8 xboundlo,yboundlo,zboundlo
      real*8 xboundhi,yboundhi,zboundhi
      integer linktree(1000000)
      real*8 safebox(2,3,1000000)
      integer numfound
      integer itfound(1000000)
      integer iflag
C
C     Indices
      integer i, j
 
C
C     Stack Variables
      integer stack1(500)
      integer top, node, index
C
C     Useful max and min arrays
      real*8 maxcoord(3)
      real*8 mincoord(3)
C
C     Epsilon factor things
      real*8 releps
C
C######################################################################
C
C     Initialize some important variables...
C
      numfound = 0
      releps = 0.0
C
C     *****************************************************************
C     Put the maxes & mins in to tbe arrays
      maxcoord(1)=xboundhi
      maxcoord(2)=yboundhi
      maxcoord(3)=zboundhi
      mincoord(1)=xboundlo
      mincoord(2)=yboundlo
      mincoord(3)=zboundlo
C
C######################################################################
C
C     Ensure that we have a small, but valid, bounding box. (Find the
C     proper slop factor for the bounding boxes)
C
      do i=1,3
         releps = max(maxcoord(i)-mincoord(i),releps)
      enddo
      if (releps.eq.0) then
         releps = 1.0e-10
      else
         releps = 1.0e-10*releps
      endif
C
C######################################################################
C
C     Now the fun begins...
C
C     *****************************************************************
C     Is the element within the bounding box of the kdtree?
      do i=1,3
         if (mincoord(i).gt.(safebox(2,i,1)+releps)) goto 9999
         if (maxcoord(i).lt.(safebox(1,i,1)-releps)) goto 9999
      enddo
C
C     *****************************************************************
C     Is the root node a leaf?
      if (linktree(1).lt.0) then
         numfound = 1
         itfound(1)= -linktree(1)
         goto 9999
      endif
C
C     *****************************************************************
C     Prep the stack
      top=1
      stack1(top)=1
C
C     *****************************************************************
C     Traverse the k-D tree via the stack
      do while (top.gt.0)
         node=stack1(top)
         top = top-1
         index=linktree(node)
C
C     *****************************************************************
C     See if node's children intersect the bounding box of the element
         do j=0,1
C
C     *****************************************************************
C     If they're leaves, then the best we can do is add them to the
C     candidate list, (There's no way to check the boxes of these
C     nodes without working with the itet arrays)
            if (linktree(index+j).lt.0) then
               numfound=numfound+1
               itfound(numfound) = -linktree(index+j)
C
C     *****************************************************************
C     Otherwise, if they are branch nodes, figure out if they are
C     within the bounding box.
            else
               do i=1,3
                  if (mincoord(i).gt.(safebox(2,i,index+j)+releps))
     &                 goto 10
                  if (maxcoord(i).lt.(safebox(1,i,index+j)-releps))
     &                 goto 10
               enddo
               top = top+1
               stack1(top) = index+j
 10            continue
            endif
         enddo
      enddo
 9999 continue
      return
      end
