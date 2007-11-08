*dk kdtree0
      subroutine kdtree0(xic,yic,zic,mtri,linkt,sbox,ierr)
c
c #####################################################################
c
c     purpose -
c
c     KDTREE0 takes the set of MTRI points and produces a k-D tree
c     that is stored in the array LINKT.  Leaf nodes in LINKT each
c     contain exactly one point index.
c     KDTREE0 also produces an array SBOX which
c     gives 'safety boxes'.  For each node in the k-D tree,
c     there is a corresponding safety box which is just big enough
c     to contain all the points in the subtree associated with
c     the node.
c
c     input arguments -
c
c         xic,yic,zic  - arrays of spatial coordinates of the
c                        points.
c         mtri -         total number of points.
c
c     output arguments -
c         linkt -        k-D tree links and leaf links to points.
c                        This array needs to have length 2*mtri.
c         sbox -         ``safety box system'' for k-D tree.
c                        This array needs to have length 12*mtri.
c         ierr -         error return.
c
c     change history -
c
C         $Log:   /pvcs.config/t3d/src/kdtree0.f_a  $
CPVCS    
CPVCS       Rev 1.3   Tue Sep 22 13:45:32 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.2   Fri Jun 20 15:56:18 1997   dcg
CPVCS    remove open statement
CPVCS    add some temporary memory calls
 
      implicit none
      include 'consts.h'
 
      integer mtri,linkt(2*mtri),ierr, length
      real*8 xic(mtri),yic(mtri),zic(mtri),sbox(2,3,2*mtri)
 
      pointer(ipitri,itri)
 
      integer itri(mtri)
 
      integer icscode,i,node,nextt,itop,imn,imx,icut,imd,k1,ierrw,
     &   icrstack(100),imin(100),imax(100),ict(100)
      pointer(ipimin,imin)
      pointer(ipimax,imax)
      pointer(ipict,ict)
      pointer(ipicrstack,icrstack)
 
      real*8 dimx,dimy,dimz
 
      character*132 logmess
      character*32 isubname
      real*8 alargenumber
      data alargenumber/1.d99/
 
      isubname='kdtree0'
      ierr=0
 
c.... If the number of points is not positive, give an error.
 
      if (mtri.le.0) then
         write(logmess,'(a,i6)') 'Error: mtri=',mtri
         call writloga('default',0,logmess,0,ierrw)
         call x3d_error(isubname,' ')
         ierr=-1
         goto 9999
      endif
 
c....Obtain allocation for array.
 
      call mmgetblk('itri',isubname,ipitri,mtri,1,icscode)
      length=100
      call mmgetblk('imin',isubname,ipimin,length,1,icscode)
      call mmgetblk('imax',isubname,ipimax,length,1,icscode)
      call mmgetblk('ict',isubname,ipict,length,1,icscode)
      call mmgetblk('icrstack',isubname,ipicrstack,length,1,icscode)
      call mmverify()
 
c.... Let the bounding box associated with the root node
c.... [SBOX(*,*,1)] exactly contain all points.
 
      sbox(1,1,1)=alargenumber
      sbox(2,1,1)=-alargenumber
      sbox(1,2,1)=alargenumber
      sbox(2,2,1)=-alargenumber
      sbox(1,3,1)=alargenumber
      sbox(2,3,1)=-alargenumber
      do i=1,mtri
         sbox(1,1,1)=min(sbox(1,1,1),xic(i))
         sbox(2,1,1)=max(sbox(2,1,1),xic(i))
         sbox(1,2,1)=min(sbox(1,2,1),yic(i))
         sbox(2,2,1)=max(sbox(2,2,1),yic(i))
         sbox(1,3,1)=min(sbox(1,3,1),zic(i))
         sbox(2,3,1)=max(sbox(2,3,1),zic(i))
      enddo
 
c.... If there is only one point, the root node is a leaf.
c.... (Our convention is to set the link corresponding to a leaf
c.... equal to the negative of the unique point contained in
c.... that leaf.)  If the root is a leaf, our work is done.
 
      if (mtri.eq.1) then
         linkt(1)=-1
         goto 9999
      endif
 
c.... DIMX, DIMY, DIMZ are equal to the x, y, and z dimensions
c.... of the bounding box corresponding to the current node
c.... under consideration.
      dimx=sbox(2,1,1)-sbox(1,1,1)
      dimy=sbox(2,2,1)-sbox(1,2,1)
      dimz=sbox(2,3,1)-sbox(1,3,1)
 
c.... ITRI will contain a permutation of the integers
c.... {1,...,MTRI}.  This permutation will be altered as we
c.... create our balanced binary tree.  NEXTT contains the
c.... address of the next available node to be filled.
 
      nextt=2
      do i=1,mtri
         itri(i)=i
      enddo
 
c.... Use a stack to create the tree.  Put the root node
c.... ``1'' on the top of the stack (ICRSTACK).  The array
c.... subset of ITRI (i.e., subset of points associated
c.... with this node) is recorded using the arrays
c.... IMIN and IMAX.
 
      itop=1
      icrstack(itop)=1
      imin(itop)=1
      imax(itop)=mtri
 
c.... Finally, we record in ICT the appropriate ``cutting
c.... direction'' for bisecting the set of points
c.... corresponding to this node.  This direction is either
c.... the x, y, or z direction, depending on which dimension
c.... of the bounding box is largest.
 
      if (dimx.ge.max(dimy,dimz)) then
         ict(itop)=1
      elseif (dimy.ge.dimz) then
         ict(itop)=2
      else
         ict(itop)=3
      endif
 
c.... Pop nodes off stack, create children nodes and put them
c.... on stack.  Continue until k-D tree has been created.
 
      do while (itop.gt.0)
 
c.... Pop top node off stack.
 
         node=icrstack(itop)
 
c.... Make this node point to next available node location (NEXTT).
c.... This link represents the location of the FIRST CHILD
c.... of the node.  The adjacent location (NEXTT+1)
c.... is implicitly taken to be the location of the SECOND
c.... child of the node.
 
         linkt(node)=nextt
         imn=imin(itop)
         imx=imax(itop)
         icut=ict(itop)
         itop=itop-1
 
c.... Partition point subset associated with this node.
c.... Using the appropriate cutting direction, use SELECT to
c.... reorder ITRI so that the point with median
c.... coordinate is itri(imd), while the
c.... points {itri(i),i<imd} have SMALLER (or equal)
c.... coordinates, and the points with
c.... {itri(i),i>imd} have GREATER coordinates.
 
         imd=(imn+imx)/2
         if (icut.eq.1) then
            call select(imd-imn+1,imx-imn+1,xic,itri(imn))
         elseif (icut.eq.2) then
            call select(imd-imn+1,imx-imn+1,yic,itri(imn))
         else
            call select(imd-imn+1,imx-imn+1,zic,itri(imn))
         endif
 
c.... If the first child's subset of points is a singleton,
c.... the child is a leaf.  Set the child's link to point to the
c.... negative of the point number.  Set the child's bounding
c.... box to be equal to a box with zero volume located at
c.... the coordinates of the point.
 
         if (imn.eq.imd) then
            linkt(nextt)=-itri(imn)
 
            do k1=1,2
               sbox(k1,1,nextt)=xic(itri(imn))
               sbox(k1,2,nextt)=yic(itri(imn))
               sbox(k1,3,nextt)=zic(itri(imn))
            enddo
            nextt=nextt+1
         else
 
c.... In this case, the subset of points corresponding to the
c.... first child is more than one point, and the child is
c.... not a leaf.  Compute the bounding box of this child to
c.... be the smallest box containing all the associated points.
 
            do k1=1,2
               sbox(k1,1,nextt)=xic(itri(imn))
               sbox(k1,2,nextt)=yic(itri(imn))
               sbox(k1,3,nextt)=zic(itri(imn))
            enddo
            do i=imn+1,imd
               sbox(1,1,nextt)=min(sbox(1,1,nextt),
     &            xic(itri(i)))
               sbox(2,1,nextt)=max(sbox(2,1,nextt),
     &            xic(itri(i)))
               sbox(1,2,nextt)=min(sbox(1,2,nextt),
     &            yic(itri(i)))
               sbox(2,2,nextt)=max(sbox(2,2,nextt),
     &            yic(itri(i)))
               sbox(1,3,nextt)=min(sbox(1,3,nextt),
     &            zic(itri(i)))
               sbox(2,3,nextt)=max(sbox(2,3,nextt),
     &            zic(itri(i)))
            enddo
 
c.... Put the first child onto the stack, noting the
c.... associated point subset in IMIN and IMAX, and
c.... putting the appropriate cutting direction in ICT.
 
            dimx=sbox(2,1,nextt)-sbox(1,1,nextt)
            dimy=sbox(2,2,nextt)-sbox(1,2,nextt)
            dimz=sbox(2,3,nextt)-sbox(1,3,nextt)
 
            itop=itop+1
            icrstack(itop)=nextt
            imin(itop)=imn
            imax(itop)=imd
            if (dimx.ge.max(dimy,dimz)) then
               ict(itop)=1
            elseif (dimy.ge.dimz) then
               ict(itop)=2
            else
               ict(itop)=3
            endif
            nextt=nextt+1
         endif
 
c.... If the first child's subset of points is a singleton,
c.... the child is a leaf.  Set the child's link to point to the
c.... negative of the point number.  Set the child's bounding
c.... box to be equal to a box with zero volume located at
c.... the coordinates of the point.
 
         if (imd+1.eq.imx) then
            linkt(nextt)=-itri(imx)
            do k1=1,2
               sbox(k1,1,nextt)=xic(itri(imx))
               sbox(k1,2,nextt)=yic(itri(imx))
               sbox(k1,3,nextt)=zic(itri(imx))
            enddo
            nextt=nextt+1
         else
 
c.... In this case, the subset of points corresponding to the
c.... second child is more than one point, and the child is
c.... not a leaf.  Compute the bounding box of this child to
c.... be the smallest box containing all the associated points.
 
            do k1=1,2
               sbox(k1,1,nextt)=xic(itri(imd+1))
               sbox(k1,2,nextt)=yic(itri(imd+1))
               sbox(k1,3,nextt)=zic(itri(imd+1))
            enddo
            do i=imd+2,imx
               sbox(1,1,nextt)=min(sbox(1,1,nextt),
     &            xic(itri(i)))
               sbox(2,1,nextt)=max(sbox(2,1,nextt),
     &            xic(itri(i)))
               sbox(1,2,nextt)=min(sbox(1,2,nextt),
     &            yic(itri(i)))
               sbox(2,2,nextt)=max(sbox(2,2,nextt),
     &            yic(itri(i)))
               sbox(1,3,nextt)=min(sbox(1,3,nextt),
     &            zic(itri(i)))
               sbox(2,3,nextt)=max(sbox(2,3,nextt),
     &            zic(itri(i)))
            enddo
 
 
c.... Put the second child onto the stack, noting the
c.... associated point subset in IMIN and IMAX, and
c.... putting the appropriate cutting direction in ICT.
 
            dimx=sbox(2,1,nextt)-sbox(1,1,nextt)
            dimy=sbox(2,2,nextt)-sbox(1,2,nextt)
            dimz=sbox(2,3,nextt)-sbox(1,3,nextt)
 
            itop=itop+1
            icrstack(itop)=nextt
            imin(itop)=imd+1
            imax(itop)=imx
            if (dimx.ge.max(dimy,dimz)) then
               ict(itop)=1
            elseif (dimy.ge.dimz) then
               ict(itop)=2
            else
               ict(itop)=3
            endif
            nextt=nextt+1
         endif
      enddo
 
 9999 continue
 
       call mmrelprt(isubname,icscode)
 
      return
      end
