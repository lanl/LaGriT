*dk kdtree
      subroutine kdtree(xic,yic,zic,itet,mtri,linkt,sbox,ierr)
c
c #####################################################################
c
c     purpose -
c
c     KDTREE takes the set of MTRI triangles and produces a k-D tree
c     that is stored in the array LINKT.  Leaf nodes in LINKT each
c     contain exactly one triangle index.  Because of the possibility
c     of triangle overlap, KDTREE also produces an array SBOX which
c     gives 'safety boxes'.  For each node in the k-D tree,
c     there is a corresponding safety box which is just big enough
c     to contain all the triangles ``under'' the node.
c     Note: kdtree0 is used by interpolate for nearest node
c
c     input arguments -
c
c         xic,yic,zic  - arrays of spatial coordinates of the
c                        triangle vertices
c         itet -         triangle-node relation for triangles
c         mtri -         total number of triangles.
c
c     output arguments -
c         linkt -        k-D tree links and leaf links to triangles.
c                        This array needs to have length 2*mtri.
c         sbox -         ``safety box system'' for k-D tree.
c                        This array needs to have length 12*mtri.
c         ierr -         error return.
c
c     change history -
c
c     $Log: kdtree.f,v $
c     Revision 2.00  2007/11/05 19:45:59  spchu
c     Import to CVS
c
CPVCS    
CPVCS       Rev 1.3   Wed Sep 30 13:13:06 1998   dcg
CPVCS    fix changes comment $Log: kdtree.f,v $
CPVCS    fix changes comment Revision 2.00  2007/11/05 19:45:59  spchu
CPVCS    fix changes comment Import to CVS
CPVCS    fix changes comment
 
      implicit none
      include 'consts.h'
 
      integer mtri,itet(3,mtri),linkt(2*mtri),ierr
      real*8 xic(1000000),yic(1000000),zic(1000000),sbox(2,3,2*mtri)
 
      pointer(ipxbbc,xbbc)
      pointer(ipybbc,ybbc)
      pointer(ipzbbc,zbbc)
      pointer(ipsboxtri,sboxtri)
      pointer(ipitri,itri)
 
      integer itri(mtri)
      real*8 sboxtri(2,3,mtri),xbbc(mtri),ybbc(mtri),zbbc(mtri)
 
      integer icscode,i,node,nextt,itop,imn,imx,icut,imd,k1,k2,
     &   icrstack(100),imin(100),imax(100),ict(100),ierrw
      integer idebug
 
      real*8 dimx,dimy,dimz
c     variables to save min and max search box for idebug
c     use volume of box lengths for debug reporting
      real*8 maxdimx,maxdimy,maxdimz,mindimx,mindimy,mindimz
      real*8 sizebox, maxbox, minbox, xmult,ymult,zmult
 
      character*132 logmess
      character*32 isubname
      real*8 alargenumber
      data alargenumber/1.d+99/
 
C---------------------------------------------------------------
C BEGIN
      isubname='kdtree'
      ierr=0
      idebug = 0
      if (ierr .gt. 0) then
         idebug = ierr
      endif
      ierr=0

 
c.... If the number of triangles is not positive, give an error.
 
      if (mtri.le.0) then
         write(logmess,'(a,i6)') 'Error: mtri=',mtri
         call writloga('default',0,logmess,0,ierrw)
         call x3d_error(isubname,' ')
         ierr=-1
         goto 9999
      endif
 
c....Obtain allocations for arrays.
 
      call mmgetblk('itri',isubname,ipitri,mtri,1,icscode)
      call mmgetblk('xbbc',isubname,ipxbbc,mtri,2,icscode)
      call mmgetblk('ybbc',isubname,ipybbc,mtri,2,icscode)
      call mmgetblk('zbbc',isubname,ipzbbc,mtri,2,icscode)
      call mmgetblk('sboxtri',isubname,ipsboxtri,6*mtri,2,icscode)
 
c.... Compute the bounding boxes for the individual triangles and
c.... put this information in SBOXTRI.  Let the bounding box
c.... associated with the root node [SBOX(*,*,1)] exactly contain
c.... all the triangle bounding boxes.  Put the centers of the
c.... triangle bounding boxes into XBBC, YBBC, ZBBC.
 
      sbox(1,1,1)=alargenumber
      sbox(2,1,1)=-alargenumber
      sbox(1,2,1)=alargenumber
      sbox(2,2,1)=-alargenumber
      sbox(1,3,1)=alargenumber
      sbox(2,3,1)=-alargenumber
      do i=1,mtri
         sboxtri(1,1,i)=min(xic(itet(1,i)),xic(itet(2,i)),
     &      xic(itet(3,i)))
         sboxtri(1,2,i)=min(yic(itet(1,i)),yic(itet(2,i)),
     &      yic(itet(3,i)))
         sboxtri(1,3,i)=min(zic(itet(1,i)),zic(itet(2,i)),
     &      zic(itet(3,i)))
         sboxtri(2,1,i)=max(xic(itet(1,i)),xic(itet(2,i)),
     &      xic(itet(3,i)))
         sboxtri(2,2,i)=max(yic(itet(1,i)),yic(itet(2,i)),
     &      yic(itet(3,i)))
         sboxtri(2,3,i)=max(zic(itet(1,i)),zic(itet(2,i)),
     &      zic(itet(3,i)))
         xbbc(i)=0.5*(sboxtri(1,1,i)+sboxtri(2,1,i))
         ybbc(i)=0.5*(sboxtri(1,2,i)+sboxtri(2,2,i))
         zbbc(i)=0.5*(sboxtri(1,3,i)+sboxtri(2,3,i))
         sbox(1,1,1)=min(sbox(1,1,1),sboxtri(1,1,i))
         sbox(2,1,1)=max(sbox(2,1,1),sboxtri(2,1,i))
         sbox(1,2,1)=min(sbox(1,2,1),sboxtri(1,2,i))
         sbox(2,2,1)=max(sbox(2,2,1),sboxtri(2,2,i))
         sbox(1,3,1)=min(sbox(1,3,1),sboxtri(1,3,i))
         sbox(2,3,1)=max(sbox(2,3,1),sboxtri(2,3,i))
      enddo

c     set defaults for saved min and max box sizes
      sizebox = 0.
      maxbox = 0.
      minbox = alargenumber

 
c.... If there is only one triangle, the root node is a leaf.
c.... (Our convention is to set the link corresponding to a leaf
c.... equal to the negative of the unique triangle contained in
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
c.... subset of ITRI (i.e., subset of triangles associated
c.... with this node) is recorded using the arrays
c.... IMIN and IMAX.
 
      itop=1
      icrstack(itop)=1
      imin(itop)=1
      imax(itop)=mtri
 
c.... Finally, we record in ICT the appropriate ``cutting
c.... direction'' for bisecting the set of triangles
c.... corresponding to this node.  This direction is either
c.... the x, y, or z directions, depending on which dimension
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
 
c.... Partition triangle subset associated with this node.
c.... Using the appropriate cutting direction, use SELECT to
c.... reorder ITRI so that the triangle with median bounding
c.... box center coordinate is itri(imd), while the
c.... triangles {itri(i),i<imd} have SMALLER (or equal)
c.... bounding box coordinates, and the triangles with
c.... {itri(i),i>imd} have GREATER (or equal) bounding box
c.... coordinates.
 
         imd=(imn+imx)/2
         if (icut.eq.1) then
            call select(imd-imn+1,imx-imn+1,xbbc,itri(imn))
         elseif (icut.eq.2) then
            call select(imd-imn+1,imx-imn+1,ybbc,itri(imn))
         else
            call select(imd-imn+1,imx-imn+1,zbbc,itri(imn))
         endif
 
c.... If the first child's subset of triangles is a singleton,
c.... the child is a leaf.  Set the child's link to point to the
c.... negative of the triangle number.  Set the child's bounding
c.... box to be equal to the triangle's bounding box.
 
         if (imn.eq.imd) then
            linkt(nextt)=-itri(imn)
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxtri(k1,k2,itri(imn))
               enddo
            enddo
            nextt=nextt+1
         else
 
c.... In this case, the subset of triangles corresponding to the
c.... first child is more than one triangle, and the child is
c.... not a leaf.  Compute the bounding box of this child to
c.... be the smallest box containing all the associated triangles'
c.... bounding boxes.
 
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxtri(k1,k2,itri(imn))
               enddo
            enddo
            do i=imn+1,imd
               do k2=1,3
                  sbox(1,k2,nextt)=min(sbox(1,k2,nextt),
     &               sboxtri(1,k2,itri(i)))
                  sbox(2,k2,nextt)=max(sbox(2,k2,nextt),
     &               sboxtri(2,k2,itri(i)))
               enddo
            enddo
 
c.... Put the first child onto the stack, noting the
c.... associated triangle subset in IMIN and IMAX, and
c.... putting the appropriate cutting direction in ICT.
 
            dimx=sbox(2,1,nextt)-sbox(1,1,nextt)
            dimy=sbox(2,2,nextt)-sbox(1,2,nextt)
            dimz=sbox(2,3,nextt)-sbox(1,3,nextt)

c           for debug reporting use volume of box
c           protect against 0 length axis
            xmult = dimx
            ymult = dimy
            zmult = dimz
            if (dimx.le. 0.0) xmult = 1.
            if (dimy.le. 0.0) ymult = 1.
            if (dimz.le. 0.0) zmult = 1.

            sizebox=xmult*ymult*zmult
            if (sizebox.ge.maxbox) then
              maxbox = sizebox
              maxdimx = dimx
              maxdimy = dimy
              maxdimz = dimz
            endif
            if (sizebox.lt.minbox) then
              minbox = sizebox
              mindimx = dimx
              mindimy = dimy
              mindimz = dimz
            endif
 
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
 
c.... If the second child's subset of triangles is a singleton,
c.... the child is a leaf.  Set the child's link to point to the
c.... negative of the triangle number.  Set the child's bounding
c.... box to be equal to the triangle's bounding box.
 
         if (imd+1.eq.imx) then
            linkt(nextt)=-itri(imx)
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxtri(k1,k2,itri(imx))
               enddo
            enddo
            nextt=nextt+1
         else
 
c.... In this case, the subset of triangles corresponding to the
c.... second child is more than one triangle, and the child is
c.... not a leaf.  Compute the bounding box of this child to
c.... be the smallest box containing all the associated triangles'
c.... bounding boxes.
 
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxtri(k1,k2,itri(imd+1))
               enddo
            enddo
            do i=imd+2,imx
               do k2=1,3
                  sbox(1,k2,nextt)=min(sbox(1,k2,nextt),
     &               sboxtri(1,k2,itri(i)))
                  sbox(2,k2,nextt)=max(sbox(2,k2,nextt),
     &               sboxtri(2,k2,itri(i)))
               enddo
            enddo
 
 
c.... Put the second child onto the stack, noting the
c.... associated triangle subset in IMIN and IMAX, and
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

       if (idebug .gt. 0) then
         write(logmess,'(a)')
     &   '  kdtree: build done.'
         call writloga('default',1,logmess,0,ierrw)

         write(logmess,'(a,f17.5)')'  Max box volume: ',maxbox
         call writloga('default',0,logmess,0,ierrw)

         write(logmess,'(a,f17.5)')'  Min box volume: ',minbox
         call writloga('default',0,logmess,0,ierrw)

         write(logmess,'(a,f17.5,f17.5,f17.5)')
     &   '  Max dim xyz:    ',
     &   maxdimx,maxdimy,maxdimz
         call writloga('default',0,logmess,0,ierrw)

         write(logmess,'(a,f17.5,f17.5,f17.5)')
     &   '  Min dim xyz:    ',
     &   mindimx,mindimy,mindimz
         call writloga('default',0,logmess,1,ierrw)
       endif

 
      return
      end
 
