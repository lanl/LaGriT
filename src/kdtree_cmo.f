*dk kdtree_cmo
      subroutine kdtree_cmo(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
c
c #####################################################################
c
c     purpose -
c
c     KDTREE_CMO takes the elements of the CMO and produces a k-D tree
c     that is stored in the arrays LINKT and SBOX that are appended to
c     the CMO.  Leaf nodes in LINKT each
c     contain exactly one element index.  Because of the possibility
c     of element overlap, KDTREE also produces an array SBOX which
c     gives 'safety boxes'.  For each node in the k-D tree,
c     there is a corresponding safety box which is just big enough
c     to contain all the elements ``under'' the node.
c
c     input arguments -
c
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
c
c     output arguments -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
c         linkt -        k-D tree links and leaf links to elements.
c                        This array needs to have length 2*nelt.
c         sbox -         ``safety box system'' for k-D tree.
c                        This array needs to have length 12*nelt.
c         ierr -         error return.
c
c     change history -
c
c         $Log: kdtree_cmo.f,v $
c         Revision 2.00  2007/11/05 19:45:59  spchu
c         Import to CVS
c
CPVCS    
CPVCS       Rev 1.3   13 Jun 2007 07:47:36   tam
CPVCS    before deleting attributes, check if they exist
CPVCS    this avoids excessive error reporting
CPVCS    
CPVCS       Rev 1.2   08 Sep 2006 09:28:04   tam
CPVCS    Changed delatt to DELATT and added the current cmo name
CPVCS    to the call passed into dotaskx3d
CPVCS    
CPVCS       Rev 1.1   Mon Sep 21 16:50:12 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.0   Tue May 19 01:18:06 1998   kuprat
CPVCS    Initial revision.
 
c
c   FORMAT:
c
c   KDTREE / {build,release}
c
c  The default is BUILD.
c
      implicit none
 
      integer nwds, imsgin(nwds), msgtype(nwds), ierror
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
 
      include 'consts.h'
      include 'local_element.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      pointer (ipitet,itet)
      integer itet(lenptr)
      pointer (ipitetoff,itetoff)
      integer itetoff(lenptr)
      pointer (ipitettyp,itettyp)
      integer itettyp(lenptr)
      pointer (ipxic,xic)
      real*8 xic(lenptr)
      pointer (ipyic,yic)
      real*8 yic(lenptr)
      pointer (ipzic,zic)
      real*8 zic(lenptr)
      pointer (iplinkt,linkt)
      integer linkt(lenptr)
      pointer (ipsbox,sbox)
      real*8 sbox(2,3,lenptr)
 
      pointer (ipielt,ielt)
      integer ielt(lenptr)
      pointer (ipsboxelt,sboxelt),(ipxbbc,xbbc),(ipybbc,ybbc),
     &   (ipzbbc,zbbc)
      real*8 sboxelt(2,3,lenptr),xbbc(lenptr),ybbc(lenptr),zbbc(lenptr)
 
      integer icscode,ics,i,j,node,nextt,itop,imn,imx,icut,imd,k1,k2,
     &   icrstack(100),imin(100),imax(100),ict(100),ierrw,nelt,icharlnf,
     &   length,icmotype,ilen,iv2,iv12,ityp,itmp
 
      character*32 cmo
      character*132 cbuf
 
      real*8 dimx,dimy,dimz
c
      character*132 logmess
      character*32 isubname
      real*8 alargenumber
      data alargenumber/1.d+99/
 
 
      isubname='kdtree_cmo'
      ierror=0
 
C...  Check that user has specified a valid mesh object.
 
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'KDTREE_CMO: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         ierror=-1
         goto 9999
      endif
 
c.... Release SBOX, LINKT attributes if command is 'release'.
 
      if (cmsgin(2)(1:icharlnf(cmsgin(2))).eq.'release') then
         ilen=icharlnf(cmo)

C        check to see if exist first
C        this avoids error reporting for when these attributes
C        have alread been released

         call cmo_get_info('sbox',cmo,ipsbox,itmp,ityp,ics)
         if (ics.eq.0) then
            cbuf='cmo/DELATT/'//cmo(1:ilen)//' /sbox ; finish'
            call dotaskx3d(cbuf,ierror)
         endif

         call cmo_get_info('v12',cmo,iv12,itmp,ityp,ics)
         if (ics.eq.0) then
            cbuf='cmo/DELATT/'//cmo(1:ilen)//' /v12 ; finish'
            call dotaskx3d(cbuf,ierror)
         endif

         call cmo_get_info('linkt',cmo,iplinkt,itmp,ityp,ics)
         if (ics.eq.0) then
            cbuf='cmo/DELATT/'//cmo(1:ilen)//' /linkt ; finish'
            call dotaskx3d(cbuf,ierror)
         endif

         call cmo_get_info('v2',cmo,iv2,itmp,ityp,ics)
         if (ics.eq.0) then
            cbuf='cmo/DELATT/'//cmo(1:ilen)//' /v2 ; finish'
            call dotaskx3d(cbuf,ierror)
         endif

         goto 9999
      endif
 
c.... Build kdtree.  Check that there are a positive
c.... number of elements.
 
      call cmo_get_info('nelements',cmo,
     *   nelt,length,icmotype,icscode)
 
      if (nelt.le.0) then
         write(logmess,'(a,i6)') 'Error: nelt=',nelt
         call writloga('default',0,logmess,0,ierrw)
         call x3d_error(isubname,' ')
         ierror=-1
         goto 9999
      endif
 
c.... Create V2, V12, SBOX, LINKT attributes if necessary.
 
      call cmo_get_info('v2',cmo,
     *   iv2,length,icmotype,icscode)
      if (icscode.ne.0) then
         cbuf='cmo/addatt//v2/INT' //
     &      '/scalar/scalar/constant/temporary//2.0' //
     &      ' ; finish'
         call dotaskx3d(cbuf,ierror)
      endif
 
      call cmo_get_info('linkt',cmo,
     *   iplinkt,length,icmotype,icscode)
      if (icscode.ne.0) then
         cbuf='cmo/addatt//linkt/VINT' //
     &      '/v2/nelements//temporary/x/0.0' //
     &      ' ; finish'
         call dotaskx3d(cbuf,ierror)
      endif
 
      call cmo_get_info('v12',cmo,
     *   iv12,length,icmotype,icscode)
      if (icscode.ne.0) then
         cbuf='cmo/addatt//v12/INT' //
     &      '/scalar/scalar/constant/temporary//12.0' //
     &      ' ; finish'
         call dotaskx3d(cbuf,ierror)
      endif
 
      call cmo_get_info('sbox',cmo,
     *   ipsbox,length,icmotype,icscode)
      if (icscode.ne.0) then
         cbuf='cmo/addatt//sbox/VDOUBLE' //
     &      '/v12/nelements/linear/temporary/x/0.0' //
     &      ' ; finish'
         call dotaskx3d(cbuf,ierror)
      endif
 
c.... Obtain cmo pointers.
 
      call cmo_get_info('itet',cmo,
     *   ipitet,length,icmotype,icscode)
      call cmo_get_info('itetoff',cmo,
     *   ipitetoff,length,icmotype,icscode)
      call cmo_get_info('itettyp',cmo,
     *   ipitettyp,length,icmotype,icscode)
      call cmo_get_info('xic',cmo,
     *   ipxic,length,icmotype,icscode)
      call cmo_get_info('yic',cmo,
     *   ipyic,length,icmotype,icscode)
      call cmo_get_info('zic',cmo,
     *   ipzic,length,icmotype,icscode)
      call cmo_get_info('linkt',cmo,
     *   iplinkt,length,icmotype,icscode)
      call cmo_get_info('sbox',cmo,
     *   ipsbox,length,icmotype,icscode)
 
c....Obtain allocations for local arrays.
 
      call mmgetblk('ielt',isubname,ipielt,nelt,1,icscode)
      call mmgetblk('xbbc',isubname,ipxbbc,nelt,2,icscode)
      call mmgetblk('ybbc',isubname,ipybbc,nelt,2,icscode)
      call mmgetblk('zbbc',isubname,ipzbbc,nelt,2,icscode)
      call mmgetblk('sboxelt',isubname,ipsboxelt,6*nelt,2,icscode)
 
c.... Compute the bounding boxes for the individual elements and
c.... put this information in SBOXELT.  Let the bounding box
c.... associated with the root node [SBOX(*,*,1)] exactly contain
c.... all the element bounding boxes.  Put the centers of the
c.... element bounding boxes into XBBC, YBBC, ZBBC.
 
      sbox(1,1,1)=alargenumber
      sbox(2,1,1)=-alargenumber
      sbox(1,2,1)=alargenumber
      sbox(2,2,1)=-alargenumber
      sbox(1,3,1)=alargenumber
      sbox(2,3,1)=-alargenumber
 
      do i=1,nelt
         ityp=itettyp(i)
         sboxelt(1,1,i)=alargenumber
         sboxelt(2,1,i)=-alargenumber
         sboxelt(1,2,i)=alargenumber
         sboxelt(2,2,i)=-alargenumber
         sboxelt(1,3,i)=alargenumber
         sboxelt(2,3,i)=-alargenumber
 
         do j=1,nelmnen(ityp)
            sboxelt(1,1,i)=min(sboxelt(1,1,i),xic(itet(j+itetoff(i))))
            sboxelt(2,1,i)=max(sboxelt(2,1,i),xic(itet(j+itetoff(i))))
            sboxelt(1,2,i)=min(sboxelt(1,2,i),yic(itet(j+itetoff(i))))
            sboxelt(2,2,i)=max(sboxelt(2,2,i),yic(itet(j+itetoff(i))))
            sboxelt(1,3,i)=min(sboxelt(1,3,i),zic(itet(j+itetoff(i))))
            sboxelt(2,3,i)=max(sboxelt(2,3,i),zic(itet(j+itetoff(i))))
         enddo
 
         xbbc(i)=0.5*(sboxelt(1,1,i)+sboxelt(2,1,i))
         ybbc(i)=0.5*(sboxelt(1,2,i)+sboxelt(2,2,i))
         zbbc(i)=0.5*(sboxelt(1,3,i)+sboxelt(2,3,i))
 
         sbox(1,1,1)=min(sbox(1,1,1),sboxelt(1,1,i))
         sbox(2,1,1)=max(sbox(2,1,1),sboxelt(2,1,i))
         sbox(1,2,1)=min(sbox(1,2,1),sboxelt(1,2,i))
         sbox(2,2,1)=max(sbox(2,2,1),sboxelt(2,2,i))
         sbox(1,3,1)=min(sbox(1,3,1),sboxelt(1,3,i))
         sbox(2,3,1)=max(sbox(2,3,1),sboxelt(2,3,i))
      enddo
 
c.... If there is only one element, the root node is a leaf.
c.... (Our convention is to set the link corresponding to a leaf
c.... equal to the negative of the unique element contained in
c.... that leaf.)  If the root is a leaf, our work is done.
 
      if (nelt.eq.1) then
         linkt(1)=-1
         goto 9999
      endif
 
c.... DIMX, DIMY, DIMZ are equal to the x, y, and z dimensions
c.... of the bounding box corresponding to the current node
c.... under consideration.
      dimx=sbox(2,1,1)-sbox(1,1,1)
      dimy=sbox(2,2,1)-sbox(1,2,1)
      dimz=sbox(2,3,1)-sbox(1,3,1)
 
c.... IELT will contain a permutation of the integers
c.... {1,...,NELT}.  This permutation will be altered as we
c.... create our balanced binary tree.  NEXTT contains the
c.... address of the next available node to be filled.
 
      nextt=2
      do i=1,nelt
         ielt(i)=i
      enddo
 
c.... Use a stack to create the tree.  Put the root node
c.... ``1'' on the top of the stack (ICRSTACK).  The array
c.... subset of IELT (i.e., subset of elements associated
c.... with this node) is recorded using the arrays
c.... IMIN and IMAX.
 
      itop=1
      icrstack(itop)=1
      imin(itop)=1
      imax(itop)=nelt
 
c.... Finally, we record in ICT the appropriate ``cutting
c.... direction'' for bisecting the set of elements
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
 
c.... Partition element subset associated with this node.
c.... Using the appropriate cutting direction, use SELECT to
c.... reorder IELT so that the element with median bounding
c.... box center coordinate is ielt(imd), while the
c.... elements {ielt(i),i<imd} have SMALLER (or equal)
c.... bounding box coordinates, and the elements with
c.... {ielt(i),i>imd} have GREATER (or equal) bounding box
c.... coordinates.
 
         imd=(imn+imx)/2
         if (icut.eq.1) then
            call select(imd-imn+1,imx-imn+1,xbbc,ielt(imn))
         elseif (icut.eq.2) then
            call select(imd-imn+1,imx-imn+1,ybbc,ielt(imn))
         else
            call select(imd-imn+1,imx-imn+1,zbbc,ielt(imn))
         endif
 
c.... If the first child's subset of elements is a singleton,
c.... the child is a leaf.  Set the child's link to point to the
c.... negative of the element number.  Set the child's bounding
c.... box to be equal to the element's bounding box.
 
         if (imn.eq.imd) then
            linkt(nextt)=-ielt(imn)
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxelt(k1,k2,ielt(imn))
               enddo
            enddo
            nextt=nextt+1
         else
 
c.... In this case, the subset of elements corresponding to the
c.... first child is more than one element, and the child is
c.... not a leaf.  Compute the bounding box of this child to
c.... be the smallest box containing all the associated elements'
c.... bounding boxes.
 
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxelt(k1,k2,ielt(imn))
               enddo
            enddo
            do i=imn+1,imd
               do k2=1,3
                  sbox(1,k2,nextt)=min(sbox(1,k2,nextt),
     &               sboxelt(1,k2,ielt(i)))
                  sbox(2,k2,nextt)=max(sbox(2,k2,nextt),
     &               sboxelt(2,k2,ielt(i)))
               enddo
            enddo
 
c.... Put the first child onto the stack, noting the
c.... associated element subset in IMIN and IMAX, and
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
 
c.... If the second child's subset of elements is a singleton,
c.... the child is a leaf.  Set the child's link to point to the
c.... negative of the element number.  Set the child's bounding
c.... box to be equal to the element's bounding box.
 
         if (imd+1.eq.imx) then
            linkt(nextt)=-ielt(imx)
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxelt(k1,k2,ielt(imx))
               enddo
            enddo
            nextt=nextt+1
         else
 
c.... In this case, the subset of elements corresponding to the
c.... second child is more than one element, and the child is
c.... not a leaf.  Compute the bounding box of this child to
c.... be the smallest box containing all the associated elements'
c.... bounding boxes.
 
            do k1=1,2
               do k2=1,3
                  sbox(k1,k2,nextt)=sboxelt(k1,k2,ielt(imd+1))
               enddo
            enddo
            do i=imd+2,imx
               do k2=1,3
                  sbox(1,k2,nextt)=min(sbox(1,k2,nextt),
     &               sboxelt(1,k2,ielt(i)))
                  sbox(2,k2,nextt)=max(sbox(2,k2,nextt),
     &               sboxelt(2,k2,ielt(i)))
               enddo
            enddo
 
 
c.... Put the second child onto the stack, noting the
c.... associated element subset in IMIN and IMAX, and
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
 
