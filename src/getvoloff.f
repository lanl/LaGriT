*dk,getvoloff
      subroutine getvoloff(ieltno,ieltary,x,y,z,itet,itetoff,itettyp,
     &   epsilonv,isubname1,
     &   ipvoloff,iplocvoloff,ipivoloffoff)
C #####################################################################
C
C     PURPOSE -
C
C        GETVOLOFF gets the volume offset list for a 3D mesh.
C        This is a list of (virtual) tetrahedra that are
C        inverted and need to be specially treated in smoothing.
C
C     INPUT ARGUMENTS -
C
C         ieltno    - length of ieltary
C         ieltary   - array of relevant elements that need
C                     to be processed.
C         x,y,z     - arrays of x, y, and z coordinates.
C         itet      - array containing element-node relation for
C                     whole mesh.
C         itetoff   - offset array for itet.
C         itettyp   - array giving element types for all elements
C                     in the mesh.
C         epsilonv  - small volume which triggers whether a tetrahedral
C                     volume is sufficiently small to require special
C                     offset treatment.
C         isubname1 - name of partition which contains the output
C                     arrays.
C
C     INPUT/OUTPUT ARGUMENTS -
C
C         ipvoloff  - Pointer to VOLOFF array which gives, for the I'th
C                     element IELTARY(I) a set of volume offsets for
C                     any bad virtual tetrahedra that may occur in that
C                     element.
C         iplocvoloff   - Pointer to LOCVOLOFF array which gives the
C                         virtual tet numbers corresponding to the
C                         entries in VOLOFF.
C         ipivoloffoff  - Pointer to IVOLOFFOFF array which gives the
C                     offsets into the VOLOFF array.
C
C     CHANGE HISTORY -
C
C        $Log: getvoloff.f,v $
C        Revision 2.00  2007/11/05 19:45:57  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   30 Jul 2001 16:17:54   kuprat
CPVCS    Effectively disabled offset volumes.
CPVCS    
CPVCS       Rev 1.0   31 Jan 2000 17:13:06   kuprat
CPVCS    Initial revision.
C#####################################################################
 
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'smooth.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      pointer (ipvoloff,voloff)
      pointer (iplocvoloff,locvoloff)
      pointer (ipivoloffoff,ivoloffoff)
 
      integer ieltno,ieltary(lenptr),itet(lenptr),itetoff(lenptr),
     &   itettyp(lenptr),locvoloff(lenptr),ivoloffoff(lenptr)
 
      real*8 x(lenptr),y(lenptr),z(lenptr),
     &   epsilonv,voloff(lenptr)
 
      pointer (ipvoloff1,voloff1), (iplocvoloff1,locvoloff1),
     &   (iplinkvoloff,linkvoloff), (ipifirstvoloff,ifirstvoloff)
      integer locvoloff1(lenptr),linkvoloff(lenptr),
     &   ifirstvoloff(lenptr)
      real*8 voloff1(lenptr)
 
      integer length,icscode,i,next,inc,ind,lenorig,
     &   nextvoloff,ii,ihyb,loctet,i1,i2,i3,i4
 
      real*8 afac(3,4),vol6
 
      character*32 isubname, isubname1
 
      real*8 crosx,crosy,crosz,x1,y1,z1,x2,y2,z2,x3,y3,z3
 
c...  Statement functions for the components of the cross product
c...  ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
 
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
      isubname='getvoloff'
 
c.... Get initial storage for temporary arrays used in compiling
c.... the topological relation as a set of linked lists.
c.... IFIRST1, for each element, points to the beginning of each linked
c.... list.  If the I'th list is empty, IFIRST(I)=0.
c.... LINK contains the links.  We make an initial (hopefully high) estimate
c.... that the total number of bad virtual tetrahedra
c.... in IELTNO elements is IELTNO/10.
c.... As more storage is needed, it is assigned in blocks
c.... of length IELTNO/10.  Since IELTNO could be very small, we make
c.... sure that memory is incremented by at least 1000.
c.... VOLOFF1, LOCVOLOFF1 hold the data fields.  VOLOFF1 holds
c.... (six times) the offset volume for each low volume tet.  (The offset
c.... volume is set to be EPSILONV less than the true volume.  This
c.... causes the functional being minimized in POLYFUN to see a volume
c.... which is at least +EPSILONV, even if the true volume is negative.)
c.... LOCVOLOFF1 contains the corresponding local virtual tet number of the
c.... bad tet.
 
      length=max(ieltno/10,1000)
      call mmgetblk('voloff1',isubname,ipvoloff1,length,2,icscode)
      call mmgetblk('locvoloff1',isubname,iplocvoloff1,length,1,icscode)
      call mmgetblk('linkvoloff',isubname,iplinkvoloff,length,1,icscode)
 
      call mmgetblk('ifirstvoloff',isubname,ipifirstvoloff,ieltno,1
     &   ,icscode)
 
      do i=1,ieltno
         ifirstvoloff(i)=0
      enddo
      nextvoloff=1
 
c.... Loop through all (possibly hybrid) elements in IELTARY.
 
      do ii=1,ieltno
         ihyb=ieltary(ii)
 
c.... Loop through all virtual tetrahedra corresponding to this type
c.... of element.
 
         do loctet=1,ihybnumtetv(itettyp(ihyb) )
 
            i1=itet( itetv(1,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i2=itet( itetv(2,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i3=itet( itetv(3,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i4=itet( itetv(4,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            afac(1,1)=crosx(x(i2),y(i2),z(i2),x(i3),
     &         y(i3),z(i3),x(i4),y(i4),z(i4))
            afac(2,1)=crosy(x(i2),y(i2),z(i2),x(i3),
     &         y(i3),z(i3),x(i4),y(i4),z(i4))
            afac(3,1)=crosz(x(i2),y(i2),z(i2),x(i3),
     &         y(i3),z(i3),x(i4),y(i4),z(i4))
            vol6=afac(1,1)*(x(i2)-x(i1))+
     &         afac(2,1)*(y(i2)-y(i1))+afac(3,1)*(z(i2)-z(i1))
 
c.... Volume is small, so tet needs an offset entry
 
            if (vol6.lt.-1.d50) then
c$$$            if (vol6.le.6.*epsilonv) then
 
c.... If memory length will be exceeded by addition of another tet,
c.... allocate more.
 
               if (length.le.nextvoloff) then
                  inc=max(ieltno/10,1000)
                  length=length+inc
                  call mmincblk('voloff1',isubname,ipvoloff1,
     &               inc,icscode)
                  call mmincblk('locvoloff1',isubname,iplocvoloff1,
     &               inc,icscode)
                  call mmincblk('linkvoloff',isubname,iplinkvoloff,inc
     &               ,icscode)
               endif
 
c.... Add required tet offset and local tet index to linked list
c.... corresponding to element II.  (If IFIRSTVOLOFF is 0, we
c.... are actually STARTING a linked list.)
 
              if (ifirstvoloff(ii).eq.0) then
                  ifirstvoloff(ii)=nextvoloff
               else
                  ind=ifirstvoloff(ii)
                  do while(linkvoloff(ind).ne.0)
                     ind=linkvoloff(ind)
                  enddo
                  linkvoloff(ind)=nextvoloff
               endif
               voloff1(nextvoloff)=vol6-6.*epsilonv
               locvoloff1(nextvoloff)=loctet
               linkvoloff(nextvoloff)=0
               nextvoloff=nextvoloff+1
            endif
         enddo
      enddo
 
c.... Convert temporary arrays containing linked lists to
c.... packed arrays in the ISUBNAME1 partition.
 
 
c.... For each array in ISUBNAME1, check if it has sufficient
c.... length or even exists.  If not, fix the problem.
 
      call mmblklen('voloff',isubname1,ipvoloff,lenorig,icscode)
      if (length.gt.lenorig) then
         if (icscode.eq.0) then
            inc=length-lenorig
            call mmincblk('voloff',isubname1,ipvoloff,inc,icscode)
         else
            call mmgetblk('voloff',isubname1,ipvoloff,length,2
     &         ,icscode)
         endif
      endif
 
      call mmblklen('locvoloff',isubname1,iplocvoloff,lenorig,icscode)
      if (length.gt.lenorig) then
         if (icscode.eq.0) then
            inc=length-lenorig
            call mmincblk('locvoloff',isubname1,iplocvoloff,inc,icscode)
         else
            call mmgetblk('locvoloff',isubname1,iplocvoloff,length,1
     &         ,icscode)
         endif
      endif
 
      call mmblklen('ivoloffoff',isubname1,ipivoloffoff,lenorig
     &   ,icscode)
      if (ieltno+1.gt.lenorig) then
         if (icscode.eq.0) then
            inc=ieltno+1-lenorig
            call mmincblk('ivoloffoff',isubname1,ipivoloffoff,inc
     &         ,icscode)
         else
            call mmgetblk('ivoloffoff',isubname1,ipivoloffoff,ieltno+1,1
     &         ,icscode)
         endif
      endif
 
 
c.... Dump out all IELTNO linked lists into compressed arrays.
 
      next=1
      do ii=1,ieltno
         ivoloffoff(ii)=next-1
         ind=ifirstvoloff(ii)
         do while (ind.ne.0)
            voloff(next)=voloff1(ind)
            locvoloff(next)=locvoloff1(ind)
            ind=linkvoloff(ind)
            next=next+1
         enddo
      enddo
      ivoloffoff(ieltno+1)=next-1
 
c.... Release temporary arrays.
 
      call mmrelprt(isubname,icscode)
 
      return
      end
 
