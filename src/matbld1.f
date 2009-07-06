*dk,matbld1
      subroutine matbld1(idiag,ipackopt,
     *                   nsd,nen,nef,
     *                   nconn,lconn,
     *                   npoints,ntets,itet,
     *                   irowmat,icolmat,isort,isendnn,
     *                   irowcnt,irowoff)
c
c #####################################################################
c
c     purpose -
c
c        none
c
c     input arguments -
c
c        none
c
c     output arguments -
c
c        none
c
c     change history -
c
c        $Log: matbld1.f,v $
c        Revision 2.00  2007/11/05 19:46:01  spchu
c        Import to CVS
c
cpvcs
cpvcs       rev 1.9   06 nov 2002 08:16:48   dcg
cpvcs    make routine implicit none
cpvcs    change lengths of requests to integer where appropriate
cpvcs    remove duplicate declarations
cpvcs
cpvcs       rev 1.8   wed aug 25 09:20:20 1999   gable
cpvcs    ssort routine replaced with hpsort1 routine. this fixed
cpvcs    the error that was happening when running large (>100000 node)
cpvcs    problems. code change made by dcg.
cpvcs
cpvcs       rev 1.7   wed aug 25 09:10:52 1999   gable
cpvcs    this version is being checked in only to keep the
cpvcs    memory io that was added. this was for debugging
cpvcs    and is not necessary. the next version had the
cpvcs    fix to the sort routine installed so this version
cpvcs    should not be used for computations.
cpvcs
cpvcs       rev 1.6   mon apr 14 16:52:54 1997   pvcs
cpvcs    no change.
cpvcs
cpvcs       rev 1.5   fri aug 23 08:19:44 1996   dcg
cpvcs    increase memory for xcount
cpvcs
cpvcs       rev 1.4   11/07/95 17:20:04   dcg
cpvcs    change flag to 2 in mmgetblk calls
cpvcs
cpvcs       rev 1.3   08/15/95 18:20:26   het
cpvcs    cleanup code and correct errors
cpvcs
cpvcs       rev 1.2   06/27/95 11:11:32   dcg
cpvcs    remove second literal argument in memory management calls
cpvcs
cpvcs       rev 1.1   01/09/95 09:37:26   het
cpvcs    correct an error on unicos.
cpvcs
cpvcs
cpvcs       rev 1.0   11/10/94 12:16:06   pvcs
cpvcs    original version.
c
c ######################################################################
c
      implicit none
c
      character*132 logmess
      character*8 isubname
c
c
c#######################################################################
c
c      purpose -
c
c         build a matrix that identifies the neighbors for each point
c
c      input arguments -
c
c         idiag     - include the diagonal (=0 ==> no, =1 ==> yes)
c         ipackopt  - sparse matrix packing option.
c         nsd       - the space dimension
c         nen       - number of element nodes.
c         nef       - number of element faces.
c         npoints   - number of points in the mesh.
c         ntets     - number of tets in the mesh.
c         itet(nen,ntets)   - tet array of 1st mass point index.
c
c      output arguments -
c
c         irowmat() - list of row numbers for each column entry.
c         icolmat() - list of column entries.
c         isort()   - list of indices defining the sorted order.
c         isendnn() - the array offset to send each (partial)tet
c                        coefficent.
c         irowcnt() - the numbers of column entries for each row.
c         irowoff() - the offset to the start of the column entries
c                        for each row.
c
c      change history -
c
c        $log$
c
c#######################################################################
c
c
c#######################################################################
c
      integer idiag,ipackopt,nsd,nen,nef,nconn,npoints,ntets
      integer itet(nen,ntets)
c
      integer lconn(2,nconn)
c
      integer icolmat(nconn*ntets+idiag*npoints)
      integer irowmat(nconn*ntets+idiag*npoints)
      integer isendnn(nconn*ntets+idiag*npoints)
      integer irowcnt(npoints), irowoff(npoints)
c
c#######################################################################
c
 
      pointer (ipxsort, xsort)
      pointer (ipisortr, isortr)
      pointer (ipisortc, isortc)
      pointer (ipitemp, itemp)
      pointer (ipxtemp, xtemp)
      pointer (ipxcount, xcount)
      pointer (ipilogical, ilogical)
      pointer (ipitmp1, itmp1)
      pointer (ipitmp2, itmp2)
      pointer (ipitmp3, itmp3)
      pointer (ipitmp4, itmp4)
      pointer (ipmask, mask)
      integer isort(nconn*ntets+idiag*npoints)
      real*8 xsort(nconn*ntets+idiag*npoints)
      integer isortr(nconn*ntets+idiag*npoints)
      integer isortc(nconn*ntets+idiag*npoints)
      integer itemp(nconn*ntets+idiag*npoints)
      real*8 xtemp(nconn*ntets+idiag*npoints)
      real*8 xcount(*)
      logical ilogical(nconn*ntets+idiag*npoints)
      integer itmp1(nconn*ntets+idiag*npoints)
      integer itmp2(nconn*ntets+idiag*npoints)
      integer itmp3(nconn*ntets+idiag*npoints)
      integer itmp4(npoints-1)
      logical mask(nconn*ntets+idiag*npoints)
      integer length,icscode,i,ii,iset,istart,istop,jj,itetmax,
     *  n12,iimax,nnmax,ierrw
      real*8 ascend
      integer isort_max, isortr_max, isortc_max
c
c
c#######################################################################
      isubname='matbld1'
c
      n12=nconn*ntets+idiag*npoints
 
      length=2*n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   xtemp: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('xtemp',isubname,ipxtemp,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  xtemp ",ierrw
        call writloga('default',0,logmess,1,icscode)
        call mmprint()
        goto 9999
      endif
 
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk  isortr: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('isortr',isubname,ipisortr,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  isortr ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk  isortc: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('isortc',isubname,ipisortc,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  isortc ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
      length=max(npoints,n12)
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk  xcount: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('xcount',isubname,ipxcount,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  xcount ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

c
c     ..................................................................
c     construct the nconn possible pairs of points for each tetrahedron. ea
c        tet edge contributes two combinations.
c
      do i=1,n12
         isortr(i)=0
         isortc(i)=0
      enddo
      do iset=1,nconn
         istart=1+(iset-1)*ntets
         istop=iset*ntets
         do i=istart,istop
            isortr(i)=itet(lconn(1,iset),i-istart+1)
            isortc(i)=itet(lconn(2,iset),i-istart+1)
         enddo
      enddo
c
c     ..................................................................
c     use an array constructor to put diagonal contributions at the end.
c
      if(idiag.eq.1) then
         iset=nconn+1
         istart=1+(iset-1)*ntets
         istop=istart+npoints-1
         jj=istart
         do ii=1,npoints
          isortr(jj) = ii
          isortc(jj) = ii
          jj=jj+1
         enddo
      endif
c
c     ..................................................................
c     construct a sorting key that has two parts. the number to the left
c        decimal points represents the 'row' number of the matrix.  the
c        to the left of the decimal point represents the 'column' entry
c        matrix.
c
      itetmax = isortc(iimax(n12,isortc,1))
c      do i=1,n12
c         xsort(i)=dble(isortr(i))+
c     *            dble(isortc(i))/(10.0d+00*dble(itetmax))
c      enddo
c
c    use a multi-key sort. the above method dies if itetmax is o(10**7)
c
      do i=1,n12
         xtemp(2*(i-1)+1)=dble(isortr(i))
         xtemp(2*(i-1)+2)=dble(isortc(i))
      enddo
c
c     ..................................................................
c     sort the keys and return an array of sorted indices.
c
      do ii=1,n12
         isort(ii) = ii
      enddo
c      do ii=1,2*n12
c         xtemp(ii) = xsort(ii)
c      enddo
      ascend=1.0
c      call hpsort1(n12,xtemp,ascend,isort)
c
      call hpsortrmp(n12,2,2,xtemp,ascend,isort)
c
c     since xtemp is used later with lenght n12, release this
c     memory of length 2*n12 and allocate xtemp again later.
c
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   xtemp: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('xtemp',isubname,ipxtemp,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed:  xtemp ",icscode
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)')
     *   "matbld1 continues... "
        call writloga('default',0,logmess,1,ierrw)
      endif

c
c     ..................................................................
c     (re)arrange the rows and column arrays according to the sort key
c        order.
c
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   itemp: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('itemp',isubname,ipitemp,length,1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  itemp ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

c
      do i=1,n12
         itemp(i) = isortr(isort(i))
      enddo
      do i=1,n12
         isortr(i) = itemp(i)
      enddo
      do i=1,n12
         itemp(i) = isortc(isort(i))
      enddo
      do i=1,n12
         isortc(i) = itemp(i)
      enddo
c
c     ..................................................................
c     create the sparse coefficient matrix and the sparse matrix pattern
c
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   itmp1: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('itmp1',isubname,ipitmp1,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  xtmp1 ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   itmp2: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('itmp2',isubname,ipitmp2,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  itmp2 ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   itmp3: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('itmp3',isubname,ipitmp3,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  itmp3 ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

c     ..................................................................
c     identify the edges of the sorted column index.
c
c
c     create tmp arrays with a right circular shift
c
      do ii=1,n12-1
       itmp1(ii) = isortr(ii+1)
       itmp2(ii) = isortc(ii+1)
      enddo
      itmp1(n12)= isortr(1)
      itmp2(n12)= isortc(1)
c
c     create tmp arrays with a left circular shift
c
      do ii=2,n12
       itmp3(ii) = isortc(ii-1)
      enddo
      itmp3(1) = isortc(n12)
 
      do ii=1,n12
       if (itmp1(ii).eq.isortr(ii) .and.
     &     itmp2(ii).ne.isortc(ii) .and.
     &     itmp3(ii).eq.isortc(ii) ) then
       isortc(ii) = -1
       endif
      enddo
 
      do ii=2,n12
       itmp1(ii) = isortr(ii-1)
      enddo
      itmp1(1) = isortr(n12)
 
      do ii=1,n12
       if (itmp1(ii).eq.isortr(ii) .and.
     &     itmp3(ii).eq.isortc(ii) ) then
       isortc(ii) = -2
       endif
      enddo
c
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   itmp2: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('itmp2',isubname,ipitmp2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed:  itmp2 ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif

 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   itmp3: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('itmp3',isubname,ipitmp3,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed:  itmp3 ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif
c
c     ..................................................................
c     create an array that gives the array offset to the start of the
c        list of tets for each point.
c
      do i=1,npoints
         irowoff(i)=0
      enddo
      do i=1,n12
         itemp(i)=i
      enddo
      do ii=1,n12-1
       itmp1(ii) = isortr(ii+1)
      enddo
      itmp1(n12) = isortr(1)
      do ii=1,n12
       if ( isortr(ii).eq.itmp1(ii)) itemp(ii) = 0
      enddo
 
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk    mask: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('mask',isubname,ipmask,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  mask ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
      do i=1,n12
         mask(i)=.false.
         if(itemp(i).ne.0) mask(i)=.true.
      enddo
 
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   itmp4: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('itmp4',isubname,ipitmp4,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  itmp4 ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
      call packsi(n12,npoints-1,itmp4,itemp,mask)
      do i=2,npoints
         irowoff(i)=itmp4(i-1)
      enddo
 
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      :mmgetblk ilogical: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('ilogical',isubname,ipilogical,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  ilogical ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
      do i=1,n12
        isendnn(i)=0
        itemp(i)=1
        ilogical(i)=.false.
      enddo
      do i=1,npoints
         ilogical(irowoff(i)+1)=.true.
      enddo
c     *** set the logical value for the first entry for each
c            point to true.
      do i=1,n12
         mask(i)=.false.
         if(isortc(i) .gt. 0) mask(i)=.true.
      enddo
 
      call cr_add(n12,isendnn,itemp,ilogical,mask)
 
      do ii=1,n12
         ilogical(ii)=.false.
         if (isendnn(ii).ne.0) ilogical(ii)=.true.
      enddo
      do ii=1,n12
         mask(ii) = .true.
         itemp(ii) = isendnn(ii)
      enddo
 
      call cr_copy(n12,isendnn,itemp,ilogical,mask)
 
c
c     ..................................................................
c     compress the row and column arrays to get the fully
c        compressed sparse matrix pattern.
c
      do i=1,n12
         icolmat(i)=0
         irowmat(i)=0
      enddo
 
      ii = 1
      do jj=1,n12
       if (isortc(jj).gt.0) then
          icolmat(ii) = isortc(jj)
          irowmat(ii) = isortr(jj)
          ii=ii+1
       endif
      enddo
 
c
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk  isortc: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('isortc',isubname,ipisortc,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed: isortc ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif

c     ..................................................................
c     create an array that counts the number of column entries in each
c        row.
c
      length=n12
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmgetblk   xtemp: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmgetblk('xtemp',isubname,ipxtemp,length,2,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmgetblk failed:  xtemp ",icscode
        call writloga('default',0,logmess,1,ierrw)
        call mmprint()
        goto 9999
      endif

 
      do ii=1,n12
         xtemp(ii)=1.0
        if (irowmat(ii).eq.0) then
         xtemp(ii) = 0.0
        else
         xtemp(ii) = 1.0
        endif
      enddo
      do i=1,npoints
         xcount(i)=0.0
      enddo
 
      call xsumsp2r(npoints,n12,xcount,irowmat,xtemp)
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   xtemp: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('xtemp',isubname,ipxtemp,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed:  xtemp ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif


 
      do i=1,npoints
         irowcnt(i)=xcount(i)
      enddo
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk  xcount: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('xcount',isubname,ipxcount,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed:  xcount ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif

c
c     ..................................................................
c     create an array that gives the array offset to the start of the
c        list of column entries for each row.
c
      do i=1,npoints
         irowoff(i)=0
      enddo
      do i=1,n12
         itemp(i)=i
      enddo
      do ii=1,n12-1
       itmp1(ii) = irowmat(ii+1)
      enddo
       itmp1(n12) = irowmat(1)
      do ii=1,n12
       if (irowmat(ii).eq.itmp1(ii)) itemp(ii) = 0
      enddo
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   itmp1: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('itmp1',isubname,ipitmp1,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed: itmp1 ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif

 
      do ii=1,n12
         mask(ii)=.false.
         if(itemp(ii) .ne. 0) mask(ii)=.true.
      enddo
 
      call packsi(n12,npoints-1,itmp4,itemp,mask)
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   itemp: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('itemp',isubname,ipitemp,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed: itemp ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif

 
      do i=2,npoints
         irowoff(i)=itmp4(i-1)
      enddo
 
c     write(logmess,'(a,i14)')
c    *   "matbld1      : mmrelblk   itmp4: ",length
c     call writloga('default',0,logmess,0,icscode)
 
      call mmrelblk('itmp4',isubname,ipitmp4,icscode)
      if (icscode .ne. 0) then
        write(logmess,'(a,i5)')
     *   "matbld1      : mmrelblk failed: itmp4 ",icscode
        call writloga('default',0,logmess,1,ierrw)
      endif

c
c     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
c     construct a list of send addresses to send all the coupling
c        coefficients to their row/column address in the sparse
c        matrix.
c
      nnmax = irowcnt(iimax(npoints,irowcnt,1))
 
      if(ipackopt.eq.0) then
         do ii=1,n12
          if(isortr(ii).gt.0) then
            isendnn(ii)=isendnn(ii)+(isortr(ii)-1)*nnmax
          endif
         enddo
      elseif(ipackopt.eq.1) then
         do ii=1,n12
          if(isortr(ii).gt.0) then
            isendnn(ii)=isendnn(ii)+irowoff(isortr(ii))
          endif
         enddo
      endif
      goto 9999
 9999 continue
 
      call mmrelprt(isubname,icscode)
 
      return
      end
