      subroutine reorder(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C ######################################################################
C
C     PURPOSE -
C
C        Decide if task if reorder_node or reorder_elemet
C
C     INPUT ARGUMENTS -
C
C     OUTPUT ARGUMENTS -
C
C     CHANGE HISTORY -
C
C        $Log: reorder.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   23 Aug 2006 15:24:18   gable
CPVCS    Added checks for case where there are zero elements.
CPVCS    
CPVCS       Rev 1.2   13 Nov 2002 13:09:16   gable
CPVCS    Remove some print statements left behind in development.
CPVCS    
CPVCS       Rev 1.1   01 Nov 2002 13:06:30   gable
CPVCS    Added option to reorder elements.
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds), ierr
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C#######################################################################
C
      character*132 logmess
C
C ######################################################################
C
      character*132 cmo_name
C
C ######################################################################
      pointer (ipisort_key, isort_key)
      integer isort_key(*)
      integer ilen_isort_key, itype_isort_key
      character*32 cname_isort_key
C
C ######################################################################
C
C    Local variables
      integer ilen, itype, nnode, nelem
C
      integer icharlnf
C
C#######################################################################
C
c
c    2 - Get the mesh object name
c
      cmo_name = cmsgin(2)
      if((cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. '-def-'))
     1   then
         call cmo_get_name(cmo_name, ierr)
         if(ierr.ne.0) then
           write(logmess,9000) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9000   format("REORDER: CMO found bad mesh object: ",a)
           call writloga('default',0,logmess,0,ierr)
           ierr = -1
           goto 9999
         endif
      endif
C#######################################################################
C
c    3 - Get pointer, length and type of sort key array
C
      if(nwds .lt. 3)then
         logmess = 'REORDER: ERROR, third arugment must be sort key'
           call writloga('default',0,logmess,0,ierr)
           ierr = -1
           goto 9999
      endif

      cname_isort_key = cmsgin(3)(1:icharlnf(cmsgin(3)))

      call cmo_get_info(
     1  cname_isort_key,cmo_name,
     2  ipisort_key,
     3  ilen_isort_key,
     4  itype_isort_key,
     5  ierr)
C
C#######################################################################
C

      call cmo_get_info('nnodes',cmo_name,nnode,ilen,itype,ierr)
      call cmo_get_info('nelements',cmo_name,nelem,ilen,itype,ierr)

      if(ilen_isort_key .eq. nnode)then
C         print *, 'CALL REORDER_NODE'
         call reorder_node(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
      elseif(ilen_isort_key .eq. nelem)then
C         print *, 'CALL REORDER_ELEMENT'
         call reorder_element(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
      else
C
C     ERROR
C
         logmess =
     1   'REORDER: ERROR, sort key must be length nnode or nelem'
         call writloga('default',0,logmess,0,ierr)
         ierr = -1
         goto 9999
      endif
C
 9999 continue
      return
      end
      subroutine reorder_element(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C ######################################################################
C
C     PURPOSE -
C
C        Reorders elements from a mesh_object based on the reorder key array.
C
C
C         FORMAT:    reorder/cmo_name/sort_key
C         FORMAT:    reorder/  -def- /sort_key
C
C         This module will reorder a MO according to a designated
C         permutation vector. The permutation vector can be any
C         integer vector nelements long with min value = 1, max value = nelements
C         and no repeated entries. The standard way that the permuation
C         vector might be computed is with the SORT/cmo/index... command.
C
C         sort_key - is the permutation vector - i.e. an integer element
C                    based mesh object attribute
C         This module will reorder all element based attribute arrays,
C         update the isn parent child pointers and update the element
C         connectivity itet and jtet array.
C
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "local_element.h"
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds), ierr
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C#######################################################################
C
      character*132 logmess
C ######################################################################
C
      character*132 cmo_name
      integer ier,index
C
C ######################################################################
C
      pointer (ipitetclr1, itetclr1)
      pointer (ipitettyp1, itettyp1)
      pointer (ipitetoff1, itetoff1)
      pointer (ipjtetoff1, jtetoff1)
      pointer (ipitet1, itet1)
      pointer (ipjtet1, jtet1)
      integer itetclr1(*),itettyp1(*),itetoff1(*)
     &       ,jtetoff1(*),itet1(*),jtet1(*)
C
      pointer (ipitetclr2, itetclr2)
      pointer (ipitettyp2, itettyp2)
      pointer (ipitetoff2, itetoff2)
      pointer (ipjtetoff2, jtetoff2)
      pointer (ipitet2, itet2)
      pointer (ipjtet2, jtet2)
      integer itetclr2(*),itettyp2(*),itetoff2(*)
     &       ,jtetoff2(*),itet2(*),jtet2(*)
C
      pointer (ipisort_key, isort_key)
      integer isort_key(*)
      integer ilen_isort_key, itype_isort_key
C
C ######################################################################
C
      integer iesave(*)
      pointer (ipiesave, iesave)
      integer iechange(*)
      pointer (ipiechange, iechange)
C
C    All 3 of these arrays are associated with the same memory pointer.
C
      character*32 cwork(1)
C      pointer (ipwork,cwork)
      pointer (ipwork,iwork)
      pointer (ipwork,rwork)
c      pointer (ipwork,cwork)
      integer      iwork(*)
      real         rwork(*)
C      character*32 cwork(*)
C
C    All 3 of these arrays are associated with the same memory pointer.
C
      character*32 ccmo(1)
c      pointer (iprcmo,ccmo)
      pointer (iprcmo,rcmo)
      pointer (iprcmo,icmo)
C      pointer (iprcmo,ccmo)
      integer      icmo(*)
      real*8       rcmo(*)
C      character*32 ccmo(*)
C
      character*32 cname_isort_key
      character*32 ctype,crank,cattr_name,clength,cio,cpers,cinter
C
      character*32 isubname
      integer i, in, ir, ie, itoff, jtoff, nnodes1, nelem1
      integer iatt, natt, ilen, irank
      integer ikey_min, ikey_max, imin_key, imax_key
      integer itype, itout, lout, ierrw

      integer icharlnf, icharln, iimin, iimax
      integer local_debug
C
C ######################################################################
C
      isubname="reorder_element"
 
      local_debug=0
      if (local_debug.gt.0) call mmverify()
C
C#######################################################################
C
c
c    2 - Get the mesh object name
c
      cmo_name = cmsgin(2)
      if((cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. '-def-'))
     1   then
         call cmo_get_name(cmo_name, ier)
         if(ier.ne.0) then
           write(logmess,9000) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9000   format("REORDER: CMO found bad mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           ierr = -1
           goto 9999
         endif
      endif
C#######################################################################
C
c    3 - Get pointer, length and type of sort key array
C
      if(nwds .lt. 3)then
         logmess = 'REORDER: ERROR, third arugment must be sort key'
           call writloga('default',0,logmess,0,ier)
           ierr = -1
           goto 9999
      endif

      cname_isort_key = cmsgin(3)(1:icharlnf(cmsgin(3)))

      call cmo_get_info(
     1  cname_isort_key,cmo_name,
     2  ipisort_key,
     3  ilen_isort_key,
     4  itype_isort_key,
     5  ier)
c
      call cmo_get_attparam(
     1   cname_isort_key,cmo_name,index,
     2   ctype,
     3   crank,
     4   clength,
     5   cinter,
     6   cpers,
     7   cio,
     8   ier)
C
C#######################################################################
C

      call cmo_get_info('nnodes',cmo_name,nnodes1,ilen,itype,ier)
      call cmo_get_info('nelements',cmo_name,nelem1,ilen,itype,ier)
      if (nelem1.lt.1.or.nnodes1.lt.1) goto 9999

      call cmo_get_info('itetclr',cmo_name,ipitetclr1,ilen,itype,ier)
      call cmo_get_info('itettyp',cmo_name,ipitettyp1,ilen,itype,ier)
      call cmo_get_info('itetoff',cmo_name,ipitetoff1,ilen,itype,ier)
      call cmo_get_info('jtetoff',cmo_name,ipjtetoff1,ilen,itype,ier)
      call cmo_get_info('itet',   cmo_name,ipitet1,   ilen,itype,ier)
      call cmo_get_info('jtet',   cmo_name,ipjtet1,   ilen,itype,ier)
C
      ilen=nelem1
      call mmgetblk('itetclr2',isubname,ipitetclr2,ilen,1,ier)
      call mmgetblk('itettyp2',isubname,ipitettyp2,ilen,1,ier)
      call mmgetblk('itetoff2',isubname,ipitetoff2,ilen,1,ier)
      call mmgetblk('jtetoff2',isubname,ipjtetoff2,ilen,1,ier)
      call mmgetblk('work',    isubname,ipwork ,   ilen,2,ier)
      call mmgetblk('iesave',  isubname,ipiesave , ilen,1,ier)
      call mmgetblk('iechange',isubname,ipiechange,ilen,1,ier)

C      print *,'reorder_element', ifelmhyb, nelem1, nelmnen(ifelmhyb)

      ilen=nelmnen(ifelmhyb)*nelem1
      call mmgetblk('itet2',isubname,ipitet2,ilen,1,ier)
      ilen=nelmnef(ifelmhyb)*nelem1
      call mmgetblk('jtet2',isubname,ipjtet2,ilen,1,ier)
C
C#######################################################################
C
C    Check that sort key is valid
C
C    Check that sort key has lenght nelements
C
      if(ilen_isort_key .ne. nelem1)then
         write(logmess,2115) ilen_isort_key
 2115    format(' REORDER: ERROR ',
     1          ' Sort key vector has length ',i11)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2117)  nelem1
 2117    format(' REORDER: ERROR ',
     2          ' Number of elements in cmo is ', i11)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C#######################################################################
C    Check that sort key is type integer
C
      if(ctype(1:4) .ne. 'VINT')then
         write(logmess,2125)
 2125    format(' REORDER: ERROR ')
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2126) ctype
 2126    format(' REORDER: ERROR Sort key vector is type ',a8)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2127)
 2127    format(' REORDER: ERROR Valid key is integer type = VINT ')
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C#######################################################################
C
C    Check that sort key has min value = 1
C    Check that sort key has max value = nelem1
C
      imin_key = iimin(nelem1, isort_key, 1)
      imax_key = iimax(nelem1, isort_key, 1)

      ikey_min = isort_key(imin_key)
      ikey_max = isort_key(imax_key)

      if((ikey_min .ne. 1) .or. (ikey_max .ne. nelem1))then
         write(logmess,2135)
 2135    format(' REORDER: ERROR ')
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2136) ikey_min
 2136    format(' REORDER: ERROR Sort key vector min value = ',i11)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2137) ikey_max
 2137    format(' REORDER: ERROR Sort key vector max value = ',i11)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2138) nelem1
 2138    format(' REORDER: ERROR Number of elements = ', i11)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C#######################################################################
C
C    Check that sort key does not have any duplicate entries
C    or holes in the permutation vector.
C
C    Use iwork and iesave temporary arrays from now on. Otherwise
C    during the reorder process, the isort_key vector of the MO
C    will be altered and then everything will fall apart.
C
      do i = 1, nelem1
        iechange(i) = 0
        iesave(i) = 0
      enddo

      do i = 1, nelem1
         iesave(i) = isort_key(i)
C         print *,'reorder_element  elem# sort_key(i) ', i, isort_key(i)
      enddo
      do i = 1, nelem1
c         iechange(isort_key(i)) = i
         iechange(i) = isort_key(i)
      enddo
      do i = 1, nelem1
C         print *,'reorder_element elem#   iechange(i) ', i, iechange(i)
      enddo

      ierr = 0
      do i = 1, nelem1
        if(iechange(i) .eq. 0)then
         ierr = -1
          write(logmess,2145) i
 2145     format(' REORDER: ERROR ',
     1          ' No pointer to entry ',i11)
          call writloga('default',0,logmess,0,ierrw)
        endif
      enddo

      if(ierr .ne. 0)then
         go to 9999
      endif
C
C#######################################################################
C
C     We appear to have a valid sort key. Begin to reorder elements.
C
C#######################################################################
C     Reorder default element attributes: itettyp, itetclr, itoff, jtoff
C
      itoff = 0
      jtoff = 0
      do ie = 1,nelem1
         itetclr2(ie)=itetclr1(iechange(ie))
         itettyp2(ie)=itettyp1(iechange(ie))
         itetoff2(ie)=itoff
         jtetoff2(ie)=jtoff
         itoff=itoff+nelmnen(itettyp2(ie))
         jtoff=jtoff+nelmnef(itettyp2(ie))
      enddo
C
C  set itet2 from itet1 values
C
      do ie=1,nelem1
         do in=1,nelmnen(itettyp2(ie))
            itet2(itetoff2(ie)+in)=itet1(itetoff1(iechange(ie))+in)
         enddo
      enddo
C
C  replace "1" info with "2" info
C
      do ie = 1,nelem1
         itetclr1(ie)=itetclr2(ie)
         itettyp1(ie)=itettyp2(ie)
         itetoff1(ie)=itetoff2(ie)
         jtetoff1(ie)=jtetoff2(ie)
         do i=1,nelmnen(itettyp2(ie))
            itet1(itetoff1(ie)+i)=itet2(itetoff2(ie)+i)
         enddo
      enddo
C
C#######################################################################
C
C        REMAP ANY ELEMENT QUANTITIES TO THE NEW ELEMENT ORDERING.
C
C        NOTE: Only implemented for scalar attributes.
C
C
C        LOOP THROUGH MESH OBJECT ATTRIBUTES AND LOOK FOR ELEMENT
C           BASED SCALAR ATTRIBUTES. Note: itetclr, itettyp, itetoff,
C           and jtetoff are special and taken care of in the loops
C           above.
C
  105    call cmo_get_info('number_of_attributes',cmo_name,natt,
     *                   ilen,itout,ier)
         do iatt=1,natt
            call cmo_get_attribute_name(cmo_name,iatt,cattr_name,ier)
            ilen=icharln(cattr_name)
            if(cattr_name(1:ilen).eq.'itetclr' .or.
     *         cattr_name(1:ilen).eq.'itettyp' .or.
     *         cattr_name(1:ilen).eq.'itet'    .or.
     *         cattr_name(1:ilen).eq.'jtet'    .or.
     *         cattr_name(1:ilen).eq.'itetoff' .or.
     *         cattr_name(1:ilen).eq.'jtetoff' ) then
C
C              DO NOTHING
C
            else
               call cmo_get_attparam(cattr_name,cmo_name,index,ctype,
     *           crank,clength,cinter,cpers,cio,ier)
               call cmo_get_info(crank,cmo_name,irank,lout,itout,ier)

               if(ier.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:7).eq.'VDOUBLE') then
                  call cmo_get_info(cattr_name,cmo_name,
     *                              iprcmo,lout,itout,ier)
                  do ir = 1,irank
                  do ie = 1,nelem1
                     rwork(ie)=rcmo(iechange(ie))
                  enddo
                  do ie = 1,nelem1
                     rcmo(ie) = rwork(ie)
                  enddo
                  enddo
               elseif(ier.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:4).eq.'VINT') then
                  call cmo_get_info(cattr_name,cmo_name,
     *                              iprcmo,lout,itout,ier)
                  do ir = 1,irank
                  do ie = 1,nelem1
                     iwork(ie)=icmo(iechange(ie))
                  enddo
                  do ie = 1,nelem1
                     icmo(ie) = iwork(ie)
                  enddo
                  enddo
               elseif(ier.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:4).eq.'VCHAR') then
                  call cmo_get_info(cattr_name,cmo_name,
     *                              iprcmo,lout,itout,ier)
                  do ir = 1,irank
                  do ie = 1,nelem1
                     cwork(ie)=ccmo(iechange(ie))
                  enddo
                  do ie = 1,nelem1
                     ccmo(ie) = cwork(ie)
                  enddo
                  enddo
               endif
            endif
         enddo
C
C#######################################################################
C       Reset the jtet array values
C
      call dotask('geniee; finish', ier)
C
C#######################################################################
C
 9999 continue
C
      call mmrelprt(isubname,ier)
      if (local_debug.gt.0) call mmverify()
C
      return
      end
      subroutine reorder_node(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C
C#######################################################################
C
C      PURPOSE -
C
C
C         FORMAT:    reorder/cmo_name/sort_key
C         FORMAT:    reorder/  -def- /sort_key
C
C         This module will reorder a MO according to a designated
C         permutation vector. The permutation vector can be any
C         integer vector nnodes long with min value = 1, max value = nnodes
C         and no repeated entries. The standard way that the permuation
C         vector might be computed is with the SORT/cmo/index... command.
C
C         sort_key - is the permutation vector - i.e. an integer node
C                    based mesh object attribute
C         This module will reorder all node based attribute arrays,
C         update the isn parent child pointers and update the element
C         connectivity itet array.
C
C      INPUT ARGUMENTS -
C
C         XMSGIN ]
C         MSGIN  ] - Input message
C         IMSGIN ]
C
C         NWDS     - Number of words in the input message
C
C
C      OUTPUT ARGUMENTS -
C
C         IERR1    - Error flag
C
C
C#######################################################################
C
C
      implicit none
C
      include 'local_element.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C#######################################################################
C
      character*132 logmess
C
C
      pointer (ipisn,     isn     )
      integer isn(*)
      pointer (ipitettyp, itettyp )
      integer itettyp(*)
      pointer (ipitetoff, itetoff )
      integer itetoff(*)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(*)
      pointer (ipitet, itet1 )
      integer itet1(*)
      pointer (ipjtet, jtet1 )
      integer jtet1(*)
C
C#######################################################################
C
      pointer(ipisort_key, isort_key)
      pointer(ipisn2   , isn2    )
      pointer(ipisave  , isave   )
      pointer(ipichang , ichange )
      pointer(iphold1  , ihold1  )
      pointer(iphold1  , xhold1  )
      pointer(ipxcmo   , xcmo )
      pointer(ipxcmo   , icmo )
      integer isn2(*),isave(*),ichange(*),ihold1(*),icmo(*)
      integer isort_key(*)
      real*8 xhold1(*),xcmo(*)
C
C#######################################################################
C
      character*40 isubname, cmo
      character*32 ctype,crank,cattr_name,clength
      character*8 cglobal, cdefault, sbname
      character*32 cname_isort_key
      character*32 clen, cinter, cpers, cio
C
C#######################################################################
C
      integer ilen_isort_key, itype_isort_key
      integer ier, ilen, ityp, icscode, nelements, length,
     1        lenxhold, ikey_min, ikey_max, imin_key, imax_key,
     2        i, isort_error, ierr1, ierrw, lout, itout, iatt,
     3        local_debug, icharln, icharlnf, index, mpnt, iseqno
      integer nnodes, nsave, natt, irank, ii, l, it, nlen
      integer iimax, iimin
C
C     ******************************************************************
C
C     Define the memory-management partition.
C
      isubname='reorder'
      cglobal='global'
      cdefault='default'
      sbname='sbcmoatt'
      local_debug=0
      if (local_debug.gt.0) call mmverify()
C
C
C     ******************************************************************
C
C     Preliminaries.
C
C#######################################################################
C
c
c    Decide what to do based on command line strings
c    
c    2 - Get the mesh object
c 
      cmo = cmsgin(2) 
      if((cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. '-def-'))
     1   then
         call cmo_get_name(cmo, icscode)
         if(ier.ne.0) then
           write(logmess,9000) cmsgin(2)(1:icharlnf(cmsgin(2)))
 9000   format("REORDER: CMO found bad mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           goto 9999
         endif
      endif
C#######################################################################
C
c
c    3 - Get pointer, length and type of sort key array
c    
c
C
      cname_isort_key = cmsgin(3)(1:icharlnf(cmsgin(3)))
      
      call cmo_get_info(
     1  cname_isort_key,cmo,
     2  ipisort_key,
     3  ilen_isort_key,
     4  itype_isort_key,
     5  ier)
c
      call cmo_get_attparam(
     1   cname_isort_key,cmo,index,
     2   ctype,
     3   crank,
     4   clen,
     5   cinter,
     6   cpers,
     7   cio,
     8   ier)
C
C#######################################################################
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,icscode)
      call cmo_get_info('nelements',cmo,
     *                   nelements,ilen,ityp,icscode)
C
      call cmo_get_info('isn',cmo,ipisn,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
C
C     Skip element info in the case of nodes with zero elements.
C
      if(nelements .gt. 0)then
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      endif
C
C     ******************************************************************
C        Get work array temporary memory from the memory manager.
C
C        Note that the same memory is used for the real and integer
C        work arrays. That is why hold1 is allocated double precision size.
C
         length=nnodes
         lenxhold=length*10
         call mmgetblk('isn2'    ,isubname,ipisn2  ,length,  1,icscode)
         call mmgetblk('isave'   ,isubname,ipisave ,lenxhold,1,icscode)
         call mmgetblk('ichange' ,isubname,ipichang,length,  1,icscode)
         call mmgetblk('hold1'   ,isubname,iphold1 ,lenxhold,2,icscode)
C
C     ******************************************************************
C
C    Check that sort key is valid
C
C    Check that sort key has lenght nnodes
C
      if(ilen_isort_key .ne. nnodes)then
         write(logmess,2115) ilen_isort_key
 2115    format(' REORDER: ERROR ',
     1          ' Sort key vector has length ',i11)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2117)  nnodes
 2117    format(' REORDER: ERROR ',
     2          ' Number of nodes in cmo is ', i11)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C     ******************************************************************
C    Check that sort key is type integer
C
      if(ctype(1:4) .ne. 'VINT')then
         write(logmess,2125)
 2125    format(' REORDER: ERROR ')
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2126) ctype
 2126    format(' REORDER: ERROR Sort key vector is type ',a8)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2127)
 2127    format(' REORDER: ERROR Valid key is integer type = VINT ')
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C     ******************************************************************
C
C    Check that sort key has min value = 1
C    Check that sort key has max value = nnodes
C
      imin_key = iimin(nnodes, isort_key, 1)
      imax_key = iimax(nnodes, isort_key, 1)
     
      ikey_min = isort_key(imin_key)
      ikey_max = isort_key(imax_key)
     
      if((ikey_min .ne. 1) .or. (ikey_max .ne. nnodes))then
         write(logmess,2135)
 2135    format(' REORDER: ERROR ')
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2136) ikey_min
 2136    format(' REORDER: ERROR Sort key vector min value = ',i11)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2137) ikey_max
 2137    format(' REORDER: ERROR Sort key vector max value = ',i11)
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,2138) nnodes
 2138    format(' REORDER: ERROR Number of nodes = ', i11)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C     ******************************************************************
C
C    Check that sort key does not have any duplicate entries
C    or holes in the permutation vector.
C
      do i = 1, nnodes
        isave(i) = 0
        ichange(i)=0
      enddo
     
      do i = 1, nnodes
        isave(i) = isort_key(i)
      enddo
      do i = 1, nnodes
         ichange(isave(i))=i
      enddo
      
      isort_error = 0
      do i = 1, nnodes
        if(ichange(i) .eq. 0)then
         isort_error = 1
          write(logmess,2145) i
 2145     format(' REORDER: ERROR ',
     1          ' No pointer to entry ',i11)
          call writloga('default',0,logmess,0,ierrw)
        endif
      enddo
      
      if(isort_error .ne. 0)then
         go to 9999
      endif
C
C     ******************************************************************
C
C     We appear to have a valid sort key if we get this far.
C
C        Construct a permutation array called ICHANGE that points to the relative
C        location of a given point in the ISAVE array.  For
C        example, ICHANGE(25)=7 would indicate that point 25 is
C        in location 7 of the ISAVE array.  ICHANGE of points not
C        contained in the ISAVE list would be set to zero.  This array
C        will establish a relation between existing point numbers and
C        the new numbers after resequencing.
C
C     The isave array, which is the sort key, can be created using
C     the sort/cmoname/index... command
C
C
C
C
C     ******************************************************************
C        Correct isn pointers to correspond to new values.
C
      nsave = nnodes
C
         do i=1,nnodes
            isn2(i)=0
         enddo
         do ii=1,nsave
            mpnt=isave(ii)
            iseqno=isn(mpnt)
            if(iseqno.ne.0) then
               isn2(mpnt)=ichange(iseqno)
            endif
         enddo
         do i=1,nnodes
            isn(i)=isn2(i)
         enddo
C
C     ******************************************************************
C        Reorder data from old to new locations.
C
C
C  Loop through all mesh object attributes
C
C
         call cmo_get_info('number_of_attributes',cmo,natt,
     *                   lout,itout,ier)
C
         do iatt=1,natt
            call cmo_get_attribute_name(cmo,iatt,cattr_name,ier)
            nlen=icharln(cattr_name,ier)
            call cmo_get_attparam(cattr_name,cmo,index,ctype,crank,
     *         clength,cinter,cpers,cio,ier)
C
C   Reorder double precision vector array
C
            if(ier.eq.0.and.ctype(1:7).eq.'VDOUBLE'.and.
     *           clength(1:6).eq.'nnodes') then
 
               call cmo_get_info(crank,cmo,irank,lout,itout,ier)
               if (irank*nsave.gt.lenxhold) then
                  lenxhold=irank*nsave+100
                  call mmnewlen('hold1',isubname,iphold1,lenxhold,ier)
               endif
               call cmo_get_info(cattr_name,cmo,ipxcmo,lout,itout,ier)
C
C   isetwd is a special case
C
               if(cattr_name(1:nlen).eq.'isetwd') then
                  do ii=1,nsave
                     do l=1,irank
                        ihold1((ii-1)*irank+l)=
     *                    icmo((isave(ii)-1)*irank+l)
                    enddo
                 enddo
                  do ii=1,nsave
                     do l=1,irank
                        icmo((ii-1)*irank+l)=
     *                   ihold1((ii-1)*irank+l)
                     enddo
                 enddo
C
C   Standard double precision real
C
               else
                  do ii=1,nsave
                     do l=1,irank
                       xhold1((ii-1)*irank+l)=
     *                    xcmo((isave(ii)-1)*irank+l)
                     enddo
                  enddo
                  do ii=1,nsave
                     do l=1,irank
                        xcmo((ii-1)*irank+l)=
     *                    xhold1((ii-1)*irank+l)
                     enddo
                  enddo
               endif
C
C   Reorder integer vector array
C
            elseif(ier.eq.0.and.ctype(1:4).eq.'VINT'.and.
     *              clength(1:6).eq.'nnodes') then
              call cmo_get_info(crank,cmo,irank,lout,itout,ier)
              call cmo_get_info(cattr_name,cmo,ipxcmo,lout,itout,ier)
              if (irank*nsave.gt.lenxhold) then
                  lenxhold=irank*nsave+100
                  call mmnewlen('hold1',isubname,iphold1,lenxhold,ier)
               endif
C
C   Standard integer
C
               do ii=1,nsave
                  do l=1,irank
                     ihold1((ii-1)*irank+l)=
     *                 icmo((isave(ii)-1)*irank+l)
                  enddo
               enddo
               do ii=1,nsave
                  do l=1,irank
                     icmo((ii-1)*irank+l)=
     *                 ihold1((ii-1)*irank+l)
                  enddo
               enddo
            endif
      enddo
C
C     ******************************************************************
C
C     Update the connectivity of all the elements, if elements exist.
C
C        Change ITET values of elements
C
C      print *, 'reorder_node: update connectivity'
C
         if(nelements .gt. 0)then
         do it=1,nelements
            do i=1,nelmnen(itettyp(it))
               itet1(itetoff(it)+i)=ichange(itet1(itetoff(it)+i))
            enddo
         enddo
         endif
C
C     ******************************************************************
C
C     Set the error flag to normal.
C
      ierr1=0
C
C     ******************************************************************
C
C     RESET lower_d_flag IF EXISTS TO INDICATE LOWER D STRUCTURES NOT VALID.
C     (or could fix -> add code ...)
C
      call cmo_get_info('lower_d_flag',cmo,i,lout,itout,ier)
      if ( ier.eq.0 .and. i.eq.1 ) then
         lout=1
         itout=1
         call cmo_set_info('lower_d_flag',cmo,2,lout,itout,ier)
      endif
C
C
C     ******************************************************************
C
C     Set up the usual CFT-immune statement for debugging.
C
      goto 9999
 9999 continue
C
C
C     ******************************************************************
C
C     Release the temporary memory back to the memory manager.
C
      call mmrelprt(isubname,icscode)
C
C
C     ******************************************************************
C
C     ******************************************************************
C
      if (local_debug.gt.0) call mmverify()
      return
      end
