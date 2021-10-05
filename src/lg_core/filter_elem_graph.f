      subroutine filter_elem_graph
     1           (imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
 
C#######################################################################
C
C     PURPOSE -
C        Search a mesh object for duplicate elements. For this
C        implementation, duplicate is defined as having the exact
C        same set of nodes in the element connectivity. Duplicates
C        will have their material color (itetclr) changed to
C        max(itetclr) + 1. The element with the larger itetclr
C        value will be kept. The color of the deleted element
C        can be recovered using the iclr1, iclr2 arrays.
C
C        New element arrays are added to the MO.
C        iclr1 = if not a duplicate, the itetclr value
C              = if a duplicated element, the color of the duplicate
C        iclr2 = if not a duplicate, the itetclr value
C              = if the duplicate element, the color of the duplicate
C
C        The search does not occur over the entire element list. The
C        default is to only look at the 10 elements above and below
C        the test element. The search range can be changed by the
C        user. Setting search_range to a number larger than the number
C        of elements will cause all elements to be searched.
C
C        The algorithm will only detect one duplicate. If there is
C        is more than one it can be found by calling filter/element
C        multiple times.
C
C        In general if you are merging together two meshes and then
C        want to delete duplicate elements the commands might be:
C
C        addmesh / merge / cmohex / cmohex1 / cmohex2
C        createpts / median
C        sort / -def- / index / ascending / ikey / xmed ymed zmed 
C        reorder/ -def- /ikey
C        filter / 1 0 0
C        rmpoint / compress
C        filter / element /  / delete
C
C
C     INPUT ARGUMENTS -
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C
C     FORMAT -
C        filter / element / [search_range] / [delete|nodelete]
C
C        $Log: filter_elem_graph.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   01 Aug 2005 10:08:38   gable
CPVCS    Initial revision.
C
C#######################################################################
c ........................................................................
 
      implicit none
      include 'local_element.h'

C arguments
      integer nwds, ierr, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

C variables

      pointer (ipitettyp,itettyp),(ipitetoff,itetoff),(ipitet,itet)
      pointer (ipitetclr, itetclr)
      integer itettyp(*),itetoff(*),itet(*), itetclr(*)
      pointer (ipiclr1, iclr1)
      integer iclr1(*)
      pointer (ipiclr2, iclr2)
      integer iclr2(*)
      pointer (ipid_seed, id_seed)
      integer id_seed(*)
      pointer (ipitet_sum, itet_sum)
      integer itet_sum(*)

      integer k_id1(8), k_id2(8)
      integer ierrw,nelements,ilen,ityp,it,ioff,ityp2,ioff2
      integer itetclr_max, itetclr_tag, k_id1_sum, it1, it2, j
      integer if_same, i_search_range, if_delete, number_found

      character*10   c_int
      character*32   cmo,isubname
      character*132  logmess
      character*1024 cmdmess
 
C#######################################################################
C BEGIN begin
C

C get some initial info
 
      isubname='filter_elem_graph'
 
      ierr=0
      number_found = 0
c
c  Only search for a single face, and assume they are sorted
c  so they have to be near (in element number space) to one another.
c
c createpts / voronoi
c sort / cmo_all / index / ascending / ikey / xvor yvor zvor 
c reorder/ cmo_all /ikey

C
C     Set search range to default
C
      i_search_range = 10
      if(nwds .ge. 3.and.msgtype(3).eq.1)then
      i_search_range=imsgin(3)
      elseif(nwds .ge. 3.and.msgtype(3).eq.2)then
      i_search_range=nint(xmsgin(3))
      endif
C
C     Set flag to delete or not delete duplicate elements.
C
      if(nwds .ge. 4)then
         if(msgtype(4).eq.3.and.cmsgin(4)(1:6).eq.'delete')then
            if_delete = 1
         elseif(msgtype(4).eq.3.and.
     1          cmsgin(4)(1:8).eq.'nodelete')then
            if_delete = 0
         else
            if_delete = 0
         endif
      endif

      call cmo_get_name(cmo,ierr)
      if (ierr.ne.0) then
         ierr = -1
         goto 9999
      endif
 
      call cmo_get_intinfo('nelements',cmo,nelements,ilen,ityp,ierr)
      if (ierr.ne.0.or.nelements.eq.0) then
         write(logmess,3005)
 3005    format('WARNING: FILTER/ELEMENT Number of elements` = 0 ')
         call writloga('default',0,logmess,0,ierr)
 3006    format('WARNING: FILTER/ELEMENT RETURN no action ')
         call writloga('default',0,logmess,1,ierr)
         ierr = -1
         goto 9999
      endif

      write(logmess,9910) i_search_range
 9910 format('FILTER/ELEMENT: Use search range =',i10)
      call writloga('default',1,logmess,0,ierrw)
 
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname, 'get_info itettyp')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname, 'get_info itetoff')
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname, 'get_info itet')
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname, 'get_info itetclr')

c
c  add attributes if don't exist
c
c     id_seed
c
      call cmo_get_info('id_seed',cmo,ipid_seed,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//id_seed/VINT/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('id_seed',cmo,ipid_seed,ilen,ityp,ierr)
      endif
c
c     itet_sum
c
      call cmo_get_info('itet_sum',cmo,ipitet_sum,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//itet_sum/VINT/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('itet_sum',cmo,ipitet_sum,ilen,ityp,ierr)
      endif
c
c     iclr1
c
      call cmo_get_info('iclr1',cmo,ipiclr1,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//iclr1/VINT/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('iclr1',cmo,ipiclr1,ilen,ityp,ierr)
      endif
c
c     iclr2
c
      call cmo_get_info('iclr2',cmo,ipiclr2,ilen,ityp,ierr)
      if(ierr.ne.0) then
         call dotask
     *    ('cmo/addatt//iclr2/VINT/scalar/nelements///gal/0.;finish',
     *    ierr)
         call cmo_get_info('iclr2',cmo,ipiclr2,ilen,ityp,ierr)
      endif
c
c     Set all arrays to zero.
c
       do it=1,nelements
          id_seed(it) = 0
          itet_sum(it) = 0
          iclr1(it) = 0
          iclr2(it) = 0
       enddo
c       
       itetclr_max = 0
       do it=1,nelements
          ioff=itetoff(it)
          ityp=itettyp(it)
          itetclr_max = max(itetclr(it), itetclr_max)
          k_id1_sum = 0
          do j=1,nelmnen(itettyp(it))
           k_id1(j) = itet(itetoff(it)+j)
           k_id1_sum = k_id1_sum + k_id1(j)
          enddo
c
c         Set element array to the sum of the node numbers of the element
c          
          itet_sum(it) = k_id1_sum
c
c         Set element array to the element number
c          
          id_seed(it) = it
c
c         Set element arrays to the element color (material id)
c          
          iclr1(it) = itetclr(it)
          iclr2(it) = itetclr(it)
       enddo
       
       itetclr_tag =  itetclr_max + 1  
      write(logmess,550)itetclr_max
  550 format('Maximum material id max(itetclr)          = ',i10)
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,551)itetclr_tag
  551 format('Duplicate Elements will be set to itetclr = ',i10)
      call writloga('default',0,logmess,0,ierrw)
c
c  done with setup
c
c
       do it1=1,nelements
          if(itetclr(it1) .ne. itetclr_tag)then
          ioff=itetoff(it1)
          ityp=itettyp(it1)
c
c           get the node id's of the test element
c
          do j=1,nelmnen(itettyp(it1))
            k_id1(j) = itet(itetoff(it1)+j)
          enddo
c
c  Only search for a single face, and assume they are sorted
c  so they have to be near (in element number space) to one another.
c
c createpts / voronoi
c sort / -def- / index / ascending / ikey / xvor yvor zvor 
c reorder/ -def- /ikey
c

          it2 = it1-i_search_range
          if(it2 .lt. 1)it2 = 1
c
c    Assume candidate elements are nearby in element number space
c    so only search i_search_range below and i_search_range above
c    the present (it) element number.
c          
          dowhile((it2 .le. nelements) .and.
     1            (it2 .lt. it1+i_search_range))
            it2 = it2 + 1
            ioff2=itetoff(it2)
            ityp2=itettyp(it2)
            if(itetclr(it1) .ne. itetclr_tag)then
            if(itetclr(it2) .ne. itetclr_tag)then
            if(it1 .ne. it2)then
            if(itet_sum(it1) .eq. itet_sum(it2))then
            if(itettyp(it1) .eq. itettyp(it2))then
c
c           get the node id's of the element to be tested
c
            do j=1,nelmnen(itettyp(it2))
              k_id2(j) = itet(itetoff(it2)+j)
            enddo
c
c       compare the k_id1 values with the k_id2 values
c
            call isort(k_id1,k_id1,nelmnen(itettyp(it2)),1)
            call isort(k_id2,k_id2,nelmnen(itettyp(it2)),1)

            if_same = 0
            do j=1,nelmnen(itettyp(it1))
              if(k_id1(j) .eq. k_id2(j))if_same = if_same + 1
            enddo
            if(if_same .eq. nelmnen(itettyp(it2)))then
                iclr1(it1) = itetclr(it1)
                iclr2(it1) = itetclr(it2)
                iclr1(it2) = itetclr(it2)
                iclr2(it2) = itetclr(it1)
                
                if(itetclr(it1) .ge. itetclr(it2))then
                itetclr(it2) = itetclr_tag
                id_seed(it2) = it1
                id_seed(it1) = -1
                else
                itetclr(it1) = itetclr_tag
                id_seed(it1) = it2
                id_seed(it2) = -1
                endif
c
c               End the search since a duplicate was found.
c
                it2 = nelements + 1
                number_found = number_found + 1
            endif
         endif
         endif
         endif
         endif
         endif
       enddo
       endif
       enddo

      write(logmess,553)nelements
  553 format('FILTER/ELEMENT number elements searched  = ',i10)
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,554)number_found
  554 format('FILTER/ELEMENT number duplicates marked  = ',i10)
      call writloga('default',0,logmess,1,ierrw)

      if(if_delete .eq. 1)then
        write(c_int,560)itetclr_tag
  560   format(i10)
        cmdmess = 'rmmat / ' // c_int // ' / element '// '; finish'
        ierr = 0
        call dotask(cmdmess,ierr)
        cmdmess = 'rmpoint/compress '// '; finish'
        ierr = 0
        call dotask(cmdmess,ierr)
      endif

        cmdmess = 'cmo/DELATT/'//cmo//'/id_seed '// '; finish'
        ierr = 0
        call dotask(cmdmess,ierr)
        cmdmess = 'cmo/DELATT/'//cmo//'/itet_sum '// '; finish'
        ierr = 0
        call dotask(cmdmess,ierr)

 9999 continue

      if(ierr.ne.0) call x3d_error(isubname, 'Exit with error.')

      return
      end
