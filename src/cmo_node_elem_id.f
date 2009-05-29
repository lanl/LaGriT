      subroutine cmo_node_elem_id
     1           (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c        this routine creates integer arrays with the node and/or
c        element numbers.
c
c             ..........................................................
c     syntax -
c
c  cmo / set_id / [cmoname] / [node|element|both] / [attribute_name_node] / [attribute_name_elem]
c
c  Default cmoname             = current active cmo
c  Default action              = both node and element
c  Default attribute_name_node = ialias
c  Default attribute_name_elem = id_elem
c
c  USAGE:
c
c  cmo / set_id / cmoname / node
c  cmo / set_id / cmoname / element
c  cmo / set_id / cmoname / both
c  cmo / set_id / cmoname / node    / attribute_name_node
c  cmo / set_id / cmoname / element / attribute_name_elem
c  cmo / set_id / cmoname / both    / attribute_name_node / attribute_name_elem
c
c     input arguments -
c
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
c
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
c     change history -
C
C        $Log: cmo_node_elem_id.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   10 Apr 2001 11:09:24   dcg
CPVCS    use logical variables correctly
CPVCS    
CPVCS       Rev 1.0   06 Oct 2000 11:13:10   gable
CPVCS    Initial revision.
C
c
c#######################################################################
c
      integer nwds, imsgin(nwds), msgtype(nwds), ierror
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      pointer(ip_id_node,id_node)
      integer id_node(*)
      pointer(ip_id_elem,id_elem)
      integer id_elem(*)
C
      integer nnodes, nelements
      integer length, itype
      integer length_cmo_name
c
      logical iset_node, iset_elem, iset_both
      integer i, icharlnf, ierrw,ierr
      integer ijob
c
      character*32 attnam_node, attnam_elem
      character*32 isubname
 
      character*32 cmo
      character*132 logmess
      character*256 cmdmess
 
      character*30  def_node_id
      character*30  def_elem_id
      data def_node_id / 'ialias' /
      data def_elem_id / 'id_elem' /
c
c
c#######################################################################
c
C     ******************************************************************
c     set the memory management path name to be the subroutine name.
 
      isubname  = 'cmo_node_elem_id'
 
c
c     ******************************************************************
c
C  Parse the required commands
c
C     4 - find out if setting nodes, elements or both
c
      if(nwds .le. 3)then
         iset_node=.true.
         iset_elem=.true.
         iset_both=.true.
         ijob = 3
 
      else
 
      if(cmsgin(4)(1:4).eq.'node') then
         iset_node=.true.
         iset_elem=.false.
         iset_both=.false.
         ijob = 1
      elseif(cmsgin(4)(1:7).eq.'element') then
         iset_node=.false.
         iset_elem=.true.
         iset_both=.false.
         ijob = 2
      elseif(cmsgin(4)(1:4).eq.'both') then
         iset_node=.true.
         iset_elem=.true.
         iset_both=.true.
         ijob = 3
      elseif(cmsgin(4)(1:5).eq.'-def-') then
         iset_node=.true.
         iset_elem=.true.
         iset_both=.true.
         ijob = 3
      else
         write(logmess,'(a,a)') ' CMO illegal option ',cmsgin(2)
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
 
      endif
c
c     3 - get mesh object name
c
      if (nwds .eq. 2) then
           call cmo_get_name(cmo,ierror)
 
      elseif(nwds .gt. 2) then
 
      if (msgtype(3).eq.3) then
        length_cmo_name=icharlnf(cmsgin(3))
        cmo=cmsgin(3)(1:length_cmo_name)
        if (cmo(1:length_cmo_name).eq.'-def-') then
           call cmo_get_name(cmo,ierror)
           if(ierror.ne.0) then
             write(logmess,'(a)') 'CMO found bad mesh object'
             call writloga('default',0,logmess,0,ierrw)
             go to 9999
           endif
        endif
      else
         ierr=1
         goto 9999
      endif
 
      endif
 
c     set ntets and nnodes
c     check that there are points -- if no nodes, return
c                                    if no elements, be sure not to try and set them
c
      call cmo_get_info('nnodes',cmo,nnodes,length,itype,ierror)
      if(nnodes.eq.0) then
        write(logmess,'(a,a)')
     1      'nnodes = 0 in subroutine cmo_node_elem_id'
        call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
      call cmo_get_info('nelements',cmo,nelements,length,itype,ierr)
      if((nelements.eq.0) .and. (iset_elem)) then
        write(logmess,'(a)') 'Warning: nelements = 0 '
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a)') 'Warning: Element id will not be set. '
        call writloga('default',0,logmess,0,ierrw)
        iset_elem = .false.
      endif
 
c     5 - get attribute names or set attribute names
      if (nwds .le. 4)then
          attnam_node    = def_node_id
          attnam_elem    = def_elem_id
      endif
      if (nwds .gt. 4)then
c
      if((ijob .eq. 1) .or. (ijob .eq. 3))then
       if (msgtype(5).eq.3) then
         if(cmsgin(5)(1:5) .eq. '-def-')then
            attnam_node    = def_node_id
         else
            attnam_node    = cmsgin(5)
         endif
       else
         write(logmess,'(a)')
     1         'Warning: Attribute name must by type=character. '
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a)') 'Warning: Using default '
         call writloga('default',0,logmess,0,ierrw)
         attnam_node    = def_node_id
       endif
      elseif(ijob .eq. 2)then
       if (msgtype(5).eq.3) then
         if(cmsgin(5)(1:5) .eq. '-def-')then
            attnam_elem    = def_elem_id
         else
            attnam_elem    = cmsgin(5)
         endif
       else
         write(logmess,'(a)')
     1         'Warning: Attribute name must by type=character. '
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a)') 'Warning: Using default '
         call writloga('default',0,logmess,0,ierrw)
         attnam_node    = def_elem_id
       endif
      endif
      endif
 
      if (nwds .gt. 5)then
        if (msgtype(6).eq.3) then
           if(cmsgin(6)(1:5) .eq. '-def-')then
              attnam_elem    = def_elem_id
           else
              attnam_elem    = cmsgin(6)
           endif
        else
         write(logmess,'(a)')
     1     'Warning: Attribute name must by type=character. '
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a)') 'Warning: Using defaults, id_elem '
         call writloga('default',0,logmess,0,ierrw)
         attnam_elem    = def_elem_id
        endif
      endif
c
c     END Parse commands
c     ******************************************************************
c
C
C     *****************************************************************
C     Check to see if the necessary attributes exist in the output
C     MO, if they don't, create them.
C
      if(iset_node)then
      call cmo_get_info(attnam_node,cmo,ip_id_node,length,itype,ierr)
      if(ierr.eq.0)then
c
c     Make sure IO for ag is turned on since this might use the ialias array
c     which defaults to just l
         write(cmdmess,10)
     &        'cmo/modatt/',
     &         cmo,
     &         attnam_node,
     &        '/ioflag/agltn; finish'
  8      format(A,A,A,A)
         call dotaskx3d(cmdmess,ierror)
      else
         write(cmdmess,10)
     &        'cmo/addatt/',
     &         cmo,
     &         attnam_node,
     &        '/VINT/scalar/nnodes/linear/permanent/   /0; finish'
 10      format(A,A,A,A)
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if (ierror.ne.0) goto 9999
      call cmo_get_info(attnam_node,cmo,ip_id_node,length,itype,ierr)
      endif
      endif
 
      if(iset_elem)then
      call cmo_get_info(attnam_elem,cmo,ip_id_elem,length,itype,ierr)
      if(ierr.ne.0) then
         write(cmdmess,20)
     &        'cmo/addatt/',
     &         cmo,
     &         attnam_elem,
     &        '/VINT/scalar/nelements/linear/permanent/   /0; finish'
 20      format(A,A,A,A)
         ierror = 0
         call dotaskx3d(cmdmess,ierror)
         if (ierror.ne.0) goto 9999
      call cmo_get_info(attnam_elem,cmo,ip_id_elem,length,itype,ierr)
      endif
      endif
C
C     *****************************************************************
c
c     Now loop through and set node and element numbers
c
      if(iset_node)then
         do i = 1, nnodes
            id_node(i) = i
         enddo
      endif
c
      if(iset_elem)then
         do i = 1, nelements
            id_elem(i) = i
         enddo
      endif
c
c     error returns transfer to this statement 9999
 9999 continue
c
      return
      end
