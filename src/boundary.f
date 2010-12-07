      subroutine boundary(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c        This routine sets the value of an attribute along a boundary
c        It will also reset the icr, icontab relationship if
c        the attr_name argument is icr or icr1.
c        It operates on the current mesh object.
c
c             ..........................................................
c boundary/dirichlet/attr_name/value/SURFACE_LIST
c  1           2         3       4      5-7
c  or
c boundary/dirichlet/attr_name/user_sub_id_field/SURFACE_LIST
c
c where:
c     dirichlet is currently unused
c     attr_name is the name of the attribute to be set
c         if attr_name is 'icr' or 'icr1' then the icr values
c         of nodes on the listed surfaces will be corrected and
c         the icr, icontab, surface relationship will be set
c         in this case the value parameter is ignored.
c     value is a constant, and is the value to which the attribute is to
c     be set
C     user_sub_id_field is a character string passed to the subroutine
C       user_boundary that must be supplied by the user.
c     SURFACE_LIST is one of:
c               -all- (all boundary nodes) (not valid for icr option)
c               surface_name/inclusive (all bndry nodes on surface)
c               surface_name/exclusive (all bndry nodes ONLY on surface)
c               surface_name/          (exclusive is the default)
c               surface_name1/surface_name2/inclusive
c                    (all bndry nodes on the intersection of the surfaces)
c               surface_name1/surface_name2/exclusive
c                    (all bndry nodes ONLY on the intersection of the
c                     surfaces)
c               surface_name1/surface_name2/  (exclusive is the default)
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
C        $Log: boundary.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Wed Apr 05 13:55:52 2000   dcg
CPVCS    use character*32 for geom_name
CPVCS    
CPVCS       Rev 1.3   Wed Feb 02 13:16:00 2000   dcg
CPVCS
CPVCS       Rev 1.7   Wed Jun 16 10:19:48 1999   nnc
CPVCS    Fixed multiple variable declaration.
CPVCS
CPVCS       Rev 1.6   Fri May 14 16:32:50 1999   dcg
CPVCS    add option to repair icr1, icontab, surface
CPVCS    relationship if attribute name is icr
CPVCS
CPVCS       Rev 1.5   Sun Mar 01 13:06:52 1998   dcg
CPVCS    remove test on max number of input arguments.
CPVCS
CPVCS       Rev 1.4   Wed Feb 11 14:50:14 1998   dcg
CPVCS    add links to user supplied command for setting boundary values
CPVCS    the subroutine called is set_user_bounds
CPVCS
CPVCS       Rev 1.3   Wed Feb 04 14:33:24 1998   dcg
CPVCS    allow an arbitrary (not quite only 32) surfaces
CPVCS
CPVCS       Rev 1.2   Wed Jan 14 13:46:30 1998   mcgavran
CPVCS    Cleaned up error returns
CPVCS
CPVCS       Rev 1.1   Wed Jan 14 13:26:16 1998   mcgavran
CPVCS    Fixed '' to be ' ' for AIX
CPVCS
CPVCS       Rev 1.0   Wed Jan 14 12:32:52 1998   mcgavran
CPVCS    Initial revision.
 
      include 'consts.h'
      include 'geom_lg.h'
c
c     Note: get_mo_attr is in math.f
c
c
      character*132 logmess
c
      pointer(ipxfield,xfield)
      real*8 xfield(1000000)
      pointer(ipxfield,ifield)
      integer ifield(1000000)
      pointer (ipicr1, icr1)
      integer icr1(1000000)
      pointer (ipicontab, icontab)
      integer icontab(50,1000000)
      pointer (ipubndpts,ubndpts)
      integer ubndpts(*)
      pointer (ipisurf,isurf)
      integer isurf(*)
      pointer (ipisurfs,isurfs)
      integer isurfs(*)
      pointer (ipsurfnum,surfnum)
      integer surfnum(*)
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
 
c
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
      integer i, j, k, is, len1, len2
      integer maxsrfind
      integer nnodes,len,type,ifind,itst,nconbnd,ncon50,iout,lout,itype,
     * nfound,ninc,nicontab,index,ierror_return,length
c
      character*32 isubname,idfield,iword1,iword2,ctype,crank,
     *clen,cinter,cpers,cio
      character*32 itest,geom_name
 
      character*5 state
C
      character*32 attr,cmo
      integer icscode,ierrw,nvalue,
     * ilen,nubndpts,nicr,lenattrname,
     * irank,
     * ityp,idum,nsurfmatch,isurflg
      integer icharlnf
      real*8 value,rout
      pointer(ipout,out)
      real*8 out(*)
      real*8 epsln
c
c     ******************************************************************
c
c     set the memory management path name to be the subroutine name.
c
      isubname='boundary'
      ierror=0
      idfield=' '
      nubndpts=0
C
c
C  Translate the operation
      if (nwds .lt. 5) then
             write(logmess,'(a)')
     *       'Not enough arguments to BOUNDARY'
             call writloga('default',0,logmess,0,ierrw)
	     goto 9999
      endif
      len=icharlnf(cmsgin(2))
      if(cmsgin(2)(1:len).ne.'dirichlet') then
             write(logmess,'(a)')
     *       'Second argument must be "dirichlet"'
             call writloga('default',0,logmess,0,ierrw)
	     goto 9999
      endif
c
c  get attribute information
c
      lenattrname=icharlnf(cmsgin(3))
      attr=cmsgin(3)(1:lenattrname)
      call cmo_get_name(cmo,ierror)
      if (ierror.ne.0) then
            write(logmess,'(a)') 'CMO found bad mesh object in BOUNDARY'
            call writloga('default',0,logmess,0,ierror)
	    goto 9999
      endif
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
 
      call cmo_get_info('nnodes',cmo, nnodes, len, type, ierror)
      if ((nnodes.eq.0) .or. (ierror .ne. 0)) then
	 write(logmess, '(a,a)') 'nnodes = 0 in subroutine BOUNDARY'
	 call writloga('default',0,logmess,0,ierror)
	 goto 9999
      endif
      call cmo_get_attparam(attr,cmo,index,ctype,crank,
     *    clen,cinter,cpers,cio,ierror_return)
      call cmo_get_length(attr,cmo,length,irank,
     *                          ierror_return)
      call cmo_get_info(attr,cmo,ipxfield,ilen,ityp,icscode)
c
c     Check attributes for length and type; nnodes and vector
c
      if (clen.ne.'nnodes') then
	  ierror = 1
          write(logmess,'(a)') 'BOUNDARY: length must be nnodes'
          call writloga('default',0,logmess,0,ierror)
	  go to 9999
      endif
      if (ctype.ne.'VINT'.and.ctype.ne.'VDOUBLE') then
	  ierror = 1
          write(logmess,'(a)') 'BOUNDARY: type must be a vector'
          call writloga('default',0,logmess,0,ierror)
	  go to 9999
      endif
 
c
c  get value
C  or if this argument is a character string values will be set by
C  a user subroutine - this character string will be passed to the user
C  subroutine and can be associated with the set of surfaces selected.
c
      if (msgtype(4).eq.1) then
	      nvalue=imsgin(4)
	      value=float(imsgin(4))
      elseif (msgtype(4).eq.3) then
              idfield = cmsgin(4)
              call mmgetblk('ubnd_pts',isubname,ipubndpts,nnodes,1,
     *             icscode)
      else
	      nvalue=nint(xmsgin(4))
	      value=xmsgin(4)
      endif
c
c  get inclusive/ exclusive
c
      nsurfmatch=nwds-4
      len = icharlnf(cmsgin(nwds))
      state = 'excl'
      if (cmsgin(nwds)(1:len) .eq. 'inclusive') then
          state = 'incl'
          nsurfmatch=nsurfmatch-1
      elseif (cmsgin(nwds)(1:len) .eq. 'exclusive') then
          nsurfmatch=nsurfmatch-1
      endif
c
c  get surface list
c
      call mmfindbk('csall',geom_name,ipcsall,len,ierror)
      call mmfindbk('istype',geom_name,ipistype,len,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,len,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,len,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,len,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,len,ierror)
      isurflg=0
      call mmgetblk('surfnum',isubname,ipsurfnum,nsurf,
     * 1,icscode)
      if (nsurfmatch.eq.1. and. cmsgin(5)(1:5).eq.'-all-') then
         if(attr(1:lenattrname).eq.'icr'.or.
     *      attr(1:lenattrname).eq.'icr1') then
	    ierror = 1
            write(logmess,'(2a)') 'BOUNDARY: -all- surface not ',
     *             'permitted with icr attribute'
            call writloga('default',0,logmess,0,ierror)
	    go to 9999
         endif
         do j=1,nsurf
            surfnum(j)=j
            isurflg=isurflg+2**j
         enddo
         state='incl'
      else
         do i=1,nsurfmatch
            do j=1,nsurf
               len = icharlnf(cmsgin(4+i))
               len1 = icharlnf(csall(j))
               if (cmsgin(4+i)(1:len) .eq.csall(j)(1:len1)) then
                  surfnum(i)=j
                  isurflg=isurflg+2**j
                  go to 10
               endif
	    enddo
            write(logmess,8) cmsgin(4+i)
  8         format ('Cannot find surface' ,a32)
            call writloga('default',0,logmess,0,ierror)
 10         continue
         enddo
      endif
c
c     Get info needed for setting the values (xfield, ifield, etc.)
c
      call cmo_get_info('icr1',cmo,ipicr1,idum,idum,ierror)
      if (ierror.ne.0) then
          write(logmess,'(a)') 'Cannot find icr1 in BOUNDARY'
          call writloga('default',0,logmess,0,ierror)
          goto 9999
      endif
      call cmo_get_info('icontab',cmo,ipicontab,idum,idum,ierror)
      if (ierror.ne.0) then
          write(logmess,'(a)') 'Cannot find icontab in BOUNDARY'
          call writloga('default',0,logmess,0,ierror)
          goto 9999
      endif
      call cmo_get_info('nconbnd',cmo,nconbnd,idum,idum,ierror)
      if(ierror.ne.0) call x3d_error('boundary','cmo_get_info')
      call cmo_get_info('ncon50',cmo,ncon50,idum,idum,ierror)
      if(ierror.ne.0) call x3d_error('boundary','cmo_get_info')
c
c  If attribute name is icr or icr1 this is a special case
c  the intention is to reset the icr values - so
c  we call surftstv to see if nodes are on surface(s) specified
c  Later we will fix up an icontab entry for combined surfaces
c
      if(attr(1:lenattrname).eq.'icr'.or.
     *   attr(1:lenattrname).eq.'icr1') then
        call get_epsilon('epsilonl',epsln)
c
c  get temp memory
c
        call mmgetblk('isurf',isubname,ipisurf,nnodes,1,icscode)
        call mmgetblk('isurfs',isubname,ipisurfs,nnodes,1,icscode)
        do i=1,nnodes
          isurf(i)=0
        enddo
        call cmo_get_info('xic',cmo,ipxic, len, type, ierror)
        call cmo_get_info('yic',cmo,ipyic, len, type, ierror)
        call cmo_get_info('zic',cmo,ipzic, len, type, ierror)
c
C      LOOP THROUGH surfaces TO FIND MATCHING SURFACE POINTERS.
C       THEN TEST WITH SURFTST.
C
        do i=1,nsurfmatch
          do  j=1,nsurf
            iword1=csall(surfnum(i))
            iword2=csall(j)
            len1=icharlnf(iword1)
            len2=icharlnf(iword2)
            len1=max(len1,len2)
            if (iword1(1:len1) .eq. iword2(1:len1)) is=j
          enddo
          itest='eq'
          call surftstv(xic,yic,zic,nnodes,epsln,cmo,
     &       istype(is),surfparam(offsparam(is)+1),sheetnm(is),
     &       itest,isurf)
          do j=1,nnodes
            if(i.eq.1) then
              isurfs(j)=isurf(j)
            else
              isurfs(j)=iand(isurf(j),isurfs(j))
            endif
          enddo
        enddo
c
c  now set icr value based on surface membership
c  icr value must index the correct icontab entry
c  first check if icontab entry exists for this combination
c  if not create one
c
        do i=1,nconbnd
          if(icontab(1,i).eq.nsurfmatch) then
             nfound=0
             do j=1,nsurfmatch
               nicontab=icontab(1,i)
               do k=1,nicontab
                  if(surfnum(j).eq.icontab(k+2,i)) nfound=nfound+1
               enddo
             enddo
             if(nfound.eq.nsurfmatch) then
c
c  found an existing icontab entry for this combination
c  save index for setting icr attribute later
c
               nicr=i
               go to 9998
             endif
           endif
        enddo
c
c  no existing entry - must add one - check if enough
c  space first
c
        if(nconbnd.ge.ncon50/50) then
           call cmo_set_info('ncon50',cmo,(nconbnd+9)*50,1,1
     *                                     ,ierror)
           ninc=(nconbnd+9)*50-ncon50
           call mmincblk('icontab',cmo,ipicontab,ninc,ierror)
           do k=nconbnd,nconbnd+9
              do j=1,50
                 icontab(j,k)=0
              enddo
           enddo
        endif
        nconbnd=nconbnd+1
        nicr=nconbnd
        icontab(1,nconbnd)=nsurfmatch
        icontab(2,nconbnd)=max(0,3-nsurfmatch)
        do k=1,nsurfmatch
           icontab(2+k,nconbnd)=surfnum(k)
        enddo
c
c  set icr attribute value for nodes that are on this
c  surface combination
c
 9998   do i=1,nnodes
           if(isurfs(i).eq.1) icr1(i)=nicr
        enddo
        call cmo_set_info('nconbnd',cmo,nconbnd,1,1,ierror)
        go to 9999
      endif
c
c     Normal path - attribute is not icr - use icr and
c     icontab to determine nodes that are associated
c     with the requested surface combination -
c     note there is no test that nodes are actually on
c     the surfaces - only that at some time the icr, icontab,
c     surface relationship was established - this is
c     useful for applications in which interfaces and
c     boundaries move, but the nodes should still be
c     identified as being in the same 'set'
c     Loop through nodes, If the node is to be changed, change it
c
      do i=1,nnodes
	  k = icr1(i)
	  if (k .eq. 0) goto 100
	  maxsrfind = 2+icontab(1,k)
          ifind=0
	  do j = 3, maxsrfind
             itst=2**icontab(j,k)
             if(iand(itst,isurflg).ne.0) then
                ifind=ifind+1
             endif
          enddo
C
c	  set attribute (check excl/incl)
c
          if (state .eq. 'incl'.and.ifind.ne.0) then
              if(idfield.ne.' ') then
                      nubndpts=nubndpts+1
                      ubndpts(nubndpts)=i
              else
                      call set_attr(ipxfield,ctype,
     *                              irank,i,value,nvalue,ierror)
		      if (ierror .ne. 0) goto 9999
              endif
	  else if (state .eq. 'excl'.and.ifind.eq.nsurfmatch) then
              if(idfield.ne.' ') then
                      nubndpts=nubndpts+1
                      ubndpts(nubndpts)=i
              else
                      call set_attr(ipxfield,ctype,
     *                              irank,i,value,nvalue,ierror)
		      if (ierror .ne. 0) goto 9999
              endif
	  endif
 100      continue
        enddo
        if (idfield.ne.' ') call set_user_bounds(nubndpts,ubndpts,cmo
     *       ,ipxfield,idfield)
c     ******************************************************************
c
c     release any local memory allocated for this routine.
c     also error returns transfer to this statement
c
 9999 call mmrelprt(isubname,icscode)
c
      return
      end
c
C     ******************************************************************
      subroutine set_attr(ipxfield,ctype,irank,
     *                    node,value,nvalue,error)
 
      pointer(ipxfield,xfield)
      real*8 xfield(1000000)
      integer irank, node, nvalue, error

      integer k
      character*32 ctype
      real*8 value

      error = 0
      if (ctype.eq.'VINT') then
c         operate on ifield
         call set_attr_int(ipxfield, irank, node, nvalue, error)
      else
c         operate on xfield
         call set_attr_real(ipxfield, irank, node, value, error)
      endif
 
      return
      end

C     ******************************************************************
C     routines to copy real or integer data to existing work array
C     needed for compilers that do not like the re assignment ipxfield
C     Error: Cannot change attributes of symbol at (1) after it has been used
C
      subroutine set_attr_int(ipxfield, irank, node, nvalue, error)
      pointer(ipxfield,ifield)
      integer ifield(1000000)
      integer irank, node, nvalue, error

      integer k

      error = 0
      do k = 1, irank
         ifield(k+irank*(node-1)) = nvalue
      enddo
      return
      end

      subroutine set_attr_real(ipxfield, irank, node, value, error)
      pointer(ipxfield,xfield)
      real*8 xfield(1000000)
      integer irank, node, error
      real*8 value

      integer k

      error = 0
      do k = 1, irank
         xfield(k+irank*(node-1)) = value
      enddo
      return
      end


