C
      subroutine elmtestd(cmo,nwrite,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine checks the "jtet" array for internal consistency
C        on general meshes (compare old subroutine tettestd below).
C        It calls the subroutine subelmtestd_lg below
C
C     INPUT ARGUMENTS -
C
C        cmo - name of mesh object
C
C     OUTPUT ARGUMENTS -
C
C        ierror - error flag: nonzero on return if error
C
C     CHANGE HISTORY -
C
C        $Log: tettestd.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.25   05 Jan 2001 12:57:24   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.24   05 May 2000 09:05:40   jtg
CPVCS    nwrite added as argument to elmtestd,sub_elmtestd
CPVCS    if nwrite<0, idebug is set to 1 (if less) so that
CPVCS    nonstandard jtet and inconsistency info spit to screen
CPVCS    for up to |nwrite| elements
CPVCS
CPVCS       Rev 1.23   03 May 2000 12:05:26   jtg
CPVCS    if idebug>0 in sub_elmtestd, more info is spit out
CPVCS    for non-standard jtet
CPVCS
CPVCS       Rev 1.22   Thu Apr 06 14:25:22 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.21   28 Jan 2000 14:43:00   jtg
CPVCS    error message prints nicer
CPVCS
CPVCS       Rev 1.20   26 Jan 2000 13:04:18   jtg
CPVCS    when jtet_reduce_nnd not= 1, add n_diff_nnd to nincons
CPVCS
CPVCS       Rev 1.19   24 Jan 2000 15:13:28   jtg
CPVCS    modifed to use jtet_cycle_max to limit jtet loops
CPVCS    and to handle elements with repeated nodes if jtet_reduce_nnd=1.
CPVCS    Also now writes to default output if there are faces with
CPVCS    repeated nodes, and if repeated faces within an element.
CPVCS
CPVCS       Rev 1.18   Wed Dec 15 13:33:02 1999   dcg
CPVCS    fix bug in calling sequence
CPVCS
CPVCS       Rev 1.17   Tue Nov 30 17:14:36 1999   jtg
CPVCS    made consistent with networks (jtet loops), and if
CPVCS    mbndry=0 uses the convention jtet<0 for interfaces.
CPVCS
CPVCS       Rev 1.16   Wed Nov 10 09:18:52 1999   dcg
CPVCS    remove references to icdname
CPVCS
CPVCS       Rev 1.15   Fri Sep 19 14:30:58 1997   gable
CPVCS    Changed inconsistencies message from fatal to warning.
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 17:04:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   Tue Mar 04 16:46:44 1997   tam
CPVCS     added idebug for level of output
CPVCS
CPVCS       Rev 1.12   Wed Jul 24 17:35:26 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.11   Wed Jun 19 10:17:36 1996   het
CPVCS    We have a fatal error in the JTET list for hybrid grids. No fix yet.
CPVCS
CPVCS       Rev 1.10   Wed May 22 07:01:26 1996   het
CPVCS    Write out the inconsistant tet list to the output file.
CPVCS
CPVCS       Rev 1.9   Tue Jan 23 09:27:08 1996   het
CPVCS    Correct the jtet1 test for hybrid grids.
CPVCS
CPVCS       Rev 1.8   12/05/95 08:20:54   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.7   08/22/95 06:52:20   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.6   06/19/95 16:37:24   dcg
CPVCS    IBM platform changes
CPVCS
CPVCS       Rev 1.5   06/08/95 03:56:40   het
CPVCS    Fix a print format problem
CPVCS
CPVCS       Rev 1.4   06/05/95 10:36:12   het
CPVCS    Make changes for hybrid_grids
C
C ######################################################################
C
      implicit none
C
C args
      character*(*) cmo
      integer nwrite, ierror
C
C ######################################################################
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(*), itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*)
      integer jtet1(*)
 
      pointer (ipisn1,isn1),(ipitp1,itp1),(ipiparent,iparent)
      integer isn1(*),itp1(*),iparent(*)
C
      character*32 isubname
 
      integer jtet_cycle_max, mbndry, itetcnt, nefcmo
      integer len, icmotype, ier, idebug, nincons, jtet_reduce_nnd
     &     ,nnodes
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
C
      call cmo_get_info('mbndry',cmo,mbndry,len,icmotype,ierror)
      if (ierror.ne.0) mbndry=0
      call cmo_get_info('nelements',cmo,itetcnt,len,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,nefcmo,len,icmotype,
     *            ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,len,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,len,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,len,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,len,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,len,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,len,icmotype,ierror)
C
      call cmo_get_info('idebug',cmo,idebug,len,icmotype,ierror)
C
C     ******************************************************************
C     DO THE WORK IN THE sub_elmtestd_lg SUBROUTINE
 
      call cmo_get_info('jtet_cycle_max',cmo,jtet_cycle_max
     &                 ,len,icmotype,ierror)
      if (ierror.ne.0.or.jtet_cycle_max.lt.2) jtet_cycle_max=2
      call cmo_get_info('jtet_reduce_nnd',cmo,jtet_reduce_nnd
     &                 ,len,icmotype,ierror)
      if (ierror.ne.0) jtet_reduce_nnd=0
 
      call cmo_get_info('nnodes',cmo,nnodes,len,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,len,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,len,icmotype,ier)
      isubname='elmtestd_lg'
      call mmggetbk('iparent',isubname,ipiparent,nnodes,1,ier)
      call unpackpc(nnodes,itp1,isn1,iparent)
 
      ierror=-1
      nincons=0
 
      call sub_elmtestd_lg(mbndry,itetcnt,nefcmo
     &       ,ipitettyp,ipjtetoff,ipjtet
     &       ,ipitetoff,ipitet,ipiparent
     &       ,jtet_cycle_max,jtet_reduce_nnd,idebug,nwrite,nincons)
 
      call mmrelblk('iparent',isubname,ipiparent,ier)
C
C     ******************************************************************
C     RETURN
C
 
 9000 if(nincons.eq.0) then
         ierror=0
      elseif(nincons.ge.1) then
         ierror=-nincons
c$$$         ! message already written by sub_elmtestd_lg
c$$$         write(logdan,1000) nincons
c$$$ 1000    format(' WARNING -- ',i6,' ELM NEIGHBOR INCONSISTENCIES')
c$$$         call writloga('default',2,logdan,2,ier)
      endif
 
      goto 9999
 9999 continue
      return
      end
C #####################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C
C #####################################################################

      subroutine sub_elmtestd_lg(mbndry,itetcnt,nefcmo
     &      ,ipitettyp,ipjtetoff,ipjtet
     &      ,ipitetoff,ipitet,ipiparent
     &      ,jtet_cycle_max,jtet_reduce_nnd
     &      ,idebug_in,nwrite_in,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine checks the "jtet" array for internal consistency
C        on networks (arguments passed re using d1 vs d0 ,etc,
C        on lower_d structures)
C
C     INPUT ARGUMENTS -
C
C        standard pointers
C        jtet_cycle_max: max allowed length of the jtet cycle
C        jtet_reduce_nnd: flag to indicate if self-degenerate
C                faces with different numbers of nodes but
C                the same unique node list can touch
C        nwrite_in: the number of elements with nonstandard jtet to print
C        idebug_in: verbosity level
C
C     OUTPUT ARGUMENTS -
C
C        ierror
C
C     CHANGE HISTORY -
C
C        $Log:
C
C ######################################################################
C
      implicit none
C
C
C ######################################################################
C
      include "local_element.h"

C args for sub_elmtestd_lg(mbndry,itetcnt,nefcmo
C           ,ipitettyp,ipjtetoff,ipjtet
C           ,ipitetoff,ipitet,ipiparent
C           ,jtet_cycle_max,jtet_reduce_nnd
C           ,idebug_in,nwrite_in,ierror)

      integer mbndry, itetcnt, nefcmo, 
     &        jtet_cycle_max, jtet_reduce_nnd, 
     &        idebug_in,nwrite_in,ierror
      pointer (ipitettyp, itettyp)
      pointer (ipjtetoff, jtetoff)
      pointer (ipjtet, jtet1)
      pointer (ipitetoff, itetoff)
      pointer (ipitet, itet)
      pointer (ipiparent, iparent)
      integer itettyp(*) ,jtetoff(*) ,jtet1(*)
     &       ,itetoff(*) ,itet(*) ,iparent(*)

C
      integer nincons, n_diff_nnd, idebug
     &      , nwrite, iwrite, it, nef, iface, j0, j1, jt
     &      , jface, ierrwrt, icycle, ncycles, ierr, loc_node(2*maxnen)
     &      , n_self_degen, n_degen, ityp, ioff, joff, j2, jf, nnd
     &      , ifsum, j, i, isort, kf, isum, jtyp, nnd2

      character*132 logdan
C
C ######################################################################
C
C     ******************************************************************
C     SET THE WRITE FLAG
C
      nwrite=nwrite_in  ! 20
      idebug=idebug_in
      if (nwrite_in.lt.0) then
         nwrite=-nwrite
         if (idebug.lt.1) idebug=1
      endif
C
      if(idebug.gt.1) nwrite = itetcnt*maxnef
 
C     ******************************************************************
C
      ierror=-1
 
c ..... (do diagnostics jtet loops complete, set of nodes same) .....
c (only need parent for checking set of nodes same)
 
      iwrite=0
      nincons=0
      n_diff_nnd=0
      ncycles=0
      do it=1,itetcnt
         ityp=itettyp(it)
         ioff=itetoff(it)
         nef=nelmnef(ityp)
         do iface=1,nef
            nnd=ielmface0(iface,ityp)
            do j=1,nnd
               loc_node(j)=iparent(itet(ioff+ielmface1(j,iface,ityp)))
            enddo
330         isort=0
            do j=1,nnd-1
               if (loc_node(j+1).lt.loc_node(j)) then
                  isort=1
                  i=loc_node(j+1)
                  loc_node(j+1)=loc_node(j)
                  loc_node(j)=i
               endif
            enddo
            if (isort.ne.0) goto 330
            j0=nefcmo*(it-1)+iface
            j1=jtet1(jtetoff(it)+iface)
            if (j1.eq.mbndry) then
               continue
            elseif ( (mbndry.eq.0.and.j1.gt.0)
     &              .or. (mbndry.gt.itetcnt.and.j1.lt.mbndry) ) then
               jt=1+(j1-1)/nefcmo
               jface=j1-nefcmo*(jt-1)
               jtyp=itettyp(jt)
               joff=itetoff(jt)
               nnd2=ielmface0(jface,jtyp)
               if (nnd2.ne.nnd) then
                  n_diff_nnd=n_diff_nnd+1
                  if (idebug.gt.0.and.n_diff_nnd.le.nwrite) then
                     write(logdan,*) 'ELMTESTD WARNING:'
     &                     //' element,face ',it,iface
     &                     ,' increments n_diff_nnd'
                     call writloga('default',0,logdan,0,ierrwrt)
                  endif
               else
                  ! test that same
                  do j=1,nnd2
                     loc_node(nnd+j)
     &                 =iparent(itet(joff+ielmface1(j,jface,jtyp)))
                  enddo
331               isort=0
                  do j=nnd+1,nnd+nnd2-1
                     if (loc_node(j+1).lt.loc_node(j)) then
                        isort=1
                        i=loc_node(j+1)
                        loc_node(j+1)=loc_node(j)
                        loc_node(j)=i
                     endif
                  enddo
                  if (isort.ne.0) goto 331
                  do j=1,nnd
                     if (loc_node(j).ne.loc_node(j+nnd)) then
                         n_diff_nnd=n_diff_nnd+1
                         if (idebug.gt.0.and.n_diff_nnd.le.nwrite) then
                            write(logdan,*) 'ELMTESTD WARNING:'
     &                           //' element,face ',it,iface
     &                           ,' increments n_diff_nnd'
                            call writloga('default',0,logdan,0,ierrwrt)
                         endif
                         goto 3311
                     endif
                  enddo
3311              continue
               endif
               j1=jtet1(jtetoff(jt)+jface)
               if (j1.ne.j0) then
                  nincons=nincons+1
                  if(iwrite.le.nwrite)then
                    write(logdan,9010) nincons,it,iface,jt,jface
                    if (idebug.gt.0) then
                       call writloga('default',0,logdan,0,ierrwrt)
                    else
                       call writloga('bat',0,logdan,0,ierrwrt)
                    endif
 9010               format('Interior inconsistency: ',5i10)
                    iwrite=iwrite+1
                  endif
               endif
            elseif (j1.gt.mbndry.or.j1.lt.0) then
               j1=abs(j1)-mbndry
               jt=1+(j1-1)/nefcmo
               jface=j1-nefcmo*(jt-1)
               jtyp=itettyp(jt)
               joff=itetoff(jt)
               nnd2=ielmface0(jface,itettyp(jt))
               if (nnd2.ne.nnd) then
                  n_diff_nnd=n_diff_nnd+1
                  if (idebug.gt.0.and.n_diff_nnd.le.nwrite) then
                     write(logdan,*) 'ELMTESTD WARNING:'
     &                    //' element,face ',it,iface
     &                    ,' increments n_diff_nnd'
                     call writloga('default',0,logdan,0,ierrwrt)
                  endif
               else
                  ! test that same
                  do j=1,nnd2
                     loc_node(nnd+j)
     &                 =iparent(itet(joff+ielmface1(j,jface,jtyp)))
                  enddo
332               isort=0
                  do j=nnd+1,nnd+nnd2-1
                     if (loc_node(j+1).lt.loc_node(j)) then
                        isort=1
                        i=loc_node(j+1)
                        loc_node(j+1)=loc_node(j)
                        loc_node(j)=i
                     endif
                  enddo
                  if (isort.ne.0) goto 332
                  do j=1,nnd
                     if (loc_node(j).ne.loc_node(j+nnd)) then
                         n_diff_nnd=n_diff_nnd+1
                         if (idebug.gt.0.and.n_diff_nnd.le.nwrite) then
                            write(logdan,*) 'ELMTESTD WARNING:'
     &                           //' element,face ',it,iface
     &                           ,' increments n_diff_nnd'
                            call writloga('default',0,logdan,0,ierr)
                         endif
                         goto 3321
                     endif
                  enddo
3321              continue
               endif
               if (mbndry.eq.0) then
                  j1=-jtet1(jtetoff(jt)+jface)
               else
                  j1=jtet1(jtetoff(jt)+jface)-mbndry
               endif
               if( j1.le.0 .or. (j1.ne.j0 .and. jtet_cycle_max.le.2)
     &               ) then
                  nincons = nincons + 1
                  if(iwrite.le.nwrite)then
                    write(logdan,9020) nincons,it,iface,jt,jface
                    if (idebug.gt.0) then
                       call writloga('default',0,logdan,0,ierrwrt)
                    else
                       call writloga('bat',0,logdan,0,ierrwrt)
                    endif
 9020               format('Interface inconsistency: ',5i10)
                    iwrite=iwrite+1
                  endif
               else if( j1 .ne. j0 ) then   ! .and. jtet_cycle_max.gt.2
                  icycle=0
                  do while (j1.ne.j0.and.icycle.lt.jtet_cycle_max+1)
                     jt=1+(j1-1)/nefcmo
                     jface=j1-nefcmo*(jt-1)
                     if (mbndry.eq.0) then
                        j1=-jtet1(jtetoff(jt)+jface)
                     else
                        j1=jtet1(jtetoff(jt)+jface)-mbndry
                     endif
                     icycle=icycle+1
                  enddo
                  if (j1.ne.j0) then
                     nincons=nincons+1
                     if(iwrite.le.nwrite)then
                        write(logdan,9030) nincons,it,iface,icycle
9030                    format('Network inconsistency: ',5i10)
                        if (idebug.gt.0) then
                           call writloga('default',0,logdan,0,ierrwrt)
                        else
                           call writloga('bat',0,logdan,0,ierrwrt)
                        endif
                        iwrite=iwrite+1
                     endif
                  endif
                  if (icycle.gt.2) then
                     ncycles=ncycles+1
                     if (idebug.ne.0.and.ncycles.le.nwrite) then
                        write(logdan,*) 'ELMTESTD WARNING:'
     &                     //' element,face ',it,iface
     &                     ,' is part of a jtet cycle of length ',icycle
                        call writloga('default',0,logdan,0,ierr)
                     endif
                  endif
               endif
            endif
         enddo
      enddo
      if (idebug.ne.0) then
         write(logdan,*) 'ELMTESTD WARNING: ',ncycles
     &             ,' faces are part of jtet loops'
         call writloga('default',0,logdan,0,ierr)
      endif
      if (n_diff_nnd.gt.0 .or. idebug.ne.0) then
         write(logdan,*) 'ELMTESTD WARNING: ',n_diff_nnd,
     &   ' jtet loops linking faces with unequal of nodes'
         call writloga('default',0,logdan,0,ierr)
         write(logdan,*)
     &        '        or diff repeated nodes, jtet_reduce_nnd='
     &                 ,jtet_reduce_nnd
         call writloga('default',0,logdan,0,ierr)
      endif
      if (n_diff_nnd.gt.0.and.jtet_reduce_nnd.ne.1)
     &     nincons=nincons+n_diff_nnd
 
c ..... (do diagnostics for n_degen, n_self_degen) .....
c (only depends on itet, not jtet)
 
      n_self_degen=0
      n_degen=0
      do it=1,itetcnt
         ityp=itettyp(it)
         ioff=itetoff(it)
         joff=jtetoff(it)
         j2=0
         do jf=1,nelmnef(ityp)
            nnd=ielmface0(jf,ityp)
            ifsum=0
            j1=0
            do j=1,nnd
               loc_node(j)=itet(ioff+ielmface1(j,jf,ityp))
               ifsum=ifsum+loc_node(j)
               do i=1,j-1
                  if (loc_node(j).eq.loc_node(i)) then
                     j1=j1+1 ! self degenerate face
                  endif
               enddo
            enddo
            if (j1.ne.0) then
               n_self_degen=n_self_degen+1
               if (idebug.gt.0.and.n_self_degen.le.nwrite) then
                  write(logdan,*) 'ELMTESTD WARNING:'
     &                           //' element,face ',it,jf
     &                           ,' increments n_self_degen'
                  call writloga('default',0,logdan,0,ierr)
               endif
            endif
333         isort=0
            do j=1,nnd-1
               if (loc_node(j+1).lt.loc_node(j)) then
                  isort=1
                  i=loc_node(j+1)
                  loc_node(j+1)=loc_node(j)
                  loc_node(j)=i
               endif
            enddo
            if (isort.ne.0) goto 333
            do kf=1,jf-1
               if (ielmface0(jf,ityp).eq.nnd) then
                  isum=0
                  do j=1,nnd
                     loc_node(j+nnd)
     &                    =itet(ioff+ielmface1(j,kf,ityp))
                     isum=isum+loc_node(j+nnd)
                  enddo
                  if (isum.eq.ifsum) then
334                  isort=0
                     do j=nnd+1,2*nnd-1
                        if (loc_node(j+1).lt.loc_node(j)) then
                           isort=1
                           i=loc_node(j+1)
                           loc_node(j+1)=loc_node(j)
                           loc_node(j)=i
                        endif
                     enddo
                     if (isort.ne.0) goto 334
                     isum=0
                     do j=1,nnd
                        if (loc_node(j).eq.loc_node(j+nnd))
     &                         isum=isum+1
                     enddo
                     if (isum.eq.nnd) j2=j2+1
                  endif
               endif
            enddo
         enddo
         if (j2.ne.0) then
            n_degen=n_degen+1
            if (idebug.ne.0.and.n_degen.le.nwrite) then
               write(logdan,*) 'ELMTESTD WARNING: element ',it
     &              ,' increments n_degen and has ',j2,' degeneracies:'
               call writloga('default',0,logdan,0,ierr)
               write(logdan,*) '   face: nodes(face)'
               call writloga('default',0,logdan,0,ierr)
               do jf=1,nelmnef(ityp)
                  nnd=ielmface0(jf,ityp)
                  write(logdan,*) '   ',jf,':'
     &              ,(itet(ioff+ielmface1(j,jf,ityp)),j=1,nnd)
                  call writloga('default',0,logdan,0,ierr)
               enddo
            endif
         endif
      enddo
      if (n_degen.gt.0.or.n_self_degen.gt.0.or.idebug.ne.0) then
         write(logdan,*)'ELMTESTD WARNING: mesh has '
     &                       ,n_degen,' degenerate elements'
         call writloga('default',0,logdan,0,ierr)
         write(logdan,*)
     &     '        and ',n_self_degen,' self-degenerate elements'
         call writloga('default',0,logdan,0,ierr)
      endif
 
c ..... (return) .........
 
 9000 if(nincons.eq.0) then
         ierror=0
      elseif(nincons.ge.1) then
         ierror=nincons
         write(logdan,1000) nincons
 1000    format(' WARNING -- ',i6,' ELM NEIGHBOR INCONSISTENCIES')
         call writloga('default',2,logdan,2,ierr)
      endif
      goto 9999
 9999 continue
      return
      end
C #####################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C
C #####################################################################

      subroutine tettestd
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine checks the "jtet" array for internal consistency.
C        This version is specific to tets and is not consistent with networks.
C        (Compare subroutine elmtestd above.) Hence this version is "old".
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: tettestd.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.3   12/27/94 23:05:36   het
CPVCS    Modified the definition of mbndry to be min(4*ntets+1,16000000).
CPVCS
CPVCS
CPVCS       Rev 1.2   12/02/94 16:19:18   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.1   12/01/94 18:57:24   het
CPVCS    Added a variable type to the "cmo" calles
CPVCS       and include the "cmo.h" include file.
CPVCS
C
C ######################################################################
      implicit none
C
      include "cmo.h"
      include "neibor.h"
C
      integer ierror,nwrite,maxnef,length,icmotype,ierr,i,it,ier,
     *        lenitetclr,lenitettyp,lenitet,lenjtet,idebug,len,
     *        mbndry_test,iwrite,nholes,nholed,nincons,k,ierrwrt
      integer jtet_cycle_max
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      call cmo_get_name(cmo,ierror)
      nwrite = 20
      maxnef=8
C
      call cmo_get_info('jtet_cycle_max',cmo,jtet_cycle_max
     &                 ,length,icmotype,ierror)
      if (ierror.eq.0.and.jtet_cycle_max.gt.2) then
         write(logdan,'(a)')
     &        'TETESTD WARNING: networks should use elmtestd instead'
         call writloga('default',2,logdan,2,ierr)
      endif
 
      call cmo_get_info('faces_per_element',cmo,i
     &     ,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,it
     &     ,length,icmotype,ierror)
      if (i.ne.4.or.it.ne.4) then
         write(logdan,'(a)')
     &    'TETESTD WARNING: non-tet meshes should use elmtestd instead'
         call writloga('default',2,logdan,2,ierr)
      endif
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,itetcnt,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      call cmo_get_info('idebug',cmo,idebug,len,icmotype,ierror)
      if(idebug.ge.1) nwrite = itetcnt*maxnef
C
C
C     ******************************************************************
C
C
      mbndry_test=min((4*itetcnt+1),mbndry)
      iwrite=0
      nholes=0
      nincons=0
      k=0
      do 10 i=1,itetcnt*4
         it=0.25*dble(i)+0.9
         if(itet(1,it).le.0) then
            if(k.ne.it) then
               nholes=nholes+1
               if(iwrite.le.nwrite)then
                 write(logdan,9000) nholes,it
                 call writloga("default",0,logdan,0,ierrwrt)
 9000            format("Tet-hole:          ",2i10)
                 iwrite=iwrite+1
               endif
            endif
         elseif(jtet1(i).lt.mbndry_test) then
            if(jtet1(jtet1(i)).ne.i) then
               nincons=nincons+1
               if(iwrite.le.nwrite)then
                 write(logdan,9010) nincons,it
                 call writloga("default",0,logdan,0,ierrwrt)
 9010            format("Tet-inconsistency: ",2i10)
                 iwrite=iwrite+1
               endif
            endif
         elseif (itet(1,it).gt.0 .and. jtet1(i).gt.mbndry) then
            if (jtet1(jtet1(i)-mbndry) .ne. (i+mbndry)) then
               nincons = nincons + 1
               if(iwrite.le.nwrite)then
                 write(logdan,9010) nincons,it
                 call writloga("default",0,logdan,0,ierrwrt)
                 iwrite=iwrite+1
               endif
            endif
         endif
 10   continue
      if(nincons.ge.1) then
         write(logdan,1000) nincons
 1000    format('WARNING --  ',i6,' TET NEIGHBOR INCONSISTENCIES')
         call writloga('default',2,logdan,2,ierr)
      endif
      goto 9999
 9999 continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
