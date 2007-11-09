      subroutine rmpoint(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C
C#######################################################################
C
C      PURPOSE -
C
C         This routine is used to remove a specified list of points and
C         and associated elements, or elements below a specified
C         volume.
C          If option is compress elements whose colors are negative
C          will also be removed.
C
C
C         FORMAT:    RMPOINT/istart/iend/istride/inclusive[exclusive]
C                            /nnlevels/mbndry_set/
C                            dud set of points
C                    RMPOINT/COMPRESS
C                            remove dudded points
C                    RMPOINT/WOMESH
C                            dud points without mesh
C                    RMPOINT/ZERO_VOLUME/threshold
C                            dud points belonging to elements with
C                            volume at or below threshold
C
c                    RMPOINT/ELEMENT
C                            remove tets whose first itet value
C                            have been set to negative
C                    RMPOINT/ELEMENT/ELTSET,GET,ename
C                            remove set of tets
C                    RMPOINT/ELEMENT/tet list
C                            remove list of elements
C
C                    note for the rmpoint/element calls
C                    elements are removed but points are dudded
C                    use rmpoint/compress to remove dudded points
C
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
C      CHANGE HISTORY -
C
C         $Log: rmpoint.f,v $
C         Revision 2.00  2007/11/09 20:04:01  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   01 Oct 2007 08:19:08   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty.
CPVCS    
CPVCS       Rev 1.7   30 Sep 2004 09:52:34   dcg
CPVCS    use .not. with logical variable in place of .eq..false
CPVCS    
CPVCS       Rev 1.6   25 Mar 2003 08:15:50   gable
CPVCS    Changed references to 'tetrahedra' to 'elements'.
CPVCS    
CPVCS       Rev 1.5   25 Mar 2003 08:09:54   gable
CPVCS    Modified screen output to say elements removed instead
CPVCS    of tetrahedra removed.
CPVCS    
CPVCS       Rev 1.4   31 May 2001 14:13:54   dcg
CPVCS    set imtmax before using - get rid of extra do loop over materials
CPVCS    
CPVCS       Rev 1.3   09 Apr 2001 09:00:50   gable
CPVCS    Removed default reorder which put nodes in contiguous imt order.
CPVCS    This can now be done using SORT and REORDER.
CPVCS    
CPVCS       Rev 1.2   26 Sep 2000 10:10:32   dcg
CPVCS    use type 2 for hold1 allocation
CPVCS    use mmnewlen to increase hold1 allocation if needed (rank*nsave)
CPVCS
CPVCS       Rev 1.1   26 Sep 2000 09:30:22   dcg
CPVCS    change allocation for isave and hold1 (length now nnodes originally)
CPVCS    change type for isave, hold1,isn2,ichange (now 1 for integer)
CPVCS
CPVCS       Rev 1.0   Mon Jan 31 09:53:12 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.29   16 Jan 2000 09:40:38   jtg
CPVCS    revision 1.27 restored: revision 1.28 breaks rmpoint
CPVCS    for jtet loops (networks)
CPVCS
CPVCS       Rev 1.28   10 Jan 2000 09:03:08   dcg
CPVCS    make sure all faces of tets to be removed from mesh have
CPVCS    their jtet values set to mbndry.
CPVCS    No assumption is made about whether the 'old neigbor' jtet
CPVCS    relationship is correct.
CPVCS
CPVCS       Rev 1.27   Thu Dec 16 12:04:54 1999   tam
CPVCS    added command  rmpoint/element/eltset,get,ename
CPVCS
CPVCS       Rev 1.26   Tue Nov 30 17:16:08 1999   jtg
CPVCS    resets lower_d_flag to indicate lower d structures must be refreshed.
CPVCS
CPVCS       Rev 1.25   Thu Nov 04 16:47:18 1999   dcg
CPVCS    remove argument from call to termcode
CPVCS
CPVCS       Rev 1.24   Thu Nov 04 16:40:16 1999   dcg
CPVCS    remove call to termgen
CPVCS
CPVCS       Rev 1.23   Fri Aug 27 10:48:30 1999   dcg
CPVCS    move isetwd values correctly to new node locations
CPVCS
CPVCS       Rev 1.22   Thu Aug 26 13:14:52 1999   dcg
CPVCS    zero out imt, itp, icr, isn for removed nodes
CPVCS
CPVCS       Rev 1.21   Wed Feb 03 14:05:40 1999   nnc
CPVCS    Fixed bugs in the handling of node-based vector attributes.
CPVCS
CPVCS       Rev 1.20   Fri Jun 19 09:40:14 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.19   Tue May 19 15:56:36 1998   dcg
CPVCS    modify 'element' option to look for tets marked with
CPVCS    itet(1,it) as negative and delete these tets
CPVCS
CPVCS       Rev 1.18   Fri Sep 26 10:14:30 1997   dcg
CPVCS    add cmo_newlen after compress to make cmo
CPVCS    attribute lengths agree with nnodes and ntets
CPVCS
CPVCS       Rev 1.17   Tue Jul 01 15:31:58 1997   dcg
CPVCS    initialize mpno in zero_volume option
CPVCS
CPVCS       Rev 1.16   Mon Apr 14 16:59:56 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.15   Mon Apr 07 10:10:16 1997   dcg
CPVCS    don't call addmesh_delete if there are no elements
CPVCS
CPVCS       Rev 1.14   Thu Oct 03 13:01:14 1996   dcg
CPVCS    make node removal inclusive or exclusive of entire element
CPVCS    changes added by kmb (Kathy Bowers)
CPVCS
CPVCS       Rev 1.13   Fri Jul 26 13:25:56 1996   dcg
CPVCS    remove elements with negative material types
CPVCS
CPVCS       Rev 1.12   Thu May 23 14:22:22 1996   dcg
CPVCS    allocate temp storage based on rank of mesh object attribute
CPVCS
CPVCS       Rev 1.11   Wed May 22 07:04:52 1996   het
CPVCS    Correct an error with element deletion.
CPVCS
CPVCS       Rev 1.10   Wed May 01 16:42:14 1996   dcg
CPVCS    replace npoints with nnodes to fix bug
CPVCS
CPVCS       Rev 1.9   Tue Mar 05 12:35:56 1996   het
CPVCS    Add the call to addmesh_delete for deleting elements.
CPVCS
CPVCS       Rev 1.8   Thu Feb 08 14:08:34 1996   dcg
CPVCS    add loop on mesh object attributes
CPVCS
CPVCS       Rev 1.7   Mon Jan 29 22:16:04 1996   het
CPVCS    Add the rmpoint/zero_volume option to delete zero volume elements.
CPVCS
CPVCS       Rev 1.6   Tue Jan 23 11:45:26 1996   het
CPVCS    Correct an error with copying ISN1s.
CPVCS
CPVCS       Rev 1.5   Tue Jan 23 09:17:46 1996   het
CPVCS    Fix an error in copying isn1 numbers.
CPVCS
CPVCS       Rev 1.4   Fri Dec 22 14:33:30 1995   het
CPVCS    Correct an error
CPVCS    .
CPVCS
CPVCS       Rev 1.3   12/05/95 08:26:02   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.2   11/16/95 15:22:28   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.1   11/07/95 17:25:26   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.0   09/20/95 09:46:32   dcg
CPVCS    Initial revision.
C
C
C#######################################################################
C
C
      implicit none
C
      include 'local_element.h'
      include 'chydro.h'
      include 'consts.h'
      include 'neibor.h'
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
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipicr1, icr1)
      integer isetwd(*)
      integer imt1(*), itp1(*),
     *        isn1(*),icr1(*)
C
C     *****************************************************************
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(*), yic(*), zic(*)
C
C
      pointer (ipitetclr, itetclr )
      integer itetclr(*)
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
      pointer(ipmpary  , mpary   )
      pointer(ipmpary1 , mpary1  )
      pointer(ipmpary2 , mpary2  )
      pointer(ipiremtet, iremtet )
      pointer(ipisn2   , isn2    )
      pointer(ipisave  , isave   )
      pointer(ipichang , ichange )
      pointer(iphold1  , ihold1  )
      pointer(iphold1  , xhold1  )
      pointer(ipxcmo   , xcmo )
      pointer(ipxcmo   , icmo )
      pointer(ipielts  , ielts )
      pointer(ipxtetwd , xtetwd )
      integer mpary(*),mpary1(*),mpary2(*),iremtet(*),xtetwd(*),
     *  isn2(*),isave(*),ichange(*),ihold1(*),icmo(*),ielts(*)
      real*8 xhold1(*),xcmo(*)
C
C#######################################################################
C
      character*32 isubname, cmo
      character*32 cpt1, cpt2, cpt3, ctype,crank,cattr_name,clength,
     *  cinter,cpers,cio
 
C
C#######################################################################
C
      real*8 xicvol(maxnen), yicvol(maxnen), zicvol(maxnen)
C
C#######################################################################
C
      logical linclusive
      integer ilen,ityp,icscode,mpno,it,i,j,nremtet,nnodes,nmelts,
     *  nelements,mbndry,jtetset,ipointi,ipointj,nremove,l,
     *  irank,nlen,iatt,ier,itout,lout,natt,imatno,nsave,imtmax,ierr1,
     *  lenxhold,iflag,ichain,mpnt2,mpnt1,iseqno,isum,ipass,i1,ii,
     *  npoints,imaxi1,mpnt,mbndry_set,nnlevels,ipt1,ipt2,ipt3,ics,
     *  ipointf,mpnttmp,length,ierrw,mbndry_old,mbndry_new,index,
     *  itp1_boundary,ierror,npoints2,itremove,mpntnxt,ierror_return
      real*8 xtetvol,smalvolc,smalvol,zmaxl,ymaxl,xmaxl,
     *  xminl,yminl,zminl,epsilonl
c
      integer icharlnf,iimax,ismax,icharln,ismin,local_debug, if_reorder
      real*8 cvmgt
C
C     ******************************************************************
C
C     Define the memory-management partition.
C
      isubname='rmpoint'
 
      local_debug=0
      if (local_debug.gt.0) call mmverify()
C
C
C     ******************************************************************
C
C     Preliminaries.
C
C
      call cmo_get_name(cmo,icscode)
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,icscode)
      call cmo_get_info('nelements',cmo,
     *                   nelements,ilen,ityp,icscode)
      if(nnodes .le. 0)then
         write(logmess,'(a)')'WARNING: NO NODES'
         call writloga('default',0,logmess,0,icscode)
         write(logmess,'(a)')'WARNING: NO ACTION'
         call writloga('default',0,logmess,0,icscode)
         write(logmess,'(a)')'RETURN'
         call writloga('default',0,logmess,0,icscode)
         return
      endif

      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,icscode)
      if (icscode.ne.0) mbndry=0
C
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,icscode)
         if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
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
      mpno=0
C
C
C
C     ******************************************************************
C     GET THE SEARCH RANGE.
C
      call get_epsilon('epsilonl', epsilonl)
C
C
C     ******************************************************************
C
C     RMPOINT/ELEMENT/...
C     Decide whether the points are to be dudded and their associated
C     elements removed, or the dudded points are to be removed; and
C     take the corresponding action.
C     iremtet is used to index tets marked for removal later in code
C
      if(cmsgin(1)(1:7).eq.'element') then
 
         if(nelements.gt.0) then
            call mmgetblk('iremtet',isubname,
     *                    ipiremtet,nelements,2,icscode)
            nremtet=0
            do it=1,nelements
               iremtet(it)=0
            enddo
         endif
         jtetset=mbndry
         nremtet=0
C
C  either tets are given specifically in which case nwds > 1
C  or we look for tets that have been marked with negative
C  itet values in itet(1,?)
 
c        RMPOINT/ELEMENT/ELTSET,GET,ename
         if(nwds.gt.2 .and. msgtype(2).eq.3) then
 
            cpt1='eset'
            cpt2='get'
            cpt3=cmsgin(4)
            call mmgetblk('ielts',isubname,ipielts,nelements,1,icscode)
            call cmo_get_info('xtetwd',cmo,ipxtetwd,ilen,ityp,ier)
            call eltlimc(cpt1,cpt2,cpt3,ipielts,nmelts,nelements,xtetwd)
            do i=1,nmelts
               itremove=ielts(i)
               if(itremove.gt.0.and.itremove.le.nelements) then
                  nremtet=nremtet+1
                  iremtet(itremove)=1
               endif
            enddo
 
c        RMPOINT/ELEMENT/ TET LIST
         elseif(nwds.ge.2 .and. msgtype(2).eq.1) then
            do i=2,nwds
               itremove=imsgin(i)
               if(itremove.gt.0.and.itremove.le.nelements) then
                  nremtet=nremtet+1
                  iremtet(itremove)=1
               endif
            enddo
         else
 
C           RMPOINT/ELEMENT
            do i=1,nelements
               if(itet1(itetoff(i)+1).lt.0) then
                  nremtet=nremtet+1
                  iremtet(i)=1
                  do j=1,nelmnen(itettyp(i))
                    itet1(itetoff(i)+j) = abs(itet1(itetoff(i)+j))
                  enddo
               endif
            enddo
         endif
C
C
C        Set the first value of ITET to negative for elements to be
C        removed.
C
         mbndry_old=mbndry
         mbndry_new=mbndry
         itp1_boundary=ifitprfl
         call addmesh_delete(cmo,
     *                       mbndry_old,
     *                       mbndry_new,
     *                       itp1_boundary,
     *                       ipiremtet,
     *                       ierror)
C
C        Adjust ITETCNT.
C        Points were dudded in addmesh_delete, count them for output
 
         nelements=nelements-nremtet
         npoints2=0
         do i=1,nnodes
           if (itp1(i) .eq. ifitpdud) then
             npoints2=npoints2+1
           endif
         enddo
 
C
C        ...............................................................
C        Write message giving element removal information.
C
         write(logmess,2110) npoints2,nremtet
 2110    format(' Dudded',i10,
     $          ' points plus parents, and',i11,
     $          ' elements removed.')
         call writloga('default',0,logmess,0,ierrw)
C
         call cmo_set_info('nelements',cmo,nelements,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
C        ...............................................................
C
C
C        _______________________________________________________________
C
 
C     RMPOINT/COMPRESS
      elseif(cmsgin(1)(1:8).ne.'compress') then
C
C        _______________________________________________________________
C
C        The points are to be dudded and their corresponding elements
C        removed.
C
C
C        ...............................................................
C        Get temporary memory from the memory manager.
C
         length=nnodes
         call mmgetblk('mpary'   ,isubname,ipmpary ,length,2,icscode)
         call mmgetblk('mpary1'  ,isubname,ipmpary1,length,2,icscode)
         call mmgetblk('mpary2'  ,isubname,ipmpary2,length,2,icscode)
         if(nelements.gt.0) then
            call mmgetblk('iremtet',isubname,
     *                    ipiremtet,nelements,2,icscode)
            nremtet=0
            do it=1,nelements
               iremtet(it)=0
            enddo
         endif
C
C        ...............................................................
C        Set the point-index boundaries.
C
         if(nwds.eq.0) then
            ipt1=1
            ipt2=nnodes
            ipt3=1
 
C        RMPOINT/ifirst,ilast,istride
C        RMPOINT/ZERO_VOLUME
C        RMPOINT/WOMESH
         elseif(nwds.eq.1) then
            if(cmsgin(1)(1:6).eq.'womesh'.or.
     *         cmsgin(1)(1:11).eq.'zero_volume') then
               ipt1=0
               ipt2=0
               ipt3=0
               if(cmsgin(1)(1:11).eq.'zero_volume') then
                  xminl=xic(ismin(nnodes,xic(1),1))
                  yminl=yic(ismin(nnodes,yic(1),1))
                  zminl=zic(ismin(nnodes,zic(1),1))
                  xmaxl=xic(ismax(nnodes,xic(1),1))
                  ymaxl=yic(ismax(nnodes,yic(1),1))
                  zmaxl=zic(ismax(nnodes,zic(1),1))
                  smalvol=epsilonl*(xmaxl-xminl)
     $                            *(ymaxl-yminl)
     $                            *(zmaxl-zminl)
                  smalvolc=max(epsilonl,smalvol)
               endif
            else
               ipt1=imsgin(1)
               ipt2=ipt1
               ipt3=1
            endif
         elseif(nwds.eq.2) then
            if(cmsgin(1)(1:11).eq.'zero_volume') then
               ipt1=0
               ipt2=0
               ipt3=0
               smalvol=xmsgin(2)
               smalvolc=smalvol
            else
               ipt1=imsgin(1)
               ipt2=imsgin(2)
               ipt3=1
            endif
         elseif(msgtype(1).eq.1) then
            ipt1=imsgin(1)
            ipt2=imsgin(2)
            ipt3=imsgin(3)
         elseif(msgtype(1).eq.2) then
            ipt1=xmsgin(1)
            ipt2=xmsgin(2)
            ipt3=xmsgin(3)
         elseif(msgtype(1).eq.3) then
            cpt1=cmsgin(1)
            cpt2=cmsgin(2)
            cpt3=cmsgin(3)
         endif
C
         nnlevels=0
         if(nwds.ge.5) then
            if(msgtype(5).eq.1) then
               nnlevels=imsgin(5)
            elseif(msgtype(5).eq.2) then
               nnlevels=xmsgin(5)
            elseif(msgtype(5).eq.3) then
               nnlevels=0
            endif
         endif
C
         mbndry_set=0
         if(nwds.ge.6) then
            if(msgtype(6).eq.1) then
               mbndry_set=imsgin(6)
            elseif(msgtype(6).eq.2) then
               mbndry_set=xmsgin(6)
            elseif(msgtype(6).eq.3) then
               mbndry_set=0
            endif
         endif
C
         linclusive=.false.
         if(nwds.ge.4) then
            if(msgtype(4).eq.1) then
               linclusive=.false.
            elseif(msgtype(4).eq.2) then
               linclusive=.false.
            elseif(msgtype(4).eq.3) then
               if(cmsgin(4)(1:icharlnf(cmsgin(4))).eq.'inclusive') then
                  linclusive=.true.
               endif
            endif
         endif
C
C        ...............................................................
C        Check point limits and prepare an array consisting of the
C        desired points.
C
         if(cmsgin(1)(1:11).eq.'zero_volume') then
            npoints=0
 
C        RMPOINT/WOMESH cont.
         elseif(cmsgin(1)(1:6).eq.'womesh') then
            do i=1,nnodes
               mpary(i)=i
            enddo
            do it=1,nelements
               do i=1,nelmnen(itettyp(it))
                  mpary(itet1(itetoff(it)+i))=0
               enddo
            enddo
            call kmprsn(nnodes,mpary,1,mpary,1,mpary,1,npoints)
         else
            npoints=nnodes
            if(msgtype(1).eq.1.or.msgtype(1).eq.2) then
               call pntlimn(ipt1,ipt2,ipt3,
     *                      ipmpary,mpno,npoints,isetwd,itp1)
            elseif(msgtype(1).eq.3) then
               call pntlimc(cpt1,cpt2,cpt3,
     *                      ipmpary,mpno,npoints,isetwd,itp1)
            endif
         endif
C
C        ...............................................................
 
C        RMPOINT/ifirst,ilast,istride
C        Set up two modified mass-point arrays, such that they contain
C        zeros for mass points that will not be dudded.  Eventually one
C        of these arrays will be compressed to contain a comprehensive
C        list of points to be dudded, which also will include the list
C        of neighbors to be dudded.  These arrays also will include
C        all points that are ONLY connected to elements to be removed,
C        namely, all interface and some external boundary points
C        associated with elements to be removed, because these points
C        must also be dudded.  Two arrays are necessary in preparing
C        such a comprehensive list.
C
C
C        Initialize MPARY1.
C
         do 100 i=1,nnodes
            mpary1(i)=0
  100    continue
C
C        Add MPARY list to MPARY1.
C
         do 200 ii=1,mpno
            mpary1(mpary(ii))=mpary(ii)
  200    continue
C
C        Exclude interface parent points from MPARY1.  We  need to
C        retain these points if there are more than two children.  If
C        there are only two children, the parent point will be dudded
C        anyway, because surely if the parent got into the list, at
C        least one child must get into the list because of their
C        identical coordinates.  Also, copy MPARY1 to MPARY2.
C
         do 300 i=1,mpno
            i1=mpary(i)
            mpary1(i1)=cvmgt(0,mpary1(i1),itp1(i1).eq.ifitpcup)
            mpary2(i1)=mpary1(i1)
  300    continue
C
C        ...............................................................
C        Modify MPARY1 and MPARY2 to include the desired neighbors.
C
         do ipass=1,nnlevels
            do it=1,nelements
               imaxi1=-1
               do i=1,nelmnen(itettyp(it))
                  imaxi1=max(imaxi1,mpary1(itet1(itetoff(it)+i)))
               enddo
               if(imaxi1.ne.0) then
                  do i=1,nelmnen(itettyp(it))
                     i1=itet1(itetoff(it)+i)
                     mpary2(i1)=i1
                  enddo
               endif
            enddo
            do i1=1,nnodes
               mpary1(i1)=mpary2(i1)
            enddo
         enddo
C
C        Now eliminate points from the above list that are connected to
C        elements that won't be removed.
C
         if(.not.linclusive) then
         do it=1,nelements
            isum=0
            do i=1,nelmnen(itettyp(it))
               if(mpary1(itet1(itetoff(it)+i)).ne.0) isum=isum+1
            enddo
            if(isum.gt.0.and.isum.lt.nelmnen(itettyp(it))) then
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  mpary2(i1)=0
               enddo
            endif
         enddo
         endif
C
C        ...............................................................
C        Remove elements associated with mass points to be dudded.
C
C
C        Make a list of elements to be removed.
C
         if(cmsgin(1)(1:11).ne.'zero_volume') then
            do it=1,nelements
               isum=0
               do i=1,nelmnen(itettyp(it))
                  if(mpary1(itet1(itetoff(it)+i)).ne.0) isum=isum+1
               enddo
               if(.not.linclusive)then
               if(isum.eq.nelmnen(itettyp(it))) then
                  nremtet=nremtet+1
                  iremtet(it)=1
               endif
               else
               if(isum.gt.0)then
                  nremtet=nremtet+1
                  iremtet(it)=1
               endif
               endif
            enddo
         else
            do it=1,nelements
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  xicvol(i)=xic(i1)
                  yicvol(i)=yic(i1)
                  zicvol(i)=zic(i1)
               enddo
C
               call volume_element(itettyp(it),
     *                             xicvol,yicvol,zicvol,
     *                             xtetvol)
C
               if(xtetvol.le.smalvolc) then
                  nremtet=nremtet+1
                  iremtet(it)=1
               endif
            enddo
         endif
C
C        Set JTET for elements adjoining those to be removed to the
C        desired value.  Also, change point types of points also
C        belonging to adjoining tetrahedron faces to IFITPFRE.
C
         if(nelements.gt.0) then
            if(mbndry_set.eq.0) then
               jtetset=mbndry
            else
               jtetset=-iabs(mbndry_set)
            endif
            mbndry_old=mbndry
            mbndry_new=jtetset
            itp1_boundary=ifitpfre
            call addmesh_delete(cmo,
     *                       mbndry_old,
     *                       mbndry_new,
     *                       itp1_boundary,
     *                       ipiremtet,
     *                       ierror)
C
C           Adjust ITETCNT.
C
            nelements=nelements-nremtet
         endif
C
C        Copy MPARY2 onto MPARY1.
C
         do i1=1,nnodes
            mpary1(i1)=mpary2(i1)
         enddo
C
C        ...............................................................
C        Compress mpary2 to contain a compressed list of mass points to
C        be dudded.
C
         call kmprsn(nnodes,mpary2,1,mpary2,1,mpary2,1,npoints2)
C
C        ...............................................................
C        Dud the points in the MPARY2 list; also dud the corresponding
C        parent points if there are only two children, and change point
C        types of points opposite interface; and reestablish interface
C        chain links if there are more than two children.
C
         do ii=1,npoints2
            mpnt=mpary2(ii)
            iseqno=isn1(mpnt)
            itp1(mpnt)=ifitpdud
C*****      volic(mpnt)=0.0
C*****      ric(mpnt)=1.0
            itp1(mpnt)=ifitpdud
            isn1(mpnt)=0
            itp1(mpnt)=ifitpdud
            if(iseqno.ne.0) then
               mpnt1=iseqno
               mpnt2=isn1(mpnt1)
               iseqno=isn1(mpnt2)
               if(iseqno.eq.mpnt) then
                  do ichain=1,2
                     if(ichain.eq.1) then
                        mpnttmp=mpnt1
                     else
                        mpnttmp=mpnt2
                     endif
                     isn1(mpnttmp)=0
                     if(itp1(mpnttmp).eq.ifitpcup) then
                        itp1(mpnttmp)=ifitpdud
C*****                  volic(mpnttmp)=0.0
C*****                  ric(mpnttmp)=1.0
                     else
                        itp1(mpnttmp)=ifitpfre
                     endif
                  enddo
               else
                  do ichain=3,nnodes
                     mpntnxt=iseqno
                     iseqno=isn1(mpntnxt)
                     if(iseqno.eq.mpnt) goto 1400
                  enddo
                  write(logmess,1300) mpnt
 1300             format(' Parent-child discrepancies for point',i10,
     $                   '.')
                  call termcode()
 1400             continue
                  isn1(mpntnxt)=mpnt1
               endif
            endif
         enddo
C
C        ...............................................................
C        Write message giving point dudding information.
C
         write(logmess,2100) npoints2,nremtet
 2100    format(' Dudded',i10,
     $          ' points plus parents, plus',i11,
     $          ' elements removed.')
         call writloga('default',0,logmess,0,ierrw)
C
         call cmo_set_info('nelements',cmo,nelements,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
C        ...............................................................
C
C
C        _______________________________________________________________
C
      else
C
C        _______________________________________________________________
C
C        The points are to be removed and the surviving points
C        resequenced, and the ITET values of existing elements
C        corrected.
C        ...............................................................
C        Check to see if any elements are connected to points that have
C           been "dudded" or otherwise removed. If so then clean up the
C           element connectivity list by deleting these elements and
C           compressing the element connectivity list.
C
C        First, sweep through the element list to find elements
C           that are connected to a point that will be removed, then
C           set the first value of ITET to negative for element to be
C           removed.
C
         if (nelements.gt.0)
     *      call mmgetblk('iremtet',isubname,
     *                    ipiremtet,nelements,2,icscode)
         nremtet=0
         do it=1,nelements
            iremtet(it)=0
            if (itetclr(it).lt.0) then
               nremtet=nremtet+1
               iremtet(it)=1
            else
               iflag=0
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  if(itp1(i1).ge.ifitpst3.and.itp1(i1).le.ifitpen3) then
                     iflag=iflag+1
                  endif
               enddo
               if(iflag.gt.0) then
                  nremtet=nremtet+1
                  iremtet(it)=1
               endif
            endif
         enddo
C
         if(nremtet.gt.0) then
C
C           Delete the tets by compressing itet and jtet.
C
            mbndry_old=mbndry
            mbndry_new=mbndry
            itp1_boundary=ifitprfl
            call addmesh_delete(cmo,
     *                          mbndry_old,
     *                          mbndry_new,
     *                          itp1_boundary,
     *                          ipiremtet,
     *                          ierror)
C
C           Adjust ITETCNT.
C
            nelements=nelements-nremtet
C
         endif
C
C        ...............................................................
C        Get temporary memory from the memory manager.
C
C
         length=nnodes
         lenxhold=length
         call mmgetblk('isn2'   ,isubname,ipisn2 ,length,1,icscode)
         call mmgetblk('isave'   ,isubname,ipisave ,lenxhold,1,icscode)
         call mmgetblk('ichange' ,isubname,ipichang,length,1,icscode)
         call mmgetblk('hold1'   ,isubname,iphold1 ,lenxhold,2,icscode)
C
C        ...............................................................
C        Make a compressed list of points to be saved
C        (real points and parent interface points).
C
C
         imtmax=imt1(iimax(nnodes,imt1(1),1))
         if_reorder = 0
C
         if(if_reorder .eq. 0) then
C         
            nsave=0
            do i=1,nnodes
               if(itp1(i).lt.ifitpst3.or.itp1(i).gt.ifitpen3)then
                  nsave=nsave+1
                  isave(nsave)=i
               endif
            enddo
C         
         elseif(if_reorder .eq. 1) then
C         
C        This part of the code reorders putting imt's of the same
C        value in contiguous order in the MO. This is being removed
C        since this can be done using the SORT and REORDER commands.
C
            nsave=0
            do imatno=0,imtmax
              do i=1,nnodes
                 if(imatno.eq.imt1(i).and.
     $            (itp1(i).lt.ifitpst3.or.itp1(i).gt.ifitpen3)
     $             ) then
                     nsave=nsave+1
                     isave(nsave)=i
                  endif
               enddo
            enddo
C
         endif
C
C        ...............................................................
C        Construct an array called ICHANGE that points to the relative
C        location of a given mass point in the ISAVE array.  For
C        example, ICHANGE(25)=7 would indicate that mass point 25 is
C        in location 7 of the ISAVE array.  ICHANGE of points not
C        contained in the ISAVE list would be set to zero.  This array
C        will establish a relation between existing point numbers and
C        the new numbers after resequencing.  For example, ICHANGE(25)=7
C        would tell us that point 25 will become point 7.
C
         do i=1,nnodes
            ichange(i)=0
         enddo
         do ii=1,nsave
            ichange(isave(ii))=ii
         enddo
C
C        ...............................................................
C        Correct sequence numbers in the fit word to correspond to new
C        numbers.
C
         do i=1,nnodes
            isn2(i)=0
         enddo
         do ii=1,nsave
            mpnt=isave(ii)
            iseqno=isn1(mpnt)
            if(iseqno.ne.0) then
               isn2(mpnt)=ichange(iseqno)
            endif
         enddo
         do i=1,nnodes
            isn1(i)=isn2(i)
         enddo
C
C        ...............................................................
C        Shift mass-point data from old to new locations.
C
C  Loop through mesh object attributes
C
         call cmo_get_info('number_of_attributes',cmo,natt,
     *                   lout,itout,ier)
         do iatt=1,natt
            call cmo_get_attribute_name(cmo,iatt,cattr_name,ier)
            nlen=icharln(cattr_name,ier)
            call cmo_get_attparam(cattr_name,cmo,index,ctype,crank,
     *         clength,cinter,cpers,cio,ierror_return)
 
            if(ier.eq.0.and.ctype(1:7).eq.'VDOUBLE'.and.
     *           clength(1:6).eq.'nnodes') then
 
               call cmo_get_info(crank,cmo,irank,lout,itout,ier)
               if (irank*nsave.gt.lenxhold) then
                  lenxhold=irank*nsave+100
                  call mmnewlen('hold1',isubname,iphold1,lenxhold,ier)
               endif
               call cmo_get_info(cattr_name,cmo,ipxcmo,lout,itout,ier)
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
            elseif(ier.eq.0.and.ctype(1:4).eq.'VINT'.and.
     *              clength(1:6).eq.'nnodes') then
              call cmo_get_info(crank,cmo,irank,lout,itout,ier)
              call cmo_get_info(cattr_name,cmo,ipxcmo,lout,itout,ier)
              if (irank*nsave.gt.lenxhold) then
                  lenxhold=irank*nsave+100
                  call mmnewlen('hold1',isubname,iphold1,lenxhold,ier)
               endif
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
C        ...............................................................
C        Change ITET values of affected elements.
C
         do it=1,nelements
            do i=1,nelmnen(itettyp(it))
               itet1(itetoff(it)+i)=ichange(itet1(itetoff(it)+i))
            enddo
         enddo
C
C        ...............................................................
C        Reset the counters.
C
         nremove=nnodes-nsave
         ipointi=1
         ipointj=nsave
         ipointf=nsave
         do i=1,nremove
c
c   zero out mesh object attributes from deleted nodes
c
            isn1(nsave+i)=0
            itp1(nsave+i)=0
            imt1(nsave+i)=0
            icr1(nsave+i)=0
         enddo
C
         call cmo_set_info('ipointi',cmo,ipointi,1,1,ics)
         if (ics .ne. 0) call x3d_error(isubname,'ipointi')
         call cmo_set_info('ipointj',cmo,ipointj,1,1,ics)
         if (ics .ne. 0) call x3d_error(isubname,'ipointj')
C
         call cmo_set_info('nnodes',cmo,ipointf,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
         call cmo_set_info('nelements',cmo,nelements,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
         call cmo_newlen(cmo,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
C        ...............................................................
C        Write message giving point removal information.
C
         write(logmess,5800) nremove,nremtet
 5800    format(i10,' points removed and ',i10,' elements removed.')
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,5810) ipointf
 5810    format('   The new point count is',i10,'.')
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,5820) nelements
 5820    format('   The new element count is',i10,'.')
         call writloga('default',0,logmess,0,ierrw)
C
C        ...............................................................
C
C
C        _______________________________________________________________
C
      endif
C
C
C     ******************************************************************
C
C     Set the error flag to normal.
C
      ierr1=0
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
      if (local_debug.gt.0) call mmverify()
      return
      end
