*dk,addmesh_add
      subroutine addmesh_add(cmoc,cmob,
     *			     nuser_refine, refine_type,
     *			     ierror)
C
C ALL comments to this procedure have been added by user lund who was
C NOT the author of the code, so beware...comments may be inaccurate.
C ######################################################################
C
C     PURPOSE -
C
C        This routine adds two meshes together, according to the
C          specifications of the 'add' option of the addmesh command.
C        It takes two meshes, finds the overlap between them, excavates
C        the 'background' mesh to make room for the incoming mesh, and
C        inserts the new (slave) mesh in the excavated hole.
C
C     INPUT ARGUMENTS -
C
C        cmoc    - The master mesh_object (source1).
C        cmob    - The slave mesh_object (source2).
C        nuser_refine - Number of refine iterations for the addmesh/add option
C	 refine_type - method of refinement
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The resulting mesh_object.
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C    $Log: addmesh_add.f,v $
C    Revision 2.00  2007/11/05 19:45:45  spchu
C    Import to CVS
C
CPVCS    
CPVCS       Rev 1.28   29 Sep 2004 10:03:54   dcg
CPVCS    add declaration for logmess in subroutine continue_refinement
CPVCS
CPVCS       Rev 1.27   11 Apr 2001 16:28:10   dcg
CPVCS    use logical variables correctly
CPVCS
CPVCS       Rev 1.26   03 Nov 2000 11:30:30   dcg
CPVCS    use derive instead of create
CPVCS
CPVCS       Rev 1.25   23 Feb 2000 11:01:34   dcg
CPVCS    fix calling sequence to geniee_cmo
CPVCS
CPVCS       Rev 1.24   Wed Oct 08 12:41:08 1997   gable
CPVCS    Changes by Loraine Lundquist to allow automatic
CPVCS    refinement of region where two mesh overlap. This
CPVCS    only works for a pair of tet meshes.
CPVCS
CPVCS       Rev 1.23   Mon Apr 14 16:38:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.22   Wed Nov 27 14:26:26 1996   het
CPVCS    Fix a tet only loop to be hybrid
CPVCS
CPVCS       Rev 1.21   Tue Nov 26 13:50:30 1996   het
CPVCS    Restore the added CMO after writing CMOD.
CPVCS
CPVCS       Rev 1.20   Tue Nov 19 08:47:14 1996   dcg
CPVCS    declare isubname type character
CPVCS
CPVCS       Rev 1.19   Mon Nov 11 20:59:20 1996   het
CPVCS    Make changes for adding hybrid grids.
CPVCS
CPVCS       Rev 1.17   Mon Oct 07 14:33:12 1996   gable
CPVCS    Added PVCS Log inside file and replaced comments that were lost in rev 1.16
C  8/7/96 lund
C   fixed tet and edge refinement of master mesh object.
C   Fixed refinement capability that existed within addmesh_add.
C   Now the "background" mesh is refined around the area that
C   overlaps with the "incoming" mesh. The number of refinement
C   iterations can be specified by the user in the following format:
C   addmesh/add/mesh_added/mesh1/mesh2/num_refine_iterations/refine_type
C   mesh_added is the new name of the resulting mesh object
C   mesh1 is the "background" mesh
C   mesh2 is the "incoming" mesh
C   num_refine_iterations specifies the number of times that the
C            background mesh will be refined.  If this number
C            is negative, or if it does not appear, then the it
C            will go with the default.  The default method determines
C            the number of refinement iterations based on the volumes of
C            the tets.  It continues to refine until the elements on the
C            interface boundary of the background mesh object are within
C            a given factor of the volume of the elements on the border
C            of the incoming mesh object.  This factor is a parameter
C            constant called size_difference in the program
C            continue_refinement.f.  For example, if size_difference is
C            set to 5.0, then the background mesh will be refined until
C            the maximum volume element on the boundary with the incoming
C            mesh object is no bigger than 5 times the volume of the
C            maximum volume element on the border of the incoming mesh object.
C
C   refine_type is the type of refinement that is executed.  If the string
C   'tet' appears, then tetrahedral refinement is performed.  Otherwise,
C   edge based refinement is performed.
C
C
C ######################################################################
C
      implicit none
C
      include 'chydro.h'
      include 'local_element.h'
C
C
C ######################################################################
C
      character*(*) cmoc, cmob
      character*(*) refine_type
      integer ierror
C
C ######################################################################
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer imt1(*),itp1(*)
      pointer (ipitettyp1, itettyp1)
      pointer (ipitetclr1, itetclr1)
      integer itettyp1(*),itetclr1

      pointer (ipitetoff1, itetoff1)
      pointer (ipjtetoff1, jtetoff1)
      integer itetoff1(*),jtetoff1(*)

      pointer (ipitet1, itet1)
      pointer (ipjtet1, jtet1)
      integer itet1(4,*),jtet1(4,*)
C
      pointer (ipitest, itest)
      integer itest(*)
C
      pointer (ipitettypb, itettypb)
      pointer (ipjtetoffb, jtetoffb)
      pointer (ipjtet1b, jtet1b)
      integer itettypb(*),jtetoffb(*),jtet1b(*)
C
      pointer (ipitetclrc, itetclrc)
      pointer (ipitettypc, itettypc)
      pointer (ipitetoffc, itetoffc)
      pointer (ipjtetoffc, jtetoffc)
      integer itetclrc(*),itettypc(*),itetoffc(*),jtetoffc(*)
      pointer (ipitet1c, itet1c)
      pointer (ipjtet1c, jtet1c)
      integer itet1c(*),jtet1c(*)
C
      pointer (ipimt1d, imt1d)
      integer imt1d(*)
C
      pointer (ipitettypd, itettypd)
      pointer (ipitetclrd, itetclrd)
      integer itettypd(*), itetclrd(*)
      pointer (ipitet1d, itet1d)
      pointer (ipjtet1d, jtet1d)
      pointer (ipitetoffd, itetoffd)
      pointer (ipjtetoffd, jtetoffd)
      integer itet1d(*),jtet1d(*),itetoffd(*),jtetoffd(*)
C
      pointer (ipxic1, xic1)
      pointer (ipyic1, yic1)
      pointer (ipzic1, zic1)
      real*8 xic1(*), yic1(*), zic1(*)

      pointer (ipxic1d, xic1d)
      pointer (ipyic1d, yic1d)
      pointer (ipzic1d, zic1d)
      integer xic1d(*), yic1d(*), zic1d(*)
C
C ######################################################################
C
      character*132 logmess
      character*32 isubname, cmonam, cmod
      character*32 itopo
C
C ######################################################################
C
      pointer (ipipoint_overlap1, ipoint_overlap1)
      pointer (ipitet_overlap1, itet_overlap1)
      pointer (ipitet_border1, itet_border1)
      integer ipoint_overlap1(*)
      integer itet_overlap1(*), itet_border1(*)
C
      pointer (ipipoint_overlap2, ipoint_overlap2)
      pointer (ipitet_overlap2, itet_overlap2)
      pointer (ipitet_border2, itet_border2)
      integer ipoint_overlap2(*)
      integer itet_overlap2(*), itet_border2(*)
C
      pointer (ipitet_delete, itet_delete)
      integer itet_delete(*)
C
      logical cont
      logical all_tets
C
C ######################################################################
C
      pointer (ipitadd, itadd)
      pointer (ipieadd, ieadd)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      integer itadd(*), ieadd(*)
      real*8 xadd(*), yadd(*), zadd(*)
      pointer (ipiadd, iadd)
      integer iadd(*)
C
      integer nuser_refine,jdebug,istitch,icscode,npoints1,
     *        ilen,icmotp,numtet1,lenitettyp,ier,npoints2,
     *        numtet2,length,i1,it,nrefine,i,ierrw,ics,nsdtopo1,
     *        icmotype,nsdgeom1,nen1,nef1,lenimt1,lenitp1,lenxic1,
     *        lenyic1,lenzic1,lenitetclr,lenitet,lenjtet,len,ierr,
     *        lenitetoff,lenjtetoff,nadd,i2,i3,i4,npoints_save,npoints,
     *        ref_distance,nadd1,ie,j1,j2,j,distance,numtet_save,
     *        itp1_boundary,nptsc,nelemc,
     *        lenitetc,lenjtetc,ifacemax,nelemd,lenimt1d,
     *        lenyic1d,lenzic1d,lenitetd,lenjtetd,itoffd,jtoffd,
     *        lenjtetoffc, lenxic1d

      integer mbndry,mbndry_old,mbndry_new

      real*8 xedge,yedge,zedge

      integer icharlnf
C
      integer itet_edge(2,6)
      data itet_edge / 1, 2,
     *                 1, 3,
     *                 1, 4,
     *                 2, 3,
     *                 2, 4,
     *                 3, 4 /
C
C
C ######################################################################
C
      data jdebug / 0 /
      data istitch / 1 /
C
C ######################################################################
C BEGIN begin
C
      isubname='addmesh_add'
C
      call cmo_select(cmoc,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_select')
 
CCC ** added by L. Lundquist, 8/15/96
CCC    USE addmesh overlap before beginning refinement to see if
CC     refinement needs to be done
C
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierror)
      call cmo_get_info('mbndry',cmoc,mbndry,ilen,icmotp,ierror)
      call cmo_get_info('itettyp',cmoc,
     *                  ipitettyp1,lenitettyp,icmotp,ier)
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierror)
      length=npoints1
         call mmgetblk('ipoint_overlap1',isubname,
     *                 ipipoint_overlap1,length,2,icscode)
         length=numtet1
         call mmgetblk('itet_overlap1',isubname,
     *                 ipitet_overlap1,length,2,icscode)
         call mmgetblk('itet_border1',isubname,
     *                 ipitet_border1,length,2,icscode)
         do i1=1,npoints1
            ipoint_overlap1(i1)=0
         enddo
         do it=1,numtet1
            itet_overlap1(it)=0
            itet_border1(it)=0
         enddo
         length=npoints2
         call mmgetblk('ipoint_overlap2',isubname,
     *                 ipipoint_overlap2,length,2,icscode)
         length=numtet2
         call mmgetblk('itet_overlap2',isubname,
     *                 ipitet_overlap2,length,2,icscode)
         call mmgetblk('itet_border2',isubname,
     *                 ipitet_border2,length,2,icscode)
         do i1=1,npoints2
            ipoint_overlap2(i1)=0
         enddo
         do it=1,numtet2
            itet_overlap2(it)=0
            itet_border2(it)=0
         enddo
C
C     *************************************************************
C     FIND OVERLAP OF TWO MESH OBJECTS
C
         call addmesh_overlap(cmoc,cmob,
     *        ipipoint_overlap1,ipitet_overlap1,ipitet_border1,
     *        ipipoint_overlap2,ipitet_overlap2,ipitet_border2,
     *        ierror)
 
C
C     *************************************************************
C     REFINEMENT OF MASTER MESH OBJECT
C     (Beginning of do loop moved by L. Lundquist 8/9/96.  It used
C      to be after all of the calls to cmo_get_info.  It was moved
C      so that the cmo information could be updated after a refine
C      was performed.)
C
      nrefine = 0
      call continue_refinement (cmoc, cmob, nuser_refine, nrefine,
     *                          ipitet_border1, cont)
C
C   Check to make sure that all elements are tets before attempt refinement.
C
      all_tets = .true.

      if (numtet1.le.0 ) then
          write (logmess,'(a)') 
     *    'No refinement attempted - 0 elements.'
          call writloga ('default',0,logmess,0,ierrw)
          all_tets = .false.
      endif

      do i=1, numtet1
         if (itettyp1(i).ne.5) then
            all_tets = .false.
         endif
      enddo
      if (.not.all_tets ) then
      	  write (logmess,'(a,a)') 'No refinement attempted.',
     *                       'All elements must be tets.'
      	  call writloga ('default',0,logmess,0,ierrw)
      endif
      do while (cont  .and. all_tets )
         nrefine = nrefine + 1

         write (logmess,'(a,i10,a)')
     *   'ADDMESH ADD: ', nrefine-1, ' refine iteration.'
         call writloga ('default',0,logmess,0,ierrw)

         call mmrelblk('ipoint_overlap1',isubname,ipipoint_overlap1,ics)
         call mmrelblk('itet_overlap1',isubname,ipitet_overlap1,icscode)
         call mmrelblk('itet_border1',isubname,ipitet_border1,icscode)
         call mmrelblk('ipoint_overlap2',isubname,ipipoint_overlap2,ics)
         call mmrelblk('itet_overlap2',isubname,ipitet_overlap2,icscode)
         call mmrelblk('itet_border2',isubname,ipitet_border2,icscode)
C
C
         call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
         call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierror)
         call cmo_get_info('mbndry',cmoc,mbndry,ilen,icmotp,ierror)
         call cmo_get_info('ndimensions_topo',cmoc,
     *                  nsdtopo1,length,icmotype,ierror)
         call cmo_get_info('ndimensions_geom',cmoc,
     *                  nsdgeom1,length,icmotype,ierror)
         call cmo_get_info('nodes_per_element',cmoc,
     *                  nen1,length,icmotype,ierror)
         call cmo_get_info('faces_per_element',cmoc,
     *                  nef1,length,icmotype,ierror)
         call cmo_get_info('imt1',cmoc,ipimt1,lenimt1,icmotp,ierror)
         call cmo_get_info('itp1',cmoc,ipitp1,lenitp1,icmotp,ierror)
         call cmo_get_info('xic' ,cmoc,ipxic1,lenxic1,icmotp,ierror)
         call cmo_get_info('yic' ,cmoc,ipyic1,lenyic1,icmotp,ierror)
         call cmo_get_info('zic' ,cmoc,ipzic1,lenzic1,icmotp,ierror)
         call cmo_get_info('itetclr',cmoc,
     *                  ipitetclr1,lenitetclr,icmotp,ier)
         call cmo_get_info('itettyp',cmoc,
     *                  ipitettyp1,lenitettyp,icmotp,ier)
         call cmo_get_info('itetoff',cmoc,
     *                  ipitetoff1,lenitetoff,icmotp,ier)
         call cmo_get_info('jtetoff',cmoc,
     *                  ipjtetoff1,lenjtetoff,icmotp,ier)
         call cmo_get_info('itet',cmoc,ipitet1,lenitet,icmotp,ierror)
         call cmo_get_info('jtet',cmoc,ipjtet1,lenjtet,icmotp,ierror)
C
C     *************************************************************
C     GET INFO ABOUT CMOB, SLAVE MESH OBJECT
C     (next two lines of code added by L. Lundquist, 8/1)
C
         call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
         call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierror)
 
CCCCCCCCCCCCCCCCCCCCCCCCCC
C     Beginning of refinement do loop used to be here.  Moved to
C     above location by L. Lundquist 8/9/96.  See above comments.
C
         length=npoints1
         call mmgetblk('ipoint_overlap1',isubname,
     *                 ipipoint_overlap1,length,2,icscode)
         length=numtet1
         call mmgetblk('itet_overlap1',isubname,
     *                 ipitet_overlap1,length,2,icscode)
         call mmgetblk('itet_border1',isubname,
     *                 ipitet_border1,length,2,icscode)
         do i1=1,npoints1
            ipoint_overlap1(i1)=0
         enddo
         do it=1,numtet1
            itet_overlap1(it)=0
            itet_border1(it)=0
         enddo
         length=npoints2
         call mmgetblk('ipoint_overlap2',isubname,
     *                 ipipoint_overlap2,length,2,icscode)
         length=numtet2
         call mmgetblk('itet_overlap2',isubname,
     *                 ipitet_overlap2,length,2,icscode)
         call mmgetblk('itet_border2',isubname,
     *                 ipitet_border2,length,2,icscode)
         do i1=1,npoints2
            ipoint_overlap2(i1)=0
         enddo
         do it=1,numtet2
            itet_overlap2(it)=0
            itet_border2(it)=0
         enddo
C
C     *************************************************************
C     FIND OVERLAP OF TWO MESH OBJECTS
C
         call addmesh_overlap(cmoc,cmob,
     *        ipipoint_overlap1,ipitet_overlap1,ipitet_border1,
     *        ipipoint_overlap2,ipitet_overlap2,ipitet_border2,
     *        ierror)
C
         itopo=refine_type
         len=icharlnf(itopo)
C
C     *************************************************************
C     TET REFINEMENT
C
         if(itopo(1:len).eq.'tet') then
            write(logmess,'(a)') "refine tets: "
            call writloga('default',0,logmess,0,ierrw)
C
            call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
            call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierr)
            call cmo_get_info('imt1',cmoc,ipimt1,lenimt1,icmotp,ierror)
            call cmo_get_info('itp1',cmoc,ipitp1,lenitp1,icmotp,ierror)
            call cmo_get_info('xic' ,cmoc,ipxic1,lenxic1,icmotp,ierror)
            call cmo_get_info('yic' ,cmoc,ipyic1,lenyic1,icmotp,ierror)
            call cmo_get_info('zic' ,cmoc,ipzic1,lenzic1,icmotp,ierror)
            call cmo_get_info('itetclr',cmoc,
     *                        ipitetclr1,lenitetclr,icmotp,ier)
            call cmo_get_info('itettyp',cmoc,
     *                        ipitettyp1,lenitettyp,icmotp,ier)
            call cmo_get_info('itetoff',cmoc,
     *                        ipitetoff1,lenitetoff,icmotp,ier)
            call cmo_get_info('jtetoff',cmoc,
     *                        ipjtetoff1,lenjtetoff,icmotp,ier)
            call cmo_get_info('itet',cmoc,ipitet1,lenitet,icmotp,ierror)
            call cmo_get_info('jtet',cmoc,ipjtet1,lenjtet,icmotp,ierror)
C
            length=numtet1
            call mmgetblk("itadd",isubname,ipitadd,length,2,icscode)
            call mmgetblk("iadd",isubname,ipiadd,length,2,icscode)
            call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
            call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
            call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
C
            nadd=0
            do it=1,numtet1
               if(itet_border1(it).ne.0) then
                  nadd=nadd+1
                  itadd(nadd)=it
               endif
            enddo
            do i=1,nadd
               it=itadd(i)
               i1=itet1(1,it)
               i2=itet1(2,it)
               i3=itet1(3,it)
               i4=itet1(4,it)
               iadd(i)=0
               xadd(i)=0.25*(xic1(i1)+xic1(i2)+xic1(i3)+xic1(i4))
               yadd(i)=0.25*(yic1(i1)+yic1(i2)+yic1(i3)+yic1(i4))
               zadd(i)=0.25*(zic1(i1)+zic1(i2)+zic1(i3)+zic1(i4))
            enddo
            write(logmess,'(a,i10)') "refine tets: ",nadd
            call writloga('default',0,logmess,0,ierrw)
            if(nadd.gt.0) then
               npoints_save=npoints
               call refine_tet_add(cmoc,
     *                             nadd,
     *                             ipitadd,
     *                             iadd,xadd,yadd,zadd)
            endif
            call mmrelblk("itadd",isubname,ipitadd,icscode)
            call mmrelblk("iadd",isubname,ipiadd,icscode)
            call mmrelblk("xadd",isubname,ipxadd,icscode)
            call mmrelblk("yadd",isubname,ipyadd,icscode)
            call mmrelblk("zadd",isubname,ipzadd,icscode)
C
C     *************************************************************
C     EDGE REFINEMENT
C
         elseif(itopo(1:len).eq.'edges') then
            write(logmess,'(a)') "refine tet edges: "
            call writloga('default',0,logmess,0,ierrw)
C
            call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
            call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierr)
            call cmo_get_info('imt1',cmoc,ipimt1,lenimt1,icmotp,ierror)
            call cmo_get_info('itp1',cmoc,ipitp1,lenitp1,icmotp,ierror)
            call cmo_get_info('xic' ,cmoc,ipxic1,lenxic1,icmotp,ierror)
            call cmo_get_info('yic' ,cmoc,ipyic1,lenyic1,icmotp,ierror)
            call cmo_get_info('zic' ,cmoc,ipzic1,lenzic1,icmotp,ierror)
            call cmo_get_info('itetclr',cmoc,
     *                        ipitetclr1,lenitetclr,icmotp,ier)
            call cmo_get_info('itettyp',cmoc,
     *                        ipitettyp1,lenitettyp,icmotp,ier)
            call cmo_get_info('itetoff',cmoc,
     *                        ipitetoff1,lenitetoff,icmotp,ier)
            call cmo_get_info('jtetoff',cmoc,
     *                        ipjtetoff1,lenjtetoff,icmotp,ier)
            call cmo_get_info('itet',cmoc,ipitet1,lenitet,icmotp,ierror)
            call cmo_get_info('jtet',cmoc,ipjtet1,lenjtet,icmotp,ierror)
C
            length=6*numtet1
            call mmgetblk("itadd",isubname,ipitadd,length,2,icscode)
            call mmgetblk("ieadd",isubname,ipieadd,length,2,icscode)
            call mmgetblk("iadd",isubname,ipiadd,length,2,icscode)
            call mmgetblk("xadd",isubname,ipxadd,length,2,icscode)
            call mmgetblk("yadd",isubname,ipyadd,length,2,icscode)
            call mmgetblk("zadd",isubname,ipzadd,length,2,icscode)
C
            nadd=0
            do it=1,numtet1
C*****         if(itet_overlap1(it).lt.0) then
               if(itet_border1(it).ne.0) then
                  do i=1,6
                     nadd=nadd+1
                     itadd(nadd)=it
                     ieadd(nadd)=i
                  enddo
               endif
            enddo
            ref_distance=1.0d+00
            nadd1=0
            do i=1,nadd
               it=itadd(i)
               ie=ieadd(i)
               i1=itet1(1,it)
               i2=itet1(2,it)
               i3=itet1(3,it)
               i4=itet1(4,it)
               j1=itet1(itet_edge(1,ie),it)
               j2=itet1(itet_edge(2,ie),it)
               xedge=(xic1(j1)+xic1(j2))/2.0d+00
               yedge=(yic1(j1)+yic1(j2))/2.0d+00
               zedge=(zic1(j1)+zic1(j2))/2.0d+00
               if(nadd1.gt.0) then
                  do j=1,nadd1
                     if(it.eq.itadd(j)) goto 302
                     distance=(xedge-xadd(j))**2 +
     *                        (yedge-yadd(j))**2 +
     *                        (zedge-zadd(j))**2
                     if(distance.lt.1.0d-06*ref_distance) goto 302
                  enddo
               endif
               nadd1=nadd1+1
               itadd(nadd1)=it
               ieadd(nadd1)=ie
               iadd(nadd1)=0
               xadd(nadd1)=xedge
               yadd(nadd1)=yedge
               zadd(nadd1)=zedge
 302           continue
            enddo
            nadd=nadd1
            write(logmess,'(a,i10)') "Refine edges: ",nadd
            call writloga('default',0,logmess,0,ierrw)
            if(nadd.gt.0) then
               npoints_save=npoints1
               numtet_save=numtet1
CCCCCCCCCCCCCCCCCCCCCCC
C   Change by Loraine Lundquist 8/7/96
C   in call to refine_edge_add,
C   changed arguments iadd,xadd,yadd,zadd to ipiadd,ipxadd,ipyadd,ipzadd
C
               call refine_edge_add(cmoc,
     *                              nadd,
     *                              ipitadd,ipieadd,
     *                              ipiadd,ipxadd,ipyadd,ipzadd)
               call mmrelblk("itadd",isubname,ipitadd,icscode)
               call mmrelblk("ieadd",isubname,ipieadd,icscode)
               call mmrelblk("iadd",isubname,ipiadd,icscode)
               call mmrelblk("xadd",isubname,ipxadd,icscode)
               call mmrelblk("yadd",isubname,ipyadd,icscode)
               call mmrelblk("zadd",isubname,ipzadd,icscode)
            endif
            if (jdebug.eq.1) goto 9998
	    call dotaskx3d ("recon/0 ; finish", ierror)
            if (ierror.ne.0) then
              write(logmess,'(a,i5)') 
     *        'ADDMESH ADD: recon finished with error: ',ierror
              call writloga('default',0,logmess,0,ierrw)
            else
              write(logmess,'(a)')'ADDMESH ADD: recon done.'
              call writloga('default',0,logmess,0,ierrw)
            endif
C
C     ENDIF EDGE REFINEMENT
         endif
      call continue_refinement (cmoc, cmob, nuser_refine, nrefine,
     *                          ipitet_border1, cont)
C
C
C     END REFINEMENT LOOP
      enddo
C
C  L. Lundquist 8/96
C  release of memory blocks moved to beginning of loop in order to
C  release memory blocks after first call of addmesh_overlap.
C  These calls added to release them again at the end of the loop
C
         call mmrelblk('ipoint_overlap1',isubname,ipipoint_overlap1,ics)
         call mmrelblk('itet_overlap1',isubname,ipitet_overlap1,icscode)
         call mmrelblk('itet_border1',isubname,ipitet_border1,icscode)
         call mmrelblk('ipoint_overlap2',isubname,ipipoint_overlap2,ics)
         call mmrelblk('itet_overlap2',isubname,ipitet_overlap2,icscode)
         call mmrelblk('itet_border2',isubname,ipitet_border2,icscode)
         if(jdebug.eq.1) goto 9998
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierr)
      length=npoints1
      call mmgetblk('ipoint_overlap1',isubname,
     *              ipipoint_overlap1,length,2,icscode)
      length=numtet1
      call mmgetblk('itet_overlap1',isubname,
     *              ipitet_overlap1,length,2,icscode)
      call mmgetblk('itet_border1',isubname,
     *              ipitet_border1,length,2,icscode)
      do i1=1,npoints1
         ipoint_overlap1(i1)=0
      enddo
      do it=1,numtet1
         itet_overlap1(it)=0
         itet_border1(it)=0
      enddo
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierr)
      length=npoints2
      call mmgetblk('ipoint_overlap2',isubname,
     *              ipipoint_overlap2,length,2,icscode)
      length=numtet2
      call mmgetblk('itet_overlap2',isubname,
     *              ipitet_overlap2,length,2,icscode)
      call mmgetblk('itet_border2',isubname,
     *              ipitet_border2,length,2,icscode)
      do i1=1,npoints2
         ipoint_overlap2(i1)=0
      enddo
      do it=1,numtet2
         itet_overlap2(it)=0
         itet_border2(it)=0
      enddo
      call addmesh_overlap(cmoc,cmob,
     *     ipipoint_overlap1,ipitet_overlap1,ipitet_border1,
     *     ipipoint_overlap2,ipitet_overlap2,ipitet_border2,
     *     ierror)
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierr)
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierr)
      length=max(numtet1,numtet2)
      call mmgetblk('itet_delete',isubname,ipitet_delete,length,2,
     *              icscode)
      do it=1,numtet1
         itet_delete(it)=0
      enddo
      do it=1,numtet1
         if(itet_overlap1(it).lt.0) then
            itet_delete(it)=-itet_overlap1(it)
         else
            itet_delete(it)=0
         endif
      enddo
      mbndry_old=mbndry
      if(istitch.eq.1) then
         mbndry_new=-1
      else
         mbndry_new=mbndry
      endif
      itp1_boundary=ifitpini
      call addmesh_delete(cmoc,
     *                    mbndry_old,
     *                    mbndry_new,
     *                    itp1_boundary,
     *                    ipitet_delete,
     *                    ierror)
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierr)
      do it=1,numtet2
         itet_delete(it)=0
      enddo
      do it=1,numtet2
         if(itet_overlap2(it).lt.0) then
            itet_delete(it)=-itet_overlap2(it)
         else
            itet_delete(it)=0
         endif
      enddo
      mbndry_old=mbndry
      mbndry_new=-1
      itp1_boundary=ifitpini
      call addmesh_delete(cmob,
     *                    mbndry_old,
     *                    mbndry_new,
     *                    itp1_boundary,
     *                    ipitet_delete,
     *                    ierror)
C
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierr)
      call cmo_get_info('itettyp',cmob,ipitettypb,lenitettyp,icmotp,ier)
      call cmo_get_info('jtetoff',cmob,ipjtetoffb,lenjtetoff,icmotp,ier)
      call cmo_get_info('jtet',cmob,ipjtet1b,lenjtet,icmotp,ierror)
      do it=1,numtet2
         do i=1,nelmnef(itettypb(it))
            if(jtet1b(jtetoffb(it)+i).eq.mbndry_old) then
               jtet1b(jtetoffb(it)+i)=mbndry_new
            endif
         enddo
      enddo
C
      call addmesh_merge(cmoc,cmob,ierror)
C
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierr)
      call cmo_get_info('itp1',cmoc,ipitp1,lenitet,icmotp,ierror)
      call cmo_get_info('itettyp',cmoc,ipitettypc,lenitettyp,icmotp,ier)
      call cmo_get_info('itetoff',cmoc,ipitetoffc,lenitetoff,icmotp,ier)
      call cmo_get_info('itet',cmoc,ipitet1c,lenitet,icmotp,ierror)
C
      length=npoints1
      call mmgetblk("itest",isubname,ipitest,length,2,icscode)
      do i1=1,npoints1
         itest(i1)=0
      enddo
      do it=1,numtet1
         do i=1,nelmnen(itettypc(it))
            itest(itet1c(itetoffc(it)+i))=1
         enddo
      enddo
      do i1=1,npoints1
         if(itest(i1).eq.0) itp1(i1)=ifitpdud
      enddo
C
 9998 continue
C
      call cmo_get_name(cmonam,ierror)
      call cmo_set_info('nnodes',cmonam,npoints1,1,1,ier)
      call cmo_set_info('nelements',cmonam,numtet1,1,1,ier)
C
C ###################################################################
C     All of the following lines were added by L. Lundquist
C     and H. Trease, 8/21/96.  They are meant to produce an output
C     file in avs format that contains two surfaces: The inner boundary
C     of the background mesh, and the outer boundary of the incoming
C     mesh.  The output file is for use in reconnecting the two meshes
C     that have been added.
C
C
      call cmo_get_info('nnodes',cmoc,nptsc,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,nelemc,ilen,icmotp,ierror)
      call cmo_get_info('itetclr',cmoc,ipitetclrc,
     *                   lenitetclr,icmotp,ierror)
      call cmo_get_info('itettyp',cmoc,ipitettypc,
     *                   lenitettyp,icmotp,ierror)
      call cmo_get_info('itetoff',cmoc,
     *                   ipitetoffc,lenjtetoffc,icmotp,ierror)
      call cmo_get_info('jtetoff',cmoc,
     *                   ipjtetoffc,lenjtetoffc,icmotp,ierror)
      call cmo_get_info('itet',cmoc,ipitet1c,lenitetc,icmotp,ierror)
      call cmo_get_info('jtet',cmoc,ipjtet1c,lenjtetc,icmotp,ierror)
C
      ifacemax=0
      nelemd = 0
      do it=1, nelemc
         do i=1, nelmnef(itettypc(it))
            if ((jtet1c(jtetoffc(it)+i)) .lt. 0) then
               nelemd = nelemd + 1
               ifacemax=max(ifacemax,ielmface0(i,itettypc(it)))
            endif
         enddo
      enddo
C
      cmod = 'cmod'
      if(ifacemax.eq.2) then
         call dotaskx3d( "cmo/derive/"//cmod//cmoc//"0/0/lin ;finish",
     *                   ierror)
      elseif(ifacemax.eq.3) then
         call dotaskx3d( "cmo/derive/"//cmod//cmoc//"0/0/tri ;finish",
     *                   ierror)
      elseif(ifacemax.eq.4) then
         call dotaskx3d( "cmo/derive/"//cmod//cmoc//"0/0/quad ;finish",
     *                   ierror)
      else
         goto 9997
      endif
C
      call cmo_set_info('nnodes',cmod,nptsc,1,1,ierror)
      call cmo_set_info('nelements',cmod,nelemd,1,1,ierror)
      call cmo_newlen(cmod,ierror)
      call cmo_get_info('imt1',cmod,ipimt1d,lenimt1d,icmotp,ierror)
      call cmo_get_info('xic' ,cmod,ipxic1d,lenxic1d,icmotp,ierror)
      call cmo_get_info('yic' ,cmod,ipyic1d,lenyic1d,icmotp,ierror)
      call cmo_get_info('zic' ,cmod,ipzic1d,lenzic1d,icmotp,ierror)
      call cmo_get_info('itetclr',cmod,
     *                   ipitetclrd,lenitetclr,icmotp,ierror)
      call cmo_get_info('itettyp',cmod,
     *                   ipitettypd,lenitettyp,icmotp,ierror)
      call cmo_get_info('itetoff',cmod,
     *                   ipitetoffd,lenitetoff,icmotp,ierror)
      call cmo_get_info('jtetoff',cmod,
     *                   ipjtetoffd,lenjtetoffc,icmotp,ierror)
      call cmo_get_info('itet',cmod,ipitet1d,lenitetd,icmotp,ierror)
      call cmo_get_info('jtet',cmod,ipjtet1d,lenjtetd,icmotp,ierror)
C
      call cmo_get_info('imt1',cmoc,ipimt1,lenimt1,icmotp,ierror)
      call cmo_get_info('xic' ,cmoc,ipxic1,lenxic1,icmotp,ierror)
      call cmo_get_info('yic' ,cmoc,ipyic1,lenyic1,icmotp,ierror)
      call cmo_get_info('zic' ,cmoc,ipzic1,lenzic1,icmotp,ierror)
      do i1=1,nptsc
         imt1d(i1)=imt1(i1)
         xic1d(i1)=xic1(i1)
         yic1d(i1)=yic1(i1)
         zic1d(i1)=zic1(i1)
      enddo
C
      itoffd=0
      jtoffd=0
      nelemd=0
      do it=1, nelemc
         do i=1, nelmnef(itettypc(it))
            if ((jtet1c(jtetoffc(it) + i)) .lt. 0) then
               if(ielmface0(i,itettypc(it)).eq.2) then
                  i2 = itet1c(itetoffc(it) +
     *                        ielmface1(1,i, itettypc(it)))
                  i3 = itet1c(itetoffc(it) +
     *                        ielmface1(2,i, itettypc(it)))
                  nelemd=nelemd+1
                  itetoffd(nelemd)=itoffd
                  jtetoffd(nelemd)=jtoffd
                  itoffd=itoffd + nelmnen(ifelmlin)
                  jtoffd=jtoffd + nelmnef(ifelmlin)
                  itetclrd(nelemd)=itetclrc(it)
                  itettypd(nelemd)=ifelmlin
                  itet1d(itetoffd(nelemd) + 1) = i2
                  itet1d(itetoffd(nelemd) + 2) = i3
                  jtet1d(jtetoffd(nelemd) + 1) = -1
                  jtet1d(jtetoffd(nelemd) + 2) = -1
               elseif(ielmface0(i,itettypc(it)).eq.3) then
                  i2 = itet1c(itetoffc(it) +
     *                        ielmface1(1,i, itettypc(it)))
                  i3 = itet1c(itetoffc(it) +
     *                        ielmface1(2,i, itettypc(it)))
                  i4 = itet1c(itetoffc(it) +
     *                        ielmface1(3,i, itettypc(it)))
                  nelemd=nelemd+1
                  itetoffd(nelemd)=itoffd
                  jtetoffd(nelemd)=jtoffd
                  itoffd=itoffd + nelmnen(ifelmtri)
                  jtoffd=jtoffd + nelmnef(ifelmtri)
                  itetclrd(nelemd)=itetclrc(it)
                  itettypd(nelemd)=ifelmtri
                  itet1d(itetoffd(nelemd) + 1) = i2
                  itet1d(itetoffd(nelemd) + 2) = i3
                  itet1d(itetoffd(nelemd) + 3) = i4
                  jtet1d(jtetoffd(nelemd) + 1) = -1
                  jtet1d(jtetoffd(nelemd) + 2) = -1
                  jtet1d(jtetoffd(nelemd) + 3) = -1
               elseif(ielmface0(i,itettypc(it)).eq.4) then
                  i1 = itet1c(itetoffc(it) +
     *                        ielmface1(1,i, itettypc(it)))
                  i2 = itet1c(itetoffc(it) +
     *                        ielmface1(2,i, itettypc(it)))
                  i3 = itet1c(itetoffc(it) +
     *                        ielmface1(3,i, itettypc(it)))
                  i4 = itet1c(itetoffc(it) +
     *                        ielmface1(4,i, itettypc(it)))
                  nelemd=nelemd+1
                  itetoffd(nelemd)=itoffd
                  jtetoffd(nelemd)=jtoffd
                  itoffd=itoffd + nelmnen(ifelmqud)
                  jtoffd=jtoffd + nelmnef(ifelmqud)
                  itetclrd(nelemd)=itetclrc(it)
                  itettypd(nelemd)=ifelmqud
                  itet1d(itetoffd(nelemd) + 1) = i1
                  itet1d(itetoffd(nelemd) + 2) = i2
                  itet1d(itetoffd(nelemd) + 3) = i3
                  itet1d(itetoffd(nelemd) + 4) = i4
                  jtet1d(jtetoffd(nelemd) + 1) = -1
                  jtet1d(jtetoffd(nelemd) + 2) = -1
                  jtet1d(jtetoffd(nelemd) + 3) = -1
                  jtet1d(jtetoffd(nelemd) + 4) = -1
               endif
            endif
         enddo
      enddo
      call geniee_cmo (cmod)
      call dotaskx3d("dump/avs/polygon.inp/" //cmod// ";finish", ierror)
      call dotaskx3d("dump/gmv/polygon.gmv/" //cmod// ";finish", ierror)
C
      call cmo_select(cmoc,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_select')
C
      goto 9997
 9997 continue
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
*dk,continue_refinement
      subroutine continue_refinement(cmoc,
     *                               cmob,
     *                               nuser_refine,
     *                               nrefine,
     *                               ipitet_border1,
     *				     cont)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine determines whether the refinement of cmoc should
C        continue.  If nuser_refine is less than 0, then the function
C        determines whether refinement should continue based on the volume
C   	 of the elements: If the largest element of the background mesh
C        bordering with the incoming mesh is more than size_difference
C        greater than the largest element on the border of the incoming
C        mesh, then refinement should continue, and cont is set to .TRUE.
C        Otherwise, the elements on the borders of the two objects are
C        similar in size, and cont is set to .FALSE. The volume difference
C        factor is a parameter named size_difference, and is hardwired
C        into the code.
C
C     INPUT ARGUMENTS -
C
C        cmoc    - The master (background) mesh_object (source1).
C        cmob    - The slave (incoming) mesh_object (source2).
C        nuser_refine - number of iterations specified by user;
C                        negative if no user specification
C        nrefine - number of refinement iterations completed
C        itet_border1 - array indicating if tets in cmoc border on cmob:
C                       0  if not on border
C                       1  if on border and outside cmob
C                       -1 if on border and inside cmob
C
C     OUTPUT ARGUMENTS -
C
C        cont - indicates wheter refinement should continue.
C        cmoc   - The mesh_object (sink).
C
C     CHANGE HISTORY -
C  8/7/96 lund
C   first version
C
C ######################################################################
C
      implicit none 
      include "local_element.h"
C
C ######################################################################
C
      character*(*) cmoc, cmob
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
C     POINTERS TO MASTER MESH OBJECT
C
      pointer (ipxic1, xic1)
      pointer (ipyic1, yic1)
      pointer (ipzic1, zic1)
      pointer (ipitet1, itet1)
      pointer (ipitettyp1, itettyp1)
      pointer (ipitetoff1, itetoff1)
      pointer (ipitp1, itp1)
C
      real*8 xic1(*), yic1(*), zic1(*)
      integer itet1(*), itettyp1(*), itetoff1(*)
      integer itp1(*)

      real*8 xicvol1(8), yicvol1(8), zicvol1(8)
C
C     POINTERS TO SLAVE MESH OBJECT
C
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      pointer (ipitet2, itet2)
      pointer (ipitettyp2, itettyp2)
      pointer (ipitetoff2, itetoff2)
      pointer (ipitp2, itp2)
C
      real*8 xic2(*), yic2(*), zic2(*)
      integer itet2(*), itettyp2(*), itetoff2(*)
      integer itp2(*)
C
CCC
C 

      integer mbndry1,mbndry2

      pointer (ipitet_border1, itet_border1)
      integer  itet_border1(*)

      real*8 xicvol2(8), yicvol2(8), zicvol2(8)

      real*8 rmax_vol1,rmax_vol2,volelm

      integer nuser_refine,nrefine,icscode,npoints1,ilen,icmotp,
     *        ierror,numtet1,lenxic1,lenyic1,lenzic1,lenitettyp,
     *        ier,npoints2,numtet2,lenxic2,lenyic2,lenzic2,
     *        nboundary_elems,it,i,i1,ierrw

      logical cont
      logical boundary

      character*132 logmess
      character*32 isubname
C
C ######################################################################
C
C     SIZE_DIFFERENCE REPRESENTS THE MAXIMUM SIZE (VOLUME) DIFFERENCE
C     THAT CAN EXIST BETWEEN THE LARGEST ELEMENT ON THE BOUNDARY OF
C     THE MASTER MESH AND THE LARGEST ELEMENT ON THE BOUNDARY OF THE
C     SLAVE MESH
C
      real*8 size_difference
      parameter (size_difference = 5.0)
C     BEGIN_ AND END_BOUNDARY REPRESENT THE BEGINNING AND END OF THE
C     SPECTRUM OF NODAL LABELS IN THE ITP ARRAY THAT INDICATE THAT THE
C     NODE IS A BOUNDARY NODE.
C
      integer begin_boundary,end_boundary
      parameter (begin_boundary = 10)
      parameter (end_boundary = 19)
C
C ######################################################################
C
      isubname='continue_refinement'
C
C     IF THE USER HAS SPECIFIED THE NUMBER OF REFINEMENT ITERATIONS, THEN
C     RETURN cont=.TRUE. IF THEY HAVE BEEN COMPLETED AND .FALSE. IF THEY
C     HAVE NOT.
C
      if (nuser_refine .ge. 0) then
         if (nrefine .ge. nuser_refine) then
            cont = .false.
         else
            cont = .true.
         endif
         goto 9999
      endif
 
      call cmo_select(cmoc,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_select')
C
C     *************************************************************
C     MASTER MESH OBJECT INFORAMTION
C
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierror)
      call cmo_get_info('mbndry',cmoc,mbndry1,ilen,icmotp,ierror)
      call cmo_get_info('itp1',cmoc,ipitp1,ilen,icmotp,ierror)
      call cmo_get_info('xic' ,cmoc,ipxic1,lenxic1,icmotp,ierror)
      call cmo_get_info('yic' ,cmoc,ipyic1,lenyic1,icmotp,ierror)
      call cmo_get_info('zic' ,cmoc,ipzic1,lenzic1,icmotp,ierror)
      call cmo_get_info('itet',cmoc,ipitet1,ilen,icmotp,ierror)
      call cmo_get_info('itettyp',cmoc,
     *                  ipitettyp1,lenitettyp,icmotp,ier)
      call cmo_get_info('itetoff',cmoc,ipitetoff1,ilen,icmotp,ierror)
C
C     *************************************************************
C     SLAVE MESH OBJECT INFORMATION
C
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierror)
      call cmo_get_info('mbndry',cmob,mbndry2,ilen,icmotp,ierror)
      call cmo_get_info('itp1',cmob,ipitp2,ilen,icmotp,ierror)
      call cmo_get_info('xic' ,cmob,ipxic2,lenxic2,icmotp,ierror)
      call cmo_get_info('yic' ,cmob,ipyic2,lenyic2,icmotp,ierror)
      call cmo_get_info('zic' ,cmob,ipzic2,lenzic2,icmotp,ierror)
      call cmo_get_info('itet',cmob,ipitet2,ilen,icmotp,ierror)
      call cmo_get_info('itettyp',cmob,
     *                  ipitettyp2,lenitettyp,icmotp,ier)
      call cmo_get_info('itetoff',cmob,ipitetoff2,ilen,icmotp,ierror)
C
C     FIND THE ELEMENTS IN CMOC THAT BORDER ON THE INCOMING MESH
C
      nboundary_elems = 0
      rmax_vol1 = 0
      rmax_vol2 = 0
      do it=1, numtet1
         if (itet_border1(it).gt.0) then
	    nboundary_elems = nboundary_elems + 1
            do i=1,nelmnen(itettyp1(it))
               i1=itet1(itetoff1(it)+i)
               xicvol1(i)=xic1(i1)
               yicvol1(i)=yic1(i1)
               zicvol1(i)=zic1(i1)
            enddo
C
C     FIND MAXIMUM VOLUME ELEMENT ON INTERFACE BORDER OF CMOC
C
            call volume_element(itettyp1(it),
     *                          xicvol1,yicvol1,zicvol1,
     *                          volelm)
            if (volelm.gt.rmax_vol1) then
	       rmax_vol1 = volelm
	    endif
         endif
      enddo
C
C     FIND ELEMENTS IN CMOB THAT ARE ON THE BORDER.  IT IS ASSUMED THAT
C     ALL THE ELEMENTS THAT ARE ON ANY BORDER OF THIS OBJECT ARE ON THE
C     INTERFACE WITH THE BACKGROUND MESH.	
C
      nboundary_elems = 0
      do it=1, numtet2
         do i=1,nelmnen(itettyp2(it))
            i1=itet2(itetoff2(it)+i)
            xicvol2(i)=xic2(i1)
            yicvol2(i)=yic2(i1)
            zicvol2(i)=zic2(i1)
            if ((itp2(it).ge.begin_boundary).and.
     *          (itp2(it).le.end_boundary)) then
               boundary = .true.
            endif
         enddo
C
C     FIND MAXIMUM VOLUME ELEMENT ON BORDER OF CMOB
C
         if (boundary) then
            nboundary_elems = nboundary_elems + 1
            call volume_element(itettyp2(it),
     *                             xicvol2,yicvol2,zicvol2,
     *                             volelm)
            if (volelm.gt.rmax_vol2) then
               rmax_vol2 = volelm
            endif
         endif
      enddo
C
C     COMPARE VOLUMES: IF THEY ARE WITHIN SIZE_DIFFERENCE, THEN STOP
C     REFINING. OTHERWISE, CONTINUE REFINEMENT.
C
      if(rmax_vol2.le.0.0) then
          cont = .false.
          go to 9999
      endif
      write(logmess,*) 'Volume ratio = ', rmax_vol1/rmax_vol2
      call writloga('default',0,logmess,0,ierrw)
      if ((rmax_vol1/rmax_vol2).gt.size_difference) then
         cont = .true.
      else
         cont = .false.
      endif
C
 9999 return
      end
 
