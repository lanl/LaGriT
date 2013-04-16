      subroutine addmesh(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE ADDS TWO MESHES TOGETHER TO PRODUCE A
C           THIRD MESH.
C
C        FORMAT:  addmesh / add    / mesh3 / mesh1 / mesh2 / &
C                          [refine_factor] / [refine_type(tet,edge)
C                 addmesh / merge  / mesh3 / mesh1 / mesh2 /
C                 addmesh / glue   / mesh3 / mesh1 / mesh2 /
C                 addmesh / append / mesh3 / mesh1 / mesh2 /
C                 addmesh / delete / mesh3 / mesh1 / mesh2 /
C                 addmesh / intersect / pset_name / mesh1 / mesh2 /
C                 addmesh / amr    / mesh3 / mesh1 / mesh2 /
C                 addmesh / match / mesh3 / mesh1 / mesh1 / i1 12 i3 i4 i5 i6
C                 addmesh / match / mesh3 / mesh1 / mesh1 / rx1 ry1 rz1 &
C                                                           rx2 ry2 rz2 &
C                                                           rx3 ry3 rz3 &
C                                                           rx4 ry4 rz4 &
C                                                           rx5 ry5 rz5 &
C                                                           rx6 ry6 rz6
C                 addmesh / pyramid / mesh3 / mesh1 / mesh1 /
c                 addmesh /reflect/mesh3/mesh1/direction/origin/bleed
C                 addmesh / excavate / mesh3 / mesh1 / mesh2
C
C        add    - Find the intersection of mesh1 and mesh2. Refine mesh1
C                 where it overlaps mesh2 using the following criteria.
C                 refine_factor specifies the number of times that the
C                 background mesh will be refined. If this number
C                 is negative, or if it does not appear, then the it
C                 will go with the default.  The default method determines
C                 the number of refinement iterations based on the volumes of
C                 the tets.  It continues to refine until the elements on the
C                 interface boundary of the background mesh object are within
C                 a given factor (5.0) of the volume of the elements on the border
C                 of the incoming mesh object.  This factor is a parameter
C                 constant called size_difference in the program
C                 continue_refinement.f.  For example, if size_difference is
C                 set to 5.0, then the background mesh will be refined until
C                 the maximum volume element on the boundary with the incoming
C                 mesh object is no bigger than 5 times the volume of the
C                 maximum volume element on the border of the incoming mesh object.
C
C                 refine_type is the type of refinement that is executed.
C                 If the string 'tet' appears, then tetrahedral refinement
C                 is performed. Otherwise, edge based refinement is performed.
C
C                 After the above refine steps have been done, the intersection
C                 of mesh1 and mesh2 is found, elements that overlap are deleted
C                 from mesh1 and mesh2 is appended to mesh1 to create mesh3.
C
C        merge  - Append mesh2 to to mesh1 and create mesh3. Essentially
C                 this just concatenates two mesh objects.
C
C        glue   - Synonym for merge.
C
C        append - Append mesh2 to mesh1 and create mesh3. Similar to
C                 merge except imt, icr, itetclr of mesh2 have the value
C                 max(imt(mesh1)) added to mesh2.
C
C        delete - Create mesh3 with is mesh1 with elements that intersect
C                 mesh2 deleted.
C
C        intersect - Create a pset called pset_name that contains all
C                    nodes in mesh1 which intersect elements of mesh2.
C
C        amr    - Use Adaptive mesh refinement to connect background
C                 mesh1 with submesh mesh2 and create mesh3.
C
C        match  - Same as MERGE except the second mesh can be moved, rotated
C                 and translated. The first mesh does not move scale or rotate.
C                 If the interface needs to be scaled, translated and
C                 rotated that is accomplished by specifing 3 node
C                 numbers in each mesh or 3 node coordinates from each
C                 mesh that are to become coincident. If nodes are given
C                 match i1-i4, i2-i5, i3-i6. If coordinates are given
C                 match (x1,y1,z1)-(x4,y4,z4), etc.
C
C        pyramid - join a hex mesh to a tet mesh. The common surface
C                  must have matching nodes. There must be exactly two
C                  triangle faces on the tet grid that fit onto the quad
C                  face of the hex grid. The region where the two meshes
C                  join will have pyramid elements.
C
C        excavate - excavates out the main mesh around the surface,
C                      inserts the surface, and reconnects the two.
C                      Ensures an unbroken surface, and good quality
C                      tets around the the interface.
C
C        NOTE: Care must be taken when using these commands because nothing
C              is done to clean up the point type (ITP) array after the
C              ADDMESH operation. The user must often execute a series
C              of resetpts/itp and filter commands to get the final
C              desired result.
C
C        NOTE: Some operation may only work with tet meshes.
C
C      INPUT ARGUMENTS -
C
C         imsgin  - INTEGER ARRAY CONTAINING THE (INTEGER) TOKENS OF
C                      THE INPUT MESSAGE.
C         xmsgin  - REAl ARRAY CONTAINING THE (REAL) TOKENS OF
C                      THE INPUT MESSAGE.
C         cmsgin  - CHARACTER ARRAY CONTAINING THE (CHARACTER) TOKENS OF
C                      THE INPUT MESSAGE.
C         msgtype - INTEGER ARRAY CONTAINING THE TYPE OF EACH TOKEN.
C                      (=1 ==> INTEGER, =2 ==> REAL, =3 ==> CHARACTER)
C         nwds    - THE NUMBER OF TOKENS CONTAINED IN THE THE MESSAGE.
C
C
C      OUTPUT ARGUMENTS -
C
C         ierror   - INTEGER ERROR FLAG (==0 ==> OK, <>0 ==> AN ERROR)
C
C      CHANGE HISTORY -
C
C        $Log: addmesh.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   24 Jan 2007 09:34:58   gable
CPVCS    Made changes to handle cases where one MO is empty or has not elements.
CPVCS    Also handle cases where MO does not even exist. Still some glitches
CPVCS    in cases where a hyb type mesh is created but the odd cases that are
CPVCS    now handled in a graceful manner is much better.
CPVCS    
CPVCS       Rev 1.6   13 May 2004 11:17:36   gable
CPVCS    Fixed bug in VCHAR type element attribute.
CPVCS    
CPVCS       Rev 1.5   14 Sep 2000 12:35:34   dcg
CPVCS    add addmesh_reflect option
CPVCS    
CPVCS       Rev 1.4   21 Apr 2000 07:03:38   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.3   Wed Apr 05 13:33:20 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   03 Feb 2000 09:16:10   dcg
CPVCS    
CPVCS       Rev 1.25   Fri Jan 22 09:20:02 1999   dcg
CPVCS    add chydro.h
CPVCS    remove undefined use of variable istitch
CPVCS
CPVCS       Rev 1.24   Mon Feb 09 13:55:00 1998   dcg
CPVCS    make master cmo into a hybrid cmo after the
CPVCS    call to refine - not before
CPVCS
CPVCS       Rev 1.23   Wed Oct 08 12:37:14 1997   gable
CPVCS    Changes to header documentation, copied changes to
CPVCS    append and merge from x3d version of the code.
CPVCS    Added intersect option developed by Loraine Lundquest.
CPVCS    Incorporated changes in add option that allow automatic
CPVCS    refinement in region of intersecting mesh.
CPVCS
CPVCS       Rev 1.22   Mon Sep 15 10:12:30 1997   het
CPVCS    Change the definition of _append and _merge
CPVCS
CPVCS       Rev 1.21   Mon Dec 09 09:02:24 1996   het
CPVCS    Add the addmesh_amr call
CPVCS
CPVCS       Rev 1.20   Thu Nov 21 19:08:44 1996   het
CPVCS
CPVCS
CPVCS       Rev 1.19   Mon Nov 11 20:58:48 1996   het
CPVCS    Make changes for adding hybrid grids.
CPVCS
CPVCS       Rev 1.18   Thu Oct 10 08:39:04 1996   het
CPVCS    Change cmo_create to cmo_derive.
CPVCS
CPVCS       Rev 1.17   Mon Oct 07 08:55:14 1996   gable
CPVCS    Modified by L. Lundquist
CPVCS    Automatic or user controled refinement of the elements that will
CPVCS    be excavated now works in the addmesh/add/m1/m2/m3/[refine] syntax.
CPVCS
CPVCS       Rev 1.16   Mon Jul 22 16:19:12 1996   dcg
CPVCS    add pyramid joins for tet and hex meshes
CPVCS
CPVCS       Rev 1.15   Fri Feb 02 14:19:52 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.14   12/15/95 13:04:28   het
CPVCS    Correct changes for hybrid grids
CPVCS
CPVCS       Rev 1.13   11/16/95 15:21:20   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.12   09/20/95 12:49:16   dcg
CPVCS    fix array sizes
CPVCS
CPVCS       Rev 1.11   09/20/95 09:19:14   dcg
CPVCS    look for added attributes to mesh objects
CPVCS
CPVCS       Rev 1.10   08/10/95 09:29:48   dcg
CPVCS    add option 'glue' as synonym for 'merge'
CPVCS
CPVCS       Rev 1.9   06/13/95 09:01:28   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.8   05/30/95 07:52:26   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.7   05/26/95 13:16:48   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.6   05/23/95 06:49:00   het
CPVCS    Change dictionary so that they are CMO specific
CPVCS
CPVCS       Rev 1.5   05/15/95 13:36:12   het
CPVCS    Make changes to the regset and surfset routines
CPVCS
CPVCS       Rev 1.4   03/31/95 14:48:34   het
CPVCS    Fix a pointer problem with the node arrays
CPVCS
CPVCS       Rev 1.3   03/31/95 11:55:20   het
CPVCS    Correct a memory management error with cmo1
CPVCS
CPVCS       Rev 1.2   02/16/95 07:32:52   het
CPVCS    Convert the indexing from tets to tets, hexes, etc.
CPVCS
CPVCS       Rev 1.1   02/13/95 00:13:32   het
CPVCS    Add the match option to the addmesh command
CPVCS
CPVCS       Rev 1.0   01/17/95 16:33:52   pvcs
CPVCS    Original Version
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:20   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C ######################################################################
C
      include "local_element.h"
      include 'chydro.h'
C
C ######################################################################
C

C arguments
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

C variables
      real*8 bz3,by3,bx3,bz2,by2,bx2,bz1,by1,bx1,ay3,ax3,az3,
     *  az1,ay1,ax1,ax2,ay2,az2
      integer j3,j2,j1,i3,istart,jf,jt,i,it,nef3,nen3,nsdtopo3,
     * nsdgeom3,numtet3,npoints3,npoints_save,nmcmoatt_c,itype,
     * nef2,nen2,nsdtopo2,nsdgeom2,numtet2,npoints2,ierror,ierr,
     *  mbndry,npoints1,length,icmotp,numtet1,mbndry1,nsdtopo1,
     *  nsdgeom1,nen1,nef1,len,ier,mbndry2,i1,i2,index,npoints,
     *  ierra, ierrb, ierrc, nnodes_a, nnodes_b, nnodes_c
C
C ######################################################################
C
C     __________________________________________________________________
C     Master Mesh Object.
C
      pointer (ipxic1, xic1)
      pointer (ipyic1, yic1)
      pointer (ipzic1, zic1)
      real*8 xic1(*), yic1(*), zic1(*)
      pointer (ipitetclr1, itetclr1(*))
      pointer (ipitettyp1, itettyp1(*))
      pointer (ipitetoff1, itetoff1(*))
      pointer (ipjtetoff1, jtetoff1(*))
      pointer (ipitet1, itet1(*))
      pointer (ipjtet1, jtet1(*))
      integer itetclr1,itettyp1,itetoff1,jtetoff1,itet1,jtet1,
     * itetclr2,itettyp2,itetoff2,jtetoff2,itet2,jtet2,
     * itetclr3,itettyp3,itetoff3,jtetoff3,itet3,jtet3
C
C     __________________________________________________________________
C     Slave Mesh Object.
C
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      real*8 xic2(*), yic2(*), zic2(*)
      pointer (ipitetclr2, itetclr2(*))
      pointer (ipitettyp2, itettyp2(*))
      pointer (ipitetoff2, itetoff2(*))
      pointer (ipjtetoff2, jtetoff2(*))
      pointer (ipitet2, itet2(*))
      pointer (ipjtet2, jtet2(*))
C
C     __________________________________________________________________
C     Resulting Mesh Object.
C
      pointer (ipxic3, xic3)
      pointer (ipyic3, yic3)
      pointer (ipzic3, zic3)
      real*8 xic3(*), yic3(*), zic3(*)
      pointer (ipitetclr3, itetclr3(*))
      pointer (ipitettyp3, itettyp3(*))
      pointer (ipitetoff3, itetoff3(*))
      pointer (ipjtetoff3, jtetoff3(*))
      pointer (ipitet3, itet3(*))
      pointer (ipjtet3, jtet3(*))
C
      pointer(ipicmo_c,icmo_c)
      pointer(ipicmo_c,xcmo_c)
      pointer(ipicmo_c,ccmo_c)
      pointer(ipicmo_a,icmo_a)
      pointer(ipicmo_a,xcmo_a)
      pointer(ipicmo_a,ccmo_a)
      integer icmo_c(*),icmo_a(*)
      real*8 xcmo_c(*),xcmo_a(*)

      integer nuser_refine,nmcmoatt_a
      character*8 refine_type
      integer ipointi,ipointj,lencmo,itpcmo,itp1_boundary,
     *  mbndry_new,mbndry_old,ilen,imat_offset,ierrw,ict,
     *  ier1,iclc,icl,ierror_return,icscode,ierw
      character*32 cinter,cpers,cio,crank
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
      character*32 cname,ctype,clength,defname
      character*32 isubname, cmoa, cmob, cmoc, cmo_empty, coperator
      character*32 ccmo_a(*),ccmo_c(*)
      character*32 pset_name
      character*132 logmess
      character*8 cglobal, cdefault
C
      character*8192 cbuff
C
      integer icharlnf
C
      integer itetface0(4), itetface1(4,4)
      integer itet_edge(2,6)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 1,
     *                 1, 4, 3, 2,
     *                 1, 2, 4, 3,
     *                 1, 3, 2, 4 /
C
      data itet_edge / 1, 2,
     *                 1, 3,
     *                 1, 4,
     *                 2, 3,
     *                 2, 4,
     *                 3, 4 /
C
C ######################################################################
C
C
C
      isubname='addmesh'
      cglobal='global'
      cdefault='default'
      defname='default'
C
      ierror = 0
C
      coperator = cmsgin(2)
      if(coperator.eq.'reflect') then
        cmoc = cmsgin(3)
        call addmesh_reflect(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *  ierror)
        if(ierror.eq.0) then
           npoints_save=0
           go to 9998
        else
           go to 9999
        endif
      endif
C
      cmoc = cmsgin(3)
      cmoa = cmsgin(4)
      cmob = cmsgin(5)
C
C     Check if the three mesh objects exist.
C
      call cmo_exist(cmoa,ierra)
      if (ierra.eq.0) then
         call cmo_get_intinfo('nnodes',cmoa,nnodes_a,length,icmotp,ierr)
      else
         nnodes_a = 0
      endif
      call cmo_exist(cmob,ierrb)
      if (ierrb.eq.0) then
         call cmo_get_intinfo('nnodes',cmob,nnodes_b,length,icmotp,ierr)
      else
         nnodes_b = 0
      endif
      call cmo_exist(cmoc,ierrc)
      if (ierrc.eq.0) then
         call cmo_get_intinfo('nnodes',cmoc,nnodes_c,length,icmotp,ierr)
      else
         nnodes_c = 0
      endif

c      call cmo_get_intinfo('nnodes',cmoa,nnodes_a,length,icmotp,ierr)
c      call cmo_get_intinfo('nnodes',cmob,nnodes_b,length,icmotp,ierr)
c      call cmo_get_intinfo('nnodes',cmoc,nnodes_c,length,icmotp,ierr)

 
C#######################################################
C       Possible cases
C       C A B
C 1     0 0 0    Normal C = C + A + B
C 2     0 - -    No change, exit
C 3     0 0 -    Modify to C = C + A
C 4     0 - 0    Modify to C = C + B
C 5     - 0 0    Normal C = A + B
C 6     - - 0    Modify C = B
C 7     - 0 -    Modify C = A
C 8     - - -    No action, exit
C
C     Case 8 - - -  Write an error message and return with no action
      if((ierrc .ne. 0).and.(ierra .ne. 0).and.(ierrb .ne. 0))then
         write(logmess,9000) cmoc
         call writloga('default',0,logmess,0,ierr)
9000     format("WARNING: The sink object doesn't exist: ",a)
         write(logmess,9010) cmoa
         call writloga('default',0,logmess,0,ierr)
9010     format("WARNING: The source object doesn't exist: ",a)
         write(logmess,9010) cmob
         call writloga('default',0,logmess,0,ierr)
         write(logmess,*)'WARNING: no action'
         call writloga('default',0,logmess,0,ierr)
         goto 9999
C     Case 2 0 - -  Write an error message and return with no action
      elseif((ierrc .eq. 0).and.(ierra .ne. 0).and.(ierrb .ne. 0))then
         write(logmess,9010) cmoa
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9010) cmob
         call writloga('default',0,logmess,0,ierr)
         write(logmess,*)'WARNING: no action'
         call writloga('default',0,logmess,0,ierr)
         goto 9999
C     Case 6 - - 0  Just copy cmob into a new mesh object, cmoc
      elseif((ierrc .ne. 0).and.(ierra .ne. 0).and.
     *       ((ierrb .eq. 0).and.(nnodes_b .eq. 0)))then
9020     format("WARNING: The source object has 0 nodes: ",a)
         write(logmess,9020) cmob
         call writloga('default',0,logmess,0,ierr)
         write(logmess,*)'WARNING: no action'
         call writloga('default',0,logmess,0,ierr)
         goto 9999
      elseif((ierrc .ne. 0).and.(ierra .ne. 0).and.
     *       ((ierrb .eq. 0).and.(nnodes_b .ne. 0)))then
            cbuff='cmo/copy/' //
     *         cmoc(1:icharlnf(cmoc)) //' / '//
     *         cmob(1:icharlnf(cmob)) //
     *         ' ; finish '
         call dotask (cbuff, ierror)
         goto 9999
C     Case 7 - 0 - Just copy cmoa into a new mesh object, cmoc
      elseif((ierrc .ne. 0).and.(ierrb .ne. 0).and.
     *       ((ierra .eq. 0).and.(nnodes_a .ne. 0)))then
            cbuff='cmo/copy/' //
     *         cmoc(1:icharlnf(cmoc)) //' / '//
     *         cmoa(1:icharlnf(cmoa)) //
     *         ' ; finish '
         call dotask (cbuff, ierror)
         goto 9999
      elseif((ierrc .ne. 0).and.(ierrb .ne. 0).and.
     *       ((ierra .eq. 0).and.(nnodes_a .eq. 0)))then
         write(logmess,9020) cmoa
         call writloga('default',0,logmess,0,ierr)
         write(logmess,*)'WARNING: no action'
         call writloga('default',0,logmess,0,ierr)
         goto 9999
C     Case 3 0 0 - Change the name of one of the sources to the existing sink and continue
       elseif(((ierrc .eq. 0).and.(nnodes_c .ne. 0)).and.
     *        ((ierra .eq. 0).and.(nnodes_a .ne. 0)).and.
     *        (ierrb .ne. 0))then
         write(logmess,9010) cmob
         call writloga('default',0,logmess,0,ierr)
         cmob = cmoc
C     Case 4 0 - 0 Change the name of one of the sources to the existing sink and continue
       elseif(((ierrc .eq. 0).and.(nnodes_c .ne. 0)).and.
     *        (ierra .ne. 0).and.
     *        ((ierrb .eq. 0).and.(nnodes_b .ne. 0)))then
         write(logmess,9010) cmoa
         call writloga('default',0,logmess,0,ierr)
         cmoa = cmoc
C     Case 1 0 0 0 If the name of the sink and one of the sources are the same, change
C     one of the sources name to -tmp_source_internal-
       elseif((ierrc .eq. 0).and.
     *        ((ierra .eq. 0).and.(nnodes_a .ne. 0)).and.
     *        ((ierrb .eq. 0).and.(nnodes_b .ne. 0)))then
          if (cmoc .eq. cmoa) then
             call cmo_exist('-tmp_source_internal-',ierr)
             if(ierr .ne. 0) then
             cbuff = 'cmo/move/-tmp_source_internal-/' 
     *       // cmoa(1:icharlnf(cmoa)) // '; finish'
             call dotaskx3d(cbuff, ierr)
             cmoa = '-tmp_source_internal-' 
             else
             write(logmess,*)'ERROR: name of internal 
     *       mesh object conflicts with existing mesh object. EXIT'
             call writloga('default',0,logmess,0,ierr)
             goto 9999
             endif
          elseif(cmoc .eq. cmob) then
             call cmo_exist('-tmp_source_internal-',ierr)
             if(ierr .ne. 0) then
             cbuff = 'cmo/move/-tmp_source_internal-/'
     *       // cmob(1:icharlnf(cmob)) // '; finish'
             call dotaskx3d(cbuff, ierr)
             cmob = '-tmp_source_internal-' 
             else
             write(logmess,*)'ERROR: name of internal 
     *       mesh object conflicts with existing mesh object. EXIT'
             call writloga('default',0,logmess,0,ierr)
             goto 9999
             endif
             cmo_empty = cmob
          endif      
C     Case 5, just continue with no changes to input syntax.
       else
          continue
       endif
C#######################################################
      call cmo_get_intinfo('nnodes',cmoa,npoints1,length,icmotp,ierr)
      call cmo_get_intinfo('nelements',cmoa,numtet1,length,icmotp,ierr)
      call cmo_get_intinfo('nnodes',cmob,npoints2,length,icmotp,ierr)
      call cmo_get_intinfo('nelements',cmob,numtet2,length,icmotp,ierr)
C
C     If the Master object exists but is empty, switch Master and Slave.
C
C     If the Master object has nodes but no elements and the
C        the Slave  object has nodes and elements,
C        switch Master and Slave.
C
      if(((npoints1 .eq. 0).and.(numtet1 .eq. 0)) .or.
     *   ((numtet1  .eq. 0).and.(numtet2 .ne. 0)) .and.
     *   (coperator(1:11) .ne. 'excavate') )then
         cmoa = cmsgin(5)
         cmob = cmsgin(4)
         write(logmess,9012) cmoa
         call writloga('default',0,logmess,0,ierr)
9012     format("WARNING: ADDMESH switching order, Master MO : ",a)
         write(logmess,9014) cmob
         call writloga('default',0,logmess,0,ierr)
9014     format("WARNING: ADDMESH switching order, Slave  MO : ",a)
      endif
C
      call cmo_exist(cmoc,ierr)
      if(ierr.ne.0) then
         call cmo_derive(cmoc,cmoa,ierr)
      endif
C
C
C     __________________________________________________________________
C     Master Mesh Object.
C
      call cmo_select(cmoa,ierr)
      call cmo_get_intinfo('nnodes',cmoa,npoints1,length,icmotp,ierr)
      call cmo_get_intinfo('nelements',cmoa,numtet1,length,icmotp,ierr)
      call cmo_get_intinfo('mbndry',cmoa,mbndry1,length,icmotp,ierr)
      call cmo_get_intinfo('ndimensions_topo',cmoa,
     *                  nsdtopo1,length,icmotp,ierr)
      call cmo_get_intinfo('ndimensions_geom',cmoa,
     *                  nsdgeom1,length,icmotp,ierr)
      call cmo_get_intinfo('nodes_per_element',cmoa,
     *                  nen1,length,icmotp,ierr)
      call cmo_get_intinfo('faces_per_element',cmoa,
     *                  nef1,length,icmotp,ierr)
      call cmo_get_info('xic',cmoa,ipxic1,len,icmotp,ierr)
      call cmo_get_info('yic',cmoa,ipyic1,len,icmotp,ierr)
      call cmo_get_info('zic',cmoa,ipzic1,len,icmotp,ierr)
      call cmo_get_info('itetclr',cmoa,ipitetclr1,len,icmotp,ier)
      call cmo_get_info('itettyp',cmoa,ipitettyp1,len,icmotp,ier)
      call cmo_get_info('itetoff',cmoa,ipitetoff1,len,icmotp,ier)
      call cmo_get_info('jtetoff',cmoa,ipjtetoff1,len,icmotp,ier)
      call cmo_get_info('itet',cmoa,ipitet1,len,icmotp,ierr)
      call cmo_get_info('jtet',cmoa,ipjtet1,len,icmotp,ierr)
C
C     __________________________________________________________________
C     Slave Mesh Object.
C
      call cmo_select(cmob,ierr)
      call cmo_get_intinfo('nnodes',cmob,npoints2,length,icmotp,ierr)
      call cmo_get_intinfo('nelements',cmob,numtet2,length,icmotp,ierr)
      call cmo_get_intinfo('mbndry',cmob,mbndry2,length,icmotp,ierr)

      if ((npoints2 .eq. 0) .and. (numtet2 .eq. 0)) then

         write(logmess,'(a,a)')
     *    'WARNING: third mesh not valid, 0 nodes and 0 elements,' //
     *    ' no action using ',cmob(1:icharlnf(cmob))
         call writloga('default',0,logmess,0,ierr)
         goto 9999

      endif

      call cmo_get_intinfo('ndimensions_topo',cmob,
     *                  nsdtopo2,length,icmotp,ierr)
      call cmo_get_intinfo('ndimensions_geom',cmob,
     *                  nsdgeom2,length,icmotp,ierr)
      call cmo_get_intinfo('nodes_per_element',cmob,
     *                  nen2,length,icmotp,ierr)
      call cmo_get_intinfo('faces_per_element',cmob,
     *                  nef2,length,icmotp,ierr)
      call cmo_get_info('xic',cmob,ipxic2,len,icmotp,ierr)
      call cmo_get_info('yic',cmob,ipyic2,len,icmotp,ierr)
      call cmo_get_info('zic',cmob,ipzic2,len,icmotp,ierr)
      call cmo_get_info('itetclr',cmob,ipitetclr2,len,icmotp,ier)
      call cmo_get_info('itettyp',cmob,ipitettyp2,len,icmotp,ier)
      call cmo_get_info('itetoff',cmob,ipitetoff2,len,icmotp,ier)
      call cmo_get_info('jtetoff',cmob,ipjtetoff2,len,icmotp,ier)
      call cmo_get_info('itet',cmob,ipitet2,len,icmotp,ierr)
      call cmo_get_info('jtet',cmob,ipjtet2,len,icmotp,ierr)
C
C     __________________________________________________________________
C     Resulting Mesh Object.
C
      nsdgeom3 = max(nsdgeom1,nsdgeom2)
      nsdtopo3 = max(nsdtopo1,nsdtopo2)
C
      if(nen1.ne.nen2) then
         nen3 = nelmnen(ifelmhyb)
      else
         nen3 = nen1
      endif
      if(nef1.ne.nef2) then
         nef3 = nelmnef(ifelmhyb)
      else
         nef3 = nef1
      endif
      if(coperator(1:7).eq.'pyramid') then
         if(nef1.ne.nelmnef(ifelmtet).or.nef2.ne.nelmnef(ifelmhex)) then
             write(logmess,12)
 12          format(' wrong mesh type for addmesh/pyramid')
             call writloga('default',0,logmess,0,ierw)
             go to 9999
         endif
         nef3=nelmnef(ifelmtet)
         nen3=nelmnen(ifelmtet)
      endif
C
C
      call cmo_select(cmoc,ierr)
      call cmo_set_info('nnodes',cmoc,npoints1,1,1,ierr)
      call cmo_set_info('nelements',cmoc,numtet1,1,1,ierr)
      call cmo_set_info('ndimensions_geom',cmoc,nsdgeom3,1,1,ier)
      call cmo_set_info('ndimensions_topo',cmoc,nsdtopo3,1,1,ier)
      call cmo_set_info('nodes_per_element',cmoc,nen3,1,1,ierr)
      call cmo_set_info('faces_per_element',cmoc,nef3,1,1,ierr)
C
      call cmo_newlen(cmoc,ierr)
C
      call cmo_get_intinfo('mbndry',cmoc,mbndry,len,icmotp,ierr)
      call cmo_get_info('xic',cmoc,ipxic3,len,icmotp,ierr)
      call cmo_get_info('yic',cmoc,ipyic3,len,icmotp,ierr)
      call cmo_get_info('zic',cmoc,ipzic3,len,icmotp,ierr)
      call cmo_get_info('itetclr',cmoc,ipitetclr3,len,icmotp,ier)
      call cmo_get_info('itettyp',cmoc,ipitettyp3,len,icmotp,ier)
      call cmo_get_info('itetoff',cmoc,ipitetoff3,len,icmotp,ier)
      call cmo_get_info('jtetoff',cmoc,ipjtetoff3,len,icmotp,ier)
      call cmo_get_info('itet',cmoc,ipitet3,len,icmotp,ierr)
      call cmo_get_info('jtet',cmoc,ipjtet3,len,icmotp,ierr)
C
C     __________________________________________________________________
C     Copy the Master Mesh Object to the Resulting Mesh Object.
C
      npoints3 = npoints1
      numtet3  = numtet1
C
      npoints_save = npoints1
C
C  get pointers and attributes for master mesh object and slave
      call cmo_get_info('number_of_attributes',cmoc,nmcmoatt_c,
     *   ilen,itype,icscode)
      call cmo_get_info('number_of_attributes',cmoa,nmcmoatt_a,
     *   ilen,itype,icscode)
C
C  loop through attributes copy in matching field where it
C  exists
C
      do i1=1,nmcmoatt_c
         call cmo_get_attribute_name(cmoc,i1,cname,icscode)
         call cmo_get_attparam(cname,cmoc,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)


         if (ctype(1:4).eq.'VINT'.and.clength(1:6).eq.'nnodes') then
             call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
             call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
             call cmo_get_info(cname,cmoa,ipicmo_a,icl,ict,ier1)
             if(ierror.eq.0.and.ier1.eq.0.and.length.ge.npoints3) then
              if(npoints3 .ge. 1)then
                do i2=1,npoints3
                   icmo_c(i2)=icmo_a(i2)
                enddo
              endif
             endif
         endif
         if (ctype(1:7).eq.'VDOUBLE'.and.clength(1:6).eq.'nnodes') then
             call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
             call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
             call cmo_get_info(cname,cmoa,ipicmo_a,icl,ict,ier1)
             if(ierror.eq.0.and.ier1.eq.0.and.length.ge.npoints3) then
              if(npoints3 .ge. 1)then
                do i2=1,npoints3
                   xcmo_c(i2)=xcmo_a(i2)
                enddo
              endif
             endif
         endif
         if (ctype(1:7).eq.'VCHAR'.and.clength(1:6).eq.'nnodes') then
             call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
             call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
             call cmo_get_info(cname,cmoa,ipicmo_a,icl,ict,ier1)
             if(ierror.eq.0.and.ier1.eq.0.and.length.ge.npoints3) then
              if(npoints3 .ge. 1)then
                do i2=1,npoints3
                   ccmo_c(i2)=ccmo_a(i2)
                enddo
              endif
             endif
         endif
      enddo
C
      do i1=1,nmcmoatt_c
         call cmo_get_attribute_name(cmoc,i1,cname,icscode)
         call cmo_get_attparam(cname,cmoc,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)
          if (ctype(1:4).eq.'VINT' .and.
     *        clength(1:9).eq.'nelements') then
             call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
             call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
             call cmo_get_info(cname,cmoa,ipicmo_a,icl,ict,ier1)
             if(ierror.eq.0.and.ier1.eq.0.and.length.ge.numtet3) then
               if(numtet3 .ge. 1)then
                do i2=1,numtet3
                   icmo_c(i2)=icmo_a(i2)
                enddo
               endif
             endif
          endif
          if (ctype(1:7).eq.'VDOUBLE' .and.
     *        clength(1:9).eq.'nelements') then
             call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
             call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
             call cmo_get_info(cname,cmoa,ipicmo_a,icl,ict,ier1)
             if(ierror.eq.0.and.ier1.eq.0.and.length.ge.numtet3) then
               if(numtet3 .ge. 1)then
                do i2=1,numtet3
                   xcmo_c(i2)=xcmo_a(i2)
                enddo
               endif
             endif
          endif
         if (ctype(1:7).eq.'VCHAR'.and.clength(1:9).eq.'nelements') 
     *       then
             call cmo_get_info(cname,cmoc,ipicmo_c,iclc,ict,ierror)
             call cmo_get_info(clength,cmoc,length,iclc,ict,ierror)
             call cmo_get_info(cname,cmoa,ipicmo_a,icl,ict,ier1)
             if(ierror.eq.0.and.ier1.eq.0.and.length.ge.numtet3) then
               if(numtet3 .ge. 1)then
                do i2=1,numtet3
                   ccmo_c(i2)=ccmo_a(i2)
                enddo
               endif
             endif
         endif
      enddo
C
      if(numtet3 .ge. 1)then
      do it=1,numtet3
         itetclr3(it)=itetclr1(it)
         itettyp3(it)=itettyp1(it)
         itetoff3(it)=itetoff1(it)
         jtetoff3(it)=jtetoff1(it)
         do i=1,nelmnen(itettyp3(it))
            itet3(itetoff3(it)+i)=itet1(itetoff1(it)+i)
         enddo
         do i=1,nelmnef(itettyp3(it))
            if(jtet1(jtetoff1(it)+i).le.0 .or.
     *         jtet1(jtetoff1(it)+i).eq.mbndry) then
               jtet3(jtetoff3(it)+i)=jtet1(jtetoff1(it)+i)
            elseif(jtet1(jtetoff1(it)+i).gt.mbndry) then
               jt=1+(jtet1(jtetoff1(it)+i)-mbndry-1)/nef1
               jf=jtet1(jtetoff1(it)+i)-mbndry-nef1*(jt-1)
               jtet3(jtetoff3(it)+i)=mbndry+nef3*(jt-1)+jf
            else
               jt=1+(jtet1(jtetoff1(it)+i)-1)/nef1
               jf=jtet1(jtetoff1(it)+i)-nef1*(jt-1)
               jtet3(jtetoff3(it)+i)=nef3*(jt-1)+jf
            endif
         enddo
      enddo
      endif
C
C     __________________________________________________________________
C     Add the Slave Mesh Object depending on the operation.
C
      if(coperator(1:3).eq.'add') then
C
C     Check for command specified refine arguments in 'add' option
C
C
         if (msgtype(6) .eq. 1) then
            nuser_refine = imsgin(6)
         else
            nuser_refine = -1
         endif
C
	 coperator = cmsgin(7)
	 if (coperator(1:3) .eq. 'tet') then
	    refine_type = 'tet'
	 else
	    refine_type = 'edges'
	 endif
CCCC
C
         call addmesh_add(cmoc,cmob,nuser_refine,refine_type,ierror)
C
      elseif(coperator(1:11).eq.'excavate') then

          call cmo_set_info('nodes_per_element',cmoc,nen1,1,1,ierr)
          call cmo_set_info('faces_per_element',cmoc,nef1,1,1,ierr)
          if (nwds .ge. 6 .and. cmsgin(6)(1:3) .eq. 'bfs') ierror = 1
          call excavate(cmoc, cmob, ierror)
          if (nwds .ge. 7 .and. cmsgin(7)(1:7) .eq. 'connect') then
              cbuff='addmesh/append/-temp-/'//cmoc//'/'//cmob
     &             //';finish'
              call dotask(cbuff, ierror)
              cbuff='cmo/release/'//cmoc//';finish'
              call dotask(cbuff, ierror)
              cbuff='copypts/'//cmoc//'/-temp-;finish'
              call dotask(cbuff, ierror)
              cbuff='connect; finish'
              call dotask(cbuff, ierror)
              cbuff='cmo/release/-temp-; finish'
              call dotask(cbuff, ierror)
          endif

      elseif(coperator(1:3).eq.'amr') then
C
         call addmesh_amr(cmoc,cmob,ierror)
C
      elseif(coperator(1:5).eq.'match') then
C
         istart=6
         if(nwds.lt.istart) then
         write(logmess,'(a,a)')
     1   'ERROR: Invalid ADDMESH Arguments: ', coperator
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,a)')
     1   'ERROR: Specify 6 node numbers or 6X3 xyz coordinates'
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,a)')
     1   'ERROR:No Action: ', coperator
         call writloga('default',0,logmess,0,ierrw)
         ierror = -3
         goto 9999
         endif
         if(msgtype(istart).eq.1) then
            i1=imsgin(istart)
            i2=imsgin(istart+1)
            i3=imsgin(istart+2)
            j1=imsgin(istart+3)
            j2=imsgin(istart+4)
            j3=imsgin(istart+5)
            ax1=xic3(i1)
            ay1=yic3(i1)
            az1=zic3(i1)
            ax2=xic3(i2)
            ay2=yic3(i2)
            az2=zic3(i2)
            ax3=xic3(i3)
            ay3=yic3(i3)
            az3=zic3(i3)
            bx1=xic2(j1)
            by1=yic2(j1)
            bz1=zic2(j1)
            bx2=xic2(j2)
            by2=yic2(j2)
            bz2=zic2(j2)
            bx3=xic2(j3)
            by3=yic2(j3)
            bz3=zic2(j3)
         elseif(msgtype(istart).eq.2) then
            ax1=xmsgin(istart)
            ay1=xmsgin(istart+1)
            az1=xmsgin(istart+2)
            ax2=xmsgin(istart+3)
            ay2=xmsgin(istart+4)
            az2=xmsgin(istart+5)
            ax3=xmsgin(istart+6)
            ay3=xmsgin(istart+7)
            az3=xmsgin(istart+8)
            bx1=xmsgin(istart+9)
            by1=xmsgin(istart+10)
            bz1=xmsgin(istart+11)
            bx2=xmsgin(istart+12)
            by2=xmsgin(istart+13)
            bz2=xmsgin(istart+14)
            bx3=xmsgin(istart+15)
 
            by3=xmsgin(istart+16)
            bz3=xmsgin(istart+17)
         elseif(msgtype(istart).eq.3) then
           write(logmess,'(a,a)')
     1     'ERROR: Invalid ADDMESH Argument: ', istart
           call writloga('default',0,logmess,0,ierrw)
           write(logmess,'(a,a)')
     1     'No Action: ', coperator
           call writloga('default',0,logmess,0,ierrw)
           goto 9999
         endif
C
         call addmesh_match(cmoc,cmob,
     *                      ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,
     *                      bx1,by1,bz1,bx2,by2,bz2,bx3,by3,bz3,
     *                      ierror)
C
      elseif(coperator(1:5).eq.'merge'.or.coperator(1:4).eq.'glue') then
C
         call addmesh_merge(cmoc,cmob,ierror)
C
      elseif(coperator(1:6).eq.'append') then
C
         imat_offset=1
         call addmesh_append(cmoc,cmob,imat_offset,ierror)
C
      elseif(coperator(1:7).eq.'pyramid') then
C
         call addmesh_pyramid(cmoc,cmob,ierror)
C
C
      elseif(coperator(1:9).eq.'intersect') then
C
         pset_name = cmsgin(3)
	 call addmesh_intersection(cmoa, cmob, pset_name, ierror)
         call cmo_select (cmoa, ierror)
C
C
      elseif(coperator(1:6).eq.'delete') then
C
         call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
         call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierror)
         call cmo_get_info('mbndry',cmoc,mbndry,ilen,icmotp,ierror)
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
 
	 call addmesh_overlap(cmoc, cmob, ipipoint_overlap1,
     *   ipitet_overlap1, ipitet_border1, ipipoint_overlap2,
     *   ipitet_overlap2, ipitet_border2, ierror)
C
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
c following code commented out 1/99 istitch not defined don't know
c original intent
c        if(istitch.eq.1) then
c           mbndry_new=-1
c        else
            mbndry_new=mbndry
c        endif
         itp1_boundary=ifitpini
C
	 call addmesh_delete(cmoc, mbndry_old, mbndry_new,
     *         itp1_boundary, ipitet_delete, ierror)
C
      else
C
         write(logmess,'(a,a)') 'Invalid ADDMESH Operation: ', coperator
         call writloga('default',0,logmess,0,ierrw)
         ierror = -3
C
      endif
C
 9998 if(ierror .eq. 0) then
         call cmo_get_info('nnodes',cmoc,npoints,lencmo,itpcmo,ierror)
         if(npoints_save .ne. npoints) then
            ipointi = npoints_save+1
            ipointj = npoints
            call cmo_set_info('ipointi',cmoc,ipointi,1,1,
     &                      icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
            call cmo_set_info('ipointj',cmoc,ipointj,1,1,
     &                      icscode)
            if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
         endif
      endif
C
      call cmo_exist('-tmp_source_internal-',ierr)
      if(ierr .eq. 0) then
         cbuff = 'cmo/delete/-tmp_source_internal-/;finish'
         call dotaskx3d(cbuff, ierr)
      endif
C
      goto 9999
 9999 continue
      return
      end
