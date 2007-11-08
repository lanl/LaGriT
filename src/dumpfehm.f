*DK dumpfehm
      subroutine dumpfehm(ifile,ifileini,ioption,iomode,
     *                    area_coef_option,attrib_option)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR FEHMN.
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
C        $Log:   /pvcs.config/t3d/src/dumpfehm.f_a  $
CPVCS    
CPVCS       Rev 1.34   23 Feb 2005 08:29:04   tam
CPVCS    added argument imat_select to dump_material_list call that
CPVCS    enables a single zone to be selected
CPVCS    
CPVCS       Rev 1.33   21 Jan 2004 11:37:18   tam
CPVCS    added ioption to arguments to allow choice between
CPVCS    fehm and stor, where stor writes stor file but
CPVCS    nothing else
CPVCS    
CPVCS       Rev 1.32   10 Mar 2000 12:46:42   gable
CPVCS    Remove call to mask_icr, put inside dump_outside_list.f
CPVCS    
CPVCS       Rev 1.31   Tue May 18 12:47:44 1999   murphy
CPVCS    Added 'alternate_scalar' option to produce compressed FEHM matrices on-the-fly
CPVCS    
CPVCS       Rev 1.30   Tue Mar 16 09:26:16 1999   murphy
CPVCS    Removed evil print statement.
CPVCS    
CPVCS       Rev 1.29   Mon Mar 15 16:46:34 1999   murphy
CPVCS    Added compression option.
CPVCS    
CPVCS       Rev 1.28   Mon Jan 25 13:01:40 1999   llt
CPVCS    passing option to delete/keep boundary attributes to
CPVCS    dump_outside_list
CPVCS    
CPVCS       Rev 1.27   Tue Oct 13 20:02:32 1998   gable
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.26   Thu Mar 12 13:35:28 1998   gable
CPVCS    Added option area_scalar, area_vector, area_both to output
CPVCS    area coefficients Aij, without division by distance xij.
CPVCS    
CPVCS       Rev 1.25   Wed Feb 11 10:37:28 1998   tam
CPVCS    added option "unformatted" for iomode
CPVCS    
CPVCS       Rev 1.24   Wed Oct 15 13:39:46 1997   gable
CPVCS    Modified to allow ascii or binary fehm stor file output.
CPVCS    Also added option to have scalar, vector or scalar and vector
CPVCS    area coefficients as part of output.
CPVCS    
CPVCS       Rev 1.23   Fri Sep 19 15:15:44 1997   gable
CPVCS    Turn of tty log when attributes for tblrfb are added.
CPVCS    
CPVCS       Rev 1.22   Wed Jul 02 09:17:42 1997   gable
CPVCS    Added call to dump_multi_mat_con.f
CPVCS    
CPVCS       Rev 1.21   Mon Jun 30 16:21:28 1997   gable
CPVCS    Assign ijob to type integer.
CPVCS    
CPVCS       Rev 1.20   Mon Jun 30 16:11:34 1997   gable
CPVCS    Added ijob to calling arguments of matbld3d_stor.
CPVCS    
CPVCS       Rev 1.19   Mon Apr 14 16:44:04 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.18   Wed May 08 12:30:28 1996   gable
CPVCS    Major changes to dumpfehm. This code is now a driver for
CPVCS    five other codes, each of which output some of the FEHM 
CPVCS    output. This will now set a bit mask in the icr array
CPVCS    to identify the top, bottom, ... nodes.
CPVCS    
CPVCS       Rev 1.17   Fri Feb 16 12:51:26 1996   het
CPVCS    Correct the parent/child entries in the .stor file
CPVCS    
CPVCS       Rev 1.16   Fri Feb 02 14:21:18 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.15   Tue Jan 23 09:28:00 1996   het
CPVCS    Add an epsilon to the Voronoi point calculation for 2D
CPVCS
CPVCS
CPVCS       Rev 1.14   Thu Jan 18 12:41:56 1996   gable
CPVCS    Add multi-material (parent/child) node list output.
CPVCS
CPVCS       Rev 1.13   Fri Dec 22 14:18:44 1995   het
CPVCS    Add the dump_hex option
CPVCS
CPVCS       Rev 1.12   12/05/95 08:23:10   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.11   11/07/95 17:16:22   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.10   10/16/95 09:05:10   het
CPVCS    Change sign(1.0 to sign(1
CPVCS
CPVCS       Rev 1.9   10/13/95 16:53:58   het
CPVCS    Latest changes
CPVCS
CPVCS       Rev 1.8   09/11/95 14:41:38   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.7   08/31/95 14:54:22   gable
CPVCS    Change constraint table to include more special cases. Constraint
CPVCS    table now goes up 0-40 cases.
CPVCS
CPVCS    Change order of zone list output. Output is now in order of materials,
CPVCS    material = 1 first.
CPVCS
CPVCS    Added a debug option. If variable, if_debug .ne. 0, then 6 new integer
CPVCS    addributes are added to the cmo. These atributes are for
CPVCS    top, bottom, left, right, front, back. The attribute is 1 if it is a side/top/...
CPVCS    and .lt. 0 if it is not.
CPVCS
CPVCS       Rev 1.6   08/15/95 18:22:42   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.5   07/25/95 15:25:02   gable
CPVCS          Change constraint table for icr array. Largest value of icr used to be
CPVCS         24. Constraint table now goes up to 32.
CPVCS
CPVCS         Correct output to fehm elem macro. Parameter ns was hardwired to 8
CPVCS         but code did not always pad connectivity to 8 integer values. Now set
CPVCS         to output ns=4 for tets, ns=3 for triangles.
CPVCS
CPVCS
CPVCS       Rev 1.4   07/14/95 10:18:54   het
CPVCS    Make output format consistent for FEHMN/AVS
CPVCS
CPVCS       Rev 1.3   06/28/95 11:30:46   het
CPVCS    Correct errors related to formatting
CPVCS
CPVCS       Rev 1.2   06/05/95 10:36:28   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.1   05/26/95 13:13:16   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.0   01/17/95 16:36:02   pvcs
CPVCS    Original Version
C
C ######################################################################
C
      implicit none
c
      character ifile*(*), ifileini*(*)
      character ioption*(*)
      character iomode*(*), area_coef_option*(*), attrib_option*(*)
      integer nen, nsdtopo, nef, ijob
      integer length, icmotype, io, num_area_coef, ierror
      integer ifcompress, imat_select
      character*132 log_io
C
      character*32 isubname, cmo
C
      isubname = 'dumpfehm'
      imat_select = 0
c
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
c
c     if ioption is stor, only write the stor file
      if (ioption(1:4) .ne. 'stor') then
        call dump_fehm_geom(ifile, ifileini)
        call dump_material_list(ifile,imat_select)
        call dump_interface_list(ifile)
        call dump_multi_mat_con(ifile)
        call dotaskx3d('log/tty/off; finish',ierror)
        call dump_outside_list(ifile, attrib_option)
        call dotaskx3d('log/tty/on; finish',ierror)
        call dump_parent_list(ifile)
      endif
C
      
      if(nsdtopo.eq.2.and.nen.eq.3.and.nef.eq.3) then

         write(log_io,100)
  100    format('*********Construct Sparse Matrix:2D********')
         call writloga('default',0,log_io,0,ierror)

         call matbld2d_stor(ifile)
         
      elseif(nsdtopo.eq.3.and.nen.eq.4.and.nef.eq.4) then
         ijob = 1
         ifcompress = 0
         if(iomode .eq. 'binary')io = 1
         if(iomode .eq. 'ascii')io = 2
         if(iomode .eq. 'unformatted')io = 3
         if(iomode .eq. 'binaryc') then
            io = 1
            ifcompress=1
         endif
         if(iomode .eq. 'asciic') then
            io = 2
            ifcompress = 1
         endif

         if(area_coef_option(1:6)     .eq.'scalar')then
            num_area_coef = 1
         elseif(area_coef_option(1:6) .eq.'vector')then
            num_area_coef = 3
         elseif(area_coef_option(1:4) .eq.'both')then
            num_area_coef = 4
         elseif(area_coef_option(1:11).eq.'area_scalar')then
            num_area_coef = -1
         elseif(area_coef_option(1:11).eq.'area_vector')then
            num_area_coef = -3
         elseif(area_coef_option(1:9) .eq.'area_both')then
            num_area_coef = -4
         endif

         if(area_coef_option(1:16) .eq.'alternate_scalar') then

         write(log_io,110)
  110    format('***Construct and Compress Sparse Matrix:3D***')
         call writloga('default',0,log_io,0,ierror)

         
            num_area_coef = 1
            call anothermatbld3d_wrapper(ifile,io,num_area_coef,
     *                ifcompress)
         else 
         
         write(log_io,130)
  130    format('*********Construct Sparse Matrix:3D********')
         call writloga('default',0,log_io,0,ierror)
         
            call matbld3d_stor(ifile,ijob,io,num_area_coef,ifcompress)
         endif
      endif
C
      return
      end
