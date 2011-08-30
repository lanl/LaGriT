*DK dumpfehm
      subroutine dumpfehm(ifile,ifileini,ioption,iomode,
     *       area_coef_option,compress_opt,attrib_option,area_option,
     *       hybrid_option)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR FEHMN.
C
C        Arguments are processed and passed in from writedump() 
C         ifile is the file or attribute name
C         ifileini is used by dump_fehm_geom 
C         ioption is fehm or stor, where fehm will write all fehm files
C         iomode is writing mode such as ascii
C         area_coef_option is coef_option such as scalar
C         compress_opt to compress coef values and indices
C         attrib_option is outside node option such as keepatt
C         area_option is attribute option such as keepatt_voronoi
C
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
C        $Log: dumpfehm.f,v $
C        Revision 2.00  2007/11/05 19:45:53  spchu
C        Import to CVS
C
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
      character iomode*(*), area_coef_option*(*)
      character compress_opt*(*),attrib_option*(*),area_option*(*)
      character hybrid_option*(*)

      integer ntets, nen, nsdtopo, nef, ijob
      integer length, icmotype, io, num_area_coef, ierror
      integer ifcompress, ifhybrid, imat_select
      integer idebug, ilen,ityp,ierr

      integer icharlnf 

      character*132 logmess
      character*32 isubname, cmo
C
C     Begin
      isubname = 'dumpfehm'
      ierror = 0
c
c     check cmo and setup
c
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)

      call cmo_get_info('idebug',cmo,idebug,ilen,ityp,ierr)

      if(idebug.ne.0) then
        write(logmess,'(a5,a6,a,a)')"dump/",ioption,
     *   ifile(1:icharlnf(ifile)),
     *  " options set to: "
        call writloga('default',0,logmess,0,ierr)

        write(logmess,'(6x,a,2x,a,2x,a,2x,a,2x,a,2x,a)')
     *   iomode(1:icharlnf(iomode)),
     *   area_coef_option(1:icharlnf(area_coef_option)),
     *   compress_opt(1:icharlnf(compress_opt)),
     *   attrib_option(1:icharlnf(attrib_option)),
     *   area_option(1:icharlnf(area_option)),
     *   hybrid_option(1:icharlnf(hybrid_option))
        call writloga('default',0,logmess,0,ierr)

        call mmverify()

      endif


C     Check and set options
C     any combination of options are allowed to pass into this routine
C     check here and warn for those that do not apply, set to defaults
C         dump/fehm option ifileini is used by dump_fehm_geom
C         dump/fehm option attrib_option default is delatt 
C         dump/fehm option area_option default is -notset- 
C         ifile is the file or attribute name
C         ioption is fehm or stor, where fehm will write all fehm files
C         iomode default is ascii
C         area_coef_option default is scalar
C         compress_opt default is all 
c
c     FEHM - Write all fehm files in addition to the stor file 
c     OPTIONS for default fehm files 
c       imat_select - dump_material_list of single value (set to zero here)
c                     since there is no way to get the value in here.
c       attrib_option - delatt|keepatt|keepatt_area for dump_outside_list
c       area_option - keepatt_voronoi | keepatt_median for dump_outside_list

      imat_select = 0
 
      if (ioption(1:4) .ne. 'stor') then
         write(logmess,101)
  101    format('*** Write FEHMN GEOM AND ZONE FILES ***')
        call writloga('default',0,logmess,0,ierror)
        call dump_fehm_geom(ifile, ifileini)
        call dump_material_list(ifile,imat_select)
        call dump_interface_list(ifile)
        call dump_multi_mat_con(ifile)

        call dotaskx3d('log/tty/off; finish',ierror)
        call dump_outside_list(ifile, attrib_option, area_option)
        call dotaskx3d('log/tty/on; finish',ierror)
        call dump_parent_list(ifile)
      endif
C
c     STOR - write the stor file 
c     As of June 3 2009
c     OPTIONS differ between the 3 routines that write stor files
c     matbld2d_stor, matbld3d_stor, anothermatbld3d_wrapper
c     the FEHM header has been modified to indicate the routine
c     and the type of compression being used.
c     matbld3d_nstor - original version, no compression (none)
c     matbld3d_cstor - original version, compress coef values (coefs)
c     matbld3d_gstor - new version, compress graph edges (graph)
c     matbld3d_astor - new version, compress indices and coef values (all)
c
c     matbld2d_stor 
c     (2D only) 
c        ifile : file name
c
c     matbld3d_stor - original sparse matrix 
c        ifile : file name
c        ijob  : 1=write coef to file and attribute  2= write attribute only  
c        io    : 1=3=unformatted 2=ascii
c        num_area_coef : 1=scalar 2=vector 3=both 4=area_scalar 
c                       -1=area_scalar -3=area_vector -4=area_both 
c        ifcompress : 0= none - no ccoef compression 
c                        1= coefs -  compression of area coef values 
c
c     anothermatbld3d_wrapper - update by Mike Murphy using linked lists 
c        ifile : file or attribute name
c        io : 1=3=unformatted 2=ascii 5=attribute only
c        num_area_coef : only scalar available, set to 1 
c        ifcompress: 0= graph - compress coef indices (edge compression)
c                       1= all - area coef and indices compression 
c

c---- 2D STOR  -----------------------------------------------c
      if(nsdtopo.eq.2.and.nen.eq.3.and.nef.eq.3) then

         write(logmess,100)
  100    format('***Construct Regular Sparse Matrix:2D***')
         call writloga('default',0,logmess,0,ierror)

         call matbld2d_stor(ifile)
         
c---- 3D STOR  -----------------------------------------------c
      elseif(nsdtopo.eq.3.and.nen.eq.4.and.nef.eq.4) then
         ijob = 1
         ifcompress = 0
         ifhybrid = 0
         if(iomode(1:3) .eq. 'bin')io = 1
         if(iomode(1:3) .eq. 'asc')io = 2
         if(iomode(1:3) .eq. 'unf')io = 3
         if(compress_opt(1:3) .eq. 'all') ifcompress=1
         if(compress_opt(1:4) .eq. 'coef') ifcompress=1
         if(compress_opt(1:4) .eq. 'none') ifcompress=0
         if(compress_opt(1:5) .eq. 'graph') ifcompress=0
         if(hybrid_option(1:6) .eq. 'hybrid') ifhybrid=1

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

C        check for old syntax
         if(area_coef_option(1:16) .eq.'alternate_scalar') then
            num_area_coef = 1
         endif

C        vector and area options not available in anothermatbld3d
C        so default to none and use matbld3d (no compression)
C        we default to none because compression may not work for these
         if (num_area_coef .ne. 1) then
            if (compress_opt(1:4).ne."coef") then
               compress_opt="none" 
               ifcompress = 0
            endif 
         endif 


c        3D Alternate STOR ----------------------------------------c
         if(compress_opt(1:5) .eq.'graph' .or. 
     *      compress_opt(1:3) .eq.'all') then

           write(logmess,110)
  110      format('*** Construct and Compress Sparse Matrix:3D ***')
           call writloga('default',1,logmess,0,ierror)
           if (ifcompress .gt. 0) then
             write(logmess,111)
             call writloga('default',0,logmess,0,ierror)
           endif 

c          check arguments
           if (num_area_coef .ne. 1) then
             write(logmess,'(a,a12)')
     *    "Alternate Matbld3d option not available: ",area_coef_option
             call writloga('default',0,logmess,0,ierror)
             write(logmess,'(a)')
     *     "Using scalar - single component area/distance coefficients"
             call writloga('default',0,logmess,0,ierror)
             num_area_coef = 1
           endif

           call anothermatbld3d_wrapper(ifile,io,num_area_coef,
     *                ifcompress, ifhybrid)


c        3D Regular STOR  -------------------------------------------c
         else 
         
           if (ifhybrid .eq. 1) then
               write(logmess,'(a,a,a)')'Warning: ignoring hybrid ',
     *         'option because it is not supported in combination ',
     *         'with the other chosen options.'
               call writloga('default',1,logmess,0,ierror)
           endif
           write(logmess,130)
  130      format('*** Construct Sparse Matrix:3D ***')
           call writloga('default',1,logmess,0,ierror)
           if (ifcompress .gt. 0) then
             write(logmess,111)
             call writloga('default',0,logmess,0,ierror)
           endif

c          check arguments
           call matbld3d_stor(ifile,ijob,io,num_area_coef,ifcompress)

         endif
  111        format('   *** Compress Area Coefficient Values ***')


c---- topo or element not usable ---------------------------------------c
      else

        write(logmess,'(a,a)') "*** DUMP/STOR ",
     *  " Mesh object does not qualify for sparse matrix. ***"
        call writloga('default',1,logmess,1,ierr)

        if (ntets .le.0) then
          write(logmess,'(a)')
     *    "Mesh object has 0 elements.  "
          call writloga('default',0,logmess,0,ierr)
        endif
        if (nef .le.0) then
          write(logmess,'(a)')
     *    "Mesh object has 0 faces.  "
          call writloga('default',0,logmess,0,ierr)
        endif
        if (nsdtopo.ne.2 .or. nsdtopo.ne.3) then
          write(logmess,'(a,i5)')
     *    "Mesh object has unsupported dimension:  ",nsdtopo
          call writloga('default',0,logmess,0,ierr)
        endif
       logmess='cmo/status/'//cmo(1:icharlnf(cmo))//' brief ; finish'
        call dotaskx3d(logmess,ierr)

        write(logmess,'(a)')
     *  "*** DUMP/STOR Finished early, no stor file written! ***"
        call writloga('default',1,logmess,1,ierr)

      endif
C
      return
      end
