*dk,writedump
      subroutine writedump(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Writes all the Dumps.
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: writedump.f,v $
C         Revision 2.00  2007/11/09 20:04:06  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.17   19 Jul 2007 15:24:44   tam
CPVCS    added zone_outside_minmax to syntax
CPVCS    
CPVCS       Rev 1.16   05 Jan 2007 10:19:30   gable
CPVCS    Added Tecplot output option implementation by
CPVCS    Daniel R Einstein, PNNL.
CPVCS    
CPVCS    Also recovered a version control problem because when I first
CPVCS    checked in rev 1.15 is was missing rev 1.12, 1.13, 1.14. This
CPVCS    fixes that glitch.
CPVCS    
CPVCS       Rev 1.14   26 Jul 2006 10:58:46   gable
CPVCS    Added options to output without node number and/or element number
CPVCS    as the first column of output. This produces non-standard AVS files
CPVCS    that in general will not be readable by AVS or read/avs. This is
CPVCS    useful for creating tabular output of node and element attributes
CPVCS    without node number and element number.
CPVCS    
CPVCS       Rev 1.13   28 Apr 2006 09:15:32   gable
CPVCS    Added some extra combinations of ways write lagrit (upper and lower case).
CPVCS    
CPVCS       Rev 1.12   12 Oct 2005 15:24:36   gable
CPVCS    Added io_format option for keywords avs and avs2
CPVCS    
CPVCS       Rev 1.11   25 Jul 2005 11:10:54   gable
CPVCS    Added option for GeoFEST format output.
CPVCS    
CPVCS       Rev 1.10   23 Feb 2005 08:54:26   tam
CPVCS    added argument to dump_material_list call that
CPVCS    enables a single zone to be selected
CPVCS    
CPVCS       Rev 1.9   30 Sep 2004 14:28:28   gable
CPVCS    Update dumpavs to support full control of
CPVCS    node,element,node_attribute,element_attribue output
CPVCS    by using 0 or 1 in tokens 5,6,7,8.
CPVCS    
CPVCS       Rev 1.8   17 Jun 2004 14:53:06   gable
CPVCS    Add calls to dump / elem_adj_node and dump / elem_adj_elem
CPVCS    
CPVCS       Rev 1.7   21 Jan 2004 11:38:28   tam
CPVCS    check for stor option as well as fehm and pass
CPVCS    to the dumpfehm routine so only stor file is written
CPVCS    
CPVCS       Rev 1.6   18 Sep 2003 16:06:06   gable
CPVCS    Added dump / zone_outside option.
CPVCS    
CPVCS       Rev 1.5   25 Mar 2003 09:40:12   gable
CPVCS    Add dump/zone_imt option to only output zone list file
CPVCS    associated with imt values but do not output outside
CPVCS    lists and multi-material connection lists.
CPVCS    
CPVCS       Rev 1.4   15 May 2002 08:38:54   gable
CPVCS    Fixed code so delatt/keepatt option works with dump/zone mode
CPVCS    
CPVCS       Rev 1.3   25 Jun 2001 10:56:02   gable
CPVCS     Added call to output multi-material connections in zone output.
CPVCS    
CPVCS       Rev 1.2   10 Mar 2000 09:50:26   gable
CPVCS    Remove call to mask_icr, put inside dump_outside_list.f
CPVCS    
CPVCS       Rev 1.1   14 Feb 2000 16:57:38   dcg
CPVCS    fix lengths of tests on mode
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 14:54:58 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.45   07 Feb 2000 17:36:32   dcg
CPVCS    remove unused comdict.h
CPVCS
CPVCS       Rev 1.44   Tue Nov 30 16:36:38 1999   jtg
CPVCS    if mbndry storage is non-existent, then assume mbndry=0
CPVCS    which is the "future" convention
CPVCS
CPVCS       Rev 1.43   Tue Nov 30 13:14:30 1999   dcg
CPVCS    make binary default for lagrit dumps
CPVCS
CPVCS       Rev 1.42   Wed Nov 10 15:28:28 1999   dcg
CPVCS    get rid of obsolete data base code
CPVCS
CPVCS       Rev 1.41   Fri Nov 05 13:27:52 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS
CPVCS       Rev 1.40   Tue Aug 31 15:52:38 1999   nnc
CPVCS    Uncommented the mistakenly(?) commented-out call to dump_recolor_lg.
CPVCS
CPVCS       Rev 1.39   Fri Jul 23 01:50:08 1999   jtg
CPVCS    dump_recolor_alt replaced by gcolor/dump/etc in gcolor
CPVCS    structure in neighbor_recolor_lg.f so lines referring to it
CPVCS    removed
CPVCS
CPVCS       Rev 1.38   Wed Jul 14 15:10:38 1999   dcg
CPVCS    Detect if ascii or binary file - look for errors on input and
CPVCS    allow for some mistakes
CPVCS
CPVCS       Rev 1.37   Fri Jul 09 16:49:50 1999   dcg
CPVCS    comment out calls to dump_recolor_alt....
CPVCS
CPVCS       Rev 1.36   Tue Jul 06 08:56:46 1999   jtg
CPVCS    dump_recolor_alt added
CPVCS
CPVCS       Rev 1.35   Mon Jun 07 17:08:54 1999   murphy
CPVCS    Added dump/flotran option
CPVCS
CPVCS       Rev 1.34   Tue May 11 16:49:20 1999   dcg
CPVCS    set defaults for lagrit dumps (ascii) is default
CPVCS
CPVCS       Rev 1.33   Fri Apr 02 09:54:24 1999   nnc
CPVCS    Overhauled the dump_recolor stanza.  Added the dump/colormap
CPVCS    option.  Cmo_get_name is now called in cases where cmsgin gives
CPVCS    the cmo name as '-def-'.
CPVCS
CPVCS       Rev 1.32   Tue Mar 16 09:24:46 1999   murphy
CPVCS    Fixed minor bug with compress.
CPVCS
CPVCS       Rev 1.31   Mon Mar 15 16:44:30 1999   murphy
CPVCS    Added 'asciic' and 'binaryc' options to dump/fehm.
CPVCS
CPVCS       Rev 1.30   Tue Mar 09 15:06:28 1999   dcg
CPVCS    read in cmo and pset, eset info for  dumps
CPVCS
CPVCS       Rev 1.29   Fri Mar 05 11:15:42 1999   dcg
CPVCS    dump/lagrit generates new format dump
CPVCS
CPVCS       Rev 1.26   Mon Jan 25 13:05:08 1999   llt
CPVCS    added option4 - to allow FEHM dumps to delete/keep
CPVCS    boundary attributes - default is to delete
CPVCS
CPVCS       Rev 1.25   Fri Jan 22 16:54:30 1999   dcg
CPVCS    add include consts.h
CPVCS
CPVCS       Rev 1.24   Mon Aug 31 12:27:08 1998   dcg
CPVCS    remove unused and undocumented options
CPVCS
CPVCS       Rev 1.23   Wed Feb 11 13:46:54 1998   dcg
CPVCS    add dump/geom command to write geometry info to a files
CPVCS
CPVCS       Rev 1.22   Wed Feb 11 10:40:30 1998   tam
CPVCS    added unformatted option
CPVCS
CPVCS       Rev 1.21   Wed Nov 05 12:33:42 1997   dcg
CPVCS    add capability to write DB_F file which contains a list
CPVCS    of all files produced by a run
CPVCS    activate this by adding this command to your input deck
CPVCS    assign sbgloprm -def- ipopname 'DB_F'
CPVCS
CPVCS       Rev 1.20   Wed Oct 15 14:22:30 1997   gable
CPVCS    Make default FEHM output scalar area coefficients.
CPVCS
CPVCS       Rev 1.19   Wed Oct 15 13:39:02 1997   gable
CPVCS    Modified to allow ascii or binary fehm stor file output.
CPVCS    Also added option to have scalar, vector or scalar and vector
CPVCS    area coefficients as part of output.
CPVCS
CPVCS       Rev 1.18   Thu Aug 21 14:13:58 1997   gable
CPVCS    Added option for dump / stl which will output a triangular
CPVCS    sheet CMO in a format appropriate for Stereo Lithography
CPVCS    as done by Pete Smith (ESA-WMM). Code changes by
CPVCS    L. Lundquist.
CPVCS
CPVCS       Rev 1.17   Thu Jun 19 18:47:46 1997   gable
CPVCS    added the option
CPVCS    dump / voronoi_stor / filename / cmoname
CPVCS    which will call the subroutine voronoi_stor.
CPVCS
CPVCS       Rev 1.16   Wed Jun 18 11:12:14 1997   gable
CPVCS    Changes to FEHM output. Added the commands
CPVCS    dump / zone / file / cmo
CPVCS    which will just output material and outside ZONE lists
CPVCS    dump / coord / file / cmo
CPVCS    will output just the coordinates and element connectivity
CPVCS
CPVCS       Rev 1.14   Mon Apr 07 14:52:38 1997   het
CPVCS    Add the rtt dump format for the Radiation Transport Team
CPVCS
CPVCS       Rev 1.13   Sun Feb 23 10:35:30 1997   het
CPVCS    Add the dump/rage and dump/rtt options.
CPVCS
CPVCS       Rev 1.12   Thu Oct 10 08:38:30 1996   het
CPVCS    Add the "dump/flag" command.
CPVCS
CPVCS       Rev 1.11   Wed Jun 19 10:26:00 1996   het
CPVCS    Add the "inventor" dump format.
CPVCS
CPVCS       Rev 1.10   Wed May 22 07:04:20 1996   het
CPVCS    Add the dump/sph option for sphinx.
CPVCS
CPVCS       Rev 1.9   Tue Apr 30 07:27:04 1996   het
CPVCS    Add the dumpsph option.
CPVCS
CPVCS       Rev 1.8   Tue Apr 02 02:25:36 1996   het
CPVCS    Add the dump/x3d_asci command.
CPVCS
CPVCS       Rev 1.7   Thu Feb 22 14:02:24 1996   dcg
CPVCS    remove call to dumpx3d_att
CPVCS
CPVCS       Rev 1.6   Thu Feb 08 09:05:36 1996   dcg
CPVCS    replace call to dumpgmv with dumpgmv_hybrid
CPVCS
CPVCS       Rev 1.5   Tue Feb 06 06:59:54 1996   het
CPVCS    Correct an error.
CPVCS
CPVCS       Rev 1.4   12/05/95 08:25:30   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.3   08/15/95 18:20:18   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.2   07/31/95 13:38:04   dcg
CPVCS    change format to write/option
CPVCS
CPVCS       Rev 1.1   07/17/95 16:11:54   dcg
CPVCS    original version
C
C#######################################################################
C
      implicit none
C     implicit real*8 (a-h, o-z)

      include 'chydro.h'
      include 'consts.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C.... Mesh Object Data.
C
      character*32 cmo,cdefault
C
C#######################################################################
C

      integer ierrw, ierror,icscode, if_cmo_exist, if_def_cmo
      integer ii,len,leno,lenfile,length,lenidsb
      integer imt_select,npoints,ntets,mbndry,nsdtopo,nsdgeom,
     *  nen,nef,iopt_elements,iopt_values_node,iopt_values_elem,
     *  ist1,io_format,ihcycle,time,dthydro,icmotype,iopt_points             
     
      integer icharlnf 

      logical lopt1, lopt2

      character*32 idsb
      character*32 ioption, ioption2, ioption3, ifile, iomode
      character*32 ioption4, ioption5, ioption6
      character*132 logmess
C
C#######################################################################
C
      integer icharln
C
C#######################################################################
C
C
C     In General the DUMP command has syntax
C     dump_type / dump_command / filename_out / cmo_in / [OPTIONS]
C
C     parse and fill values for ifile, cmo 
C     iomode, ioption, ioption2, ioption3, ioption4 imt_select
C     ioption5 and ioption6 are attribute options
C     Note parsing can happen before case for dump_command
C
C
C
      cdefault='default'
      idsb = cmsgin(1)
      ioption='-notset-'
      ioption2='-notset-'
      ioption3='-notset-'
      ioption4='-notset-'
      ioption5='-notset-'
      ioption6='-notset-'
      leno=1
      imt_select=0
      ierror_return = 0
      ierror = 0
      if_cmo_exist = 0
      if_def_cmo = 0
C
      lenidsb = icharln(idsb)
C
c DUMP_RECOLOR
c CALL dump_recolor_lg (type, file, cmo, restore, create, iomode, ierror)
c    type    -- Type of dump: "gmv", "x3d", or "avs".
c    file    -- Dumpfile name.
c    cmo     -- Mesh object name.
c    restore -- Logical flag.  If true, then the original ITETCLR and IMT1
c               values are restored before exiting.  If false, the mesh is
c               left recolored.
c    create  -- Logical flag.  If true, a new colormap is created and used.
c               If false, the existing colormap is used.
c    iomode  -- Dump format; e.g. "ascii" or "binary".  Which values are
c               valid will depend on the type of dump being done.

C     case idsb - first token 
C     DUMP_RECOLOR 
      if(idsb(1:lenidsb).eq.'dump_recolor') then
 
         ioption='gmv'
         if(nwds.ge.2) then
            if(msgtype(2).eq.3 .and. cmsgin(4).ne.'-def-') then
               ioption=cmsgin(2)
            endif
         endif
 
         ifile='-def-'
         if(nwds.ge.3) then
            if(msgtype(3).eq.3) ifile=cmsgin(3)
         endif
 
         call cmo_get_name(cmo,if_def_cmo)
         if(nwds.ge.4) then
            if(msgtype(4).eq.3 .and. cmsgin(4).ne.'-def-') then
               cmo=cmsgin(4)
            endif
         end if
 
         lopt1=.true.
         if(nwds.ge.5) then
            if(msgtype(5).eq.3 .and. cmsgin(5).eq.'norestore') then
               lopt1=.false.
            endif
         endif
 
         lopt2=.true.
         if(nwds.ge.6) then
            if(msgtype(6).eq.3 .and. cmsgin(6).eq.'existing') then
               lopt2=.false.
            endif
         endif
 
         iomode='binary'
         if(nwds.ge.7) then
            if(msgtype(7).eq.3) iomode=cmsgin(7)
         endif
 
         call dump_recolor_lg(ioption,ifile,cmo,lopt1,lopt2,iomode,
     *                        ierror_return)
 
         go to 9999
C
C     case idsb - first token
C     DUMP 
      elseif(idsb(1:lenidsb).eq.'dump') then
C
         ioption=cmsgin(2)
         leno=icharlnf(ioption)
 
C     DUMP / colormap
         if (ioption .eq. 'colormap') then
 
           ifile = 'colormap'
           if (nwds .ge. 3) then
             if (msgtype(3).eq.3 .and. cmsgin(3).ne.'-def-') then
               ifile = cmsgin(3)
             endif
           endif
 
           call dump_colormap_lg (ifile, ierror_return)
 
           go to 9999

C     DUMP / lagrit
C     DUMP / lgt
C     DUMP / LAGRIT
C     DUMP / LaGriT
C     DUMP / x3d
        elseif ( ioption(1:leno).eq.'lagrit'.or.
     *           ioption(1:leno).eq.'lgt'.or.
     *           ioption(1:leno).eq.'LAGRIT'.or.
     *           ioption(1:leno).eq.'LaGriT'.or.
     *           ioption(1:leno).eq.'x3d') then
           if ( ioption(1:leno).eq.'x3d') then
              write(logmess,12)
 12           format(' dump/x3d no longer supported',
     *          ' dump/lagrit will be executed instead')
              call writloga(cdefault,0,logmess,0,icscode)
           endif
           if(nwds.le.2.or.msgtype(3).ne.3) then
             ifile='lgdump'
             cmo='-all-'
             iomode='binary'
           elseif(nwds.le.3) then
             ifile=cmsgin(3)
             cmo='-all-'
             iomode='binary'
           elseif(nwds.le.4) then
             ifile=cmsgin(3)
             iomode='binary'
             cmo=cmsgin(4)
             if (cmsgin(4)(1:5).eq.'-def-')  cmo='-all-'
             if (cmsgin(4)(1:6).eq.'binary')  cmo='-all-'
             if (cmsgin(4)(1:5).eq.'ascii') then
                cmo='-all-'
                iomode='ascii'
             endif
           elseif(nwds.le.5) then
             ifile=cmsgin(3)
             cmo=cmsgin(4)
             if (cmo.eq.'-def-')  cmo='-all-'
             if(cmsgin(5)(1:5).eq.'ascii') then
                iomode='ascii'
             else
                iomode='binary'
             endif
           endif
C
           call dump_lagrit(ifile,cmo,iomode,ierror_return)
           go to 9999
         endif
c
C        pre-process some common arguments and options 
C        before continuing on to the specific dump commands
C
C        FILL cmo, ifile, iomode, ioption2, ioption3, ioption4
C        check cmo but do not return, not needed for all situations?
C        instead set the error flag which is used later 
C
         call cmo_get_name (cmo, if_def_cmo)
         if (nwds .ge. 4) then
           if (msgtype(4).eq.3 .and. cmsgin(4).ne.'-def-') then
             cmo = cmsgin(4)
           endif
         endif
 
         if (nwds.le.2) then
            ifile='-def-'
         else
            ifile=cmsgin(3)
         endif
         lenfile=icharlnf(ifile)
 
         if(cmo(1:5).ne.'-all-') call cmo_select(cmo,if_def_cmo)
         if (nwds.le.4) then
            iomode='binary'
         else
            iomode=cmsgin(5)
            if(iomode(1:5).eq.'ascii') then
               iomode='ascii'
            elseif(iomode(1:6).eq.'binary') then
               iomode='binary'
            elseif(iomode(1:5).eq.'unfor') then
               iomode='unformatted'
            else
               iomode='binary'
            endif
 
            if(cmsgin(5)(1:6).eq.'asciic') then
               iomode='asciic'
            elseif(cmsgin(5)(1:7).eq.'binaryc') then
               iomode='binaryc'
            endif
         endif

         if (nwds.le.5) then
            ioption2=' '
         else
            if(cmsgin(6)(1:5) .eq. '-def-')then
               ioption2=' '
            else
               ioption2=cmsgin(6)
            endif
         endif
         if (nwds.le.6) then
            ioption3='scalar'
         else
            ioption3=cmsgin(7)
         endif

C        check special options for dump/fehm and dump/stor
C        only check for integer setting, all other
C        keywords are checked just before call to dumpfehm

         if((ioption(1:leno).eq.'fehm') .or. 
     1      (ioption(1:leno).eq.'stor')) then

C           check for integer imt1 value for material list
            if (msgtype(nwds).eq.1) then
               imt_select = imsgin(nwds)
               nwds=nwds-1
            endif

         endif

C        check special options for dump/zone 
C        ioption5 used for outside attributes
C        ioption6 used for area type and attributes

         if((ioption(1:leno).eq.'zone') .or. 
     1      (ioption(1:leno).eq.'zone_outside') .or.
     2      (ioption(1:leno).eq.'zone_imt')) then

C           check for selected imt1 value for material list
            if (msgtype(nwds).eq.1) then
               imt_select = imsgin(nwds)
               nwds=nwds-1
            endif

            if (nwds .le. 4) then
               ioption5='delatt'
               ioption6='-notset-'
            
            else
c           look for attribute and area options in 5 and 6
c           option 5 is for outside attributes
c           option 6 is for area type and attributes

            do ii = 5, nwds
               if (cmsgin(ii).eq.'keepatt') ioption5='keepatt'
               if (cmsgin(ii).eq.'delatt') ioption5='delatt'
               if (cmsgin(ii).eq.'keepatt_area') 
     *                     ioption6='keepatt_voronoi'
               if (cmsgin(ii).eq.'keepatt_voronoi') 
     *                     ioption6='keepatt_voronoi'
               if (cmsgin(ii).eq.'keepatt_median') 
     *                     ioption6='keepatt_median'
            enddo
            endif

c        pass outside minmax option keepatt through string argument
C        ioption4 used for outside attributes
C        ioption5 used for area type and attributes

         else if (ioption(1:leno).eq.'zone_outside_minmax')then

           do ii = 5, nwds
               if (cmsgin(ii).eq.'keepatt') ioption5='keepatt'
               if (cmsgin(ii).eq.'delatt') ioption5='delatt'
               if (cmsgin(ii).eq.'keepatt_area') 
     *                     ioption6='keepatt_voronoi'
               if (cmsgin(ii).eq.'keepatt_voronoi') 
     *                     ioption6='keepatt_voronoi'
               if (cmsgin(ii).eq.'keepatt_median') 
     *                     ioption6='keepatt_median'
            enddo

         endif

 
C     case idsb - first token
C     NOT DUMP_RECOLOR OR DUMP
      else
         ifile=cmsgin(2)
         lenfile=icharlnf(ifile)
 
         call cmo_get_name (cmo, if_def_cmo)
         if (nwds .ge. 3) then
           if (msgtype(3).eq.3 .and. cmsgin(3).ne.'-def-') then
             cmo = cmsgin(3)
           endif
         endif
 
         call cmo_select(cmo,if_def_cmo)
         if (nwds.le.3) then
            iomode='binary'
         else
            iomode=cmsgin(4)
            if(iomode(1:5).eq.'ascii') then
               iomode='ascii'
            elseif(iomode(1:6).eq.'binary') then
               iomode='binary'
            elseif(iomode(1:5).eq.'unfor') then
               iomode='unformatted'
            else
               iomode='binary'
            endif
 
            if(cmsgin(4)(1:6).eq.'asciic') then
               iomode='asciic'
            elseif(cmsgin(4)(1:7).eq.'binaryc') then
               iomode='binaryc'
            endif
 
         endif
         if (nwds.le.4) then
            ioption2=' '
         else
            ioption2=cmsgin(5)
         endif
      endif
C     DONE with 
C     case DUMP_RECOLOR
C     case DUMP pre-process of some commands   

C     set cmo and ierror_return in case things blow up
      call cmo_exist(cmo,if_cmo_exist)
      if(if_def_cmo.ne.0) then
        write(logmess,*) 
     *  'WRITEDUMP Warning: cannot find default mesh object.'
        call writloga('default',0,logmess,0,ierrw)
        ierror_return = -3
      endif

      call cmo_exist(cmo,if_cmo_exist)
      if(if_cmo_exist.ne.0) then
        write(logmess,*) 
     *  'WRITEDUMP Warning: cannot find selected mesh object '
     *   //cmo(1:icharlnf(cmo))
        call writloga('default',0,logmess,0,ierrw)
        ierror_return = -2
      endif

C     swith on case idsb again and call appropriate routines 
C     assume cmo is set, if_cmo_exist holds result from cmo check

C     DUMPGMV 
C     DUMP / gmv 
      if(idsb(1:lenidsb ).eq.'dumpgmv' .or.
     *         ioption(1:leno).eq.'gmv') then
C
         if(if_cmo_exist.eq.0) then
            call dumpgmv_hybrid(ifile(1:lenfile),cmo,iomode)
            ierror_return = 0
         endif
C
C     DUMPINV  
C     DUMP / inv
      elseif(idsb(1:lenidsb ).eq.'dumpinv' .or.
     *         ioption(1:leno).eq.'inv') then
C
         if(if_cmo_exist.eq.0) then
            call dumpinventor(ifile(1:lenfile),cmo)
            ierror_return = 0
         endif

C     DUMPSTL
C     DUMP / stl
CCCCCCC  ADDED BY LORAINE
CCCCCCC  stl output
CCCCCCC  Don't forget that you commented out the 'rtt' option!!!CCC
      elseif(idsb(1:lenidsb ).eq.'dumpstl' .or.
     *         ioption(1:3).eq.'stl') then
         if(if_cmo_exist.eq.0) then
            call dumpstl(ifile(1:lenfile),cmo)
            ierror_return = 0
         else
            write(logmess,'(a)') 'DUMPSTL cannot find mesh object'
            call writloga('default',0,logmess,0,ierrw)
         endif
C
C     DUMPAVS 
C     DUMP / avs
C     DUMP / att_node
C     DUMP / att_elem
C     DUMP / geofest
      elseif(idsb(1:lenidsb ).eq.'dumpavs' .or.
     *         ioption(1:3).eq.'avs' .or. 
     *         ioption(1:8).eq.'att_node' .or. 
     *         ioption(1:8).eq.'att_elem' .or. 
     *         ioption(1:7).eq.'geofest') then
C
C
         if(if_cmo_exist.eq.0) then
            len=icharlnf(cmo)
            call cmo_get_info('nnodes',cmo,
     *                        npoints,length,icmotype,ierror)
            call cmo_get_info('nelements',cmo,
     *                        ntets,length,icmotype,ierror)
            call cmo_get_info('mbndry',cmo,
     *                        mbndry,length,icmotype,ierror)
            if (ierror.ne.0) mbndry=0
            call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,length,icmotype,ierror)
            call cmo_get_info('ndimensions_geom',cmo,
     *                        nsdgeom,length,icmotype,ierror)
            call cmo_get_info('nodes_per_element',cmo,
     *                        nen,length,icmotype,ierror)
            call cmo_get_info('faces_per_element',cmo,
     *                        nef,length,icmotype,ierror)
            if (nwds.le.4) then
C
C   Default to everything turned on
C
               iopt_points=1
               iopt_elements=1
               iopt_values_node=1
               iopt_values_elem=1
               if(ioption(1:8).eq.'att_node')then
                  iopt_points=0
                  iopt_elements=0
                  iopt_values_node=2
                  iopt_values_elem=0
               endif
               if(ioption(1:8).eq.'att_elem')then
                  iopt_points=0
                  iopt_elements=0
                  iopt_values_node=0
                  iopt_values_elem=2
               endif
               if(ioption(1:7).eq.'geofest')then
                  iopt_values_node=0
                  iopt_values_elem=0
               endif
            endif
            if (nwds.ge.5) then
C
C   Default to elements connectivity, node att, element att turned on
C
C   Valid input, 0, 1, 2
C   iopt_points      = 0 Do not output node coordinate information
C   iopt_points      = 1 Output node coordinate information node#, x, y, z (DEFAULT)
C   iopt_points      = 2 Output node coordinates information without node number in first column, x, y, z
C   iopt_elements    = 0 Do not output element connectivity information
C   iopt_elements    = 1 Output element connectivity information (DEFAULT)
C   iopt_values_node = 0 Do not output node attribute information
C   iopt_values_node = 1 Output node attribute information (DEFAULT)
C   iopt_values_node = 2 Output node attribute information without node number in first column
C   iopt_values_elem = 0 Do not output element attribute information
C   iopt_values_elem = 1 Output element attribute information (DEFAULT)
C   iopt_values_elem = 2 Output element attribute information without node number in first column
C
               ist1=5
               if(msgtype(ist1).eq.1) then
                   if(imsgin(ist1).gt. 2) then
                      iopt_points = 1
                   elseif(imsgin(ist1).lt. 0) then
                      iopt_points = 1                   
                   else
                      iopt_points = max(0,min(2,imsgin(ist1)))
                   endif
               elseif(msgtype(ist1).eq.2) then
                   if(xmsgin(ist1).gt. 2.0) then
                      iopt_points = 1
                   elseif(xmsgin(ist1).lt. 0.0) then
                      iopt_points = 1   
                   else
                      iopt_points = max(zero,min(two,xmsgin(ist1)))
                   endif
               else
                   iopt_points=1
               endif
               iopt_elements=1
               iopt_values_node=1
               iopt_values_elem=1
               if(ioption(1:7).eq.'geofest')then
                  iopt_values_node=0
                  iopt_values_elem=0
               endif
            endif
            if (nwds.ge.6) then
C
C   Default to node att, element att turned on
C
               ist1=6
               if(msgtype(ist1).eq.1) then
                   if(imsgin(ist1).gt. 2) then
                      iopt_elements = 1
                   elseif(imsgin(ist1).lt. 0) then
                      iopt_elements = 1                   
                   else
                      iopt_elements = max(0,min(2,imsgin(ist1)))
                   endif
               elseif(msgtype(ist1).eq.2) then
                   if(xmsgin(ist1).gt. 2.0) then
                      iopt_elements = 1
                   elseif(xmsgin(ist1).lt. 0.0) then
                      iopt_elements = 1   
                   else
                      iopt_elements = max(zero,min(two,xmsgin(ist1)))
                   endif
               else
                  iopt_elements=1
               endif
               iopt_values_node=1
               iopt_values_elem=1
               if(ioption(1:7).eq.'geofest')then
                  iopt_values_node=0
                  iopt_values_elem=0
               endif
            endif
            if (nwds.ge.7) then
C
C   Default to element att set the same as node attribute flag
C   This keeps things compatible with the days when element attribute
C   flag did not exist and there were only 3 arguments after the cmo name.
C
               ist1=7
               if(msgtype(ist1).eq.1) then
                   if(imsgin(ist1).gt. 2) then
                      iopt_values_node = 1
                   elseif(imsgin(ist1).lt. 0) then
                      iopt_values_node = 1                   
                   else
                      iopt_values_node = max(0,min(2,imsgin(ist1)))
                   endif
               elseif(msgtype(ist1).eq.2) then
                   if(xmsgin(ist1).gt. 2.0) then
                      iopt_values_node = 1
                   elseif(xmsgin(ist1).lt. 0.0) then
                      iopt_values_node = 1   
                   else
                      iopt_values_node = max(zero,min(two,xmsgin(ist1)))
                   endif
               else
                  iopt_values_node = 1
               endif
               iopt_values_elem=iopt_values_node
            endif
            if (nwds.ge.8) then
               ist1=8
               if(msgtype(ist1).eq.1) then
                   if(imsgin(ist1).gt. 2) then
                      iopt_values_elem = 1
                   elseif(imsgin(ist1).lt. 0) then
                      iopt_values_elem = 1                   
                   else
                      iopt_values_elem = max(0,min(2,imsgin(ist1)))
                   endif
               elseif(msgtype(ist1).eq.2) then
                   if(xmsgin(ist1).gt. 2.0) then
                      iopt_values_elem = 1
                   elseif(xmsgin(ist1).lt. 0.0) then
                      iopt_values_elem = 1   
                   else
                      iopt_values_elem = max(zero,min(two,xmsgin(ist1)))
                   endif
               else
                  iopt_values_elem=1
               endif
            endif
C
C    Reset flags
C
               if(ioption(1:8).eq.'att_node')then
                  iopt_points=0
                  iopt_elements=0
                  iopt_values_node=2
                  iopt_values_elem=0
               endif
               if(ioption(1:8).eq.'att_elem')then
                  iopt_points=0
                  iopt_elements=0
                  iopt_values_node=0
                  iopt_values_elem=2
               endif
C
            if(idsb(1:lenidsb ).eq.'dumpavs' .or.
     *         ioption(1:3).eq.'avs' .or.
     *         ioption(1:8).eq.'att_node' .or.
     *         ioption(1:8).eq.'att_elem')then
     
            if(ioption(1:4).eq.'avs2')then
               io_format = 2
            elseif(ioption(1:8).eq.'att_node')then
               io_format = 3
            elseif(ioption(1:8).eq.'att_elem')then
               io_format = 4
            else
               io_format = 1
            endif
            if((iopt_points .eq. 0) .or.
     *         (iopt_points .eq. 2) .or.
     *         (iopt_elements .eq. 2) .or.
     *         (iopt_values_node .eq. 2) .or.
     *         (iopt_values_elem .eq. 2)) then
                if(io_format .ne. 3 .and. io_format .ne. 4)then
               write(logmess,'(a,i4)')
     *    'WARNING: dump/avs  iopt_points= ', iopt_points
               call writloga('default',0,logmess,0,ierrw)
               write(logmess,'(a,i4)')
     *    'WARNING: dump/avs  iopt_elements= ', iopt_elements
               call writloga('default',0,logmess,0,ierrw)
               write(logmess,'(a,i4)')
     *    'WARNING: dump/avs  iopt_values_node= ', iopt_values_node
               call writloga('default',0,logmess,0,ierrw)
               write(logmess,'(a,i4)')
     *    'WARNING: dump/avs  iopt_values_elem= ', iopt_values_elem
               call writloga('default',0,logmess,0,ierrw)
               write(logmess,'(a)')
     *    'WARNING: dump/avs  will produce non-standard AVS output that'
               call writloga('default',0,logmess,0,ierrw)
               write(logmess,'(a)')
     *    'WARNING: read/avs may not be able to read.'
               call writloga('default',0,logmess,0,ierrw)
            endif
            endif
            call dumpavs(ifile(1:lenfile),cmo,
     *                   nsdtopo,nen,nef,
     *                   npoints,ntets,mbndry,
     *                   ihcycle,time,dthydro,
     *                   iopt_points,iopt_elements,
     *                   iopt_values_node,iopt_values_elem,io_format)
            ierror_return = 0
            endif
            if(ioption(1:7).eq.'geofest')then
            iopt_values_elem=0
            call dumpgeofest(ifile(1:lenfile),cmo,
     *                   nsdtopo,nen,nef,
     *                   npoints,ntets,mbndry,
     *                   ihcycle,time,dthydro,
     *                   iopt_points,iopt_elements,
     *                   iopt_values_node,iopt_values_elem)
            ierror_return = 0
            endif
         else
            write(logmess,'(a)') 'DUMPAVS cannot find mesh object'
            call writloga('default',0,logmess,0,ierrw)
         endif
C
C     DUMPCHAD
C     DUMP / chad 
      elseif(idsb(1:lenidsb ).eq.'dumpchad' .or.
     *         ioption(1:leno).eq.'chad') then
C
         if(if_cmo_exist.eq.0) then
            len=icharlnf(cmo)
            call cmo_get_info('nnodes',cmo,
     *                        npoints,length,icmotype,ierror)
            call cmo_get_info('nelements',cmo,
     *                        ntets,length,icmotype,ierror)
            call cmo_get_info('mbndry',cmo,
     *                        mbndry,length,icmotype,ierror)
            if (ierror.ne.0) mbndry=0
            call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,length,icmotype,ierror)
            call cmo_get_info('ndimensions_geom',cmo,
     *                        nsdgeom,length,icmotype,ierror)
            call cmo_get_info('nodes_per_element',cmo,
     *                        nen,length,icmotype,ierror)
            call cmo_get_info('faces_per_element',cmo,
     *                        nef,length,icmotype,ierror)
            call dumpchad(ifile(1:lenfile),ierror)
         endif
C
c      elseif(ioption(1:leno).eq.'tecplot_ascii') then
c
c     Output ascii mesh for TECPLOT
c         if(if_cmo_exist.eq.0) then
c            len=icharlnf(cmo)
c            call dumptecplot_ascii(ifile(1:lenfile),
c     *          ioption2(1:icharlnf(ioption2)),iomode,ioption3)
             ierror_return = 0
c         endif

C     DUMP / tecplot
      elseif(ioption(1:leno).eq.'tecplot') then
c
c     Output hybrid  mesh for TECPLOT
           if(msgtype(5).eq.3 .and. cmsgin(5).eq.'fsets') then
               ioption=cmsgin(5)
            endif
         if(if_cmo_exist.eq.0) then
            len=icharlnf(cmo)
            call dumptecplot_hybrid(ifile(1:lenfile),
     *          ioption2(1:icharlnf(ioption2)),iomode)
            ierror_return = 0
         endif

C     DUMPFEHM
C     DUMP / fehm
C     DUMP / stor
C
      elseif((idsb(1:lenidsb ).eq.'dumpfehm') .or.
     *         (ioption(1:leno).eq.'stor') .or.
     *         (ioption(1:leno).eq.'fehm')) then

C       Allow keywords any place after the file_name and cmo
C       Set defaults, find and send option settings 
C       dumpfehm will check allowable combinations
C       ifile, 
C         ioption2 is ifileini name for dump_fehm_geom
C         ioption is fehm or stor
C         iomode is writing mode (default ascii) 
C         ioption3 is coef_option (default scalar)
C         ioption4 is compress option (default all)
C         ioption5 is outside attribute option (default delatt)
C         ioption6 is outside area option (default -notset-)
C       

        if(if_cmo_exist.eq.0) then
           len=icharlnf(cmo)
c          make sure default ioptions are set 
           if (ioption(1:leno).ne.'stor') ioption='fehm'
           iomode = 'ascii'
           ioption3 = 'scalar'
           ioption4 = 'all'
           ioption5 = 'delatt'
           ioption6 = '-notset-'


C          loop through msgtype to find options
           if (idebug .ne. 0) then
           print*,'writedump msgtype for stor and fehm: '
           endif

           do ii = 1,nwds

            if (idebug .ne. 0) then
              if (msgtype(ii) .eq. 3) print*, ii, cmsgin(ii)
              if (msgtype(ii) .eq. 2) print*, ii, imsgin(ii)
              if (msgtype(ii) .eq. 1) print*, ii, xmsgin(ii)
            endif

c           Find keywords and set options for dumpfehm routine
 
            if (msgtype(ii) .eq. 3) then
              if (cmsgin(ii)(1:5).eq.'ascii') iomode='ascii'
              if (cmsgin(ii)(1:6).eq.'binary') iomode='binary'
              if (cmsgin(ii).eq.'unformatted') iomode='binary'
              if (cmsgin(ii).eq.'scalar') ioption3='scalar'
              if (cmsgin(ii).eq.'vector') ioption3='vector'
              if (cmsgin(ii).eq.'both') ioption3='both'
              if (cmsgin(ii).eq.'area_scalar') ioption3='area_scalar'
              if (cmsgin(ii).eq.'area_vector') ioption3='area_vector'
              if (cmsgin(ii).eq.'area_both') ioption3='area_both'
              if (cmsgin(ii).eq.'all') ioption4='all'
              if (cmsgin(ii).eq.'graph') ioption4='graph'
              if (cmsgin(ii)(1:4).eq.'coef') ioption4='coefs'
              if (cmsgin(ii).eq.'none') ioption4='none'
              if (cmsgin(ii).eq.'keepatt') ioption5='keepatt'
              if (cmsgin(ii).eq.'delatt') ioption5='delatt'
              if (cmsgin(ii).eq.'keepatt_voronoi') 
     *                      ioption6='keepatt_voronoi'
              if (cmsgin(ii).eq.'keepatt_median') 
     *                      ioption6='keepatt_median'

C          check for old syntax with alternate_scalar
C          dump / stor / file_name_as / cmo / asciic / / alternate_scalar
              if (cmsgin(ii).eq.'alternate_scalar') then
                 ioption3='scalar'
                 ioption4='graph'
                 if (cmsgin(5).eq.'asciic') ioption4='all'
                 if (cmsgin(5).eq.'binaryc') ioption4='all'
              endif 
            endif
           enddo

           call dumpfehm(ifile(1:icharlnf(ifile)),
     *          ioption2(1:icharlnf(ioption2)),
     *          ioption, iomode,
     *          ioption3, ioption4, ioption5, ioption6)
           ierror_return = 0
            
         endif

C     DUMP / coord (for fehm)
      elseif(ioption(1:leno).eq.'coord') then
c
c     Output only the FEHM COORD and ELEM macro information
         if(if_cmo_exist.eq.0) then
            len=icharlnf(cmo)
            call dump_fehm_geom(ifile(1:lenfile),
     *          ioption2(1:icharlnf(ioption2)),iomode,ioption3)
            ierror_return = 0
         endif

C     DUMP / zone_imt (for fehm)
      elseif(ioption(1:leno).eq.'zone_imt') then
c
c     Output only the FEHM imt zone macro information
C
         if(if_cmo_exist.eq.0) then
          len=icharlnf(cmo)
          call dump_material_list (ifile(1:lenfile),imt_select)
          ierror_return = 0
         endif

C     DUMP / zone_outside (for fehm)
      elseif(ioption(1:leno).eq.'zone_outside') then
c
c     Output only the FEHM outside (normal) zone and areas
C         ioption5 is outside attribute option (default delatt)
C         ioption6 is outside area option (default -notset-)
C
         if (idebug .gt. 0) then
           print*,'call dump zone_outside with ',ioption5,ioption6
         endif

         if(if_cmo_exist.eq.0) then
          len=icharlnf(cmo)
          call dump_outside_list(ifile(1:lenfile),
     *                      ioption5,ioption6)
          ierror_return = 0
         endif

C     DUMP / zone_outside_minmax (for fehm)
      elseif(ioption(1:leno).eq.'zone_outside_minmax') then
c
c     Output only the minmax ijk of outside (normal) nodes
C     pass double argument through string argument
C       ioption5 is outside attribute option (default delatt)
C       ioption6 is outside area option (default -notset-)
C
         if(if_cmo_exist.eq.0) then

          if (ioption5(1:12).eq.'keepatt_area' ) then
             ioption5='minmax_keepatt_area'
          else if (ioption5(1:6).eq.'delatt' ) then
             ioption5='minmax_delatt'
          else if (ioption5(1:7).eq.'keepatt' ) then
             ioption5='minmax_keepatt'
          endif
          
          len=icharlnf(cmo)
          call dump_outside_list(ifile(1:lenfile),
     *             ioption5, ioption6)
          ierror_return = 0
         endif

C     DUMP / zone (for fehm)
      elseif(ioption(1:leno).eq.'zone') then
c
c     Output only the FEHM COORD and ELEM macro information
C
         if(if_cmo_exist.eq.0) then
          len=icharlnf(cmo)
          call dump_material_list(ifile(1:lenfile),imt_select)
          call dump_outside_list(ifile(1:lenfile),ioption5,ioption6)
          call dump_interface_list(ifile(1:lenfile))
          call dump_multi_mat_con (ifile(1:lenfile))
          ierror_return = 0
         endif
c
C     DUMP / voronoi_stor
c     Output stor file using voronoi search algorithm
c     writes stor file and gmv file
C
      elseif(ioption(1:leno).eq.'voronoi_stor') then
            len = icharlnf(ifile)
            if (ifile(1:len) .eq. '-def-') then
              ifile ='voronoi'
            endif
            if(if_cmo_exist.eq.0) then
               call voronoi_stor(cmo,'all',ifile)
               ierror_return = 0
            endif
 
C
Cdcg     elseif(idsb(1:lenidsb ).eq.'dumpsgi' .or.
Cdcg    *       idsb(1:lenidsb ).eq.'sgidump' .or.
Cdcg    *         ioption(1:leno).eq.'sgi') then
C
Cdcg        if(ierror.eq.0) then
Cdcg           call sgidump(ifile(1:lenfile))
Cdcg        endif
C
C
C     DUMPDATEX
C     DUMPSIMUL
C     DUMP / datex
C     DUMP / imul
      elseif(idsb(1:lenidsb ).eq.'dumpdatex' .or.
     *    idsb(1:lenidsb ).eq.'dumpsimul' .or.
     *    ioption(1:leno).eq.'datex'.or.ioption(1:leno).eq.'simul') then
         if(if_cmo_exist.eq.0) then
            call cmo_get_info('nnodes',cmo,
     *                        npoints,length,icmotype,ierror)
            call cmo_get_info('nelements',cmo,
     *                        ntets,length,icmotype,ierror)
            call cmo_get_info('mbndry',cmo,
     *                        mbndry,length,icmotype,ierror)
            if (ierror.ne.0) mbndry=0
            call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,length,icmotype,ierror)
            call cmo_get_info('ndimensions_geom',cmo,
     *                        nsdgeom,length,icmotype,ierror)
            call cmo_get_info('nodes_per_element',cmo,
     *                        nen,length,icmotype,ierror)
            call cmo_get_info('faces_per_element',cmo,
     *                        nef,length,icmotype,ierror)
            call dumpdatex(ifile(1:lenfile),cmo,
     *                     nsdgeom,nen,nef,
     *                     npoints,ntets,
     *                     ihcycle,time,dthydro)
            ierror_return = 0
         endif
C
C      DUMPSPH
C      DUMP / sph
c      elseif(idsb(1:lenidsb ).eq.'dumpsph' .or.
c     *         ioption(1:leno).eq.'sph') then
C
c        if(ierror.eq.0) then
c            call dumpsph_binary(ifile(1:lenfile),cmo)
c         endif
C
c      elseif(ioption(1:leno).eq.'rage') then
C
c         call dumprage(ifile(1:lenfile),cmo)
 
C     DUMPFLOTRAN
C     DUMP / flotran
      elseif(idsb(1:lenidsb ).eq.'dumpflotran' .or.
     *         ioption(1:leno).eq.'flotran') then
 
         call dumpflotran(ifile(1:lenfile))
         ierror_return = 0
 
      elseif(ioption(1:leno).eq.'elem_adj_elem') then
 
         call write_element_element
     *    (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror_return)
         
 
      elseif(ioption(1:leno).eq.'elem_adj_node') then
 
         call write_element_node_neigh
     *    (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror_return)
 
C      elseif(ioption(1:leno).eq.'rtt') then
C
C         call dumprtt(ifile(1:lenfile),cmo)
C
      else
C
         write(logmess,'(a,a)') 'Invalid DUMP Option', idsb
         call writloga('default',0,logmess,0,ierrw)
         ierror_return=-1
C
      endif
C
 9999 continue

C     catch any errors might that have passed through

      if (ierror_return .ne. 0 ) then
         write(logmess,'(a6,a,2x,a,a,i5)') 
     *   'Write ', idsb(1:icharlnf(idsb)),
     *    ioption(1:icharlnf(ioption)),
     *   ' returned with error: ', ierror_return 
         call writloga('default',1,logmess,1,ierrw)
      endif

      return
      end
