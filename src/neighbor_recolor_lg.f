*dk,neighbor_recolor_lg
C #####################################################################
C
C   PURPOSE -
C
C      This suite of subroutines handles the "gcolor" commands which
C      deal with creating and using a "color" for each imt1 in all
C      cmos used in the map which can be used to recolor the cmos
C      (eg, in a gmv output dump) so that a minimal number of colors
C      is used and no color touches a material with a like color.
C      If distance=2, then also no material touches a material which
C      touches a material with like color, and etc for distance>2.
C
C   COMMAND FORMAT -
C
C   (Note: to modify command root replace "gcolor/" with
C    whatever you want and modify nwdsskip appropriately.
C    See also below near the start of the subroutine.)
C
C NOTE 9/20/99: "gcolor" made equivalent to 
C        "colormap/{isn|isn1|pointwise|gcolor}" in the following commands
C        and, e.g., "colormap/unique" allowed too (changes to msgtty)
C
C   COMMAND FORMAT -
C     for operations just on the map:
C
C     gcolor[/mapname]/create
C        - release map storage space to make room for new map
C          (created on first add)
C     gcolor[/mapname]/select
C        - select mapname
C     gcolor[/mapname]/release
C        - release map storage space
C     gcolor[/mapname]/map[/#]
C        - form the map [at distance #]
C     gcolor[/mapname]/distance/#
C        - assign a further neighbor distance to use for the map
C     gcolor[/mapname]/valid
C        - reset the validity table to all imt values
C          (only valid materials are recolored or made unique,
C           the default is all materials)
C     gcolor[/mapname]/invalid
C        - reset the validity table to no imt values
C     gcolor[/mapname][/valid|invalid]/new
C        - reset the default validity for new imt1 values added to the map
C           (valid is the default)
C     gcolor[/mapname][/valid|invalid]/range/firstimt,lastimt
C        - add or subtract a range of imt values from the
C          validity table for recoloring
C     gcolor[/mapname][/valid|invalid]/list/imt1_1,imt1_2,....
C        - add or subtract a list of imt values from the
C          validity table for recoloring
C     gcolor[/mapname]/put[/filename|-def-|-full-]
C        - print the map to filename (default output if no filename,
C         ('-full-' prints table to default output too)
C     gcolor[/mapname]/get/filename
C        - get the map from filename
C          (releases previous if any)
C
C   COMMAND FORMAT -
C     for operations using the cmo's to modify the map:
C     (also creates the map if none currently exists)
C
C     gcolor[/mapname]/add[/cmoname]
C        - use cmoname to increment the map
C     gcolor[/mapname]/addfiles[/cmoname]/readtype/filename1 filename2 ....
C        - read and use the info in filename1,filename2,... to increment the map
C          readtype=gmv|avs|lgt:
C             * create cmoname if it doesn't exist,
C             * do the indicated read of file1,etc (into cmoname if not lgt),
C             * use the info in cmoname to increment the map
C     gcolor[/mapname]/addfilein[/cmoname]/readtype/filename
C        - same as addfiles except that filename contains a list of
C          files of the specified type to be read and used rather than
C          being a dump file.
C
C   NOTE: in general you must add the map before dumping a file using it, etc...
C
C   COMMAND FORMAT -
C     for operations on the cmo's using the map:
C
C     gcolor[/mapname]/recolor[/cmoname/oldimtname]
C        - recolor cmoname using the map
C          if oldimtname is present and not= -none-|imt1|imt,
C          the the old imt value is stored in the attribute oldimtname
C     gcolor[/mapname]/dump/dumptype/filename[/oldimtname/cmoname]
C        - dump with dumptype using the map cmoname into filename
C          (will not be left recolored)
C          unless oldimtname='-none-' (default), the original imt values
C          will be output in the dump in the attribute oldimtname
C     gcolor[/mapname]/dumpfiles/dumptype[/oldimtname/cmoname]/readtype/filename1 filename2 ...
C        - read, recolor, and dump into 'GC'//filename1 etc using the extant
C          map (not incremented)
C     gcolor[/mapname]/dumpfilein/dumptype[/oldimtname/cmoname]/readtype/filename
C        - read, recolor, and dump (with 'GC' prepended to name)
C          the files listed in filename using the extant map (not incremented)
C
C     gcolor[/mapname]/files/dumptype[/oldimtname/cmoname]/readtype/filename1 filename2 ...
C        - keep previous map info if any (eg, distance may have been set),
C          read filename1,filename2,etc and increment map,
C          read again and recolor, and dump into 'GC'//filename1 etc
C     gcolor[/mapname]/filein/dumptype[/oldimtname/cmoname]/readtype/filename
C        - keep previous map info if any (eg, distance may have been set),
C          read all files in filein and increment map,
C          then reread, recolor, and dump (with 'GC' prepended to name)
C
C   COMMAND FORMAT -
C     for other operations on the cmo's related to "region colors":
C
C     gcolor[/mapname]/unique[/cmoname/oldimtname]
C        - assign a unique, ordered number to each connected material region
C          if oldimtname=-none-|imt|imt1 or is not present, the old color will
C          not be stored, otherwise it will be added as an attribute
C          with the name oldimtname
C          (modify?: perhaps use and eltset rather than the valid flag? ...)
C          mapname needed re validity table possibly set
C     gcolor[/mapname]/stats[/cmoname/filename]
C        - give statistics on the connected regions
C          (number of unique matl regions; av,mean vol/region;
C           min,max vol/region)
C          print to filename if exists (incremented), otherwise to screen
C          mapname needed re validity table possibly set
C
C   USAGE NOTES -
C
C        - mesh independent
C        - based on the point-wise connectivity of the isn chains
C        - compare colormap.f90, which recolors based on face-connectivity
C          and is only for simplicies, and does not have a distance option
C
C   CHANGE HISTORY -
C
C  $Log: neighbor_recolor_lg.f,v $
C  Revision 2.00  2007/11/05 19:46:02  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   05 May 2000 17:51:42   jtg
CPVCS    corrections made re 'dotask' destroying cmsgin etc
CPVCS    
CPVCS       Rev 1.4   05 May 2000 15:25:40   jtg
CPVCS    dumptype x3d converted to lgt
CPVCS    
CPVCS       Rev 1.3   03 May 2000 01:28:08   jtg
CPVCS    vor2d set to "no" for recolored dump
CPVCS    
CPVCS       Rev 1.2   02 May 2000 12:14:34   jtg
CPVCS    on dump, if unlisted itetclrs are encountered, a temporary
CPVCS    color is created for that itetclr if less than 128 new colors
CPVCS    would be created, otherwise the color is set to 
CPVCS    the max of 128 or ncolors+1
CPVCS    
CPVCS       Rev 1.1   26 Jan 2000 16:33:04   jtg
CPVCS    fixed Log line, removed get_info_c calls (not cmo_get_attparam).
CPVCS    
CPVCS       Rev 1.13   Tue Nov 30 17:11:04 1999   jtg
CPVCS    mapname can now be specified (and hence multiple maps can exist).
CPVCS    Alos, made consistent with networks and mbndry=0
CPVCS    
CPVCS       Rev 1.12   Mon Sep 20 14:40:30 1999   jtg
CPVCS    "gcolor" made equivalent to "colormap/isn"
CPVCS    
CPVCS       Rev 1.11   Fri Jul 23 12:51:44 1999   jtg
CPVCS    made valid check in unique slightly safer
CPVCS
CPVCS       Rev 1.10   Fri Jul 23 09:58:48 1999   dcg
CPVCS    split 'too long' line
CPVCS
CPVCS       Rev 1.9   Fri Jul 23 07:50:10 1999   jtg
CPVCS    gcolor/valid option (sort of eltset - really matl set) added
CPVCS
CPVCS       Rev 1.8   Fri Jul 23 02:06:54 1999   jtg
CPVCS    made name of controlling routine consistent with filename
CPVCS
CPVCS       Rev 1.7   Fri Jul 23 01:45:42 1999   jtg
CPVCS    initial "gcolor" version
CPVCS
CPVCS    (Rev 1.0-1.6 is obsolete code:
CPVCS     while the general approach of Rev 1.7 and on is the same,
CPVCS     the dependence on reading the file with the largest nmats first
CPVCS     and various other legacies of the initial realization of the idea
CPVCS     have been removed in Rev 1.7 and following, and the code has been
CPVCS     substantially cleaned up)
C
C HISTORICAL NOTES:
C
C  This replaces the original neighbor_recolor_lg and dump_recolor_lg
C  (called dump_recolor_alt_lg in latest version which was
C  concatentated into Rev 1.6  of neighbor_recolor_lg)
C
c23456789012345678901234567890123456789012345678901234567890123456789012
c ........................................................................
C #####################################################################

C #####################################################################
C     BLOCK DATA for neighbor_recolor_lg
C
C     PURPOSE -
C      initialization
C
C -----------------------------------------------------------------
C
      block data gcolor_data_lg

      integer nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params(5,20)
      common /gcolor_com_lg/ nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params
      save /gcolor_com_lg/
      data nmats,ncolors,distance,nclrnbrs,valid_new / 0,0,1,0,1 /
      data current_map,nmaps / 0,0 /
      data save_gcolor_params /
     &          0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1
     &        , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1
     &        , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1
     &        , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1
     &        , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1 , 0,0,1,0,1 /

      character*32 mapname,save_mapname(20)
      common /gcolor_mapname_lg/ mapname,save_mapname
      save /gcolor_mapname_lg/
      data mapname / 'gcolor_lg' /
      data save_mapname /'-none-','-none-','-none-','-none-','-none-'
     &                  ,'-none-','-none-','-none-','-none-','-none-'
     &                  ,'-none-','-none-','-none-','-none-','-none-'
     &                  ,'-none-','-none-','-none-','-none-','-none-'/

      end

C #####################################################################

C #####################################################################
C     SUBROUTINE neighbor_recolor_lg
C
C     PURPOSE -
C      preprocessor for the gcolor commands (see above)
C
C     INPUT ARGUMENTS -
C
C        imsgin() - Integer array of command input tokens
C        xmsgin() - Real array of command input tokens
C        cmsgin() - Character array of command input tokens
C        msgtyp() - Integer array of command input token types
C        nwds     - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c                 if =-10, then needed the other colormap routine
C
C -----------------------------------------------------------------
C
      subroutine neighbor_recolor_lg(
     *   imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)


      implicit none

      integer nwds, nwdsskip, imsgin(nwds), msgtyp(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror,iwd

      integer nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params(5,20)
      common /gcolor_com_lg/ nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params
      save /gcolor_com_lg/

      pointer (ip_files,files)
      character*32 files(*)

      pointer (ip_gclr_valid,gclr_valid)
      integer gclr_valid(*)

      integer icharlnf,nextlun
      external icharlnf,nextlun

      character*132 cbuf
      character*32 dumptype,action,oldimtname,filename
     &            ,cmo,cmosave,readtype,vaction
      character*32 mapname,save_mapname(20)
      common /gcolor_mapname_lg/ mapname,save_mapname
      save /gcolor_mapname_lg/
      integer ierr,lenaction,ifile,nfiles,iunit,readlen,len
     &       ,filelen,dumplen,iexist,i,j,k,local_debug,ivalid


c ===============================================================

      local_debug=0

      call cmo_get_name(cmosave,ierr)
      if (ierr.ne.0.or.cmosave.eq.'-notset-') cmosave='3dmesh'

      ierror=0
      current_map=0

c  modify if command root changed ..............

      lenaction=icharlnf(cmsgin(1))
      action=cmsgin(1)(1:lenaction)
      lenaction=icharlnf(action)
      if (nwds.lt.1.or.msgtyp(1).ne.3.or.lenaction.lt.1) then
         goto 9999
      elseif (action(1:lenaction).eq.'colormap') then
         lenaction=icharlnf(cmsgin(2))
         action=cmsgin(2)(1:lenaction)
         lenaction=icharlnf(action)
         if (action(1:lenaction).eq.'jtet') then
            ierror=-10
            goto 9998
         elseif (action(1:lenaction).eq.'isn'
     &         .or.action(1:lenaction).eq.'isn1'
     &         .or.action(1:lenaction).eq.'pointwise'
     &         .or.action(1:lenaction).eq.'gcolor') then
            nwdsskip=2
         else
            nwdsskip=1
         endif
      elseif (action(1:lenaction).eq.'gcolor') then
         nwdsskip=1
      else
         goto 9999
      endif

      iwd=nwdsskip

c  find mapname ................

      iwd=iwd+1
      if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
         lenaction=icharlnf(cmsgin(iwd))
         if (cmsgin(iwd)(1:lenaction).eq.'stats'
     &        .or.cmsgin(iwd)(1:lenaction).eq.'unique') then
            action=cmsgin(iwd)(1:lenaction)
            goto 55
         else
            if (lenaction.gt.32) lenaction=32
            if (cmsgin(iwd)(1:1).ne.'-')
     &             mapname=cmsgin(iwd)(1:lenaction)
         endif
      endif
      lenaction=icharlnf(mapname)
      len=icharlnf(mapname)
      j=0
      do current_map=1,nmaps
         k=icharlnf(save_mapname(current_map))
         if (mapname(1:len).eq.save_mapname(current_map)(1:k)) then
            goto 44
         elseif (j.eq.0.and.mapname(1:1).eq.'-') then
            j=current_map
         endif
      enddo
      if ((nmaps.lt.20.or.j.ne.0).and.nmaps.ge.0) then
         if (j.ne.0) then
            current_map=j
         else
            nmaps=nmaps+1
            current_map=nmaps
         endif
         save_mapname(current_map)=mapname
         save_gcolor_params(1,current_map)=0
         save_gcolor_params(2,current_map)=0
         save_gcolor_params(3,current_map)=1
         save_gcolor_params(4,current_map)=0
         save_gcolor_params(5,current_map)=1
      else
         current_map=0
         cbuf='COLORMAP/GCOLOR ERROR: too many maps (max=20)'
         call writloga('default',0,cbuf,0,ierr)
         goto 9999
      endif
44    continue
      nmats=save_gcolor_params(1,current_map)
      ncolors=save_gcolor_params(2,current_map)
      distance=save_gcolor_params(3,current_map)
      nclrnbrs=save_gcolor_params(4,current_map)
      valid_new=save_gcolor_params(5,current_map)

c  find action ................

      iwd=iwd+1
      if (nwds.lt.iwd.or.msgtyp(iwd).ne.3) goto 9999
      lenaction=icharlnf(cmsgin(iwd))
      action=cmsgin(iwd)(1:lenaction)
      lenaction=icharlnf(action)
      if (action(1:lenaction).eq.'burn') then
         action='unique'
         lenaction=6
      endif

c ...............................................................
c gcolor[/mapname]/stats[/cmoname/filename]
c
c just select mapname
c ..........................

55    if (action(1:lenaction).eq.'select') then

         ! already selected
         goto 9000

c ...............................................................
c gcolor[/mapname]/stats[/cmoname/filename]
c gcolor[/mapname]/unique[/cmoname/oldimtname]

c unique is the "uncoloring" operation:
c there is no clear way to decide if regions in different cmos/files
c were related prior to recolor, so this "uncolor" is for the current cmo only
c and it just gives a number 1->n, where n is the number of disconnected regions
c since it is cheap to code stats at the same time, added here
c (in which case filename is the name of the file to dump the stats to:
c it is incremented, not overwritten)
c ..........................

      elseif (action(1:lenaction).eq.'stats'
     &    .or.action(1:lenaction).eq.'unique') then
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            cmo=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (cmo(1:1).eq.'-') cmo=cmosave
         else
            cmo=cmosave
         endif
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            oldimtname=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            i=icharlnf(oldimtname)
            if (i.lt.1.or.oldimtname(1:1).eq.'-') oldimtname='-none-'
            if (oldimtname(1:i).eq.'imt'
     &          .or.oldimtname(1:i).eq.'imt1') oldimtname='-none-'
         else
            oldimtname='-none-'
         endif
         ! be careful not to "dotask" earlier as destroys the cmsgin etc
         cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
         call dotask(cbuf,ierr)
         call gcolor_unique_lg(nmats,valid_new,mapname,action
     &                         ,cmo,oldimtname,ierror)
         goto 9000

c ...............................................................
c gcolor[/mapname]/distance/#

c set the distance to avoid similar colors
c ..........................

      elseif (action(1:lenaction).eq.'distance') then
         iwd=iwd+1
         if (nwds.lt.iwd) then
            distance=1
         elseif (msgtyp(iwd).eq.1) then
            distance=imsgin(iwd)
         elseif (msgtyp(iwd).eq.2) then
            distance=xmsgin(iwd)
         else
            distance=1
         endif
         ncolors=0
         goto 9000

c ...............................................................
c gcolor[/mapname][/valid|invalid]
c gcolor[/mapname][/valid|invalid]/new
c gcolor[/mapname][/valid|invalid]/range/firstimt,lastimt
c gcolor[/mapname][/valid|invalid]/list/imt1_1,imt1_2,....

c add or subtract imt1 values from the validity table for recoloring
c ..........................

      elseif (action(1:lenaction).eq.'valid'
     &     .or.action(1:lenaction).eq.'invalid') then
         ncolors=0
         if (action(1:2).eq.'in') then
            ivalid=0
         else
            ivalid=1
         endif
         iwd=iwd+1
         if (nwds.gt.iwd.and.msgtyp(iwd).eq.3) then
            vaction=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
         else
            vaction='all'
         endif
         if (vaction(1:icharlnf(vaction)).eq.'new') then
            valid_new=ivalid
         elseif (nmats.gt.0) then
            call mmfindbk('gclr_valid',mapname,ip_gclr_valid
     &                       ,len,ierr)
            if (ierr.ne.0) goto 9999
            if (vaction(1:len).eq.'list') then
71             iwd=iwd+1
               if (nwds.lt.iwd) then
                  goto 9000
               elseif (msgtyp(iwd).eq.1) then
                  j=imsgin(iwd)
               elseif (msgtyp(iwd).eq.2) then
                  j=int(xmsgin(iwd)+0.1d0)
               else
                  j=0
               endif
               if (j.gt.0.and.j.le.nmats) gclr_valid(j)=ivalid
               goto 71
            elseif (vaction(1:len).eq.'range') then
               iwd=iwd+1
               if (nwds.lt.iwd) then
                  j=1
               elseif (msgtyp(iwd).eq.1) then
                  j=imsgin(iwd)
               elseif (msgtyp(iwd).eq.2) then
                  j=xmsgin(iwd)
               else
                  j=1
               endif
               iwd=iwd+1
               if (nwds.lt.iwd) then
                  k=nmats
               elseif (msgtyp(iwd).eq.1) then
                  k=imsgin(iwd)
               elseif (msgtyp(iwd).eq.2) then
                  k=xmsgin(iwd)
               else
                  k=nmats
               endif
               if (j.lt.1) k=1
               if (k.gt.nmats) k=nmats
               do i=j,k
                  gclr_valid(i)=ivalid
               enddo
            else ! if (vaction(1:len).eq.'all') then
               do i=1,nmats
                  gclr_valid(i)=ivalid
               enddo
            endif
         endif
         goto 9000

c ...............................................................
c gcolor[/mapname]/create
c gcolor[/mapname]/release
c gcolor[/mapname]/map[/dist]
c gcolor[/mapname]/put[/filename|'-def-'|-full-']
c gcolor[/mapname]/get/filename

c commands just relating to the map itself handled in gcolor_map_lg
c ...........................

      elseif (action(1:lenaction).eq.'create'
     &    .or.action(1:lenaction).eq.'release'
     &    .or.action(1:lenaction).eq.'get'
     &    .or.action(1:lenaction).eq.'put'
     &    .or.action(1:lenaction).eq.'map') then
         if (action(1:lenaction).eq.'map') then
            iwd=iwd+1
            if (nwds.ge.iwd.and.msgtyp(iwd).eq.1) then
               distance=imsgin(iwd)
            elseif (nwds.ge.iwd.and.msgtyp(iwd).eq.2) then
               distance=xmsgin(iwd)
            endif
            filename='-def-'
            ncolors=0
         else
            iwd=iwd+1
            if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
               filename=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            else
               filename='-def-'
            endif
         endif
         call gcolor_map_lg(mapname,action,filename,ierror)
         if (action(1:lenaction).eq.'release') 
     &         save_mapname(current_map)='-none-'
         goto 9000

c ...............................................................
c gcolor[/mapname]/add[/cmoname]

c add the connectivity of the indicated cmo to the current map
c ....................

      elseif (action(1:lenaction).eq.'add') then
         ncolors=0
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            cmo=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (cmo(1:1).eq.'-') cmo=cmosave
         else
            cmo=cmosave
         endif
         cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
         call dotask(cbuf,ierr)
         call cmo_exist(cmo,ierror)
         if (ierror.ne.0) goto 9999
         call gcolor_add_lg(mapname,cmo,ierror)
         goto 9000

c ...............................................................
c gcolor[/mapname]/recolor[/cmoname/oldimtname]

c recolor the indicated cmo using the current map
c .....................

      elseif (action(1:lenaction).eq.'recolor') then

         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            cmo=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (cmo(1:1).eq.'-') cmo=cmosave
         else
            cmo=cmosave
         endif
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            oldimtname=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            i=icharlnf(oldimtname)
            if (i.lt.1.or.oldimtname(1:1).eq.'-') oldimtname='-none-'
            if (oldimtname(1:i).eq.'imt'
     &          .or.oldimtname(1:i).eq.'imt1') oldimtname='-none-'
         else
            oldimtname='-none-'
         endif

         dumptype='-not-'
         filename='FFF'
         ! be careful not to "dotask" earlier as destroys the cmsgin etc
         cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
         call dotask(cbuf,ierr)
         call gcolor_dump_lg(mapname,cmo,oldimtname
     &                      ,dumptype,filename,ierror)
         goto 9000

c ...............................................................
c gcolor[/mapname]/dump/dumptype/filename/oldimtname/cmoname

c dump the requested cmo into the indicated file after recoloring
c (restored after dump)
c.....................

      elseif (action(1:lenaction).eq.'dump') then
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            dumptype=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (dumptype(1:1).eq.'-') dumptype='gmv'
         else
            dumptype='gmv'
         endif
         dumplen=icharlnf(dumptype)
         if (dumptype(1:dumplen).eq.'x3d') dumptype='lgt'
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            filename=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            filelen=icharlnf(filename)
            if (filelen.lt.1.or.filename(1:1).eq.'-') goto 9999
         else
            goto 9999
         endif
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            oldimtname=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            i=icharlnf(oldimtname)
            if (i.lt.1.or.oldimtname(1:1).eq.'-') oldimtname='-none-'
            if (oldimtname(1:i).eq.'imt'
     &          .or.oldimtname(1:i).eq.'imt1') oldimtname='-none-'
         else
            oldimtname='-none-'
         endif
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            cmo=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (cmo(1:1).eq.'-') cmo=cmosave
         else
            cmo=cmosave
         endif
         ! be careful not to "dotask" earlier as destroys the cmsgin etc
         cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
         call dotask(cbuf,ierr)
         call cmo_exist(cmo,ierror)
         if (ierror.ne.0) goto 9999
         if (nmats.eq.0) then
            call gcolor_add_lg(mapname,cmo,ierror)
            if (ierror.ne.0) goto 9999
         endif
         if (ncolors.eq.0) then
            call gcolor_map_lg(mapname,'map',filename,ierror)
            if (ierror.ne.0) goto 9999
         endif
         call gcolor_dump_lg(mapname,cmo,oldimtname
     &                      ,dumptype,filename,ierror)
         goto 9000

c ...............................................................
c gcolor[/mapname]/addfiles/cmoname/readtype/filename1 filename2 ....
c gcolor[/mapname]/dumpfiles/dumptype/oldimtname/cmoname/readtype/filename1 filename2 ...
c gcolor[/mapname]/files/dumptype/oldimtname/cmoname/readtype/filename1 filename2 ...
c gcolor[/mapname]/addfilein/cmoname/readtype/filename
c gcolor[/mapname]/dumpfilein/dumptype/oldimtname/cmoname/readtype/fileinname
c gcolor[/mapname]/filein/dumptype/oldimtname/cmoname/readtype/fileinname

c all the iterative versions of the command
c .........................................

      elseif (  action(1:lenaction).eq.'addfiles'
     &      .or.action(1:lenaction).eq.'dumpfiles'
     &      .or.action(1:lenaction).eq.'files'
     &      .or.action(1:lenaction).eq.'addfilein'
     &      .or.action(1:lenaction).eq.'dumpfilein'
     &      .or.action(1:lenaction).eq.'filein') then

c... figure out the rest of the command line up to "readtype"
         if (action(1:3).eq.'add') then
            dumptype='gmv'
         else
            iwd=iwd+1
            if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
               dumptype=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
               if (dumptype(1:1).eq.'-') dumptype='gmv'
            else
               dumptype='gmv'
            endif
            dumplen=icharlnf(dumptype)
            if (dumptype(1:dumplen).eq.'x3d') dumptype='lgt'
            iwd=iwd+1
            if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
               oldimtname=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
               i=icharlnf(oldimtname)
               if (i.lt.1.or.oldimtname(1:1).eq.'-')
     &                      oldimtname='-none-'
               if (oldimtname(1:i).eq.'imt'
     &          .or.oldimtname(1:i).eq.'imt1') oldimtname='-none-'
            else
               oldimtname='-none-'
            endif
         endif
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            cmo=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (cmo(1:1).eq.'-') cmo=cmosave
         else
            cmo=cmosave
         endif
         iwd=iwd+1
         if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
            readtype=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
            if (readtype(1:1).eq.'-') readtype=dumptype
         else
            readtype=dumptype
         endif
         readlen=icharlnf(readtype)
         if (readlen.lt.3) goto 9999
         if (readtype(1:readlen).eq.'x3d') readtype='lgt'

c... store the names of the files to read in "files" array
         if (action(lenaction-1:lenaction).eq.'in') then
c... if the action ends in "in", read the names from the indicated file
            iwd=iwd+1
            if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
               filename=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
               filelen=icharlnf(filename)
               if (filelen.lt.1) goto 9999
               call fexist(filename(1:filelen),iexist)
               if (iexist.ne.1) goto 9999
            else
               goto 9999
            endif
            iunit=nextlun()
            nfiles=0
            open (unit=iunit,file=filename,status='old',err=299)
201         read(iunit,'(a)',end=202,err=202) filename
            filelen=icharlnf(filename)
            call fexist(filename(1:filelen),iexist)
            if (iexist.eq.1) nfiles=nfiles+1
            goto 201
202         if (nfiles.lt.1) goto 299
            ! note: 4 characters equivalent to 1 integer
            !  ->  length_needed=nfiles*(max_characters/file)/4
            len=10*nfiles
            call mmggetbk('files',mapname,ip_files,len,1,ierror)
            if (ierror.ne.0) goto 299
            rewind(iunit)
            nfiles=0
203         read(iunit,'(a)',end=204,err=204) filename
            filelen=icharlnf(filename)
            call fexist(filename(1:filelen),iexist)
            if (iexist.eq.1) then
               nfiles=nfiles+1
               files(nfiles)=filename(1:filelen)
            endif
            goto 203
299         close(iunit)
            goto 9999
204         close(iunit)
         else ! if (action(lenaction-1:lenaction).eq.'es') then
c... otherwise read the files from the cmsgin array
            ! read and keep last in memory (no release)
            ! store cmsgin as destroyed by read dotask's
            nfiles=nwds-iwd
            if (nfiles.lt.1) goto 9999
            ! note: 4 characters equivalent to 1 integer
            !  ->  length_needed=nfiles*(max_characters/file)/4
            nfiles=10*nfiles
            call mmggetbk('files',mapname,ip_files,nfiles,1,ierror)
            if (ierror.ne.0) goto 9999
            nfiles=0
200         iwd=iwd+1
            if (nwds.ge.iwd.and.msgtyp(iwd).eq.3) then
               nfiles=nfiles+1
               files(nfiles)=cmsgin(iwd)(1:icharlnf(cmsgin(iwd)))
               call fexist(files(nfiles)
     &               (1:icharlnf(files(nfiles))),iexist)
               if (iexist.ne.1) nfiles=nfiles-1
            endif
            if (iwd.lt.nwds) goto 200
         endif

c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C         * * * WARNING: msgin's detroyed below here * * *
C
c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c... note this "dotask" detroys the cmsgin array
c... (which is OK as it is not used again)

         cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
         call dotask(cbuf,ierr)

c... create the mesh in case not lgt reads
         cbuf='cmo/create/'//cmo(1:icharlnf(cmo))
     &            //'/; finish'
         call dotask(cbuf,ierr)
         call cmo_exist(cmo,ierror)
         if (ierror.ne.0) goto 9999

c... increment map with files if requested
         if (action(1:3).eq.'add'.or.action(1:3).eq.'fil') then
            ncolors=0
            do ifile=1,nfiles
               cbuf='read/'//readtype(1:readlen)
     &            //'/'//files(ifile)(1:icharlnf(files(ifile)))
     &            //'/'//cmo(1:icharlnf(cmo))
     &            //'/; finish'
               call dotask(cbuf,ierr)
               cbuf='cmo/select/'//cmo(1:icharlnf(cmo))
     &            //'/; finish'
               call dotask(cbuf,ierr)
               call gcolor_add_lg(mapname,cmo,ierror)
            enddo
         endif

c... if just adding, finished
         if (action(1:3).eq.'add') goto 9000

c... form map if not already formed
         if (ncolors.eq.0) then
            call gcolor_map_lg(mapname,'map',filename,ierror)
            if (ierror.ne.0) goto 9999
         endif

c... dump using the colormap
c... if doing read and dump, remember last file already in memory
         if (action(1:3).eq.'fil') then
            filename='GC'//files(nfiles)
            call gcolor_dump_lg(mapname,cmo,oldimtname
     &                      ,dumptype,filename,ierror)
            nfiles=nfiles-1
         endif
         do ifile=nfiles,1,-1
            cbuf='read/'//readtype(1:readlen)
     &            //'/'//files(ifile)(1:icharlnf(files(ifile)))
     &            //'/'//cmo(1:icharlnf(cmo))
     &            //'/; finish'
            call dotask(cbuf,ierr)
            cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
            call dotask(cbuf,ierr)
            filename='GC'//files(ifile)
            call gcolor_dump_lg(mapname,cmo,oldimtname
     &                      ,dumptype,filename,ierror)
         enddo
         goto 9000

c ...............................................................
c anything else: illegal action

      else
         goto 9999
c ...............................................................
      endif

c =================================================================
c normal return

9000  if (ierror.ne.0) goto 9999
      if (current_map.ne.0) then
         save_gcolor_params(1,current_map)=nmats
         save_gcolor_params(2,current_map)=ncolors
         save_gcolor_params(3,current_map)=distance
         save_gcolor_params(4,current_map)=nclrnbrs
         save_gcolor_params(5,current_map)=valid_new
      endif
      cbuf='cmo/select/'//cmosave(1:icharlnf(cmosave))
     &            //'/; finish'
      call dotask(cbuf,ierr)
      if (local_debug.gt.0) call mmverify()
      return

c ...............................................................
c error return

9999  if (ierror.lt.0) ierror=-ierror
9998  if (ierror.eq.0) ierror=1
      if (current_map.ne.0) then
         save_gcolor_params(1,current_map)=nmats
         save_gcolor_params(2,current_map)=ncolors
         save_gcolor_params(3,current_map)=distance
         save_gcolor_params(4,current_map)=nclrnbrs
         save_gcolor_params(5,current_map)=valid_new
      endif
      cbuf='COLORMAP/GCOLOR ERROR: command failed'
      call writloga('default',0,cbuf,0,ierr)
      if (local_debug.gt.0) call mmverify()
      return

c ...............................................................
      end

C ###############################################################

C ###############################################################
C     SUBROUTINE gcolor_map_lg
C
C     PURPOSE -
C      create,release,put,get,or make connectivity gcolor
C
C     INPUT ARGUMENTS -
C
C        mapname - name of map
C        action - action to take:
C                 'create','release','get,'put','map'
C        filename - for put or get
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C -----------------------------------------------------------------
      subroutine gcolor_map_lg(mapname,action,filename,ierror)

      implicit none
      character*(*) mapname,action,filename
      integer ierror

      integer nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params(5,20)
      common /gcolor_com_lg/ nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params
      save /gcolor_com_lg/

      integer icharlnf
      external icharlnf

      pointer (ip_gcolor,gcolor),(ip_gclr1nbr,gclr1nbr)
     &       ,(ip_gclrnbrs,gclrnbrs),(ip_gclrs,gclrs)
     &       ,(ip_gtmp,gtmp),(ip_gclr_valid,gclr_valid)
      integer gcolor(*),gclr1nbr(*),gclrnbrs(*),gclrs(*)
     &       ,gtmp(*),gclr_valid(*)

      integer lenfile,lenmap,lenact,iexist,iunit,i,j
     &       ,ierr,len,inbr,jnbr,k
      character*132 cbuf

c =================================================================
c initial info

      lenfile=icharlnf(filename)
      lenmap=icharlnf(mapname)
      lenact=icharlnf(action)

c .................................................................
c gcolor[/mapname]/map[/dist]

c form map
c ...................

      if (action(1:lenact).eq.'map'
     &      .or.(action(1:lenact).eq.'put'.and.ncolors.eq.0)
     &    ) then

         if (nmats.lt.1) goto 9999
         call mmggetbk('gcolor',mapname,ip_gcolor,nmats,1,ierror)
         if (ierror.ne.0) goto 9999
         if (nclrnbrs.eq.0.or.nmats.eq.1) then
            ! single matl: no need to map
            do i=1,nmats
               gcolor(i)=1
            enddo
            goto 600
         endif

         ! there are neighbors
         call mmfindbk('gclr1nbr',mapname,ip_gclr1nbr,i,ierr)
         if (ierr.ne.0) goto 9999
         call mmfindbk('gclr_valid',mapname,ip_gclr_valid,i,ierr)
         if (ierr.ne.0) goto 9999
         call mmfindbk('gclrnbrs',mapname,ip_gclrnbrs,i,ierr)
         if (ierr.ne.0) goto 9999
         call mmfindbk('gclrs',mapname,ip_gclrs,i,ierr)
         if (ierr.ne.0) goto 9999

         if (distance.gt.2.and.nmats.gt.3) goto 500
         if (distance.gt.1.and.nmats.gt.2) goto 400

c form map at distance=1
         call mmggetbk('gtmp',mapname,ip_gtmp,nmats,1,ierror)
         gcolor(1)=1
         do i=2,nmats
            inbr=gclr1nbr(i)
            if (inbr.gt.0.and.gclr_valid(i).ne.0) then
               do j=1,i-1
                  gtmp(j)=0
                  if (gclr_valid(j).eq.0) gtmp(j)=1
               enddo
               do while (inbr.gt.0.and.gclrs(inbr).lt.i)
                  gtmp(gcolor(gclrs(inbr)))=1
                  inbr=gclrnbrs(inbr)
               enddo
               do j=1,i-1
                  if (gtmp(j).eq.0) then
                     gcolor(i)=j
                     goto 333
                  endif
               enddo
               gcolor(i)=i
333            continue
            elseif (gclr_valid(i).eq.0) then
               gcolor(i)=i
            else
               gcolor(i)=1
            endif
         enddo
         call mmrelblk('gtmp',mapname,ip_gtmp,ierr)

         goto 600

c form map at distance=2
400      continue
         call mmggetbk('gtmp',mapname,ip_gtmp,nmats,1,ierror)
         gcolor(1)=1
         do i=2,nmats
            inbr=gclr1nbr(i)
            if (inbr.gt.0.and.gclr_valid(i).ne.0) then
               do j=1,i-1
                  gtmp(j)=0
                  if (gclr_valid(j).eq.0) gtmp(j)=1
               enddo
               do while (inbr.gt.0)
                  j=gclrs(inbr)
                  if (j.lt.i) gtmp(gcolor(j))=1
                  jnbr=gclr1nbr(j)
                  if (gclr_valid(j).eq.0) jnbr=0
                  do while (jnbr.gt.0.and.gclrs(jnbr).lt.i)
                     gtmp(gcolor(gclrs(jnbr)))=1
                     jnbr=gclrnbrs(jnbr)
                  enddo
                  inbr=gclrnbrs(inbr)
               enddo
               do j=1,i-1
                  if (gtmp(j).eq.0) then
                     gcolor(i)=j
                     goto 433
                  endif
               enddo
               gcolor(i)=i
433            continue
            elseif (gclr_valid(i).eq.0) then
               gcolor(i)=i
            else
               gcolor(i)=1
            endif
         enddo
         call mmrelblk('gtmp',mapname,ip_gtmp,ierr)

         goto 600

c form map at distance>2
500      continue
         call mmggetbk('gtmp',mapname,ip_gtmp,nmats,1,ierror)
         gcolor(1)=1
         do i=2,nmats
            inbr=gclr1nbr(i)
            if (inbr.gt.0.and.gclr_valid(i).eq.0) then
               do j=1,nmats
                  gtmp(j)=0
                  if (gclr_valid(j).eq.0) gclr_valid(j)=-1
               enddo
               do while (inbr.gt.0)
                  j=gclrs(inbr)
                  if (gtmp(j).eq.0) gtmp(j)=1
                  inbr=gclrnbrs(inbr)
               enddo
               do k=2,distance
                  do j=1,nmats
                     if (gtmp(j).eq.k-1) then
                        jnbr=gclr1nbr(j)
                        do while (jnbr.gt.0)
                           inbr=gclrs(jnbr)
                           if (gtmp(inbr).eq.0) gtmp(inbr)=k
                           jnbr=gclrnbrs(jnbr)
                        enddo
                     endif
                  enddo
               enddo
               do j=1,i-1
                  if (gtmp(j).gt.0) then
                     gtmp(j)=0
                     gtmp(gcolor(j))=1
                  endif
               enddo
               do j=1,i-1
                  if (gtmp(j).eq.0) then
                     gcolor(i)=j
                     goto 533
                  endif
               enddo
               gcolor(i)=i
533            continue
            elseif (gclr_valid(i).eq.0) then
               gcolor(i)=i
            else
               gcolor(i)=1
            endif
         enddo
         call mmrelblk('gtmp',mapname,ip_gtmp,ierr)

         goto 600

600      ncolors=0
         do i=1,nmats
            if (gcolor(i).gt.ncolors) ncolors=gcolor(i)
         enddo
         if (ncolors.eq.0) goto 9999
         if (action(1:lenact).eq.'map') goto 9000

      endif

c .................................................................
c gcolor[/mapname]/create

c "create" map storage (actually just initialize params)
c ...................

      if (action.eq.'create') then
         ncolors=0
         nmats=0
         distance=1
         nclrnbrs=0
         valid_new=1
         !no: keep storage! call mmrelprt(mapname,ierror)

c .................................................................
c gcolor[/mapname]/release

c release map storage
c ...................

      elseif (action.eq.'release') then
         ncolors=0
         nmats=0
         distance=1
         nclrnbrs=0
         valid_new=1
         call mmrelprt(mapname,ierror)

c .................................................................
c gcolor[/mapname]/get/filename

c get map from file
c Q: add "read from screen"?
c ...................

      elseif (action.eq.'get') then
         nmats=0
         distance=1
         ncolors=0
         nclrnbrs=0
         valid_new=1
         if (lenmap.lt.1) goto 9999
         call fexist(filename(1:lenfile),iexist)
         if (iexist.ne.1) goto 9999
         iunit=-1
         call hassign(iunit,filename(1:lenfile),ierror)
         if (ierror.ne.0) goto 199
10       read(iunit,*,err=10,end=199) nmats,ncolors
     &             ,distance,nclrnbrs,valid_new
         if (nmats.gt.0.and.ncolors.gt.0) then
           call mmggetbk('gcolor',mapname,ip_gcolor,nmats,1,ierr)
15         read(iunit,*,err=15,end=199) (gcolor(i),i=1,nmats)
         endif
         if (nmats.gt.0) then
            len=max(nmats,100)
            call mmggetbk('gclr1nbr',mapname,ip_gclr1nbr,len,1,ierr)
            if (ierr.ne.0) goto 199
            call mmggetbk('gclr_valid',mapname,ip_gclr_valid
     &                    ,len,1,ierr)
            if (ierr.ne.0) goto 199
            len=max(nclrnbrs,100)
            call mmggetbk('gclrnbrs',mapname,ip_gclrnbrs,len,1,ierr)
            if (ierr.ne.0) goto 199
            call mmggetbk('gclrs',mapname,ip_gclrs,len,1,ierr)
            if (ierr.ne.0) goto 199
c$$ 20      ! read(iunit,*,err=20,end=199) (gclr1nbr(i),i=1,nmats)
c$$ 30      ! read(iunit,*,err=30,end=199) (gclrnbrs(i),i=1,nclrnbrs)
c$$ 40      ! read(iunit,*,err=40,end=199) (gclrs(i),i=1,nclrnbrs)
c$$ 50      ! read(iunit,*,err=50,end=199) (gclr_valid(i),i=1,nmats)
            do i=1,nmats
               gclr1nbr(i)=0
            enddo
            inbr=0
20          read(iunit,*,err=20,end=40) i,k
            if (i.gt.nmats.or.i.lt.1) goto 199
            if (inbr+k.gt.len) then
               len=len+100+k
               call mmggetbk('gclrnbrs',mapname,ip_gclrnbrs,len,1,ierr)
               if (ierr.ne.0) goto 199
               call mmggetbk('gclrs',mapname,ip_gclrs,len,1,ierr)
               if (ierr.ne.0) goto 199
            endif
            if (k.gt.0) then
30             read(iunit,*,err=30,end=40) (gclrs(inbr+j),j=1,k)
               gclr1nbr(i)=inbr+1
               do j=1,k
                 inbr=inbr+1
                 gclrnbrs(inbr)=inbr+1
               enddo
               gclrnbrs(inbr)=0
            endif
            goto 20
40          continue
50          read(iunit,*,err=50,end=51) (gclr_valid(i),i=1,nmats)
            goto 52
51          do i=1,nmats
               gclr_valid(i)=1
            enddo
52          if (nclrnbrs.ne.inbr) then
               if (nclrnbrs.gt.0) then
                  write(cbuf,*) 'GCOLOR MAP WARNING: nclrnbrs.ne.inbr'
     &                          ,nclrnbrs,inbr
                  call writloga('default',0,cbuf,0,ierr)
                  nclrnbrs=inbr
               endif
               nclrnbrs=inbr
            endif
         elseif (nmats.gt.0) then
            call mmggetbk('gclr1nbr',mapname,ip_gclr1nbr,nmats,1,ierr)
            if (ierr.ne.0) goto 199
            call mmggetbk('gclr_valid',mapname,ip_gclr_valid
     &                    ,nmats,1,ierr)
            if (ierr.ne.0) goto 199
            do i=1,nmats
               gclr1nbr(i)=0
               gclr_valid(i)=1
            enddo
            len=100
            call mmggetbk('gclrnbrs',mapname,ip_gclrnbrs,len,1,ierr)
            call mmggetbk('gclrs',mapname,ip_gclrs,len,1,ierr)
         endif
         goto 45
199      close(iunit)
         nmats=0
         ncolors=0
         nclrnbrs=0
         distance=1
         valid_new=1
         goto 9999
45       close(iunit)

c .................................................................
c gcolor[/mapname]/put/
c gcolor[/mapname]/put/filename
c gcolor[/mapname]/put/-full-

c put map to screen or file
c ...................

      elseif (action.eq.'put'.and.filename(1:1).eq.'-') then
c        ! gcolor/put/
c        ! gcolor/put/-full-
         write(cbuf,*) 'nmats,distance,ncolors'
     &                 ,nmats,distance,ncolors
         call writloga('default',0,cbuf,0,ierr)
         if (nmats.gt.0) then
            call mmfindbk('gcolor',mapname,ip_gcolor,i,ierr)
            if (ierr.ne.0) ncolors=0
            if (ncolors.gt.0) then
               write(cbuf,*) 'gcolor'
               call writloga('default',0,cbuf,0,ierr)
               do i=1,nmats,10
                  write(cbuf,'(10i7)') (gcolor(i+j),j=0,min(9,nmats-i))
                  call writloga('default',0,cbuf,0,ierr)
               enddo
               if (filename(1:6).eq.'-full-') then
                  write(cbuf,*) 'color table'
                  call writloga('default',0,cbuf,0,ierr)
                  call mmggetbk('gtmp',mapname,ip_gtmp,nmats,1,ierr)
                  do i=1,nmats
                     inbr=gclr1nbr(i)
                     k=0
                     do while (inbr.gt.0)
                        k=k+1
                        gtmp(k)=gclrs(inbr)
                        inbr=gclrnbrs(inbr)
                     enddo
                     write(cbuf,*) i,k,' = imt,#nbr ; nbr-imt ='
                     call writloga('default',0,cbuf,0,ierr)
                     do jnbr=1,k,10
                        write(cbuf,'(10i7)')
     &                     (gtmp(jnbr+j),j=0,min(9,k-jnbr))
                        call writloga('default',0,cbuf,0,ierr)
                     enddo
                  enddo
                  call mmrelblk('gtmp',mapname,ip_gtmp,ierr)
                  call mmfindbk('gclr_valid',mapname,ip_gclr_valid
     &                          ,i,ierr)
                  if (ierr.eq.0) then
                     do i=1,nmats,10
                        write(cbuf,'(10i7)')
     &                     (gclr_valid(i+j),j=0,min(9,nmats-i))
                        call writloga('default',0,cbuf,0,ierr)
                     enddo
                  endif
               endif
            else
               write(cbuf,*) 'no gcolor'
               call writloga('default',0,cbuf,0,ierr)
            endif
         endif
      elseif (action.eq.'put') then
         call mmfindbk('gcolor',mapname,ip_gcolor,i,ierr)
         if (ierr.ne.0) ncolors=0
         call mmfindbk('gclr1nbr',mapname,ip_gclr1nbr,i,ierr)
         if (ierr.ne.0) ncolors=-abs(ncolors)
         call mmfindbk('gclrnbrs',mapname,ip_gclrnbrs,i,ierr)
         if (ierr.ne.0) nclrnbrs=0
         call mmfindbk('gclrs',mapname,ip_gclrs,i,ierr)
         if (ierr.ne.0) nclrnbrs=0
c        ! gcolor/put/filename
         iunit=-1
         call hassign(iunit,filename(1:lenfile),ierr)
         if (ierr.eq.0) then
            write(iunit,*) 'nmats,ncolors,distance,nclrnbrs,valid_new'
            write(iunit,*) nmats,ncolors,distance,nclrnbrs,valid_new
            if (nmats.gt.0) then
               if (ncolors.gt.0) then
                  write(iunit,*) 'gcolor'
                  write(iunit,*) (gcolor(i),i=1,nmats)
               endif
               if (nclrnbrs.gt.0) then
c$$               ! write(iunit,*) 'gclr1nbr'
c$$               ! write(iunit,*) (gclr1nbr(i),i=1,nmats)
c$$               ! write(iunit,*) 'gclrnbrs'
c$$               ! write(iunit,*) (gclrnbrs(i),i=1,nclrnbrs)
c$$               ! write(iunit,*) 'gclrs'
c$$               ! write(iunit,*) (gclrs(i),i=1,nclrnbrs)
                  write(iunit,*)
     &               'color table (clr,#nbr,nbrclrs,clr=1,nmats)'
                  call mmggetbk('gtmp',mapname,ip_gtmp,nmats,1,ierr)
                  do i=1,nmats
                     inbr=gclr1nbr(i)
                     k=0
                     do while (inbr.gt.0)
                        k=k+1
                        gtmp(k)=gclrs(inbr)
                        inbr=gclrnbrs(inbr)
                     enddo
                     if (k.gt.0) then
                        write(iunit,*) i,k
                        write(iunit,*) (gtmp(j),j=1,k)
                     endif
                  enddo
                  call mmrelblk('gtmp',mapname,ip_gtmp,ierr)
                  call mmfindbk('gclr_valid',mapname,ip_gclr_valid
     &                          ,i,ierr)
                  if (ierr.ne.0) write(iunit,*)
     &                          (gclr_valid(i),i=1,nmats)
               endif
            endif
         endif
         close(iunit)

c .................................................................
c anything else: illegal action

      else
         goto 9999
      endif

c ===============================================================
c normal return

9000  if (ierror.ne.0) goto 9999
      return

c ...............................................................
c error return

9999  if (ierror.eq.0) ierror=1
      cbuf='GCOLOR MAP ERROR'
      call writloga('default',0,cbuf,0,ierr)
      return

c ...............................................................
      end

C ###############################################################

C ###############################################################
C     SUBROUTINE gcolor_add_lg
C
C     PURPOSE -
C      add connectivity info in a cmo to mapname
C
C     INPUT ARGUMENTS -
C
C        mapname - name of map
C        cmo - cmo to use
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C -----------------------------------------------------------------

      subroutine gcolor_add_lg(mapname,cmo,ierror)

      implicit none
      include "local_element.h"
      include "chydro.h"
      character*(*) mapname,cmo
      integer ierror

      integer nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params(5,20)
      common /gcolor_com_lg/ nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params
      save /gcolor_com_lg/

      pointer (ip_gclr1nbr,gclr1nbr),(ip_gclrnbrs,gclrnbrs)
     &       ,(ip_gclrs,gclrs),(ip_gclr_valid,gclr_valid)
      integer gclr1nbr(*),gclrnbrs(*),gclrs(*),gclr_valid(*)
      pointer (ip_imt1,imt1),(ip_itp1,itp1),(ip_isn1,isn1)
      integer imt1(*),itp1(*),isn1(*)

      integer ierr,len,typ,nmatsnew,nmatsnew0,nnodes,i,ip
     &       ,j,im,im0,lenclrnbrs,lic,ic,first,nelements
     &       ,nmatsold
      character*132 cbuf

c -----------------------------------------------------------------
      ncolors=0
      call cmo_exist(cmo,ierror)
      if (ierror.ne.0) goto 9999

      first=0
10    call cmo_get_info('nnodes',cmo,nnodes,len,typ,ierr)
      if (ierr.ne.0.or.nnodes.lt.1) goto 9999
      call cmo_get_info('imt1',cmo,ip_imt1,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('isn1',cmo,ip_isn1,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('itp1',cmo,ip_itp1,len,typ,ierr)
      if (ierr.ne.0) goto 9999

c check to see what the max number of matls in current cmo is
c also check to make sure children are there.
c if not and there are elements, then color the points, otherwise abort

      nmatsnew=0
      nmatsnew0=1
      do i=1,nnodes
        ip=itp1(i)
        if (ip.ne.ifitpdud.and.ip.ne.ifitpmrg
     &                    .and.ip.ne.ifitpcup) then
           if (imt1(i).gt.nmatsnew) nmatsnew=imt1(i)
           if (imt1(i).lt.nmatsnew0) nmatsnew0=imt1(i)
           if (isn1(i).eq.0.and.ip.ne.ifitpint
     &      .and.ip.ne.ifitprfl.and.ip.ne.ifitpfre.and.ip.ne.ifitprfb
     &      .and.ip.ne.ifitpvrt.and.ip.ne.ifitpvrb
     &      .and.ip.ne.ifitpvfb.and.ip.ne.ifitpvrf) then
              if (first.ne.0) goto 9999
              call cmo_get_info('nelements',cmo,nelements,len,typ,ierr)
              if (ierr.ne.0.or.nelements.lt.1) goto 9999
c add: COMPRESSWHEREPOSSIBLE
c and this vs building local isn...
              cbuf='resetpts parents; settets color_points'
     &                        //'; rmpoint compress; finish'
              call dotask(cbuf,ierr)
              first=1
              goto 10
           endif
        endif
      enddo
      if (nmatsnew0.lt.1) goto 9999

c increase memory if necessary
c print warning if nmats>nnodes and nmats large
c   should really re-write in this case, or nmatsmin<0 case, so that
c   use table differently (similar to icontab...)
c presumption is that this is not the case and press on ...

      if (nmatsnew.gt.nnodes.and.nmatsnew.gt.100000) then
         write(cbuf,*) 'GCOLOR ADD WARNING: storage inefficient'
     &       //' as nnodes=',nnodes,'<nmats=',nmats
         call writloga('default',0,cbuf,0,ierr)
      endif

      if (nmats.eq.0) nclrnbrs=0
      nmatsold=nmats
      if (nmatsnew.gt.nmats) nmats=nmatsnew
      len=max(nmats,100)
      call mmggetbk('gclr1nbr',mapname,ip_gclr1nbr,len,1,ierr)
      call mmggetbk('gclr_valid',mapname,ip_gclr_valid,len,1,ierr)
      len=max(10*nmats+100,nclrnbrs)
      call mmggetbk('gclrnbrs',mapname,ip_gclrnbrs,len,1,ierr)
      call mmggetbk('gclrs',mapname,ip_gclrs,len,1,ierr)
      call mmblklen('gclrnbrs',mapname,ip_gclrnbrs,lenclrnbrs,ierr)
      do i=nmatsold+1,nmats
         gclr1nbr(i)=0
         gclr_valid(i)=valid_new
      enddo

c add new color info to linked lists
      do i=1,nnodes
        ip=itp1(i)
        if (ip.ne.ifitpcup) goto 100
        ip=isn1(i)
        do while (ip.ne.i)
           j=isn1(ip)
           im0=imt1(ip)
           if (im0.lt.1.or.im0.gt.nmats) goto 9999
           do while (j.ne.ip)
              im=imt1(j)
              if (j.ne.i.and.im.ne.im0.and.im.gt.0
     &                              .and.im.le.nmats) then
                 ! add im to color neighbors of im0
                 ic=gclr1nbr(im0)
                 if (ic.eq.0) then
                    gclr1nbr(im0)=nclrnbrs+1
                    gclr_valid(im0)=valid_new
                 else
                    lic=0
                    do while (ic.gt.0)
                       if (gclrs(ic).gt.im) then
                          if (lic.gt.0) then
                             gclrnbrs(lic)=nclrnbrs+1
                          else
                             gclr1nbr(im0)=nclrnbrs+1
                             gclr_valid(im0)=valid_new
                          endif
                          goto 200
                       elseif (gclrs(ic).eq.im) then
                          goto 210
                       endif
                       lic=ic
                       ic=gclrnbrs(ic)
                    enddo
                    gclrnbrs(lic)=nclrnbrs+1
                 endif
200              nclrnbrs=nclrnbrs+1
                 if (nclrnbrs.gt.lenclrnbrs) then
                    lenclrnbrs=nclrnbrs+1000
                    call mmggetbk('gclrnbrs',mapname,ip_gclrnbrs
     &                       ,lenclrnbrs,1,ierr)
                    call mmggetbk('gclrs',mapname,ip_gclrs
     &                       ,lenclrnbrs,1,ierr)
                 endif
                 gclrnbrs(nclrnbrs)=ic
                 gclrs(nclrnbrs)=im
210              continue
              elseif ((im.lt.0.or.im.gt.nmats).and.j.ne.i) then
                 continue ! goto 9999
              endif
              j=isn1(j)
           enddo
           ip=isn1(ip)
        enddo
100     continue
      enddo

c ===============================================================
c normal return

9000  if (ierror.ne.0) goto 9999
      return

c ...............................................................
c error return

9999  if (ierror.eq.0) ierror=1
      cbuf='GCOLOR ADD ERROR'
      call writloga('default',0,cbuf,0,ierr)
      return

c ...............................................................
      end

C ###############################################################

C ###############################################################
C     SUBROUTINE gcolor_dump_lg
C
C     PURPOSE -
C      color a cmo with mapname, and dump (unless dumptype='-not-'
C      note that vor2d set to "no" for dump
C         (so no vor and med in gmv file,
C          re their adding unneeded extra colors)
C
C     INPUT ARGUMENTS -
C
C        mapname - name of map
C        cmo - cmo to recolor and dump
C        oldimtname - name to assign to attribute for pre-recolored imt
C        dumptype - type of dump
C        filename - file to dump into
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C -----------------------------------------------------------------

      subroutine gcolor_dump_lg(mapname,cmo,oldimtname
     &                      ,dumptype,filename,ierror)

      implicit none
      character*(*) cmo,mapname,oldimtname,dumptype,filename
      integer ierror

      integer nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params(5,20)
      common /gcolor_com_lg/ nmats,ncolors,distance,nclrnbrs,valid_new
     &         ,current_map,nmaps,save_gcolor_params
      save /gcolor_com_lg/

      pointer (ip_gcolor,gcolor)
      integer gcolor(*)
      pointer (ip_imt1,imt1),(ip_itetclr,itetclr)
     &       ,(ip_oldimt,oldimt),(ip_oldtetc,oldtetc)
      integer imt1(*),itetclr(*),oldimt(*),oldtetc(*)

      integer icharlnf
      external icharlnf

      integer ierr,nnodes,nelements,i,j,k,index
     &       ,lold,ldump,lfile,lcmo,len,typ
      character*32 dumpopt,ctype,clength,crank,cinter,cpers,cio
      character*132 cbuf

      integer local_clr(128),n_local,ivoronoi2d
      real*8 xxx

c -----------------------------------------------------------------

      lcmo=icharlnf(cmo)
      if (lcmo.lt.1) goto 9999
      call cmo_exist(cmo(1:lcmo),ierror)
      if (ierror.ne.0) goto 9999
      cbuf='cmo/select/'//cmo(1:lcmo)//'/; finish'
      call dotask(cbuf,ierr)

c create if not created
      if (nmats.eq.0) then
         call gcolor_add_lg(mapname,cmo,ierror)
         if (ierror.ne.0) goto 9999
      endif
c color if not already colored
      if (ncolors.eq.0) then
         call gcolor_map_lg(mapname,'map',dumpopt,ierror)
         if (ierror.ne.0) goto 9999
      endif

      lold=icharlnf(oldimtname)
      if (lold.lt.1.or.oldimtname(1:lold).eq.'-none-'
     &   .or.oldimtname(1:lold).eq.'imt1'
     &   .or.oldimtname(1:lold).eq.'imt1') lold=0

      call mmfindbk('gcolor',mapname,ip_gcolor,len,ierr)
      if (ierr.ne.0) goto 9999
      if (lold.eq.0) then
         call cmo_get_info('nnodes',cmo,nnodes,len,typ,ierr)
         call mmggetbk('oldimt',mapname,ip_oldimt,nnodes,1,ierr)
      else
         call cmo_get_info(oldimtname(1:lold),cmo
     &          ,ip_oldimt,len,typ,ierr)
         if (ierr.ne.0) then
            write(cbuf,*)'cmo addatt '
     &          //cmo(1:lcmo)//'/'
     &          //oldimtname(1:lold)
     &          //' VINT/scalar/nnodes; finish'
            call dotask(cbuf,ierr)
            call cmo_get_info(oldimtname(1:lold),cmo
     &          ,ip_oldimt,len,typ,ierr)
            if (ierr.ne.0) goto 9999
         else
            call cmo_get_attparam(oldimtname(1:lold),cmo
     &             ,index,ctype,crank,clength
     &             ,cinter,cpers,cio,ierr)
            if (ierr.ne.0) goto 9999
            if (ctype.ne.'VINT'.or.clength.ne.'nnodes') goto 9999
         endif
      endif

c store matl info and recolor
      call cmo_get_info('nnodes',cmo,nnodes,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('imt1',cmo,ip_imt1,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('nelements',cmo,nelements,len,typ,ierr)
      if (ierr.eq.0.and.nelements.gt.0) then
         call mmggetbk('oldtetc',mapname,ip_oldtetc,nelements,1,ierr)
         if (ierr.ne.0) nelements=0
         call cmo_get_info('itetclr',cmo,ip_itetclr,len,typ,ierr)
         if (ierr.ne.0) nelements=0
      else
         nelements=0
      endif
      if (ierr.ne.0) goto 9999
      n_local=0
      do i=1,nelements
         j=itetclr(i)
         oldtetc(i)=j
         if (j.gt.0.and.j.le.nmats) then
            itetclr(i)=gcolor(j)
         elseif (j.le.0) then
            itetclr(i)=0
         else ! if (j.gt.nmats) then
            do k=1,n_local
               if (local_clr(k).eq.j) then
                  itetclr(i)=ncolors+k
                  goto 230
               endif
            enddo
            if (ncolors+n_local+1.ge.128) then
               itetclr(i)=ncolors+n_local+1
               !debug! write(*,*) j,ncolors,n_local,ncolors+n_local+1
            else
               n_local=n_local+1
               local_clr(n_local)=ncolors+n_local
               itetclr(i)=ncolors+n_local
               !debug! write(*,*) j,ncolors,n_local,ncolors+n_local
            endif
230         continue
         endif
         !debug! write(*,*) 'itetclr',i,itetclr(i),oldtetc(i)
      enddo
      do i=1,nnodes
         j=imt1(i)
         oldimt(i)=j
         if (j.gt.0.and.j.le.nmats) then
            imt1(i)=gcolor(j)
         elseif (j.le.0) then
            imt1(i)=0
         else ! if (j.gt.nmats) then
            do k=1,n_local
               if (local_clr(k).eq.j) then
                  imt1(i)=ncolors+k
                  goto 231
               endif
            enddo
            imt1(i)=ncolors+n_local+1
231         continue
         endif
         !debug! write(*,*) 'imt',i,imt1(i),oldimt(i)
      enddo

      ldump=icharlnf(dumptype)
      if (dumptype(1:ldump).eq.'-not-') goto 9000

c get current value of vor2d
      call cmo_get_attinfo('vor2d',cmo,k,xxx,ctype,i,len,typ,ierr)
      ivoronoi2d=1
      if (ctype.eq.'no') ivoronoi2d=0

c set vor2d to false
      if (ivoronoi2d.ne.0) then
         cbuf='cmo/setatt/'//cmo(1:lcmo)//'/vor2d////no; finish'
         call dotask(cbuf,ierr)
      endif

c dump
      lfile=icharlnf(filename)
      lcmo=icharlnf(cmo)
      if (ldump.gt.5.and.dumptype(ldump-4:ldump).eq.'ascii') then
         ldump=ldump-5
         dumpopt='ascii'
         cbuf='dump/'//dumptype(1:ldump)
     &         //'/'//filename(1:lfile)
     &         //'/'//cmo(1:lcmo)
     &         //'/ascii; finish'
      else
         cbuf='dump/'//dumptype(1:ldump)
     &         //'/'//filename(1:lfile)
     &         //'/'//cmo(1:lcmo)
     &         //'; finish'
      endif
      call dotask(cbuf,ierr)

c restore vor2d
      if (ivoronoi2d.ne.0) then
         cbuf='cmo/setatt/'//cmo(1:lcmo)//'/vor2d////yes; finish'
         call dotask(cbuf,ierr)
      endif

c restore old matl info
      do i=1,nnodes
         j=imt1(i)
         imt1(i)=oldimt(i)
         oldimt(i)=j
      enddo
      do i=1,nelements
         itetclr(i)=oldtetc(i)
      enddo

c delete temporary storage
      call mmrelblk('oldtetc',mapname,ip_oldtetc,ierr)
      if (lold.eq.0) then
         call mmrelblk('oldimt',mapname,ip_oldimt,ierr)
      else
c$$      if (???) then
c$$         ! leave info as to new color in attribute oldimt
c$$         continue
c$$      else
            ! delete oldimt attribute
            write(cbuf,*)'cmo delatt '//cmo(1:lcmo)//'/'
     &          //oldimtname(1:lold)//'; finish'
            call dotask(cbuf,ierr)
c$$      endif
      endif

c ===============================================================
c normal return

9000  if (ierror.ne.0) goto 9999
      return

c ...............................................................
c error return

9999  if (ierror.eq.0) ierror=1
      cbuf='GCOLOR COLOR ERROR'
      call writloga('default',0,cbuf,0,ierr)
      return

c ...............................................................
      end

C ###############################################################

C ###############################################################
C     SUBROUTINE gcolor_unique_lg
C
C     PURPOSE -
C      recolor cmo so that connected material regions
C      have a unique, sequential number (for action=unique)
C      or just do statistics and dump in oldimtname (for action=stats)
C
C     INPUT ARGUMENTS -
C
C        mapname - name of partition where validity info stored
C        action - action to take [stats|unique]
C        cmo - cmo to recolor
C        oldimtname - name to assign to attribute for pre-uinique imt1
C                     or filename if action=stats
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C -----------------------------------------------------------------

      subroutine gcolor_unique_lg(nmatsin,valid_new,mapname
     &                           ,action,cmo,oldimtname,ierror)

      implicit none
      include 'local_element.h'
      include 'chydro.h'

      character*(*) cmo,action,oldimtname,mapname
      integer ierror

      pointer (ip_imt1,imt1),(ip_itetclr,itetclr)
     &       ,(ip_itet,itet),(ip_itetoff,itetoff)
     &       ,(ip_jtet,jtet),(ip_jtetoff,jtetoff)
     &       ,(ip_itettyp,itettyp),(ip_itp1,itp1)
      integer imt1(*),itetclr(*),itet(*),itetoff(*)
     &       ,jtet(*),jtetoff(*),itettyp(*),itp1(*)

      pointer (ip_oldimt,oldimt),(ip_newtetc,newtetc)
     &       ,(ip_ellist,ellist),(ip_gclr_valid,gclr_valid)
      integer oldimt(*),newtetc(*),ellist(*),gclr_valid(*)

      pointer (ip_xic,xic),(ip_yic,yic),(ip_zic,zic)
      real*8 xic(*),yic(*),zic(*)

      pointer (ip_xlist,xlist),(ip_ylist,ylist),(ip_zlist,zlist)
     &       ,(ip_vols,vols)
      real*8 xlist(*),ylist(*),zlist(*),vols(*)

      real*8 vvv,v_av,v_mean

      integer icharlnf,nextlun
      external icharlnf,nextlun

      integer ierr,nnodes,nelements,i,j,iunit,mbndry,valid_new
     &       ,lold,len,typ,lenact,iel,nels,jel,kel,index
     &       ,imin,imax,ncolors,nef_cmo,nen_cmo,nmats,nmatsin

      character*32 ctype,clength,crank,cinter,cpers,cio
      character*132 cbuf

c -----------------------------------------------------------------
c gcolor[/mapname]/unique[/cmoname/oldimtname]
c    - assign a unique, ordered number to each connected material region
c      if oldimtname=-none-|imt|imt1 or is not present, the old color will
c      be placed in imt1, otherwise it will be added as an attribute
c      with the name oldimtname
c gcolor[/mapname]/stats[/cmoname/filename]
c    - give statistics on the connected regions

c unique is the "uncoloring" operation:
c there is no clear way to decide if regions in different cmos/files
c were related prior to recolor, so this "uncolor" is for the current cmo only
c and it just gives a number 1->n, where n is the number of disconnected regions
c since it is cheap to code stats at the same time, added here
c (in which case filename is the name of the file to dump the stats to:
c it is incremented, not overwritten)
c -----------------------------------------------------------------
      mapname='gcolor_lg'
      call cmo_exist(cmo,ierror)
      if (ierror.ne.0) goto 9999
      cbuf='cmo/select/'//cmo(1:icharlnf(cmo))//'/; finish'
      call dotask(cbuf,ierr)

c nmats materials
      call cmo_get_info('nelements',cmo,nelements,len,typ,ierr)
      if (ierr.ne.0.or.nelements.lt.1) goto 9999
      call cmo_get_info('itetclr',cmo,ip_itetclr,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      nmats=1
      do iel=1,nelements
         if (itetclr(iel).gt.nmats) nmats=itetclr(iel)
      enddo

c find validity table
      call mmfindbk('gclr_valid',mapname,ip_gclr_valid,len,ierr)
      if (ierr.ne.0) then
         call mmggetbk('gclr_valid',mapname,ip_gclr_valid,nmats,1,ierr)
         do i=1,nmats
            gclr_valid(i)=valid_new
         enddo
      elseif (len.lt.nmats) then
         do i=len+1,nmats
            gclr_valid(i)=valid_new
         enddo
      endif
      do i=nmatsin+1,nmats
         gclr_valid(i)=valid_new
      enddo

c find which action to take ('stats' vs 'unique')
      lenact=icharlnf(action)
      if (action(1:lenact).eq.'stats') then
         goto 2000
      else
         goto 1000
      endif

c recolor uniquely
1000  continue

c save old info if desired
      lold=icharlnf(oldimtname)
      if (lold.lt.1.or.oldimtname(1:lold).eq.'-none-'
     &   .or.oldimtname(1:lold).eq.'imt1'
     &   .or.oldimtname(1:lold).eq.'imt1') lold=0
      if (lold.ne.0) then
         call cmo_get_info(oldimtname(1:lold),cmo
     &          ,ip_oldimt,len,typ,ierr)
         if (ierr.ne.0) then
            write(cbuf,*)'cmo addatt '
     &          //cmo(1:icharlnf(cmo))//'/'
     &          //oldimtname(1:lold)
     &          //' VINT/scalar/nnodes; finish'
            call dotask(cbuf,ierr)
            call cmo_get_info(oldimtname(1:lold),cmo
     &          ,ip_oldimt,len,typ,ierr)
            if (ierr.ne.0) goto 9999
         else
            call cmo_get_attparam(oldimtname(1:lold),cmo
     &             ,index,ctype,crank,clength
     &             ,cinter,cpers,cio,ierr)
            if (ierr.ne.0) goto 9999
            if (ctype.ne.'VINT'.or.clength.ne.'nnodes') goto 9999
         endif
         call cmo_get_info('nnodes',cmo,nnodes,len,typ,ierr)
         if (ierr.ne.0.or.nnodes.lt.1) goto 9999
         call cmo_get_info('imt1',cmo,ip_imt1,len,typ,ierr)
         if (ierr.ne.0) goto 9999
         do i=1,nnodes
            oldimt(i)=imt1(i)
         enddo
      endif

      call cmo_get_info('imt1',cmo,ip_imt1,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('nelements',cmo,nelements,len,typ,ierr)
      if (ierr.ne.0.or.nelements.lt.1) goto 9999
      call cmo_get_info('itet',cmo,ip_itet,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('itetoff',cmo,ip_itetoff,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('jtet',cmo,ip_jtet,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('itetclr',cmo,ip_itetclr,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('itettyp',cmo,ip_itettyp,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('mbndry',cmo,mbndry,len,typ,ierr)
      if (ierr.ne.0) mbndry=0
      call cmo_get_info('faces_per_element',cmo,nef_cmo,len,typ,ierr)
      if (ierr.ne.0.or.nef_cmo.lt.1) goto 9999
      call cmo_get_info('nodes_per_element',cmo,nen_cmo,len,typ,ierr)
      if (ierr.ne.0.or.nen_cmo.lt.1) goto 9999
      call mmggetbk('ellist',mapname,ip_ellist,nelements,1,ierr)
      if (ierr.ne.0) goto 9999

c recolor elements,nodes with unique,ordered new color

      ncolors=0
      do iel=1,nelements
         j=itetclr(iel)
         if (j.lt.1.or.j.gt.nmats) then
             itetclr(iel)=0
         elseif (gclr_valid(j).ne.0) then
             itetclr(iel)=0
         endif
      enddo
      do iel=1,nelements
         if (itetclr(iel).eq.0) then
            ncolors=ncolors+1
            if (ncolors.le.nmats.and.gclr_valid(ncolors).eq.0)
     &            ncolors=ncolors+1
            itetclr(iel)=ncolors
            nels=1
            ellist(1)=iel
1100        kel=ellist(1)
            do j=1,nelmnen(itettyp(kel))
               imt1(itet(j+itetoff(iel)))=ncolors
            enddo
            do j=1,nelmnef(itettyp(kel))
              jel=jtet(jtetoff(kel)+j)
              if ( (mbndry.gt.0.and.jel.lt.mbndry)
     &             .or.(mbndry.eq.0.and.jel.gt.0) ) then
                 jel=1+(jel-1)/nef_cmo
                 if (itetclr(jel).eq.0) then
                    itetclr(jel)=ncolors
                    nels=nels+1
                    ellist(nels)=jel
                 endif
              endif
            enddo
            if (nels.gt.1) then
               ellist(1)=ellist(nels)
               nels=nels-1
               goto 1100
            endif
         endif
      enddo

c fix parent color
      call cmo_get_info('nnodes',cmo,nnodes,len,typ,ierr)
      if (ierr.ne.0.or.nnodes.lt.1) goto 9999
      call cmo_get_info('itp1',cmo,ip_itp1,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('imt1',cmo,ip_imt1,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      do i=1,nnodes
         if (itp1(i).eq.ifitpcup) imt1(i)=ncolors+1
      enddo

      lold=0
      ip_newtetc=ip_itetclr

      goto 3000

c ........................................................................
c statistics
2000  continue

      call cmo_get_info('nelements',cmo,nelements,len,typ,ierr)
      if (ierr.ne.0.or.nelements.lt.1) goto 9999
      call cmo_get_info('jtet',cmo,ip_jtet,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('jtetoff',cmo,ip_jtetoff,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('itettyp',cmo,ip_itettyp,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('itetclr',cmo,ip_itetclr,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call cmo_get_info('mbndry',cmo,mbndry,len,typ,ierr)
      if (ierr.ne.0) mbndry=0
      call cmo_get_info('faces_per_element',cmo,nef_cmo,len,typ,ierr)
      if (ierr.ne.0) goto 9999
      call mmggetbk('newtetc',mapname,ip_newtetc,nelements,1,ierr)
      if (ierr.ne.0) goto 9999
      call mmggetbk('ellist',mapname,ip_ellist,nelements,1,ierr)
      if (ierr.ne.0) goto 9999

      ncolors=0
      do iel=1,nelements
         j=itetclr(iel)
         if (j.lt.1.or.j.gt.nmats) then
             newtetc(iel)=0
         elseif (gclr_valid(j).ne.0) then
             newtetc(iel)=0
         else
             newtetc(iel)=itetclr(iel)
         endif
      enddo
      do iel=1,nelements
         if (newtetc(iel).eq.0) then
            ncolors=ncolors+1
            if (ncolors.le.nmats.and.gclr_valid(ncolors).eq.0)
     &            ncolors=ncolors+1
            newtetc(iel)=ncolors
            nels=1
            ellist(1)=iel
2100        kel=ellist(1)
            do j=1,nelmnef(itettyp(kel))
              jel=jtet(jtetoff(kel)+j)
              if ( (mbndry.gt.0.and.jel.lt.mbndry)
     &             .or. (mbndry.eq.0.and.jel.gt.0) ) then
                 jel=1+(jel-1)/nef_cmo
                 if (newtetc(jel).eq.0) then
                    newtetc(jel)=ncolors
                    nels=nels+1
                    ellist(nels)=jel
                 endif
              endif
            enddo
            if (nels.gt.1) then
               ellist(1)=ellist(nels)
               nels=nels-1
               goto 2100
            endif
         endif
      enddo

c print statistics
c   - number of regions
c   - average volume/region, mean volume/region
c   - smallest volume region, largest volume region
c   X- average surface_area/region, mean surface_area/region
c   X- average int_surface_area/region, mean int_surface_area/region
c
      lold=icharlnf(oldimtname)
      if (lold.gt.0.and.oldimtname(1:1).eq.'-') lold=0
3000  if (lold.eq.0) then
         write(cbuf,*) '# connected matl regions:',ncolors
         call writloga('default',0,cbuf,0,ierr)
      else
         iunit=nextlun()
         open (unit=iunit,file=oldimtname(1:lold))
3100     read(iunit,*,end=3200)
         goto 3100
3200     backspace(unit=iunit)
         write(iunit,*) '----------------'
         write(iunit,*) '# connected matl regions:',ncolors
      endif

      call cmo_get_info('itet',cmo,ip_itet,len,typ,ierr)
      call cmo_get_info('itetoff',cmo,ip_itetoff,len,typ,ierr)
      call cmo_get_info('xic',cmo,ip_xic,len,typ,ierr)
      call cmo_get_info('yic',cmo,ip_yic,len,typ,ierr)
      call cmo_get_info('zic',cmo,ip_zic,len,typ,ierr)
      call cmo_get_info('nodes_per_element',cmo,nen_cmo,len,typ,ierr)
      if (ierr.ne.0.or.nen_cmo.lt.1) goto 9999
      call mmggetbk('xlist',mapname,ip_xlist,nen_cmo,2,ierr)
      call mmggetbk('ylist',mapname,ip_ylist,nen_cmo,2,ierr)
      call mmggetbk('zlist',mapname,ip_zlist,nen_cmo,2,ierr)
      call mmggetbk('vols',mapname,ip_vols,ncolors,2,ierr)
      do i=1,ncolors
        vols(i)=0
      enddo
      do iel=1,nelements
         do i=1,nelmnen(itettyp(iel))
            j=itet(itetoff(iel)+i)
            xlist(i)=xic(j)
            ylist(i)=yic(j)
            zlist(i)=zic(j)
         enddo
         call  volume_element(itettyp(iel),
     &                xlist,ylist,zlist,vvv)
         vols(newtetc(iel))=vols(newtetc(iel))+vvv
      enddo
      imin=1
      imax=1
      v_av=0.d0
      v_mean=0.d0
      do i=1,ncolors
        if (vols(i).lt.vols(imin)) imin=i
        if (vols(i).gt.vols(imax)) imax=i
        v_av=v_av+vols(i)
        v_mean=v_mean+vols(i)*vols(i)
      enddo
      v_av=v_av/ncolors
      v_mean=sqrt(v_mean/ncolors)

      if (lold.eq.0) then
         write(cbuf,*) 'average,mean vol/region',v_av,v_mean
         call writloga('default',0,cbuf,0,ierr)
         write(cbuf,*) 'min,max vol/region',vols(imin),vols(imax)
         call writloga('default',0,cbuf,0,ierr)
      else
         write(iunit,*) 'average,mean vol/region',v_av,v_mean
         write(iunit,*) 'min,max vol/region',vols(imin),vols(imax)
      endif

      if (lold.eq.0) then
         continue
      else
         close(iunit)
      endif

      if (action(1:lenact).eq.'stats')
     &   call mmrelblk('newtetc',mapname,ip_newtetc,ierr)
      call mmrelblk('ellist',mapname,ip_ellist,ierr)
      call mmrelblk('xlist',mapname,ip_xlist,ierr)
      call mmrelblk('ylist',mapname,ip_ylist,ierr)
      call mmrelblk('zlist',mapname,ip_zlist,ierr)
      call mmrelblk('vols',mapname,ip_vols,ierr)

c ===============================================================
c normal return

9000  if (ierror.ne.0) goto 9999
      return

c ...............................................................
c error return

9999  if (ierror.eq.0) ierror=1
      cbuf='GCOLOR COLOR ERROR'
      call writloga('default',0,cbuf,0,ierr)
      return

c ...............................................................
      end

C ###############################################################
