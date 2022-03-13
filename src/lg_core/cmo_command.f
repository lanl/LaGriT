      subroutine cmo_command(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                       ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Processes a CMO Command.
C
C         FORMAT: CMO / 
C                       ADDATT  / mo_name / name / type / rank / length
C                               / interpol / persistence / io / value /
C                       COMPRESS/ [mo_name | -cmo- | -all-] 
C                       CONSTRAINT / mo_sink / mo_source /
C                       COPY    / [mo_name | -cmo-] / master_mo | -cmo- /
C                       COPYATT / mo_name / master_mo / attribute / src_attribute
C                       CREATE  / mo_name /[npoints/nelements/meshtype]
C                       DELATT  / mo_name |-cmo-|-default- / atttibute | -all- /
C                       DESTROY / mo_name /
C                       DELETE  / mo_name /
C                       DERIVE  / [mo_name| -cmo- | -default-] / master_mo /
C                       GEOMETRY  / mo_name / geometry_name /
C                       FILLATT / mo_name / name  
C                       KDTREE /  BUILD  
C                       LENGTH  / [mo_name |-cmo-|-default-| -all-]/[attribute | -all-] 
C                       LIST
C                       MEMORY  / mo_name | -cmo- /number_nodes/number_elements
C                       MODATT  / mo_name / atttibute /field/new_field
C                       MOVE    / mo_name| -cmo- / [master_mo |-cmo-] /
C                       NEWLEN  / mo_name | -cmo-/
C                       PRINTATT/ mo_name /attribute/
C                                 ifirst/ilast/istride/value
C                       READATT / mo_name /attr1/attr2/.../file_name/
C                       RECOLOR / mo_name / mode /
C                       RELEASE / mo_name /
C                       SELECT  / mo_name /
C                       SET_ID  / mo_name /[node|element|both]/[att_name_node]/[att_name_elem]
C                       SETATT  / mo_name / attribute ifirst ilast istride / / value
C                       STATUS  / mo_name| -cmo- | -default-|-all-] / [output_format]
C                       VERIFY  / [mo_name | -cmo- | -default-|-all-] 
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
C         $Log: cmo_command.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.12   17 Oct 2007 15:12:28   tam
CPVCS    modified error checking  for existing cmo so the
CPVCS    check is done only for actions where necessary
CPVCS    
CPVCS       Rev 1.10   01 Oct 2007 08:19:32   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty.
CPVCS    
CPVCS       Rev 1.9   23 Feb 2005 08:21:58   tam
CPVCS    added fillatt option that avoids warnings for attributes
CPVCS    that already exist and allows the attribute to be filled
CPVCS    
CPVCS       Rev 1.8   27 Jul 2001 11:01:38   tam
CPVCS    add coption to choose brief version of cmo_status()
CPVCS    
CPVCS       Rev 1.7   06 Oct 2000 11:12:32   gable
CPVCS    Added cmo/set_id option
CPVCS    
CPVCS       Rev 1.6   Wed Apr 05 13:54:06 2000   dcg
CPVCS    use geom_name for geometry calls
CPVCS    
CPVCS       Rev 1.5   Thu Feb 17 13:39:38 2000   dcg
CPVCS    add cmo_constraints routine
CPVCS
CPVCS       Rev 1.4   Tue Feb 15 10:43:16 2000   dcg
CPVCS    add geometry command
CPVCS
CPVCS       Rev 1.3   Tue Feb 08 14:11:06 2000   dcg
CPVCS
CPVCS       Rev 1.2   28 Jan 2000 10:46:30   kuprat
CPVCS    Shifted kdtree arguments to correct position.
CPVCS
CPVCS       Rev 1.1   26 Jan 2000 13:20:14   jtg
CPVCS    cmo/DELATT calls cmo_delatt_all_lg to force deletetion
CPVCS    of attribute and, if scalar, all fields with it as length or rank
CPVCS    (vs cmo/delatt calling cmo_delatt -> delete only this and only if temporary)
CPVCS
CPVCS       Rev 1.0   Thu Jan 20 14:50:54 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.21   Fri Apr 02 09:41:02 1999   nnc
CPVCS    Added cmo/recolor option.
CPVCS
CPVCS       Rev 1.20   Tue Sep 29 14:21:46 1998   dcg
CPVCS    add cmo_readatt
CPVCS    use length of option in comparisons
CPVCS
CPVCS       Rev 1.19   Mon Jun 22 09:47:48 1998   kmb
CPVCS    Added call to kdtree routine
CPVCS
CPVCS       Rev 1.18   Mon Jun 22 09:01:34 1998   dcg
CPVCS    add cmo/copyatt option
CPVCS
CPVCS       Rev 1.17   Mon Nov 03 16:10:10 1997   tam
CPVCS    changed nwds check for printatt and setatt to 4 mininum
CPVCS
CPVCS       Rev 1.16   Mon Sep 08 13:52:28 1997   dcg
CPVCS    add setatt and printatt options
CPVCS
CPVCS       Rev 1.15   Mon Apr 14 16:39:56 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.14   09/14/95 16:37:30   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.13   03/15/95 15:22:26   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.12   02/16/95 09:55:46   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.11   02/10/95 14:06:36   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.9   01/30/95 06:21:58   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.8   01/26/95 11:22:12   het
CPVCS
CPVCS
CPVCS       Rev 1.7   01/24/95 08:52:20   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.6   01/23/95 17:02:30   het
CPVCS    Correct some character problems with the table driven
CPVCS       cmo attributes.
CPVCS
CPVCS
CPVCS       Rev 1.5   01/23/95 12:47:00   het
CPVCS    Put in the define_attribute table.
CPVCS
CPVCS
CPVCS       Rev 1.4   01/11/95 08:31:38   het
CPVCS    Correct an error with the definition of cmo and cmo_name.
CPVCS
CPVCS
CPVCS       Rev 1.3   01/04/95 22:01:28   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.2   12/24/94 12:51:22   het
CPVCS    Corrected an error in the cmo_create area. I was calling
CPVCS    cmo_get_name instead of cmo_set_name.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/21/94 18:45:58   het
CPVCS    Add the "cmo/status" option.
CPVCS
CPVCS
CPVCS       Rev 1.0   12/09/94 22:49:30   het
CPVCS    Original version.
CPVCS
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'cmo_lg.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      character*32 cmo_name, cmo1,cmo2, coption 
      character*32 cmolist, attlist,cmo_sink,cmo_src,geom_name_arg

      integer len, ierr, ierr1, ierr2, lenopt, ierrw
      integer inum1,inum2,inum3,inum4,inum5
C
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C BEGIN begin
C
C
      ierror_return = 0 
      cmo_name = '-notset-'
      coption = '-notset-'
     
      if (nwds.ge.2 .and. msgtype(2).eq.3) then
         coption = cmsgin(2)
         lenopt=icharlnf(coption)
      else
         write(logmess,'(a)')
     *   ' ERROR CMO: Invalid option, must be character type.'
         call writloga('default',0,logmess,1,ierr)
         ierror_return = -1
         go to 9999
      endif

      if (nwds.gt.2) then
         if (msgtype(3).eq.3) then
            cmo_name = cmsgin(3)
         endif
      endif

      if(cmo_name(1:icharlnf(cmo_name)) .eq. '-def-')then
        call cmo_get_name(cmo_name,ierr)
        if(ierr.ne.0) then
           write(logmess,'(a)') 'CMO found bad mesh object'
           call writloga('default',0,logmess,0,ierrw)
           go to 9999
        endif
      endif

C
C**** The cmo MAY or MAY NOT exist for these actions. 
C   DESTROY / mo_name /
C   DELETE  / mo_name /
C   PRINTATT/ mo_name /attribute/
C   RELEASE / mo_name /
C   SELECT  / mo_name /
      
C**** The cmo does not need to exist for these actions. 
C   COPY    / [mo_name | -cmo-] / master_mo | -cmo- /
C   COMPRESS/ [mo_name | -cmo- | -all-]
C   CREATE  / mo_name /[npoints/nelements/meshtype]
C   DELATT  / mo_name |-cmo-|-default- / atttibute | -all- /
C   DERIVE  / [mo_name| -cmo- | -default-] / master_mo /
C   KDTREE /  BUILD
C   LIST
C   LENGTH  / [mo_name |-cmo-|-default-| -all-]/[attribute | -all-] 
C   MEMORY  / mo_name | -cmo- /number_nodes/number_elements
C   MOVE    / mo_name| -cmo- / [master_mo |-cmo-] /
C   NEWLEN  / mo_name | -cmo-/
C   STATUS  / mo_name| -cmo- | -default-|-all-] / [output_format]
C   VERIFY  / [mo_name | -cmo- | -default-|-all-] 

C****  The cmo MUST exist for these actions. 
C   ADDATT  / mo_name / name / type / rank / length
C   CONSTRAINT / mo_sink / mo_source /
C   COPYATT / mo_name / master_mo / attribute / src_attribute
C   FILLATT / mo_name / name
C   GEOMETRY  / mo_name / geometry_name /
C   MODATT  / mo_name / atttibute /field/new_field
C   READATT / mo_name /attr1/attr2/.../file_name/
C   RECOLOR / mo_name / mode /
C   SET_ID  / mo_name /[node|element|both]/[att_name_node]/[att_name_elem]
C   SETATT  / mo_name / attribute ifirst ilast istride / / value
 
      if((coption(1:lenopt).eq. 'addatt') .or.
     1   (coption(1:lenopt).eq. 'constraint') .or.
     1   (coption(1:lenopt).eq. 'copyatt') .or.
     1   (coption(1:lenopt).eq. 'fillatt')   .or.
     1   (coption(1:lenopt).eq. 'geometry')   .or.
     1   (coption(1:lenopt).eq. 'modatt')   .or.
     1   (coption(1:lenopt).eq. 'readatt')   .or.
     1   (coption(1:lenopt).eq. 'recolor')   .or.
     1   (coption(1:lenopt).eq. 'set_id')   .or.
     1   (coption(1:lenopt).eq. 'setatt')) then

       call cmo_exist(cmo_name,ierr)
       if(ierr .ne. 0)then
         write(logmess,'(a)')
     *   "Warning: MO DOES NOT EXIST: "//cmo_name
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')
     *   "Warning: NO ACTION: "//coption(1:lenopt)
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a)')'cmo_command: RETURN'
         call writloga('default',0,logmess,0,ierr)
         return
       endif
      endif
C
      if(coption(1:lenopt).eq.'addatt' .or. 
     *    coption(1:lenopt).eq.'fillatt') then
C
C....    ADDATT Option.
C
         if(nwds .ge. 4) then
C
            call cmo_addatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a)')
     *  ' ERROR CMO addatt: Must specify Mesh Object and Attribute'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'set_id') then
C
C....    SETATT Option.
C
         call cmo_node_elem_id
     *            (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror_return)
C     
      elseif(coption(1:lenopt).eq.'setatt') then
C
C....    SETATT Option.
C
         if(nwds .ge. 4) then
C
            call cmo_setatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a,a)')
     *     ' ERROR CMO setatt: Must specify Mesh Object and Attribute'
     *     ,' point or element set and value'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
 
      elseif(coption(1:lenopt).eq.'printatt') then
C
C....    PRINTATT Option.
C
         if(nwds .ge. 4) then
C
            call cmo_setatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a,a)')
     *   ' ERROR CMO printatt: Must specify Mesh Object and Attribute'
     *      ,' point or element set'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
 
      elseif(coption(1:lenopt).eq.'copyatt') then
C....    COPYATT Option.
C
         if(nwds .ge. 4) then
C
            call cmo_copyatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a,a)')
     *   '  ERROR CMO copyatt: Must specify Mesh Object and Attribute'
     *      ,' point or element set'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'compress') then
C
C....    COMPRESS Option.
C
         if(nwds.lt.3) then
            cmolist='-cmo-'
         else
            cmolist=cmo_name
         endif
C
         call cmo_compress(cmolist,ierror_return)
C
      elseif(coption(1:lenopt).eq.'copy') then
C
C....    COPY Option.
C
         if(nwds .ge. 3) then
C
            if(nwds.lt.4) then
               cmsgin(4)='-cmo-'
            endif
C
            call cmo_copy(cmsgin(3),cmsgin(4),ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a)')
     *   ' ERROR CMO copy: No Destination Mesh Object defined'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'create') then
C
C....    CREATE Option.
C
         if(nwds.ge.3) then
C
            call cmo_create_cmo(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                          ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a)')
     *      ' ERROR CMO create: No Mesh Object defined'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'delatt'
     *         .or. coption(1:lenopt).eq.'DELATT') then
C
C....    DELATT Option.
C
         if(nwds .ge. 4) then
C
            if (coption(1:lenopt).eq.'DELATT') then
 
               !... delete even if permanent, and if a scalar
               !... also delete anything with this as rank or length
 
               call cmo_delatt_all_lg(imsgin,xmsgin,cmsgin,msgtype,
     *                      nwds,ierror_return)
 
            else
 
               !... delete only if temporary
 
               call cmo_delatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
            endif
C
         else
C
            ierror_return=-1
            write(logmess,'(a)')
     *   ' ERROR CMO delatt: Must specify Mesh Object and Attribute'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'derive') then
C
C....    DERIVE Option.
C
         if(nwds .ge. 3) then
C
            if(nwds.lt.4) then
               cmo2 = '-cmo-'
            elseif (msgtype(4).eq.3) then
               cmo2 = cmsgin(4)
            else
              write(logmess,'(a)')
     *   ' ERROR CMO derive: need second mesh object.'
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif

            if (msgtype(3).eq.3) then
               cmo1 = cmsgin(3)
            else
              write(logmess,'(a)')
     *       ' ERROR CMO derive: need first mesh object.'
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif
C
            call cmo_derive(cmo1,cmo2,ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a)')
     *  ' ERROR CMO derive: No Destination Mesh Object defined.'
            call writloga('default',0,logmess,0,ierr)
C
         endif

c
      elseif(coption(1:lenopt).eq.'attribute_derive') then
C
C....    ATT_DERIVE Option.
C
         if(nwds .ge. 3) then

            if(nwds.lt.4) then
               cmo2 = '-cmo-'
            elseif (msgtype(4).eq.3) then
               cmo2 = cmsgin(4)
            else
              write(logmess,'(a)')
     * ' ERROR CMO attribute_derive: need second mesh object.'
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif

            if (msgtype(3).eq.3) then
               cmo1 = cmsgin(3)
            else
              write(logmess,'(a)')
     * ' ERROR CMO attribute_derive: need first mesh object.'
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif

            call cmo_att_derive(cmo1,cmo2,ierror_return)
         else
            ierror_return=-1
            write(logmess,'(a,a)')
     *      ' ERROR CMO attribute_derive: ',
     *      'No Destination Mesh Object defined.'
            call writloga('default',0,logmess,0,ierr)
         endif
c
      elseif(coption(1:lenopt).eq.'attribute_union') then
C
C....    ATT_UNION Option.
C
         if(nwds .eq. 4) then

            if (msgtype(4).eq.3) then
               cmo2 = cmsgin(4)
            else
              write(logmess,'(a,i5)')
     *   ' ERROR CMO union: invalid type argument 4: ',msgtype(4)
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif

            if (msgtype(3).eq.3) then
               cmo1 = cmsgin(3)
            else
              write(logmess,'(a,i5)')
     *   ' ERROR CMO union: invalid type argument 3: ',msgtype(3)
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif

C tam -remove - this kind of self call overwrites arguments
C           call cmo_att_union(cmo1,cmo2,ierror_return)

            call cmo_att_derive(cmo1, cmo2, ierr1)
            if (ierr1.eq.0) then
               call cmo_att_derive(cmo2, cmo1, ierr2)
            endif 

            if (ierr1.ne.0 .or. ierr2.ne.0) ierror_return = -1

         else
            ierror_return=-1
            write(logmess,'(a)')
     *      ' ERROR CMO union: Need two mesh objects defined. '
            call writloga('default',0,logmess,0,ierr)
         endif
C
      elseif(coption(1:lenopt).eq.'length') then
C
C....    LENGTH Option.
C
         if(nwds.lt.3) then
            cmolist='-all-'
         else
            cmolist=cmo_name
         endif
C
         if(nwds.lt.4) then
            attlist='-all-'
         else
            attlist=cmsgin(4)
         endif
C
         call cmo_length(cmolist,attlist,ierror_return)
C
      elseif(coption(1:lenopt).eq.'list') then
C
C....    LIST Option.
C
         call cmo_list(ierror_return)
 
 
 
      elseif(coption(1:lenopt).eq.'kdtree') then
C
C....    KDTREE Option.
C
         call kdtree_cmo(imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1
     &      ,ierror_return)
C
      elseif(coption(1:lenopt).eq.'memory') then
C
C....    MEMORY Option.
C
         if(nwds.lt.3) then
            cmolist='-cmo-'
         else
            cmolist=cmo_name
         endif
C
         if(nwds.lt.4) then
            inum4=1
         else
            inum4 = imsgin(4)
         endif
C
         if(nwds.lt.5) then
            inum5=1
         else
            inum5= imsgin(5)
         endif
C
         call cmo_memory(cmolist,inum4,inum5,ierror_return)
C
      elseif(coption(1:lenopt).eq.'modatt') then
C
C....    MODATT Option.
C
         if(nwds .ge. 6) then
C
            call cmo_modatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a,a)')
     *     ' ERROR CMO modatt: Must specify Mesh Object, Attribute,',
     *     ' Field, and the new Field.'
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'move') then
C
C....    MOVE Option.
C
         if(nwds .ge. 3) then

           if(nwds.lt.4) then
               cmo2 = '-cmo-'
            elseif (msgtype(4).eq.3) then
               cmo2 = cmsgin(4)
            else
              write(logmess,'(a)')
     *     ' ERROR CMO move: invalid type argument 4'
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif

            if (msgtype(3).eq.3) then
               cmo1 = cmsgin(3)
            else
              write(logmess,'(a)')
     *     ' ERROR CMO move: invalid type argument 3'
              call writloga('default',0,logmess,0,ierrw)
              goto 9999
            endif
C
            call cmo_move(cmo1,cmo2,ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a)')
     *      ' ERROR CMO move: No Destination Mesh Object defined. '
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'newlen') then
C
C....    NEWLEN Option.
C
         if(nwds.lt.3) then
            cmolist='-cmo-'
         else
            cmolist=cmo_name
         endif
C
         call cmo_newlen(cmolist,ierror_return)
C
C
      elseif(coption(1:lenopt).eq.'recolor') then
C
C....    RECOLOR Option.
C
         call cmo_get_name(cmo_name,ierr)
         if(nwds.ge.3) then
            if(msgtype(3).eq.3 .and. cmsgin(3).ne.'-def-') then
               cmo_name=cmsgin(3)
            endif
         endif
 
         if(nwds.ge.4) then
            if(msgtype(4).eq.3 .and. cmsgin(4).ne.'-def-') then
               cmo2 = cmsgin(4)
            else
               cmo2='save'
            endif

            call cmo_recolor(cmo_name,cmo2,ierror_return)

         endif
 
C
      elseif(coption(1:lenopt).eq.'release' .or.
     *       coption(1:lenopt).eq.'destroy'.or.
     *       coption(1:lenopt).eq.'delete') then
C
C....    RELEASE Option.
C
         if(nwds.ge.3) then
C
            ierror_return = 0
            call cmo_release(cmo_name,ierror_return)
C
         else
C
            ierror_return=-1
            write(logmess,'(a,a,a)')
     *   ' ERROR CMO ',coption(1:lenopt),
     *   ': No Mesh Object defined. '
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'readatt') then
C
C....    SELECT Option.
C
         call cmo_readatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                    ierror_return)
C
      elseif(coption(1:lenopt).eq.'select') then
C
C....    SELECT Option.
C
         if(number_of_mesh_objects.gt.0) then
            if(nwds.eq.2) then
C
               ierror_return=0
C
               call cmo_get_name(cmo_name,ierror_return)
               call cmo_select(cmo_name,ierror_return)
C
            else
C
               call cmo_select(cmo_name,ierror_return)
C
            endif
C
         else
C
            ierror_return=0
            write(logmess,'(a)') 'No Mesh Objects defined: '
            call writloga('default',0,logmess,0,ierr)
C
         endif
C
      elseif(coption(1:lenopt).eq.'status') then
C
C....    STATUS Option.
C        coption defines format of output
C        cmo status
C        cmo status coption
C        cmo status cmo_name
C        cmo status cmo_name coption
C        cmo_name is set earlier to cmsgin(3)

c        formats
c        brief is just the header with 2 cols and 4 lines
c        long is a long header with 8 lines, 1 col
         coption = 'default'
C
         if(nwds.le.2) then
            cmolist='-all-'

         else

           if(cmo_name(1:5).eq.'brief') cmo_name='-all-'
           if(cmo_name(1:4).eq.'long') cmo_name='-all-'
           if (cmsgin(nwds)(1:5).eq.'brief') coption=cmsgin(nwds) 
           if (cmsgin(nwds)(1:4).eq.'long') coption=cmsgin(nwds) 
           cmolist = cmo_name
         endif
C
         len=icharlnf(cmolist)
         if(cmolist(1:len).eq.'-default-') then
C
            call cmo_status('-default-',coption,ierror_return)
C
         elseif(number_of_mesh_objects.gt.0) then

            call cmo_status(cmolist,coption,ierror_return)
         else
            ierror_return=0
            write(logmess,'(a)') 'No Mesh Object defined. '
            call writloga('default',0,logmess,0,ierr)
         endif
C
c
      elseif(coption(1:lenopt).eq.'geometry') then
C
C....    GEOMETRY Option.
C
         call cmo_get_name(cmo_name,ierr)
         if(nwds.ge.3) then
            if(msgtype(3).eq.3 .and. cmsgin(3).ne.'-def-') then
               cmo_name=cmsgin(3)
            endif
         endif
 
         if(nwds.ge.4) then
            geom_name_arg=cmsgin(4)
            if(msgtype(4).eq.3 .and. cmsgin(4).ne.'-def-') then
               call cmo_geometry(cmo_name,geom_name_arg,ierror_return)
            else
               ierror_return=-1
            endif
         else
            ierror_return=-1
         endif
c
      elseif(coption(1:10).eq.'constraint') then
C
C....    CONTRAINTS Option.
C
         cmo_sink=cmsgin(3)
         if(nwds.lt.3) then
            ierror_return=-1
         elseif(nwds.lt.4) then
            call cmo_get_name(cmo_src,ierr)
            call cmo_constraints(cmo_sink,cmo_src,ierror_return)
         else
            cmo_src=cmsgin(4)
            call cmo_constraints(cmo_sink,cmo_src,ierror_return)
         endif
 
c
      elseif(coption(1:lenopt).eq.'verify') then
C
C....    VERIFY Option.
C
         if(nwds.lt.3) then
            cmolist='-all-'
         else
            cmolist=cmo_name
         endif
C
         call cmo_verify(cmolist,ierror_return)
C
         if (ierror_return.eq.0) then
            write(logmess,'(a)') 'CMO Mesh Object is OK '
            call writloga('default',0,logmess,0,ierr)
         endif
C
C
      else
C
C....    Illegal Option.
C
         ierror_return=-1
         write(logmess,'(a,a)') 
     *   'ERROR CMO: invalid option: '
     *           ,coption(1:lenopt)
         call writloga('default',0,logmess,0,ierr)
C
      endif
C
 9999 if(ierror_return.ne.0) then
         write(logmess,'(a,a,2x,a,2x,i5)') 
     *   ' ERROR CMO: ',coption(1:icharlnf(coption)),
     *   cmo_name(1:icharlnf(cmo_name)),ierror_return
         call writloga('default',0,logmess,1,ierr)
      endif
     
      return
      end
