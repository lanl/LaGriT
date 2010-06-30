      subroutine cmo_att_derive(cmo1,cmo2,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine copies the set of attributes from mesh object 2
C         into mesh object 1 (so that merging them will work properly).
C         The new attributes are left uninitialized.
C
C      INPUT ARGUMENTS -
C
C         cmo1 - (character) Derived Mesh Object Name.
C         cmo2 - (character) Master Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C        
CPVCS       Rev 1.0   06/21/10 11:41:28   agable
CPVCS     Original Version
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
      character*32 cmo1, cmo2
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, ierror, ierr, len,index, icscode

C  Variables for subroutine calls
      integer numatts, lout, itype
      character*32 attname, src_mo,sink_mo
      character*32 ctype, crank, clen, cinter, cpers, cio
      character*32 ctype2, crank2, clen2, cinter2, cpers2, cio2

      character*32  cmoname
      character*132 logmess
      character*512 cmd
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
      
      ierror_return = 0
      src_mo = '-notset-'
      sink_mo = '-notset-'
      attname = '-notset-'

      src_mo = cmo1
      sink_mo = cmo2
C
      len=icharlnf(sink_mo)
      if((sink_mo(1:len).eq.'-cmo-') .or.
     *   (sink_mo(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Sink.
C
         call cmo_get_name(sink_mo,ierror_return)
C
      endif
C
      len=icharlnf(src_mo)
      if((src_mo(1:len).eq.'-cmo-') .or.
     *   (src_mo(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Master.
C
         call cmo_get_name(src_mo,ierror_return)
C
      endif
C
      len=max(icharlnf(sink_mo), icharlnf(src_mo))
      if(sink_mo(1:len) .eq. src_mo(1:len)) then
C
C....    The two Mesh Objects are identical.
C
         ierror_return=-1
         write(logmess,'(a,a,a)')
     *      'Mesh Objects are identical: ',sink_mo, src_mo
         call writloga('default',0,logmess,0,ierr)
         goto 9999
      endif

C....    Make sure both mesh objects actually exist.
      call cmo_exist(sink_mo, ierror)
      if (ierror .ne. 0) then
          ierror_return = ierror
          write(logmess,'(a,a)') 
     *     'ERROR: Invalid sink object: ',sink_mo
            call writloga('default',0,logmess,0,ierr)
          write(logmess,'(a)') 
     *     'Can not derive mesh object.'
            call writloga('default',0,logmess,0,ierr)
          goto 9999
      endif

      call cmo_exist(src_mo, ierror)
      if (ierror .ne. 0) then
         ierror_return = ierror
          write(logmess,'(a,a)') 
     *     'ERROR: Invalid source object: ',src_mo
            call writloga('default',0,logmess,0,ierr)
          write(logmess,'(a)')  
     *     'Can not derive mesh object.'
            call writloga('default',0,logmess,0,ierr)
          goto 9999
      endif

C....    Determine the number of attributes in the source object
      call cmo_get_info('number_of_attributes', src_mo, 
     *                  numatts, lout, itype, ierror)
      
C....    Loop through all the attributes in the source. For each
C        attribute, get its important parameters. Check if it exists in
C        the sink mesh already. If it doesn't, create it using a dotask.
      do i=1,numatts

          attname = '-notset-'

          call cmo_get_attribute_name(src_mo,i,attname,ierror)
          if (ierror .ne. 0) then
            ierror_return = ierror
            write(logmess,'(a,i5)') 
     *           'ERROR: Can not get attribute name at index ',i
            call writloga('default',0,logmess,0,ierr)
            goto 9999
          endif

          call cmo_get_attparam(attname, src_mo, index, 
     *                          ctype, crank, clen, cinter, cpers, cio,
     *                          ierror)


C         error from this call is ok 
          call cmo_get_attparam(attname, sink_mo, index,
     *                          ctype2,crank2,clen2,cinter2,cpers2,cio2,
     *                          ierror)

C....    If the sink mesh already has an attribute with this name:
          if (ierror .eq. 0) then
C...     See if it is the same or a different attribute
              if (crank .ne. crank2 .or. clen .ne. clen2) then
                  ierror_return = -1
                  write(logmess, '(a,a)') 
     *                  'ERROR: attributes with the '//
     *                  'same name but different parameters exist in '//
     *                  'source and sink meshes: ', attname
                  call writloga('default',0,logmess,0,ierr)
                  goto 9999
              endif
              if (ctype .ne. ctype2) then
                  if (ctype .eq. 'CHAR' .or. ctype .eq. 'VCHAR' .or.
     *                ctype2 .eq. 'CHAR' .or. ctype2 .eq. 'VCHAR') then
                      ierror_return = -1
                      write(logmess, '(a,a)') 
     *                  'ERROR: attributes with the '//
     *                  'same name but different parameters exist in '//
     *                  'source and sink meshes: ', attname
                      call writloga('default',0,logmess,0,ierr)
                      goto 9999
                  else
                      write(logmess, '(a,a,a)')
     *                  'WARNING: attributes with the same name but '//
     *                  'slightly different parameters exist in the '//
     *                  'source and sink meshes: ', attname,
     *                  '. Continuing.'
                      call writloga('default',0,logmess,0,ierr)
                      goto 9000
                  endif
              endif
              if (cinter .ne. cinter2 .or. cpers .ne. cpers2 .or.
     *            cio .ne. cio2) then
                  write(logmess, '(a,a,a)')
     *              'WARNING: attributes with the same name but '//
     *              'slightly different parameters exist in the '//
     *              'source and sink meshes: ', attname,
     *              '. Continuing.'
                  call writloga('default',0,logmess,0,ierr)
                  goto 9000
              endif
              goto 9000 
          endif
C...    Now, since the attribute isn't in the sink mesh, we add it

          cmoname = sink_mo
          cmd = 'cmo/addatt/'
     *        //cmoname(1:icharlnf(cmoname))//'/'
     *        //attname(1:icharlnf(attname))//'/'
     *        //ctype(1:icharlnf(ctype))//'/'
     *        //crank(1:icharlnf(crank))//'/'
     *        //clen(1:icharlnf(clen))//'/'
     *        //cinter(1:icharlnf(cinter))//'/'
     *        //cpers(1:icharlnf(cpers))//'/'
     *        //cio(1:icharlnf(cio))//'; finish'

          call dotask(cmd, ierror)
          if (ierror.ne.0) then
            call x3d_error('dotask in ','cmo_att_derive')
            ierror_return = 1
          endif

9000      continue
      enddo
9999  return
      end




      subroutine cmo_att_union(mesh1,mesh2,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine copies the set of attributes from mesh object 2
C         into mesh object 1 *and vice versa* (so that merging them will
C         work properly). The new attributes are left uninitialized.
C
C      INPUT ARGUMENTS -
C
C         mesh1 - (character) Derived Mesh Object Name.
C         mesh2 - (character) Master Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C        
CPVCS       Rev 1.0   06/21/10 11:41:28   agable
CPVCS     Original Version
C
C#######################################################################
C
      implicit none

      character*32 mesh1, mesh2
      integer ierror_return

      character*32 cmoname1, cmoname2
      character*132 logmess
      integer ierr, ierror, ierror2
C
C#######################################################################
C
      ierror_return = 0
      ierror = 0
      ierror2 = 0

      cmoname1 = mesh1
      cmoname2 = mesh2
      call cmo_att_derive(cmoname1, cmoname2, ierror)

      if (ierror .ne. 0 ) then
         call x3d_error('cmo_att_derive from first cmo: ',mesh1)
         ierror_return = -1
      else
         write(logmess,'(a,a)')
     *     'Done with cmo_att_derive from first cmo: ',mesh1
         call writloga('default',0,logmess,0,ierr)
      endif

      cmoname1 = mesh2
      cmoname2 = mesh1
      call cmo_att_derive(cmoname1, cmoname2, ierror2)

      if (ierror2 .ne. 0) then
         call x3d_error('cmo_att_derive from second cmo: ',mesh2)
         ierror_return = -2
      else
         write(logmess,'(a,a)')
     *     'Done with cmo_att_derive from second cmo: ',mesh2
         call writloga('default',0,logmess,0,ierr)
      endif

      return
      end


