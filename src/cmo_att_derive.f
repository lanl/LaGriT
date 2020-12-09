      subroutine cmo_att_derive(sink_in,src_in,ierror_return)
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
C         sink_in - (character) Derived Mesh Object Name.
C         src_in - (character) Master Mesh Object Name.
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
      character*32 sink_in, src_in
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, ierror, ierr, len,index
C
      character*132 logmess
      character*132 cmd
C
      integer icscode
C
C  Variables for subroutine calls
      integer numatts, lout, itype

C  copy arguments into local variables to avoid losing them during dotask
      character*32 attname, src_mo, sink_mo
      character*32 ctype, crank, clen, cinter, cpers, cio
      character*32 ctype2, crank2, clen2, cinter2, cpers2, cio2
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C BEGIN begin
C
      ierror_return = 0

      len=icharlnf(sink_in)
      sink_mo = sink_in(1:len)
      if((sink_mo(1:len).eq.'-cmo-') .or.
     *   (sink_mo(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Sink.
C
         call cmo_get_name(sink_mo,ierror_return)
C
      endif
C
      len=icharlnf(src_in)
      src_mo = src_in(1:len)
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
          write(logmess,'(a)') 'ERROR: Invalid sink object'
          call writloga('default',0,logmess,0,ierr)
          goto 9999
      endif
      call cmo_exist(src_mo, ierror)
      if (ierror .ne. 0) then
          ierror_return = ierror
          write(logmess,'(a)') 'ERROR: Invalid source object'
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


          call cmo_get_attribute_name(src_mo,i,attname,ierror)

          call cmo_get_attparam(attname, src_mo, index, 
     *                          ctype, crank, clen, cinter, cpers, cio,
     *                          ierror)

          call cmo_get_attparam(attname, sink_mo, index,
     *                          ctype2,crank2,clen2,cinter2,cpers2,cio2,
     *                          ierror)

C....    If the sink mesh already has an attribute with this name:
C...     See if it is the same or a different type
          if (ierror .eq. 0) then
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
C...      Now, since the attribute isn't in the sink mesh, we add it
          
          if (ierror.eq.0) print*, "Exists, but add anyway!"
          print*,"adding ",sink_mo,attname

          cmd = 'cmo/addatt/'
     *        //sink_mo(1:icharlnf(sink_mo))//'/'
     *        //attname(1:icharlnf(attname))//'/'
     *        //ctype(1:icharlnf(ctype))//'/'
     *        //crank(1:icharlnf(crank))//'/'
     *        //clen(1:icharlnf(clen))//'/'
     *        //cinter(1:icharlnf(cinter))//'/'
     *        //cpers(1:icharlnf(cpers))//'/'
     *        //cio(1:icharlnf(cio))//'; finish'

          call dotask(cmd, ierror)

9000      continue
      enddo

      print*,"Done adding attributes to ",
     *      sink_mo(1:icharlnf(sink_mo))
      print*,"from mesh object ",src_mo(1:icharlnf(src_mo))

9999  return

      end

      subroutine cmo_att_union(sink_in,src_in,ierror_return)
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
C         sink_in - (character) Derived Mesh Object Name.
C         src_in - (character) Master Mesh Object Name.
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

      integer ierror, ierror2, ierror_return
      character*32 sink_in, src_in
      character*32 cmo1, cmo2
C
C#######################################################################
C
C     protect mesh names from being overwritten by dotask calls
      cmo1 = sink_in
      cmo2 = src_in

      print*,"cmo_att_union: in ",sink_in,src_in,cmo1,cmo2

      call cmo_att_derive(cmo1, cmo2, ierror)

      cmo1 = sink_in
      cmo2 = src_in


      call cmo_att_derive(cmo2, cmo1, ierror2)

      if (ierror .ne. 0 .or. ierror2 .ne. 0) ierror_return = -1

      print*,"cmo_att_union: out ",sink_in,src_in,cmo1,cmo2

      return
      end


