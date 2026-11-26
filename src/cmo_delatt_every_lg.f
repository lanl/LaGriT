C SUBROUTINE BEGIN ####################################################
      subroutine cmo_delatt_every_lg(cmo_in,ifall,ierror)
C #####################################################################
C
C     PURPOSE - cmo / delatt / mo / -all-  and  cmo / DELATT / mo / -all-
C     (GitHub #163, #108; Carl Gable, Nov 26 2026):
C
C       delatt / -all-   delete every attribute of the mesh object whose
C                        persistence is temporary; permanent ones stay,
C                        and so do the standard attributes (the one
C                        temporary standard attribute is -def-, a
C                        placeholder)
C       DELATT / -all-   delete every attribute that is not one of the
C                        standard attributes, those cmo / create makes
C                        (the attributes of the mesh object -default-),
C                        whatever its persistence
C
C     Each attribute is deleted with its own command (cmo / delatt or
C     cmo / DELATT, by dotask), so it is deleted as that command does
C     it. The list is scanned again after each deletion, because
C     DELATT also deletes the attributes that use a deleted scalar as
C     their length or rank.
C
C     INPUT   cmo_in  the mesh object (-cmo- and -def-: the current one)
C             ifall   0 for delatt, 1 for DELATT
C     OUTPUT  ierror  0, or 1 when the mesh object does not exist
C
C #####################################################################
      implicit none
      include 'cmo_lg.h'
      character*(*) cmo_in
      integer ifall, ierror
C
      character*32 cmo, cname, ctype, crank, clen, cinter, cpers, cio
      character*32 cdef
      pointer (ipdefl, deflist)
      character*32 deflist(*)
      integer lend, ics
      character*132 logmess
      character*160 ctask
      integer natts, ndef, i, j, idx, ierr, ilen, ityp, ndel
      integer lcmo, isstd, ifound, ipass, nafter
      integer icharlnf
C
      ierror=0
      cmo=cmo_in
      if (cmo(1:5).eq.'-cmo-' .or. cmo(1:5).eq.'-def-')
     *   call cmo_get_name(cmo,ierr)
      lcmo=icharlnf(cmo)
      call cmo_exist(cmo,ierr)
      if (ierr.ne.0) then
         write(logmess,'(a,a)')
     *     'ERROR cmo / delatt / -all-: no mesh object ',cmo(1:lcmo)
         call writloga('default',0,logmess,0,ierr)
         ierror=1
         return
      endif
C
      ndel=0
C     the standard attributes: those of the mesh object -default-
      ndef=number_of_default_attributes
      call mmfindbk('cmo_attlist','default_cmo_lg',ipdefl,lend,ics)
      if (ics.ne.0) ndef=0
C     one pass per deletion: find the first attribute to delete
      do ipass=1,100000
         call cmo_get_info('number_of_attributes',cmo,natts,ilen,
     *      ityp,ierr)
         ifound=0
         do i=1,natts
            call cmo_get_attribute_name(cmo,i,cname,ierr)
C           a standard attribute always stays (also the temporary
C           placeholder -def-)
            isstd=0
            do j=1,ndef
               cdef=deflist(number_of_params_per_att*(j-1)+1)
               if (cdef.eq.cname) isstd=1
            enddo
            if (isstd.eq.1) goto 40
            if (ifall.eq.0) then
               call cmo_get_attparam(cname,cmo,idx,ctype,crank,clen,
     *            cinter,cpers,cio,ierr)
               if (ierr.eq.0 .and. cpers(1:9).eq.'temporary') then
                  ifound=i
                  goto 50
               endif
            else
               ifound=i
               goto 50
            endif
 40         continue
         enddo
 50      continue
         if (ifound.eq.0) goto 100
         if (ifall.eq.0) then
            ctask='cmo / delatt / '//cmo(1:lcmo)//' / '//
     *         cname(1:icharlnf(cname))//' ; finish'
         else
            ctask='cmo / DELATT / '//cmo(1:lcmo)//' / '//
     *         cname(1:icharlnf(cname))//' ; finish'
         endif
         call dotask(ctask,ierr)
C        an attribute that will not go: stop rather than loop
         call cmo_get_info('number_of_attributes',cmo,nafter,ilen,
     *      ityp,ierr)
         if (nafter.ge.natts) then
            write(logmess,'(a,a)')
     *        'ERROR cmo / delatt / -all-: cannot delete ',
     *        cname(1:icharlnf(cname))
            call writloga('default',0,logmess,0,ierr)
            ierror=1
            goto 100
         endif
         ndel=ndel+(natts-nafter)
      enddo
 100  continue
      call cmo_get_info('number_of_attributes',cmo,natts,ilen,ityp,
     *   ierr)
      if (ifall.eq.0) then
         write(logmess,'(a,a,a,i0,a,i0,a)') 'delatt -all- ',
     *     cmo(1:lcmo),': deleted ',ndel,
     *     ' temporary attributes, kept ',natts
      else
         write(logmess,'(a,a,a,i0,a,i0,a)') 'DELATT -all- ',
     *     cmo(1:lcmo),': deleted ',ndel,
     *     ' attributes, kept ',natts,' standard'
      endif
      call writloga('default',0,logmess,0,ierr)
      return
      end
