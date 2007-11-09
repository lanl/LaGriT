      subroutine read_lagrit(ifile,cmoread,iomode,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C    This routine reads the geometry information, surfaces,
C    regions and mregions from a file.  It also reads cmo info
C    from the file
C
C
C     INPUT ARGUMENTS -
C
C     iunit  unit number of file
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log: read_lagrit.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   10 Apr 2001 11:04:26   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.3   21 Mar 2000 12:59:40   dcg
CPVCS    if mesh objects already exist, add to list don't replace
CPVCS
CPVCS       Rev 1.2   Mon Feb 14 17:43:12 2000   dcg
CPVCS    set ascii or binary automatically
CPVCS
CPVCS       Rev 1.1   07 Feb 2000 16:50:54   dcg
CPVCS
CPVCS       Rev 1.4   Wed Jul 14 15:10:46 1999   dcg
CPVCS    Detect if ascii or binary file - look for errors on input and
CPVCS    allow for some mistakes
CPVCS
CPVCS       Rev 1.3   Tue May 11 16:50:52 1999   dcg
CPVCS    allow for binary or ascii lagrit dumps
CPVCS
CPVCS       Rev 1.2   Wed Mar 31 14:54:44 1999   dcg
CPVCS    close file when finished reading
CPVCS
CPVCS       Rev 1.1   Tue Mar 09 15:05:28 1999   dcg
CPVCS     read in cmo and pset, eset info for lagrit dumps
CPVCS
CPVCS       Rev 1.0   Fri Mar 05 11:15:20 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'cmo_lg.h'
 
      character ifile*(*)
      character*(*) cmoread
      character*32 partname,cmoin,cmo_name_sb
      character*(*) iomode
      integer i,iunit,ierror,ierror_return,icscode,len,nmo
c
C#######################################################################
C
      ierror_return=0
      iunit=-1
      cmoin=cmoread
      if(iomode(1:5).eq.'ascii') then
         call hassign(iunit,ifile,ierror)
      else
         call hassign(iunit,ifile,ierror)
         close (iunit)
         open (iunit,file=ifile,form='unformatted')
      endif
c
c read and store global variables
c
      call read_globals_lg(iunit,ifile,iomode,ierror_return)
c
c  read geometries
      call read_geometries_lg(iunit,iomode,ierror)
c
c  read in mesh objects
c  first write out geometry info - then write mesh object by attribute
c
      partname='define_cmo_lg'
      call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
 5    if(cmoin(1:5).eq.'-all-') then
         if (iomode(1:5).eq.'ascii') then
            read(iunit,10) nmo,
     *   number_of_params_per_att,
     *   current_cmo_index,number_of_default_attributes,
     *   number_of_default_attparam_name
 10         format (10i10)
         else
            read(iunit)nmo,
     *   number_of_params_per_att,
     *   current_cmo_index,number_of_default_attributes,
     *   number_of_default_attparam_name
         endif
c
c  -default- mesh object is not written out so increment
c   index when reading mesh objects
c
C
         do i=2,nmo
 
C....       NAME Field.
C
            cmo_name_sb=cmo_names(i)
            if(cmo_name_sb(1:9).ne.'-default-') then
C
               if(iomode(1:5).eq.'ascii') then
                  read(iunit,9000) cmo_name_sb
               else
                  read(iunit) cmo_name_sb
               endif
 9000          format(a32)
               call cmo_readdump_cmo(cmo_name_sb,iunit,iomode,ierror)
               if(ierror.ne.0) go to 9999
            endif
         enddo
      else
         if(iomode(1:5).eq.'ascii') then
         i=1
            read(iunit,10) i,
     *   number_of_params_per_att,
     *   i,number_of_default_attributes,
     *   number_of_default_attparam_name
            read(iunit,9000) cmoin
         else
            read(iunit)i,
     *   number_of_params_per_att,
     *   i,number_of_default_attributes,
     *   number_of_default_attparam_name
            read(iunit) cmoin
         endif
         call cmo_readdump_cmo(cmoin,iunit,iomode,ierror)
      endif
      go to 9999
 
 
c
 9999 continue
      close(iunit)
      return
      end
