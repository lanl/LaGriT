      subroutine dump_lagrit(ifile,cmoin,iomode,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C    This routine writes the geometry information, surfaces,
C    regions and mregions to a file.  It also writes cmo info
C    to the file
C
C
C     INPUT ARGUMENTS -
C
C     iunit  unit number of file
c     cmoin  name of mesh object to dump
c     iomode 'binary' or 'ascii'
C
C     OUTPUT ARGUMENTS -
C
C     ierror error flag
C
C     CHANGE HISTORY -
C
C        $Log: dump_lagrit.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   06 Mar 2006 09:47:30   gable
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   10 Apr 2001 11:04:24   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.1   Mon Feb 07 14:09:40 2000   dcg
CPVCS
CPVCS       Rev 1.5   Thu Dec 16 10:05:06 1999   dcg
CPVCS    read and write global variables/ ihcycle, time, monitor
CPVCS
CPVCS       Rev 1.4   Wed Jul 14 15:10:52 1999   dcg
CPVCS    Detect if ascii or binary file - look for errors on input and
CPVCS    allow for some mistakes
CPVCS
CPVCS       Rev 1.3   Tue May 11 16:50:44 1999   dcg
CPVCS    allow for binary or ascii lagrit dumps
CPVCS
CPVCS       Rev 1.2   Wed Mar 31 14:56:08 1999   dcg
CPVCS    close file when finished writing
CPVCS
CPVCS       Rev 1.1   Tue Mar 09 15:02:44 1999   dcg
CPVCS    read in cmo and pset, eset info
CPVCS
CPVCS       Rev 1.0   Fri Mar 05 11:15:14 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'cmo_lg.h'
      include 'geom_lg.h'
      character ifile*(*)
      character*(*) cmoin
      character*32 cmo_name_sb
      integer i,iunit,ierror,icscode,len
      character*(*) iomode
      character*32 partname
      character*32 isubname
 
 
C
C#######################################################################
C
      ierror=0
      isubname="dump_lagrit"

      call fexist(ifile,i)
      if(i.eq.1) call close_lg(ifile)
      iunit=-1
      if (iomode(1:5).eq.'ascii') then
         call hassign(iunit,ifile,ierror)
         if (iunit.lt.0 .or. ierror.lt.0) then
           call x3d_error(isubname,'hassign bad file unit')
           ierror = -1
           goto 9999
         endif

      else
         call hassign(iunit,ifile,ierror)
         if (iunit.lt.0 .or. ierror.lt.0) then
           call x3d_error(isubname,'hassign bad file unit')
           ierror = -1
           goto 9999
         endif
         close (iunit)
         open (iunit,file=ifile,form='unformatted')
      endif
c
c  write out global variables
c
      call dump_globals_lg(iunit,iomode,ierror)
c
c  dump out geometries
      call dump_geometries_lg(iunit,iomode,ierror)
c
c  if -all- requested write out all mesh objects
c  otherwise write out requested mesh objects
c  first write out geometry info - then write mesh object by attribute
c
      partname='define_cmo_lg'
      call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
      if(cmoin(1:5).eq.'-all-') then
      if (iomode(1:5).eq.'ascii') then
            write(iunit,10) number_of_mesh_objects,
     *   number_of_params_per_att,
     *   current_cmo_index,number_of_default_attributes,
     *   number_of_default_attparam_name
 10         format (10i10)
      else
            write(iunit)number_of_mesh_objects,
     *   number_of_params_per_att,
     *   current_cmo_index,number_of_default_attributes,
     *   number_of_default_attparam_name
      endif
C
         do i=2,number_of_mesh_objects
 
C....       NAME Field.
C
            cmo_name_sb=cmo_names(i)
            if(cmo_name_sb(1:5).ne.'-default-') then
C
               if(iomode(1:5).eq.'ascii') then
                  write(iunit,9000) cmo_name_sb
               else
                  write(iunit) cmo_name_sb
               endif
 9000          format(a32)
               call cmo_dump_cmo(cmo_name_sb,iunit,iomode,ierror)
               if(ierror.ne.0) go to 9999
            endif
         enddo
      else
         if(iomode(1:5).eq.'ascii') then
         i=1
            write(iunit,10) i,
     *   number_of_params_per_att,
     *   i,number_of_default_attributes,
     *   number_of_default_attparam_name
            write(iunit,9000) cmoin
         else
            write(iunit)i,
     *   number_of_params_per_att,
     *   i,number_of_default_attributes,
     *   number_of_default_attparam_name
            write(iunit) cmoin
         endif
         call cmo_dump_cmo(cmoin,iunit,iomode,ierror)
      endif
c
 9999 continue
      if (iunit.gt.0) close(iunit)

      return
      end
