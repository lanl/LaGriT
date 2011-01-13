      subroutine read_lagrit_old(ifile,cmoread,iomode,ierror_return)
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
C        $Log: read_lagrit_old.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Feb 2000 16:27:26   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.6   14 Feb 2000 15:18:28   dcg
CPVCS    fix check for binary or ascii
CPVCS
CPVCS       Rev 1.5   Thu Dec 16 10:05:26 1999   dcg
CPVCS    read and write global variables/ ihcycle, time, monitor
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
      character ifile*(*)
      character*(*) cmoread
      character*32 cmotemp,cout,isubname
      character*(*) iomode
      integer i,iunit,ierror,ierror_return,icscode,ihcycle,monitor,
     *  iout
      real*8 time,rout
      character*132 logmess
      character*8 chkstrg
      logical isglobal
C#######################################################################
C begin
      isubname = "read_lagrit_old"
      rout=0.0
      cout=' '
      iout=0
      ierror_return=0
      iunit=-1
      if(iomode(1:5).eq.'ascii') then
         call hassign(iunit,ifile,ierror)
         if (iunit.lt.0 .or. ierror.lt.0) then
         call x3d_error(isubname,'hassign bad file unit')
         ierror_return = -1
         goto 9999
         endif

      else
         call hassign(iunit,ifile,ierror)
         if (iunit.lt.0 .or. ierror.lt.0) then
         call x3d_error(isubname,'hassign bad file unit')
         ierror_return = -1
         goto 9999
         endif

         close (iunit)
         open (iunit,file=ifile,form='unformatted')
      endif
c
c read and store global variables
c
 3    isglobal=.false.
      if(iomode(1:5).eq.'ascii') then
         read(iunit,10,end=9999,err=9997) chkstrg,cmotemp,i
         if(chkstrg.eq.'global ') then
            read(iunit,'(i10,f22.14,i10)') ihcycle,time,monitor
            call set_global('ihcycle',ihcycle,rout,cout,1,icscode)
            call set_global('time',iout,time,cout,2,icscode)
            call set_global('monitor',monitor,rout,cout,1,icscode)
            isglobal=.true.
         endif
      else
         read(iunit)i,cmotemp
         if(cmotemp(1:6).eq.'global') then
            read(iunit)ihcycle,time,monitor
            call set_global('ihcycle',ihcycle,rout,cout,1,icscode)
            call set_global('time',iout,time,cout,2,icscode)
            call set_global('monitor',monitor,rout,cout,1,icscode)
            isglobal=.true.
         endif
      endif
c
c  read name of cmo
c
 5    if(iomode(1:5).eq.'ascii') then
         if(isglobal) then
           read(iunit,10,end=9999,err=9997)chkstrg,cmotemp,i
         else
           isglobal=.true.
         endif
         if(chkstrg.ne.'lagrit ') go to 9997
      else
         if(isglobal) then
            read(iunit,end=9999,err=9998) i,cmotemp
         else
            isglobal=.true.
         endif
      endif
 10   format(a7,a32,i3)
c  see if cmo exists - if not create it
 12   call cmo_exist(cmotemp,ierror)
      if(ierror.ne.0) then
          call cmo_create(cmotemp,ierror)
      endif
      call cmo_select(cmotemp,ierror)
C  read geometry information
      call read_lagrit_geom_old(iunit,cmotemp,iomode,ierror_return)
      call cmo_readdump_cmo_old(cmotemp,iunit,iomode,ierror_return)
      go to 5
c
c  got an error on ascii read try binary
c
 9997 write(logmess,"('ascii read failed - try binary')")
      call writloga('default',0,logmess,0,icscode)
      close (iunit)
      open (iunit,file=ifile,form='unformatted')
      iomode='binary'
      go to 3
 
c
c read error
c
 9998 write(logmess,"(' Error in reading lagrit dump')")
      call writloga('default',0,logmess,0,icscode)
 
c
c
 9999 continue
      if (iunit.gt.0) close(iunit)
      return
      end
