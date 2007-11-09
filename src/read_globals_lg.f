      subroutine read_globals_lg(iunit,ifile,iomode,ierror)
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine dumps global information to a file
C
C     INPUT ARGUMENTS -
C
C        iunit   - file number.
C        ifile   - file name
c        iomode  - ascii or binary
C
C     OUTPUT ARGUMENTS -
C
C        ierror  - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: read_globals_lg.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   05 May 2000 08:55:04   dcg
CPVCS    extend test for ascii/binary reads
CPVCS    
CPVCS       Rev 1.3   Mon Feb 14 17:43:22 2000   dcg
CPVCS    set ascii or binary automatically
CPVCS
CPVCS       Rev 1.2   07 Feb 2000 16:47:48   dcg
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'global_lg.h'
C
C#######################################################################
C
      integer  iunit
      character*(*) iomode,ifile
C
      integer ierror
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, len,  icscode, numint,numreal,numchar,j
      character*32 partname
      character*132 logmess
C
 
C
C#######################################################################
C
C.... access global variable data blocks
C
      partname='global_lg'
      call mmfindbk('global_name',partname,ipglobal_name,len,icscode)
      call mmfindbk('global_type',partname,ipglobal_type,len,icscode)
      call mmfindbk('global_index',partname,ipglobal_index,len,icscode)
      call mmfindbk('global_integer',partname,
     *            ipglobal_integer,len,icscode)
      call mmfindbk('global_real',partname,
     *            ipglobal_real,len,icscode)
      call mmfindbk('global_character',partname,
     *            ipglobal_character,len,icscode)
c
c... read number of globals, names, types and indices
c
 5    if(iomode.eq.'ascii') then
         read(iunit,10,err=9998) number_of_globals
      else
         read(iunit) number_of_globals
      endif
      if(iomode.eq.'ascii') then
         read(iunit,40,err=9998) (global_name(i),i=1,number_of_globals)
         read(iunit,20,err=9998) (global_type(i),i=1,number_of_globals)
         read(iunit,20,err=9998) (global_index(i),i=1,number_of_globals)
 
      else
         read(iunit) (global_name(i),global_type(i),
     *     global_index(i),i=1,number_of_globals)
 
      endif
 
 10   format(i10)
 20   format(10i10)
c
c... read global values and defaults
c... first count up number of each
c
 
      if(iomode.eq.'ascii') then
         read(iunit,20,err=9998) numint,numreal,numchar
      else
         read(iunit)  numint,numreal,numchar
      endif
      if(numint.ne.0) then
         if(iomode.eq.'ascii') then
            read(iunit,20,err=9998) (global_integer(1,j),
     *          global_integer(2,j),j=1,numint)
         else
            read(iunit)   (global_integer(1,j),
     *          global_integer(2,j),j=1,numint)
         endif
      endif
      if(numreal.ne.0) then
         if(iomode.eq.'ascii') then
            read(iunit,30,err=9998) (global_real(1,j),
     *          global_real(2,j),j=1,numreal)
         else
            read(iunit)   (global_real(1,j),
     *          global_real(2,j),j=1,numreal)
         endif
      endif
      if(numchar.ne.0) then
         if(iomode.eq.'ascii') then
            read(iunit,40,err=9998) (global_character(1,j),
     *          global_character(2,j),j=1,numchar)
         else
            read(iunit)   (global_character(1,j),
     *          global_character(2,j),j=1,numchar)
         endif
      endif
      ierror=0
      go to 9999
 9998 write(logmess,"('ascii read failed - will try binary')")
      call writloga('default',0,logmess,0,icscode)
      close (iunit)
      open (iunit,file=ifile,form='unformatted')
      iomode='binary'
      go to 5
 30   format(5f22.14)
 40   format(4a32)
 9999 return
      end
