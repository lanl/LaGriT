      subroutine close_lg (ifile)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE CLOSES A FILE THAT HAS BEEN PREVIOUSLY OPENDED
C            FOR I/O.
C
C      INPUT ARGUMENTS -
C
C         ifile    - THE FILE NAME.
C
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C      CHANGE HISTORY -
C
C         $Log: close.f,v $
C         Revision 2.00  2007/11/03 00:49:10  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   Wed Feb 11 16:21:12 1998   dcg
CPVCS    don
CPVCS    skip inquire if file not opened
CPVCS
CPVCS       Rev 1.5   Wed Feb 11 15:39:32 1998   dcg
CPVCS    replace name of subroutine close with close_lg
CPVCS    this removes conflicts with some standard librarier
CPVCS
CPVCS       Rev 1.4   Thu Dec 18 10:24:16 1997   dcg
CPVCS    allow for 64 bit addresses by include rdwrt.h
CPVCS    use integer*8 version for 64 bit addresses
CPVCS    and link with 64 bit version of io package
CPVCS
CPVCS       Rev 1.3   10/20/95 13:07:52   dcg
CPVCS    check for existence
CPVCS
CPVCS       Rev 1.2   10/18/95 12:12:54   het
CPVCS    Add the dummy Fortran file/unit by preappending an F
CPVCS
CPVCS       Rev 1.1   07/14/95 10:11:32   het
CPVCS    Correct some errors for writing restart dumps
CPVCS
CPVCS       Rev 1.0   03/17/95 19:33:44   het
CPVCS    Original version converted from the CRAY
C
C#######################################################################
C
      implicit none

c     Remove use of include rdwrt.h
c     The I/O library does not have entry points for handling 
c     type INTEGER*8 I/O control list specifiers
      integer*4 iunitrw,iaddress,ierrrw

      integer iflaga, ierror,lenmax
      character ifile*(*)
      logical opend
      pointer (ipival, ival)
      integer ival(10000000)
      character*132 filename
      integer icharlnf
C
C#######################################################################
C
      iflaga=-1
      ierror=-1
      if(len(ifile).eq.0) then
         lenmax=0
C        *** IF THIS HAPPENS, THIS IS "PROABALY" AN INTEGER FIELD WITH A
C               WITH A CHARACTER CONTAINED WITHIN, MAKE AN ASSUMPTION.
      else
         lenmax=icharlnf(ifile)
      endif
      if(lenmax.eq.0) then
         ipival=loc(ifile)
         iunitrw=ival(1)
         inquire(unit=iunitrw,opened=opend,err=9999)
      else
          inquire(file=ifile(1:lenmax),
     *           opened=opend,number=iunitrw,err=9999)
      endif
      if(opend.eqv. .true.) then
         close(iunitrw)
         call cclose(iunitrw,ierrrw)
      else
c        inquire(unit=iunitrw,name=filename,err=9999)
C        print *,"File already closed: ",iunitrw,filename
      endif
      goto 9999
 9999 continue
      return
      end
*dk,closef
      subroutine closef(ifile)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE CLOSES A FILE THAT HAS BEEN PREVIOUSLY OPENDED
C            FOR I/O.
C
C      INPUT ARGUMENTS -
C
C         ifile    - THE FILE NAME.
C
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C      CHANGE HISTORY -
C
C         $Log: close.f,v $
C         Revision 2.00  2007/11/03 00:49:10  spchu
C         Import to CVS
C
CPVCS
CPVCS       Rev 1.1   07/14/95 10:11:32   het
CPVCS    Correct some errors for writing restart dumps
CPVCS
CPVCS       Rev 1.0   03/17/95 19:33:44   het
CPVCS    Original version converted from the CRAY
C
C#######################################################################
C
      implicit none

c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
      integer*4 iunitrw,iaddress,ierrrw

      integer iflaga, ierror,lenmax,iflag
      character ifile*(*)
      logical opend
      pointer (ipival, ival)
      integer ival(10000000)
      integer icharlnf
      character*132 filename, filename_fortran
C
C#######################################################################
C
      iflaga=-1
      ierror=-1
      if(len(ifile).eq.0) then
         lenmax=0
C        *** IF THIS HAPPENS, THIS IS "PROABALY" AN INTEGER FIELD WITH A
C               WITH A CHARACTER CONTAINED WITHIN, MAKE AN ASSUMPTION.
      else
         lenmax=icharlnf(ifile)
      endif
      if(lenmax.eq.0) then
         ipival=loc(ifile)
         iunitrw=ival(1)
         inquire(unit=iunitrw,opened=opend,err=9999)
      else
          filename_fortran='F' // ifile(1:lenmax)
          call fexist(filename_fortran,iflag)
          if (iflag.eq.0) then
             print *,"File does not exist: ",filename_fortran
             go to 9999
          endif
          lenmax=lenmax+1
          inquire(file=filename_fortran(1:lenmax),
     *           opened=opend,number=iunitrw,err=9999)
      endif
      if(opend.eqv. .true.) then
         close(iunitrw)
         call cclose(iunitrw,ierrrw)
      else
         inquire(unit=iunitrw,name=filename)
         print *,"File already closed: ",iunitrw,filename
      endif
      goto 9999
 9999 continue
      return
      end
