c     **************************************************************
c     IMPORTANT NOTE:
c     File preprocessing is performed below in the event that the 
c     user does not have ExodusII built.
c 
c     Note that this file *must* be compiled with the -cpp flag to enable 
c     pre-processing.
c     Note also that this is not necessarily a portable solution to 
c     other compilers, i.e. ifort.
c
c     USAGE:
c
c     To enable ExodusII output:
c        gfortran $(FFLAGS) -cpp -o dumpexodusII.o dumpexodusII.f
c
c     To disable ExodusII output:
c        gfortran $(FFLAGS) -cpp -DNOEXODUS -o dumpexodusII.o dumpexodusII.f
c
c     For more information, see:
c        https://stackoverflow.com/a/41234703/5150303
c     **************************************************************

c     **************************************************************
c     code substitution when not using ExodusII libraries

      subroutine dumpexodusII(
     >               imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)

      implicit none

      include 'lagrit.h'

      integer nwds
      integer ierror, ierr
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

      character*132 logmess

      write(logmess,'(a)')'ExodusII exiting: '
      call writloga('default',1,logmess,0,ierr)
      write(logmess,'(a)')'Not available in this version.'
      call writloga('default',0,logmess,2,ierr)
      
      return
      end