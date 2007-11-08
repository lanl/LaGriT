      subroutine dump_globals_lg(iunit,iomode,ierror)
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
c        iomode  - ascii or binary
C
C     OUTPUT ARGUMENTS -
C
C        ierror  - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/dump_globals_lg_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Feb 07 14:08:58 2000   dcg
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
      character*32 iomode
C
      integer ierror
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, len,  icscode, numint,numreal,numchar,j
      character*32 partname
C
      character*132 logmess

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
c... write number of globals, names, types and indices
c
      if(iomode.eq.'ascii') then
         write(iunit,10) number_of_globals
      else
         write(iunit) number_of_globals
      endif
      if(iomode.eq.'ascii') then
         write(iunit,40) (global_name(i),i=1,number_of_globals)
         write(iunit,20) (global_type(i),i=1,number_of_globals)
         write(iunit,20) (global_index(i),i=1,number_of_globals)
      else
         write(iunit) (global_name(i),global_type(i),
     *     global_index(i),i=1,number_of_globals)
      endif
 10   format(i10)
 20   format(10i10)
c
c... write out global values and defaults
c... first count up number of each
c
      numint=0
      numreal=0
      numchar=0
      do i=1,number_of_globals
         if(1.eq.global_type(i)) numint=numint+1
         if(2.eq.global_type(i)) numreal=numreal+1
         if(3.eq.global_type(i)) numchar=numchar+1
      enddo
      if(iomode.eq.'ascii') then
         write(iunit,20) numint,numreal,numchar
      else
         write(iunit)  numint,numreal,numchar
      endif
      if(numint.ne.0) then
         if(iomode.eq.'ascii') then
            write(iunit,20) (global_integer(1,j),
     *          global_integer(2,j),j=1,numint)
         else
            write(iunit)   (global_integer(1,j),
     *          global_integer(2,j),j=1,numint)
         endif
      endif
      if(numreal.ne.0) then
         if(iomode.eq.'ascii') then
            write(iunit,30) (global_real(1,j),
     *          global_real(2,j),j=1,numreal)
         else
            write(iunit)   (global_real(1,j),
     *          global_real(2,j),j=1,numreal)
         endif
      endif
      if(numchar.ne.0) then
         if(iomode.eq.'ascii') then
            write(iunit,40) (global_character(1,j),
     *          global_character(2,j),j=1,numchar)
         else
            write(iunit)   (global_character(1,j),
     *          global_character(2,j),j=1,numchar)
         endif
      endif
 30   format(5f22.14)
 40   format(4a32)
      ierror=0
 9999 return
      end
