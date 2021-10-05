*dk,fexist
      subroutine fexist(ifile, iflaga)
C       
C#######################################################################
C       
C      PURPOSE -
C       
C         THIS ROUTINE CHECKS THE EXISTENCE OF A FILE.
C       
C      INPUT ARGUMENTS -
C       
C         ifile    - THE FILE NAME.
C       
C      OUTPUT ARGUMENTS -       
C       
C         iflaga - = 0 ==> FILE DOES NOT EXIST.
C                  = 1 ==> FILE DOES EXIST.
C       
C      CHANGE HISTORY - 
C       
C         $Log: fexist.f,v $
C         Revision 2.00  2007/11/03 00:49:10  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   03/17/95 19:34:50   het
CPVCS    Original version converted from the CRAY
C       
C#######################################################################
C       

      character ifile*(*)
      logical iexist
      pointer (ipival, ival(1))

c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
      integer*4 iunit
       
C
C#######################################################################
C
      iflaga=0
      ierror=-1
      if(len(ifile).eq.0) then
         lenmax=0
C        *** IF THIS HAPPENS, THIS IS "PROABALY" AN INTEGER FIELD WITH A
C               WITH A CHARACTER CONTAINED WITHIN, MAKE AN ASSUMPTION.
      else
         lenmax=icharlnf(ifile)
      endif
      iexist=.false.
      if(lenmax.eq.0) then
         ipival=loc(ifile)
         iunit=ival(1)
         inquire(unit=iunit,exist=iexist,err=9999)
      else
         inquire(file=ifile(1:lenmax),exist=iexist,err=9999)
      endif
      if(iexist .eqv. .true.) then
         iflaga=1
      elseif(iexist .eqv. .false.) then
         iflaga=0
      endif
      goto 9999
 9999 continue
      return
      end
