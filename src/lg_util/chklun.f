*dk,chklun
      subroutine chklun(ifile, iflaga, ignore)
C       
C#######################################################################
C       
C      PURPOSE -
C       
C         THIS ROUTINE CHECK TO SEE IF A FILE HAS BEEN OPENED AND
C              ASSIGN A LOGICAL UNIT NUMBER.
C       
C      INPUT ARGUMENTS -
C       
C         ifile    - THE FILE NAME.
C         IGNORE   - IGNORE THIS FIELD.
C       
C      OUTPUT ARGUMENTS -       
C       
C         iflaga - INDICATES IF THE FILE (OR UNIT) NUMBER IS OPEN.
C                  = 0 ==> FILE IS NOT OPEN.
C                  = 1 ==> FILE IS OPEN.
C       
C      CHANGE HISTORY - 
C       
C         $Log: chklun.f,v $
C         Revision 2.00  2007/11/03 00:49:10  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   03/17/95 19:33:42   het
CPVCS    Original version converted from the CRAY
C       
C#######################################################################
C       
      character ifile*(*)
      integer iflaga, ignore
      logical opend

c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
      integer*4 iunit

      pointer (ipival, ival(1))
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
         iunit=ival(1)
         inquire(unit=iunit,opened=opend,err=9999)
      else
         inquire(file=ifile(1:lenmax),opened=opend,err=9999)
      endif
      if(opend.eqv..false.) then
         iflaga=0
      else
         iflaga=1
      endif
      goto 9999
 9999 continue
      return
      end
