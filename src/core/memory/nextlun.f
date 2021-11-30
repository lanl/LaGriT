*dk,nextlun
      function nextlun()
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS FUNCTION PICKS THE NEXT FREE LOGICAL UNIT NUMBER.
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C     OUTPUT ARGUMENTS -
C
C        nextlun - THE NEXT LOGICAL UNIT NUMBER.
C
C     CHANGE HISTORY -
C
C        $Log: nextlun.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:43:44   pvcs
CPVCS    Original version.
C
C#######################################################################
C

c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
      integer*4 iunit,kunit,nextlun
      logical opend
C
C#######################################################################
C
      do 100 iunit=1,99
         if(iunit.ge.5.and.iunit.le.7) goto 100
         inquire(iunit,opened=opend,err=100)
         if(opend.eqv..false.) then
            kunit=iunit
            goto 200
         endif
 100  continue
 200  continue
      nextlun=kunit
      goto 9999
 9999 continue
      return
      end
