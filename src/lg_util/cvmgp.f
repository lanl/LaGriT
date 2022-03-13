*dk,cvmgp
      function cvmgp(i1,i2,value)
C
C ######################################################################
C
C        $Log: cvmgp.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
       implicit real*8 (a-h,o-z)
      integer i1,i2,value
      if(value.ge.0) then
         cvmgp=i1
      else
         cvmgp=i2
      endif
      goto 9999
 9999 continue
      return
      end
