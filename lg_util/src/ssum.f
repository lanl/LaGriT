*dk,ssum
      function ssum(n,x,ix)
C
C ######################################################################
C
C        $Log: ssum.f,v $
C        Revision 2.00  2007/11/03 00:49:13  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
       implicit real*8 (a-h,o-z)
      dimension x(n)
      ssum=0.0
      do i=1,n,ix
         ssum=ssum+x(i)
      enddo
      goto 9999
 9999 continue
      return
      end
