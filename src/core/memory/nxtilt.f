*dk,nxtilt
      function nxtilt(array,number,target,stride)
C
C ######################################################################
C
C        $Log: nxtilt.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
       implicit real*8 (a-h,o-z)
      integer number,target,stride
      integer array(number)
      if(number.le.0) then
         nxtilt=0
         goto 9999
      endif
      nxtilt=number+1
      do i=1,number,stride
         if(array(i).lt.target) then
            nxtilt=i
            goto 9999
         endif
      enddo
      goto 9999
 9999 continue
      return
      end
