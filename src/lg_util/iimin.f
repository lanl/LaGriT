*dk,iimin
      function iimin(n,ix,incx)
C
C ######################################################################
C
C        $Log: iimin.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
       implicit real*8 (a-h,o-z)
      integer n, incx, ix(n)
      iimin=-1
      i1=ix(1)
      do i=1,n,incx
         i1=min(i1,ix(i))
      enddo
      icount=0
      do i=1,n,incx
         icount=icount+1
         if(ix(i).eq.i1) then
            iimin=icount
            goto 9999
         endif
      enddo
      goto 9999
 9999 continue
      return
      end
