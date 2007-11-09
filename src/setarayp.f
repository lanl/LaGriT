*dk,setarayp
      subroutine setarayp(npoints,iparray,ivalue)
c
       implicit real*8 (a-h,o-z)
c
c set the elements in the array to ivalue
c
      pointer ( iparray, iarray(npoints) )
c
      do 100 i1=1,npoints
         iarray(i1)=ivalue
 100  continue
      goto 9999
 9999 continue
      return
      end
