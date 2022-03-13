*dk,setarayp
      subroutine setarayp(npoints,iparray,ivalue)
C
C ######################################################################
C
C        $Log: setarayp.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
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
