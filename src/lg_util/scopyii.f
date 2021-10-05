*dk,scopyii
      subroutine scopyii(n,isource,isource_stride,isink,isink_stride)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE WRITES A DUMP FILE FOR CHAD.
C
C     INPUT ARGUMENTS -
C
C        n              - NUMBER OF ELEMENTS TO COPY.
C        isource        - INTEGER SOURCE ARRAY
C        isource_stride - INTEGER SOURCE ARRAY STRIDE
C        isink_stride   - INTEGER SINK ARRAY STRIDE
C
C     OUTPUT ARGUMENTS  -
C
C        isink          - INTEGER SINK ARRAY
C
C
C     CHANGE HISTORY -
C
C        $Log: scopyii.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   01/17/95 16:42:04   pvcs
CPVCS    Original Version
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
C ######################################################################
C
      dimension isource(n), isink(n)
C
C ######################################################################
C
      i1=1
      i2=1
      do i=1,n
         isink(i2)=isource(i1)
         i1=i1+isource_stride
         i2=i2+isink_stride
      enddo
      goto 9999
 9999 continue
      return
      end
