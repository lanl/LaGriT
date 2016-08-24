*dk,kmprsm
      subroutine kmprsm(n,z,iz,x,ix,y,iy,count)
C 
C                                                                      
C ##################################################################### 
C                                                                       
C     PURPOSE -                                                         
C                                                                       
C        Compress a vector x into vector y where z is less than zero.                                                           
C                                                                       
C     INPUT ARGUMENTS -                                                 
C                                                                       
C        n   - (integer) the length of the input vector.
C        z   - (integer) the mask vector.
C        iz  - (integer) the stride in z.
C        x   - (integer) the source vector to be compressed.
C        ix  - (integer) the stride in x. 
C        iy  - (integer) the stride in y.                                                          
C                                                                       
C     OUTPUT ARGUMENTS -                                                
C                                                                       
C        y     - (integer) the compressed output vector.
C        count - (integer) the number of elements in y.                                                          
C                                                                       
C     CHANGE HISTORY -                                                  
C                                                                       
C        $Log: kmprsm.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   03/16/95 08:02:54   ejl
CPVCS    Cleaned up, Implicit none.
CPVCS    
CPVCS       Rev 1.1   01/04/95 21:55:52   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:42:44   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
      implicit none
C                                                                       
C ######################################################################
C
      integer n, iz, ix, iy, count
      integer z(*), x(*), y(*)
C                                                                       
C ######################################################################
C
      integer imax, i, jx, jy
C
C ######################################################################
C
C
C
      imax=iz*n
C
      jx=1
      jy=1
C
      count=0
C
      do i=1,imax,iz
C
         if(z(i).lt.0) then
            count=count+1
            y(jy)=x(jx)
            jy=jy+iy
         endif
C
         jx=jx+ix
C
      enddo
C
      return
      end
