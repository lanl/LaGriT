*dk,xsumspi
      subroutine xsumspi(nsink,nsource,result,index,value)
C                                                                       
C ##################################################################### 
C                                                                       
C     PURPOSE -                                                         
C                                                                       
C        None                                                           
C                                                                       
C     INPUT ARGUMENTS -                                                 
C                                                                       
C        None                                                           
C                                                                       
C     OUTPUT ARGUMENTS -                                                
C                                                                       
C        None                                                           
C                                                                       
C     CHANGE HISTORY -                                                  
C                                                                       
C        $Log: xsumspi.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 17:06:14 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:20:38   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
      integer result(nsink)
      integer index(nsource)
      integer value(nsource)
CMF$ LAYOUT result(:news)
CMF$ LAYOUT index(:news)
CMF$ LAYOUT value(:news)
C
       do i = 1, nsource
          result(index(i)) = result(index(i))+value(i)
       enddo
       return
       end
