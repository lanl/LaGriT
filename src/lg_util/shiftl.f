*dk,shiftl
      integer function shiftl(num,i)
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
C        $Log: shiftl.f,v $
C        Revision 2.00  2007/11/03 00:49:13  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:45:46   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
      shiftl=ishft(num,i)
      return
      end
