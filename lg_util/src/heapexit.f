*dk,heapexit
      subroutine heapexit()
C 
C                                                                      
C ########################################################################
C                                                                       
C     PURPOSE -                                                         
C                                                                       
C        Stops code for memory management error.
C                                                                       
C     INPUT ARGUMENTS -                                                 
C                                                                       
C        NONE                                                           
C                                                                       
C     OUTPUT ARGUMENTS -                                                
C                                                                       
C        NONE                                                           
C                                                                       
C     CHANGE HISTORY -                                                  
C                                                                       
C        $Log: heapexit.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   05/30/95 13:46:04   ejl
CPVCS    Cleaned up, Implicit none.
CPVCS    
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:41:54   pvcs
CPVCS    Original version.
C                                                                       
C ########################################################################
C
      implicit none
C
C ########################################################################
C                      
      print *, 'Code stopping with a memory management error'
      stop
C
      end
