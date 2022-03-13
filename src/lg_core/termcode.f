*dk,termcode
      subroutine termcode()
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
C        $Log: termcode.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:04:32 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   01/04/95 22:06:26   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.0   11/13/94 11:44:50   pvcs
CPVCS    Orginal Version
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
      idebug=0
      if(idebug.eq.1) print *,"In stub routine: termcode"
      stop
      end
