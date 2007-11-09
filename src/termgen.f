*dk,termgen
      subroutine termgen()
C 
C                                                                      
C ##################################################################### 
C                                                                       
C     PURPOSE -                                                         
C                                                                       
C        None                                                           
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
C        $Log: termgen.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:04:32 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   05/19/95 13:50:48   ejl
CPVCS    Implicit none.
CPVCS    
CPVCS       Rev 1.2   03/17/95 21:12:42   het
CPVCS    Add the model and dictionary calles
CPVCS    
CPVCS       Rev 1.1   01/04/95 22:06:28   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.0   11/13/94 11:44:52   pvcs
CPVCS    Orginal Version
C                                                                       
C ######################################################################
C                      
      implicit none
C
C ######################################################################
C
      integer ierr
C
      character*132 logmess
C
C ######################################################################
C
      write(logmess,'(a)') "Generator failed in termgen"
      call writloga('default',2,logmess,2,ierr)
C
      stop
      end
