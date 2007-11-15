*dk, packsi
       subroutine packsi( ncoefs, nrows, ISINK, ISOURCE, MASK)
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
C        $Log: packsi.f,v $
C        Revision 2.00  2007/11/05 19:46:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:56:34 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:16:58   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
C$$$
C      Pack ISOURCE where MASK is .true. into ISINK
C$$$
       dimension ISINK(nrows), ISOURCE(ncoefs)
       logical MASK(ncoefs)
 
       i=1
       do 10 j=1,ncoefs
 
          if (MASK(j) .eqv. .true.) then
             ISINK(i) = ISOURCE(j)
            i = i + 1
          endif
 
 10    continue
       return
       end
