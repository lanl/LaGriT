*dk,xsumsp2r
      subroutine xsumsp2r(nsink,nsource,result,index,value)
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
C        $Log: xsumsp2r.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:06:14 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Thu Aug 22 17:13:04 1996   dpk
CPVCS    Added if test for zero index
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:20:38   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
      dimension result(nsink)
      integer index(nsource)
      dimension value(nsource)
CMF$ LAYOUT result(:news)
CMF$ LAYOUT index(:news)
CMF$ LAYOUT value(:news)
C
       do i = 1, nsource
          if(index(i).gt.0) then
            result(index(i)) = result(index(i))+value(i)
          endif
       enddo
       return
       end
