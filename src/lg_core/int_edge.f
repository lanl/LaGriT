*dk,int_edge
      subroutine int_edge(x1,y1,z1,x2,y2,z2,x3,y3,z3,xi,yi,zi)
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
C        $Log: int_edge.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:51:54 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:15:26   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
      xsmall=1.0d-10
      cx=  (y2-y1)*(z3-z1)-(y3-y1)*(z2-z1)
      cy=-((x2-x1)*(z3-z1)-(x3-x1)*(z2-z1))
      cz=  (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)
      x1p=x1+cx
      y1p=y1+cy
      z1p=z1+cz
      call eullag3(1,x1,y1,z1,x1p,y1p,z1p,x1,y1,z1,a1,b1,c1)
      call eullag3(1,x1,y1,z1,x1p,y1p,z1p,x2,y2,z2,a2,b2,c2)
      call eullag3(1,x1,y1,z1,x1p,y1p,z1p,x3,y3,z3,a3,b3,c3)
      da32=a3-a2
      db32=b3-b2
      if(abs(da32).le.xsmall) then
         xa=a2
         ya=b1
         za=0.0
      elseif(abs(db32).le.xsmall) then
         xa=a1
         ya=b2
         za=0.0
      else
         c1=db32/da32
         c2=-1.0/c1
         d1=-c1*a2+b2
         d2=-c2*a1+b1
         xa=(d2-d1)/(c1-c2+xsmall)
         ya=(c1*d2-c2*d1)/(c1-c2+xsmall)
         za=0.0
      endif
      call eullag3(2,x1,y1,z1,x1p,y1p,z1p,xa,ya,za,xi,yi,zi)
      return
      end
