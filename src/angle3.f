*DK angle3
      subroutine angle3(xxi,yyi,zzi,xxj,yyj,zzj,sinph,cosph,sinth,costh)
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
C        $Log: angle3.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 13:34:02 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:38:58 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:10:50   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
C
ccht
ccht
ccht  this routine calculates the rotation angles needed to rotate
ccht  the x-axis from i to j and the from here
ccht  to rotate the z-axis from i to j.
ccht
ccht determine the angles of rotation needed to point the
ccht z-axis from mass point 'i' to neighbor 'j'.
ccht
ccht find the cosine and sine of the angle with reference to the z-axis
ccht
C
C     $Log: angle3.f,v $
C     Revision 2.00  2007/11/05 19:45:46  spchu
C     Import to CVS
C
C
      xnoise=1.0e-10
      dsxysq=(xxj-xxi)**2 + (yyj-yyi)**2
      dsxy=sqrt(dsxysq)
      ds=sqrt(dsxysq+(zzj-zzi)**2)
      dsiv=1.0/ds
      costh=(zzj-zzi)*dsiv
      sinth=dsxy*dsiv
ccht
ccht find the cosine and sine of the angle with reference to the x-axis
ccht
C****      zonly=cvmgt(1.0,dsxy,dsxy.lt.ds*xnoise)
C****      cosph=cvmgt(0.0,-(yyj-yyi)/zonly,dsxy.lt.ds*xnoise)
C****      sinph=cvmgt(1.0,(xxj-xxi)/zonly,dsxy.lt.ds*xnoise)
      if(dsxy.lt.ds*xnoise) then
         cosph=0.0
         sinph=1.0
      else
         cosph=-(yyj-yyi)/dsxy
         sinph= (xxj-xxi)/dsxy
      endif
      goto 9999
9999  continue
      return
      end
