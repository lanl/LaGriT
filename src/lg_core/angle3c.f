*dk,angle3c
      subroutine angle3c(xxi,yyi,zzi,xxj,yyj,zzj,                       angle3c2
     *                   sinph,cosph,sinth,costh)                       angle3c3
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
C        $Log: angle3c.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:38:58 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:10:52   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
      implicit real*8 (a-h,o-z)
ccht                                                                    angle3c4
ccht                                                                    angle3c5
ccht  this routine calculates the rotation angles needed to rotate      angle3c6
ccht  the x-axis from i to j and the from here                          angle3c7
ccht  to rotate the z-axis from i to j.                                 angle3c8
ccht                                                                    angle3c9
ccht determine the angles of rotation needed to point the               angle310
ccht z-axis from mass point 'i' to neighbor 'j'.                        angle311
ccht                                                                    angle312
ccht find the cosine and sine of the angle with reference to the z-axis angle313
ccht                                                                    angle314
      xnoise=1.0e-10                                                    angle315
      dsxysq=(xxj-xxi)**2 + (yyj-yyi)**2                                angle316
      dsxy=sqrt(dsxysq)                                                 angle317
      ds=sqrt(dsxysq+(zzj-zzi)**2)                                      angle318
      dsiv=1.0/ds                                                       angle319
      costh=(zzj-zzi)*dsiv                                              angle320
      sinth=dsxy*dsiv                                                   angle321
ccht                                                                    angle322
ccht find the cosine and sine of the angle with reference to the x-axis angle323
ccht                                                                    angle324
      if(dsxy.lt.ds*xnoise) then
         cosph=0.0
         sinph=1.0
      else
         cosph=(xxj-xxi)/dsxy
         sinph=(yyj-yyi)/dsxy
      endif
      goto 9999                                                         angle328
9999  continue                                                          angle329
      return                                                            angle330
      end                                                               angle331
