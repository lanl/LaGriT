*DK eullag3
      subroutine eullag3(itp,xxi,yyi,zzi,xxj,yyj,zzj,xix,xiy,xiz,
     * xoa,xob,xoc)
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
C        $Log:   /pvcs.config/t3d/src/eullag3.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:45:00 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:11:42   pvcs
CPVCS    Original version.
C                                                                       
C ######################################################################
C                      
       implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
ccht
ccht
ccht
ccht  this routine converts between two geometric reference frames
ccht  given a point i(xxi,yyi,zzi) and a nearest neighbor j(xxj,yyj,zzj)
ccht  along with a vector (xix,xiy,xiz) do a rotation of this
ccht  vector form x,y,z space to a,b,c space.
ccht      itp = 1 ==> x,y,z to a,b,c
ccht      itp= 2 ==> a,b,c to x,y,z
ccht
ccht
ccht
      call angle3(xxi,yyi,zzi,xxj,yyj,zzj,sinph,cosph,sinth,costh)
      goto (100,200) itp
100   continue
      a11=cosph
      a12=sinph
      a13=0.0
      a21=-costh*sinph
      a22=costh*cosph
      a23=sinth
      a31=sinth*sinph
      a32=-sinth*cosph
      a33=costh
      xoa=a11*xix+a12*xiy+a13*xiz
      xob=a21*xix+a22*xiy+a23*xiz
      xoc=a31*xix+a32*xiy+a33*xiz
      goto 9999
200   continue
      ai11=cosph
      ai12=-sinph*costh
      ai13=sinph*sinth
      ai21=sinph
      ai22=cosph*costh
      ai23=-cosph*sinth
      ai31=0.0
      ai32=sinth
      ai33=costh
      xoa=ai11*xix+ai12*xiy+ai13*xiz
      xob=ai21*xix+ai22*xiy+ai23*xiz
      xoc=ai31*xix+ai32*xiy+ai33*xiz
      goto 9999
9999  continue
      return
      end
