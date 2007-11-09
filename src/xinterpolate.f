*dk,xinterpolate
      function xinterpolate(ioption,itype,xvalue)
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
C        $Log: xinterpolate.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Thu Nov 04 16:46:50 1999   dcg
CPVCS    fix error return to not change input
CPVCS
CPVCS       Rev 1.3   Thu Nov 04 16:39:48 1999   dcg
CPVCS    remove call to termgen
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 17:06:12 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   01/04/95 22:06:42   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/13/94 11:44:48   pvcs
CPVCS    Orginal Version
C
C ######################################################################
C
C
      implicit real*8 (a-h,o-z)
C
C        itype    - TYPE OF FUNCTION TO APPLY TO FIELD
C                   1 -- LINEAR
C                   2 -- LOG
C                   3 -- ASINH
C
C     STATEMENT FUNCTION FOR ASINH SINCE IT IS NOT AN INTRINSIC
C     (The ibm seems to not be able to handle statement functions)
C
C
C
      if(ioption.eq.1) then
         if(itype.eq.1) then
            yvalue=xvalue
         elseif(itype.eq.2) then
            xsmall=1.0d-30
            yvalue=log(abs(max(xsmall,abs(xvalue))))
         elseif(itype.eq.3) then
            xone=1.0d+00
            yvalue = sign(log(abs(xvalue)+
     *                    sqrt(xvalue**2+xone)),xvalue)
            yvalue1 = xvalue+sqrt(xvalue*xvalue+xone)
C*****      yvalue = log(xvalue+sqrt(xvalue*xvalue+xone))
         else
            write(logmess,9000) interp
            call writloga('default',0,logmess,0,ierrw)
            yvalue=xvalue
         endif
      elseif(ioption.eq.2) then
         if(itype.eq.1) then
            yvalue = xvalue
         elseif(itype.eq.2) then
            yvalue = exp(xvalue)
         elseif(itype.eq.3) then
            yvalue = sinh(xvalue)
         else
            write(logmess,9000) interp
            call writloga('default',0,logmess,0,ierrw)
            yvalue = xvalue
         endif
      else
         write(logmess,9000) interp
 9000    format("Incorrect interpolation scheme: ",i10)
         call writloga('default',0,logmess,0,ierrw)
         xinterpolate=xvalue
         goto 9999
      endif
      xinterpolate=yvalue
      goto 9999
 9999 continue
      return
      end
