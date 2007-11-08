*DK occonv
      subroutine occonv(itype,icount,xn,i4,n,
     *                  xic,yic,zic)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE CONVERTS FROM THE ICOSAHEDRAL VERTEX
C     GRID TO THE CELL CENTERED GRID.
C
C      NOTE:  THESE COORDINATES ARE NOT NORMALIZED TO THE UNIT SPHERE
C
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        HT0823AA-87, HT0908AA-87
C
C
C        $Log: occonv.f,v $
C        Revision 2.00  2007/11/05 19:46:02  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C#######################################################################
C
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
C#######################################################################
C
      real xn(0:n+1,0:n+1,3)
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C
C#######################################################################
C
      goto 9998
      do 100 i1=0,n-1
      do 110 j1=1,n
      icount=icount+1
      xic(icount)=0.25*(xn(i1,j1,1)+xn(i1,j1+1,1)+xn(i1+1,j1,1)+
     *  xn(i1+1,j1+1,1))
      yic(icount)=0.25*(xn(i1,j1,2)+xn(i1,j1+1,2)+xn(i1+1,j1,2)+
     *  xn(i1+1,j1+1,2))
      zic(icount)=0.25*(xn(i1,j1,3)+xn(i1,j1+1,3)+xn(i1+1,j1,3)+
     *  xn(i1+1,j1+1,3))
 110  continue
 100  continue
      goto 9999
 9998 continue
      xfac=0.05
      if(itype.gt.0) then
         do 200 i1=0,n
            do 210 j1=1,n+1
               icount=icount+1
               xic(icount)=xn(i1,j1,1)
               yic(icount)=xn(i1,j1,2)
               zic(icount)=xn(i1,j1,3)
 210        continue
 200     continue
      elseif(itype.eq.0) then
         do 300 i1=0,(n-1)
            do 310 j1=1,n
               x1=xn(i1,j1,1)
               y1=xn(i1,j1,2)
               z1=xn(i1,j1,3)
               x2=xn(i1+1,j1,1)
               y2=xn(i1+1,j1,2)
               z2=xn(i1+1,j1,3)
               x3=xn(i1,j1+1,1)
               y3=xn(i1,j1+1,2)
               z3=xn(i1,j1+1,3)
               if(i1.eq.0.or.j1.eq.1) then
                  if(i1.eq.0) then
                     xc123=0.25*x1+0.75*x3
                     yc123=0.25*y1+0.75*y3
                     zc123=0.25*z1+0.75*z3
                     icount=icount+1
                     xic(icount)=xc123
                     yic(icount)=yc123
                     zic(icount)=zc123
                  endif
                  if(j1.eq.1) then
                     xc123=0.25*x1+0.75*x2
                     yc123=0.25*y1+0.75*y2
                     zc123=0.25*z1+0.75*z2
                     icount=icount+1
                     xic(icount)=xc123
                     yic(icount)=yc123
                     zic(icount)=zc123
                  endif
               endif
               xa=x1
               ya=y1
               za=z1
               xb=x2-xa
               yb=y2-ya
               zb=z2-za
               xc=x3-xa
               yc=y3-ya
               zc=z3-za
               x1n=crosx(xb,yb,zb,xc,yc,zc)
               y1n=crosy(xb,yb,zb,xc,yc,zc)
               z1n=crosz(xb,yb,zb,xc,yc,zc)
               x2n=crosx(x1n,y1n,z1n,xb,yb,zb)
               y2n=crosy(x1n,y1n,z1n,xb,yb,zb)
               z2n=crosz(x1n,y1n,z1n,xb,yb,zb)
               dot2c=x2n*xc+y2n*yc+z2n*zc+1.0e-30
               q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/dot2c
               xc123=q*x2n+0.5*xb+xa
               yc123=q*y2n+0.5*yb+ya
               zc123=q*z2n+0.5*zb+za
               icount=icount+1
               xic(icount)=xc123
               yic(icount)=yc123
               zic(icount)=zc123
               x1=xn(i1+1,j1+1,1)
               y1=xn(i1+1,j1+1,2)
               z1=xn(i1+1,j1+1,3)
               x2=xn(i1+1,j1,1)
               y2=xn(i1+1,j1,2)
               z2=xn(i1+1,j1,3)
               x3=xn(i1,j1+1,1)
               y3=xn(i1,j1+1,2)
               z3=xn(i1,j1+1,3)
               if(i1.eq.(n-1).or.j1.eq.n) then
                  if(i1.eq.(n-1)) then
                     xc123=0.25*x1+0.75*x2
                     yc123=0.25*y1+0.75*y2
                     zc123=0.25*z1+0.75*z2
                     icount=icount+1
                     xic(icount)=xc123
                     yic(icount)=yc123
                     zic(icount)=zc123
                  endif
                  if(j1.eq.n) then
                     xc123=0.25*x1+0.75*x3
                     yc123=0.25*y1+0.75*y3
                     zc123=0.25*z1+0.75*z3
                     icount=icount+1
                     xic(icount)=xc123
                     yic(icount)=yc123
                     zic(icount)=zc123
                  endif
               endif
               xa=x1
               ya=y1
               za=z1
               xb=x2-xa
               yb=y2-ya
               zb=z2-za
               xc=x3-xa
               yc=y3-ya
               zc=z3-za
               x1n=crosx(xb,yb,zb,xc,yc,zc)
               y1n=crosy(xb,yb,zb,xc,yc,zc)
               z1n=crosz(xb,yb,zb,xc,yc,zc)
               x2n=crosx(x1n,y1n,z1n,xb,yb,zb)
               y2n=crosy(x1n,y1n,z1n,xb,yb,zb)
               z2n=crosz(x1n,y1n,z1n,xb,yb,zb)
               dot2c=x2n*xc+y2n*yc+z2n*zc+1.0e-30
               q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/dot2c
               xc123=q*x2n+0.5*xb+xa
               yc123=q*y2n+0.5*yb+ya
               zc123=q*z2n+0.5*zb+za
               icount=icount+1
               xic(icount)=xc123
               yic(icount)=yc123
               zic(icount)=zc123
 310        continue
 300     continue
C*****      elseif(itype.lt.0) then
C*****         do 400 i1=0,(n-1)
C*****            do 410 j1=1,n
C*****               x1=xn(i1,j1,1)
C*****               y1=xn(i1,j1,2)
C*****               z1=xn(i1,j1,3)
C*****               x2=xn(i1+1,j1,1)
C*****               y2=xn(i1+1,j1,2)
C*****               z2=xn(i1+1,j1,3)
C*****               x3=xn(i1,j1+1,1)
C*****               y3=xn(i1,j1+1,2)
C*****               z3=xn(i1,j1+1,3)
C*****               if(i1.eq.0.or.j1.eq.1) then
C*****                  if(i1.eq.0) then
C*****                     xc123=0.25*x1+0.75*x3
C*****                     yc123=0.25*y1+0.75*y3
C*****                     zc123=0.25*z1+0.75*z3
C*****                     xc123=xc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     yc123=yc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     zc123=zc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     icount=icount+1
C*****                     xic(icount)=xc123
C*****                     yic(icount)=yc123
C*****                     zic(icount)=zc123
C*****                  endif
C*****                  if(j1.eq.1) then
C*****                     xc123=0.25*x1+0.75*x2
C*****                     yc123=0.25*y1+0.75*y2
C*****                     zc123=0.25*z1+0.75*z2
C*****                     xc123=xc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     yc123=yc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     zc123=zc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     icount=icount+1
C*****                     xic(icount)=xc123
C*****                     yic(icount)=yc123
C*****                     zic(icount)=zc123
C*****                  endif
C*****               endif
C*****               xc123=(x1+x2+x3)/3.0
C*****               yc123=(y1+y2+y3)/3.0
C*****               zc123=(z1+z2+z3)/3.0
C*****               icount=icount+1
C*****               xic(icount)=xc123
C*****               yic(icount)=yc123
C*****               zic(icount)=zc123
C*****               x1=xn(i1+1,j1+1,1)
C*****               y1=xn(i1+1,j1+1,2)
C*****               z1=xn(i1+1,j1+1,3)
C*****               x2=xn(i1+1,j1,1)
C*****               y2=xn(i1+1,j1,2)
C*****               z2=xn(i1+1,j1,3)
C*****               x3=xn(i1,j1+1,1)
C*****               y3=xn(i1,j1+1,2)
C*****               z3=xn(i1,j1+1,3)
C*****               if(i1.eq.(n-1).or.j1.eq.n) then
C*****                  if(i1.eq.(n-1)) then
C*****                     xc123=0.25*x1+0.75*x2
C*****                     yc123=0.25*y1+0.75*y2
C*****                     zc123=0.25*z1+0.75*z2
C*****                     xc123=xc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     yc123=yc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     zc123=zc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     icount=icount+1
C*****                     xic(icount)=xc123
C*****                     yic(icount)=yc123
C*****                     zic(icount)=zc123
C*****                  endif
C*****                  if(j1.eq.n) then
C*****                     xc123=0.25*x1+0.75*x3
C*****                     yc123=0.25*y1+0.75*y3
C*****                     zc123=0.25*z1+0.75*z3
C*****                     xc123=xc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     yc123=yc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     zc123=zc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                     icount=icount+1
C*****                     xic(icount)=xc123
C*****                     yic(icount)=yc123
C*****                     zic(icount)=zc123
C*****                  endif
C*****               endif
C*****               xc123=(x1+x2+x3)/3.0
C*****               yc123=(y1+y2+y3)/3.0
C*****               zc123=(z1+z2+z3)/3.0
C*****               icount=icount+1
C*****               xic(icount)=xc123
C*****               yic(icount)=yc123
C*****               zic(icount)=zc123
C***** 410        continue
C***** 400     continue
C*****         do 420 i1=0,n
C*****            do 430 j1=1,n+1
C*****               if(i1.eq.(n+1-j1)) then
C*****                  xc123=xn(i1,j1,1)
C*****                  yc123=xn(i1,j1,2)
C*****                  zc123=xn(i1,j1,3)
C*****                  xc123=xc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                  yc123=yc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                  zc123=zc123+xfac*(2.0*ranf()-1.0)*ranf()
C*****                  icount=icount+1
C*****                  xic(icount)=xc123
C*****                  yic(icount)=yc123
C*****                  zic(icount)=zc123
C*****               endif
C***** 430        continue
C***** 420     continue
      endif
      goto 9999
 9999 continue
      return
      end
