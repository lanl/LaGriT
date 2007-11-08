C*DK ocgrid
C*DF unicos
      subroutine ocgrid(xn,i4,n)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE
C
C      INPUT ARGUMENTS -
C
C         input_message - CHARACTER STRING CONTAINING THE INPUT
C                            MESSAGE.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - ERROR RETURN CODE (==0 ==> OK, <>0 ==> ERROR)
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/ocgrid.f_a  $
CPVCS    
CPVCS       Rev 1.2   30 Sep 2004 10:57:02   dcg
CPVCS    make pi5 double precision
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:56:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   01/17/95 16:38:54   pvcs
CPVCS     Original version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*132 logmess
C
C#######################################################################
C
c
c        This routine constructs an icosahedral grid for
c        diamond i4 on the unit sphere.
c
      real xn(0:n+1,0:n+1,3)
      integer ip(10)
      ip( 1)=-1
      ip( 2)= 1
      ip( 3)= 3
      ip( 4)= 5
      ip( 5)=-3
      ip( 6)= 0
      ip( 7)= 2
      ip( 8)= 4
      ip( 9)=-4
      ip(10)=-2
      pi5   =0.62831853071796d0
c
      lv   = 1.45*alog(float(n))
      nn   = (n+2)**2
      sgn  = sign(1.0,5.5-i4)
      phi  = ip(i4)*pi5
      w    = 2.*acos(1./(2.*sin(pi5)))
      cosw = cos(w)
      sinw = sin(w)
c
      xn(0,  1,1) =  0.
      xn(0,  1,2) =  0.
      xn(0,  1,3) =  sgn
      xn(n,  1,1) =  sinw*cos(phi)
      xn(n,  1,2) =  sinw*sin(phi)
      xn(n,  1,3) =  cosw*sgn
      xn(0,n+1,1) =  sinw*cos(phi+pi5+pi5)
      xn(0,n+1,2) =  sinw*sin(phi+pi5+pi5)
      xn(0,n+1,3) =  cosw*sgn
      xn(n,n+1,1) =  sinw*cos(phi+pi5)
      xn(n,n+1,2) =  sinw*sin(phi+pi5)
      xn(n,n+1,3) = -cosw*sgn
c
c        Construct the hexagonal grid.
c
      do 80 k=0,lv-1
         m  = 2**k
         l  = n/m
         l2 = l/2
c
c     rows of diamond--
         do 20 j1=1,m+1
         do 20 j2=1,m
             i1 = (j1-1)*l
             i2 = (j2-1)*l + l2 + 1
 20      call ocmidpt(xn(i1,i2,1),xn(i1,i2-l2,1),xn(i1,i2+l2,1),nn)
c
c     columns of diamond--
         do 40 j1=1,m+1
         do 40 j2=1,m
             i1 = (j2-1)*l + l2
             i2 = (j1-1)*l + 1
 40      call ocmidpt(xn(i1,i2,1),xn(i1-l2,i2,1),xn(i1+l2,i2,1),nn)
c
c     diagonals of diamond--
         do 60 j1=1,m
         do 60 j2=1,m
             i1 = (j1-1)*l + l2
             i2 = (j2-1)*l + l2 + 1
 60      call ocmidpt(xn(i1,i2,1),xn(i1-l2,i2+l2,1),xn(i1+l2,i2-l2,1),
     *  nn )
 80   continue
c
      call ocgrdedg(xn,n)
c
ccht
      goto 9999
 9999 continue
      return
      end
