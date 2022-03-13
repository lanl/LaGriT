*dk volume_tet
      subroutine volume_tet(x1,y1,z1,
     *                      x2,y2,z2,
     *          x3,y3,z3,
     *                      x4,y4,z4,
     *                      voltet)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A TET-ELEMENT DEFINDED BY
C            4 COORDINATE NODES. THE VOLUME IS FOUND BY TAKING THE
C            DOT(CROSS) PRODUCT OF THREE VECTORS.
C
C      see also volume_tet_voronoi below
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC TETRAHEDRON.
C
C
C                     i4 [itet(4,it)]
C                            $
C                           **  *
C                          * *     *
C                         *  *       *
C                        *   *         *
C                       *    *           * b34
C                  b41 *     *             *
C                     *      *               *
C                    *       *                 *
C                   *        * b24               *
C                  *         *                     *
C                 *          *    b31     _   _   _  $
C            i1  $_   _  -   * -   -   -          *   i3 [itet(3,it)]
C    [itet(1,it)] *          *                *
C                   *        *             *
C                 b21 *      *         *    b23
C                       *    *     *
C                         *  *  *
C                            $
C                     i2 [itet(2,it)]
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x4,y4,z4) - THE COORDINATES OF THE TET.
C
C     OUTPUT ARGUMENTS -
C
C        voltet - THE VOLUME OF THE TET.
C
C     CHANGE HISTORY -
C
C        $Log: volume_tet.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   08 Feb 2006 14:35:38   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.6   Tue Oct 19 13:16:34 1999   jtg
CPVCS    added a few comments
CPVCS    
CPVCS       Rev 1.5   Fri Jan 22 16:59:36 1999   dcg
CPVCS    initialize imedian to 0
CPVCS
CPVCS       Rev 1.4   Fri Aug 28 14:25:34 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 17:05:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Thu Oct 10 08:44:22 1996   het
CPVCS    Calculate Voronoi volumes.
CPVCS
CPVCS       Rev 1.1   08/15/95 18:29:34   het
CPVCS    Modify some comment lines
C
C#######################################################################
C
C
      implicit none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,voltet,dx,dy,dz
C
C#######################################################################
C
C
C     ..................................................................
C     TAKE THE CROSS PRODUCT OF THE 23 AND 43 VECTORS TO THE AN AREA
C        VECTOR.
C
      dx= ((y2-y3)*(z4-z3)-(y4-y3)*(z2-z3))
      dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
      dz= ((x2-x3)*(y4-y3)-(x4-x3)*(y2-y3))
C
C     ..................................................................
C     THEN DOT THIS AREA VECTOR WITH THE 31 VECTOR TO GET THE TET VOLUME
C
      voltet=-((x3-x1)*dx+(y3-y1)*dy+(z3-z1)*dz)/6.0d+00
C
      return
      end
C
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C=======================================================================
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
C
*dk volume_tet_voronoi
      subroutine volume_tet_voronoi(xl1,yl1,zl1,
     *                              xl2,yl2,zl2,
     *                              xl3,yl3,zl3,
     *                              xl4,yl4,zl4,
     *                              voltet_vor, voledge_vor)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FINDS THE VOLUME OF A TET-ELEMENT DEFINDED BY
C            4 COORDINATE NODES. THE VOLUME IS FOUND BY TAKING THE
C            DOT(CROSS) PRODUCT OF THREE VECTORS.
C
C         it also calculates the voronoi info voltet_vor, voledge_vor
C
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC TETRAHEDRON.
C
C
C                     i4 [itet(4,it)]
C                            $
C                           **  *
C                          * *     *
C                         *  *       *
C                        *   *         *
C                       *    *           * b34
C                  b41 *     *             *
C                     *      *               *
C                    *       *                 *
C                   *        * b24               *
C                  *         *                     *
C                 *          *    b31     _   _   _  $
C            i1  $_   _  -   * -   -   -          *   i3 [itet(3,it)]
C    [itet(1,it)] *          *                *
C                   *        *             *
C                 b21 *      *         *    b23
C                       *    *     *
C                         *  *  *
C                            $
C                     i2 [itet(2,it)]
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x4,y4,z4) - THE COORDINATES OF THE TET.
C
C     OUTPUT ARGUMENTS -
C
C        voltet - THE VOLUME OF THE TET.
C
C     CHANGE HISTORY -
C
C        $Log: volume_tet.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
C
C#######################################################################
C
C
      implicit none
      real*8 alargenumber
      parameter (alargenumber=1.0d+20)
      real*8 a,b,c,d,e,f,crosx,crosy,crosz,xl1,yl1,zl1,
     *  xl2,yl2,zl2,xl3,yl3,zl3,xl4,yl4,zl4,dx,dy,dz,voltet,xa,ya,za,
     *  xfac,xb,yb,zb,xd,yd,zd,xn1,yn1,zn1,xn,yn,zn,rn,
     *  dotb3,dot3,rb3,ql,xl,yl,zl,ds11,ds21,ds31,xv1,yv1,zv1,
     *  ds12,ds22,ds32,xv2,yv2,zv2,ds13,ds23,ds33,xv3,yv3,zv3,ds14,ds24,
     *  xv4,yv4,zv4,x1,y1,z1,x2,y2,z2,x3,y3,z3,xm,ym,zm,
     *  ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,ax4,ay4,az4,voltot,x234,
     *  ds34, y234,z234,x143,y143,z143,x124,y124,z124,x132,y132,z132
      real*8 xtestmax,xtest,xc,yc,zc,q,dvor,qvor2,xvor,yvor,zvor,
     *  distsqa,distsqb,distsqc,distsqd,x4,y4,z4,x12,y12,z12,
     *  x13,y13,z13,x14,y14,z14,x23,y23,z23,xaa,y24,z24,x34,y34,z34,
     *  ax11,ay11,az11,vol11,vol13,ax12,ay12,az12,vol12,vol14,xdot1,
     *  ax21,ay21,az21,vol21,xdot2
      real*8 xdot6,ax6,ay6,az6,az62,ay62,ax62,vol63,vol61,ax61,ay61,
     *  az61,vol51,vol53,ax52,ay52,az52,vol52,vol54,xdot5,xdot4,
     *  vol44,ax5,ax51,ay51,az51,ax42,ay42,az42,vol41,vol43,
     *  vol23,ax22,ay22,az22,vol22,vol24,ax31,ay31,az31,vol31,
     *  vol33,ax32,ay32,az32,vol32,vol34,xdot3,ax41,ay41,az41,
     *  vol42,ay5,az5,vol62,vol64
      integer it,i1,i2,i3,i4,idebug,imedian
      real*8 vol1a,vol1b,vol2a,vol2b,vol3a,vol3b,vol4a,vol4b,vol5a,vol5b
     * ,vol6a,vol6b,voltet_vor1,volvortet1,volvortet2,volvortet3,
     *  volvortet4,voltot_vor1,voltet_vor2,voltot_vor2,voltetvor2
C
      real*8 voltet_vor(4), voledge_vor(6)
      data imedian /0/
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
      data idebug / 0 /
C
C
C#######################################################################
C
C
C     ..................................................................
C     TAKE THE CROSS PRODUCT OF THE 23 AND 43 VECTORS TO THE AN AREA
C        VECTOR.
C
      dx= ((yl2-yl3)*(zl4-zl3)-(yl4-yl3)*(zl2-zl3))
      dy=-((xl2-xl3)*(zl4-zl3)-(xl4-xl3)*(zl2-zl3))
      dz= ((xl2-xl3)*(yl4-yl3)-(xl4-xl3)*(yl2-yl3))
C
C     ..................................................................
C     THEN DOT THIS AREA VECTOR WITH THE 31 VECTOR TO GET THE TET VOLUME
C
      voltet=-((xl3-xl1)*dx+(yl3-yl1)*dy+(zl3-zl1)*dz)/6.0d+00
C
         xa=xl2
         ya=yl2
         za=zl2
         xfac=1.0d+00
         xb=xfac*(xl3-xa)
         yb=xfac*(yl3-ya)
         zb=xfac*(zl3-za)
         xd=xfac*(xl4-xa)
         yd=xfac*(yl4-ya)
         zd=xfac*(zl4-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds11=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds21=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds31=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv1=xl+xa
         yv1=yl+ya
         zv1=zl+za
         xa=xl1
         ya=yl1
         za=zl1
         xfac=1.0d+00
         xb=xfac*(xl4-xa)
         yb=xfac*(yl4-ya)
         zb=xfac*(zl4-za)
         xd=xfac*(xl3-xa)
         yd=xfac*(yl3-ya)
         zd=xfac*(zl3-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds12=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds22=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds32=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv2=xl+xa
         yv2=yl+ya
         zv2=zl+za
         xa=xl1
         ya=yl1
         za=zl1
         xfac=1.0d+00
         xb=xfac*(xl2-xa)
         yb=xfac*(yl2-ya)
         zb=xfac*(zl2-za)
         xd=xfac*(xl4-xa)
         yd=xfac*(yl4-ya)
         zd=xfac*(zl4-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds13=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds23=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds33=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv3=xl+xa
         yv3=yl+ya
         zv3=zl+za
         xa=xl1
         ya=yl1
         za=zl1
         xfac=1.0d+00
         xb=xfac*(xl3-xa)
         yb=xfac*(yl3-ya)
         zb=xfac*(zl3-za)
         xd=xfac*(xl2-xa)
         yd=xfac*(yl2-ya)
         zd=xfac*(zl2-za)
         xn1=crosx(xb,yb,zb,xd,yd,zd)
         yn1=crosy(xb,yb,zb,xd,yd,zd)
         zn1=crosz(xb,yb,zb,xd,yd,zd)
         xn=crosx(xb,yb,zb,xn1,yn1,zn1)
         yn=crosy(xb,yb,zb,xn1,yn1,zn1)
         zn=crosz(xb,yb,zb,xn1,yn1,zn1)
         rn=1.0/sqrt(xn*xn+yn*yn+zn*zn)
         xn=xn*rn
         yn=yn*rn
         zn=zn*rn
         dotb3=xb*xd+yb*yd+zb*zd
         dot3=dotb3/(xd*xd+yd*yd+zd*zd)
         rb3=1.0/(xb*xb+yb*yb+zb*zb)
         ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3)
         xl=0.5*(ql*(xd-dotb3*rb3*xb)+xb)
         yl=0.5*(ql*(yd-dotb3*rb3*yb)+yb)
         zl=0.5*(ql*(zd-dotb3*rb3*zb)+zb)
         ds14=sqrt((xl)**2+(yl)**2+(zl)**2)
         ds24=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
         ds34=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
         xv4=xl+xa
         yv4=yl+ya
         zv4=zl+za
         x1=xl1
         y1=yl1
         z1=zl1
         x2=xl2
         y2=yl2
         z2=zl2
         x3=xl3
         y3=yl3
         z3=zl3
         xm=(xl1+xl2+xl3+xl4)/4.0
         ym=(yl1+yl2+yl3+yl4)/4.0
         zm=(zl1+zl2+zl3+zl4)/4.0
         xl1=xl1
         yl1=yl1
         zl1=zl1
         xl2=xl2
         yl2=yl2
         zl2=zl2
         xl3=xl3
         yl3=yl3
         zl3=zl3
         xl4=xl4
         yl4=yl4
         zl4=zl4
         ax1=  (yl3-yl2)*(zl4-zl2)-(zl3-zl2)*(yl4-yl2)
         ay1=-((xl3-xl2)*(zl4-zl2)-(zl3-zl2)*(xl4-xl2))
         az1=  (xl3-xl2)*(yl4-yl2)-(yl3-yl2)*(xl4-xl2)
         ax2=  (yl4-yl1)*(zl3-zl1)-(zl4-zl1)*(yl3-yl1)
         ay2=-((xl4-xl1)*(zl3-zl1)-(zl4-zl1)*(xl3-xl1))
         az2=  (xl4-xl1)*(yl3-yl1)-(yl4-yl1)*(xl3-xl1)
         ax3=  (yl2-yl1)*(zl4-zl1)-(zl2-zl1)*(yl4-yl1)
         ay3=-((xl2-xl1)*(zl4-zl1)-(zl2-zl1)*(xl4-xl1))
         az3=  (xl2-xl1)*(yl4-yl1)-(yl2-yl1)*(xl4-xl1)
         ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
         ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
         az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
         voltet=-((xl4-xl1)*ax4+(yl4-yl1)*ay4+(zl4-zl1)*az4)
         voltot=voltot+voltet
         x234=(xl2+xl3+xl4)/3.0
         y234=(yl2+yl3+yl4)/3.0
         z234=(zl2+zl3+zl4)/3.0
         x143=(xl1+xl4+xl3)/3.0
         y143=(yl1+yl4+yl3)/3.0
         z143=(zl1+zl4+zl3)/3.0
         x124=(xl1+xl2+xl4)/3.0
         y124=(yl1+yl2+yl4)/3.0
         z124=(zl1+zl2+zl4)/3.0
         x132=(xl1+xl3+xl2)/3.0
         y132=(yl1+yl3+yl2)/3.0
         z132=(zl1+zl3+zl2)/3.0
         xtestmax=alargenumber
         xtest=xtestmax
         xa=xl2
         ya=yl2
         za=zl2
         xb=xl3-xa
         yb=yl3-ya
         zb=zl3-za
         xc=xl4-xa
         yc=yl4-ya
         zc=zl4-za
         xd=xl1-xa
         yd=yl1-ya
         zd=zl1-za
         xn=  yb*zc-yc*zb
         yn=-(xb*zc-xc*zb)
         zn=  xb*yc-xc*yb
         x2=  yn*zb-yb*zn
         y2=-(xn*zb-xb*zn)
         z2=  xn*yb-xb*yn
         q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *          (x2*xc+y2*yc+z2*zc+1.0e-30)
         xl=q*x2+0.5*xb
         yl=q*y2+0.5*yb
         zl=q*z2+0.5*zb
         dvor=-0.5*(xd*xd+yd*yd+zd*zd)
         qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/(xd*xn+yd*yn+zd*zn+1.0d-30)
         xvor=qvor2*xn+xl+xa
         yvor=qvor2*yn+yl+ya
         zvor=qvor2*zn+zl+za
         distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
         distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
         distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
         distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
         x1=xl1
         y1=yl1
         z1=zl1
         x2=xl2
         y2=yl2
         z2=zl2
         x3=xl3
         y3=yl3
         z3=zl3
         x4=xl4
         y4=yl4
         z4=zl4
         x12=0.5*(xl1+xl2)
         y12=0.5*(yl1+yl2)
         z12=0.5*(zl1+zl2)
         x13=0.5*(xl1+xl3)
         y13=0.5*(yl1+yl3)
         z13=0.5*(zl1+zl3)
         x14=0.5*(xl1+xl4)
         y14=0.5*(yl1+yl4)
         z14=0.5*(zl1+zl4)
         x23=0.5*(xl2+xl3)
         y23=0.5*(yl2+yl3)
         z23=0.5*(zl2+zl3)
c***         x24=0.5*(xl2+xl4)
         xaa=0.5*(xl2+xl4)
         y24=0.5*(yl2+yl4)
         z24=0.5*(zl2+zl4)
         x34=0.5*(xl3+xl4)
         y34=0.5*(yl3+yl4)
         z34=0.5*(zl3+zl4)
         if(imedian.eq.1) then
            xv1=(xl2+xl3+xl4)/3.0
            yv1=(yl2+yl3+yl4)/3.0
            zv1=(zl2+zl3+zl4)/3.0
            xv2=(xl1+xl3+xl4)/3.0
            yv2=(yl1+yl3+yl4)/3.0
            zv2=(zl1+zl3+zl4)/3.0
            xv3=(xl1+xl2+xl4)/3.0
            yv3=(yl1+yl2+yl4)/3.0
            zv3=(zl1+zl2+zl4)/3.0
            xv4=(xl1+xl2+xl3)/3.0
            yv4=(yl1+yl2+yl3)/3.0
            zv4=(zl1+zl2+zl3)/3.0
            xvor=xm
            yvor=ym
            zvor=zm
         endif
         ax11=  (yv4-y12)*(zvor-z12)-(yvor-y12)*(zv4-z12)
         ay11=-((xv4-x12)*(zvor-z12)-(xvor-x12)*(zv4-z12))
         az11=  (xv4-x12)*(yvor-y12)-(xvor-x12)*(yv4-y12)
         vol11=-((x1-x12)*ax11+(y1-y12)*ay11+(z1-z12)*az11)
         vol13= ((x2-x12)*ax11+(y2-y12)*ay11+(z2-z12)*az11)
         ax12=-((yv3-y12)*(zvor-z12)-(yvor-y12)*(zv3-z12))
         ay12=+((xv3-x12)*(zvor-z12)-(xvor-x12)*(zv3-z12))
         az12=-((xv3-x12)*(yvor-y12)-(xvor-x12)*(yv3-y12))
         vol12=-((x1-x12)*ax12+(y1-y12)*ay12+(z1-z12)*az12)
         vol14= ((x2-x12)*ax12+(y2-y12)*ay12+(z2-z12)*az12)
         ax1=ax11+ax12
         ay1=ay11+ay12
         az1=az11+az12
         xdot1=(x2-x1)*ax1+(y2-y1)*ay1+(z2-z1)*az1
         ax21=  (yv2-y13)*(zvor-z13)-(yvor-y13)*(zv2-z13)
         ay21=-((xv2-x13)*(zvor-z13)-(xvor-x13)*(zv2-z13))
         az21=  (xv2-x13)*(yvor-y13)-(xvor-x13)*(yv2-y13)
         vol21=-((x1-x13)*ax21+(y1-y13)*ay21+(z1-z13)*az21)
         vol23= ((x3-x13)*ax21+(y3-y13)*ay21+(z3-z13)*az21)
         ax22=-((yv4-y13)*(zvor-z13)-(yvor-y13)*(zv4-z13))
         ay22=+((xv4-x13)*(zvor-z13)-(xvor-x13)*(zv4-z13))
         az22=-((xv4-x13)*(yvor-y13)-(xvor-x13)*(yv4-y13))
         vol22=-((x1-x13)*ax22+(y1-y13)*ay22+(z1-z13)*az22)
         vol24= ((x3-x13)*ax22+(y3-y13)*ay22+(z3-z13)*az22)
         ax2=ax21+ax22
         ay2=ay21+ay22
         az2=az21+az22
         xdot2=(x3-x1)*ax2+(y3-y1)*ay2+(z3-z1)*az2
         ax31=  (yv3-y14)*(zvor-z14)-(yvor-y14)*(zv3-z14)
         ay31=-((xv3-x14)*(zvor-z14)-(xvor-x14)*(zv3-z14))
         az31=  (xv3-x14)*(yvor-y14)-(xvor-x14)*(yv3-y14)
         vol31=-((x1-x14)*ax31+(y1-y14)*ay31+(z1-z14)*az31)
         vol33= ((x4-x14)*ax31+(y4-y14)*ay31+(z4-z14)*az31)
         ax32=-((yv2-y14)*(zvor-z14)-(yvor-y14)*(zv2-z14))
         ay32=+((xv2-x14)*(zvor-z14)-(xvor-x14)*(zv2-z14))
         az32=-((xv2-x14)*(yvor-y14)-(xvor-x14)*(yv2-y14))
         vol32=-((x1-x14)*ax32+(y1-y14)*ay32+(z1-z14)*az32)
         vol34= ((x4-x14)*ax32+(y4-y14)*ay32+(z4-z14)*az32)
         ax3=ax31+ax32
         ay3=ay31+ay32
         az3=az31+az32
         xdot3=(x4-x1)*ax3+(y4-y1)*ay3+(z4-z1)*az3
         ax41=  (yv4-y23)*(zvor-z23)-(yvor-y23)*(zv4-z23)
         ay41=-((xv4-x23)*(zvor-z23)-(xvor-x23)*(zv4-z23))
         az41=  (xv4-x23)*(yvor-y23)-(xvor-x23)*(yv4-y23)
         vol41=-((x2-x23)*ax41+(y2-y23)*ay41+(z2-z23)*az41)
         vol43= ((x3-x23)*ax41+(y3-y23)*ay41+(z3-z23)*az41)
         ax42=-((yv1-y23)*(zvor-z23)-(yvor-y23)*(zv1-z23))
         ay42=+((xv1-x23)*(zvor-z23)-(xvor-x23)*(zv1-z23))
         az42=-((xv1-x23)*(yvor-y23)-(xvor-x23)*(yv1-y23))
         vol42=-((x2-x23)*ax42+(y2-y23)*ay42+(z2-z23)*az42)
         vol44= ((x3-x23)*ax42+(y3-y23)*ay42+(z3-z23)*az42)
         ax4=ax41+ax42
         ay4=ay41+ay42
         az4=az41+az42
         xdot4=(x3-x2)*ax5+(y3-y2)*ay4+(z3-z2)*az4
         ax51=  (yv1-y24)*(zvor-z24)-(yvor-y24)*(zv1-z24)
         ay51=-((xv1-xaa)*(zvor-z24)-(xvor-xaa)*(zv1-z24))
         az51=  (xv1-xaa)*(yvor-y24)-(xvor-xaa)*(yv1-y24)
         vol51=-((x2-xaa)*ax51+(y2-y24)*ay51+(z2-z24)*az51)
         vol53= ((x4-xaa)*ax51+(y4-y24)*ay51+(z4-z24)*az51)
         ax52=-((yv3-y24)*(zvor-z24)-(yvor-y24)*(zv3-z24))
         ay52=+((xv3-xaa)*(zvor-z24)-(xvor-xaa)*(zv3-z24))
         az52=-((xv3-xaa)*(yvor-y24)-(xvor-xaa)*(yv3-y24))
         vol52=-((x2-xaa)*ax52+(y2-y24)*ay52+(z2-z24)*az52)
         vol54= ((x4-xaa)*ax52+(y4-y24)*ay52+(z4-z24)*az52)
         ax5=ax51+ax52
         ay5=ay51+ay52
         az5=az51+az52
         xdot5=(x4-x2)*ax5+(y4-y2)*ay5+(z4-z2)*az5
         ax61=  (yv2-y34)*(zvor-z34)-(yvor-y34)*(zv2-z34)
         ay61=-((xv2-x34)*(zvor-z34)-(xvor-x34)*(zv2-z34))
         az61=  (xv2-x34)*(yvor-y34)-(xvor-x34)*(yv2-y34)
         vol61=-((x3-x34)*ax61+(y3-y34)*ay61+(z3-z34)*az61)
         vol63= ((x4-x34)*ax61+(y4-y34)*ay61+(z4-z34)*az61)
         ax62=-((yv1-y34)*(zvor-z34)-(yvor-y34)*(zv1-z34))
         ay62=+((xv1-x34)*(zvor-z34)-(xvor-x34)*(zv1-z34))
         az62=-((xv1-x34)*(yvor-y34)-(xvor-x34)*(yv1-y34))
         vol62=-((x3-x34)*ax62+(y3-y34)*ay62+(z3-z34)*az62)
         vol64= ((x4-x34)*ax62+(y4-y34)*ay62+(z4-z34)*az62)
         ax6=ax61+ax62
         ay6=ay61+ay62
         az6=az61+az62
         xdot6=(x4-x3)*ax6+(y4-y3)*ay6+(z4-z3)*az6
         vol1a=vol11+vol12
         vol1b=vol13+vol14
         vol2a=vol21+vol22
         vol2b=vol23+vol24
         vol3a=vol31+vol32
         vol3b=vol33+vol34
         vol4a=vol41+vol42
         vol4b=vol43+vol44
         vol5a=vol51+vol52
         vol5b=vol53+vol54
         vol6a=vol61+vol62
         vol6b=vol63+vol64
         voltet_vor1=abs(vol1a)+abs(vol1b)+
     *           abs(vol2a)+abs(vol2b)+
     *           abs(vol3a)+abs(vol3b)+
     *           abs(vol4a)+abs(vol4b)+
     *           abs(vol5a)+abs(vol5b)+
     *           abs(vol6a)+abs(vol6b)
         voltet_vor1=vol1a+vol1b+
     *           vol2a+vol2b+
     *           vol3a+vol3b+
     *           vol4a+vol4b+
     *           vol5a+vol5b+
     *           vol6a+vol6b
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        CALCULATE THE VOLUME OF THE TET USING THE VORONOI POINT AS THE
C           PIVOT POINT.
C
         x1=xvor
         y1=yvor
         z1=zvor
         x2=xl2
         y2=yl2
         z2=zl2
         x3=xl3
         y3=yl3
         z3=zl3
         x4=xl4
         y4=yl4
         z4=zl4
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet1=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
         x2=xl1
         y2=yl1
         z2=zl1
         x3=xl4
         y3=yl4
         z3=zl4
         x4=xl3
         y4=yl3
         z4=zl3
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet2=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
         x2=xl1
         y2=yl1
         z2=zl1
         x3=xl2
         y3=yl2
         z3=zl2
         x4=xl4
         y4=yl4
         z4=zl4
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet3=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
         x2=xl1
         y2=yl1
         z2=zl1
         x3=xl3
         y3=yl3
         z3=zl3
         x4=xl2
         y4=yl2
         z4=zl2
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         volvortet4=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz)
C
         voltot_vor1=voltot_vor1+voltet_vor1
         voltet_vor2=volvortet1+volvortet2+volvortet3+volvortet4
         voltot_vor2=voltot_vor2+voltet_vor2
C
         voledge_vor(1)=vol1a/6.0d+00
         voledge_vor(2)=vol2a/6.0d+00
         voledge_vor(3)=vol3a/6.0d+00
         voledge_vor(4)=vol4a/6.0d+00
         voledge_vor(5)=vol5a/6.0d+00
         voledge_vor(6)=vol6a/6.0d+00
C
         voltet_vor(1)=(vol1a+vol2a+vol3a)/6.0d+00
         voltet_vor(2)=(vol1b+vol4a+vol5a)/6.0d+00
         voltet_vor(3)=(vol2b+vol4b+vol6a)/6.0d+00
         voltet_vor(4)=(vol3b+vol5b+vol6b)/6.0d+00
C
         if(idebug.ge.2) then
            print *,voltet,voltet_vor1,voltetvor2
            print *,it,i1,i2,i3,i4,voltet_vor1,voltot_vor1
            print *,'   ',vol1a,vol1b,vol11,vol12,vol13,vol14
            print *,'   ',vol2a,vol2b,vol21,vol22,vol23,vol24
            print *,'   ',vol3a,vol3b,vol31,vol32,vol33,vol34
            print *,'   ',vol4a,vol4b,vol41,vol42,vol43,vol44
            print *,'   ',vol5a,vol5b,vol51,vol52,vol53,vol54
            print *,'   ',vol6a,vol6b,vol61,vol62,vol63,vol64
         endif
C
      return
      end
