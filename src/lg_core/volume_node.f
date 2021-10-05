*dk,volume_node
      subroutine volume_node(ioption,
     *                       nsd,nen,nef,
     *                       nnodes,nelements,mbndry,
     *                       imt1,itp1,
     *                       xic,yic,zic,
     *                       volume,
     *                       itetclr,itet,jtet)
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
C        $Log: volume_node.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   08 Feb 2006 14:38:10   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.3   Fri Aug 28 14:25:36 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 17:05:38 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   01/04/95 22:06:38   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/13/94 11:44:48   pvcs
CPVCS    Orginal Version
C
C ######################################################################
C
      implicit none
      real*8 alargenumber,asmallnumber
      parameter (alargenumber= 1.d+20)
      parameter (asmallnumber= 1.d-30)
      integer nsd,nen,nef,nnodes,nelements,mbndry
      real*8 distsqa,distsqb,distsqc,distsqd,xvor,yvor,zvor,dvor,qvor2,
     *  a,b,c,d,e,f,xc,yc,zc,xtest,xtestmax,crosx,crosy,crosz,
     * xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,xl4,yl4,zl4,ax1,ay1,az1,
     * ax2,ay2,az2,ax3,ay3,az3,ax4,ay4,az4,voltet,voltot,
     * x234,y234,z234,x143,y143,z143,x132,y132,z132,q,xv1,yv1,zv1,
     *xv2,yv2,zv2,xv3,yv3,zv3,ds14,ds24,ds34,
     *  xv,yv,zv,xn1,yn1,zn1,xdot2,cmx1,cmy1,cmz1
      real*8 x124,y124,z124,xv4,yv4,zv4,ds11,ds21,ds31,ds22,ds32,ds13,
     *  ds23,ds33,x14,y14,z14,x24,y24,z24,x34,y34,z34,cmx3,cmy3,cmz3,
     *  xdot3,x13,y13,z13,x23,y23,z23,cmx2,cmy2,cmz2
      real*8 ds12,xdot1,cvx1,cvy1,cvz1,cvx3,cvy3,cvz3,cvx2,cvy2,cvz2,
     *  xb,yb,zb,xd,yd,zd,xn,yn,zn,rn,dotb3,dot3,rb3,ql,xl,yl,zl,
     *  ds1,ds2,ds3,x12,y12,z12,xfac,xa,ya,za,xm,ym,zm,x1,y1,z1,
     *  x2,y2,z2,x3,y3,z3,xquarter
      integer i4,i1,i2,i3,it,ivoronoi2d,ivoronoi3d,iclr,
     *  maxclrelement
      character ioption*(*)
      integer imt1(nnodes), itp1(nnodes)
      real*8 xic(nnodes), yic(nnodes), zic(nnodes)
      real*8 volume(nnodes)
      integer itetclr(nelements)
      integer itet(nen,nelements), jtet(nef,nelements)
C
      integer nentri,nfacetri,nentet,nfacetet,nenprism,nfaceprism,
     *  nenhex,nfacehex
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      integer ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      integer iprismface0(nfaceprism), iprismface1(4,nfaceprism)
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      integer intpairhex(2,12)
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
      integer itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      integer itriface0(nfacetri), itriface1(3,nfacetri)
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
C
C
C
      data ivoronoi2d / 1 /
      data ivoronoi3d / 1 /
C
      data xquarter / 0.25d+00 /
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C
C     Write polygon data
C
      do i1=1,nnodes
         volume(i1)=0.0d+00
      enddo
      if(nsd.eq.2.and.nen.eq.3.and.nef.eq.3) then
         maxclrelement=0
         do it=1,nelements
            maxclrelement=max(maxclrelement,max(1,itetclr(it)))
         enddo
         if(ivoronoi2d.gt.0) then
            do it=1,nelements
               i1=itet(1,it)
               i2=itet(2,it)
               i3=itet(3,it)
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
               xm=(xic(i1)+xic(i2)+xic(i3))/3.0
               ym=(yic(i1)+yic(i2)+yic(i3))/3.0
               zm=(zic(i1)+zic(i2)+zic(i3))/3.0
               xv=xm
               yv=ym
               zv=zm
               xa=xic(i1)
               ya=yic(i1)
               za=zic(i1)
               xfac=1.0
               xb=xfac*(xic(i2)-xa)
               yb=xfac*(yic(i2)-ya)
               zb=xfac*(zic(i2)-za)
               xd=xfac*(xic(i3)-xa)
               yd=xfac*(yic(i3)-ya)
               zd=xfac*(zic(i3)-za)
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
               ds1=sqrt((xl)**2+(yl)**2+(zl)**2)
               ds2=sqrt((xl-xb)**2+(yl-yb)**2+(zl-zb)**2)
               ds3=sqrt((xl-xd)**2+(yl-yd)**2+(zl-zd)**2)
               xv=xl+xa
               yv=yl+ya
               zv=zl+za
               x12=0.5*(xic(i1)+xic(i2))
               y12=0.5*(yic(i1)+yic(i2))
               z12=0.5*(zic(i1)+zic(i2))
               x13=0.5*(xic(i1)+xic(i3))
               y13=0.5*(yic(i1)+yic(i3))
               z13=0.5*(zic(i1)+zic(i3))
               x23=0.5*(xic(i2)+xic(i3))
               y23=0.5*(yic(i2)+yic(i3))
               z23=0.5*(zic(i2)+zic(i3))
               cmx3=  (y2-y1)*(zm-z1)-(ym-y1)*(z2-z1)
               cmy3=-((x2-x1)*(zm-z1)-(xm-x1)*(z2-z1))
               cmz3=  (x2-x1)*(ym-y1)-(xm-x1)*(y2-y1)
               cvx3=  (y2-y1)*(zv-z1)-(yv-y1)*(z2-z1)
               cvy3=-((x2-x1)*(zv-z1)-(xv-x1)*(z2-z1))
               cvz3=  (x2-x1)*(yv-y1)-(xv-x1)*(y2-y1)
               xdot3=cmx3*cvx3+cmy3*cvy3+cmz3*cvz3
               cmx2=  (y1-y3)*(zm-z3)-(ym-y3)*(z1-z3)
               cmy2=-((x1-x3)*(zm-z3)-(xm-x3)*(z1-z3))
               cmz2=  (x1-x3)*(ym-y3)-(xm-x3)*(y1-y3)
               cvx2=  (y1-y3)*(zv-z3)-(yv-y3)*(z1-z3)
               cvy2=-((x1-x3)*(zv-z3)-(xv-x3)*(z1-z3))
               cvz2=  (x1-x3)*(yv-y3)-(xv-x3)*(y1-y3)
               xdot2=cmx2*cvx2+cmy2*cvy2+cmz2*cvz2
               cmx1=  (y3-y2)*(zm-z2)-(ym-y2)*(z3-z2)
               cmy1=-((x3-x2)*(zm-z2)-(xm-x2)*(z3-z2))
               cmz1=  (x3-x2)*(ym-y2)-(xm-x2)*(y3-y2)
               cvx1=  (y3-y2)*(zv-z2)-(yv-y2)*(z3-z2)
               cvy1=-((x3-x2)*(zv-z2)-(xv-x2)*(z3-z2))
               cvz1=  (x3-x2)*(yv-y2)-(xv-x2)*(y3-y2)
               xdot1=cmx1*cvx1+cmy1*cvy1+cmz1*cvz1
               if(ioption.eq.'voronoi') then
                  volume(i1)=volume(i1)+
     *               sign(xquarter*sqrt(cvx2**2+cvy2**2+cvz2**2),xdot2)+
     *               sign(xquarter*sqrt(cvx3**2+cvy3**2+cvz3**2),xdot3)
                  volume(i2)=volume(i2)+
     *               sign(xquarter*sqrt(cvx1**2+cvy1**2+cvz1**2),xdot1)+
     *               sign(xquarter*sqrt(cvx3**2+cvy3**2+cvz3**2),xdot3)
                  volume(i3)=volume(i3)+
     *               sign(xquarter*sqrt(cvx1**2+cvy1**2+cvz1**2),xdot1)+
     *               sign(xquarter*sqrt(cvx2**2+cvy2**2+cvz2**2),xdot2)
               elseif(ioption.eq.'median') then
                  volume(i1)=volume(i1)+
     *               sign(xquarter*sqrt(cmx2**2+cmy2**2+cmz2**2),xdot2)+
     *               sign(xquarter*sqrt(cmx3**2+cmy3**2+cmz3**2),xdot3)
                  volume(i2)=volume(i2)+
     *               sign(xquarter*sqrt(cmx1**2+cmy1**2+cmz1**2),xdot1)+
     *               sign(xquarter*sqrt(cmx3**2+cmy3**2+cmz3**2),xdot3)
                  volume(i3)=volume(i3)+
     *               sign(xquarter*sqrt(cmx1**2+cmy1**2+cmz1**2),xdot1)+
     *               sign(xquarter*sqrt(cmx2**2+cmy2**2+cmz2**2),xdot2)
               endif
            enddo
         endif
      elseif(nsd.eq.3.and.nen.eq.4.and.nef.eq.4) then
         maxclrelement=0
         do it=1,nelements
            maxclrelement=max(maxclrelement,max(1,itetclr(it)))
         enddo
         iclr=maxclrelement
         if(ivoronoi3d.gt.0) then
            do it=1,nelements
            iclr=min(maxclrelement,itetclr(it))+maxclrelement
            i1=itet(1,it)
            i2=itet(2,it)
            i3=itet(3,it)
            i4=itet(4,it)
            x12=0.5*(xic(i1)+xic(i2))
            y12=0.5*(yic(i1)+yic(i2))
            z12=0.5*(zic(i1)+zic(i2))
            x13=0.5*(xic(i1)+xic(i3))
            y13=0.5*(yic(i1)+yic(i3))
            z13=0.5*(zic(i1)+zic(i3))
            x14=0.5*(xic(i1)+xic(i4))
            y14=0.5*(yic(i1)+yic(i4))
            z14=0.5*(zic(i1)+zic(i4))
            x23=0.5*(xic(i2)+xic(i3))
            y23=0.5*(yic(i2)+yic(i3))
            z23=0.5*(zic(i2)+zic(i3))
            x24=0.5*(xic(i2)+xic(i4))
            y24=0.5*(yic(i2)+yic(i4))
            z24=0.5*(zic(i2)+zic(i4))
            x34=0.5*(xic(i3)+xic(i4))
            y34=0.5*(yic(i3)+yic(i4))
            z34=0.5*(zic(i3)+zic(i4))
            if(ivoronoi3d.eq.1) then
               xa=xic(i2)
               ya=yic(i2)
               za=zic(i2)
               xfac=1.0d+00
               xb=xfac*(xic(i3)-xa)
               yb=xfac*(yic(i3)-ya)
               zb=xfac*(zic(i3)-za)
               xd=xfac*(xic(i4)-xa)
               yd=xfac*(yic(i4)-ya)
               zd=xfac*(zic(i4)-za)
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
               xa=xic(i1)
               ya=yic(i1)
               za=zic(i1)
               xfac=1.0d+00
               xb=xfac*(xic(i4)-xa)
               yb=xfac*(yic(i4)-ya)
               zb=xfac*(zic(i4)-za)
               xd=xfac*(xic(i3)-xa)
               yd=xfac*(yic(i3)-ya)
               zd=xfac*(zic(i3)-za)
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
               xa=xic(i1)
               ya=yic(i1)
               za=zic(i1)
               xfac=1.0d+00
               xb=xfac*(xic(i2)-xa)
               yb=xfac*(yic(i2)-ya)
               zb=xfac*(zic(i2)-za)
               xd=xfac*(xic(i4)-xa)
               yd=xfac*(yic(i4)-ya)
               zd=xfac*(zic(i4)-za)
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
               xa=xic(i1)
               ya=yic(i1)
               za=zic(i1)
               xfac=1.0d+00
               xb=xfac*(xic(i3)-xa)
               yb=xfac*(yic(i3)-ya)
               zb=xfac*(zic(i3)-za)
               xd=xfac*(xic(i2)-xa)
               yd=xfac*(yic(i2)-ya)
               zd=xfac*(zic(i2)-za)
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
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
               xl1=xic(i1)
               yl1=yic(i1)
               zl1=zic(i1)
               xl2=xic(i2)
               yl2=yic(i2)
               zl2=zic(i2)
               xl3=xic(i3)
               yl3=yic(i3)
               zl3=zic(i3)
               xl4=xic(i4)
               yl4=yic(i4)
               zl4=zic(i4)
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
     *                (x2*xc+y2*yc+z2*zc+asmallnumber)
               xl=q*x2+0.5*xb
               yl=q*y2+0.5*yb
               zl=q*z2+0.5*zb
               dvor=-0.5*(xd*xd+yd*yd+zd*zd)
               qvor2=-(xd*xl+yd*yl+zd*zl+dvor)/(xd*xn+yd*yn+zd*zn+
     *               1.0d-30)
               xvor=qvor2*xn+xl+xa
               yvor=qvor2*yn+yl+ya
               zvor=qvor2*zn+zl+za
               distsqa=(xvor-xl2)**2+(yvor-yl2)**2+(zvor-zl2)**2
               distsqb=(xvor-xl3)**2+(yvor-yl3)**2+(zvor-zl3)**2
               distsqc=(xvor-xl4)**2+(yvor-yl4)**2+(zvor-zl4)**2
               distsqd=(xvor-xl1)**2+(yvor-yl1)**2+(zvor-zl1)**2
            elseif(ivoronoi3d.eq.2) then
               xv1=(xic(i2)+xic(i3)+xic(i4))/3.0
               yv1=(yic(i2)+yic(i3)+yic(i4))/3.0
               zv1=(zic(i2)+zic(i3)+zic(i4))/3.0
               xv2=(xic(i1)+xic(i3)+xic(i4))/3.0
               yv2=(yic(i1)+yic(i3)+yic(i4))/3.0
               zv2=(zic(i1)+zic(i3)+zic(i4))/3.0
               xv3=(xic(i1)+xic(i2)+xic(i4))/3.0
               yv3=(yic(i1)+yic(i2)+yic(i4))/3.0
               zv3=(zic(i1)+zic(i2)+zic(i4))/3.0
               xv4=(xic(i1)+xic(i2)+xic(i3))/3.0
               yv4=(yic(i1)+yic(i2)+yic(i3))/3.0
               zv4=(zic(i1)+zic(i2)+zic(i3))/3.0
               xm=(xic(i1)+xic(i2)+xic(i3)+xic(i4))/4.0
               ym=(yic(i1)+yic(i2)+yic(i3)+yic(i4))/4.0
               zm=(zic(i1)+zic(i2)+zic(i3)+zic(i4))/4.0
            endif
         enddo
         endif
      elseif(nsd.eq.3.and.nen.eq.8.and.nef.eq.6) then
         maxclrelement=0
         do it=1,nelements
            maxclrelement=max(maxclrelement,max(1,itetclr(it)))
         enddo
      endif
      goto 9999
 9999 continue
      return
      end
