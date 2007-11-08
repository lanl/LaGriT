 
      subroutine voronoi_center_2d(x1,y1,z1,x2,y2,z2,
     *                   x3,y3,z3,xcnt,ycnt,zcnt)
C
C  calculate voronoi center of a triangle with vertices
C  x1,y1,z1,x2,y2,z2,x3,y3,z3
C  return results in xcnt,ycnt,zcnt
C  code lifted from matbld3d_stor
C
      implicit none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,xcnt,ycnt,zcnt,
     * xa,ya,za,xb,yb,zb,xc,yc,zc,dotb3,dot3,rb3,ql,xl,yl,zl
C
      xa=x1
      ya=y1
      za=z1
      xb=x2-x1
      yb=y2-y1
      zb=z2-z1
      xc=x3-x1
      yc=y3-y1
      zc=z3-z1
      dotb3=xb*xc+yb*yc+zb*zc
      dot3=dotb3/(xc*xc+yc*yc+zc*zc)
      rb3=1.0/(xb*xb+yb*yb+zb*zb)
      ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
      xl=0.5*(ql*(xc-dotb3*rb3*xb)+xb)
      yl=0.5*(ql*(yc-dotb3*rb3*yb)+yb)
      zl=0.5*(ql*(zc-dotb3*rb3*zb)+zb)
      xcnt=xl+xa
      ycnt=yl+ya
      zcnt=zl+za
C      print *, xcnt,ycnt,zcnt
      if (dotb3.eq.0) then
      xa=x2
      ya=y2
      za=z2
      xb=x3-x2
      yb=y3-y2
      zb=z3-z2
      xc=x1-x2
      yc=y1-y2
      zc=z1-z2
      dotb3=xb*xc+yb*yc+zb*zc
      dot3=dotb3/(xc*xc+yc*yc+zc*zc)
      rb3=1.0/(xb*xb+yb*yb+zb*zb)
      ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
      xl=0.5*(ql*(xc-dotb3*rb3*xb)+xb)
      yl=0.5*(ql*(yc-dotb3*rb3*yb)+yb)
      zl=0.5*(ql*(zc-dotb3*rb3*zb)+zb)
      xcnt=xl+xa
      ycnt=yl+ya
      zcnt=zl+za
      endif
      return
      end
      subroutine coefficient(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *     xcoef,ycoef,zcoef)
c#######################################################################
c
c     purpose -
c
C  calculate the contribution to the coupling coefficient of node1
C  (x1,y1,z1) to node2 (x2,y2,z2)
C  node3 (x3,y3,z3) is the third node of the face in question
C  node4 (x4,y4,z4) is the fourth node of the tet
C
C  input arguments
C     x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4  nodes defining the tet
C
C  ouput arguments
C     xcoef,ycoef,zcoef - vector area determined by the midpoint of
C       edge from (x1,y1,z2) to (x2,y2,z2) the voronoi point of the
C       tet and the 2d voronoi point of the two faces containing the
C       edge
C
c#######################################################################
      implicit none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,xcoef,ycoef,zcoef,
     *   xvor,yvor,zvor,radius,xm,ym,zm,ax1,ay1,az1,
     *   xv2d1,yv2d1,zv2d1,xv2d2,yv2d2,zv2d2,ax2,ay2,az2
C
C  get the voronoi center of the tet
C
      call voronoi_center(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *  x4,y4,z4,xvor,yvor,zvor,radius)
C
C  get the voronoi center of the two faces that share the edge
C
      call voronoi_center_2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *   xv2d1,yv2d1,zv2d1)
      call voronoi_center_2d(x2,y2,z2,x1,y1,z1,x4,y4,z4,
     *   xv2d2,yv2d2,zv2d2)
C
C  get midpoint of edge
C
      xm=(x2+x1)*0.5
      ym=(y2+y1)*0.5
      zm=(z2+z1)*0.5
C
C  calculate vector area of triangle determined by the tet
C  voronoi point one of the triangle voronoi points and the
C  midpoint of the edge
C
      ax1=  (yv2d1-ym)*(zvor-zm)-(yvor-ym)*(zv2d1-zm)
      ay1=-((xv2d1-xm)*(zvor-zm)-(xvor-xm)*(zv2d1-zm))
      az1=  (xv2d1-xm)*(yvor-ym)-(xvor-xm)*(yv2d1-ym)
 
C  repeat for second face
      ax2=-((yv2d2-ym)*(zvor-zm)-(yvor-ym)*(zv2d2-zm))
      ay2=  (xv2d2-xm)*(zvor-zm)-(xvor-xm)*(zv2d2-zm)
      az2=-((xv2d2-xm)*(yvor-ym)-(xvor-xm)*(yv2d2-ym))
 
      xcoef= - ax1 -ax2
      ycoef= - ay1 -ay2
      zcoef= - az1 -az2
      return
      end
