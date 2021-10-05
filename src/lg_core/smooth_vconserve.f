      subroutine smoothedge(n1,n2,coord,xcycle,ycycle,zcycle,weight,
     *                      ierr2)
c
c #####################################################################
c
c    PURPOSE -
c
c       This subroutine calls a smoothing subroutine and then makes
c    volume conserving corrections.  Although the current smoothing
c    subroutine performs Laplacian smoothing, any smoothing subroutine
c    can be used.
c
c    INPUT ARGUMENTS -
c
c       n1 - number of nodes surrounding x1
c
c       n2 - number of nodes surrounding x2
c
c       coord - array of size 6 containing the x-coordinate of x1,
c    y-coordinate of x1,  z-coordinate of x1, x-coordinate of x2,
c    y-coordinate of x2, z-coordinate of x2
c
c       xcycle - array of size n1+n2-4 containing the x-coordinates of
c    the counterclockwise cycle of nodes surrounding the edge x1-x2,
c    starting with the first node counterclockwise of x2 when rotating
c    around x1
c
c       ycycle - same as xcycle except contains y-coordinates
c
c       zcycle - same as xcycle except contains z-coordinates
c
c       weight - number between 0 and 1, inclusive, which controls the
c    amount of smoothing
c
c       ierr2 - error flag
c
c    OUTPUT ARGUMENTS -
c
c       coord, ierr2
c
c    CHANGE HISTORY -
c
c $Log: smooth_vconserve.f,v $
c Revision 2.00  2007/11/09 20:04:03  spchu
c Import to CVS
c
CPVCS    
CPVCS       Rev 1.3   18 Feb 2001 16:29:20   nnc
CPVCS    Trivial constant change to satisfy Absoft compiler.
CPVCS    
CPVCS       Rev 1.2   25 Oct 2000 12:55:54   dcg
CPVCS    fix typo
CPVCS    
CPVCS       Rev 1.1   18 Oct 2000 16:43:58   dcg
CPVCS    add subroutine smoothedgea to smooth edges where the
CPVCS    neighborhood is not uniform in the icr sense - smooth
CPVCS    as a line not as a patch in this case
CPVCS    
CPVCS       Rev 1.0   28 Jul 2000 16:01:16   dcg
CPVCS    Initial revision.
c
c #####################################################################
c
      implicit none
      integer n1,n2
      real*8 coord(6),xcycle(n1+n2-4),ycycle(n1+n2-4),zcycle(n1+n2-4)
      real*8 weight
      integer ierr2
c
      real*8 small
      integer it
      real*8 e(3,200)
c  The first n1 columns of e are vectors from x1 to surrounding nodes,
c  starting with x2, rotating counterclockwise.  The next n2 columns of
c  e are vectors from x2 to surrounding nodes, starting with x1,
c  rotating counterclockwise.
      real*8 A1(3),A2(3),v(3)
      real*8 dx1s(3),dx2s(3)
      real*8 diff(3),A(3)
      real*8 normAsquared,h
      real*8 n(3)
c
      ierr2=0
c  small should actually be computed
      small=1.0e-13
c
c  compute e
      do it=1,3
         e(it,1)=coord(3+it)-coord(it)
         e(it,n1+1)=-e(it,1)
      enddo
      do it=2,n1
         e(1,it)=xcycle(it-1)-coord(1)
         e(2,it)=ycycle(it-1)-coord(2)
         e(3,it)=zcycle(it-1)-coord(3)
      enddo
      do it=n1+2,n1+n2-1
         e(1,it)=xcycle(it-3)-coord(4)
         e(2,it)=ycycle(it-3)-coord(5)
         e(3,it)=zcycle(it-3)-coord(6)
      enddo
      e(1,n1+n2)=xcycle(1)-coord(4)
      e(2,n1+n2)=ycycle(1)-coord(5)
      e(3,n1+n2)=zcycle(1)-coord(6)
c
c  compute A1 and A2
      do it=1,3
         A1(it)=0
         A2(it)=0
      enddo
      do it=1,n1-1
         A1(1)=A1(1)+e(2,it)*e(3,it+1)-e(3,it)*e(2,it+1)
         A1(2)=A1(2)+e(3,it)*e(1,it+1)-e(1,it)*e(3,it+1)
         A1(3)=A1(3)+e(1,it)*e(2,it+1)-e(2,it)*e(1,it+1)
      enddo
      A1(1)=A1(1)+e(2,n1)*e(3,1)-e(3,n1)*e(2,1)
      A1(2)=A1(2)+e(3,n1)*e(1,1)-e(1,n1)*e(3,1)
      A1(3)=A1(3)+e(1,n1)*e(2,1)-e(2,n1)*e(1,1)
      do it=n1+1,n1+n2-1
         A2(1)=A2(1)+e(2,it)*e(3,it+1)-e(3,it)*e(2,it+1)
         A2(2)=A2(2)+e(3,it)*e(1,it+1)-e(1,it)*e(3,it+1)
         A2(3)=A2(3)+e(1,it)*e(2,it+1)-e(2,it)*e(1,it+1)
      enddo
      A2(1)=A2(1)+e(2,n1+n2)*e(3,n1+1)-e(3,n1+n2)*e(2,n1+1)
      A2(2)=A2(2)+e(3,n1+n2)*e(1,n1+1)-e(1,n1+n2)*e(3,n1+1)
      A2(3)=A2(3)+e(1,n1+n2)*e(2,n1+1)-e(2,n1+n2)*e(1,n1+1)
c
c  compute v
      do it=1,3
         v(it)=e(it,n1+n2)-e(it,n1+2)
      enddo
c
c
c  call smoothing subroutine
      call laplsmooth(n1,n2,coord,xcycle,ycycle,zcycle,weight,dx1s,dx2s)
c
c  compute A
      do it=1,3
         diff(it)=dx1s(it)-dx2s(it)
      enddo
      A(1)=A1(1)+A2(1)+v(2)*diff(3)-v(3)*diff(2)
      A(2)=A1(2)+A2(2)+v(3)*diff(1)-v(1)*diff(3)
      A(3)=A1(3)+A2(3)+v(1)*diff(2)-v(2)*diff(1)
c
      normAsquared=A(1)**2+A(2)**2+A(3)**2
      if (normAsquared.gt.small) then
         h=dx2s(1)*(v(2)*dx1s(3)-v(3)*dx1s(2))+
     *     dx2s(2)*(v(3)*dx1s(1)-v(1)*dx1s(3))+
     *     dx2s(3)*(v(1)*dx1s(2)-v(2)*dx1s(1))
         do it=1,3
            h=h+dx1s(it)*A1(it)+dx2s(it)*A2(it)
         enddo
         h=-h/normAsquared
         do it=1,3
            n(it)=h*A(it)
         enddo
         do it=1,3
            coord(it)=coord(it)+dx1s(it)+n(it)
            coord(it+3)=coord(it+3)+dx2s(it)+n(it)
         enddo
      else
         print *, 'normAsquared <= small, nodes not moved'
      endif
      return
      end
c
c
c
      subroutine smoothedgea(n1,n2,coord,xcycle,ycycle,
     *                     zcycle,n0,n3,weight,ierr2)
      implicit none
      integer n1,n2,n0,n3
      real*8 coord(6),xcycle(n1+n2-4),ycycle(n1+n2-4),zcycle(n1+n2-4)
     *    ,temp1,weight
      integer ierr2
c
      real*8 small
      integer it
      real*8 e(3,200)
c  The first n1 columns of e are vectors from x1 to surrounding nodes,
c  starting with x2, rotating counterclockwise.  The next n2 columns of
c  e are vectors from x2 to surrounding nodes, starting with x1,
c  rotating counterclockwise.
      real*8 A1(3),A2(3),v(3)
      real*8 dx1s(3),dx2s(3)
      real*8 diff(3),A(3)
      real*8 normAsquared,h
      real*8 n(3)
c
      ierr2=0
c  small should actually be computed
      small=1.0e-13
c
c  compute e
      do it=1,3
         e(it,1)=coord(3+it)-coord(it)
         e(it,n1+1)=-e(it,1)
      enddo
      do it=2,n1
         e(1,it)=xcycle(it-1)-coord(1)
         e(2,it)=ycycle(it-1)-coord(2)
         e(3,it)=zcycle(it-1)-coord(3)
      enddo
      do it=n1+2,n1+n2-1
         e(1,it)=xcycle(it-3)-coord(4)
         e(2,it)=ycycle(it-3)-coord(5)
         e(3,it)=zcycle(it-3)-coord(6)
      enddo
      e(1,n1+n2)=xcycle(1)-coord(4)
      e(2,n1+n2)=ycycle(1)-coord(5)
      e(3,n1+n2)=zcycle(1)-coord(6)
c
c  compute A1 and A2
      do it=1,3
         A1(it)=0
         A2(it)=0
      enddo
      do it=1,n1-1
         A1(1)=A1(1)+e(2,it)*e(3,it+1)-e(3,it)*e(2,it+1)
         A1(2)=A1(2)+e(3,it)*e(1,it+1)-e(1,it)*e(3,it+1)
         A1(3)=A1(3)+e(1,it)*e(2,it+1)-e(2,it)*e(1,it+1)
      enddo
      A1(1)=A1(1)+e(2,n1)*e(3,1)-e(3,n1)*e(2,1)
      A1(2)=A1(2)+e(3,n1)*e(1,1)-e(1,n1)*e(3,1)
      A1(3)=A1(3)+e(1,n1)*e(2,1)-e(2,n1)*e(1,1)
      do it=n1+1,n1+n2-1
         A2(1)=A2(1)+e(2,it)*e(3,it+1)-e(3,it)*e(2,it+1)
         A2(2)=A2(2)+e(3,it)*e(1,it+1)-e(1,it)*e(3,it+1)
         A2(3)=A2(3)+e(1,it)*e(2,it+1)-e(2,it)*e(1,it+1)
      enddo
      A2(1)=A2(1)+e(2,n1+n2)*e(3,n1+1)-e(3,n1+n2)*e(2,n1+1)
      A2(2)=A2(2)+e(3,n1+n2)*e(1,n1+1)-e(1,n1+n2)*e(3,n1+1)
      A2(3)=A2(3)+e(1,n1+n2)*e(2,n1+1)-e(2,n1+n2)*e(1,n1+1)
c
c  compute v
      do it=1,3
         v(it)=e(it,n1+n2)-e(it,n1+2)
      enddo
c
c
c  compute dx1s and dx2s
      temp1=2.d0/3.d0
      dx1s(1)=xcycle(n3)/3.d0+temp1*xcycle(n0)-coord(1)
      dx1s(2)=ycycle(n3)/3.d0+temp1*ycycle(n0)-coord(2)
      dx1s(3)=zcycle(n3)/3.d0+temp1*zcycle(n0)-coord(3)
      dx2s(1)=temp1*xcycle(n3)+xcycle(n0)/3.d0-coord(4)
      dx2s(2)=temp1*ycycle(n3)+ycycle(n0)/3.d0-coord(5)
      dx2s(3)=temp1*zcycle(n3)+zcycle(n0)/3.d0-coord(6)
c
c  compute A
      do it=1,3
         diff(it)=dx1s(it)-dx2s(it)
      enddo
      A(1)=A1(1)+A2(1)+v(2)*diff(3)-v(3)*diff(2)
      A(2)=A1(2)+A2(2)+v(3)*diff(1)-v(1)*diff(3)
      A(3)=A1(3)+A2(3)+v(1)*diff(2)-v(2)*diff(1)
c
      normAsquared=A(1)**2+A(2)**2+A(3)**2
      if (normAsquared.gt.small) then
         h=dx2s(1)*(v(2)*dx1s(3)-v(3)*dx1s(2))+
     *     dx2s(2)*(v(3)*dx1s(1)-v(1)*dx1s(3))+
     *     dx2s(3)*(v(1)*dx1s(2)-v(2)*dx1s(1))
         do it=1,3
            h=h+dx1s(it)*A1(it)+dx2s(it)*A2(it)
         enddo
         h=-h/normAsquared
         do it=1,3
            n(it)=h*A(it)
         enddo
         do it=1,3
            coord(it)=coord(it)+dx1s(it)+n(it)
            coord(it+3)=coord(it+3)+dx2s(it)+n(it)
         enddo
      else
         print *, 'normAsquared <= small, nodes not moved'
      endif
      return
      end
c
c
      subroutine laplsmooth(n1,n2,coord,xcycle,ycycle,zcycle,weight,
     *                      dx1s,dx2s)
c
      implicit none
      integer n1,n2
      real*8 coord(6),xcycle(n1+n2-4),ycycle(n1+n2-4),zcycle(n1+n2-4)
      real*8 weight
      real*8 dx1s(3),dx2s(3)
c
      real*8 r1,r2
      real*8 c1,c2,c3
      integer it
      real*8 s1(3),s2(3),x2s(3),x1s(3)
c
      r1=dfloat(n1)
      r2=dfloat(n2)
      c1=1/(r1*r2-1)
      c2=r1*c1
      c3=1/r1
      do it=1,3
         s1(it)=0
      enddo
      do it=1,n1-1
         s1(1)=s1(1)+xcycle(it)
         s1(2)=s1(2)+ycycle(it)
         s1(3)=s1(3)+zcycle(it)
      enddo
      s2(1)=xcycle(1)
      s2(2)=ycycle(1)
      s2(3)=zcycle(1)
      do it=n1-1,n1+n2-4
         s2(1)=s2(1)+xcycle(it)
         s2(2)=s2(2)+ycycle(it)
         s2(3)=s2(3)+zcycle(it)
      enddo
      do it=1,3
         x2s(it)=c2*s2(it)+c1*s1(it)
         dx2s(it)=weight*(x2s(it)-coord(it+3))
         x1s(it)=c3*(s1(it)+x2s(it))
         dx1s(it)=weight*(x1s(it)-coord(it))
      enddo
      return
      end
