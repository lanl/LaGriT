      subroutine smoothedgetriple(an1,an2,bn1,bn2,coord,axcycle,aycycle,
     *                      azcycle,bxcycle,bycycle,bzcycle,nx0,nx3,
     *                      weight,ierr2)
c
c #####################################################################
c
c    PURPOSE -
c
c       This subroutine does volume-conserving smoothing of a triple
c    line using edge relaxations.
c
c    INPUT ARGUMENTS -
c
c       an1 (bn1) - number of nodes on the surface of material a (b)
c    that surround x1
c
c       an2 (bn2) - number of nodes on the surface of material a (b)
c    that surround x2
c
c       coord - array of size 6 containing the x-coordinate of x1,
c    y-coordinate of x1,  z-coordinate of x1, x-coordinate of x2,
c    y-coordinate of x2, z-coordinate of x2
c
c       axcycle (bxcycle) - array of size an1+an2-4 (bn1+bn2-4)
c    containing the x-coordinates of the counterclockwise cycle of
c    nodes on the surface of material a (b) that surround the edge
c    x1-x2, starting with the first node counterclockwise of x2 when
c    rotating around x1
c
c       aycycle (bycycle) - same as axcycle (bxcycle) except contains
c    y-coordinates
c
c       azcycle (bzcycle) - same as axcycle (bxcycle) except contains
c    z-coordinates
c
c       nx0 (nx3) - index of x0 (x3) in axcycle, aycycle, azcycle
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
c $Log: smooth_vconserve_triple.f,v $
c Revision 2.00  2007/11/09 20:04:03  spchu
c Import to CVS
c
CPVCS    
CPVCS       Rev 1.2   18 Feb 2001 16:29:58   nnc
CPVCS    Trivial constant change to satisfy Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   16 Aug 2000 08:48:20   dcg
CPVCS    change subroutine name to match call from cycle_lg
CPVCS    
CPVCS       Rev 1.0   10 Aug 2000 11:58:32   dcg
CPVCS    Initial revision.
c
c #####################################################################
c
      implicit none
      integer an1,an2,bn1,bn2
      real*8 coord(6)
      real*8 axcycle(an1+an2-4),aycycle(an1+an2-4),azcycle(an1+an2-4)
      real*8 bxcycle(bn1+bn2-4),bycycle(bn1+bn2-4),bzcycle(bn1+bn2-4)
      integer nx0,nx3
      real*8 weight
      integer ierr2
c
      real*8 small
      integer it
      real*8 ae(3,204),be(3,204)
c  The first an1 (bn1) columns of ae (be) are vectors from x1 to
c  surrounding nodes, starting with x2, rotating counterclockwise on
c  the surface of material a (b).  The next an2 (bn2) columns of ae
c  (be) are vectors from x2 to surrounding nodes, starting with x1,
c  rotating counterclockwise on the surface of material a (b).
      real*8 aA1(3),aA2(3),av(3)
      real*8 bA1(3),bA2(3),bv(3)
      real*8 temp1,dx1s(3),dx2s(3)
      real*8 diff(3),aA(3),bA(3)
      real*8 normaAsquared,normbAsquared,aAdotbA,det,ag,bg,h,k
      real*8 n(3)
c
      ierr2=0
c  small should actually be computed
      small=1.0e-13
c
c  compute ae
      do it=1,3
         ae(it,1)=coord(3+it)-coord(it)
         ae(it,an1+1)=-ae(it,1)
      enddo
      do it=2,an1
         ae(1,it)=axcycle(it-1)-coord(1)
         ae(2,it)=aycycle(it-1)-coord(2)
         ae(3,it)=azcycle(it-1)-coord(3)
      enddo
      do it=an1+2,an1+an2-1
         ae(1,it)=axcycle(it-3)-coord(4)
         ae(2,it)=aycycle(it-3)-coord(5)
         ae(3,it)=azcycle(it-3)-coord(6)
      enddo
      ae(1,an1+an2)=axcycle(1)-coord(4)
      ae(2,an1+an2)=aycycle(1)-coord(5)
      ae(3,an1+an2)=azcycle(1)-coord(6)
c
c  compute be
      do it=1,3
         be(it,1)=coord(3+it)-coord(it)
         be(it,bn1+1)=-be(it,1)
      enddo
      do it=2,bn1
         be(1,it)=bxcycle(it-1)-coord(1)
         be(2,it)=bycycle(it-1)-coord(2)
         be(3,it)=bzcycle(it-1)-coord(3)
      enddo
      do it=bn1+2,bn1+bn2-1
         be(1,it)=bxcycle(it-3)-coord(4)
         be(2,it)=bycycle(it-3)-coord(5)
         be(3,it)=bzcycle(it-3)-coord(6)
      enddo
      be(1,bn1+bn2)=bxcycle(1)-coord(4)
      be(2,bn1+bn2)=bycycle(1)-coord(5)
      be(3,bn1+bn2)=bzcycle(1)-coord(6)
c
c  compute aA1 and aA2
      do it=1,3
         aA1(it)=0
         aA2(it)=0
      enddo
      do it=1,an1-1
         aA1(1)=aA1(1)+ae(2,it)*ae(3,it+1)-ae(3,it)*ae(2,it+1)
         aA1(2)=aA1(2)+ae(3,it)*ae(1,it+1)-ae(1,it)*ae(3,it+1)
         aA1(3)=aA1(3)+ae(1,it)*ae(2,it+1)-ae(2,it)*ae(1,it+1)
      enddo
      aA1(1)=aA1(1)+ae(2,an1)*ae(3,1)-ae(3,an1)*ae(2,1)
      aA1(2)=aA1(2)+ae(3,an1)*ae(1,1)-ae(1,an1)*ae(3,1)
      aA1(3)=aA1(3)+ae(1,an1)*ae(2,1)-ae(2,an1)*ae(1,1)
      do it=an1+1,an1+an2-1
         aA2(1)=aA2(1)+ae(2,it)*ae(3,it+1)-ae(3,it)*ae(2,it+1)
         aA2(2)=aA2(2)+ae(3,it)*ae(1,it+1)-ae(1,it)*ae(3,it+1)
         aA2(3)=aA2(3)+ae(1,it)*ae(2,it+1)-ae(2,it)*ae(1,it+1)
      enddo
      aA2(1)=aA2(1)+ae(2,an1+an2)*ae(3,an1+1)-ae(3,an1+an2)*ae(2,an1+1)
      aA2(2)=aA2(2)+ae(3,an1+an2)*ae(1,an1+1)-ae(1,an1+an2)*ae(3,an1+1)
      aA2(3)=aA2(3)+ae(1,an1+an2)*ae(2,an1+1)-ae(2,an1+an2)*ae(1,an1+1)
c
c  compute av
      do it=1,3
         av(it)=ae(it,an1+an2)-ae(it,an1+2)
      enddo
c
c  compute bA1 and bA2
      do it=1,3
         bA1(it)=0
         bA2(it)=0
      enddo
      do it=1,bn1-1
         bA1(1)=bA1(1)+be(2,it)*be(3,it+1)-be(3,it)*be(2,it+1)
         bA1(2)=bA1(2)+be(3,it)*be(1,it+1)-be(1,it)*be(3,it+1)
         bA1(3)=bA1(3)+be(1,it)*be(2,it+1)-be(2,it)*be(1,it+1)
      enddo
      bA1(1)=bA1(1)+be(2,bn1)*be(3,1)-be(3,bn1)*be(2,1)
      bA1(2)=bA1(2)+be(3,bn1)*be(1,1)-be(1,bn1)*be(3,1)
      bA1(3)=bA1(3)+be(1,bn1)*be(2,1)-be(2,bn1)*be(1,1)
      do it=bn1+1,bn1+bn2-1
         bA2(1)=bA2(1)+be(2,it)*be(3,it+1)-be(3,it)*be(2,it+1)
         bA2(2)=bA2(2)+be(3,it)*be(1,it+1)-be(1,it)*be(3,it+1)
         bA2(3)=bA2(3)+be(1,it)*be(2,it+1)-be(2,it)*be(1,it+1)
      enddo
      bA2(1)=bA2(1)+be(2,bn1+bn2)*be(3,bn1+1)-be(3,bn1+bn2)*be(2,bn1+1)
      bA2(2)=bA2(2)+be(3,bn1+bn2)*be(1,bn1+1)-be(1,bn1+bn2)*be(3,bn1+1)
      bA2(3)=bA2(3)+be(1,bn1+bn2)*be(2,bn1+1)-be(2,bn1+bn2)*be(1,bn1+1)
c
c  compute bv
      do it=1,3
         bv(it)=be(it,bn1+bn2)-be(it,bn1+2)
      enddo
c
c  compute dx1s and dx2s
      temp1=2.d0/3.d0
      dx1s(1)=axcycle(nx3)/3.d0+temp1*axcycle(nx0)-coord(1)
      dx1s(2)=aycycle(nx3)/3.d0+temp1*aycycle(nx0)-coord(2)
      dx1s(3)=azcycle(nx3)/3.d0+temp1*azcycle(nx0)-coord(3)
      dx2s(1)=temp1*axcycle(nx3)+axcycle(nx0)/3.d0-coord(4)
      dx2s(2)=temp1*aycycle(nx3)+aycycle(nx0)/3.d0-coord(5)
      dx2s(3)=temp1*azcycle(nx3)+azcycle(nx0)/3.d0-coord(6)
      do it=1,3
         dx1s(it)=weight*dx1s(it)
         dx2s(it)=weight*dx2s(it)
      enddo
c
c  compute aA
      do it=1,3
         diff(it)=dx1s(it)-dx2s(it)
      enddo
      aA(1)=aA1(1)+aA2(1)+av(2)*diff(3)-av(3)*diff(2)
      aA(2)=aA1(2)+aA2(2)+av(3)*diff(1)-av(1)*diff(3)
      aA(3)=aA1(3)+aA2(3)+av(1)*diff(2)-av(2)*diff(1)
c
c  compute bA
      bA(1)=bA1(1)+bA2(1)+bv(2)*diff(3)-bv(3)*diff(2)
      bA(2)=bA1(2)+bA2(2)+bv(3)*diff(1)-bv(1)*diff(3)
      bA(3)=bA1(3)+bA2(3)+bv(1)*diff(2)-bv(2)*diff(1)
c
      normaAsquared=aA(1)**2+aA(2)**2+aA(3)**2
      normbAsquared=bA(1)**2+bA(2)**2+bA(3)**2
      aAdotbA=aA(1)*bA(1)+aA(2)*bA(2)+aA(3)*bA(3)
      det=normaAsquared*normbAsquared-aAdotbA**2
      if (det.le.small) then
         print *, 'det <= small, nodes not moved'
      elseif (det/(normaAsquared*normbAsquared).le.small) then
         print *, 'sin2theta <= small, nodes not moved'
      else
         ag=dx2s(1)*(av(2)*dx1s(3)-av(3)*dx1s(2))+
     *      dx2s(2)*(av(3)*dx1s(1)-av(1)*dx1s(3))+
     *      dx2s(3)*(av(1)*dx1s(2)-av(2)*dx1s(1))
         do it=1,3
            ag=ag+dx1s(it)*aA1(it)+dx2s(it)*aA2(it)
         enddo
         ag=-ag
         bg=dx2s(1)*(bv(2)*dx1s(3)-bv(3)*dx1s(2))+
     *      dx2s(2)*(bv(3)*dx1s(1)-bv(1)*dx1s(3))+
     *      dx2s(3)*(bv(1)*dx1s(2)-bv(2)*dx1s(1))
         do it=1,3
            bg=bg+dx1s(it)*bA1(it)+dx2s(it)*bA2(it)
         enddo
         bg=-bg
         h=(normbAsquared*ag-aAdotbA*bg)/det
         k=(normaAsquared*bg-aAdotbA*ag)/det
         do it=1,3
            n(it)=h*aA(it)+k*bA(it)
            coord(it)=coord(it)+dx1s(it)+n(it)
            coord(it+3)=coord(it+3)+dx2s(it)+n(it)
         enddo
      endif
      return
      end
