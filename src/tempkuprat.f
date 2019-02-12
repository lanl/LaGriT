C  temp files for use by Andrew Kuprat
C        $Log: tempkuprat.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.46   31 Jan 2000 17:13:36   kuprat
CPVCS    Broke out 8 subroutines.
CPVCS    
CPVCS       Rev 1.45   Wed Nov 04 02:43:42 1998   kuprat
CPVCS    Broke PRIMESTEP out.
CPVCS    
CPVCS       Rev 1.44   Tue Sep 22 13:15:50 1998   dcg
CPVCS    remove zox, conc, dconc, erfcxy, odparm, mxy, dmxy, 
CPVCS    getcurvinfo
CPVCS
CPVCS       Rev 1.43   Fri Apr 10 17:29:10 1998   kuprat
CPVCS    Took refine_allsurftets out.
CPVCS
CPVCS       Rev 1.42   Fri Apr 10 14:44:36 1998   kuprat
CPVCS    Added refine_allsurftets.
CPVCS
CPVCS       Rev 1.41   Wed Oct 29 17:00:12 1997   kuprat
CPVCS    Broke out getnodhyb.f and damp_pt_by_volume.f
CPVCS
CPVCS       Rev 1.40   Tue Sep 02 23:17:06 1997   kuprat
CPVCS    Stripped out damp_pt_by_value.f, freemove.f, mega3d.f.
CPVCS
CPVCS       Rev 1.39   Tue Jun 03 12:59:06 1997   kuprat
CPVCS    Added routine EFIELD.
CPVCS
CPVCS       Rev 1.38   Tue Jun 03 12:52:44 1997   kuprat
CPVCS    Added code for constrained smoothing on cones, but kept
CPVCS    it disabled until strict guards against grid collapsed
CPVCS    are enacted.  Fixed a bug in GETNODHYB relevant to the
CPVCS    case where the mass point array is not everything.
CPVCS
CPVCS       Rev 1.37   Thu Nov 21 08:57:18 1996   kuprat
CPVCS    Changed all the calls to MMGETLEN to calls to MMBLKLEN.  This
CPVCS    is because stale pointers can fool MMGETLEN.
CPVCS
CPVCS       Rev 1.36   Tue Nov 19 15:13:14 1996   kuprat
CPVCS    Fixed memory management bug.
CPVCS
CPVCS       Rev 1.35   Thu Nov 07 12:47:30 1996   kuprat
CPVCS    Fixed bug where picky hp compiler did not accept the integer
CPVCS    array IREAL1 as a logical in an IF test.
CPVCS
CPVCS       Rev 1.34   Thu Oct 31 23:39:22 1996   kuprat
CPVCS    We added routines getnodhyb, primestep, and getvoloff.  We
CPVCS    now have the ability to smooth hybrid grids in 3d.
CPVCS
CPVCS       Rev 1.33   Thu Aug 08 15:20:06 1996   kuprat
CPVCS    In mega2d, changed functional in the geometry option to be the
CPVCS    sum of SQUARES of aspect ratios.
CPVCS
CPVCS       Rev 1.32   Sun Jul 28 21:03:28 1996   kuprat
CPVCS    Added 'geometry' option to mega2d.
CPVCS
CPVCS       Rev 1.31   Sun Jul 28 18:26:32 1996   kuprat
CPVCS    Added ELLIPTIC2d.  (Inefficient Gauss-Jacobi iteration: soon
CPVCS    to be replaced by more efficient Gauss-Seidel.)
CPVCS
CPVCS       Rev 1.30   Thu Jul 25 21:45:46 1996   kuprat
CPVCS    Removed unnecessary debug 'print' lines.
CPVCS
CPVCS       Rev 1.29   Thu Jul 25 21:30:58 1996   kuprat
CPVCS    Added routines mega2d, laplace2d, and getnoditet.
CPVCS
CPVCS       Rev 1.28   Tue May 28 21:32:38 1996   kuprat
CPVCS    Compress out noninterior points from 'mpary'
CPVCS    (until xcontab gets fixed).
CPVCS
CPVCS       Rev 1.27   Wed May 22 21:44:12 1996   kuprat
CPVCS    Deleted conbasis1 from mega3d
CPVCS
CPVCS       Rev 1.26   Wed May 22 09:34:10 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.25   Thu May 16 10:33:56 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.24   Sun Apr 28 22:56:36 1996   kuprat
CPVCS    Added code for 'control' option for isotropic smoothing.
CPVCS
CPVCS       Rev 1.23   Sun Apr 28 20:25:42 1996   kuprat
CPVCS    Modified mega3d so that smoothing is usable for more
CPVCS    than just the TEMPE demo problem.  Eliminated routines
CPVCS    for avoiding cross-connected tets on curvilinear boundaries.
CPVCS
CPVCS       Rev 1.21   Thu Mar 14 13:40:16 1996   het
CPVCS    Change the call to the refine commands to add names.
CPVCS
CPVCS       Rev 1.20   Thu Feb 15 11:28:46 1996   dcg
CPVCS    split off kdtree,select,nearestpoint,lineseq_inter routines
CPVCS
CPVCS       Rev 1.19   Tue Feb 13 22:01:42 1996   kuprat
CPVCS    Added a couple of lines of documentation.
CPVCS
CPVCS       Rev 1.18   Tue Feb 13 21:35:20 1996   kuprat
CPVCS    Added documention to k-D tree routines.
CPVCS
CPVCS       Rev 1.17   Tue Feb 13 14:12:06 1996   kuprat
CPVCS    Added routines kdtree, select, lineseg_inter, and nearestpoint
CPVCS    for building k-D trees, and performing line segment and nearest point
CPVCS    queries against the k-D tree.
CPVCS
CPVCS       Rev 1.16   12/05/95 08:24:38   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.15   11/16/95 15:22:52   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.14   11/15/95 15:34:28   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.13   11/07/95 17:28:44   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.12   11/07/95 14:48:12   kuprat
CPVCS    Improved mega3d so that it ran the 'boron' problem for Tempe
CPVCS    conference.  Added routines 'differentgrid' and 'interioredge'.
CPVCS
CPVCS       Rev 1.11   10/13/95 13:30:48   kuprat
CPVCS    put in fudge_xcontab.f
CPVCS
CPVCS       Rev 1.10   10/13/95 01:00:52   kuprat
CPVCS    Added 3d smoothing routines.
CPVCS
CPVCS       Rev 1.9   09/19/95 14:44:58   kuprat
CPVCS    Changed order of last two arguments to dtmesh3d so that
CPVCS    output DT occurs last.
CPVCS
CPVCS       Rev 1.8   09/19/95 14:35:54   kuprat
CPVCS    Added dtmesh3d.
CPVCS
CPVCS       Rev 1.7   09/17/95 21:29:26   kuprat
CPVCS    Added Kent's 3d boron concentration routines:
CPVCS    conc.f  erfcxy.f odparm.f xymask.f
CPVCS
CPVCS       Rev 1.6   09/10/95 20:57:54   kuprat
CPVCS    File emptied; routines split out to individual files.
CPVCS
CPVCS
CPVCS       Rev 1.5   06/28/95 10:40:06   dcg
CPVCS    esug arguments fixes
CPVCS
CPVCS       Rev 1.3   06/02/95 23:57:10   kuprat
CPVCS    Added getiedge
CPVCS
CPVCS       Rev 1.2   06/02/95 22:06:44   kuprat
CPVCS    Corrected esug2d.
CPVCS
CPVCS
CPVCS       Rev 1.1   06/02/95 21:41:10   kuprat
CPVCS    Added routines trifind, esug2d, and outdraw.
CPVCS
 
      subroutine dtmesh3d(itet,ntet,xic,yic,zic,uic,vic,wic,voltol,
     &   iopt,dt)
 
c     Andrew Kuprat  1/13/95
 
c     This subroutine takes point and velocity information in each of
c     the NTET tets and calculates a DT at which time the smallest volume
c     tet should have volume (close to) VOLTOL.  (VOLTOL can be negative to
c     allow minor tet inversions.)  The volume function for each
c     tet is a cubic equation, and if IOPT=0, we solve
c     these equations to obtain an exact estimate.  If IOPT=1, however,
c     we base our estimate on the linearization of these equations at dt=0
c     to obtain an approximate (but much cheaper) estimate.
 
c     Note:  In the unlikely event that this subroutine 'bombs' with IOPT=0,
c     you can use IOPT=1 to arrive at an approximate DT estimate.
 
      implicit real*8 (a-h,o-z)
      dimension itet(4,1000000), xic(1000000),
     &   yic(1000000), zic(1000000),
     &   uic(1000000), vic(1000000), wic(1000000)
      parameter (pi=3.1415926535897932d0)
      parameter (tol=1.e-8)
      character*132 logmess
 
      det(a2,b2,c2,a3,b3,c3,a4,b4,c4) = a2*(b3*c4-b4*c3)+
     & a3*(b4*c2-b2*c4)+a4*(b2*c3-b3*c2)
 
c                                                         2   3
c   For each tet, the formula for vol(t) is  1/6 ( A+Bt+Ct +Dt )  , where
c
c   A=|x2 x3 x4| (where | | represents determinant, and xi is the displacement
c      vector between node 'i' and node 1),
c   B=|v2 x3 x4| + |x2 v3 x4| + |x2 x3 v4| (where vi is the RELATIVE velocity
c      vector between node 'i' and node 1),
c   C=|x2 v3 v4| + |v2 x3 v4| + |v2 v3 x4|, and
c   D=|v2 v3 v4| .
 
c   If IOPT=0, we solve for each tet, the equation
c
c              2    3
c   A + Bt + Ct + Dt  = 6*VOLTOL for the smallest positive t value, and
c   return the minimum positive root over all tets.
c
c   If IOPT=1, we solve the linearization at t=0:
c
c   A + Bt = 6*VOLTOL.  This is much cheaper.
 
c   For convenience, the largest possible DT is set to 1.d99
      dt=1.d99
 
c   Even in the IOPT=0 case, we first compute a cheap linear estimate for DT.
      do i=1,ntet
         xoff=xic(itet(1,i))
         yoff=yic(itet(1,i))
         zoff=zic(itet(1,i))
         x2=xic(itet(2,i))-xoff
         y2=yic(itet(2,i))-yoff
         z2=zic(itet(2,i))-zoff
         x3=xic(itet(3,i))-xoff
         y3=yic(itet(3,i))-yoff
         z3=zic(itet(3,i))-zoff
         x4=xic(itet(4,i))-xoff
         y4=yic(itet(4,i))-yoff
         z4=zic(itet(4,i))-zoff
         a=det(x2,y2,z2,x3,y3,z3,x4,y4,z4)
         if (a/6.lt.voltol) then
            write(logmess,'(a,i8,a,g14.7)')
     &         'Tet ',i,' already has volume ',a/6
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a,g14.7,a)')
     &         'Since that''s less than ',voltol,', we return dt=0.'
            call writloga('default',0,logmess,0,ierrw)
            dt=0.
            goto 9999
         endif
         uoff=uic(itet(1,i))
         voff=vic(itet(1,i))
         woff=wic(itet(1,i))
         u2=uic(itet(2,i))-uoff
         v2=vic(itet(2,i))-voff
         w2=wic(itet(2,i))-woff
         u3=uic(itet(3,i))-uoff
         v3=vic(itet(3,i))-voff
         w3=wic(itet(3,i))-woff
         u4=uic(itet(4,i))-uoff
         v4=vic(itet(4,i))-voff
         w4=wic(itet(4,i))-woff
         b=det(u2,v2,w2,x3,y3,z3,x4,y4,z4)+
     &      det(x2,y2,z2,u3,v3,w3,x4,y4,z4)+
     &      det(x2,y2,z2,x3,y3,z3,u4,v4,w4)
         if (b.lt.0.) dt=min(dt,(6.*voltol-a)/b)
      enddo
 
      write(logmess,'(a,g14.7)')
     &   'dt (linear estimate) = ',dt
      call writloga('default',0,logmess,0,ierrw)
 
      if (iopt.eq.1) goto 9999
 
c   Exact cubic estimate.
 
c   The linear estimate defines a characteristic time of interest.  If any
c   of the "cubic" tet determinants are very nearly quadratic or linear
c   over the characteristic time, they are treated as such.
      chartime=dt
      dt=1.d99
      do i=1,ntet
         xoff=xic(itet(1,i))
         yoff=yic(itet(1,i))
         zoff=zic(itet(1,i))
         x2=xic(itet(2,i))-xoff
         y2=yic(itet(2,i))-yoff
         z2=zic(itet(2,i))-zoff
         x3=xic(itet(3,i))-xoff
         y3=yic(itet(3,i))-yoff
         z3=zic(itet(3,i))-zoff
         x4=xic(itet(4,i))-xoff
         y4=yic(itet(4,i))-yoff
         z4=zic(itet(4,i))-zoff
         a=det(x2,y2,z2,x3,y3,z3,x4,y4,z4)
         uoff=uic(itet(1,i))
         voff=vic(itet(1,i))
         woff=wic(itet(1,i))
         u2=uic(itet(2,i))-uoff
         v2=vic(itet(2,i))-voff
         w2=wic(itet(2,i))-woff
         u3=uic(itet(3,i))-uoff
         v3=vic(itet(3,i))-voff
         w3=wic(itet(3,i))-woff
         u4=uic(itet(4,i))-uoff
         v4=vic(itet(4,i))-voff
         w4=wic(itet(4,i))-woff
         b=det(u2,v2,w2,x3,y3,z3,x4,y4,z4)+
     &      det(x2,y2,z2,u3,v3,w3,x4,y4,z4)+
     &      det(x2,y2,z2,x3,y3,z3,u4,v4,w4)
         c=det(x2,y2,z2,u3,v3,w3,u4,v4,w4)+
     &      det(u2,v2,w2,x3,y3,z3,u4,v4,w4)+
     &      det(u2,v2,w2,u3,v3,w3,x4,y4,z4)
         d=det(u2,v2,w2,u3,v3,w3,u4,v4,w4)
         a0=a-6.*voltol
c If the following is true, the equation is nontrivially cubic over the
c characteristic time interval, so we solve using the exact formula
c in Numerical Recipes.
         if (abs(d)*chartime**3.gt.tol*a0) then
            a0=a0/d
            b=b/d
            c=c/d
            q=(c*c-3.*b)/9.
            r=(2.*c**3-9.*c*b+27.*a0)/54.
            r2=r*r
            q3=q**3
            if (r2.lt.q3) then
               theta=acos(r/sqrt(q3))
               qh=sqrt(q)
               x1=-2.*qh*cos(theta/3.)-c/3.
               x2=-2.*qh*cos((theta+2.*pi)/3.)-c/3.
               x3=-2.*qh*cos((theta-2.*pi)/3.)-c/3.
               if (x1.gt.0.) dt=min(dt,x1)
               if (x2.gt.0.) dt=min(dt,x2)
               if (x3.gt.0.) dt=min(dt,x3)
            else
               exponent=1.0d+00/3.0d+00
               anr=-sign( (abs(r)+sqrt(r2-q3))**(exponent) , r)
               if (anr.ne.0.) then
                  bnr=q/anr
               else
                  bnr=0.
               endif
               x1=(anr+bnr)-c/3.
               if (x1.gt.0.) dt=min(dt,x1)
            endif
         elseif(abs(c)*chartime**2.gt.tol*a0) then
c If the following is true, the equation is nontrivially quadratic over the
c characteristic time interval, so we solve using the exact formula
c for quadratics.
            discr=b*b-4.*c*a0
            if (discr.ge.0.) then
               q=-0.5*(b+sign( sqrt(discr), b))
               x1=q/c
               x2=a0/q
               if (x1.gt.0.) dt=min(dt,x1)
               if (x2.gt.0.) dt=min(dt,x2)
            endif
         else
c Here the equation is very close to linear over the characteristic time
c interval, so we solve the linearization.
            if (b.lt.0.) then
               x1=-a0/b
               dt=min(dt,x1)
            endif
         endif
      enddo
 
      write(logmess,'(a,g14.7)')
     &   'dt (exact) = ',dt
      call writloga('default',0,logmess,0,ierrw)
 9999 continue
      return
      end
 
      subroutine get1drange(a,u)
C #####################################################################
C
C     PURPOSE -
C                                                  T
C        Given the rank 1 3x3 symmetric matrix A=uu  , which represents
C     the projection operator onto the 1D subspace spanned by the
C     unit vector 'u', we recover 'u' (at least to within a minus sign).
C
C     INPUT ARGUMENTS -
C                                              T
C        A ... rank 1 3x3 symmetric matrix A=uu
C
C     OUTPUT ARGUMENTS -
C
C        u ... recovered vector (see definition of A) to within a
C              minus sign.
C
C     CHANGE HISTORY -
C$Log
C ######################################################################
      implicit none
      include 'consts.h'
 
      real*8 a(3,3),u(3),diag1,diag2,diag3,vlen
      integer indx
 
C The algorithm is to find the largest element on the diagonal
C of A and to return the column that contains it, after
C normalizing.
 
      diag1=abs(a(1,1))
      diag2=abs(a(2,2))
      diag3=abs(a(3,3))
 
      if ((diag3.ge.diag1).and.(diag3.ge.diag2)) then
         indx=3
      elseif (diag2.ge.diag1) then
         indx=2
      else
         indx=1
      endif
 
c   Take column 'indx' of A...
 
      u(1)=a(indx,1)
      u(2)=a(indx,2)
      u(3)=a(indx,3)
 
c   And normalize it.
 
      vlen=sqrt(u(1)**2+u(2)**2+u(3)**2)
      u(1)=u(1)/vlen
      u(2)=u(2)/vlen
      u(3)=u(3)/vlen
 
9999  continue
      return
      end
 
      subroutine getconbasis(nexact,ncurv,xcontab,curvdir,conbasis)
C #####################################################################
C
C     PURPOSE -
C
C        GETCONBASIS returns a matrix CONBASIS consisting of
C        an orthonormal basis to be used in moving a point under
C        the influence of NEXACT exact (linear) and NCURV curvilinear
C        constraints.
C
C     INPUT ARGUMENTS -
C        NEXACT... Number of exact linear constraints.
C        NCURV.... Number of curvilinear constraints.
C        XCONTAB.. Constraint operator induced by exact linear
C                  constraints.  Not used if nexact=0.
C        CURVDIR.. Preferred movement directions for satisfying
C                  curvilinear constraints.  Not used if
C                  if ncurv=0.
C
C     OUTPUT ARGUMENTS -
C        CONBASIS... R^3 basis suitable for satisfying all constraints.
C
C     CHANGE HISTORY -
C$Log: tempkuprat.f,v $
CRevision 2.00  2007/11/09 20:04:04  spchu
CImport to CVS
C
C ######################################################################
C        Given the velocity constraint operator
C        XCONTAB of exact linear constraints and the matrix
C        CURVDIR of preferred movement directions for satisfying
C        curvilinear constraints (usually directions NORMAL to the
C        curvilinear surface), we return the matrix CONBASIS.  CONBASIS
C        consists of three orthonormal vectors U, V, W.  Starting with
C        W and working our way backward to U, our basis vectors come
C        from the following three sets:
C           #1. Unit vectors annihilated by XCONTAB
C           #2. Projections, using XCONTAB, of elements in CURVDIR.
C               (Normalized to unit length.)
C           #3. Unit vectors orthogonal to those chosen from #1. and #2.
C               such that U-V-W forms a righthanded system.
C
C        Suppose for example, there was one linear constraint, and one
C        curvilinear moving constraint of the form
C                                                  f(x,y,z,t)=0.
C        Then XCONTAB has rank 2, so that set #1 contains one vector (to
C        within a sign) which is assigned to W.  CURVDIR has one nonzero
C        vector which is 'grad f'.  So set #2 is XCONTAB of 'grad f',
C        normalized to unity, and this is assigned to V.  Finally, we
C        choose the unique unit vector from set #3 and call it U.
C
C           In general, it is possible that any or all vectors U,V,W will
C        come from any of sets #1, #2, or #3.  However, in our
C        initial implementation, set #2 will not contribute more than
C        one member to U-V-W.  That is, we allow at most ONE curvilinear
C        constraint for the moment.
C
C           The rationale behind this choice of basis is as follows.
C        Vectors chosen from set #1 will be the 'forbidden directions'
C        in which point movement must be zero under all circumstances.
C        Vectors from set #2 represent the best directions in which to
C        move points to satisfy curvilinear constraints.  They are a compromise
C        in that any components in the 'forbidden directions' must be
C        projected out.  (This implies that this subroutine will fail if the
C        curvilinear constraints request a movement exactly annihilated by
C        XCONTAB at any point.  This is to be expected, however, because in
C        this case the constraints are inconsistent!)  For a constraint
C        of the form f(x,y,z,t)=0, the degree of annihilation of 'grad f'
C        (in CURVDIR) by XCONTAB will be the degree to which the moving
C        point will have to deviate from NORMAL MOTION with respect to the
C        surface f=0 in order to satisfy BOTH the exact linear constraints
C        and the f=0. constraint.  Finally, set #3 consists of any remaining
C        'free' directions in which the point can move to meet
C        other remaining criteria not referred to in this subroutine.
C
C
      implicit none
      include 'consts.h'
 
      integer nexact,ncurv,ierrw,i
      real*8 xcontab(3,3),curvdir(3,3),conbasis(3,3),u(3),v(3),w(3),
     &   one_minus_xcontab(3,3),vlen
 
      character*132 logmess
      if (nexact.eq.3) then
 
C In this case, the linear constraint space is everything, and
C we simply return the usual basis for R^3.
 
         u(1)=1.
         u(2)=0.
         u(3)=0.
         v(1)=0.
         v(2)=1.
         v(3)=0.
         w(1)=0.
         w(2)=0.
         w(3)=1.
 
      elseif (nexact.eq.2) then
 
C In this case, there is only one direction that is not constrained linearly
C and we obtain it from XCONTAB using GET1DRANGE
 
         call get1drange(xcontab,u)
 
C Now, given the vector U, we obtain the vectors V, W, using FINISHBASIS
C which returns an orthonormal U-V-W basis given a unit vector U.
 
         call finishbasis(u,v,w)
 
      elseif (nexact.eq.1) then
 
C In this case, there is only one direction that is constrained linearly
C and we obtain it by feeding (Identity-XCONTAB) (a rank 1 matrix) to
C GET1DRANGE.
 
         one_minus_xcontab(1,1)=one-xcontab(1,1)
         one_minus_xcontab(2,1)=-xcontab(2,1)
         one_minus_xcontab(3,1)=-xcontab(3,1)
         one_minus_xcontab(1,2)=-xcontab(1,2)
         one_minus_xcontab(2,2)=one-xcontab(2,2)
         one_minus_xcontab(3,2)=-xcontab(3,2)
         one_minus_xcontab(1,3)=-xcontab(1,3)
         one_minus_xcontab(2,3)=-xcontab(2,3)
         one_minus_xcontab(3,3)=one-xcontab(3,3)
 
         call get1drange(one_minus_xcontab,w)
 
         if (ncurv.eq.1) then
 
C In this case, there is one curvilinear constraint direction which we have
C to project using XCONTAB.
 
            v(1)=xcontab(1,1)*curvdir(1,1)+
     &         xcontab(1,2)*curvdir(2,1)+
     &         xcontab(1,3)*curvdir(3,1)
 
            v(2)=xcontab(2,1)*curvdir(1,1)+
     &         xcontab(2,2)*curvdir(2,1)+
     &         xcontab(2,3)*curvdir(3,1)
 
            v(3)=xcontab(3,1)*curvdir(1,1)+
     &         xcontab(3,2)*curvdir(2,1)+
     &         xcontab(3,3)*curvdir(3,1)
 
C Normalize v.
            vlen=sqrt(v(1)**2+v(2)**2+v(3)**2)
            v(1)=v(1)/vlen
            v(2)=v(2)/vlen
            v(3)=v(3)/vlen
 
C Now u = v x w.
            u(1)=v(2)*w(3)-w(2)*v(3)
            u(2)=v(3)*w(1)-w(3)*v(1)
            u(3)=v(1)*w(2)-w(1)*v(2)
 
         elseif (ncurv.eq.0) then
 
C In this case, U and V are 'free directions' and are obtained using
C FINISHBASIS.
 
            call finishbasis(w,u,v)
 
         else
 
C It is currently not allowed to have NCURV not equal to zero or one.
 
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
            stop
 
         endif
 
      elseif (nexact.eq.0) then
 
C In this case, there are no exact linear constraints and U,V,W are
C all free or constrained by curvilinear constraints.
 
         if (ncurv.eq.1) then
 
C In this case, W will be the normalized curvilinear constraint direction.
C Set W equal to normalized first vector of CURVDIR.
            w(1)=curvdir(1,1)
            w(2)=curvdir(2,1)
            w(3)=curvdir(3,1)
            vlen=sqrt(w(1)**2+w(2)**2+w(3)**2)
            w(1)=w(1)/vlen
            w(2)=w(2)/vlen
            w(3)=w(3)/vlen
 
C Now U and V are 'free directions' and are obtained using
C FINISHBASIS.
 
            call finishbasis(w,u,v)
 
         elseif (ncurv.eq.0) then
 
C In this case, there are no constraints whatsoever, and
C we simply return the usual basis for R^3.
 
            u(1)=1.
            u(2)=0.
            u(3)=0.
            v(1)=0.
            v(2)=1.
            v(3)=0.
            w(1)=0.
            w(2)=0.
            w(3)=1.
 
         else
 
C It is currently not allowed to have NCURV not equal to zero or one.
 
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
            stop
 
         endif
 
      else
 
C NEXACT not in range 0 to 3.
 
            write(logmess,'(a)')
     *         'Sorry.  Illegal value for NEXACT.'
            call writloga('default',0,logmess,0,ierrw)
            stop
 
      endif
 
c...  Copy u,v,w into conbasis matrix.
 
      do i=1,3
         conbasis(i,1)=u(i)
         conbasis(i,2)=v(i)
         conbasis(i,3)=w(i)
      enddo
 
9999  continue
      return
      end
 
*dk,efield
      subroutine efield
C
C
C #####################################################################
C
C     PURPOSE -  Define electric field by taking gradient of
C                potential.
C
C
C     INPUT ARGUMENTS -
C
C         none
C
C     OUTPUT ARGUMENTS -
C
C         none
C
C     CHANGE HISTORY -
C
C #####################################################################
 
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'smooth.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer ierror,ierrw
 
      character*132 logmess
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      pointer (ipsolve_v,solve_v)
      pointer (ipe_x,e_x)
      pointer (ipe_y,e_y)
      pointer (ipe_z,e_z)
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      pointer (ipitettyp,itettyp)
      real*8 xic(lenptr),yic(lenptr),zic(lenptr),
     &   solve_v(lenptr),e_x(lenptr),e_y(lenptr),e_z(lenptr)
      integer itet(lenptr),itetoff(lenptr),itettyp(lenptr)
C
      character*32 cmo,isubname
      integer length,icmotype,icscode,nnodes,nelements,ier,ii,ityp,
     &   loctet,i,loc2,loc3,loc4,i2,i3,i4,i1,ich
      real*8 volwt,afac(3,4),vol6,epsilonl,epsilonv
 
c...  Statement functions for the components of the cross product
c...  ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
 
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,crosx,crosy,crosz
 
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
C
      isubname = 'efield'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'EFIELD: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,nelements,length,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('solve_v',cmo,ipsolve_v,length,icmotype,ierror)
      call cmo_get_info('e_x',cmo,ipe_x,length,icmotype,ierror)
      call cmo_get_info('e_y',cmo,ipe_y,length,icmotype,ierror)
      call cmo_get_info('e_z',cmo,ipe_z,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ierror)
 
c...  get epsilon length.
 
      call get_epsilon('epsilonl', epsilonl)
      print*,'epsilonl=',epsilonl
 
c...  compute epsilon volume.  the expression in brackets is the
c...  epsilon length divided by the machine epsilon... this
c...  ought to be the characteristic length of the grid.  thus
c...  the epsilon volume is (machine epsilon)*(char. length)**3.
 
      epsilonv=epsilonl*(epsilonl/1.d-8)**2
      print*,'epsilonv=',epsilonv
 
c.... Loop over elements used and compute negative gradients.
 
      do ii=1,nelements
         ityp=itettyp(ii)
         e_x(ii)=0.
         e_y(ii)=0.
         e_z(ii)=0.
         volwt=0.
 
c.... Loop over all virtual tetrahedra in the element.
 
         do loctet=1,ihybnumtetv(ityp)
 
c.... Loop over all four faces and compute area vectors.
 
            do i=1,4
               loc2=ielmface1(1,i,ifelmtet)
               loc3=ielmface1(2,i,ifelmtet)
               loc4=ielmface1(3,i,ifelmtet)
               i2=itet(itetv(loc2,loctet,ityp)
     &            +itetoff(ii))
               i3=itet(itetv(loc3,loctet,ityp)
     &            +itetoff(ii))
               i4=itet(itetv(loc4,loctet,ityp)
     &            +itetoff(ii))
               afac(1,i)=crosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &            yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
               afac(2,i)=crosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &            yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
               afac(3,i)=crosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &            yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            enddo
 
c.... Compute the volume of the tet.
 
            i1=itet( itetv(1,loctet,ityp) + itetoff(ii))
            i2=itet( itetv(2,loctet,ityp) + itetoff(ii))
            vol6=afac(1,1)*(xic(i2)-xic(i1))+
     &         afac(2,1)*(yic(i2)-yic(i1))+afac(3,1)*(zic(i2)-zic(i1))
 
c.... If the volume is bigger than EPSILONV, we compute the negative gradient of
c.... this tet.
c.... Since we are computing a weighted sum of gradients of tetrahedra, we
c.... then multiply by the weights.  The weight is equal to the
c.... volume of the tet multiplied by the WTTETV value in smooth.h.
 
c$$$            if (vol6.gt.6.*epsilonv) then
            if (vol6.gt.zero) then
               do i=1,4
                  ich=itet( itetv(i,loctet,ityp)
     &               + itetoff(ii))
                  e_x(ii)=e_x(ii)+wttetv(loctet,ityp)*
     &               solve_v(ich)*afac(1,i)
                  e_y(ii)=e_y(ii)+wttetv(loctet,ityp)*
     &               solve_v(ich)*afac(2,i)
                  e_z(ii)=e_z(ii)+wttetv(loctet,ityp)*
     &               solve_v(ich)*afac(3,i)
               enddo
 
               volwt=volwt+wttetv(loctet,ityp)*vol6
 
            endif
         enddo
 
c.... Now divide through the sum of negative gradients by the sum of weights
c.... to get an average field for the element.
 
         if (volwt.gt.zero) then
            e_x(ii)=e_x(ii)/volwt
            e_y(ii)=e_y(ii)/volwt
            e_z(ii)=e_z(ii)/volwt
         endif
      enddo
 
 9999 continue
 
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
C
      return
      end
 