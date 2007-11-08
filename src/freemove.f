      subroutine freemove(action,node,itp1,icr1,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,icontab,surfparam,offsparam,istype,itet,itetoff,x,y,z,
     &   epsilonl,rdamp,lplanar,pf,pfx,pfxx,dx,dy,dz)
C #####################################################################
C
C     PURPOSE -
C
C        FREEMOVE computes a displacement (DX,DY,DZ) designed to
C     minimize the polyhedral function POLYFUN.  Free moves (for
C     interior points) are computed, but also moves constrained
C     by planes or quadrics are computed.
C
C     INPUT ARGUMENTS -
C
C        ACTION-  Type of smoothing done by polyfun.  Passed to polyfun.
C        NODE  -  The node whose movement we are limiting.
C        ITP1  -  Array of point types
C        ICR1  -  Array of point constraints types
C        NODHYB-  Node-element relation.  Passed to polyfun.
C        NODHYBOFF-  Node-element relation offsets.  Passed to polyfun.
C        IELTARY- Array of relevant elements.
C        IELTNO-  Length of element array.
C        INVMPARY- Inverse mapping of that defined by mass point array
C                  MPARY.
C        VOLOFF-   Array of volume offsets.
C        VOLOFFOFF- Offset array of volume offsets.
C        ITETTYP-  Array of element types.
C        ICONTAB,
C        surfparam  Constraint data arrays
C        offsparam
C        ISTYPE
C        ITET-     Element-node relation.
C        ITETOFF-  Offsets for Element-node relation.
C        X,Y,Z -  The xic,yic,zic arrays for the mesh.
C        EPSILONL- Length epsilon.
C        RDAMP-    Dampening factor for parametric space displacement
C                  (cone case).
C
C     OUTPUT ARGUMENTS -
C
C        LPLANAR-  .true. iff we discover that node has a nonlinear
C                  constraint.
C        PF -     The value of POLYFUN at DX=DY=DZ=0.
C        PFX   -  The gradient of POLYFUN at DX=DY=DZ=0.
C        PFXX
C        DX,DY,-  Proposed changes to add to X(NODE), Y(NODE), Z(NODE).
C        DZ
C
C     CHANGE HISTORY -
C
C$Log: freemove.f,v $
CRevision 2.00  2007/11/05 19:45:56  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.5   10 Apr 2002 10:29:44   dcg
CPVCS    test isdiscrete for surface nodes
CPVCS    
CPVCS       Rev 1.4   05 Feb 2002 10:39:10   dcg
CPVCS    in case where we don't have special code, just call solvsym3
CPVCS    to get new direction to move node - assume calling program
CPVCS    will know what to do with the new coordinates
CPVCS    
CPVCS       Rev 1.3   30 Jul 2001 15:49:22   kuprat
CPVCS    We now pass in the PF, PFX, PFXX info that will determine the move.
CPVCS    
CPVCS       Rev 1.2   03 Feb 2000 09:23:08   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:56   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   06 Jan 2000 12:55:02   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.1   Mon Nov 24 16:36:12 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.0   Tue Sep 02 23:07:32 1997   kuprat
CPVCS    Initial revision.
C
C ######################################################################
 
      implicit none
 
      include 'consts.h'
      include 'massage.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      pointer (ipvoloff,voloff),(iplocvoloff,locvoloff),
     &   (ipivoloffoff,ivoloffoff)
 
      integer node,nodhyb(lenptr),nodhyboff(lenptr),
     &   ieltary(lenptr),ieltno,locvoloff(lenptr),
     &   ivoloffoff(lenptr),itettyp(lenptr),itet(lenptr),
     &   itetoff(lenptr),icr1(lenptr),itp1(lenptr),invmpary(lenptr),
     &   icontab(50,lenptr),icr,isurf,i,j,k,m
 
      real*8 a,b,c,rnorm,xu(3,3),pfu(3),pfuu(3,3),det,du(3),rdamp
      logical lplanar
 
      character*32 istype(lenptr)
 
      real*8 voloff(lenptr),pf,pfx(3),pfxx(3,3),x(lenptr),
     &   y(lenptr),z(lenptr),dx,dy,dz,xc(3),xe(3),
     &   r,d,dist,xrot(3,3),xr(3),u(2),xuu(3,2,2),unew(2),
     &   xrnew(3),xnew(3),epsilonl,pfrxx(3,3),pfrx(3),surfparam(*)
 
      integer offsparam(*)
 
      character*32 action
 
      lplanar=.true.
 
c.... Initialize to a null move.
 
      dx=0.
      dy=0.
      dz=0.
 
c.... If itp1/icr1 values indicate a point type or constraint
c.... that we don't currently know how to process, just return.
 
      icr=icr1(node)
 
C.... Case of interior node where movement is unconstrained.
 
      if (icr.eq.0) then
 
C.... Solve PFXX*DX=-PFX to get node movement DX which will
C.... minimize (or at least lower) PF.
 
         call solvsym3(pfxx(1,1),pfxx(2,1),pfxx(3,1),pfxx(2,2),
     &      pfxx(3,2),pfxx(3,3),-pfx(1),-pfx(2),-pfx(3),dx,dy,dz)
 
C.... Case of single planar constraint.
 
      elseif (icontab(1,icr).eq.1.and.
     &   istype(icontab(3,icr))(1:5).eq.'plane') then
 
c.... Get normal XU(*,3) to planar constraint.
 
        isurf=icontab(3,icr)
        a=surfparam(offsparam(isurf)+10)
        b=surfparam(offsparam(isurf)+11)
        c=surfparam(offsparam(isurf)+12)
        rnorm=sqrt(a*a+b*b+c*c)
        xu(1,3)=a/rnorm
        xu(2,3)=b/rnorm
        xu(3,3)=c/rnorm
 
c.... Finish basis XU by adding two in-plane basis vectors.
 
        call finishbasis(xu(1,3),xu(1,1),xu(1,2))
 
c.... Initialize PFU, PFUU arrays for solving a reduced system
c.... PFUU*DU=-PFU, where DU are coefficients for allowed node
c.... movement in the in-plane directions.
 
         do i=1,2
            do j=1,2
               pfuu(i,j)=0.
            enddo
            pfu(i)=0.
         enddo
 
c.... Solve reduced system for DU.
 
         do i=1,2
            do j=1,2
               do k=1,3
                  do m=1,3
                     pfuu(i,j)=pfuu(i,j)+
     &                  xu(k,i)*pfxx(k,m)*xu(m,j)
                  enddo
               enddo
            enddo
         enddo
 
         do i=1,2
            do k=1,3
                pfu(i)=pfu(i)+xu(k,i)*pfx(k)
            enddo
         enddo
 
         det=pfuu(1,1)*pfuu(2,2)-pfuu(1,2)*pfuu(2,1)
         du(1)=
     &      - ( (pfuu(2,2)*pfu(1)-pfuu(1,2)*pfu(2))/det )
         du(2)=
     &      - ( (-pfuu(2,1)*pfu(1)+pfuu(1,1)*pfu(2))/det )
 
c.... Convert DU to space movement DX.
 
         dx=xu(1,1)*du(1)+xu(1,2)*du(2)
         dy=xu(2,1)*du(1)+xu(2,2)*du(2)
         dz=xu(3,1)*du(1)+xu(3,2)*du(2)
 
c.... Case of two planar constraints.
 
      elseif (icontab(1,icr).eq.2.and.
     &   istype(icontab(3,icr))(1:5).eq.'plane'.and.
     &   istype(icontab(4,icr))(1:5).eq.'plane') then
 
c.... Get normals for two planes.
 
         isurf=icontab(3,icr)
         a=surfparam(offsparam(isurf)+10)
         b=surfparam(offsparam(isurf)+11)
         c=surfparam(offsparam(isurf)+12)
         rnorm=sqrt(a*a+b*b+c*c)
         xu(1,2)=a/rnorm
         xu(2,2)=b/rnorm
         xu(3,2)=c/rnorm
 
         isurf=icontab(4,icr)
         a=surfparam(offsparam(isurf)+10)
         b=surfparam(offsparam(isurf)+11)
         c=surfparam(offsparam(isurf)+12)
         rnorm=sqrt(a*a+b*b+c*c)
         xu(1,3)=a/rnorm
         xu(2,3)=b/rnorm
         xu(3,3)=c/rnorm
 
c.... Take cross-product of normals to get lone 'free' direction.
 
         xu(1,1)=xu(2,2)*xu(3,3)-xu(2,3)*xu(3,2)
         xu(2,1)=xu(3,2)*xu(1,3)-xu(1,2)*xu(3,3)
         xu(3,1)=xu(1,2)*xu(2,3)-xu(1,3)*xu(2,2)
 
c.... Reduce constrained problem to one-dimensional minimization
c.... problem in free direction.  Solve for coefficient of this vector.
 
         pfuu(1,1)=0.
         pfu(1)=0.
         do i=1,3
            do j=1,3
               pfuu(1,1)=pfuu(1,1)+
     &            xu(i,1)*pfxx(i,j)*xu(j,1)
            enddo
            pfu(1)=pfu(1)+xu(i,1)*pfx(i)
         enddo
 
         du(1)=-pfu(1)/pfuu(1,1)
 
c.... Convert DU to spatial movement DX.
 
         dx=xu(1,1)*du(1)
         dy=xu(2,1)*du(1)
         dz=xu(3,1)*du(1)
 
c.... Case of one conical constraint.
 
      elseif (icontab(1,icr).eq.1.and.
     &   istype(icontab(3,icr))(1:4).eq.'cone') then
 
c....    Bailout.  Singularity of solution at time of
c....    cone is too severe for now.
 
c         goto 9999
 
         lplanar=.false.
 
         isurf=icontab(3,icr)
C        ---------------------------------------------------------------
C        GET THE CENTER OR VERTEX POINT, THE END POINT AND THE RADIUS
C        FROM THE STORAGE BLOCK
C
         xc(1)=surfparam(offsparam(isurf)+8)
         xc(2)=surfparam(offsparam(isurf)+9)
         xc(3)=surfparam(offsparam(isurf)+10)
         xe(1)=surfparam(offsparam(isurf)+11)
         xe(2)=surfparam(offsparam(isurf)+12)
         xe(3)=surfparam(offsparam(isurf)+13)
         r=surfparam(offsparam(isurf)+14)
         d=surfparam(offsparam(isurf)+15)
 
c....   If the distance from NODE to the cone vertex is too small,
c....   bail out.
 
        dist=sqrt((x(node)-xc(1))**2+(y(node)-xc(2))**2+
     &      (z(node)-xc(3))**2)
        if (dist.lt.epsilonl) goto 9999
 
c....   Compute unit vectors for a rotated Cartesian coordinate
c....   system aligned with the cone.
 
        do i=1,3
           xrot(i,3)=(xe(i)-xc(i))/d
        enddo
        call finishbasis(xrot(1,3),xrot(1,1),xrot(1,2))
 
c....   Compute the coordinates of NODE in this rotated coordinate
c....   system.
 
        do i=1,3
           xr(i)=(x(node)-xc(1))*xrot(1,i)+(y(node)-xc(2))*xrot(2,i)+
     &        (z(node)-xc(3))*xrot(3,i)
        enddo
 
c....   If we are on the negative side of the cone, something is
c....   wrong... bailout.
 
        if (xr(3).lt.zero) goto 9999
 
c....   u(1)=theta ; u(2)=z.  These are the parametric coordinates
c....   for the cone.
 
        u(1)=atan2(xr(2),xr(1))
        u(2)=xr(3)
 
c....   Compute dx/du, the first derivative of the mapping from the
c....   parametric space to the rotated Cartesian space.
 
        xu(1,1)=-xr(2)
        xu(2,1)=xr(1)
        xu(3,1)=0.
        xu(1,2)=xr(1)/xr(3)
        xu(2,2)=xr(2)/xr(3)
        xu(3,2)=1.
 
c....   Compute d^2x/(du dv), the second derivative of the mapping from the
c....   parametric space to the rotated Cartesian space.
 
        xuu(1,1,1)=-xr(1)
        xuu(2,1,1)=-xr(2)
        xuu(3,1,1)=0.
        xuu(1,2,1)=-xr(2)/xr(3)
        xuu(2,2,1)=xr(1)/xr(3)
        xuu(3,2,1)=0.
        xuu(1,1,2)=xuu(1,2,1)
        xuu(2,1,2)=xuu(2,2,1)
        xuu(3,1,2)=xuu(3,2,1)
        xuu(1,2,2)=0.
        xuu(2,2,2)=0.
        xuu(3,2,2)=0.
 
c...    Compute the Hessian and gradient in rotated Cartesian space.
 
         do i=1,3
            do j=1,3
               pfrxx(i,j)=0.
            enddo
            pfrx(i)=0.
         enddo
         do i=1,3
            do j=1,3
               do k=1,3
                  do m=1,3
                     pfrxx(i,j)=pfrxx(i,j)+
     &                  xrot(k,i)*pfxx(k,m)*xrot(m,j)
                  enddo
               enddo
            enddo
         enddo
 
         do i=1,3
            do k=1,3
                pfrx(i)=pfrx(i)+xrot(k,i)*pfx(k)
            enddo
         enddo
 
c....   Translate the constrained 3x3 minimization problem in rotated Cartesian
c....   space into an unconstrained 2x2 minimization problem in
c....   parametric space.
 
         do i=1,2
            do j=1,2
               pfuu(i,j)=0.
            enddo
            pfu(i)=0.
         enddo
         do i=1,2
            do j=1,2
               do k=1,3
                  do m=1,3
                     pfuu(i,j)=pfuu(i,j)+
     &                  xu(k,i)*pfrxx(k,m)*xu(m,j)
                  enddo
                  pfuu(i,j)=pfuu(i,j)+
     &               pfrx(k)*xuu(k,i,j)
               enddo
            enddo
         enddo
 
         do i=1,2
            do k=1,3
                pfu(i)=pfu(i)+xu(k,i)*pfrx(k)
            enddo
         enddo
 
c....    Solve for new parametric values.
 
         det=pfuu(1,1)*pfuu(2,2)-pfuu(1,2)*pfuu(2,1)
         du(1)=
     &      - ( (pfuu(2,2)*pfu(1)-pfuu(1,2)*pfu(2))/det )
         du(2)=
     &      - ( (-pfuu(2,1)*pfu(1)+pfuu(1,1)*pfu(2))/det )
 
c....    Dampen motion.
 
         du(1)=du(1)*rdamp
         du(2)=du(2)*rdamp
 
         unew(1)=u(1)+du(1)
         unew(2)=u(2)+du(2)
 
c....    Compute new coordinates of NODE in rotated Cartesian space.
 
         xrnew(3)=unew(2)
         xrnew(1)=xrnew(3)*r/d*cos(unew(1))
         xrnew(2)=xrnew(3)*r/d*sin(unew(1))
 
c....    Compute new coordinates of NODE in usual Cartesian space.
 
         do i=1,3
            xnew(i)=xc(i)
            do j=1,3
               xnew(i)=xnew(i)+xrnew(j)*xrot(i,j)
            enddo
         enddo
 
c....    Output coordinate increments.
 
         dx=xnew(1)-x(node)
         dy=xnew(2)-y(node)
         dz=xnew(3)-z(node)
 
      else
 
c.... don't have special code - return best position - assume
c.... calling program will know what to do with result.
C.... Solve PFXX*DX=-PFX to get node movement DX which will
C.... minimize (or at least lower) PF.
      if(.not.isdiscrete) go to 9999
         lplanar=.false.
         call solvsym3(pfxx(1,1),pfxx(2,1),pfxx(3,1),pfxx(2,2),
     &      pfxx(3,2),pfxx(3,3),-pfx(1),-pfx(2),-pfx(3),dx,dy,dz)
 
 
      endif
 
 9999 continue
 
      return
      end
 
