      subroutine polyfun(itask,action,node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,itetclr,itet,itetoff,x,y,z,
     &   nnodes,nelements,iedge,iedgeoff,iedgemat,ichildary,
     &   ichildno,invchildary,imt1,epsilonv,fvec,reffield,iparent,
     &   hxx,hxy,hxz,hyy,hyz,hzz,range,pf,pfx,pfxx)
C     #####################################################################
C
C     PURPOSE -
C
C     POLYFUN returns the value of the H1 seminorm of the error
C     between a function with specified piecewise constant
C     Hessian and the piecewise linear approximation over the
C     tetrahedra.  The ``POLY'' refers to the fact that the
C     seminorm functional is evaluated over the polyhedron
C     consisting of the union of tetrahedra sharing node NODE.
C     Also returned are the derivatives and Hessian matrix of
C     the H1 seminorm, where the derivatives are taken with
C     respect to the x-, y-, and z- coordinates of NODE.
C     (By altering the position of NODE, the functional may be
C     minimized and the mesh smoothed.  The functional being used
C     here is a generalization to 3D of the 2D analysis
C     of Bank and Smith.)
C
C     INPUT ARGUMENTS -
C
C     ITASK  -  Task.  Choices are:
C        1. Initial ('constructor') call. Use for setting up arrays
C           needed by user for computing PF and derivatives.
C        2. Outer loop call.  Use for possibly (re)computing
C           quantities in user arrays needed for computation
C           of PF, etc.
C        3. Inner loop call.  Compute PF, and derivatives.
C        4. Short Inner loop call.  Compute PF only.
C        5. Final ('destructor') call.  Use for releasing any
C           arrays that user allocated for computing
C     PF, etc.
C     NFAC  -  The number of faces of the polhedron (= the number
C        of tets sharing NODE).
C     ITET  -  The itet array for the tetrahedral mesh considered.
C     X,Y,Z -  The xic, yic, zic arrays for the tet mesh.
C     NODE  -  The node over whose polyhedron we evaluate the
C        functional.
C     FVEC - Values at the nodes of the adaption function.
C
C     OUTPUT ARGUMENTS -
C
C     PF       The value of the smoothing functional.
C     PFX, ... First derivatives of the smoothing functional.
C     PFXX,... Second derivatives of the smoothing functional.
C
C     CHANGE HISTORY -
C     $Log:   /pvcs.config/t3d/src/polyfun_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.12   04 Mar 2002 16:25:26   dcg
CPVCS    adaptive merging
CPVCS    
CPVCS       Rev 1.12   01 Mar 2002 14:44:24   dcg
CPVCS    adaptive merging
CPVCS    
CPVCS       Rev 1.11   21 Dec 2001 18:19:26   kuprat
CPVCS    Major revision.  We take out Hessian evaluation from
CPVCS    polyfun.  Instead it is done now by the SETHESSIAN 
CPVCS    command.
CPVCS    
CPVCS       Rev 1.10   19 Dec 2001 14:31:08   kuprat
CPVCS    Divide through approximate functional by 576, so that computer
CPVCS    functional is better estimate of Sobolev seminorm.
CPVCS    
CPVCS       Rev 1.9   06 Nov 2001 17:59:22   kuprat
CPVCS    Rev. 1.8 was wrong; this version is same as 1.7.
CPVCS    
CPVCS       Rev 1.7   06 Nov 2001 15:45:04   kuprat
CPVCS    Fixed syntax error.
CPVCS    
CPVCS       Rev 1.6   06 Nov 2001 15:38:02   kuprat
CPVCS    Pass ITETCLR to subroutine for possible edge distinction.
CPVCS    
CPVCS       Rev 1.5   05 Nov 2001 16:00:34   kuprat
CPVCS    Print error message and stop if ERRADPT specified with non-tet elements.
CPVCS    
CPVCS       Rev 1.4   30 Jul 2001 15:44:14   kuprat
CPVCS    Improved Regularization.  Added 'exact' mega option.
CPVCS    
CPVCS       Rev 1.3   21 Jun 2001 18:05:18   kuprat
CPVCS    We changed some 'else' statements to 'elseif' statements to
CPVCS    make the branching clearer.  We have put in a brach that 
CPVCS    corresponds to 'exact mega', but have yet to put in the correct
CPVCS    code for it.
CPVCS    
CPVCS       Rev 1.2   01 Jun 2001 17:56:06   kuprat
CPVCS    Indented code.
CPVCS    
CPVCS       Rev 1.1   21 Dec 2000 23:20:24   kuprat
CPVCS    Added ERRADPT option.
CPVCS    
CPVCS       Rev 1.0   27 Jan 2000 13:21:16   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.16   Wed Nov 10 15:23:36 1999   dcg
CPVCS    declare time local variable - get value from storage block
CPVCS
CPVCS       Rev 1.15   Tue Sep 02 23:14:44 1997   kuprat
CPVCS    Made changes in keeping with new SMOOTH/RADAPT bifurcation.
CPVCS
CPVCS       Rev 1.14   Tue Jun 03 13:09:34 1997   kuprat
CPVCS    Added capability of evaluating second derivatives from a CMO
c field and
CPVCS    then using that as a basis for adaptive smoothing.
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 16:56:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Thu Oct 31 23:07:34 1996   kuprat
CPVCS    We now handle general elements, and loop over 'virtual
CPVCS    tetrahedra' in the elements.  Thus we can now smooth
CPVCS    hexahedral grids.  The adaption function now can now
CPVCS    be sensitive to material type.
CPVCS
CPVCS       Rev 1.11   Tue Aug 06 18:12:16 1996   kuprat
CPVCS    New geometry functional:  We minimize the sum of squares of
CPVCS    the 'numerical analysis' aspect ratios.  These ratios are
CPVCS    roughly the longest edge length divided by the shallowest
CPVCS    tet altitude.
CPVCS
CPVCS       Rev 1.10   Wed Jul 31 19:45:04 1996   kuprat
CPVCS    Add 'geometry' option.
CPVCS
CPVCS       Rev 1.9   Tue May 28 21:30:26 1996   kuprat
CPVCS    Improved regularization.
CPVCS
CPVCS       Rev 1.8   Tue May 14 16:37:36 1996   kuprat
CPVCS    Removed CHARACTER*(*) declaration.
CPVCS
CPVCS       Rev 1.7   Sun Apr 28 20:30:48 1996   kuprat
CPVCS    Added special code for Hessian = Identity option.
CPVCS
CPVCS       Rev 1.6   Wed Jan 03 10:08:22 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS
CPVCS       Rev 1.5   11/16/95 15:21:58   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.4   11/07/95 17:22:44   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.3   11/07/95 14:53:38   kuprat
CPVCS    Changed regularization terms.  Version used for Tempe
c conference
CPVCS    results.
CPVCS
CPVCS       Rev 1.2   10/13/95 00:58:52   kuprat
CPVCS    Pulled preprocessing operations into routine
C
C     ######################################################################
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'smooth.h'
 
      pointer (iplocvoloff,locvoloff), (ipivoloffoff,ivoloffoff),
     &   (ipvoloff,voloff)
      integer iedge(*), iedgeoff(*),nodhyb(*),nodhyboff(*),locvoloff(*)
     &   ,ivoloffoff(*),iedgemat(*)
      real*8 voloff(*)
      pointer (ipsrho,srho),(iprho1,rho1)
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*),srho(*),rho1(*)
      real*8 crosx,crosy,crosz,crosx_x1,crosx_y1,crosx_z1,crosy_x1,
     &   crosy_y1,crosy_z1,crosz_x1,crosz_y1,crosz_z1,
     &   x1,y1,z1,x2,y2,z2,
     &   x3,y3,z3,dfg,dfovg,hfg,hfovg,f,fi,fj,fij,g,gi,gj,gij,
     &   a1x,a1y,a1z,afac(3,4),
     &   ssq,a2x,a2y,a2z,a3x,a3y,a3z,a4x,a4y,a4z,
     &   a2x_x,a2x_y,a2x_z,a2y_x,a2y_y,a2y_z,a2z_x,a2z_y,a2z_z,
     &   a3x_x,a3x_y,a3x_z,a3y_x,a3y_y,a3y_z,a3z_x,a3z_y,a3z_z,
     &   a4x_x,a4x_y,a4x_z,a4y_x,a4y_y,a4y_z,a4z_x,a4z_y,a4z_z,
     &   ssq_x,ssq_y,ssq_z,ssq_xx,ssq_xy,ssq_xz,ssq_yy,ssq_yz,
     &   ssq_zz,vol,vol_x,vol_y,vol_z,geom,geom_x,geom_y,geom_z,
     &   geom_xx,geom_xy,geom_xz,geom_yy,geom_yz,geom_zz,ex,ey,ez,
     &   v1,v2,v3,v4,v5,v6,v1_x,v1_y,v1_z,v2_x,v2_y,v2_z,
     &   v3_x,v3_y,v3_z,edgerr,edgerr_x,edgerr_y,edgerr_z,
     &   edgerr_xx,edgerr_xy,edgerr_xz,edgerr_yy,edgerr_yz,
     &   edgerr_zz,xi1,yi1,zi1,x(*),y(*),z(*),
     &   fvec(*),pf,pfx(3),pfxx(3,3),rho,reffield(*),ratio,srho_node
     &   ,rho_node
 
      real*8 sum,sum_x,sum_y,sum_z
 
      real*8 v1id,v2id,v3id,v4id,v5id,v6id,edgerrid,regrel,regabs,
     &   v1reg,v2reg,v3reg,v4reg,v5reg,v6reg,v1reg_x,v1reg_y,
     &   v1reg_z,v2reg_x,v2reg_y,v2reg_z,v3reg_x,v3reg_y,v3reg_z,
     &   v1id_x,v1id_y,v1id_z,v2id_x,v2id_y,v2id_z,
     &   v3id_x,v3id_y,v3id_z,xmax_,xmin_,
     &   ymax_,ymin_,zmax_,zmin_,range,diamsq
      real*8 vol2,vol2_x,vol2_y,vol2_z,vol2_xx,vol2_xy,vol2_xz,
     &   vol2_yy,vol2_yz,vol2_zz,htinv,htinv_x,htinv_y,htinv_z,
     &   htinv_xx,htinv_xy,htinv_xz,htinv_yy,htinv_yz,htinv_zz
 
      parameter (regrel=0.01d0)
      parameter (regabs=0.01d0)
 
      real*8 six
      parameter (six=6.0d0)
 
      integer itet(*),itetoff(*),ieltary(*),itettyp(*),itetclr(*),imt1(
     &   *),ichildary(*),invchildary(*),iparent(*)
 
      integer i,i2,i3,i4,node,loctet,itask,nnodes,icscode,nelements,ii
     &   ,ieltno,ichildno,j,invmpary(*),k,ihyb,lochybnod,kk,jtetvi
     &   ,locnod,mpk,k1
 
      integer icharlnf
 
      real*8 voff(maxhybnumtetv),aa(4,4),volfac,b(6,6),v(6),vid(6),bv(6)
     &   ,bvid(6),pftet,pfidtet
 
      real*8 epsilonv
 
      character*32 isubname,action
 
      save
 
c...  Statement functions for the components of the cross product
c...  ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
 
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
c...  Derivatives of the components of the cross product with respect to
c...  x1,y1, and z1.
 
      crosx_x1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=0.
      crosx_y1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=z2-z3
      crosx_z1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=y3-y2
      crosy_x1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=z3-z2
      crosy_y1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=0.
      crosy_z1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=x2-x3
      crosz_x1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=y2-y3
      crosz_y1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=x3-x2
      crosz_z1(x1,y1,z1,x2,y2,z2,x3,y3,z3)=0.
 
c...  d(f*g)/dxi and d(f/g)/dxi in terms of f, df/dxi, g, dg/dxi
 
      dfg(f,fi,g,gi)=f*gi+fi*g
      dfovg(f,fi,g,gi)=(-f*gi/g+fi)/g
 
c...  d2(f*g)/(dxi*dxj) and d2(f/g)/(dxi*dxj) in terms of f, df/dxi,
c...  df/dxj, d2f/(dxi*dxj), g, dg/dxi, dg/dxj, d2g/(dxi*dxj).
 
      hfg(f,fi,fj,fij,g,gi,gj,gij)=fj*gi+f*gij+fij*g+fi*gj
      hfovg(f,fi,fj,fij,g,gi,gj,gij)=
     &   ((2.*f*gj*gi/g-fj*gi-f*gij-gj*fi)/g+fij)/g
 
c$$$      real*8 safe,xsafe
c$$$
c$$$      safe(xsafe)=max(abs(xsafe),1.d-50)*sign(1.d0,xsafe)
 
      isubname='polyfun'
 
 
c.... We handle several types of functionals.
c.... The functionals include (1) 'geometry'
c.... which minimizes the sum of squares of aspect ratios
c.... of virtual tetrahedra in the mesh, (2) 'generic mega'
c.... which minimizes the L**2 norm of the gradient of the
c.... error EVALUATED WITH A GENERIC FUNCTION which has
c.... constant Hessian, and (3) 'true mega' which minimizes
c.... the mega functional using actual function values obtained
c.... using calls to FADPT.
 
      if (action(1:8).eq.'geometry') then
 
c.... Geometry option.
 
         if (itask.eq.1) then
 
c.... No arrays need be set up in 'geometry' option.
 
         elseif (itask.eq.2) then
 
c.... In the outer loop call, we recompute volume offsets for
c.... any bad tets that may now exist in the mesh.
 
            call getvoloff(ieltno,ieltary,x,y,z,itet,itetoff,
     &         itettyp,epsilonv,isubname,ipvoloff,iplocvoloff,
     &         ipivoloffoff)
 
         elseif (itask.eq.3) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.
            pfx(1)=0.
            pfx(2)=0.
            pfx(3)=0.
            pfxx(1,1)=0.
            pfxx(2,1)=0.
            pfxx(3,1)=0.
            pfxx(2,2)=0.
            pfxx(3,2)=0.
            pfxx(3,3)=0.
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  ssq=a1x**2+a1y**2+a1z**2
 
c...  Compute area vectors of faces containing NODE.
 
                  a2x=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2y=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2z=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a3x=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3y=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3z=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a4x=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4y=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4z=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
 
c...  Compute sum of squares of area vectors
 
                  ssq=ssq+
     &               a2x**2+a2y**2+a2z**2+
     &               a3x**2+a3y**2+a3z**2+
     &               a4x**2+a4y**2+a4z**2
 
c...  Compute derivatives of area vectors.
 
                  a2x_x=crosx_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2x_y=crosx_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2x_z=crosx_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_x=crosy_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_y=crosy_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_z=crosy_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_x=crosz_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_y=crosz_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_z=crosz_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a3x_x=crosx_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3x_y=crosx_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3x_z=crosx_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_x=crosy_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_y=crosy_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_z=crosy_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_x=crosz_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_y=crosz_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_z=crosz_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a4x_x=crosx_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4x_y=crosx_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4x_z=crosx_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_x=crosy_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_y=crosy_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_z=crosy_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_x=crosz_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_y=crosz_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_z=crosz_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
 
c...  Compute derivatives of sum of squares of area vectors.
 
                  ssq_x=2.*(a2x*a2x_x+a2y*a2y_x+a2z*a2z_x+
     &               a3x*a3x_x+a3y*a3y_x+a3z*a3z_x+
     &               a4x*a4x_x+a4y*a4y_x+a4z*a4z_x)
                  ssq_y=2.*(a2x*a2x_y+a2y*a2y_y+a2z*a2z_y+
     &               a3x*a3x_y+a3y*a3y_y+a3z*a3z_y+
     &               a4x*a4x_y+a4y*a4y_y+a4z*a4z_y)
                  ssq_z=2.*(a2x*a2x_z+a2y*a2y_z+a2z*a2z_z+
     &               a3x*a3x_z+a3y*a3y_z+a3z*a3z_z+
     &               a4x*a4x_z+a4y*a4y_z+a4z*a4z_z)
                  ssq_xx=2.*(a2x_x*a2x_x+a2y_x*a2y_x+a2z_x*a2z_x+
     &               a3x_x*a3x_x+a3y_x*a3y_x+a3z_x*a3z_x+
     &               a4x_x*a4x_x+a4y_x*a4y_x+a4z_x*a4z_x)
                  ssq_xy=2.*(a2x_y*a2x_x+a2y_y*a2y_x+a2z_y*a2z_x+
     &               a3x_y*a3x_x+a3y_y*a3y_x+a3z_y*a3z_x+
     &               a4x_y*a4x_x+a4y_y*a4y_x+a4z_y*a4z_x)
                  ssq_xz=2.*(a2x_z*a2x_x+a2y_z*a2y_x+a2z_z*a2z_x+
     &               a3x_z*a3x_x+a3y_z*a3y_x+a3z_z*a3z_x+
     &               a4x_z*a4x_x+a4y_z*a4y_x+a4z_z*a4z_x)
                  ssq_yy=2.*(a2x_y*a2x_y+a2y_y*a2y_y+a2z_y*a2z_y+
     &               a3x_y*a3x_y+a3y_y*a3y_y+a3z_y*a3z_y+
     &               a4x_y*a4x_y+a4y_y*a4y_y+a4z_y*a4z_y)
                  ssq_yz=2.*(a2x_z*a2x_y+a2y_z*a2y_y+a2z_z*a2z_y+
     &               a3x_z*a3x_y+a3y_z*a3y_y+a3z_z*a3z_y+
     &               a4x_z*a4x_y+a4y_z*a4y_y+a4z_z*a4z_y)
                  ssq_zz=2.*(a2x_z*a2x_z+a2y_z*a2y_z+a2z_z*a2z_z+
     &               a3x_z*a3x_z+a3y_z*a3y_z+a3z_z*a3z_z+
     &               a4x_z*a4x_z+a4y_z*a4y_z+a4z_z*a4z_z)
 
c...  Compute (square of) volume of tetrahedron and derivatives.
 
                  vol=(x(i2)-xi1)*a1x+(y(i2)-yi1)*a1y+(z(i2)-zi1)*a1z
     &               -voff(loctet)
                  vol_x=-a1x
                  vol_y=-a1y
                  vol_z=-a1z
                  vol2=vol**2
                  vol2_x=2.*vol*vol_x
                  vol2_y=2.*vol*vol_y
                  vol2_z=2.*vol*vol_z
                  vol2_xx=2.*vol_x*vol_x
                  vol2_xy=2.*vol_x*vol_y
                  vol2_xz=2.*vol_x*vol_z
                  vol2_yy=2.*vol_y*vol_y
                  vol2_yz=2.*vol_y*vol_z
                  vol2_zz=2.*vol_z*vol_z
 
c...  Compute the edge length squares and derivatives.
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v4=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v5=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v6=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v1=ex*ex+ey*ey+ez*ez
                  v1_x=-2.*ex
                  v1_y=-2.*ey
                  v1_z=-2.*ez
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v2=ex*ex+ey*ey+ez*ez
                  v2_x=-2.*ex
                  v2_y=-2.*ey
                  v2_z=-2.*ez
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v3=ex*ex+ey*ey+ez*ez
                  v3_x=-2.*ex
                  v3_y=-2.*ey
                  v3_z=-2.*ez
 
c...  Compute sum of squares of edge lengths and derivatives.
 
                  sum=v1+v2+v3+v4+v5+v6
                  sum_x=v1_x+v2_x+v3_x
                  sum_y=v1_y+v2_y+v3_y
                  sum_z=v1_z+v2_z+v3_z
 
C...  Compute the sum of squares of the inverse heights, and derivatives
c .
 
                  htinv=ssq/vol2
 
                  htinv_x=dfovg(ssq,ssq_x,vol2,vol2_x)
                  htinv_y=dfovg(ssq,ssq_y,vol2,vol2_y)
                  htinv_z=dfovg(ssq,ssq_z,vol2,vol2_z)
 
                  htinv_xx=hfovg(ssq,ssq_x,ssq_x,ssq_xx,
     &               vol2,vol2_x,vol2_x,vol2_xx)
                  htinv_xy=hfovg(ssq,ssq_x,ssq_y,ssq_xy,
     &               vol2,vol2_x,vol2_y,vol2_xy)
                  htinv_xz=hfovg(ssq,ssq_x,ssq_z,ssq_xz,
     &               vol2,vol2_x,vol2_z,vol2_xz)
                  htinv_yy=hfovg(ssq,ssq_y,ssq_y,ssq_yy,
     &               vol2,vol2_y,vol2_y,vol2_yy)
                  htinv_yz=hfovg(ssq,ssq_y,ssq_z,ssq_yz,
     &               vol2,vol2_y,vol2_z,vol2_yz)
                  htinv_zz=hfovg(ssq,ssq_z,ssq_z,ssq_zz,
     &               vol2,vol2_z,vol2_z,vol2_zz)
 
C...  Compute the (squares of the) `numerical analysis aspect ratio'
c...  and derivatives.  Add into total functional value.
 
                  pf=pf+sum*htinv*wttetv(loctet,itettyp(ihyb))
 
                  pfx(1)=pfx(1)+dfg(sum,sum_x,htinv,htinv_x)
     &               *wttetv(loctet,itettyp(ihyb))
                  pfx(2)=pfx(2)+dfg(sum,sum_y,htinv,htinv_y)
     &               *wttetv(loctet,itettyp(ihyb))
                  pfx(3)=pfx(3)+dfg(sum,sum_z,htinv,htinv_z)
     &               *wttetv(loctet,itettyp(ihyb))
 
                  pfxx(1,1)=pfxx(1,1)+hfg(sum,sum_x,sum_x,six,
     &               htinv,htinv_x,htinv_x,htinv_xx)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(2,1)=pfxx(2,1)+hfg(sum,sum_x,sum_y,zero,
     &               htinv,htinv_x,htinv_y,htinv_xy)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,1)=pfxx(3,1)+hfg(sum,sum_x,sum_z,zero,
     &               htinv,htinv_x,htinv_z,htinv_xz)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(2,2)=pfxx(2,2)+hfg(sum,sum_y,sum_y,six,
     &               htinv,htinv_y,htinv_y,htinv_yy)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,2)=pfxx(3,2)+hfg(sum,sum_y,sum_z,zero,
     &               htinv,htinv_y,htinv_z,htinv_yz)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,3)=pfxx(3,3)+hfg(sum,sum_z,sum_z,six,
     &               htinv,htinv_z,htinv_z,htinv_zz)*wttetv(loctet
     &               ,itettyp(ihyb))
 
               enddo
            enddo
 
c...   Copy off-diagonal terms.
 
            pfxx(1,2)=pfxx(2,1)
            pfxx(1,3)=pfxx(3,1)
            pfxx(2,3)=pfxx(3,2)
 
         elseif (itask.eq.4) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  ssq=a1x**2+a1y**2+a1z**2
 
c...  Compute area vectors of faces containing NODE.
 
                  a2x=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2y=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2z=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a3x=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3y=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3z=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a4x=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4y=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4z=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
 
c...  Compute sum of squares of area vectors
 
                  ssq=ssq+
     &               a2x**2+a2y**2+a2z**2+
     &               a3x**2+a3y**2+a3z**2+
     &               a4x**2+a4y**2+a4z**2
 
c...  Compute (square of) volume of tetrahedron.
 
                  vol=(x(i2)-xi1)*a1x+(y(i2)-yi1)*a1y+(z(i2)-zi1)*a1z
     &               -voff(loctet)
 
c.... If VOL is too small, we are done.
 
                  if (vol.lt.0.6*epsilonv) then
                     pf=1.d99
                     goto 9999
                  endif
 
                  vol2=vol**2
 
c...  Compute the edge length squares.
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v4=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v5=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v6=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v1=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v2=ex*ex+ey*ey+ez*ez
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v3=ex*ex+ey*ey+ez*ez
 
c...  Compute sum of squares of edge lengths.
 
                  sum=v1+v2+v3+v4+v5+v6
 
C...  Compute the sum of squares of the inverse heights.
 
                  htinv=ssq/vol2
 
C...  Compute the (square of the) `numerical analysis aspect ratio'.
c...  Add into total functional value.
 
                  pf=pf+sum*htinv*wttetv(loctet,itettyp(ihyb))
 
               enddo
            enddo
 
         elseif (itask.eq.5) then
 
c.... No arrays to be released in this option.
 
         endif
 
      elseif (action(1:6).eq.'idhess') then
 
c.... Generic MEGA (Hessian = Identity)
 
         if (itask.eq.1) then
 
c.... No arrays need be set up.
 
         elseif (itask.eq.2) then
 
c.... In the outer loop call, we recompute volume offsets for
c.... any bad tets that may now exist in the mesh.
 
            call getvoloff(ieltno,ieltary,x,y,z,itet,itetoff,
     &         itettyp,epsilonv,isubname,ipvoloff,iplocvoloff
     &         ,ipivoloffoff)
 
         elseif (itask.eq.3) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.
            pfx(1)=0.
            pfx(2)=0.
            pfx(3)=0.
            pfxx(1,1)=0.
            pfxx(2,1)=0.
            pfxx(3,1)=0.
            pfxx(2,2)=0.
            pfxx(3,2)=0.
            pfxx(3,3)=0.
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  ssq=a1x**2+a1y**2+a1z**2
 
c...  Compute area vectors of faces containing NODE.
 
                  a2x=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2y=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2z=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a3x=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3y=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3z=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a4x=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4y=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4z=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
 
c...  Compute sum of squares of area vectors
 
                  ssq=ssq+
     &               a2x**2+a2y**2+a2z**2+
     &               a3x**2+a3y**2+a3z**2+
     &               a4x**2+a4y**2+a4z**2
 
c...  Compute derivatives of area vectors.
 
                  a2x_x=crosx_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2x_y=crosx_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2x_z=crosx_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_x=crosy_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_y=crosy_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_z=crosy_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_x=crosz_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_y=crosz_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_z=crosz_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a3x_x=crosx_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3x_y=crosx_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3x_z=crosx_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_x=crosy_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_y=crosy_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_z=crosy_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_x=crosz_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_y=crosz_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_z=crosz_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a4x_x=crosx_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4x_y=crosx_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4x_z=crosx_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_x=crosy_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_y=crosy_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_z=crosy_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_x=crosz_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_y=crosz_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_z=crosz_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
 
c...  Compute derivatives of sum of squares of area vectors.
 
                  ssq_x=2.*(a2x*a2x_x+a2y*a2y_x+a2z*a2z_x+
     &               a3x*a3x_x+a3y*a3y_x+a3z*a3z_x+
     &               a4x*a4x_x+a4y*a4y_x+a4z*a4z_x)
                  ssq_y=2.*(a2x*a2x_y+a2y*a2y_y+a2z*a2z_y+
     &               a3x*a3x_y+a3y*a3y_y+a3z*a3z_y+
     &               a4x*a4x_y+a4y*a4y_y+a4z*a4z_y)
                  ssq_z=2.*(a2x*a2x_z+a2y*a2y_z+a2z*a2z_z+
     &               a3x*a3x_z+a3y*a3y_z+a3z*a3z_z+
     &               a4x*a4x_z+a4y*a4y_z+a4z*a4z_z)
                  ssq_xx=2.*(a2x_x*a2x_x+a2y_x*a2y_x+a2z_x*a2z_x+
     &               a3x_x*a3x_x+a3y_x*a3y_x+a3z_x*a3z_x+
     &               a4x_x*a4x_x+a4y_x*a4y_x+a4z_x*a4z_x)
                  ssq_xy=2.*(a2x_y*a2x_x+a2y_y*a2y_x+a2z_y*a2z_x+
     &               a3x_y*a3x_x+a3y_y*a3y_x+a3z_y*a3z_x+
     &               a4x_y*a4x_x+a4y_y*a4y_x+a4z_y*a4z_x)
                  ssq_xz=2.*(a2x_z*a2x_x+a2y_z*a2y_x+a2z_z*a2z_x+
     &               a3x_z*a3x_x+a3y_z*a3y_x+a3z_z*a3z_x+
     &               a4x_z*a4x_x+a4y_z*a4y_x+a4z_z*a4z_x)
                  ssq_yy=2.*(a2x_y*a2x_y+a2y_y*a2y_y+a2z_y*a2z_y+
     &               a3x_y*a3x_y+a3y_y*a3y_y+a3z_y*a3z_y+
     &               a4x_y*a4x_y+a4y_y*a4y_y+a4z_y*a4z_y)
                  ssq_yz=2.*(a2x_z*a2x_y+a2y_z*a2y_y+a2z_z*a2z_y+
     &               a3x_z*a3x_y+a3y_z*a3y_y+a3z_z*a3z_y+
     &               a4x_z*a4x_y+a4y_z*a4y_y+a4z_z*a4z_y)
                  ssq_zz=2.*(a2x_z*a2x_z+a2y_z*a2y_z+a2z_z*a2z_z+
     &               a3x_z*a3x_z+a3y_z*a3y_z+a3z_z*a3z_z+
     &               a4x_z*a4x_z+a4y_z*a4y_z+a4z_z*a4z_z)
 
c...  Compute volume and derivatives of volume of tetrahedron.
 
                  vol=(x(i2)-xi1)*a1x+(y(i2)-yi1)*a1y+(z(i2)-zi1)*a1z
     &               -voff(loctet)
                  vol_x=-a1x
                  vol_y=-a1y
                  vol_z=-a1z
 
c...  Compute the 'geometric factor', and its derivatives.
 
                  geom=ssq/vol
                  geom_x=dfovg(ssq,ssq_x,vol,vol_x)
                  geom_y=dfovg(ssq,ssq_y,vol,vol_y)
                  geom_z=dfovg(ssq,ssq_z,vol,vol_z)
                  geom_xx=hfovg(ssq,ssq_x,ssq_x,ssq_xx,vol,vol_x,vol_x
     &               ,zero)
                  geom_xy=hfovg(ssq,ssq_x,ssq_y,ssq_xy,vol,vol_x,vol_y
     &               ,zero)
                  geom_xz=hfovg(ssq,ssq_x,ssq_z,ssq_xz,vol,vol_x,vol_z
     &               ,zero)
                  geom_yy=hfovg(ssq,ssq_y,ssq_y,ssq_yy,vol,vol_y,vol_y
     &               ,zero)
                  geom_yz=hfovg(ssq,ssq_y,ssq_z,ssq_yz,vol,vol_y,vol_z
     &               ,zero)
                  geom_zz=hfovg(ssq,ssq_z,ssq_z,ssq_zz,vol,vol_z,vol_z
     &               ,zero)
 
c...  Compute the edge errors (and derivatives) over the edges {1,...,6}
c .
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v4=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v5=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v6=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v1=ex*ex+ey*ey+ez*ez
                  v1_x=-2.*ex
                  v1_y=-2.*ey
                  v1_z=-2.*ez
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v2=ex*ex+ey*ey+ez*ez
                  v2_x=-2.*ex
                  v2_y=-2.*ey
                  v2_z=-2.*ez
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v3=ex*ex+ey*ey+ez*ez
                  v3_x=-2.*ex
                  v3_y=-2.*ey
                  v3_z=-2.*ez
 
c...  v1_xy=2.*hxy(jt), etc.
 
C...  Compute the sum of squares of edge errors, and sum of squares of
c...  edge errors if we assume the Hessian is equal to the identity.
 
                  edgerr=v1**2+v2**2+v3**2+
     &               v4**2+v5**2+v6**2
 
                  edgerr_x=2.*(v1*v1_x+v2*v2_x+v3*v3_x)
                  edgerr_y=2.*(v1*v1_y+v2*v2_y+v3*v3_y)
                  edgerr_z=2.*(v1*v1_z+v2*v2_z+v3*v3_z)
 
                  edgerr_xx=2.*( v1_x*v1_x+v2_x*v2_x+v3_x*v3_x+
     &               2.*(v1+v2+v3) )
                  edgerr_xy=2.*( v1_x*v1_y+v2_x*v2_y+v3_x*v3_y )
                  edgerr_xz=2.*( v1_x*v1_z+v2_x*v2_z+v3_x*v3_z )
                  edgerr_yy=2.*( v1_y*v1_y+v2_y*v2_y+v3_y*v3_y+
     &               2.*(v1+v2+v3) )
                  edgerr_yz=2.*( v1_y*v1_z+v2_y*v2_z+v3_y*v3_z )
                  edgerr_zz=2.*( v1_z*v1_z+v2_z*v2_z+v3_z*v3_z+
     &               2.*(v1+v2+v3) )
 
C...  Compute the H1 seminorm functional and its derivatives.
 
                  pf=pf+geom*edgerr*wttetv(loctet,itettyp(ihyb))
 
                  pfx(1)=pfx(1)+dfg(geom,geom_x,edgerr,edgerr_x)
     &               *wttetv(loctet,itettyp(ihyb))
                  pfx(2)=pfx(2)+dfg(geom,geom_y,edgerr,edgerr_y)
     &               *wttetv(loctet,itettyp(ihyb))
                  pfx(3)=pfx(3)+dfg(geom,geom_z,edgerr,edgerr_z)
     &               *wttetv(loctet,itettyp(ihyb))
 
                  pfxx(1,1)=pfxx(1,1)+hfg(geom,geom_x,geom_x,geom_xx,
     &               edgerr,edgerr_x,edgerr_x,edgerr_xx)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(2,1)=pfxx(2,1)+hfg(geom,geom_x,geom_y,geom_xy,
     &               edgerr,edgerr_x,edgerr_y,edgerr_xy)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,1)=pfxx(3,1)+hfg(geom,geom_x,geom_z,geom_xz,
     &               edgerr,edgerr_x,edgerr_z,edgerr_xz)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(2,2)=pfxx(2,2)+hfg(geom,geom_y,geom_y,geom_yy,
     &               edgerr,edgerr_y,edgerr_y,edgerr_yy)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,2)=pfxx(3,2)+hfg(geom,geom_y,geom_z,geom_yz,
     &               edgerr,edgerr_y,edgerr_z,edgerr_yz)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,3)=pfxx(3,3)+hfg(geom,geom_z,geom_z,geom_zz,
     &               edgerr,edgerr_z,edgerr_z,edgerr_zz)*wttetv(loctet
     &               ,itettyp(ihyb))
 
               enddo
            enddo
 
c...   Copy off-diagonal terms.
 
            pfxx(1,2)=pfxx(2,1)
            pfxx(1,3)=pfxx(3,1)
            pfxx(2,3)=pfxx(3,2)
 
         elseif (itask.eq.4) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do 520 k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  ssq=a1x**2+a1y**2+a1z**2
 
c...  Compute area vectors of faces containing NODE.
 
                  a2x=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2y=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2z=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a3x=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3y=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3z=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a4x=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4y=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4z=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
 
c...  Compute sum of squares of area vectors
 
                  ssq=ssq+
     &               a2x**2+a2y**2+a2z**2+
     &               a3x**2+a3y**2+a3z**2+
     &               a4x**2+a4y**2+a4z**2
 
c...  Compute volume of tetrahedron.
                  vol=(x(i2)-xi1)*a1x+(y(i2)-yi1)*a1y+(z(i2)-zi1)*a1z
     &               -voff(loctet)
 
c.... If VOL is too small, we are done.
 
                  if (vol.lt.0.6*epsilonv) then
                     pf=1.d99
                     goto 9999
c$$$                     pf=pf+1.d99*wttetv(loctet,itettyp(ihyb))
c$$$                     goto 520
                  endif
 
c...  Compute the 'geometric factor'.
                  geom=ssq/vol
 
c...  Compute the edge errors over the edges {1,...,6}.
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v4=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v5=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v6=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v1=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v2=ex*ex+ey*ey+ez*ez
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v3=ex*ex+ey*ey+ez*ez
 
C...  Compute the sum of squares of edge errors, and sum of squares of
c...  edge errors if we assume the Hessian is equal to the identity.
 
                  edgerr=v1**2+v2**2+v3**2+v4**2+v5**2+v6**2
 
C...  Compute the H1 seminorm functional.
 
                  pf=pf+geom*edgerr*wttetv(loctet,itettyp(ihyb))
 
 520           continue
            enddo
 
         elseif (itask.eq.5) then
 
c.... No arrays to be released in this option.
 
         endif
 
      elseif ((action(1:5).eq.'field'.or.action(1:4).eq.'user'.or
     &      .action(1:7).eq.'erradpt').and
     &      .index(action(1:icharlnf(action)),':exact').eq.0) then
 
c.... MEGA option with approximate functional.
 
         if (itask.eq.1) then
 
            call mmgetblk('srho',isubname,ipsrho,ieltno,2,icscode)
 
         elseif (itask.eq.2) then
 
c...Sample diameter of domain.  We do this
c...in order to be able to have 'scale invariant' regularization.
 
                  xmax_=-1.d99
                  ymax_=-1.d99
                  zmax_=-1.d99
                  xmin_=1.d99
                  ymin_=1.d99
                  zmin_=1.d99
                  do i=1,ichildno
                     xmax_=max(xmax_,x(ichildary(i)))
                     xmin_=min(xmin_,x(ichildary(i)))
                     ymax_=max(ymax_,y(ichildary(i)))
                     ymin_=min(ymin_,y(ichildary(i)))
                     zmax_=max(zmax_,z(ichildary(i)))
                     zmin_=min(zmin_,z(ichildary(i)))
                  enddo
                  diamsq=(xmax_-xmin_)**2+(ymax_-ymin_)**2+(zmax_-zmin_)
     &               **2
 
c...if the adaption function has no variation, give the function a small
c...range to avoid problems with smoothing.
 
               range=max(range,1.d-30)
 
               do ii=1,ieltno
 
c.... We compute for the current element an estimate
c.... of the relative value of the functional using the true
c.... adaption function and the value of the functional using the
c.... Identity matrix for a Hessian.  Since the Hessian appears
c.... quadratically, a good estimate for the ratio is
c....  RATIO=||H||_F^2 / ||I||_F^2 = ||H||_F^2 / 3,
c.... where _F denotes the Frobenius norm.
                  ihyb=ieltary(ii)
                  ratio=(hxx(ihyb)**2+2.*hxy(ihyb)**2+2.*hxz(ihyb)**2+
     &               hyy(ihyb)**2+2.*hyz(ihyb)**2+hzz(ihyb)**2)/3.d0
 
c...  Compute the local regularization constant 'rho' which will determine
c...  the size of the regularization term.  The regularization term is
c equal
c...  to 'rho' times the H1 norm functional, but with the Hessian
c...  set equal to the identity.
 
c...  First compute the 'relative' contribution to regularization.
c...  Here 'rho' can vary from element to element, and we select it so
c that
c...  the magnitude of the regularization term is equal to 'regrel'*RATIO.
 
                  rho=regrel*ratio
 
c...  Now increase rho by a constant to provide an absolute, global
c regularization
c...  in case of a totally linear region.  Note that paraboloid function
c ,
c...
c...     f(x,y,z)=(range/diamsq) * [(x-x0)**2+(y-y0)**2+(z-z0)**2],
c...
c...  with (x0,y0,z0) being at one corner of the diagonal of the domain,
c...  has the same scale as the function being adapted to (over the
c...  bounding box of the domain) and has Hessian equal to
c...
c...     2*range/diamsq * (Identity Matrix).
c...
c...  RHO is incremented proportional to the square of this, because
c...  the Hessian appears quadratically in the functional expression.
 
                  rho=rho+regabs*(2.d0*range/diamsq)**2
 
c...  Finally compute 'srho', the square root of 'rho' which
c...  is actually used in computation.  The total regularization
c...  on each element is equivalent to the H1 norm functional
c...  with Hessian set to 'srho'*I.
 
                  srho(ii)=sqrt(rho)
 
               enddo
 
c.... Compute volume offset list for bad tets in the mesh.
 
               call getvoloff(ieltno,ieltary,x,y,z,itet,itetoff,
     &            itettyp,epsilonv,isubname,ipvoloff,iplocvoloff
     &            ,ipivoloffoff)
 
         elseif (itask.eq.3) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.
            pfx(1)=0.
            pfx(2)=0.
            pfx(3)=0.
            pfxx(1,1)=0.
            pfxx(2,1)=0.
            pfxx(3,1)=0.
            pfxx(2,2)=0.
            pfxx(3,2)=0.
            pfxx(3,3)=0.
 
c...  Loop over elements in polyhedron and fix
c...  regularization strength.
 
            mpk=invmpary(node)
            srho_node=0.d0
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               srho_node=max(srho_node,srho(ii))
            enddo
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  ssq=a1x**2+a1y**2+a1z**2
 
c...  Compute area vectors of faces containing NODE.
 
                  a2x=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2y=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2z=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a3x=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3y=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3z=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a4x=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4y=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4z=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
 
c...  Compute sum of squares of area vectors
 
                  ssq=ssq+
     &               a2x**2+a2y**2+a2z**2+
     &               a3x**2+a3y**2+a3z**2+
     &               a4x**2+a4y**2+a4z**2
 
c...  Compute derivatives of area vectors.
 
                  a2x_x=crosx_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2x_y=crosx_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2x_z=crosx_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_x=crosy_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_y=crosy_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2y_z=crosy_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_x=crosz_x1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_y=crosz_y1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a2z_z=crosz_z1(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))
                  a3x_x=crosx_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3x_y=crosx_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3x_z=crosx_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_x=crosy_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_y=crosy_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3y_z=crosy_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_x=crosz_x1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_y=crosz_y1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a3z_z=crosz_z1(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))
                  a4x_x=crosx_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4x_y=crosx_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4x_z=crosx_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_x=crosy_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_y=crosy_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4y_z=crosy_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_x=crosz_x1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_y=crosz_y1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
                  a4z_z=crosz_z1(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))
 
c...  Compute derivatives of sum of squares of area vectors.
 
                  ssq_x=2.*(a2x*a2x_x+a2y*a2y_x+a2z*a2z_x+
     &               a3x*a3x_x+a3y*a3y_x+a3z*a3z_x+
     &               a4x*a4x_x+a4y*a4y_x+a4z*a4z_x)
                  ssq_y=2.*(a2x*a2x_y+a2y*a2y_y+a2z*a2z_y+
     &               a3x*a3x_y+a3y*a3y_y+a3z*a3z_y+
     &               a4x*a4x_y+a4y*a4y_y+a4z*a4z_y)
                  ssq_z=2.*(a2x*a2x_z+a2y*a2y_z+a2z*a2z_z+
     &               a3x*a3x_z+a3y*a3y_z+a3z*a3z_z+
     &               a4x*a4x_z+a4y*a4y_z+a4z*a4z_z)
                  ssq_xx=2.*(a2x_x*a2x_x+a2y_x*a2y_x+a2z_x*a2z_x+
     &               a3x_x*a3x_x+a3y_x*a3y_x+a3z_x*a3z_x+
     &               a4x_x*a4x_x+a4y_x*a4y_x+a4z_x*a4z_x)
                  ssq_xy=2.*(a2x_y*a2x_x+a2y_y*a2y_x+a2z_y*a2z_x+
     &               a3x_y*a3x_x+a3y_y*a3y_x+a3z_y*a3z_x+
     &               a4x_y*a4x_x+a4y_y*a4y_x+a4z_y*a4z_x)
                  ssq_xz=2.*(a2x_z*a2x_x+a2y_z*a2y_x+a2z_z*a2z_x+
     &               a3x_z*a3x_x+a3y_z*a3y_x+a3z_z*a3z_x+
     &               a4x_z*a4x_x+a4y_z*a4y_x+a4z_z*a4z_x)
                  ssq_yy=2.*(a2x_y*a2x_y+a2y_y*a2y_y+a2z_y*a2z_y+
     &               a3x_y*a3x_y+a3y_y*a3y_y+a3z_y*a3z_y+
     &               a4x_y*a4x_y+a4y_y*a4y_y+a4z_y*a4z_y)
                  ssq_yz=2.*(a2x_z*a2x_y+a2y_z*a2y_y+a2z_z*a2z_y+
     &               a3x_z*a3x_y+a3y_z*a3y_y+a3z_z*a3z_y+
     &               a4x_z*a4x_y+a4y_z*a4y_y+a4z_z*a4z_y)
                  ssq_zz=2.*(a2x_z*a2x_z+a2y_z*a2y_z+a2z_z*a2z_z+
     &               a3x_z*a3x_z+a3y_z*a3y_z+a3z_z*a3z_z+
     &               a4x_z*a4x_z+a4y_z*a4y_z+a4z_z*a4z_z)
 
c...  Compute volume and derivatives of volume of tetrahedron.
 
                  vol=(x(i2)-xi1)*a1x+(y(i2)-yi1)*a1y+(z(i2)-zi1)*a1z
     &               -voff(loctet)
                  vol_x=-a1x
                  vol_y=-a1y
                  vol_z=-a1z
 
c...  Compute the 'geometric factor', and its derivatives.
 
                  geom=ssq/vol
                  geom_x=dfovg(ssq,ssq_x,vol,vol_x)
                  geom_y=dfovg(ssq,ssq_y,vol,vol_y)
                  geom_z=dfovg(ssq,ssq_z,vol,vol_z)
                  geom_xx=hfovg(ssq,ssq_x,ssq_x,ssq_xx,vol,vol_x,vol_x
     &               ,zero)
                  geom_xy=hfovg(ssq,ssq_x,ssq_y,ssq_xy,vol,vol_x,vol_y
     &               ,zero)
                  geom_xz=hfovg(ssq,ssq_x,ssq_z,ssq_xz,vol,vol_x,vol_z
     &               ,zero)
                  geom_yy=hfovg(ssq,ssq_y,ssq_y,ssq_yy,vol,vol_y,vol_y
     &               ,zero)
                  geom_yz=hfovg(ssq,ssq_y,ssq_z,ssq_yz,vol,vol_y,vol_z
     &               ,zero)
                  geom_zz=hfovg(ssq,ssq_z,ssq_z,ssq_zz,vol,vol_z,vol_z
     &               ,zero)
 
c...  Compute the edge errors (and derivatives) over the edges {1,...,6}
c .
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v4=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v4id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v5=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v5id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v6=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v6id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v1=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v1_x=-2.*(hxx(ihyb)*ex+hxy(ihyb)*ey+hxz(ihyb)*ez)
                  v1_y=-2.*(hxy(ihyb)*ex+hyy(ihyb)*ey+hyz(ihyb)*ez)
                  v1_z=-2.*(hxz(ihyb)*ex+hyz(ihyb)*ey+hzz(ihyb)*ez)
                  v1id=ex*ex+ey*ey+ez*ez
                  v1id_x=-2.*ex
                  v1id_y=-2.*ey
                  v1id_z=-2.*ez
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v2=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v2_x=-2.*(hxx(ihyb)*ex+hxy(ihyb)*ey+hxz(ihyb)*ez)
                  v2_y=-2.*(hxy(ihyb)*ex+hyy(ihyb)*ey+hyz(ihyb)*ez)
                  v2_z=-2.*(hxz(ihyb)*ex+hyz(ihyb)*ey+hzz(ihyb)*ez)
                  v2id=ex*ex+ey*ey+ez*ez
                  v2id_x=-2.*ex
                  v2id_y=-2.*ey
                  v2id_z=-2.*ez
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v3=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v3_x=-2.*(hxx(ihyb)*ex+hxy(ihyb)*ey+hxz(ihyb)*ez)
                  v3_y=-2.*(hxy(ihyb)*ex+hyy(ihyb)*ey+hyz(ihyb)*ez)
                  v3_z=-2.*(hxz(ihyb)*ex+hyz(ihyb)*ey+hzz(ihyb)*ez)
                  v3id=ex*ex+ey*ey+ez*ez
                  v3id_x=-2.*ex
                  v3id_y=-2.*ey
                  v3id_z=-2.*ez
 
c...  v1_xy=2.*hxy(ihyb), etc.
 
C...  Compute the sum of squares of edge errors, and sum of squares of
c...  edge errors if we assume the Hessian is equal to the identity.
 
                  edgerr=v1**2+v2**2+v3**2+v4**2+v5**2+v6**2
                  edgerrid=v1id**2+v2id**2+v3id**2+
     &               v4id**2+v5id**2+v6id**2
 
c...  Now increment the total edge error to reflect the regularization
c...  contribution.
 
                  edgerr=edgerr+srho_node**2*edgerrid
 
c...  Compute the edge errors and derivatives using the Hessian=srho*I
c...  assumption.
 
                  v1reg=v1id*srho_node
                  v2reg=v2id*srho_node
                  v3reg=v3id*srho_node
                  v4reg=v4id*srho_node
                  v5reg=v5id*srho_node
                  v6reg=v6id*srho_node
                  v1reg_x=v1id_x*srho_node
                  v1reg_y=v1id_y*srho_node
                  v1reg_z=v1id_z*srho_node
                  v2reg_x=v2id_x*srho_node
                  v2reg_y=v2id_y*srho_node
                  v2reg_z=v2id_z*srho_node
                  v3reg_x=v3id_x*srho_node
                  v3reg_y=v3id_y*srho_node
                  v3reg_z=v3id_z*srho_node
 
                  edgerr_x=2.*(v1*v1_x+v2*v2_x+v3*v3_x)
                  edgerr_y=2.*(v1*v1_y+v2*v2_y+v3*v3_y)
                  edgerr_z=2.*(v1*v1_z+v2*v2_z+v3*v3_z)
                  edgerr_x=edgerr_x+2.*(v1reg*v1reg_x+
     &               v2reg*v2reg_x+v3reg*v3reg_x)
                  edgerr_y=edgerr_y+2.*(v1reg*v1reg_y+
     &               v2reg*v2reg_y+v3reg*v3reg_y)
                  edgerr_z=edgerr_z+2.*(v1reg*v1reg_z+
     &               v2reg*v2reg_z+v3reg*v3reg_z)
 
                  edgerr_xx=2.*( v1_x*v1_x+v2_x*v2_x+v3_x*v3_x+
     &               2.*hxx(ihyb)*(v1+v2+v3) )
                  edgerr_xx=edgerr_xx+2.*( v1reg_x*v1reg_x+v2reg_x
     &               *v2reg_x+v3reg_x*v3reg_x+2.*srho_node*(v1reg+v2reg
     &               +v3reg) )
                  edgerr_xy=2.*( v1_x*v1_y+v2_x*v2_y+v3_x*v3_y+
     &               2.*hxy(ihyb)*(v1+v2+v3) )
                  edgerr_xy=edgerr_xy+2.*( v1reg_x*v1reg_y+v2reg_x
     &               *v2reg_y+v3reg_x*v3reg_y )
                  edgerr_xz=2.*( v1_x*v1_z+v2_x*v2_z+v3_x*v3_z+
     &               2.*hxz(ihyb)*(v1+v2+v3) )
                  edgerr_xz=edgerr_xz+2.*( v1reg_x*v1reg_z+v2reg_x
     &               *v2reg_z+v3reg_x*v3reg_z )
                  edgerr_yy=2.*( v1_y*v1_y+v2_y*v2_y+v3_y*v3_y+
     &               2.*hyy(ihyb)*(v1+v2+v3) )
                  edgerr_yy=edgerr_yy+2.*( v1reg_y*v1reg_y+v2reg_y
     &               *v2reg_y+v3reg_y*v3reg_y+2.*srho_node*(v1reg+v2reg
     &               +v3reg) )
                  edgerr_yz=2.*( v1_y*v1_z+v2_y*v2_z+v3_y*v3_z+
     &               2.*hyz(ihyb)*(v1+v2+v3) )
                  edgerr_yz=edgerr_yz+2.*( v1reg_y*v1reg_z+v2reg_y
     &               *v2reg_z+v3reg_y*v3reg_z )
                  edgerr_zz=2.*( v1_z*v1_z+v2_z*v2_z+v3_z*v3_z+
     &               2.*hzz(ihyb)*(v1+v2+v3) )
                  edgerr_zz=edgerr_zz+2.*( v1reg_z*v1reg_z+v2reg_z
     &               *v2reg_z+v3reg_z*v3reg_z+2.*srho_node*(v1reg+v2reg
     &               +v3reg) )
 
C...  Compute the H1 seminorm functional and its derivatives.
 
                  pf=pf+geom*edgerr*wttetv(loctet,itettyp(ihyb))
 
                  pfx(1)=pfx(1)+dfg(geom,geom_x,edgerr,edgerr_x)
     &               *wttetv(loctet,itettyp(ihyb))
                  pfx(2)=pfx(2)+dfg(geom,geom_y,edgerr,edgerr_y)
     &               *wttetv(loctet,itettyp(ihyb))
                  pfx(3)=pfx(3)+dfg(geom,geom_z,edgerr,edgerr_z)
     &               *wttetv(loctet,itettyp(ihyb))
 
                  pfxx(1,1)=pfxx(1,1)+hfg(geom,geom_x,geom_x,geom_xx,
     &               edgerr,edgerr_x,edgerr_x,edgerr_xx)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(2,1)=pfxx(2,1)+hfg(geom,geom_x,geom_y,geom_xy,
     &               edgerr,edgerr_x,edgerr_y,edgerr_xy)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,1)=pfxx(3,1)+hfg(geom,geom_x,geom_z,geom_xz,
     &               edgerr,edgerr_x,edgerr_z,edgerr_xz)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(2,2)=pfxx(2,2)+hfg(geom,geom_y,geom_y,geom_yy,
     &               edgerr,edgerr_y,edgerr_y,edgerr_yy)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,2)=pfxx(3,2)+hfg(geom,geom_y,geom_z,geom_yz,
     &               edgerr,edgerr_y,edgerr_z,edgerr_yz)*wttetv(loctet
     &               ,itettyp(ihyb))
                  pfxx(3,3)=pfxx(3,3)+hfg(geom,geom_z,geom_z,geom_zz,
     &               edgerr,edgerr_z,edgerr_z,edgerr_zz)*wttetv(loctet
     &               ,itettyp(ihyb))
 
               enddo
            enddo
 
c...   Copy off-diagonal terms.
 
            pfxx(1,2)=pfxx(2,1)
            pfxx(1,3)=pfxx(3,1)
            pfxx(2,3)=pfxx(3,2)
 
c... Divide everything thru by 576, to make it an actual
c... estimate of the Sobolev seminorm.
 
             pf=pf/576.d0
             pfx(1)=pfx(1)/576.d0
             pfx(2)=pfx(2)/576.d0
             pfx(3)=pfx(3)/576.d0
             pfxx(1,1)=pfxx(1,1)/576.d0
             pfxx(2,1)=pfxx(2,1)/576.d0
             pfxx(3,1)=pfxx(3,1)/576.d0
             pfxx(1,2)=pfxx(1,2)/576.d0
             pfxx(2,2)=pfxx(2,2)/576.d0
             pfxx(3,2)=pfxx(3,2)/576.d0
             pfxx(1,3)=pfxx(1,3)/576.d0
             pfxx(2,3)=pfxx(2,3)/576.d0
             pfxx(3,3)=pfxx(3,3)/576.d0
 
         elseif (itask.eq.4) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.
 
c...  Loop over elements in polyhedron and fix
c...  regularization strength.
 
            mpk=invmpary(node)
            srho_node=0.d0
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               srho_node=max(srho_node,srho(ii))
            enddo
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))
                  ssq=a1x**2+a1y**2+a1z**2
 
c...  Compute area vectors of faces containing NODE.
 
                  a2x=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2y=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a2z=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3),y(i3)
     &               ,z(i3))
                  a3x=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3y=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a3z=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4),y(i4)
     &               ,z(i4))
                  a4x=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4y=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
                  a4z=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2),y(i2)
     &               ,z(i2))
 
c...  Compute sum of squares of area vectors
 
                  ssq=ssq+
     &               a2x**2+a2y**2+a2z**2+
     &               a3x**2+a3y**2+a3z**2+
     &               a4x**2+a4y**2+a4z**2
 
c...  Compute volume of tetrahedron.
 
                  vol=(x(i2)-xi1)*a1x+(y(i2)-yi1)*a1y+(z(i2)-zi1)*a1z
     &               -voff(loctet)
 
c.... If VOL is too small, we are done.
 
                  if (vol.lt.0.6*epsilonv) then
                     pf=1.d99
                     goto 9999
                  endif
 
c...  Compute the 'geometric factor'.
                  geom=ssq/vol
 
c...  Compute the edge errors over the edges {1,...,6}.
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v4=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v4id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v5=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v5id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v6=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v6id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v1=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v1id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v2=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v2id=ex*ex+ey*ey+ez*ez
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v3=ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*hxz(ihyb)*ez)+
     &               ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)+ez*hzz(ihyb)*ez
                  v3id=ex*ex+ey*ey+ez*ez
 
C...  Compute the sum of squares of edge errors, and sum of squares of
c...  edge errors if we assume the Hessian is equal to the identity.
 
                  edgerr=v1**2+v2**2+v3**2+v4**2+v5**2+v6**2
                  edgerrid=v1id**2+v2id**2+v3id**2+
     &               v4id**2+v5id**2+v6id**2
 
c...  Now increment the total edge error to reflect the regularization
c...  contribution.
 
                  edgerr=edgerr+srho_node**2*edgerrid
 
C...  Compute the H1 seminorm functional.
                  pf=pf+geom*edgerr*wttetv(loctet,itettyp(ihyb))
 
               enddo
            enddo
 
c... Divide everything thru by 576, to make it an actual
c... estimate of the Sobolev seminorm.
 
            pf=pf/576.d0
 
         elseif (itask.eq.5) then
 
c.... Release temp arrays
 
            call mmrelprt(isubname,icscode)
 
         endif
 
      elseif ((action(1:5).eq.'field'.or.action(1:4).eq.'user'.or
     &      .action(1:7).eq.'erradpt').and
     &      .index(action(1:icharlnf(action)),':exact').gt.0) then
 
c.... MEGA option with exact MEGA functional.
 
         if (itask.eq.1) then
 
            print*,'Using exact!!!'
 
               call mmgetblk('rho1',isubname,iprho1,ieltno,2,icscode)
 
         elseif (itask.eq.2) then
 
c...Sample diameter of domain.  We do this
c...in order to be able to have 'scale invariant' regularization.
 
                  xmax_=-1.d99
                  ymax_=-1.d99
                  zmax_=-1.d99
                  xmin_=1.d99
                  ymin_=1.d99
                  zmin_=1.d99
                  do i=1,ichildno
                     xmax_=max(xmax_,x(ichildary(i)))
                     xmin_=min(xmin_,x(ichildary(i)))
                     ymax_=max(ymax_,y(ichildary(i)))
                     ymin_=min(ymin_,y(ichildary(i)))
                     zmax_=max(zmax_,z(ichildary(i)))
                     zmin_=min(zmin_,z(ichildary(i)))
                  enddo
                  diamsq=(xmax_-xmin_)**2+(ymax_-ymin_)**2+(zmax_-zmin_)
     &               **2
 
c...if the adaption function has no variation, give the function a small
c...range to avoid problems with smoothing.
 
               range=max(range,1.d-30)
 
               do ii=1,ieltno
 
c.... We now compute for the current element an estimate
c.... of the relative value of the functional using the true
c.... adaption function and the value of the functional using the
c.... Identity matrix for a Hessian.  Since the Hessian appears
c.... quadratically, a good estimate for the ratio is
c....  RATIO=||H||_F^2 / ||I||_F^2 = ||H||_F^2 / 3,
c.... where _F denotes the Frobenius norm.
 
                  ratio=(hxx(ihyb)**2+2.*hxy(ihyb)**2+2.*hxz(ihyb)**2+
     &              hyy(ihyb) **2+2.*hyz(ihyb)**2+hzz(ihyb)**2)/3.d0
 
c...  Compute the local regularization constant 'rho1' which will determine
c...  the size of the regularization term.  The regularization term is equal
c...  to 'rho1' times the H1 norm functional, but with the Hessian
c...  set equal to the identity.
 
c...  First compute the 'relative' contribution to regularization.
c...  Here 'rho1' can vary from element to element, and we select it so that
c...  the magnitude of the regularization term is equal to 'regrel'*RATIO.
 
                  rho1(ii)=regrel*ratio
 
c...  Now increase rho1 by a constant to provide an absolute, global
c regularization
c...  in case of a totally linear region.  Note that paraboloid function
c ,
c...
c...     f(x,y,z)=(range/diamsq) * [(x-x0)**2+(y-y0)**2+(z-z0)**2],
c...
c...  with (x0,y0,z0) being at one corner of the diagonal of the domain,
c...  has the same scale as the function being adapted to (over the
c...  bounding box of the domain) and has Hessian equal to
c...
c...     2*range/diamsq * (Identity Matrix).
c...
c...  RHO1 is incremented proportional to the square of this, because
c...  the Hessian appears quadratically in the functional expression.
 
                  rho1(ii)=rho1(ii)+regabs*(2.d0*range/diamsq)**2
 
               enddo
 
c.... Compute volume offset list for bad tets in the mesh.
 
               call getvoloff(ieltno,ieltary,x,y,z,itet,itetoff,
     &            itettyp,epsilonv,isubname,ipvoloff,iplocvoloff
     &            ,ipivoloffoff)
 
               do k=1,ivoloffoff(ieltno+1)
                  voloff(k)=voloff(k)*one6th
               enddo
 
         elseif (itask.eq.3) then
 
            print*,'error:no analytic derivatives available'
            print*,'for exact MEGA'
            stop
 
         elseif (itask.eq.4) then
 
            xi1=x(node)
            yi1=y(node)
            zi1=z(node)
 
            pf=0.d0
 
c...  Loop over elements in polyhedron and fix
c...  regularization strength.
 
            mpk=invmpary(node)
            rho_node=0.d0
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               rho_node=max(rho_node,rho1(ii))
            enddo
 
c...  Loop over all (possibly hybrid) elements in polyhedron
 
            mpk=invmpary(node)
            do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
               ii=1+(nodhyb(i)-1)/maxnen
               lochybnod=nodhyb(i)-maxnen*(ii-1)
               ihyb=ieltary(ii)
 
c.... Calculate volume offsets for virtual tetrahedra in
c.... current element.
 
               do k1=1,ihybnumtetv(itettyp(ihyb))
                  voff(k1)=0.
               enddo
               do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
                  voff(locvoloff(k))=voloff(k)
               enddo
 
c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.
c.... Compute contribution of each virtual tet to the
c.... polyhedral functional.
 
               do kk=1,nodnumtetv(lochybnod,itettyp(ihyb))
                  jtetvi=jtetv(kk,lochybnod,itettyp(ihyb))
                  loctet=1+(jtetvi-1)/4
                  locnod=jtetvi-4*(loctet-1)
                  i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &               ,loctet,itettyp(ihyb))+itetoff(ihyb))
 
c...  Compute area vector of face opposite NODE
 
                  afac(1,1)=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))*0.5d0
                  afac(2,1)=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))*0.5d0
                  afac(3,1)=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &               x(i4),y(i4),z(i4))*0.5d0
 
c...  Compute area vectors of faces containing NODE.
 
                  afac(1,2)=crosx(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))*0.5d0
                  afac(2,2)=crosy(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))*0.5d0
                  afac(3,2)=crosz(xi1,yi1,zi1,x(i4),y(i4),z(i4),x(i3)
     &               ,y(i3),z(i3))*0.5d0
                  afac(1,3)=crosx(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))*0.5d0
                  afac(2,3)=crosy(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))*0.5d0
                  afac(3,3)=crosz(xi1,yi1,zi1,x(i2),y(i2),z(i2),x(i4)
     &               ,y(i4),z(i4))*0.5d0
                  afac(1,4)=crosx(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))*0.5d0
                  afac(2,4)=crosy(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))*0.5d0
                  afac(3,4)=crosz(xi1,yi1,zi1,x(i3),y(i3),z(i3),x(i2)
     &               ,y(i2),z(i2))*0.5d0
 
c...  Compute volume of tetrahedron.
 
                  vol=((x(i2)-xi1)*afac(1,1)+(y(i2)-yi1)*afac(2,1)+(z(i2
     &               )-zi1)*afac(3,1))*one3rd-voff(loctet)
 
c.... Compute matrix of dot products of area vectors
 
                  do j=1,4
                     aa(j,j)=afac(1,j)**2+afac(2,j)**2+afac(3,j)**2
                     do k=j+1,4
                        aa(j,k)=afac(1,j)*afac(1,k)+afac(2,j)*afac(2,k)
     &                     +afac(3,j)*afac(3,k)
c$$$                        aa(k,j)=aa(j,k)
                     enddo
                  enddo
 
c.... Compute matrix of dot products of gradients of quadratic basis
c.... functions.
 
                  volfac=1.d0/(180.d0*vol)
c$$$                  do j=1,6
c$$$                     j1=ielmedge1(1,j,ifelmtet)
c$$$                     j2=ielmedge1(2,j,ifelmtet)
c$$$                     b(j,j)=2.d0*(aa(j1,j1)+aa(j1,j2)+aa(j2,j2))*volfac
c$$$                     do k=j+1,6
c$$$                        k1=ielmedge1(1,k,ifelmtet)
c$$$                        k2=ielmedge1(2,k,ifelmtet)
c$$$                        b(j,k)=0.d0
c$$$                        if (j1.eq.k1) then
c$$$                           b(j,k)=b(j,k)+2.d0*aa(j2,k2)*volfac
c$$$                        else
c$$$                           b(j,k)=b(j,k)+aa(j2,k2)*volfac
c$$$                        endif
c$$$                        if (j1.eq.k2) then
c$$$                           b(j,k)=b(j,k)+2.d0*aa(j2,k1)*volfac
c$$$                        else
c$$$                           b(j,k)=b(j,k)+aa(j2,k1)*volfac
c$$$                        endif
c$$$                        if (j2.eq.k1) then
c$$$                           b(j,k)=b(j,k)+2.d0*aa(j1,k2)*volfac
c$$$                        else
c$$$                           b(j,k)=b(j,k)+aa(j1,k2)*volfac
c$$$                        endif
c$$$                        if (j2.eq.k2) then
c$$$                           b(j,k)=b(j,k)+2.d0*aa(j1,k1)*volfac
c$$$                        else
c$$$                           b(j,k)=b(j,k)+aa(j1,k1)*volfac
c$$$                        endif
c$$$                     enddo
c$$$                  enddo
 
c.... Diagonal terms
                  b(1,1)=2*volfac*(aa(1,1)+aa(1,2)+aa(2,2))
                  b(2,2)=2*volfac*(aa(1,1)+aa(1,3)+aa(3,3))
                  b(3,3)=2*volfac*(aa(1,1)+aa(1,4)+aa(4,4))
                  b(4,4)=2*volfac*(aa(2,2)+aa(2,3)+aa(3,3))
                  b(5,5)=2*volfac*(aa(2,2)+aa(2,4)+aa(4,4))
                  b(6,6)=2*volfac*(aa(3,3)+aa(3,4)+aa(4,4))
 
c.... Skew-Edge Interactions
                  b(1,6)=volfac*(aa(1,3)+aa(1,4)+aa(2,3)+aa(2,4))
                  b(2,5)=volfac*(aa(1,2)+aa(1,4)+aa(2,3)+aa(3,4))
                  b(3,4)=volfac*(aa(1,2)+aa(1,3)+aa(2,4)+aa(3,4))
 
c.... Adjacent Edge Interactions
 
                  b(1,2)=volfac*(2*aa(2,3)-aa(1,4))
                  b(5,6)=b(1,2)
 
                  b(1,3)=volfac*(2*aa(2,4)-aa(1,3))
                  b(4,6)=b(1,3)
 
                  b(1,4)=volfac*(2*aa(1,3)-aa(2,4))
                  b(3,6)=b(1,4)
 
                  b(1,5)=volfac*(2*aa(1,4)-aa(2,3))
                  b(2,6)=b(1,5)
 
                  b(2,3)=volfac*(2*aa(3,4)-aa(1,2))
                  b(4,5)=b(2,3)
 
                  b(2,4)=volfac*(2*aa(1,2)-aa(3,4))
                  b(3,5)=b(2,4)
 
c...  We experience better results when the adjacent edge terms are
c...  zeroed.  Why is to be left to future research.
                  b(1,2)=0.d0
                  b(1,3)=0.d0
                  b(1,4)=0.d0
                  b(1,5)=0.d0
 
                  b(2,3)=0.d0
                  b(2,4)=0.d0
                  b(2,6)=0.d0
 
                  b(3,5)=0.d0
                  b(3,6)=0.d0
 
                  b(4,5)=0.d0
                  b(4,6)=0.d0
 
                  b(5,6)=0.d0
 
c...  Compute the edge errors over the edges {1,...,6}.
 
                  ex=x(i2)-xi1
                  ey=y(i2)-yi1
                  ez=z(i2)-zi1
                  v(1)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*
     &               hxz(ihyb)*ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)
     &               +ez*hzz(ihyb)*ez)
                  vid(1)=-0.5d0*(ex*ex+ey*ey+ez*ez)
 
                  ex=x(i3)-xi1
                  ey=y(i3)-yi1
                  ez=z(i3)-zi1
                  v(2)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*
     &               hxz(ihyb)*ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)
     &               +ez*hzz(ihyb)*ez)
                  vid(2)=-0.5d0*(ex*ex+ey*ey+ez*ez)
 
                  ex=x(i4)-xi1
                  ey=y(i4)-yi1
                  ez=z(i4)-zi1
                  v(3)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*
     &               hxz(ihyb)*ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)
     &               +ez*hzz(ihyb)*ez)
                  vid(3)=-0.5d0*(ex*ex+ey*ey+ez*ez)
 
                  ex=x(i2)-x(i3)
                  ey=y(i2)-y(i3)
                  ez=z(i2)-z(i3)
                  v(4)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*
     &               hxz(ihyb)*ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)
     &               +ez*hzz(ihyb)*ez)
                  vid(4)=-0.5d0*(ex*ex+ey*ey+ez*ez)
 
                  ex=x(i2)-x(i4)
                  ey=y(i2)-y(i4)
                  ez=z(i2)-z(i4)
                  v(5)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*
     &               hxz(ihyb)*ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)
     &               +ez*hzz(ihyb)*ez)
                  vid(5)=-0.5d0*(ex*ex+ey*ey+ez*ez)
 
                  ex=x(i3)-x(i4)
                  ey=y(i3)-y(i4)
                  ez=z(i3)-z(i4)
                  v(6)=-0.5d0*(ex*(hxx(ihyb)*ex+2.*hxy(ihyb)*ey+2.*
     &               hxz(ihyb)*ez)+ey*(hyy(ihyb)*ey+2.*hyz(ihyb)*ez)
     &               +ez*hxx(ihyb)*ez)
                  vid(6)=-0.5d0*(ex*ex+ey*ey+ez*ez)
 
c.... Compute the products B * V and B * VID.
 
                  do j=1,6
                     bv(j)=0.d0
                     bvid(j)=0.d0
                  enddo
                  do j=1,6
                     bv(j)=bv(j)+b(j,j)*v(j)
                     bvid(j)=bvid(j)+b(j,j)*vid(j)
                     do k=j+1,6
                        bv(j)=bv(j)+b(j,k)*v(k)
                        bv(k)=bv(k)+b(j,k)*v(j)
                        bvid(j)=bvid(j)+b(j,k)*vid(k)
                        bvid(k)=bvid(k)+b(j,k)*vid(j)
                     enddo
                  enddo
 
c...  Compute PFTET=V * BV and PFIDTET=VID * BVID.
                  pftet=0.d0
                  pfidtet=0.d0
                  do j=1,6
                     pftet=pftet+v(j)*bv(j)
                     pfidtet=pfidtet+vid(j)*bvid(j)
                  enddo
 
c...  Now increment the functional contribution due to this tet
c...  to reflect the regularization contribution.
 
                  pftet=pftet+rho_node*pfidtet
 
C...  Increment the H1 seminorm functional by the tet contribution.
                  pf=pf+pftet*wttetv(loctet,itettyp(ihyb))
 
               enddo
            enddo
 
         elseif (itask.eq.5) then
 
c.... Release temp arrays
 
            call mmrelprt(isubname,icscode)
 
         endif
 
      endif
 
 9999 continue
      return
      end
