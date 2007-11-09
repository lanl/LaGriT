*dk,safemove
      subroutine safemove(time,nfac,nodjtet,itet,node,x,y,z,a,v,fold,
     &   dfx,dfy,dfz,dxpoly,dypoly,dzpoly,ijob,hxx,hxy,hxz,hyy,hyz,hzz,
     &   dx,dy,dz)
C #####################################################################
C
C     PURPOSE -
C
C        SAFEMOVE considers the proposed move of NODE by DX,DY,DZ,
C        and limits it so that tet inversions are avoided.  Further,
C        it checks that the functional POLYFUN decreases sufficiently,
C        and if not, does a line search a la the numerical recipes
C        subroutine lnsrch.f (2nd ed.).
C
C
C     INPUT ARGUMENTS -
C
C        TIME  -  Time, not used here, but passed onto POLYFUN.
C        NFAC  -  The number of faces of the polyhedron (= the number
C                 of tets sharing NODE).
C        NODJTET- A list of the NFAC jtet values of the tets sharing
C                 NODE.
C        ITET  -  The itet array for the tetrahedral mesh considered.
C        NODE  -  The node whose movement we are limiting.
C        X,Y,Z -  The xic,yic,zic arrays for the tet mesh.
C        A(3*NFAC) The three components of the outward (double)
C                 area vectors for all NFAC faces of the polyhedron.  Also
C                 used by POLYFUN.
C        V,F   -  Arrays passed for use by POLYFUN
C        FOLD  -  The value of POLYFUN when DX=DY=DZ=0.
C        DFX,DFY, The gradient of POLYFUN at DX=DY=DZ=0.
C        DFZ
C        DXPOLY,- Widths of the polyhedron about NODE in the x,y,and
C        DYPOLY,  z directions respectively.
C        DZPOLY
C        DX,DY,-  Proposed changes to add to X(NODE), Y(NODE), Z(NODE).
C        DZ
C
C     OUTPUT ARGUMENTS -
C
C        DX,DY,-  Safe changes to add to X(NODE), Y(NODE), Z(NODE),
C        DZ       which decrease POLYFUN sufficiently.
C
C     CHANGE HISTORY -
C
C ######################################################################
C$Log:   /pvcs.config/t3d/src/safemove.f_a  $
CPVCS    
CPVCS       Rev 1.5   08 Feb 2006 14:35:40   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:00:40 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Wed Jan 03 10:08:24 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS    
CPVCS       Rev 1.2   06/29/95 09:46:30   dcg
CPVCS    add version log and consts.h include file
      implicit none
      include 'consts.h'
      integer nfac,nodjtet(1000000),itet(4,1000000),node,ijob
      real*8 time,x(1000000),y(1000000),z(1000000),a(3,nfac),
     &   v(3*nfac),fold,dfx,dfy,dfz,dxpoly,dypoly,
     &   dzpoly,dx,dy,dz,hxx(1000000),hxy(1000000),hxz(1000000),
     &   hyy(1000000),hyz(1000000),hzz(1000000)
 
      real*8 t,det,rate,slope,d,alamin,alam,dxnew,dynew,dznew,
     &   f1,alf,tmplam,rhs1,rhs2,a1,b,disc,alam2,f2,fold2,polyfun
      integer i,i2,jtet,jt,ji
      parameter (alf=1.e-4)
      integer itetface1(4,4)
      data itetface1 / 2, 3, 4, 0,
     *     1, 4, 3, 0,
     *     1, 2, 4, 0,
     *     1, 3, 2, 0 /
 
      t=1.
      do i=1,nfac
         jtet=nodjtet(i)
         jt=1+(jtet-1)/4
         ji=jtet-4*(jt-1)
         i2=itet(itetface1(1,ji),jt)
         det=(x(i2)-x(node))*a(1,i)+(y(i2)-y(node))*a(2,i)+
     &      (z(i2)-z(node))*a(3,i)
         rate=-dx*a(1,i)-dy*a(2,i)-dz*a(3,i)
 
         if (-rate*t.gt.0.5*det) then
            t=-det/rate*0.5
         endif
      enddo
      dx=t*dx
      dy=t*dy
      dz=t*dz
 
      slope=dfx*dx+dfy*dy+dfz*dz
      d=sqrt((dx/dxpoly)**2+(dy/dypoly)**2+
     &   (dz/dzpoly)**2)
 
      alamin=1.e-2/d
      alam=1.
 
      do while (.true.)
        dxnew=alam*dx
        dynew=alam*dy
        dznew=alam*dz
 
        f1=polyfun(ijob,time,nfac,nodjtet,itet,x,y,z,dxnew,dynew,dznew,
     &   node,hxx,hxy,hxz,hyy,hyz,hzz,a,v)
 
        if(f1.le.fold+alf*alam*slope)then
          dx=dxnew
          dy=dynew
          dz=dznew
          goto 9999
        elseif(alam.lt.alamin)then
          dx=0.
          dy=0.
          dz=0.
          goto 9999
        else
          if(alam.eq.1.)then
            tmplam=-slope/(2.*(f1-fold-slope))
            if(tmplam.gt..75)tmplam=.75
          else
            rhs1=f1-fold-alam*slope
            rhs2=f2-fold2-alam2*slope
            a1=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a1.eq.0.)then
              tmplam=-slope/(2.*b)
            else
              disc=b*b-3.*a1*slope
              tmplam=(-b+sqrt(max(zero,disc)))/(3.*a1)
            endif
            if(tmplam.gt..5*alam)tmplam=.5*alam
          endif
        endif
        alam2=alam
        f2=f1
        fold2=fold
        alam=max(tmplam,.1*alam)
      enddo
C  (C) Copr. 1986-92 Numerical Recipes Software
 
 9999 continue
      return
      end
