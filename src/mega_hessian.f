*dk,mega_hessian()
      subroutine mega_hessian()
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine builds the 2D or 3D Hessian matrix and places
C           it in the CMO.
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C     CHANGE HISTORY -
C
C        $Log: mega_hessian.f,v $
C        Revision 2.00  2007/11/05 19:46:01  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      integer ierror
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
 
      pointer (ipitet, itet1)
      integer itet1(*)
      pointer (ipitetclr, itetclr)
      integer itetclr(*)
 
      pointer (ipxmegah, xmegah)
      pointer (iptmp, tmp)
      real*8 xmegah(*), tmp(*)

      integer igeom_gwt,ivoronoi,ilen,itype,icscode,ierrwrt,
     *  nen,nef,nsd,mbndry,nnodes,nelements,length,icmotype,
     *  kdim,lenout,ier,ityp,ierr,kpe

      character*32 isubname, cmo
      character*132 logmess
      character*8092 cbuff

      common / kent1 / igeom_gwt
      external fcn
C
C********************************************************
C BEGIN begin
C
      isubname='mega_hessian'
      igeom_gwt=0
C
      call cmo_get_name(cmo,ierror)

      if(ierror.ne.0) then
         write(logmess,9000)
 9000    format('No CMOs defined')
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif

      call cmo_get_info('ivoronoi',cmo,
     *                ivoronoi,ilen,itype,icscode)
      if(ivoronoi.eq.-2) igeom_gwt=1
C
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
C
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,icmotype,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
 
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
C
C
      length=nnodes
      call mmgetblk('tmp',isubname,iptmp,length,2,icscode)
C
      kdim=3
C
      if(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         call mmfindbk('megah',cmo,ipxmegah,lenout,icscode)
         if(icscode.ne.0) then
            cbuff = 'cmo/addatt/-def-/mega2d/INT/scalar/' //
     *              'scalar/linear/permanent/x/9.0 ;  ' //
     *              'cmo/addatt/-def-/megah/VDOUBLE/mega2d/' //
     *              'nnodes/linear/permanent/x/0.0 ;  ' //
     *              'finish'
            call dotaskx3d(cbuff,ier)
            call cmo_get_info('megah',cmo,ipxmegah,ilen,ityp,ierr)
         endif
         kpe=kdim
         call mkh2d (nnodes,kdim,xic,yic,zic,nelements,kpe,itet1,
     *               itetclr,xmegah,tmp,fcn)
      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
         call mmfindbk('megah',cmo,ipxmegah,lenout,icscode)
         if(icscode.ne.0) then
            cbuff = 'cmo/addatt/-def-/mega3d/INT/scalar/' //
     *              'scalar/linear/permanent/x/9.0 ;  ' //
     *              'cmo/addatt/-def-/megah/VDOUBLE/mega3d/' //
     *              'nnodes/linear/permanent/x/0.0 ;  ' //
     *              'finish'
            call dotaskx3d(cbuff,ier)
            call cmo_get_info('megah',cmo,ipxmegah,ilen,ityp,ierr)
         endif
         kpe=kdim+1
         call mkh3d (nnodes,kdim,xic,yic,zic,nelements,kpe,itet1,
     *               itetclr,xmegah,tmp,fcn)
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*dk,mkh2d
      subroutine mkh2d (nk,kdim,xic,yic,zic,
     +           nel,kpe,kel,ireg,h,tmp,fcn)
c
c     Compute 2D Surface Hessian Matrix
c     kpe = kdim = 3

C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none

C arguments
      integer nk,kdim
      real*8 xic(nk),yic(nk),zic(nk)
      integer nel,kpe
      integer kel(kpe,nel)
      integer ireg(nel)
      real*8 h(kdim,kdim,nk),tmp(nk)
      external fcn

c variables
      real*8 u(3),xm(3),g(2,3),ht(3,3)
      real*8 v1(3),v2(3),w(3)

      real*8 h0,vtv,a1,b1,scl,a2,b2,a11,a12,a21,a22,
     *       um,em,g11,g21,g22,det
      integer ix,iy,k,it,k1,k2,k3,ie,i,j,iv,jv
c
      integer ne
      data ne / 3 /

      integer edge(2,3)
      data edge(1,1),edge(2,1) / 1, 2 /
      data edge(1,2),edge(2,2) / 1, 3 /
      data edge(1,3),edge(2,3) / 2, 3 /
c
      real*8 zero,one
      data zero,one /0.0d0, 1.0d0/
c
      integer  igeom_gwt
      common / kent1 / igeom_gwt
c
c ***********************************************
c
      h0=float(igeom_gwt)
      do ix=1,kdim
         do iy=1,kdim
            do k=1,nk
               h(ix,iy,k)=h0
            end do
         end do
      end do
      if (igeom_gwt.ne.0) return
c
      do k=1,nk
         tmp(k)=1.0d-50
      end do
c
c     Element Loop
c
      do it=1,nel
c
c........Knots
         k1=kel(1,it)
         k2=kel(2,it)
         k3=kel(3,it)
c
c........Edge vectors
         v1(1)=xic(k1)-xic(k3)
         v1(2)=yic(k1)-yic(k3)
         v1(3)=zic(k1)-zic(k3)
         v2(1)=xic(k2)-xic(k3)
         v2(2)=yic(k2)-yic(k3)
         v2(3)=zic(k2)-zic(k3)
c........Rotate v1 to (-a1,0,0)
         vtv=v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3)
         a1=dsign(sqrt(vtv),v1(1))
         v1(1)=v1(1)+a1
c........b1 = 2 / vTv
         b1=one/(a1*v1(1))
c........v2 = (I - b1 v1 v1T )  v2
         scl=b1*(v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3))
         v2(1)=v2(1)-scl*v1(1)
         v2(2)=v2(2)-scl*v1(2)
         v2(3)=v2(3)-scl*v1(3)
c........Rotate v2 to (v2x,-a2,0)
         vtv=v2(2)*v2(2)+v2(3)*v2(3)
         a2=dsign(sqrt(vtv),v2(2))
         v2(2)=v2(2)+a2
c........b2 = 2 / vTv
         b2=one/(a2*v2(2))
c
c........Affine Matrix
         a11=-a1
         a12=v2(1)
         a21=zero
         a22=-a2
c
c........The Determinant
         det=a11*a22
         det=abs(det)
c
c........The Inverse
         g(1,1)= a22/det
         g(1,2)=-a21/det
         g(1,3)=-g(1,1)-g(1,2)
         g(2,1)=-a12/det
         g(2,2)= a11/det
         g(2,3)=-g(2,1)-g(2,2)
c
c........Edge Error
         do k=1,kpe
            iv=kel(k,it)
            xm(1)=xic(iv)
            xm(2)=yic(iv)
            xm(3)=zic(iv)
            call fcn (kdim,xm,ireg(it),u(k))
         end do
         do ix=1,kdim
            do iy=1,kdim
               ht(ix,iy)=zero
            end do
         end do
         do ie=1,ne
            i=edge(1,ie)
            iv=kel(i,it)
            j=edge(2,ie)
            jv=kel(j,it)
            xm(1)=(xic(iv)+xic(jv))/2.0d0
            xm(2)=(yic(iv)+yic(jv))/2.0d0
            xm(3)=(zic(iv)+zic(jv))/2.0d0
            call fcn (kdim,xm,ireg(it),um)
            em=det*2.0d0*(2.0d0*um-u(i)-u(j))
            g11=g(1,i)*g(1,j)+g(1,j)*g(1,i)
            ht(1,1)=ht(1,1)+em*g11
            g21=g(2,i)*g(1,j)+g(2,j)*g(1,i)
            ht(2,1)=ht(2,1)+em*g21
            g22=g(2,i)*g(2,j)+g(2,j)*g(2,i)
            ht(2,2)=ht(2,2)+em*g22
         end do
         ht(1,2)=ht(2,1)
c
c........Ht = P2 Ht P2
         w(2)=b2*ht(2,2)*v2(2)
         w(3)=zero
         scl=(b2/2.0d0)*v2(2)*w(2)
         w(2)=w(2)-scl*v2(2)
         w(3)=w(3)-scl*v2(3)
         do ix=2,kdim
            do iy=2,ix
               ht(ix,iy)=ht(ix,iy)
     +                  -v2(ix)*w(iy)-w(ix)*v2(iy)
               ht(iy,ix)=ht(ix,iy)
            end do
         end do
c........Ht = P1 Ht P1
         w(1)=b1*(ht(1,1)*v1(1)+ht(1,2)*v1(2)              )
         w(2)=b1*(ht(2,1)*v1(1)+ht(2,2)*v1(2)+ht(2,3)*v1(3))
         w(3)=b1*(              ht(3,2)*v1(2)+ht(3,3)*v1(3))
         scl=(b1/2.0d0)*(v1(1)*w(1)+v1(2)*w(2)+v1(3)*w(3))
         w(1)=w(1)-scl*v1(1)
         w(2)=w(2)-scl*v1(2)
         w(3)=w(3)-scl*v1(3)
         do ix=1,kdim
            do iy=1,ix
               ht(ix,iy)=ht(ix,iy)
     +                  -v1(ix)*w(iy)-w(ix)*v1(iy)
               ht(iy,ix)=ht(ix,iy)
            end do
         end do
c
c........Average
         do i=1,kpe
            k=kel(i,it)
            tmp(k)=tmp(k)+det
            do ix=1,kdim
               do iy=ix,kdim
                  h(ix,iy,k)=h(ix,iy,k)+ht(ix,iy)
               end do
            end do
         end do
c
      end do
c
      do ix=1,kdim
         do k=1,nk
            h(ix,ix,k)=h(ix,ix,k)/tmp(k)
         end do
         do iy=ix+1,kdim
            do k=1,nk
               h(ix,iy,k)=h(ix,iy,k)/tmp(k)
               h(iy,ix,k)=h(ix,iy,k)
            end do
         end do
      end do
c
      return
      end
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*dk,b2dnxm
      subroutine b2dnxm (n,ntri,en,m,mtri,em,
     +           nk,kdim,xic,yic,zic,h,
     +           nel,kpe,kel,det,err,flip)
c
c     Test for n x m flip
c
c     Triangle Errors
c
C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none
   
c arguments
      integer n,m,nk,kdim,nel,kpe
      integer ntri(n),mtri(m)
      real*8  en,em
      real*8  xic(nk),yic(nk),zic(nk)
      real*8  h(kdim,kdim,nk)
      integer kel(kpe,nel)
      real*8  det(nel),err(nel)
      logical flip

c variables
      real*8 dx(3),dy(3),dz(3)

      real*8 eps,area,geom,d11,d12,d22,esum,
     *       e1,e2,ehe,atol,etri

      integer ii,it,ie,i,iv,j,jv 
c
      integer ne
      data ne / 3 /
      integer edge(2,3)
      data edge(1,1),edge(2,1) / 1, 2 /
      data edge(1,2),edge(2,2) / 1, 3 /
      data edge(1,3),edge(2,3) / 2, 3 /
c
      real*8 zero,one
      data zero,one /0.0d0, 1.0d0/
c
      integer  igeom_gwt
      common / kent1 / igeom_gwt
c
c *****************************************
c
      flip=.false.
      eps=1.0d-10
      area=zero
c
c     n tri error
c
      en=zero
      do ii=1,n
         it=ntri(ii)
         if (err(it).le.zero.or.det(it).lt.zero) then
c
c...........Edge Lengths
            geom=zero
            do ie=1,ne
               i=edge(1,ie)
               iv=kel(i,it)
               j=edge(2,ie)
               jv=kel(j,it)
               dx(ie)=xic(jv)-xic(iv)
               dy(ie)=yic(jv)-yic(iv)
               dz(ie)=zic(jv)-zic(iv)
               geom=geom+dx(ie)*dx(ie)+dy(ie)*dy(ie)+dz(ie)*dz(ie)
            end do
c
c...........Triangle Area
            d11=dx(1)*dx(1)+dy(1)*dy(1)+dz(1)*dz(1)
            d12=dx(1)*dx(2)+dy(1)*dy(2)+dz(1)*dz(2)
            d22=dx(2)*dx(2)+dy(2)*dy(2)+dz(2)*dz(2)
            det(it)=d11*d22-d12*d12
            det(it)=abs(det(it))
c
c...........Edge Error
            if (igeom_gwt.eq.1) then
               esum=one
            else
               esum=zero
               do ie=1,ne
                  i=edge(1,ie)
                  iv=kel(i,it)
                  j=edge(2,ie)
                  jv=kel(j,it)
                  e1=dx(ie)*(h(1,2,iv)+h(1,2,jv))*dy(ie)
     +              +dx(ie)*(h(1,3,iv)+h(1,3,jv))*dz(ie)
     +              +dy(ie)*(h(2,3,iv)+h(2,3,jv))*dz(ie)
                  e2=dx(ie)*(h(1,1,iv)+h(1,1,jv))*dx(ie)
     +              +dy(ie)*(h(2,2,iv)+h(2,2,jv))*dy(ie)
     +              +dz(ie)*(h(3,3,iv)+h(3,3,jv))*dz(ie)
                  ehe=2.0d0*e1+e2
                  esum=esum+ehe*ehe
               end do
            end if
            err(it)=geom*esum/det(it)
         end if
c
c........Triangle Error
         area=area+det(it)
         en=en+err(it)
c
      end do
      atol=eps*area
      area=area+atol
c
c     m tri error
c
      em=zero
      do ii=1,m
         it=mtri(ii)
c
c........Edge Lengths
         geom=zero
         do ie=1,ne
            i=edge(1,ie)
            iv=kel(i,it)
            j=edge(2,ie)
            jv=kel(j,it)
            dx(ie)=xic(jv)-xic(iv)
            dy(ie)=yic(jv)-yic(iv)
            dz(ie)=zic(jv)-zic(iv)
            geom=geom+dx(ie)*dx(ie)+dy(ie)*dy(ie)+dz(ie)*dz(ie)
         end do
c
c........Triangle Area
         d11=dx(1)*dx(1)+dy(1)*dy(1)+dz(1)*dz(1)
         d12=dx(1)*dx(2)+dy(1)*dy(2)+dz(1)*dz(2)
         d22=dx(2)*dx(2)+dy(2)*dy(2)+dz(2)*dz(2)
         det(it)=d11*d22-d12*d12
         det(it)=abs(det(it))
c
c........Edge Error
         if (igeom_gwt.eq.1) then
            esum=one
         else
            esum=zero
            do ie=1,ne
               i=edge(1,ie)
               iv=kel(i,it)
               j=edge(2,ie)
               jv=kel(j,it)
               e1=dx(ie)*(h(1,2,iv)+h(1,2,jv))*dy(ie)
     +           +dx(ie)*(h(1,3,iv)+h(1,3,jv))*dz(ie)
     +           +dy(ie)*(h(2,3,iv)+h(2,3,jv))*dz(ie)
               e2=dx(ie)*(h(1,1,iv)+h(1,1,jv))*dx(ie)
     +           +dy(ie)*(h(2,2,iv)+h(2,2,jv))*dy(ie)
     +           +dz(ie)*(h(3,3,iv)+h(3,3,jv))*dz(ie)
               ehe=2.0d0*e1+e2
               esum=esum+ehe*ehe
            end do
         end if
c
c........Triangle Error
         etri=geom*esum
         area=area-det(it)
         flip=(det(it)*(en-em).gt.etri)
     +   .and.(area.gt.zero)
         if (flip) then
            err(it)=etri/det(it)
            em=em+err(it)
         else
            det(it)=-one
            err(it)=-one
            em=en+one
            return
         end if
c
      end do
      area=area-atol
      flip=(en-em).gt.eps*en
     +   .and.(abs(area).lt.atol)
c
      return
      end
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*dk,errb2d
      subroutine errb2d (n,ntri,nk,kdim,xic,yic,zic,h,
     +           nel,kpe,kel,det,err)
c
c     2D Triangle Errors
c
C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none

c arguments
      integer n,nk,kdim,nel,kpe
      integer ntri(n)
      real*8  xic(nk),yic(nk),zic(nk)
      real*8  h(kdim,kdim,nk)
      integer kel(kpe,nel)
      real*8  det(nel),err(nel)

c variables

      real*8 dx(3),dy(3),dz(3)

      real*8 geom,d11,d12,d22,esum,e1,e2,ehe

      integer in,it,i,iv,j,jv,ie

      integer ne
      data ne / 3 /
      integer edge(2,3)
      data edge(1,1),edge(2,1) / 1, 2 /
      data edge(1,2),edge(2,2) / 1, 3 /
      data edge(1,3),edge(2,3) / 2, 3 /
c
      real*8 zero,one
      data zero,one /0.0d0, 1.0d0/
c
      integer igeom_gwt
      common / kent1 / igeom_gwt
c
c ****************************************************
c     Triangle Errors
c
      do in=1,n
         it=ntri(in)
c
c........Edge Lengths
         geom=zero
         do ie=1,ne
            i=edge(1,ie)
            iv=kel(i,it)
            j=edge(2,ie)
            jv=kel(j,it)
            dx(ie)=xic(jv)-xic(iv)
            dy(ie)=yic(jv)-yic(iv)
            dz(ie)=zic(jv)-zic(iv)
            geom=geom+dx(ie)*dx(ie)+dy(ie)*dy(ie)+dz(ie)*dz(ie)
         end do
c
c........Triangle Area
         d11=dx(1)*dx(1)+dy(1)*dy(1)+dz(1)*dz(1)
         d12=dx(1)*dx(2)+dy(1)*dy(2)+dz(1)*dz(2)
         d22=dx(2)*dx(2)+dy(2)*dy(2)+dz(2)*dz(2)
         det(it)=d11*d22-d12*d12
         det(it)=abs(det(it))
c
c........Edge Error
         if (igeom_gwt.eq.1) then
            esum=one
         else
            esum=zero
            do ie=1,ne
               i=edge(1,ie)
               iv=kel(i,it)
               j=edge(2,ie)
               jv=kel(j,it)
               e1=dx(ie)*(h(1,2,iv)+h(1,2,jv))*dy(ie)
     +           +dx(ie)*(h(1,3,iv)+h(1,3,jv))*dz(ie)
     +           +dy(ie)*(h(2,3,iv)+h(2,3,jv))*dz(ie)
               e2=dx(ie)*(h(1,1,iv)+h(1,1,jv))*dx(ie)
     +           +dy(ie)*(h(2,2,iv)+h(2,2,jv))*dy(ie)
     +           +dz(ie)*(h(3,3,iv)+h(3,3,jv))*dz(ie)
               ehe=2.0d0*e1+e2
               esum=esum+ehe*ehe
            end do
         end if
c
c........Triangle Error
         err(it)=geom*esum/det(it)
c
      end do
c
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*dk,mkh3d
      subroutine mkh3d (nk,kdim,xic,yic,zic,
     +           nel,kpe,kel,ireg,h,tmp,fcn)
c
c     Compute the Nodal Error Matrix
c

C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none

c arguments
      integer nk,kdim,nel,kpe
      real*8  xic(nk),yic(nk),zic(nk)
      integer kel(kpe,nel),ireg(nel)
      real*8  h(kdim,kdim,nk),tmp(nk)
      external fcn

c variables
      real*8 u(4),xm(3),g(3,4),ht(3,3)

      real*8 h0,a11,a12,a21,a22,
     *       um,em,det,c33,det2,gxy,a23,c22,
     *       a13,a31,a32,a33,c11,c12,c13,c21,c23,c31,c32

      integer ix,iy,k,it,k1,k2,k3,ie,i,j,iv,jv,k4

c
      integer ne
      data ne / 6 /
      integer edge(2,6)
      data edge(1,1),edge(2,1) / 1, 2 /
      data edge(1,2),edge(2,2) / 1, 3 /
      data edge(1,3),edge(2,3) / 2, 3 /
      data edge(1,4),edge(2,4) / 1, 4 /
      data edge(1,5),edge(2,5) / 2, 4 /
      data edge(1,6),edge(2,6) / 3, 4 /
c
      real*8 zero,one
      data zero,one /0.0d0, 1.0d0/
c
      integer igeom_gwt
      common / kent1 / igeom_gwt
c
c*********************************************
c
c.....Initialize
      h0=float(igeom_gwt)
      do ix=1,kdim
         do iy=1,kdim
            do k=1,nk
               h(ix,iy,k)=h0
            end do
         end do
      end do
      if (igeom_gwt.ne.0) return
c
      do k=1,nk
         tmp(k)=1.0d-50
      end do
c
c     Element Loop
c
      do it=1,nel
c
c........Knots
         k1=kel(1,it)
         k2=kel(2,it)
         k3=kel(3,it)
         k4=kel(4,it)
c
c........Affine Matrix
         a11=xic(k1)-xic(k4)
         a12=xic(k2)-xic(k4)
         a13=xic(k3)-xic(k4)
         a21=yic(k1)-yic(k4)
         a22=yic(k2)-yic(k4)
         a23=yic(k3)-yic(k4)
         a31=zic(k1)-zic(k4)
         a32=zic(k2)-zic(k4)
         a33=zic(k3)-zic(k4)
c
c........The Cofactors
         c11= (a22*a33-a23*a32)
         c12=-(a21*a33-a23*a31)
         c13= (a21*a32-a22*a31)
         c21=-(a12*a33-a13*a32)
         c22= (a11*a33-a13*a31)
         c23=-(a11*a32-a12*a31)
         c31= (a12*a23-a13*a22)
         c32=-(a11*a23-a13*a21)
         c33= (a11*a22-a12*a21)
c
c........The Determinant
         det=a11*c11+a12*c12+a13*c13
         det=abs(det)
c
c........The Inverse
         g(1,1)= c11/det
         g(1,2)= c12/det
         g(1,3)= c13/det
         g(1,4)=-g(1,1)-g(1,2)-g(1,3)
         g(2,1)= c21/det
         g(2,2)= c22/det
         g(2,3)= c23/det
         g(2,4)=-g(2,1)-g(2,2)-g(2,3)
         g(3,1)= c31/det
         g(3,2)= c32/det
         g(3,3)= c33/det
         g(3,4)=-g(3,1)-g(3,2)-g(3,3)
c
c........Edge Error
         do k=1,kpe
            iv=kel(k,it)
            xm(1)=xic(iv)
            xm(2)=yic(iv)
            xm(3)=zic(iv)
            call fcn (kdim,xm,ireg(it),u(k))
         end do
         do ix=1,kdim
            do iy=ix,kdim
               ht(ix,iy)=zero
            end do
         end do
         det2=2.0d0*det
         do ie=1,ne
            i=edge(1,ie)
            iv=kel(i,it)
            j=edge(2,ie)
            jv=kel(j,it)
            xm(1)=(xic(iv)+xic(jv))/2.0d0
            xm(2)=(yic(iv)+yic(jv))/2.0d0
            xm(3)=(zic(iv)+zic(jv))/2.0d0
            call fcn (kdim,xm,ireg(it),um)
            em=det2*(2.0d0*um-u(i)-u(j))
            do ix=1,kdim
               do iy=ix,kdim
                  gxy=g(ix,i)*g(iy,j)+g(ix,j)*g(iy,i)
                  ht(ix,iy)=ht(ix,iy)+em*gxy
               end do
            end do
         end do
c
c........Average
         do i=1,kpe
            k=kel(i,it)
            tmp(k)=tmp(k)+det
            do ix=1,kdim
               do iy=ix,kdim
                  h(ix,iy,k)=h(ix,iy,k)+ht(ix,iy)
               end do
            end do
         end do
c
      end do
c
      do ix=1,kdim
         do k=1,nk
            h(ix,ix,k)=h(ix,ix,k)/tmp(k)
         end do
         do iy=ix+1,kdim
            do k=1,nk
               h(ix,iy,k)=h(ix,iy,k)/tmp(k)
               h(iy,ix,k)=h(ix,iy,k)
            end do
         end do
      end do
c
      return
      end
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*dk,errb3d
      subroutine errb3d (n,ntet,nk,kdim,xic,yic,zic,h,
     +           nel,kpe,kel,det,err)
c
c     3D Tetrahedra Error ( Error Bound )
c
C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none

c arguments
      integer n,nk,kdim,nel,kpe
      integer ntet(n)
      real*8  xic(nk),yic(nk),zic(nk)
      real*8  h(kdim,kdim,nk)
      integer kel(kpe,nel)
      real*8  det(nel),err(nel)

c variables
      real*8 dx(6),dy(6),dz(6)

      real*8 geom,cx,cy,cz,esum,e1,e2,ehe

      integer in,it,i,iv,j,jv,ie,i1,i2 
c
      integer ne,nf
      data ne,nf / 6, 4 /
c
      integer edge(2,6),face(2,4)
      data edge(1,1),edge(2,1) / 1, 2 /
      data edge(1,2),edge(2,2) / 1, 3 /
      data edge(1,3),edge(2,3) / 2, 3 /
      data edge(1,4),edge(2,4) / 1, 4 /
      data edge(1,5),edge(2,5) / 2, 4 /
      data edge(1,6),edge(2,6) / 3, 4 /
c
      data face(1,1),face(2,1) / 3, 5 /
      data face(1,2),face(2,2) / 2, 4 /
      data face(1,3),face(2,3) / 1, 4 /
      data face(1,4),face(2,4) / 1, 2 /
c
      real*8 zero,one
      data zero,one /0.0d0, 1.0d0/
C
      integer igeom_gwt
      common / kent1 / igeom_gwt
C
c ********************************************
c
c     Tetrahedra Errors
c
      do in=1,n
         it=ntet(in)
c
c........Edge Lengths
         do ie=1,ne
            i=edge(1,ie)
            iv=kel(i,it)
            j=edge(2,ie)
            jv=kel(j,it)
            dx(ie)=xic(jv)-xic(iv)
            dy(ie)=yic(jv)-yic(iv)
            dz(ie)=zic(jv)-zic(iv)
         end do
c
c........Tetrahedron Face Areas
         geom=zero
         do i=1,nf
            i1=face(1,i)
            i2=face(2,i)
            cx=dy(i1)*dz(i2)-dz(i1)*dy(i2)
            cy=dz(i1)*dx(i2)-dx(i1)*dz(i2)
            cz=dx(i1)*dy(i2)-dy(i1)*dx(i2)
            geom=geom+cx*cx+cy*cy+cz*cz
         end do
c
c........Tetrahedron Volume
         det(it)=dx(4)*cx+dy(4)*cy+dz(4)*cz
         det(it)=abs(det(it))
c
c........Edge Error
            if(igeom_gwt.eq.1) then
               esum=zero
               do ie=1,ne
                  esum=esum+dx(ie)**2+dy(ie)**2+dz(ie)**2
               enddo
               geom=sqrt(geom*esum)
               esum=one
            else
               esum=zero
               do ie=1,ne
                  i=edge(1,ie)
                  iv=kel(i,it)
                  j=edge(2,ie)
                  jv=kel(j,it)
                  e1=dx(ie)*(h(1,2,iv)+h(1,2,jv))*dy(ie)
     +              +dx(ie)*(h(1,3,iv)+h(1,3,jv))*dz(ie)
     +              +dy(ie)*(h(2,3,iv)+h(2,3,jv))*dz(ie)
                  e2=dx(ie)*(h(1,1,iv)+h(1,1,jv))*dx(ie)
     +              +dy(ie)*(h(2,2,iv)+h(2,2,jv))*dy(ie)
     +              +dz(ie)*(h(3,3,iv)+h(3,3,jv))*dz(ie)
                  ehe=2.0d0*e1+e2
                  esum=esum+ehe*ehe
               end do
            endif
c
c........Tetrahedron Error
         err(it)=geom*esum/det(it)
c
      end do
c
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*dk,b3dnxm
      subroutine b3dnxm (n,ntet,en,m,mtet,em,
     +           nk,kdim,xic,yic,zic,h,
     +           nel,kpe,kel,det,err,flip)
c
c     Test for n x m flip ( Error Bound )
c
C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none

c arguments
      integer n,m,nk,kdim,nel,kpe
      real*8 en,em
      integer ntet(n),mtet(m)
      real*8  xic(nk),yic(nk),zic(nk)
      real*8  h(kdim,kdim,nk)
      integer kel(kpe,nel)
      real*8  det(nel),err(nel)
      logical flip

c variables
      real*8 dx(6),dy(6),dz(6)

      real*8 vol,geom,eps,cx,cy,cz,esum,ehe,vtol,
     *       etet,e1,e2

      integer i,ii,it,iv,j,jv,ie,i1,i2
c
      integer ne,nf
      data ne,nf / 6, 4 /
c
      integer edge(2,6),face(2,4)
      data edge(1,1),edge(2,1) / 1, 2 /
      data edge(1,2),edge(2,2) / 1, 3 /
      data edge(1,3),edge(2,3) / 2, 3 /
      data edge(1,4),edge(2,4) / 1, 4 /
      data edge(1,5),edge(2,5) / 2, 4 /
      data edge(1,6),edge(2,6) / 3, 4 /
c
      data face(1,1),face(2,1) / 3, 5 /
      data face(1,2),face(2,2) / 2, 4 /
      data face(1,3),face(2,3) / 1, 4 /
      data face(1,4),face(2,4) / 1, 2 /
c
      real*8 zero,one
      data zero,one /0.0d0, 1.0d0/
C
      integer igeom_gwt
      common / kent1 / igeom_gwt
c
c ****************************************************
c
c     Tetrahedra Errors
c
      flip=.false.
      eps=1.0d-10
      vol=zero
c
c     n tet error
c
      en=zero
      do ii=1,n
         it=ntet(ii)
         if (err(it).le.zero.or.det(it).lt.zero) then
c
c...........Edge Lengths
            do ie=1,ne
               i=edge(1,ie)
               iv=kel(i,it)
               j=edge(2,ie)
               jv=kel(j,it)
               dx(ie)=xic(jv)-xic(iv)
               dy(ie)=yic(jv)-yic(iv)
               dz(ie)=zic(jv)-zic(iv)
            end do
c
c...........Tetrahedron Face Areas
            geom=zero
            do i=1,nf
               i1=face(1,i)
               i2=face(2,i)
               cx=dy(i1)*dz(i2)-dz(i1)*dy(i2)
               cy=dz(i1)*dx(i2)-dx(i1)*dz(i2)
               cz=dx(i1)*dy(i2)-dy(i1)*dx(i2)
               geom=geom+cx*cx+cy*cy+cz*cz
            end do
c
c...........Tetrahedron Volume
            det(it)=dx(4)*cx+dy(4)*cy+dz(4)*cz
            det(it)=abs(det(it))
c
c...........Edge Error
            if(igeom_gwt.eq.1) then
               esum=zero
               do ie=1,ne
                  esum=esum+dx(ie)**2+dy(ie)**2+dz(ie)**2
               enddo
               geom=sqrt(geom*esum)
               esum=one
            else
               esum=zero
               do ie=1,ne
                  i=edge(1,ie)
                  iv=kel(i,it)
                  j=edge(2,ie)
                  jv=kel(j,it)
                  e1=dx(ie)*(h(1,2,iv)+h(1,2,jv))*dy(ie)
     +              +dx(ie)*(h(1,3,iv)+h(1,3,jv))*dz(ie)
     +              +dy(ie)*(h(2,3,iv)+h(2,3,jv))*dz(ie)
                  e2=dx(ie)*(h(1,1,iv)+h(1,1,jv))*dx(ie)
     +              +dy(ie)*(h(2,2,iv)+h(2,2,jv))*dy(ie)
     +              +dz(ie)*(h(3,3,iv)+h(3,3,jv))*dz(ie)
                  ehe=2.0d0*e1+e2
                  esum=esum+ehe*ehe
               end do
            endif
            err(it)=geom*esum/det(it)
         end if
c
c........Tetrahedron Error
         vol=vol+det(it)
         en=en+err(it)
c
      end do
      vtol=eps*vol
      vol=vol+vtol
c
c     m tet error
c
      em=zero
      do ii=1,m
         it=mtet(ii)
c
c........Edge Lengths
         do ie=1,ne
            i=edge(1,ie)
            iv=kel(i,it)
            j=edge(2,ie)
            jv=kel(j,it)
            dx(ie)=xic(jv)-xic(iv)
            dy(ie)=yic(jv)-yic(iv)
            dz(ie)=zic(jv)-zic(iv)
         end do
c
c........Tetrahedron Face Areas
         geom=zero
         do i=1,nf
            i1=face(1,i)
            i2=face(2,i)
            cx=dy(i1)*dz(i2)-dz(i1)*dy(i2)
            cy=dz(i1)*dx(i2)-dx(i1)*dz(i2)
            cz=dx(i1)*dy(i2)-dy(i1)*dx(i2)
            geom=geom+cx*cx+cy*cy+cz*cz
         end do
c
c........Tetrahedron Volume
         det(it)=dx(4)*cx+dy(4)*cy+dz(4)*cz
         det(it)=abs(det(it))
c
c........Edge Error
            if(igeom_gwt.eq.1) then
               esum=zero
               do ie=1,ne
                  esum=esum+dx(ie)**2+dy(ie)**2+dz(ie)**2
               enddo
               geom=sqrt(geom*esum)
               esum=one
            else
               esum=zero
               do ie=1,ne
                  i=edge(1,ie)
                  iv=kel(i,it)
                  j=edge(2,ie)
                  jv=kel(j,it)
                  e1=dx(ie)*(h(1,2,iv)+h(1,2,jv))*dy(ie)
     +              +dx(ie)*(h(1,3,iv)+h(1,3,jv))*dz(ie)
     +              +dy(ie)*(h(2,3,iv)+h(2,3,jv))*dz(ie)
                  e2=dx(ie)*(h(1,1,iv)+h(1,1,jv))*dx(ie)
     +              +dy(ie)*(h(2,2,iv)+h(2,2,jv))*dy(ie)
     +              +dz(ie)*(h(3,3,iv)+h(3,3,jv))*dz(ie)
                  ehe=2.0d0*e1+e2
                  esum=esum+ehe*ehe
               end do
            endif
c
c........Tetrahedron Error
         etet=geom*esum
         vol=vol-det(it)
         flip=(det(it)*(en-em).gt.etet)
     +   .and.(vol.ge.zero)
         if (flip) then
            err(it)=etet/det(it)
            em=em+err(it)
         else
            det(it)=-one
            err(it)=-one
            em=en+one
            return
         end if
c
      end do
      vol=vol-vtol
      flip=(en-em).gt.eps*en
     +   .and.(abs(vol).lt.vtol)
c
      return
      end
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine fcn (kdim,x,ireg,f)
c
c     f(x)
c
C     implicit integer (i-n),real*8 (a-h,o-z)
      implicit none
c arguments
      integer kdim,ireg
      real*8 x(kdim),f
c variables
      integer k

      real*8 ev(3)
      data ev / 1.0d0, 1.0d0, 1.0d0 /

C***********************************************
c
      f=0.0d0
      do k=1,kdim
         f=f+ev(k)*x(k)*x(k)
      end do
      f=f/2.0d0
c
      return
      end
