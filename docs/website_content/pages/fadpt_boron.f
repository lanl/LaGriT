      subroutine fadpt(x,y,z,mat,nvec,time,f)
C #####################################################################
C
C     PURPOSE -
C
C        Adaption function for smoothing algorithms.  This is the
C        boron density function supplied by Kent Smith.  It produces
C        field values that peak at 1.1e18 on an T-shaped region at
C        z=-.28.  
C
C     INPUT ARGUMENTS -
C
C        X,Y,Z - Input spatial coordinate arrays.
C        MAT - Material type arrays.  (This is for cases where the
C              function value depends BOTH on position and material
C              type.)
C        NV - Length of spatial arrays.  (Evaluate function at each
C             spatial coordinate.)
C        TIME  - Current time (for time dependent adaption).
C
C     OUTPUT ARGUMENTS -
C
C        F - Array of adaption function values.
C
C     CHANGE HISTORY -
C
C ######################################################################
      implicit none
 
      integer lenptr
      parameter (lenptr=1000000)

      pointer (ipvk,vk), (ipknots,knots)
      real*8 x(lenptr),y(lenptr),z(lenptr),time
      integer nk,kdim,nv,knots(lenptr),length,j,
     &   icscode,nvec, mat(lenptr)
      real*8 vk(3,lenptr),f(lenptr)
      character*32 isubname
      character*8 cglobal, cdefault
 
      isubname='fadpt'
      cglobal='global'
      cdefault='default'
 
      nv=nvec
      nk=nv
 
      length=nv
      call mmgetblk('knots',isubname,ipknots,length,2,icscode)
      length=3*nv
      call mmgetblk('vk',isubname,ipvk,length,2,icscode)
 
      kdim=3
      do j=1,nvec
         knots(j)=j
         vk(1,j)=x(j)
         vk(2,j)=y(j)
         vk(3,j)=z(j)
      enddo
 
c.... CONC computes a vector of NV boron field values.

      call conc (time,nk,kdim,vk,nv,knots,f)

      call mmrelprt(isubname,icscode)
      return
      end

      subroutine conc (time,nk,kdim,vk,nv,knots,c)
      implicit integer (i-n),double precision (a-h,o-z)
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension c(nk)
c
c     Diffusion Profile C(x) from L shaped Mask
c
      dimension xparm(10),yparm(10),zparm(10)
c
      tiny=-dlog(1.0 d-30)
      emax=dsqrt(tiny)
c
      call odparm ('boron',time,xparm,yparm,zparm)
      z0=zparm(1)
      c0=zparm(2)
      sigma=zparm(3)
c
c.....C(x) = M(x,y)
      call mxy (nk,kdim,vk,nv,knots,sigma,0,c)
c
c.....z Function
      do i=1,nv
         k=knots(i)
         zk=dabs(vk(3,k)-z0)/sigma
         ze=dmin1(zk, emax)
         c(k)=c0*dexp(-ze*ze)*c(k)
      end do
c
      return
      end
      subroutine dconc (time,nk,kdim,vk,nv,knots,c,vel,tmp)
      implicit integer (i-n),double precision (a-h,o-z)
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension c(nk),vel(kdim,nk)
      dimension tmp(nk)
c
c     Diffusion Profile C(x) from L shaped Mask
c
      dimension xparm(10),yparm(10),zparm(10)
c
      tiny=-dlog(1.0 d-30)
      emax=dsqrt(tiny)
c
      call odparm ('boron',time,xparm,yparm,zparm)
      z0=zparm(1)
      c0=zparm(2)
      sigma=zparm(3)
      dsigma=zparm(4)
c
c.....C(x) = M(x,y)
      call dmxy (nk,kdim,vk,nv,knots,sigma,0,
     +     c,vel,tmp)
c
c.....z Function
      zscl=2.0d0/sigma
      do i=1,nv
         k=knots(i)
         zk=(vk(3,k)-z0)/sigma
         z1=dmax1(zk,-emax)
         ze=dmin1(z1, emax)
c
         vel(3,k)=-zscl*ze*c(k)
         vnorm=1.0 d-30
     +        +vel(1,k)*vel(1,k)
     +        +vel(2,k)*vel(2,k)
     +        +vel(3,k)*vel(3,k)
         dcdt=(tmp(k)-vel(3,k)*ze)*dsigma
         v=-dcdt/vnorm
         c(k)=c0*dexp(-ze*ze)*c(k)
         vel(1,k)=v*vel(1,k)
         vel(2,k)=v*vel(2,k)
         vel(3,k)=v*vel(3,k)
c
      end do
c
c.....Modify Boundary Velocities
c     vel(b) = vel(b) + (vbdy - vel(b).n ) n
c
      return
      end
      subroutine mxy (nk,kdim,vk,nv,knots,sigma,isig,c)
      implicit integer (i-n),double precision (a-h,o-z)
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension sigma(1)
      dimension c(nk)
c
c     Mask Profile C(x,y) from L shaped Mask
c
      dimension xk(256,3),ex(256,2),ey(256,2)
      dimension xmask(10),ymask(10),zmask(10)
c
      tiny=-dlog(1.0 d-30)
      emax=dsqrt(tiny)
      call odparm ('geom',tmax,xmask,ymask,zmask)
c
c.....M(x,y) mask function
c
      nseg=(nv-1)/256
      nn=nv-nseg*256
      i0=0
      do iseg=0,nseg
c
c........x Error Functions
         do i=1,nn
            k=knots(i+i0)
            is=1+isig*(k-1)
c
            xk(i,1)=(vk(1,k)-xmask(1))/sigma(is)
            xk(i,1)=dmax1(xk(i,1),-emax)
            xk(i,1)=dmin1(xk(i,1), emax)
c
            xk(i,2)=(vk(1,k)-xmask(2))/sigma(is)
            xk(i,2)=dmax1(xk(i,2),-emax)
            xk(i,2)=dmin1(xk(i,2), emax)
c
            xk(i,3)=(vk(1,k)-xmask(3))/sigma(is)
            xk(i,3)=dmax1(xk(i,3),-emax)
            xk(i,3)=dmin1(xk(i,3), emax)
c
         end do
         call erfcxy (nn,xk(1,1),xk(1,2),ex(1,1))
         call erfcxy (nn,xk(1,2),xk(1,3),ex(1,2))
c
c........y Error Functions
         do i=1,nn
            k=knots(i+i0)
            is=1+isig*(k-1)
c
            xk(i,1)=(vk(2,k)-ymask(1))/sigma(is)
            xk(i,1)=dmax1(xk(i,1),-emax)
            xk(i,1)=dmin1(xk(i,1), emax)
c
            xk(i,2)=(vk(2,k)-ymask(2))/sigma(is)
            xk(i,2)=dmax1(xk(i,2),-emax)
            xk(i,2)=dmin1(xk(i,2), emax)
c
            xk(i,3)=(vk(2,k)-ymask(3))/sigma(is)
            xk(i,3)=dmax1(xk(i,3),-emax)
            xk(i,3)=dmin1(xk(i,3), emax)
c
         end do
         call erfcxy (nn,xk(1,1),xk(1,2),ey(1,1))
         call erfcxy (nn,xk(1,2),xk(1,3),ey(1,2))
c
c........Function
         do i=1,nn
            k=knots(i+i0)
            c(k)=ex(i,1)*ey(i,1)+ex(i,2)*ey(i,1)+ex(i,1)*ey(i,2)
            c(k)=dmax1(c(k),1.0 d-30)
         end do
c
         i0=i0+nn
         nn=256
      end do
c
      return
      end
      subroutine dmxy (nk,kdim,vk,nv,knots,sigma,isig,c,grad,dc)
      implicit integer (i-n),double precision (a-h,o-z)
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension sigma(1)
      dimension c(nk),grad(kdim,nk),dc(nk)
c
c     Diffusion Profile Derivatives C(x) from L shaped Mask
c
c          c(k) =   M(x,y,sigma)
c     grad(1,k) = d M(x,y,sigma) / dx
c     grad(2,k) = d M(x,y,sigma) / dy
c         dc(k) = d M(x,y,sigma) / d(sigma)
c
      dimension xk(256,3)
      dimension ex(256,2),ey(256,2)
      dimension gex(256,2),gey(256,2)
      dimension dex(256,2),dey(256,2)
      dimension xmask(10),ymask(10),zmask(10)
c
      tiny=-dlog(1.0 d-30)
      emax=dsqrt(tiny)
      pi=4.0d0*datan(1.0d0)
      gscl=1.0d0/dsqrt(pi)
c
      call odparm ('geom',tmax,xmask,ymask,zmask)
c
c.....x,y mask function
c
      nseg=(nv-1)/256
      nn=nv-nseg*256
      i0=0
      do iseg=0,nseg
c
c........x Error Functions
         do i=1,nn
            k=knots(i+i0)
            is=1+isig*(k-1)
c
            xk(i,1)=(vk(1,k)-xmask(1))/sigma(is)
            xk(i,1)=dmax1(xk(i,1),-emax)
            xk(i,1)=dmin1(xk(i,1), emax)
c
            xk(i,2)=(vk(1,k)-xmask(2))/sigma(is)
            xk(i,2)=dmax1(xk(i,2),-emax)
            xk(i,2)=dmin1(xk(i,2), emax)
c
            xk(i,3)=(vk(1,k)-xmask(3))/sigma(is)
            xk(i,3)=dmax1(xk(i,3),-emax)
            xk(i,3)=dmin1(xk(i,3), emax)
c
            e1=gscl*dexp(-xk(i,1)*xk(i,1))/sigma(is)
            e2=gscl*dexp(-xk(i,2)*xk(i,2))/sigma(is)
            e3=gscl*dexp(-xk(i,3)*xk(i,3))/sigma(is)
            gex(i,1)=e1-e2
            dex(i,1)=xk(i,2)*e2-xk(i,1)*e1
            gex(i,2)=e2-e3
            dex(i,2)=xk(i,3)*e3-xk(i,2)*e2
c
         end do
         call erfcxy (nn,xk(1,1),xk(1,2),ex(1,1))
         call erfcxy (nn,xk(1,2),xk(1,3),ex(1,2))
c
c........y Error Functions
         do i=1,nn
            k=knots(i+i0)
            is=1+isig*(k-1)
c
            xk(i,1)=(vk(2,k)-ymask(1))/sigma(is)
            xk(i,1)=dmax1(xk(i,1),-emax)
            xk(i,1)=dmin1(xk(i,1), emax)
c
            xk(i,2)=(vk(2,k)-ymask(2))/sigma(is)
            xk(i,2)=dmax1(xk(i,2),-emax)
            xk(i,2)=dmin1(xk(i,2), emax)
c
            xk(i,3)=(vk(2,k)-ymask(3))/sigma(is)
            xk(i,3)=dmax1(xk(i,3),-emax)
            xk(i,3)=dmin1(xk(i,3), emax)
c
            e1=gscl*dexp(-xk(i,1)*xk(i,1))/sigma(is)
            e2=gscl*dexp(-xk(i,2)*xk(i,2))/sigma(is)
            e3=gscl*dexp(-xk(i,3)*xk(i,3))/sigma(is)
            gey(i,1)=-(e2-e1)
            gey(i,2)=-(e3-e2)
            dey(i,1)=xk(i,2)*e2-xk(i,1)*e1
            dey(i,2)=xk(i,3)*e3-xk(i,2)*e2
c
         end do
         call erfcxy (nn,xk(1,1),xk(1,2),ey(1,1))
         call erfcxy (nn,xk(1,2),xk(1,3),ey(1,2))
c
c........Function
         do i=1,nn
            k=knots(i+i0)
            c(k)=ex(i,1)*ey(i,1)+ex(i,2)*ey(i,1)+ex(i,1)*ey(i,2)
            c(k)=dmax1(c(k),1.0 d-30)
            gx=gex(i,1)*ey(i,1)+gex(i,2)*ey(i,1)+gex(i,1)*ey(i,2)
            gy=ex(i,1)*gey(i,1)+ex(i,2)*gey(i,1)+ex(i,1)*gey(i,2)
            grad(1,k)=gx
            grad(2,k)=gy
            dx=dex(i,1)*ey(i,1)+dex(i,2)*ey(i,1)+dex(i,1)*ey(i,2)
            dy=ex(i,1)*dey(i,1)+ex(i,2)*dey(i,1)+ex(i,1)*dey(i,2)
            dc(k)=dx+dy
         end do
c
         i0=i0+nn
         nn=256
      end do
c
      return
      end
      subroutine odparm (option,time,xparm,yparm,zparm)
      implicit integer (i-n),double precision (a-h,o-z)
      character*(*) option
      dimension xparm(1),yparm(1),zparm(1)
c
c     Oxidation / Diffusion Example Parameters
c
c.....Mask Edges
      xparm(1)=-dble(0.5)
      xparm(2)= dble(0.0)
      xparm(3)= dble(0.5)
      yparm(1)=-dble(0.5)
      yparm(2)= dble(0.0)
      yparm(3)= dble(0.5)
c.....tmax
      tmax=dfloat(3600)
c
c.....Box Geometry
      tsi=dble(1.0)
      tox=dble(0.02)
      tpoly=dble(0.06)
      tmask=dble(0.10)
      tair=dble(0.40)
      zparm(1)=0.0d0
      zparm(2)=zparm(1)+tsi
      zparm(3)=zparm(2)+tox
      zparm(4)=zparm(3)+tpoly
      zparm(5)=zparm(4)+tmask
      zparm(6)=zparm(5)+tair
c.....Orgin
      iz0=4
      z0=zparm(iz0)
      do i=1,6
         zparm(i)=zparm(i)-z0
      end do
      zparm(iz0)=0.0d0
c
      if (option.eq.'geom') then
c........Geometry
         box=1.0d0
         xmin=xparm(1)-box
         xmax=xparm(3)+box
         ymin=yparm(1)-box
         ymax=yparm(3)+box
         xparm(4)=xmin
         xparm(5)=xmax
         yparm(4)=ymin
         yparm(5)=ymax
         time=tmax
c
      else if (option.eq.'boron') then
c
c........Boron Diffusion
         z0=zparm(2)-dble(0.2)
         c0=dble(1.1e+18)
         s0=dble(0.05)**2
         d=dble(0.1)/tmax
         s2=s0+d*time
         sigma=dsqrt(s2)
         dsigma=d/(2.0d0*sigma)
c
         zparm(1)=z0
         zparm(2)=c0
         zparm(3)=sigma
         zparm(4)=dsigma
c
      else if (option.eq.'oxide') then
c
c........Oxide
         z0=zparm(4)
         s0=dble(1.0e-4)
         r=dble(0.5)/tmax
         zparm(1)=z0
c
c........Top Oxide 
         up=dble(0.56)
         d=r*up
         dt=d*time
         sigma=s0+dt
         zparm(2)=sigma
         zparm(3)=d
         sxy=sigma**(2.0d0/3.0d0)
         zparm(4)=sxy
         zparm(5)=2.0d0/3.0d0*(sxy/sigma)
c
c........Bottom Oxide 
         down=1.0d0-up
         d=r*down
         dt=d*time
         sigma=s0+dt
         zparm(6)=sigma
         zparm(7)=d
         sxy=sigma**(2.0d0/3.0d0)
         zparm(8)=sxy
         zparm(9)=2.0d0/3.0d0*(sxy/sigma)
c
      end if
c
      return
      end
      subroutine erfcxy (n,x,y,f)
      implicit integer (i-n),double precision (a-h,o-z)
      dimension x(n),y(n),f(n)
c
c     f(i)=1/2 ( erfc(y)-erfc(x) )
c
      integer indx(256),indy(256)
c
c     Ref:Computer Approximations
c         by John F. Hart
c            d. W. Cheney
c            Charles L. Lawson
c            Hans J. Maehly
c            Charles K. Mesztenyi
c            John R. Rice
c            Henry G. Thacher, Jr.
c            Christoph Witzgall
c         John Wiley & Sons., New York, 1968
c         ERFC (5708) x = [0,8]
c         Precision: 16.31
c         ERFC (5725) x = [8,100]
c         Precision: 17.49
c
      save indx,indy
      save p,q
      save x0,half
c
      dimension p(0:19),q(0:19)
      data x0/ 8.0d0 /
      data half/ 0.5 d+0/
c
c     x = [0,8]
c
      data p(0)/ +.37235 07981 55480 67225 6717       d+4/
      data p(1)/ +.71136 63246 95404 98734 09986      d+4/
      data p(2)/ +.67582 16964 11048 58863 32758 6    d+4/
      data p(3)/ +.40322 67010 83004 97362 09572 8    d+4/
      data p(4)/ +.16317 60268 75371 46963 51509 13   d+4/
      data p(5)/ +.45626 14587 06092 63064 18003 11   d+3/
      data p(6)/ +.86082 76221 19484 95117 55453 07   d+2/
      data p(7)/ +.10064 85897 49095 42535 50505 591  d+2/
      data p(8)/ +.56418 95867 61813 61369 25465 862  d+0/
      data p(9)/ 0.0                                  d+0/
c
      data q(0)/ +.37235 07981 55480 65435 2472       d+4/
      data q(1)/ +.11315 19208 18544 05468 20144 3    d+5/
      data q(2)/ +.15802 53599 94020 42527 35884 57   d+5/
      data q(3)/ +.13349 34656 12844 57371 72173 17   d+5/
      data q(4)/ +.75424 79510 19347 57554 72085 83   d+4/
      data q(5)/ +.29680 04901 48230 87164 27652 719  d+4/
      data q(6)/ +.81762 23863 04544 07702 82502 642  d+3/
      data q(7)/ +.15307 77107 50362 21585 69520 624  d+3/
      data q(8)/ +.17839 49843 91395 56528 84238 734  d+2/
      data q(9)/ +.1                                  d+1/
c
c     x = [8,100]
c
      data p(10)/ +.29788 65626 39399 28862            d+1/
      data p(11)/ +.74097 40605 96474 17944 25         d+1/
      data p(12)/ +.61602 09853 10963 05440 906        d+1/
      data p(13)/ +.50190 49726 78426 74634 50058      d+1/
      data p(14)/ +.12753 66644 72996 59524 79585 264  d+1/
      data p(15)/ +.56418 95835 47755 07412 53201 704  d+0/
      data p(16)/ 0.0                                  d+0/
      data p(17)/ 0.0                                  d+0/
      data p(18)/ 0.0                                  d+0/
      data p(19)/ 0.0                                  d+0/
c
      data q(10)/ +.33690 75206 98275 27677            d+1/
      data q(11)/ +.96089 65327 19278 78706 98         d+1/
      data q(12)/ +.17081 44074 74660 04315 71095      d+2/
      data q(13)/ +.12048 95192 78551 29036 03404 91   d+2/
      data q(14)/ +.93960 34016 23505 41504 30579 648  d+1/
      data q(15)/ +.22605 28520 76732 69695 91866 945  d+1/
      data q(16)/ +.1                                  d+1/
      data q(17)/ 0.0                                  d+0/
      data q(18)/ 0.0                                  d+0/
      data q(19)/ 0.0                                  d+0/
c
      data indx/256*0/
      data indy/256*0/
c
      nseg=(n-1)/256
      nn=n-nseg*256
      i0=0
      do iseg=0,nseg 
c
c........Set index
         do i=1,nn
            k=i+10
            xa=dabs(x(k))
            if (xa.gt.x0) indx(i)=10
            ya=dabs(y(k))
            if (ya.gt.x0) indy(i)=10
         end do
c
c........erfc
         do i=1,nn
            k=i+i0
            xa=dabs(x(k))
            kx=indx(i)
c
            px7=p(kx+7)+xa*p(kx+8)
            px6=p(kx+6)+xa*px7
            px5=p(kx+5)+xa*px6
            px4=p(kx+4)+xa*px5
            px3=p(kx+3)+xa*px4
            px2=p(kx+2)+xa*px3
            px1=p(kx+1)+xa*px2
            px0=p(kx)+xa*px1
c
            qx8=q(kx+8)+xa*q(kx+9)
            qx7=q(kx+7)+xa*qx8
            qx6=q(kx+6)+xa*qx7
            qx5=q(kx+5)+xa*qx6
            qx4=q(kx+4)+xa*qx5
            qx3=q(kx+3)+xa*qx4
            qx2=q(kx+2)+xa*qx3
            qx1=q(kx+1)+xa*qx2
            qx0=q(kx)+xa*qx1
c
            fx=dexp(-xa*xa)*(px0/qx0)
            sx=dsign(half,x(k))
            indx(i)=0
c
            ya=dabs(y(k))
            ky=indy(i)
c
            py7=p(ky+7)+ya*p(ky+8)
            py6=p(ky+6)+ya*py7
            py5=p(ky+5)+ya*py6
            py4=p(ky+4)+ya*py5
            py3=p(ky+3)+ya*py4
            py2=p(ky+2)+ya*py3
            py1=p(ky+1)+ya*py2
            py0=p(ky)+ya*py1
c
            qy8=q(ky+8)+ya*q(ky+9)
            qy7=q(ky+7)+ya*qy8
            qy6=q(ky+6)+ya*qy7
            qy5=q(ky+5)+ya*qy6
            qy4=q(ky+4)+ya*qy5
            qy3=q(ky+3)+ya*qy4
            qy2=q(ky+2)+ya*qy3
            qy1=q(ky+1)+ya*qy2
            qy0=q(ky)+ya*qy1
c
            fy=dexp(-ya*ya)*(py0/qy0)
            sy=dsign(half,y(k))
            indy(i)=0
c
            f(k)=(sx-sy)+sy*fy-sx*fx
         end do
         i0=i0+nn
         nn=256
      end do
c
      return
      end
      subroutine zox (option,nk,kdim,vk,time,nv,knots,z)
      implicit integer (i-n),double precision (a-h,o-z)
      character*(*) option
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension z(nk)
c
c     Compute Moving Oxide Boundary
c
      dimension xparm(10),yparm(10),zparm(10)
c
c.....Oxide Parameters
      call odparm ('oxide',time,xparm,yparm,zparm)
      z0=zparm(1)
      if (option(1:1).eq.'u') then
         sigma= zparm(2)
         sxy=zparm(4)
      else
         sigma=-zparm(6)
         sxy=zparm(8)
      end if
c
c.....Oxide Boundary
      call mxy (nk,kdim,vk,nv,knots,sxy,0,z)
      do i=1,nv
         k=knots(i)
         z(k)=z0+sigma*dsqrt(z(k))
      end do
c
      return
      end
      subroutine dzox (option,nk,kdim,vk,time,nv,knots,z,vel,tmp)
      implicit integer (i-n),double precision (a-h,o-z)
      character*(*) option
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension z(nk),vel(kdim,nk)
      dimension tmp(nk)
c
c     Compute Moving Oxide Boundary and Velocity Field
c
      dimension xparm(10),yparm(10),zparm(10)
c
c.....Storage
      m=0
      idt=m+nk
c
c.....Oxide Parameters
      call odparm ('oxide',time,xparm,yparm,zparm)
      z0=zparm(1)
c
      if (option(1:1).eq.'u') then
         sigma= zparm(2)
         vz=zparm(3)
         sxy=zparm(4)
         dsxy=zparm(5)
      else
         sigma=-zparm(6)
         vz=zparm(7)
         sxy=zparm(8)
         dsxy=zparm(9)
      end if
c
c.....d M(x,y)
      call dmxy (nk,kdim,vk,nv,knots,sxy,0,
     +     tmp(m+1),vel,tmp(idt+1))
c
c     Boundary Points and Velocity Field
c
      gscl=2.0d0/sigma
      do i=1,nv
         k=knots(i)
         zk=dsqrt(tmp(m+k))
         z(k)=z0+sigma*zk
c........Gradient
         vel(1,k)=-vel(1,k)
         vel(2,k)=-vel(2,k)
         vel(3,k)=gscl*zk
         vnorm=1.0 d-30
     +        +vel(1,k)*vel(1,k)
     +        +vel(2,k)*vel(2,k)
     +        +vel(3,k)*vel(3,k)
         scl=vz/vnorm
         v=scl*(zk*vel(3,k)+dsxy*tmp(idt+k))
         vel(1,k)=v*vel(1,k)
         vel(2,k)=v*vel(2,k)
         vel(3,k)=v*vel(3,k)
      end do
c
      return
      end
      subroutine vox (nk,kdim,vk,time,nv,knots,sk,vel,
     +           itmp,tmp)
      implicit integer (i-n),double precision (a-h,o-z)
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension sk(nk)
      dimension vel(kdim,nk)
      integer itmp(nv)
      dimension tmp(nk)
c
c     Compute Velocity Field for Moving Oxide Grid
c
      dimension xparm(10),yparm(10),zparm(10)
c
      newton=25
      tol=dble(1.0e-6)
      zmin=dble(1.0e-6)
      one=1.0d0+tol
c
c.....Storage
      isxy=0
      ic=isxy+nk
      idc=ic+nk
c
c.....Oxide Parameters
      call odparm ('oxide',time,xparm,yparm,zparm)
      z0=zparm(1)
c
c.....Initialize
      do i=1,nv
         k=knots(i)
         vel(1,k)=0.0d0
         vel(2,k)=0.0d0
         vel(3,k)=0.0d0
      end do
c
      do ireg=1,2
         iz=2+4*(ireg-1)
         sigma=zparm(iz)
         sgn=dfloat(3-2*ireg)
         vz=zparm(iz+1)
c
c........Oxide Points
         sxy=zparm(iz+2)
         call mxy (nk,kdim,vk,nv,knots,sxy,0,tmp)
         nox=0
         do i=1,nv
            k=knots(i)
            zk=sgn*(vk(3,k)-z0)
            z2=(zk/sigma)**2
            if (zk.gt.zmin.and.z2.le.one*tmp(k)) then
               nox=nox+1
               itmp(nox)=k
            end if
         end do
         if (nox.eq.0) go to 200
c
c        Find sigma contour
c
         do nwt=1,newton
c
c...........sxy(k)
            do i=1,nox
               k=itmp(i)
               tmp(isxy+k)=sk(k)**(2.0d0/3.0d0)
            end do
c
c...........d M(x,y)
            call dmxy (nk,kdim,vk,nox,itmp,tmp(isxy+1),1,
     +           tmp(ic+1),vel,tmp(idc+1))
            do i=1,nox
               k=itmp(i)
               zk=(vk(3,k)-z0)/sk(k)
               g=zk*zk-tmp(ic+k)
               dsxy=2.0d0/3.0d0*(tmp(isxy+k)/sk(k))
               dg=(2.0d0/sk(k))*zk*zk+dsxy*tmp(idc+k)
c..............Bisection ??
               g0=g+dg*sk(k)
               if (dg.le.0.0d0) then
                  sk(k)=sk(k)/2.0d0
                  tmp(i)=sk(k)
               else if (g0.gt.0.0d0) then
                  tmp(i)=g/dg
                  sk(k)=sk(k)+tmp(i)
               else
                  sk(k)=sk(k)/2.0d0
                  tmp(i)=sk(k)
               end if
               tmp(ic+i)=sk(k)
            end do
c
c...........Convergence Test
c           dsnorm=snrm2(nox,tmp,1)
c           snorm=snrm2(nox,tmp(ic+1),1)
c           if (dsnorm.lt.tol*snorm) go to 100
c
         end do
  100    continue
c
c........Velocity Field
c
         do i=1,nox
            k=itmp(i)
            zk=(vk(3,k)-z0)/sk(k)
c........Gradient
            vel(1,k)=-vel(1,k)
            vel(2,k)=-vel(2,k)
            vel(3,k)=2.0d0*zk/sk(k)
            vnorm=1.0 d-30
     +           +vel(1,k)*vel(1,k)
     +           +vel(2,k)*vel(2,k)
     +           +vel(3,k)*vel(3,k)
            dsxy=2.0d0/3.0d0*(tmp(isxy+k)/sk(k))
            scl=vz*(sk(k)/sigma)/vnorm
            v=scl*(zk*vel(3,k)+dsxy*tmp(idc+k))
            vel(1,k)=v*vel(1,k)
            vel(2,k)=v*vel(2,k)
            vel(3,k)=v*vel(3,k)
         end do
c
  200    continue
      end do
c
      return
      end
      subroutine voxb (nk,kdim,vk,time,nv,knots,vel,itmp,tmp)
      implicit integer (i-n),double precision (a-h,o-z)
      integer knots(nv)
      dimension vk(kdim,nk)
      dimension vel(kdim,nk)
      integer itmp(nv)
      dimension tmp(nk)
c
c     Compute Oxide Boundary Velocity
c
      dimension xparm(10),yparm(10),zparm(10)
c
c.....Storage
      m=0
      idt=m+nk
c
c.....Oxide Parameters
      call odparm ('oxide',time,xparm,yparm,zparm)
      z0=zparm(1)
c
c.....Initialize
      do i=1,nv
         k=knots(i)
         vel(1,k)=0.0d0
         vel(2,k)=0.0d0
         vel(3,k)=0.0d0
      end do
c
      do ireg=1,2
         iz=2+4*(ireg-1)
         sigma=zparm(iz)
         sgn=dfloat(3-2*ireg)
         vz=zparm(iz+1)
         sxy=zparm(iz+2)
         dsxy=zparm(iz+3)
c
c........Oxide Points
         nox=0
         do i=1,nv
            k=knots(i)
            zk=sgn*(vk(3,k)-z0)
            if (zk.ge.0.0d0) then
               nox=nox+1
               itmp(nox)=k
            end if
         end do
c
         if (nox.eq.0) go to 100
c
c...........d M(x,y)
            call dmxy (nk,kdim,vk,nox,itmp,sxy,0,
     +           tmp(m+1),vel,tmp(idt+1))
c
c           Boundary Velocity Field
c
            gscl=2.0d0/sigma
            do i=1,nox
               k=itmp(i)
               zk=dsqrt(tmp(m+k))
               vk(3,k)=z0+sigma*zk
c..............Gradient
               vel(1,k)=-vel(1,k)
               vel(2,k)=-vel(2,k)
               vel(3,k)=gscl*zk
               vnorm=1.0 d-30
     +              +vel(1,k)*vel(1,k)
     +              +vel(2,k)*vel(2,k)
     +              +vel(3,k)*vel(3,k)
               scl=vz/vnorm
               v=scl*(zk*vel(3,k)+dsxy*tmp(idt+k))
               vel(1,k)=v*vel(1,k)
               vel(2,k)=v*vel(2,k)
               vel(3,k)=v*vel(3,k)
            end do
  100    continue
      end do
c
      return
      end
