      subroutine mega3d_inner_loop(action,node,lusefd,ctrl,
     &               nodhyb,nodhyboff,itp1,icr1,icontab,
     &               ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &               ipivoloffoff,itettyp,itetclr,itet,itetoff,xic
     &               ,yic,zic,nnodes,nelements,iedge,iedgeoff
     &               ,iedgemat,ichildary,ichildno,invchildary,imt1
     &               ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz
     &               ,hyy,hyz,hzz,range,pf,pfx,pfxx,xsave,ysave,
     &               zsave,ipoffsparam,ipsurfparam,ipistype,err,pftot)
C
C #####################################################################
C
C     PURPOSE -
C
C     MEGA3D_inner_loop returns the new coordinates of node
C     and err
C
C     CHANGE HISTORY -
C
C        $Log: mega3d_inner_loop.f,v $
C        Revision 2.00  2007/11/05 19:46:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   08 Feb 2006 14:35:42   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.3   19 Nov 2002 14:33:22   dcg

CPVCS    'fix geom option to skip calls having to do with calculating and using hessian'

CPVCS    

CPVCS       Rev 1.2   26 Feb 2002 12:07:02   dcg

CPVCS    pass ctrl and range variables correctly

CPVCS    

CPVCS       Rev 1.1   20 Feb 2002 17:00:20   kuprat

CPVCS     If no. of incident elements greater than MAXEL, do not move node.

CPVCS    

CPVCS       Rev 1.0   05 Feb 2002 10:35:24   dcg

CPVCS    Initial revision.

C
      implicit none
 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'geom_lg.h'
      include 'smooth.h'
 
      character*32 action

      integer imt1(*),itp1(*),icr1(*),icontab(*)
      integer iedgeoff(*),iedge(*),iedgemat(*)
      integer itetclr(*),itet(*),itetoff(*),itettyp(*)
 
      pointer (iplocvoloff,locvoloff), (ipivoloffoff,ivoloffoff)
      integer locvoloff(*),ivoloffoff(*)

      integer invmpary(*),ichildary(*),
     &   invchildary(*),ieltary(*)
 
      integer iparent(*)
      integer nodhyb(*),nodhyboff(*)

      pointer (ipvoloff,voloff)
      real*8 voloff(*)

      real*8 xic(*)
      real*8 yic(*)
      real*8 zic(*)
      real*8 fvec(*),reffield(*)
      real*8 xsave(*),ysave(*),zsave(*)
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*)

      real*8 err,
     &   ctrl,dx,dy,dz,xnew,ynew,znew,
     &   pf,pfx(3),pfxx(3,3),
     &   pftot,epsilonl,epsilonv,rdamp,ds_in,dx_in,
     &   dy_in,dz_in,ds,xmn,xmx,ymn,ymx,zmn,zmx,dxpoly,dypoly
     &   ,dzpoly,fdx,fdy,fdz,range

      real*8 absvoltol,relvoltol
      real*8 pfval(-1:1,-1:1,-1:1)

      integer mpk,lochybnod
 
      integer nnodes,nelements,i,k,
     &   ineghess,izeromove_forcvol,izeromove_free,
     &   izeromove_val,node,ichildno,ieltno,j,
     &   ii,ihyb,idamp,nincelts
 
 
      integer istencil(3,10)
      data istencil / 0,0,0,
     &   1,0,0,
     &   0,1,0,
     &   0,0,1,
     &   -1,0,0,
     &   0,-1,0,
     &   0,0,-1,
     &   0,1,1,
     &   1,0,1,
     &   1,1,0 /
      save istencil

      logical lplanar
      logical lusefd

C
C ##############################################################
C BEGIN begin
C
 
      idamp=0
      rdamp=1.
 1010 continue
 
      mpk=invmpary(node)
      nincelts=nodhyboff(mpk+1)-nodhyboff(mpk)
c.... We do a no-op here, because NODE is likely a node
c.... of symmetry that should not be moved.
      if (nincelts.gt.maxel) then
         goto 30
      endif
 
      if (lusefd) then
         xmn=1.d99
         xmx=-1.d99
         ymn=1.d99
         ymx=-1.d99
         zmn=1.d99
         zmx=-1.d99
         do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
            ii=1+(nodhyb(i)-1)/maxnen
            lochybnod=nodhyb(i)-maxnen*(ii-1)
            ihyb=ieltary(ii)
            do j=1,nelmnen(itettyp(ihyb))
               xmn=min(xmn,xic(itet(itetoff(ihyb)+j)))
               xmx=max(xmx,xic(itet(itetoff(ihyb)+j)))
               ymn=min(ymn,yic(itet(itetoff(ihyb)+j)))
               ymx=max(ymx,yic(itet(itetoff(ihyb)+j)))
               zmn=min(zmn,zic(itet(itetoff(ihyb)+j)))
               zmx=max(zmx,zic(itet(itetoff(ihyb)+j)))
            enddo
         enddo
 
         dxpoly=xmx-xmn
         dypoly=ymx-ymn
         dzpoly=zmx-zmn
         fdx=1.e-5*dxpoly+epsilonl
         fdy=1.e-5*dypoly+epsilonl
         fdz=1.e-5*dzpoly+epsilonl
 
c.... Increment the x,y,z coordinates of NODE in a stencil
c.... pattern to make possible numerical differencing to
c.... evaluate the derivatives of the polyhedral function.
c.... Since PARENT nodes are being relaxed, we only have
c.... to increment NODE's coordinates.
c.... Although child point function values can be discontinuous,
c.... their discontinuity only enters into the computation
c.... of element gradients.  Point gradients (and element
c.... hessians) live in a parent point world and so do
c.... node relaxations.
 
         do i=1,10
            dx=fdx*istencil(1,i)
            dy=fdy*istencil(2,i)
            dz=fdz*istencil(3,i)
            xic(node)=xic(node)+dx
            yic(node)=yic(node)+dy
            zic(node)=zic(node)+dz
            call polyfun(4,action,node,nodhyb,nodhyboff,
     &               ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &               ipivoloffoff,itettyp,itetclr,itet,itetoff,xic
     &               ,yic,zic,nnodes,nelements,iedge,iedgeoff
     &               ,iedgemat,ichildary,ichildno,invchildary,imt1
     &               ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz
     &               ,hyy,hyz,hzz,range,pf,pfx,pfxx)
            pfval(istencil(1,i),istencil(2,i),istencil(3,i))
     &               =pf
            xic(node)=xic(node)-dx
            yic(node)=yic(node)-dy
            zic(node)=zic(node)-dz
         enddo
 
         pf=pfval(0,0,0)
 
         pfx(1)=( pfval(1,0,0) - pfval(-1,0,0) )/(2.*fdx)
         pfx(2)=( pfval(0,1,0) - pfval(0,-1,0) )/(2.*fdy)
         pfx(3)=( pfval(0,0,1) - pfval(0,0,-1) )/(2.*fdz)
 
         pfxx(1,1)= ( pfval(1,0,0) - 2.*pfval(0,0,0) +
     &            pfval(-1,0,0) )/(fdx**2)
         pfxx(2,2)= ( pfval(0,1,0) - 2.*pfval(0,0,0) +
     &            pfval(0,-1,0) )/(fdy**2)
         pfxx(3,3)= ( pfval(0,0,1) - 2.*pfval(0,0,0) +
     &            pfval(0,0,-1) )/(fdz**2)
 
         pfxx(1,2)= ( pfval(1,1,0) + pfval(0,0,0) - pfval(1
     &            ,0,0)- pfval(0,1,0) )/(fdx*fdy)
         pfxx(1,3)= ( pfval(1,0,1) + pfval(0,0,0) - pfval(1
     &            ,0,0)- pfval(0,0,1) )/(fdx*fdz)
         pfxx(2,3)= ( pfval(0,1,1) + pfval(0,0,0) - pfval(0
     &            ,0,1)- pfval(0,1,0) )/(fdy*fdz)
         pfxx(2,1)=pfxx(1,2)
         pfxx(3,1)=pfxx(1,3)
         pfxx(3,2)=pfxx(2,3)
 
      else
         call polyfun(3,action,node,nodhyb,nodhyboff,
     &            ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &            ipivoloffoff,itettyp,itetclr,itet,itetoff,xic
     &            ,yic,zic,nnodes,nelements,iedge,iedgeoff
     &            ,iedgemat,ichildary,ichildno,invchildary,imt1
     &            ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz,hyy
     &            ,hyz,hzz,range,pf,pfx,pfxx)
       endif
 
       call freemove(action,node,itp1,icr1,nodhyb,nodhyboff,
     &         ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &         ipivoloffoff,itettyp,icontab,surfparam,
     &         offsparam,istype,
     &         itet,itetoff,xic,yic,zic,epsilonl,rdamp,lplanar,
     &         pf,pfx,pfxx,dx,dy,dz)
 
       pftot=pftot+pf
 
       if (dx.eq.0..and.dy.eq.0..and.dz.eq.0.) then
               izeromove_free=izeromove_free+1
               goto 30
       endif
 
       if (pfx(1)*dx+pfx(2)*dy+pfx(3)*dz.gt.0.) then
          ineghess=ineghess+1
          if (lplanar) then
             dx=-dx
             dy=-dy
             dz=-dz
          else
             rdamp=rdamp*0.5
             idamp=idamp+1
             if (idamp.le.25) then
                  goto 1010
             else
               if (idebug.ge.1) then
                  print*,'Cannot converge on nonplanar'
     &                     ,' constraint'
               endif
               dx=0.
               dy=0.
               dz=0.
               goto 30
             endif
          endif
      endif
 
      ds_in=sqrt(dx**2+dy**2+dz**2)
      dx_in=dx
      dy_in=dy
      dz_in=dz
 
 
      absvoltol=epsilonv
 
      relvoltol=0.25
      call damp_pt_by_volume(node,nodhyb,nodhyboff,
     &         ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &         ipivoloffoff,
     &         itettyp,itet,itetoff,xic,yic,zic,
     &         absvoltol,relvoltol,
     &         dx,dy,dz)
 
      if (dx.eq.0..and.dy.eq.0..and.dz.eq.0.) then
          izeromove_forcvol=izeromove_forcvol+1
          goto 30
      endif
 
      call damp_pt_by_value(action,node,nodhyb,nodhyboff,
     &         ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &         ipivoloffoff,itettyp,itetclr,itet,itetoff,xic
     &         ,yic,zic,nnodes,nelements,iedge,iedgeoff
     &         ,iedgemat,ichildary,ichildno,invchildary,imt1
     &         ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz,hyy
     &         ,hyz,hzz,range,pf,pfx,pfxx,dx,dy,dz)
 
      if (dx.eq.0..and.dy.eq.0..and.dz.eq.0.) then
         izeromove_val=izeromove_val+1
         goto 30
      endif
 
      if (dx.ne.dx_in.or.dy.ne.dy_in.or.dz.ne.dz_in) then
         if (.not.lplanar) then
            ds=sqrt(dx**2+dy**2+dz**2)
            rdamp=rdamp*ds/ds_in*0.8
            idamp=idamp+1
            if (idamp.le.25) then
               goto 1010
            else
               if (idebug.ge.1) then
                  print*,'Cannot converge on nonplanar'
     &                     ,' constraint'
               endif
               dx=0.
               dy=0.
               dz=0.
               goto 30
            endif
         endif
      endif
 
      xnew=xic(node)+dx
      ynew=yic(node)+dy
      znew=zic(node)+dz
 
c...controlled smoothing.
 
      if (ctrl.ne.0.) then
         xnew=xnew*(1.-ctrl)+xsave(k)*ctrl
         ynew=ynew*(1.-ctrl)+ysave(k)*ctrl
         znew=znew*(1.-ctrl)+zsave(k)*ctrl
      endif
 
      dx=xnew-xic(node)
      dy=ynew-yic(node)
      dz=znew-zic(node)
      if(action(1:4).ne.'geom') then
         call interpolate_hessian(node,nodhyb,nodhyboff,
     &         ieltary,invmpary,itettyp,itet,itetoff,xic,yic,zic
     &         ,dx,dy,dz,hxx,hxy,hxz,hyy,hyz,hzz)
      endif
      err=err+dx**2+dy**2+dz**2
 
      xic(node)=xnew
      yic(node)=ynew
      zic(node)=znew
 
 30   continue
 
      return
      end
