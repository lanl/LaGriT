      subroutine laplace2d(cmo,mpary,mpno,ctrl,ierror)
c
c #####################################################################
c
c     purpose -
c
c     LAPLACE2d smooths 2d mesh objects using
c     node position averaging.  We do not attempt
c     to inhibit flipping of element orientations.
c
c     input arguments -
c
c         cmo - name of current mesh object
c         mpary - array of nodes to be smoothed
c         mpno - length of mpary
c         ctrl - control parameter for controlled smoothing
c
c     output arguments -
c
c         ierror - error return code (==0 ==> ok, <>0 ==> error)
c
c     change history -
c
c        $Log: laplace2d.f,v $
c        Revision 2.00  2007/11/05 19:46:00  spchu
c        Import to CVS
c
CPVCS    
CPVCS       Rev 1.0   08 Feb 2000 10:01:40   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   31 Jan 2000 17:13:18   kuprat
CPVCS    Initial revision.
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer mpary(lenptr)
 
      character*132 logmess
 
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
 
      integer itp1(lenptr)
      real*8 xic(lenptr)
      real*8 yic(lenptr)
      real*8 zic(lenptr)
      integer itet(lenptr)
      integer itetoff(lenptr)
      integer itettyp(lenptr)
 
      pointer (ipxsave,xsave), (ipysave,ysave), (ipzsave,zsave)
      real*8 xsave(lenptr), ysave(lenptr), zsave(lenptr)
 
      pointer (ipnoditet,noditet), (ipnoditetoff,noditetoff),
     &   (ipwtnoditet,wtnoditet)
 
      pointer (ipireal1,ireal1)
      integer noditet(2,lenptr),noditetoff(lenptr),
     &   ireal1(lenptr)
      real*8 wtnoditet(lenptr)
      pointer (ipout,out)
      real*8 out(*)
 
      real*8 tn1(3),tni(3),ctrl,x1,y1,z1,
     &   x2,y2,z2,x3,y3,z3,dcross,areamin,areamax,epsilonl,
     &   tolconv_sm,err,xold,yold,zold,xnew,ynew,
     &   znew,err1,dcrossp,areaminp,areamaxp,wt
 
      integer mpno,ierror,nnodes,length,icmotype,nelements,
     &   mbndry,ilen,ityp,icscode,nod1,nod2,nod3,nod4,ierrw,
     &   i,ioff,maxiter_sm,itypconv_sm,k,iter,node,nnitet,
     &   ierrdum,indx,ntri,nqud,nsimp,j,iout
 
      real*8 epsilona
 
      character*32 cmo,cout
      character*32 isubname

 
c statement functions for the components of the cross product
c ((x2,y2,z2)-(x1,y1,z1)) x ((x3,y3,z3)-(x1,y1,z1)) .
      real*8 x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_,crosx,crosy,crosz
      crosx(x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_)=(y2_-y1_)*(z3_-z1_)
     &   -(z2_-z1_)*(y3_-y1_)
      crosy(x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_)=(z2_-z1_)*(x3_-x1_)
     &   -(x2_-x1_)*(z3_-z1_)
      crosz(x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_)=(x2_-x1_)*(y3_-y1_)
     &   -(y2_-y1_)*(x3_-x1_)
 
      isubname = 'laplace2d'

      ierror=0
 
c  get info from mesh object to be smoothed
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *   mbndry,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierror)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ierror)
 
c...  get epsilon length.
 
      call get_epsilon('epsilonl', epsilonl)
      print*,'epsilonl=',epsilonl
 
c...  compute epsilon area.  The expression in brackets is the
c...  epsilon length divided by the machine epsilon... this
c...  ought to be the characteristic length of the grid.
 
      epsilona=epsilonl*(epsilonl/1.d-8)
      print*,'epsilona=',epsilona
 
C        1) Do we have a real point?
C             ireal1() =  0 ==> not a real point.
C             ireal1() =  1 ==> a real point.
C
 
      call mmgetblk('ireal1',isubname,ipireal1,nnodes,1,icscode)
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
 
c  Compute a unit normal for the 'whole mesh' by adding up all the
c  area vectors for all the elements and normalizing.
 
      tn1(1)=0.
      tn1(2)=0.
      tn1(3)=0.
      do i=1,nelements
         ioff=itetoff(i)
         nod1=itet(1+ioff)
         nod2=itet(2+ioff)
         nod3=itet(3+ioff)
         x1=xic(nod1)
         y1=yic(nod1)
         z1=zic(nod1)
         x2=xic(nod2)
         y2=yic(nod2)
         z2=zic(nod2)
         x3=xic(nod3)
         y3=yic(nod3)
         z3=zic(nod3)
         tni(1)=crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         tni(2)=crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         tni(3)=crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         if (itettyp(i).eq.ifelmqud) then
            nod4=itet(4+ioff)
            x1=xic(nod3)
            y1=yic(nod3)
            z1=zic(nod3)
            x2=xic(nod4)
            y2=yic(nod4)
            z2=zic(nod4)
            x3=xic(nod1)
            y3=yic(nod1)
            z3=zic(nod1)
            tni(1)=tni(1)+crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            tni(2)=tni(2)+crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            tni(3)=tni(3)+crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         endif
 
         tn1(1)=tn1(1)+tni(1)
         tn1(2)=tn1(2)+tni(2)
         tn1(3)=tn1(3)+tni(3)
 
      enddo
 
 
      dcross=sqrt(tn1(1)**2+tn1(2)**2+tn1(3)**2)
 
      if (dcross.lt.2*epsilona) then
 
         write(logmess,'(a)')
     *      'laplace2d: Cannot get a single unit normal for mesh.'
         call writloga('default',0,logmess,0,ierrw)
 
      else
 
         tn1(1)=tn1(1)/dcross
         tn1(2)=tn1(2)/dcross
         tn1(3)=tn1(3)/dcross
 
      endif
 
      areamin=1.d99
      areamax=-1.d99
      areaminp=1.d99
      areamaxp=-1.d99
      do i=1,nelements
         ioff=itetoff(i)
         nod1=itet(1+ioff)
         nod2=itet(2+ioff)
         nod3=itet(3+ioff)
         x1=xic(nod1)
         y1=yic(nod1)
         z1=zic(nod1)
         x2=xic(nod2)
         y2=yic(nod2)
         z2=zic(nod2)
         x3=xic(nod3)
         y3=yic(nod3)
         z3=zic(nod3)
         tni(1)=crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         tni(2)=crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         tni(3)=crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         if (itettyp(i).eq.ifelmqud) then
            nod4=itet(4+ioff)
            x1=xic(nod3)
            y1=yic(nod3)
            z1=zic(nod3)
            x2=xic(nod4)
            y2=yic(nod4)
            z2=zic(nod4)
            x3=xic(nod1)
            y3=yic(nod1)
            z3=zic(nod1)
            tni(1)=tni(1)+crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            tni(2)=tni(2)+crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            tni(3)=tni(3)+crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         endif
 
         dcross=sqrt(tni(1)**2+tni(2)**2+tni(3)**2)
 
         areamin=min(areamin,dcross*0.5)
         areamax=max(areamax,dcross*0.5)
 
         dcrossp=tni(1)*tn1(1)+tni(2)*tn1(2)+tni(3)*tn1(3)
 
         areaminp=min(areaminp,dcrossp*0.5)
         areamaxp=max(areamaxp,dcrossp*0.5)
 
      enddo
 
 
      write(logmess,'(2(a,e14.7))')
     *   ' min/max area before smoothing=',areamin,' / ',areamax
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(2(a,e14.7))')
     *   ' min/max projected area before smoothing=',
     &   areaminp,' / ',areamaxp
      call writloga('default',0,logmess,0,ierrw)
 
c if we are using the 'control' option, we need to save the
c node positions of the unsmoothed mesh.
      if (ctrl.ne.0.) then
         call mmgetblk('xsave',isubname,ipxsave,mpno,2,icscode)
         call mmgetblk('ysave',isubname,ipysave,mpno,2,icscode)
         call mmgetblk('zsave',isubname,ipzsave,mpno,2,icscode)
         do i=1,mpno
            xsave(i)=xic(mpary(i))
            ysave(i)=yic(mpary(i))
            zsave(i)=zic(mpary(i))
         enddo
      endif
 
      call cmo_get_info('maxiter_sm',cmo,
     *   maxiter_sm,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('itypconv_sm',cmo,
     *   itypconv_sm,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_attinfo('tolconv_sm',cmo,iout,
     *   tolconv_sm,cout,ipout,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_r')
 
      itypconv_sm=1
      tolconv_sm=.05
 
      write(logmess,'(a,i5)')
     *   'maxiter_sm=',maxiter_sm
 
c laplace smoothing.
 
c obtain node-itet relation.  (for a given node, the node-itet relation
c is a sequence of pairs of nodes that describe triangles that the
c given node belongs to.  if the given node belongs to a quad,
c three virtual triangles exist in this relation, each with weight 1/2.)
 
      ntri=0
      nqud=0
      do i=1,nelements
         if (itettyp(i).eq.ifelmtri) then
            ntri=ntri+1
         elseif (itettyp(i).eq.ifelmqud) then
            nqud=nqud+1
         endif
      enddo
 
      nsimp=12*nqud+3*ntri
 
      call mmgetblk('noditet',isubname,ipnoditet,2*nsimp,1,icscode)
      call mmgetblk('wtnoditet',isubname,ipwtnoditet,nsimp,2,icscode)
      call mmgetblk('noditetoff',isubname,ipnoditetoff,mpno+1,1
     &   ,icscode)
 
      call getnoditet(itet,itetoff,itettyp,mpary,mpno,
     &   nelements,nnodes,noditet,noditetoff,wtnoditet)
 
c... smoothing iterations.
 
      do iter=1,maxiter_sm
         err=0.
 
         do 30 k=1,mpno
            node=mpary(k)
            nnitet=noditetoff(k+1)-noditetoff(k)
            if (ireal1(node).ne.1) goto 30
            if (itp1(node).ne.0) goto 30
 
            xold = xic(node)
            yold = yic(node)
            zold = zic(node)
 
            xnew=0.
            ynew=0.
            znew=0.
            wt=0.
            do j=1,nnitet
               indx=noditetoff(k)+j
               xnew=xnew+wtnoditet(indx)*
     &            (xic(noditet(1,indx))+xic(noditet(2,indx)))
               ynew=ynew+wtnoditet(indx)*
     &            (yic(noditet(1,indx))+yic(noditet(2,indx)))
               znew=znew+wtnoditet(indx)*
     &            (zic(noditet(1,indx))+zic(noditet(2,indx)))
               wt=wt+2.*wtnoditet(indx)
            enddo
            xnew=xnew/wt
            ynew=ynew/wt
            znew=znew/wt
 
            if (ctrl.gt.zero) then
               xic(node)  = xnew*(1.-ctrl)+xsave(k)*ctrl
               yic(node)  = ynew*(1.-ctrl)+ysave(k)*ctrl
               zic(node)  = znew*(1.-ctrl)+zsave(k)*ctrl
            else
               xic(node)  = xnew
               yic(node)  = ynew
               zic(node)  = znew
            endif
 
            err=err+(xic(node)-xold)**2+
     &         (yic(node)-yold)**2+(zic(node)-zold)**2
 30      continue
         err=sqrt(err/mpno)
 
         write(logmess,'(a,i4,a,e14.7)')
     *      'Iteration=',iter,'; root mean square error=',err
         call writloga('default',0,logmess,0,ierrw)
 
         if (iter.eq.1) then
            err1=err
         else
            if (err.le.err1*.01) goto 9000
         endif
 
      enddo
 
c     work out areas of smoothed elements.
 
 9000 continue
 
      areamin=1.d99
      areamax=-1.d99
      areaminp=1.d99
      areamaxp=-1.d99
      do i=1,nelements
         ioff=itetoff(i)
         nod1=itet(1+ioff)
         nod2=itet(2+ioff)
         nod3=itet(3+ioff)
         x1=xic(nod1)
         y1=yic(nod1)
         z1=zic(nod1)
         x2=xic(nod2)
         y2=yic(nod2)
         z2=zic(nod2)
         x3=xic(nod3)
         y3=yic(nod3)
         z3=zic(nod3)
         tni(1)=crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         tni(2)=crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         tni(3)=crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         if (itettyp(i).eq.ifelmqud) then
            nod4=itet(4+ioff)
            x1=xic(nod3)
            y1=yic(nod3)
            z1=zic(nod3)
            x2=xic(nod4)
            y2=yic(nod4)
            z2=zic(nod4)
            x3=xic(nod1)
            y3=yic(nod1)
            z3=zic(nod1)
            tni(1)=tni(1)+crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            tni(2)=tni(2)+crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            tni(3)=tni(3)+crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
         endif
 
         dcross=sqrt(tni(1)**2+tni(2)**2+tni(3)**2)
 
         areamin=min(areamin,dcross*0.5)
         areamax=max(areamax,dcross*0.5)
 
         dcrossp=tni(1)*tn1(1)+tni(2)*tn1(2)+tni(3)*tn1(3)
 
         areaminp=min(areaminp,dcrossp*0.5)
         areamaxp=max(areamaxp,dcrossp*0.5)
 
      enddo
 
 
      write(logmess,'(2(a,e14.7))')
     *   ' min/max area after smoothing=',areamin,' / ',areamax
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(2(a,e14.7))')
     *   ' min/max projected area after smoothing=',
     &   areaminp,' / ',areamaxp
      call writloga('default',0,logmess,0,ierrw)
 
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
 
