      subroutine mega2d(cmo,mpary,mpno,ctrl,igeom,ierror)
c
c #####################################################################
c
c     purpose -
c
c     mega2d smooths 2d mesh objects using
c     minimum error gradient adaption
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
c        $Log:   /pvcs.config/t3d/src/mega2d_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   30 Sep 2004 11:20:44   dcg
CPVCS    make alf,tolx double precision
CPVCS    
CPVCS       Rev 1.0   08 Feb 2000 09:51:52   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   31 Jan 2000 17:13:30   kuprat
CPVCS    Initial revision.
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer mpary(lenptr)
 
      character*132 logmess
 
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
 
      integer itp1(lenptr)
      integer isn1(lenptr)
      real*8 xic(lenptr)
      real*8 yic(lenptr)
      real*8 zic(lenptr)
      integer itet(lenptr)
      integer itetoff(lenptr)
      integer itettyp(lenptr)
 
      pointer (ipu,u), (ipv,v)
      pointer (ipxsave,xsave), (ipysave,ysave), (ipzsave,zsave)
      real*8 u(0:lenptr), v(0:lenptr),
     &   xsave(lenptr), ysave(lenptr), zsave(lenptr)
 
      pointer (ipnoditet,noditet), (ipnoditetoff,noditetoff),
     &   (ipwtnoditet,wtnoditet), (ipvoff,voff),
     &   (ipvcurr,vcurr)
      pointer (ipireal1,ireal1)
      integer noditet(2,lenptr),noditetoff(lenptr),
     &   ireal1(lenptr)
      real*8 wtnoditet(lenptr), voff(lenptr), vcurr(lenptr)
 
      real*8 tn1(3),eu1(3),ev1(3),tni(3),ctrl,x1,y1,z1,
     &   x2,y2,z2,x3,y3,z3,dcross,areamin,areamax,epsilonl,
     &   tolconv_sm,area(3),err,f,dfu,dfv,d2fuu,d2fuv,d2fvv,
     &   e1u,e1v,e2u,e2v,e3u,e3v,s1,s2,s3,sshare,a,b,dau,dav,
     &   dbu,dbv,qu,qv,dftu,dftv,d2auu,d2avv,d2auv,ruu,ruv,rvv,
     &   suu,suv,svv,d2ftuu,d2ftuv,d2ftvv,det,du,dv,xold,yold,
     &   zold,slope,alamin,alam,dunew,dvnew,unew,vnew,f1,tmplam,
     &   rhs1,rhs2,f2,fold2,alam2,a1,disc,xnew,ynew,znew,err1,
     &   areatinv,dareatu,dareatv,ft,dcrossp,areaminp,areamaxp,
     &   denom,wt,sqt,dsqtu,dsqtv,d2sqtuu,d2sqtuv,d2sqtvv
 
      integer mpno,ierror,nnodes,length,icmotype,nelements,
     &   mbndry,ilen,ityp,icscode,nod1,nod2,nod3,nod4,ierrw,
     &   i,ioff,maxiter_sm,itypconv_sm,maxdeg,iout,
     &   k,iouter,iter,node,nnitet,ierrdum,indx,i1,i2,
     &   ineghess,izeromov,ntri,nqud,nsimp,j,j1,j2,ii,igeom
 
      logical  negvol
 
      integer innerit
      parameter (innerit=4)
 
      real*8 epsilona
      pointer (ipout,out)
      real*8 alf,tolx,rout,out(*)
      parameter (alf=1.d-4,tolx=1.d-7)
 
      character*32 cmo,cout
      character*32 isubname
 
 
      real*8 x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_,crosx,crosy,crosz
      real*8 f_,fi,fj,fij,dfto2,hfto2
 
c statement functions for the components of the cross product
c ((x2,y2,z2)-(x1,y1,z1)) x ((x3,y3,z3)-(x1,y1,z1)) .
      crosx(x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_)=(y2_-y1_)*(z3_-z1_)
     &   -(z2_-z1_)*(y3_-y1_)
      crosy(x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_)=(z2_-z1_)*(x3_-x1_)
     &   -(x2_-x1_)*(z3_-z1_)
      crosz(x1_,y1_,z1_,x2_,y2_,z2_,x3_,y3_,z3_)=(x2_-x1_)*(y3_-y1_)
     &   -(y2_-y1_)*(x3_-x1_)
 
c...  d(f**2)/dxi in terms of f, df/dxi.
 
      dfto2(f_,fi)=2.d0*f_*fi
 
c...  d2(f**2)/(dxi*dxj) in terms of f, df/dxi, df/dxj, d2f/(dxi*dxj).
 
      hfto2(f_,fi,fj,fij)=2.d0*(fi*fj+f_*fij)
 
      isubname = 'mega2d'
 
      ierror=0
 
c  get info from mesh object to be smoothed
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
     *   mbndry,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierror)
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
         ierror=1
         write(logmess,'(a)')
     *      'mega2d: Cannot get a single unit normal for mesh.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
 
      tn1(1)=tn1(1)/dcross
      tn1(2)=tn1(2)/dcross
      tn1(3)=tn1(3)/dcross
 
      call finishbasis(tn1,eu1,ev1)
 
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
 
c mega smoothing without adaption to function.
 
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
 
c u and v will contain, for a node node, the u-v coordinates for the
c node and for all its 'itet' neighbours.  the 0'th element of the
c u,v arrays corresponds to node, while the other elements
c correspond to the 'itet' neighbours of node.
c hence we need to dimension u,v to have length equal to one plus twice
c the
c maximum number of triangle neighbours that any node has in the grid.
 
      maxdeg=0
      do k=1,mpno
         maxdeg=max(maxdeg,noditetoff(k+1)-noditetoff(k))
      enddo
      length=2*maxdeg+1
      call mmgetblk('u',isubname,ipu,length,2,icscode)
      call mmgetblk('v',isubname,ipv,length,2,icscode)
 
      call mmgetblk('vcurr',isubname,ipvcurr,maxdeg,2,icscode)
 
      length=noditetoff(mpno+1)-noditetoff(1)
      call mmgetblk('voff',isubname,ipvoff,length,2,icscode)
 
c... smoothing iterations.
 
      do iouter=1,1+(maxiter_sm-1)/innerit
         do k=1,mpno
            nod1=mpary(k)
            x1=xic(nod1)
            y1=yic(nod1)
            z1=zic(nod1)
            do i=noditetoff(k)+1,noditetoff(k+1)
               nod2=noditet(1,i)
               nod3=noditet(2,i)
               x2=xic(nod2)
               y2=yic(nod2)
               z2=zic(nod2)
               x3=xic(nod3)
               y3=yic(nod3)
               z3=zic(nod3)
               area(1)=crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)
               area(2)=crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)
               area(3)=crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)
               voff(i)=area(1)*tn1(1)+area(2)*tn1(2)+area(3)*tn1(3
     &            )
               if (voff(i).gt.2.*epsilona) then
                  voff(i)=0.d0
               else
                  voff(i)=voff(i)-2.*epsilona
               endif
            enddo
         enddo
 
         do iter=1+innerit*(iouter-1),min(innerit*iouter,maxiter_sm)
            err=0.
            ineghess=0
            izeromov=0
 
            do 30 k=1,mpno
               node=mpary(k)
               nnitet=noditetoff(k+1)-noditetoff(k)
               if (ireal1(node).ne.1) goto 30
               if (itp1(node).ne.0) goto 30
 
               xold = xic(node)
               yold = yic(node)
               zold = zic(node)
 
               call getuv(node,noditet(1,noditetoff(k)+1),2*nnitet
     &            ,xic,yic,zic,eu1,ev1,.true.,u,v)
 
c.... Special logic for untangling.  If there is a negative volume
c.... triangle, see if movement to one of the neighbouring nodes is
c.... possible without producing areas too close to the 'offset' areas
c VOFF.
c.... (This is only possible if all the triangle neighbours of this node
c.... have VOFF < 0.  The movement to the node zeros the areas of those
c.... triangles, leaving us hopefully in a position to produce positive
c.... areas in the functional minimization step.)
 
               negvol=.false.
               do i=1,nnitet
                  i1=2*i-1
                  i2=2*i
 
                  e1u=u(i1)-u(0)
                  e1v=v(i1)-v(0)
                  e2u=u(i2)-u(0)
                  e2v=v(i2)-v(0)
 
                  vcurr(i)=e1u*e2v-e1v*e2u
 
                  if (vcurr(i).lt.-2.*epsilona) negvol=.true.
               enddo
               if (negvol) then
                  do j1=1,2
                     do 20 j2=1,nnitet
                        ii=2*(j2-1)+j1
                        if (abs(u(0)-u(ii)).lt.epsilonl
     &                     .and.abs(v(0)-v(ii)).lt.epsilonl) goto 20
                        do j=1,nnitet
                           indx=noditetoff(k)+j
                           i1=2*j-1
                           i2=2*j
 
                           e1u=u(i1)-u(ii)
                           e1v=v(i1)-v(ii)
                           e2u=u(i2)-u(ii)
                           e2v=v(i2)-v(ii)
 
c$$$                           e1u=u(i1)-(1.01*u(ii)-.01*u(0))
c$$$                           e1v=v(i1)-(1.01*v(ii)-.01*v(0))
c$$$                           e2u=u(i2)-(1.01*u(ii)-.01*u(0))
c$$$                           e2v=v(i2)-(1.01*v(ii)-.01*v(0))
 
                           vcurr(j)=e1u*e2v-e1v*e2u
                           if (vcurr(j)-voff(indx).lt.0.2*epsilona) goto
     &                        20
                        enddo
 
c$$$                        u(0)=1.01*u(ii)-.01*u(0)
c$$$                        v(0)=1.01*v(ii)-.01*v(0)
                        u(0)=u(ii)
                        v(0)=v(ii)
c$$$                        xic(node)=
c$$$     &                     1.01*xic(noditet(j1,noditetoff(k)+j2))-
c$$$     &                     .01*xic(node)
c$$$                        yic(node)=
c$$$     &                     1.01*yic(noditet(j1,noditetoff(k)+j2))-
c$$$     &                     .01*yic(node)
c$$$                        zic(node)=
c$$$     &                     1.01*zic(noditet(j1,noditetoff(k)+j2))-
c$$$     &                     .01*zic(node)
                        xic(node)=xic(noditet(j1,noditetoff(k)+j2))
                        yic(node)=yic(noditet(j1,noditetoff(k)+j2))
                        zic(node)=zic(noditet(j1,noditetoff(k)+j2))
 
                        do j=1,nnitet
                           indx=noditetoff(k)+j
                           if (vcurr(j).gt.2.*epsilona) then
                              voff(indx)=0.
                           else
                              voff(indx)=vcurr(j)-2.*epsilona
                           endif
                        enddo
                        goto 40
 20                  continue
                  enddo
               endif
               negvol=.false.
               do i=1,nnitet
                  i1=2*i-1
                  i2=2*i
 
                  e1u=u(i1)-u(0)
                  e1v=v(i1)-v(0)
                  e2u=u(i2)-u(0)
                  e2v=v(i2)-v(0)
 
                  vcurr(i)=e1u*e2v-e1v*e2u
 
                  if (vcurr(i).lt.2.*epsilona) negvol=.true.
               enddo
               if (negvol) then
                  unew=0.
                  vnew=0.
                  wt=0.
                  do j=1,nnitet
                     indx=noditetoff(k)+j
                     unew=unew+wtnoditet(indx)*(u(2*j-1)+u(2*j))
                     vnew=vnew+wtnoditet(indx)*(v(2*j-1)+v(2*j))
                     wt=wt+2.*wtnoditet(indx)
                  enddo
                  unew=unew/wt
                  vnew=vnew/wt
                  do j=1,nnitet
                     i1=2*j-1
                     i2=2*j
 
                     e1u=u(i1)-unew
                     e1v=v(i1)-vnew
                     e2u=u(i2)-unew
                     e2v=v(i2)-vnew
 
                     vcurr(j)=e1u*e2v-e1v*e2u
                     if (vcurr(j)-voff(indx).lt.0.2*epsilona) goto 40
                  enddo
 
                  xic(node)=xic(node)+(unew-u(0))*eu1(1)+(vnew-v(0))
     &               *ev1(1)
                  yic(node)=yic(node)+(unew-u(0))*eu1(2)+(vnew-v(0))
     &               *ev1(2)
                  zic(node)=zic(node)+(unew-u(0))*eu1(3)+(vnew-v(0))
     &               *ev1(3)
                  u(0)=unew
                  v(0)=vnew
 
                  do j=1,nnitet
                     indx=noditetoff(k)+j
                     if (vcurr(j).gt.2.*epsilona) then
                        voff(indx)=0.
                     else
                        voff(indx)=vcurr(j)-2.*epsilona
                     endif
                  enddo
               endif
 
 40            continue
 
               f=0.
               dfu=0.
               dfv=0.
               d2fuu=0.
               d2fuv=0.
               d2fvv=0.
               do i=1,nnitet
 
                  indx=noditetoff(k)+i
 
                  i1=2*i-1
                  i2=2*i
 
                  e1u=u(i1)-u(0)
                  e1v=v(i1)-v(0)
                  e2u=u(i2)-u(0)
                  e2v=v(i2)-v(0)
                  e3u=u(i2)-u(i1)
                  e3v=v(i2)-v(i1)
 
                  if (igeom.eq.1) then
                     s1=e1u**2+e1v**2
                     s2=e2u**2+e2v**2
                     s3=e3u**2+e3v**2
                     b=s1+s2+s3
                     areatinv=2./((e1u*e2v-e1v*e2u)-voff(indx))
                     dareatu=-0.5*e3v
                     dareatv=0.5*e3u
                     ft=b*areatinv
                     sqt=ft**2
                     f=f+sqt*wtnoditet(indx)
                     dbu=-2.*(e1u+e2u)
                     dbv=-2.*(e1v+e2v)
                     dftu=(dbu-b*dareatu*areatinv)*areatinv
                     dftv=(dbv-b*dareatv*areatinv)*areatinv
                     dsqtu=dfto2(ft,dftu)
                     dsqtv=dfto2(ft,dftv)
                     dfu=dfu+dsqtu*wtnoditet(indx)
                     dfv=dfv+dsqtv*wtnoditet(indx)
                     d2ftuu=((b*dareatu*areatinv-dbu)*2.*dareatu
     &                  *areatinv+4.)*areatinv
                     d2ftuv=(2.*b*dareatu*dareatv*areatinv-
     &                  dbu*dareatv-dbv*dareatu)*areatinv**2
                     d2ftvv=((b*dareatv*areatinv-dbv)*2.*dareatv
     &                  *areatinv+4.)*areatinv
                     d2sqtuu=hfto2(ft,dftu,dftu,d2ftuu)
                     d2sqtuv=hfto2(ft,dftu,dftv,d2ftuv)
                     d2sqtvv=hfto2(ft,dftv,dftv,d2ftvv)
                     d2fuu=d2fuu+d2sqtuu*wtnoditet(indx)
                     d2fuv=d2fuv+d2sqtuv*wtnoditet(indx)
                     d2fvv=d2fvv+d2sqtvv*wtnoditet(indx)
                  else
                     s1=e1u**2+e1v**2
                     s2=e2u**2+e2v**2
                     s3=e3u**2+e3v**2
                     sshare=s1+s2
                     b=sshare+s3
                     a=s1**2+s2**2+s3**2
c$$$                  areatinv=2./(e1u*e2v-e1v*e2u)
                     areatinv=2./((e1u*e2v-e1v*e2u)-voff(indx))
                     dareatu=-0.5*e3v
                     dareatv=0.5*e3u
                     ft=a*b*areatinv
                     f=f+ft*wtnoditet(indx)
                     dau=-4.*(s1*e1u+s2*e2u)
                     dav=-4.*(s1*e1v+s2*e2v)
                     dbu=-2.*(e1u+e2u)
                     dbv=-2.*(e1v+e2v)
                     qu=dau*b+dbu*a
                     qv=dav*b+dbv*a
                     dftu=(qu-a*b*dareatu*areatinv)*areatinv
                     dftv=(qv-a*b*dareatv*areatinv)*areatinv
                     dfu=dfu+dftu*wtnoditet(indx)
                     dfv=dfv+dftv*wtnoditet(indx)
                     d2auu=8.*(e1u**2+e2u**2)+4.*sshare
                     d2avv=8.*(e1v**2+e2v**2)+4.*sshare
                     d2auv=8.*(e1u*e1v+e2u*e2v)
                     ruu=2.*dau*dbu
                     ruv=dav*dbu+dau*dbv
                     rvv=2.*dav*dbv
                     suu=2.*qu*dareatu
                     suv=qv*dareatu+qu*dareatv
                     svv=2.*qv*dareatv
                     d2ftuu=((2.*a*b*dareatu**2*areatinv-suu)
     &                  *areatinv+d2auu*b+ruu+4.*a)*areatinv
                     d2ftuv=((2.*a*b*dareatu*dareatv*areatinv-suv)*
     &                  areatinv+d2auv*b+ruv)*areatinv
                     d2ftvv=((2.*a*b*dareatv**2*areatinv-svv)
     &                  *areatinv+d2avv*b+rvv+4.*a)*areatinv
                     d2fuu=d2fuu+d2ftuu*wtnoditet(indx)
                     d2fuv=d2fuv+d2ftuv*wtnoditet(indx)
                     d2fvv=d2fvv+d2ftvv*wtnoditet(indx)
                  endif
               enddo
               det=d2fuu*d2fvv-d2fuv**2
               du=(d2fuv*dfv-dfu*d2fvv)/det
               dv=(d2fuv*dfu-dfv*d2fuu)/det
 
               slope=dfu*du+dfv*dv
               if (slope.gt.0.) then
                  ineghess=ineghess+1
                  du=-du
                  dv=-dv
                  slope=-slope
               endif
 
               alamin=tolx
               alam=1.
 
               do while (.true.)
                  dunew=alam*du
                  dvnew=alam*dv
 
                  unew=u(0)+dunew
                  vnew=v(0)+dvnew
 
 
                  f1=0.
                  do i=1,nnitet
 
                     indx=noditetoff(k)+i
 
                     i1=2*i-1
                     i2=2*i
 
                     e1u=u(i1)-unew
                     e1v=v(i1)-vnew
                     e2u=u(i2)-unew
                     e2v=v(i2)-vnew
                     e3u=u(i2)-u(i1)
                     e3v=v(i2)-v(i1)
 
                     s1=e1u**2+e1v**2
                     s2=e2u**2+e2v**2
                     s3=e3u**2+e3v**2
                     b=s1+s2+s3
                     denom=(e1u*e2v-e1v*e2u)-voff(indx)
                     if (denom.lt.0.2*epsilona) then
                        ft=1.d99
                     else
                        areatinv=2./denom
                        if (igeom.eq.1) then
                           ft=b*areatinv
                           ft=ft**2
                        else
                           a=s1**2+s2**2+s3**2
                           ft=a*b*areatinv
                        endif
                     endif
                     f1=f1+ft*wtnoditet(indx)
                  enddo
 
                  if(f1.le.f+alf*alam*slope)then
                     du=dunew
                     dv=dvnew
                     goto 200
                  elseif(alam.lt.alamin)then
                     du=0.
                     dv=0.
                     izeromov=izeromov+1
                     goto 200
                  else
                     if(alam.eq.1.)then
                        tmplam=-slope/(2.*(f1-f-slope))
                        if(tmplam.gt..75)tmplam=.75
                     else
                        rhs1=f1-f-alam*slope
                        rhs2=f2-fold2-alam2*slope
                        a1=(rhs1/alam**2-rhs2/alam2**2)/(alam
     &                     -alam2)
                        b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)
     &                     /(alam-alam2)
                        if(a1.eq.0.)then
                           tmplam=-slope/(2.*b)
                        else
                           disc=b*b-3.*a1*slope
                           tmplam=(-b+sqrt(max(zero,disc)))/(3.*a1
     &                        )
                        endif
                        if(tmplam.gt..5*alam)tmplam=.5*alam
                     endif
                  endif
                  alam2=alam
                  f2=f1
                  fold2=f
                  alam=max(tmplam,.1*alam)
               enddo
 
 200           continue
 
               xnew=xic(node)+du*eu1(1)+dv*ev1(1)
               ynew=yic(node)+du*eu1(2)+dv*ev1(2)
               znew=zic(node)+du*eu1(3)+dv*ev1(3)
 
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
     &            (yic(node)-yold)**2+(zic(node)-zold)**2
 30         continue
            err=sqrt(err/mpno)
 
            write(logmess,'(a,i4,a,e14.7)')
     *         'Iteration=',iter,'; root mean square error=',err
            call writloga('default',0,logmess,0,ierrw)
 
            write(logmess,'(6(i4,a))')
     &         ineghess,' neg Hess/',izeromov
     &         ,' zero move/'
            call writloga('default',0,logmess,0,ierrw)
 
            if (iter.eq.1) then
               err1=err
            else
               if (err.le.err1*.01) goto 9000
            endif
 
         enddo
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
 
