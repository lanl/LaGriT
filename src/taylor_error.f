*dk,taylor_error
      subroutine taylor_error(fieldname,badval,iparent)
C     #####################################################################
C
C     PURPOSE -
C
C     ERROR_EST generates an attribute with edges whose H1 seminorm
C     of the error
C     between a function with specified piecewise constant
C     Hessian and the piecewise linear approximation over the
C     tetrahedra is greater than badval.  The attribute contains
C     10* element number + edge number.  The name of the attribute
C     is quality_taylor.  The length of this array is stored in
C     the attribute quality_taylor_len.  The
C     seminorm functional is evaluated over the polyhedron
C     consisting of the union of tetrahedra sharing node NODE.
C     The functional being used
C     here is a generalization to 3D of the 2D analysis
C     of Bank and Smith.)
C
C     INPUT ARGUMENTS -
C
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
C$Log:   /pvcs.config/t3d/src/taylor_error.f_a  $
CPVCS    
CPVCS       Rev 1.2   Fri Jan 22 16:51:28 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.1   Wed Sep 03 11:32:26 1997   dcg
CPVCS    change output format for max error
CPVCS
CPVCS       Rev 1.0   Fri Aug 29 14:05:14 1997   dcg
CPVCS    Initial revision.
C
C     ######################################################################
      implicit none
      include 'consts.h'
      include 'local_element.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      character*132 cbuff
      character* 132 logmess
 
      character*32 fieldname
      real*8 badval
 
      pointer (ipfvecx,fvecx),(ipfvecy,fvecy),(ipfvecz,fvecz)
      pointer (ipwt,wt),(ipldivide,ldivide)
      logical ldivide(lenptr)
      real*8 fvecx(lenptr),fvecy(lenptr),fvecz(lenptr)
      real*8 wt(lenptr)
      pointer (iphxx,hxx),(iphxy,hxy),(iphxz,hxz)
      pointer (iphyy,hyy),(iphyz,hyz)
      pointer (iphzz,hzz)
      real*8 hxx(lenptr),hxy(lenptr),hxz(lenptr),hyy(lenptr)
      real*8 hyz(lenptr),hzz(lenptr)
      real*8 crosx,crosy,crosz
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3
      real*8 afac(3,4),vol6
      real*8 ssq
      real*8 ex,ey,ez
      real*8 edgerr,edgerrid
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)
      pointer (ipreffield,reffield)
      real*8 reffield(lenptr)
      integer iparent(lenptr)
 
      integer i,i1,i2,i3,i4,k,k1
      integer it,ielmtyp,ipar
      integer loc2,loc3,loc4
      integer icscode
      integer nelements,nnodes
      real*8 volwt
      real*8 v1,v1id,h1,h1id
 
      real*8 xmin,xmax,ymin,ymax,zmin,zmax,epsilona,epsilonv
      pointer(ipitetoff,itetoff),(ipitettyp,itettyp),(ipitet,itet)
      integer itetoff(lenptr),itet(lenptr),itettyp(lenptr)
 
      integer badedges(lenptr), numbadedges, quality_taylor(lenptr)
      pointer (ipbadedges,badedges),(ipquality_taylor,quality_taylor)
      real*8 maxerror
 
      integer ier,ilen,ityp
 
      character*32 cmo,isubname
 
c...  Statement functions for the components of the cross product
c...  ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
 
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
      isubname='error_est'
      maxerror = 0.
 
      call cmo_get_name(cmo,ier)
      if (ier .ne. 0) then
         write(logmess,"('No current mesh object')")
         call writloga('default',0,logmess,0,icscode)
         return
      endif
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ier)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,ityp,ier)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ier)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ier)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ier)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ier)
 
      call cmo_get_info(fieldname,cmo,ipreffield,ilen,ityp,ier)
      if (ier .ne. 0) then
         write (logmess,"('Couldnt get field')")
         call writloga('default',0,logmess,0,icscode)
      endif
 
      call mmgetblk('fvecx',isubname,ipfvecx,nnodes,2,ier)
      call mmgetblk('fvecy',isubname,ipfvecy,nnodes,2,ier)
      call mmgetblk('fvecz',isubname,ipfvecz,nnodes,2,ier)
      call mmgetblk('wt',isubname,ipwt,nnodes,2,ier)
      call mmgetblk('ldivide',isubname,ipldivide,nnodes,1,ier)
      call mmgetblk('hxx',isubname,iphxx,nelements,2,ier)
      call mmgetblk('hxy',isubname,iphxy,nelements,2,ier)
      call mmgetblk('hxz',isubname,iphxz,nelements,2,ier)
      call mmgetblk('hyy',isubname,iphyy,nelements,2,ier)
      call mmgetblk('hyz',isubname,iphyz,nelements,2,ier)
      call mmgetblk('hzz',isubname,iphzz,nelements,2,ier)
 
 
      call setsize()
      call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsilona,
     *             epsilonv)
 
      do i=1,nnodes
         fvecx(i)=0.
         fvecy(i)=0.
         fvecz(i)=0.
         wt(i)=0.
      enddo
 
      numbadedges = 0
      call mmgetblk('badedges',isubname,ipbadedges,
     *                        nelements*6,1,ier)
 
c.... Loop over all elements in mesh.  Distribute gradients to
c.... parent nodes weighted by element volumes.
 
      do i=1,nelements
         ielmtyp=itettyp(i)
 
c.... Loop over all four faces of tet and compute area vectors.
 
         do k=1,4
            loc2=ielmface1(1,k,ielmtyp)
            loc3=ielmface1(2,k,ielmtyp)
            loc4=ielmface1(3,k,ielmtyp)
            i2=itet(loc2+itetoff(i))
            i3=itet(loc3+itetoff(i))
            i4=itet(loc4+itetoff(i))
            afac(1,k)=crosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &                yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            afac(2,k)=crosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &                yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            afac(3,k)=crosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &                yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
         enddo
 
c.... Compute the volume of the tet.
 
         i1=itet(1 + itetoff(i))
         i2=itet(2 + itetoff(i))
         vol6=afac(1,1)*(xic(i2)-xic(i1))+
     &        afac(2,1)*(yic(i2)-yic(i1))+afac(3,1)*(zic(i2)-zic(i1))
 
c.... If the volume is bigger than EPSILONV, we compute the gradient of
c.... this tet and distribute this value to the four nodes, weighted
c.... by the volume times the tet weight.
 
         if (vol6.gt.6.*epsilonv) then
            do k=1,4
               ipar=iparent(itet(k + itetoff(i)))
               do k1=1,4
                  i1=itet(k1 + itetoff(i))
                  fvecx(ipar)=fvecx(ipar) - reffield(i1)*afac(1,k1)
                  fvecy(ipar)=fvecy(ipar) - reffield(i1)*afac(2,k1)
                  fvecz(ipar)=fvecz(ipar) - reffield(i1)*afac(3,k1)
               enddo
               wt(ipar)=wt(ipar) + vol6
            enddo
         endif
      enddo
 
      do i=1,nnodes
         ldivide(i)=.false.
      enddo
      do i=1,nnodes
         ipar=iparent(i)
         if (wt(ipar).gt.zero) then
            ldivide(ipar)=.true.
         endif
      enddo
      do i=1,nnodes
         ipar=iparent(i)
         if (ldivide(ipar)) then
            ldivide(ipar)=.false.
            fvecx(ipar)=fvecx(ipar)/wt(ipar)
            fvecy(ipar)=fvecy(ipar)/wt(ipar)
            fvecz(ipar)=fvecz(ipar)/wt(ipar)
         endif
      enddo
 
c.... Loop over elements used in computation and compute Hessians
c.... and regularization strengths for each element.
 
      do it=1,nelements
         ielmtyp=itettyp(it)
         hxx(it)=0.
         hxy(it)=0.
         hxz(it)=0.
         hyy(it)=0.
         hyz(it)=0.
         hzz(it)=0.
         volwt=0.
 
c.... Loop over all four faces and compute the sum of squares
c.... of area vectors.
 
         ssq=0.
         do i=1,4
            loc2=ielmface1(1,i,ielmtyp)
            loc3=ielmface1(2,i,ielmtyp)
            loc4=ielmface1(3,i,ielmtyp)
            i2=itet(loc2 + itetoff(it))
            i3=itet(loc3 + itetoff(it))
            i4=itet(loc4 + itetoff(it))
            afac(1,i)=crosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &           yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            afac(2,i)=crosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &           yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            afac(3,i)=crosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &           yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            ssq=ssq+afac(1,i)**2+afac(2,i)**2+afac(3,i)**2
         enddo
 
c.... Compute the volume of the tet.
 
         i1=itet(1 + itetoff(it))
         i2=itet(2 + itetoff(it))
         vol6=afac(1,1)*(xic(i2)-xic(i1))+
     &        afac(2,1)*(yic(i2)-yic(i1))+afac(3,1)*(zic(i2)-zic(i1))
 
c.... If the volume is bigger than EPSILONV, we compute the Hessian of
c.... this tet.  The Hessian is computed by differencing the
c.... node values of the first derivatives in FVECX, FVECY, and FVECZ.
c.... Since we are computing a weighted sum of Hessians of tetrahedra, we
c.... then multiply by the weights.  The weight is equal to the
c.... volume of the tet.
 
         if (vol6.gt.6.*epsilonv) then
            do i=1,4
               ipar=iparent(itet(i + itetoff(it)))
               hxx(it)=hxx(it) - fvecx(ipar)*afac(1,i)
               hxy(it)=hxy(it) - fvecx(ipar)*afac(2,i)
               hxz(it)=hxz(it) - fvecx(ipar)*afac(3,i)
               hyy(it)=hyy(it) - fvecy(ipar)*afac(2,i)
               hyz(it)=hyz(it) - fvecy(ipar)*afac(3,i)
               hzz(it)=hzz(it) - fvecz(ipar)*afac(3,i)
            enddo
 
            volwt=volwt + vol6
 
         else
 
c.... If the volume of the tet is bad, we don't bother computing
c.... a Hessian for the tet, and we compute a 'geometry factor'
c.... equal to the sum of squares of the area vectors divided by
c.... the epsilon volume.  (If the tet was good, we would divide
c.... by the true volume.)  The idea is to compute a LARGE
c.... (but numerically stable) geometry factor which gives the
c.... algorithm a chance to fix the tet.
 
         endif
 
c.... Now divide through the sum of Hessians by the sum of weights
c.... to get an average Hessian for the element.
 
         hxx(it)=hxx(it)/volwt
         hxy(it)=hxy(it)/volwt
         hxz(it)=hxz(it)/volwt
         hyy(it)=hyy(it)/volwt
         hyz(it)=hyz(it)/volwt
         hzz(it)=hzz(it)/volwt
 
c.... We now compute for the current element the
c.... value of the functional using the true adaption function,
c.... and the value of the functional using the Identity matrix
c.... for a Hessian.  This will allow us to calculate a regularization
c.... strength for the element.  This reg. strength is the ratio between
c.... the two functional values.  The calculation of the functional
c.... over the element is performed by doing a weighted sum of
c.... functional values over the virtual tets in the element.
 
C         do loctet=1,itnumtetv(itettyp(it))
 
c...  Compute the edge errors over the edges {1,...,6}.
 
         edgerr = 0
         edgerrid = 0
         do i = 1,nelmnee(ielmtyp)
            i1 = itet(ielmedge1(1,i,ielmtyp) + itetoff(it))
            i2 = itet(ielmedge1(2,i,ielmtyp) + itetoff(it))
 
            ex=xic(i2)-xic(i1)
            ey=yic(i2)-yic(i1)
            ez=zic(i2)-zic(i1)
            v1=ex*(hxx(it)*ex+2.*hxy(it)*ey+2.*hxz(it)*ez)+
     &           ey*(hyy(it)*ey+2.*hyz(it)*ez)+ez*hzz(it)*ez
            v1id=ex*ex+ey*ey+ez*ez
            edgerr = edgerr + v1**2
            edgerrid = edgerrid + v1id**2
            if (abs(v1) .gt. badval) then
               numbadedges = numbadedges + 1
               badedges(numbadedges) = 10*it + i
            endif
            if (abs(v1) .gt. maxerror) then
               maxerror = abs(v1)
            endif
         enddo
 
C...  Compute the sum of squares of edge errors, and sum of squares of
c...  edge errors if we assume the Hessian is equal to the identity.
 
         h1=h1 + edgerr
         h1id=h1id + edgerrid
C     enddo
 
      enddo
C
C  Write error info
      write(logmess,"('Max abs Taylor error = ',e18.13)") maxerror
      call writloga('default',0,logmess,0,icscode)
 
      cbuff='cmo/addatt//quality_taylor_len/INT/' //
     *     'scalar/scalar/constant//x/0' //
     *     ' ; finish'
      call dotaskx3d(cbuff,ier)
      if (ier .ne. 0) then
         write (logmess,"('Couldnt create length attribute')")
         call writloga('default',0,logmess,0,icscode)
      endif
      call cmo_set_info('quality_taylor_len',cmo,
     *                    numbadedges,1,1,ier)
      if (ier .ne. 0) then
         write (logmess,"('Couldnt set length attribute')")
         call writloga('default',0,logmess,0,icscode)
      endif
 
      if (numbadedges .gt. 0) then
         cbuff='cmo/addatt//quality_taylor/VINT/' //
     *        'scalar/quality_taylor_len/constant//x/0' //
     *        ' ; finish'
         call dotaskx3d(cbuff,ier)
         if (ier .ne. 0) then
            write (logmess,"('Couldnt create attribute')")
            call writloga('default',0,logmess,0,icscode)
         endif
         call mmgetpr('quality_taylor',cmo,ipquality_taylor,ier)
         if (ier .ne. 0) then
            write (logmess,"('Couldnt access attribute')")
            call writloga('default',0,logmess,0,icscode)
         endif
 
         do i=1,numbadedges
            quality_taylor(i) = badedges(i)
         enddo
      endif
 
 9999 continue
 
      call mmrelprt(isubname,icscode)
      return
      end
