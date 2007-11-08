      subroutine hess3d(cmo,action,cfield,ierror)
C     #####################################################################
C
C     PURPOSE -
C
C     HESS3D returns the Hessian of a (1) specified CMO field
C     ("field" option), (2) the Hessian implied by a user-
C     supplied subroutine FADPT ("user" option), or (3) the
C     Hessian implied by user-supplied local edge errors
C     ("erradpt" option).
C
C     INPUT ARGUMENTS -
C         CMO -- current mesh object
C         ACTION -- 'field', 'user', or 'erradpt'
C         CFIELD --- character name of field for 'field' option
C
C     OUTPUT ARGUMENTS -
C         IERROR -- error return
C
C     CHANGE HISTORY -
C     $Log:   /pvcs.config/t3d/src/hess3d.f_a  $
CPVCS    
CPVCS       Rev 1.3   26 Feb 2002 12:05:26   dcg
CPVCS    fix overflow of cbuf character string
CPVCS    
CPVCS       Rev 1.2   24 Jan 2002 13:21:02   dcg
CPVCS    fix bad argument in call to get_global
CPVCS    
CPVCS       Rev 1.1   07 Jan 2002 20:25:48   kuprat
CPVCS    Corrected bug where IELTNO was undefined.
CPVCS    
CPVCS       Rev 1.0   21 Dec 2001 18:07:44   kuprat
CPVCS    Initial revision.
C
C     ######################################################################
      implicit none
 
      include 'consts.h'
      include 'local_element.h'
      include 'smooth.h'
 
      pointer(ipout,out)
      real*8 out(*)
 
      pointer (ipxvec,xvec),(ipyvec,yvec),(ipzvec,zvec),
     &   (ipfvecx,fvecx),(ipfvecy,fvecy),
     &   (ipfvecz,fvecz),(ipwt,wt),(ipldivide,ldivide)
      real*8 xvec(*),yvec(*),zvec(*),fvecx(*),fvecy(*),fvecz(*),wt(*)
      logical ldivide(*)
 
      pointer (ipiedge,iedge), (ipiedgeoff,iedgeoff), (ipiedgemat
     &   ,iedgemat)
      integer iedge(*),iedgeoff(*),iedgemat(*)
 
      pointer (ipeerrg,eerrg),(iphxx,hxx),(iphxy,hxy),(iphxz,hxz),(iphyy
     &   ,hyy),(iphyz,hyz),(iphzz,hzz),(iperrvec,errvec)
      real*8 eerrg(*), hxx(*), hxy(*), hxz(*), hyy(*), hyz(*), hzz(*),
     &   errvec(6,*)
 
      pointer (ipx,x), (ipy,y), (ipz,z), (ipfvec,fvec), (ipreffield
     &   ,reffield)
      real*8 x(*),y(*),z(*),fvec(*),reffield(*)
 
      pointer (ipitet,itet), (ipitetoff,itetoff), (ipieltary,ieltary),
     &   (ipitettyp,itettyp), (ipimt1,imt1),(ipiparent,iparent)
     &   ,(ipitetclr,itetclr),(ipmpary,mpary),(ipitp1,itp1),(ipisn1,isn1
     &   )
      integer itet(*), itetoff(*), ieltary(*),itettyp(*),imt1(*),
     &   iparent(*),itetclr(*),mpary(*),itp1(*),isn1(*)
 
      integer i,i2,i3,i4,loctet,iout,itype,idata,nnodes,icscode,cmol
     &   ,nelements,nedges,ii,ieltno,i1,ie,j,k,ihyb,loc1,loc2,nodmin
     &   ,nodmax,loc3,loc4,ityp,ipar,k1,ilen,ierrw,ierror,mpno,length
     &   ,ierr,locnod,jteti
 
      real*8 hxxloc,hxyloc,hxzloc,hyyloc,hyzloc,hzzloc,volwt,eerrl(6)
     &   ,afac(3,4),ssq,vol6,valmax,valmin,range
      character*32 isubname,action,cout,cfield,blkname,cdata,cmo
      character*132 logmess
      character*256 cbuf
 
      integer icharlnf
      real*8 time
 
      include 'statementfunctions.h'
 
      isubname='hess3d'
 
      ierror = 0
      cmol=icharlnf(cmo)
 
c.... Derive the nodnumtetv and jtetv arrays from the
c.... ihybnumtetv and itetv arrays.  ihybnumtetv and itetv are defined
c.... in data statements.  ihybnumtetv gives, for a given element type,
c.... the number of `virtual tetrahedra' that are equivalent to this
c.... element.  (the number of virtual tets in the `equivalent
c.... simplicial ensemble'.)  itetv gives the ``itet'' relation for
c.... these virtual tets.  nodnumtetv, for each local node number in
c.... a given element type, gives the number of virtual tets that
c.... contain that local node number. jtetv relates local element node
c.... numbers to virtual tet/ local virtual tet node numbers. more
c.... precisely, if the i'th local node in a given element happens
c.... to be the j'th node in the k'th virtual tet, then there is a jtetv
c.... value equal to (k-1)*4+j, corresponding to node i.
 
      do i=ifelmtet,ifelmhex
         do j=1,nelmnen(i)
            nodnumtetv(j,i)=0
         enddo
         do j=1,ihybnumtetv(i)
            do k=1,4
               locnod=itetv(k,j,i)
               jteti=4*(j-1)+k
               nodnumtetv(locnod,i)=nodnumtetv(locnod,i)+1
               jtetv(nodnumtetv(locnod,i),locnod,i)=jteti
            enddo
         enddo
      enddo
 
c.... Add HXX, HXY, etc. to CMO to hold Hessian.
 
      call cmo_get_info('hxx',cmo,iphxx,length,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/hxx',
     &      '/vdouble/scalar/nelements/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
      call cmo_get_info('hxy',cmo,iphxy,length,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/hxy',
     &      '/vdouble/scalar/nelements/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
      call cmo_get_info('hxz',cmo,iphxz,length,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/hxz',
     &      '/vdouble/scalar/nelements/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
      call cmo_get_info('hyy',cmo,iphyy,length,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/hyy',
     &      '/vdouble/scalar/nelements/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
      call cmo_get_info('hyz',cmo,iphyz,length,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/hyz',
     &      '/vdouble/scalar/nelements/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
      call cmo_get_info('hzz',cmo,iphzz,length,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/hzz',
     &      '/vdouble/scalar/nelements/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
      call cmo_get_attinfo('frange',cmo,iout,range,cout,ipout,length
     &   ,itype,ierr)
      if (ierr.ne.0) then
         write(cbuf,*) 'cmo/addatt/',cmo(1:cmol),'/frange',
     &      '/real/scalar/scalar/copy/temporary/gl/0. ; finish'
         call dotask(cbuf,ierr)
      endif
 
c...  Get pointers to mesh object.
 
      call cmo_get_info('hxx',cmo,iphxx,length,itype,ierr)
      call cmo_get_info('hxy',cmo,iphxy,length,itype,ierr)
      call cmo_get_info('hxz',cmo,iphxz,length,itype,ierr)
      call cmo_get_info('hyy',cmo,iphyy,length,itype,ierr)
      call cmo_get_info('hyz',cmo,iphyz,length,itype,ierr)
      call cmo_get_info('hzz',cmo,iphzz,length,itype,ierr)
      call cmo_get_attinfo('frange',cmo,iout,range,cout,ipout,length
     &   ,itype,ierr)
      call cmo_get_intinfo('nnodes',cmo,nnodes,length,itype,ierr)
      call cmo_get_intinfo('nelements',cmo,nelements,length,itype,ierr)
      call cmo_get_info('xic',cmo,ipx,length,itype,ierr)
      call cmo_get_info('yic',cmo,ipy,length,itype,ierr)
      call cmo_get_info('zic',cmo,ipz,length,itype,ierr)
      call cmo_get_info('itet',cmo,ipitet,length,itype,ierr)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,itype,ierr)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,itype,ierr)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,itype,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,length,itype,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,length,itype,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,length,itype,ierr)
 
      ieltno=nelements
 
c     ..................................................................
c     find the parents of each node.
c
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
 
c.... FIELD option.
 
      if (action(1:5).eq.'field') then
         blkname=cfield(1:icharlnf(cfield))
         call cmo_get_info(blkname,cmo,ipreffield,ilen,ityp,icscode)
         if(icscode.ne.0) then
            ierror=icscode
            write(logmess,'(a)')
     *         'HESS3D: bad reference field'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
 
c.... Allocate arrays to hold numerical first derivatives of REFFIELD.
c.... These arrays are set to be NNODES, and conceivably could be
c.... shortened to be the length of ICHILDARY plus first neighbours
c.... of the points in ICHILDARY.
 
         call mmgetblk('fvecx',isubname,ipfvecx,nnodes,2,icscode)
         call mmgetblk('fvecy',isubname,ipfvecy,nnodes,2,icscode)
         call mmgetblk('fvecz',isubname,ipfvecz,nnodes,2,icscode)
 
c.... Allocate arrays that facilitate the construction of weighted sums
c.... of derivatives at the nodes.
 
         call mmgetblk('wt',isubname,ipwt,nnodes,2,icscode)
         call mmgetblk('ldivide',isubname,ipldivide,nnodes,1
     &      ,icscode)
 
 
c.... Compute Hessian on elements based on REFFIELD.
 
c...Sample range of function.
 
         valmax=-1.d99
         valmin=1.d99
         do i=1,nnodes
            valmax=max(valmax,reffield(i))
            valmin=min(valmin,reffield(i))
         enddo
         range=valmax-valmin
 
         call cmo_set_attinfo('frange',cmo,idata,range,cdata,2
     &      ,ierr)
 
         do i=1,nnodes
            fvecx(i)=0.
            fvecy(i)=0.
            fvecz(i)=0.
            wt(i)=0.
         enddo
 
c.... Loop over all elements in mesh.  Distribute gradients to
c.... parent nodes weighted by element volumes.  For nontetrahedral
c.... elements, the gradients are evaluated on virtual tetrahedra
c.... and distributed to parent nodes weighted by volume times
c.... virtual tet weight.
 
         do i=1,nelements
            ityp=itettyp(i)
 
c.... Loop over virtual tets in elements.  If the element IS a tet,
c.... then the virtual tet is the tet itself.
 
            do j=1,ihybnumtetv(ityp)
 
c.... Loop over all four faces of virtual tet and compute
c.... area vectors.
 
               do k=1,4
                  loc2=ielmface1(1,k,ifelmtet)
                  loc3=ielmface1(2,k,ifelmtet)
                  loc4=ielmface1(3,k,ifelmtet)
                  i2=itet(itetv(loc2,j,ityp)+itetoff(i))
                  i3=itet(itetv(loc3,j,ityp)+itetoff(i))
                  i4=itet(itetv(loc4,j,ityp)+itetoff(i))
                  afac(1,k)=dcrosx(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(2,k)=dcrosy(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(3,k)=dcrosz(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
               enddo
 
c.... Compute the volume of the tet.
 
               i1=itet( itetv(1,j,ityp) + itetoff(i))
               i2=itet( itetv(2,j,ityp) + itetoff(i))
               vol6=afac(1,1)*(x(i2)-x(i1))+
     &            afac(2,1)*(y(i2)-y(i1))+afac(3,1)*(z(i2)-z(i1))
 
c.... Compute the gradient of
c.... this tet and distribute this value to the four nodes, weighted
c.... by the volume times the virtual tet weight.
 
               do k=1,4
                  ipar=iparent(itet( itetv(k,j,ityp)
     &               + itetoff(i)))
                  do k1=1,4
                     i1=itet( itetv(k1,j,ityp) + itetoff(i))
                     fvecx(ipar)=fvecx(ipar)-
     &                  wttetv(j,ityp)*reffield(i1)*afac(1,k1)
                     fvecy(ipar)=fvecy(ipar)-
     &                  wttetv(j,ityp)*reffield(i1)*afac(2,k1)
                     fvecz(ipar)=fvecz(ipar)-
     &                  wttetv(j,ityp)*reffield(i1)*afac(3,k1)
                  enddo
                  wt(ipar)=wt(ipar)+wttetv(j,ityp)*vol6
               enddo
            enddo
         enddo
 
         do i=1,nnodes
            ldivide(i)=.true.
         enddo
         do i=1,nnodes
            ipar=iparent(i)
            if (ldivide(ipar)) then
               ldivide(ipar)=.false.
               fvecx(ipar)=fvecx(ipar)/safe(wt(ipar))
               fvecy(ipar)=fvecy(ipar)/safe(wt(ipar))
               fvecz(ipar)=fvecz(ipar)/safe(wt(ipar))
            endif
         enddo
 
c.... Loop over elements used in computation and compute Hessians
c.... and regularization strengths for each element.
 
         do ii=1,ieltno
            ihyb=ii
            ityp=itettyp(ihyb)
            hxx(ii)=0.
            hxy(ii)=0.
            hxz(ii)=0.
            hyy(ii)=0.
            hyz(ii)=0.
            hzz(ii)=0.
            volwt=0.
 
c.... Loop over all virtual tetrahedra in the element.
 
            do loctet=1,ihybnumtetv(itettyp(ihyb))
 
c.... Loop over all four faces and compute the sum of squares
c.... of area vectors.
 
               ssq=0.
               do i=1,4
                  loc2=ielmface1(1,i,ifelmtet)
                  loc3=ielmface1(2,i,ifelmtet)
                  loc4=ielmface1(3,i,ifelmtet)
                  i2=itet(itetv(loc2,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i3=itet(itetv(loc3,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i4=itet(itetv(loc4,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  afac(1,i)=dcrosx(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(2,i)=dcrosy(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(3,i)=dcrosz(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  ssq=ssq+afac(1,i)**2+afac(2,i)**2+afac(3,i)**2
               enddo
 
c.... Compute the volume of the tet.
 
               i1=itet( itetv(1,loctet,itettyp(ihyb)) +
     &            itetoff(ihyb))
               i2=itet( itetv(2,loctet,itettyp(ihyb)) +
     &            itetoff(ihyb))
               vol6=afac(1,1)*(x(i2)-x(i1))+
     &            afac(2,1)*(y(i2)-y(i1))+afac(3,1)*(z(i2)-z(i1))
 
c.... Compute the Hessian of
c.... this tet.  The Hessian is computed by differencing the
c.... node values of the first derivatives in FVECX, FVECY, and FVECZ.
c.... Since we are computing a weighted sum of Hessians of tetrahedra,
c we
c.... then multiply by the weights.  The weight is equal to the
c.... volume of the tet multiplied by the WTTETV value in smooth.h.
 
               do i=1,4
                  ipar=iparent(itet( itetv(i,loctet,ityp)
     &               + itetoff(ihyb)))
                  hxx(ii)=hxx(ii)-wttetv(loctet,ityp)*
     &               fvecx(ipar)*afac(1,i)
                  hxy(ii)=hxy(ii)-wttetv(loctet,ityp)*
     &               fvecx(ipar)*afac(2,i)
                  hxz(ii)=hxz(ii)-wttetv(loctet,ityp)*
     &               fvecx(ipar)*afac(3,i)
                  hyy(ii)=hyy(ii)-wttetv(loctet,ityp)*
     &               fvecy(ipar)*afac(2,i)
                  hyz(ii)=hyz(ii)-wttetv(loctet,ityp)*
     &               fvecy(ipar)*afac(3,i)
                  hzz(ii)=hzz(ii)-wttetv(loctet,ityp)*
     &               fvecz(ipar)*afac(3,i)
               enddo
 
               volwt=volwt+wttetv(loctet,ityp)*vol6
 
            enddo
 
c.... Now divide through the sum of Hessians by the sum of weights
c.... to get an average Hessian for the element.
 
            hxx(ii)=hxx(ii)/safe(volwt)
            hxy(ii)=hxy(ii)/safe(volwt)
            hxz(ii)=hxz(ii)/safe(volwt)
            hyy(ii)=hyy(ii)/safe(volwt)
            hyz(ii)=hyz(ii)/safe(volwt)
            hzz(ii)=hzz(ii)/safe(volwt)
 
         enddo
 
      elseif (action(1:4).eq.'user') then
 
c.... Generate edges in IEDGE, IEDGEOFF which will be used
c.... for user-function calls.
 
         mpno=nnodes
         call mmgetblk('mpary',isubname,ipmpary,mpno,1,icscode)
         do i=1,nnodes
            mpary(i)=i
         enddo
         ieltno=nelements
         call mmgetblk('ieltary',isubname,ipieltary,ieltno,1,icscode)
         do i=1,nelements
            ieltary(i)=i
         enddo
 
         call getiedge(mpno,mpary,ieltno,ieltary,nnodes,itet,itetoff,
     &      itetclr,itettyp,isubname,ipiedge,ipiedgeoff,ipiedgemat)
 
         nedges=iedgeoff(nnodes+1)
 
c.... We allocate the following array which will contain 'edge
c.... errors associated with each edge.
 
         call mmgetblk('eerrg',isubname,ipeerrg,nedges,2,icscode)
 
c.... FVEC will contain user-function values at the nodes.
 
         call mmgetblk('fvec',isubname,ipfvec,nnodes,2,icscode)
 
c.... XVEC, YVEC, ZVEC will contain user-function values at the edges.
 
         call mmgetblk('xvec',isubname,ipxvec,nedges,2,icscode)
         call mmgetblk('yvec',isubname,ipyvec,nedges,2,icscode)
         call mmgetblk('zvec',isubname,ipzvec,nedges,2,icscode)
 
c.... Get the current time, in case the adaption function is
c.... time dependent.
 
         call get_global('time',iout,
     *      time,cout,itype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname
     &      ,'get_info_r')
 
         call fadpt(x,y,z,imt1,nnodes,time,fvec)
 
c...Sample range of function.
 
         valmax=-1.d99
         valmin=1.d99
         do i=1,nnodes
            valmax=max(valmax,fvec(i))
            valmin=min(valmin,fvec(i))
         enddo
         range=valmax-valmin
 
         call cmo_set_attinfo('frange',cmo,idata,range,cdata,2
     &      ,ierr)
 
c.... Sample the function at midpoints of all edges.
 
         do i1=1,nnodes
            do j=iedgeoff(i1)+1,iedgeoff(i1+1)
               i2=iedge(j)
               xvec(j)=0.5*(x(i1)+x(i2))
               yvec(j)=0.5*(y(i1)+y(i2))
               zvec(j)=0.5*(z(i1)+z(i2))
            enddo
         enddo
         call fadpt(xvec,yvec,zvec,iedgemat,nedges,time,eerrg)
 
c.... Compute edge errors.
 
         do i1=1,nnodes
            do j=iedgeoff(i1)+1,iedgeoff(i1+1)
               i2=iedge(j)
               eerrg(j)=eerrg(j)-
     &            0.5*(fvec(i1)+fvec(i2))
            enddo
         enddo
 
c.... Loop over all elements in polyhedron.  For each element
c.... compute the Hessian, which will be a weighted average of the
c.... Hessians computed for each virtual tetrahedron in the element.
 
         do ii=1,ieltno
            ihyb=ii
            hxx(ii)=0.
            hxy(ii)=0.
            hxz(ii)=0.
            hyy(ii)=0.
            hyz(ii)=0.
            hzz(ii)=0.
            volwt=0.
 
c.... Loop over all virtual tetrahedra in the element.
 
            do loctet=1,ihybnumtetv(itettyp(ihyb))
 
 
c.... For each tet, loop over all six edges and obtain
c.... the six local edge errors for the global array of
c.... edge errors.
 
               do 98 i=1,6
                  loc1=ielmedge1(1,i,ifelmtet)
                  loc2=ielmedge1(2,i,ifelmtet)
                  i1=itet(itetv(loc1,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i2=itet(itetv(loc2,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  nodmin=min(i1,i2)
                  nodmax=max(i1,i2)
                  do ie=iedgeoff(nodmin)+1,iedgeoff(nodmin+1)
                     if ((iedge(ie).eq.nodmax).and.(iedgemat(ie
     &                  ).eq.itetclr(ihyb))) then
                        eerrl(i)=eerrg(ie)
                        goto 98
                     endif
                  enddo
                  print*
     &               ,'POLYFUN ERROR: GLOBAL/LOCAL EDGE MATCHING'
                  stop
 98            continue
 
 
c.... Loop over all four faces and compute the sum of squares
c.... of area vectors.
 
               ssq=0.
               do i=1,4
                  loc2=ielmface1(1,i,ifelmtet)
                  loc3=ielmface1(2,i,ifelmtet)
                  loc4=ielmface1(3,i,ifelmtet)
                  i2=itet(itetv(loc2,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i3=itet(itetv(loc3,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i4=itet(itetv(loc4,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  afac(1,i)=dcrosx(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(2,i)=dcrosy(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(3,i)=dcrosz(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  ssq=ssq+afac(1,i)**2+afac(2,i)**2+afac(3,i)**2
               enddo
 
c.... Compute the volume of the tet.
 
               i1=itet( itetv(1,loctet,itettyp(ihyb)) +
     &            itetoff(ihyb))
               i2=itet( itetv(2,loctet,itettyp(ihyb)) +
     &            itetoff(ihyb))
               vol6=afac(1,1)*(x(i2)-x(i1))+
     &            afac(2,1)*(y(i2)-y(i1))+afac(3,1)*(z(i2)-z(i1))
 
c.... The Hessian is computed by summing contributions
c.... coming from each edge.  Each edge's contribution involves the
c.... product of the edge error for that edge with the two area
c.... vectors of the faces which DO NOT CONTAIN the edge.  This is
c.... then divided by the volume of the tet squared.  However, since
c.... we are computing a weighted sum of Hessians of tetrahedra, we
c.... then multiply by the weights.  The weight is equal to the
c.... volume of the tet (thus canceling one factor of volume in the
c.... denominator) multiplied by the WTTETV value in smooth.h.
 
               hxxloc=0.
               hxyloc=0.
               hxzloc=0.
               hyyloc=0.
               hyzloc=0.
               hzzloc=0.
               do i=1,6
                  i1=ielmedge1(1,i,ifelmtet)
                  i2=ielmedge1(2,i,ifelmtet)
                  hxxloc=hxxloc+eerrl(i)*
     &               2.*afac(1,i1)*afac(1,i2)
                  hxyloc=hxyloc+eerrl(i)*
     &               (afac(1,i1)*afac(2,i2)+afac(2,i1)*afac(1
     &               ,i2))
                  hxzloc=hxzloc+eerrl(i)*
     &               (afac(1,i1)*afac(3,i2)+afac(3,i1)*afac(1
     &               ,i2))
                  hyyloc=hyyloc+eerrl(i)*
     &               2.*afac(2,i1)*afac(2,i2)
                  hyzloc=hyzloc+eerrl(i)*
     &               (afac(2,i1)*afac(3,i2)+afac(3,i1)*afac(2
     &               ,i2))
                  hzzloc=hzzloc+eerrl(i)*
     &               2.*afac(3,i1)*afac(3,i2)
               enddo
 
               hxx(ii)=hxx(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hxxloc/safe(vol6)
               hxy(ii)=hxy(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hxyloc/safe(vol6)
               hxz(ii)=hxz(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hxzloc/safe(vol6)
               hyy(ii)=hyy(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hyyloc/safe(vol6)
               hyz(ii)=hyz(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hyzloc/safe(vol6)
               hzz(ii)=hzz(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hzzloc/safe(vol6)
 
               volwt=volwt+wttetv(loctet,itettyp(ihyb))*vol6
 
            enddo
 
c.... Now divide through the sum of Hessians by the sum of weights
c.... to get an average Hessian for the element.
 
            hxx(ii)=hxx(ii)/safe(volwt)
            hxy(ii)=hxy(ii)/safe(volwt)
            hxz(ii)=hxz(ii)/safe(volwt)
            hyy(ii)=hyy(ii)/safe(volwt)
            hyz(ii)=hyz(ii)/safe(volwt)
            hzz(ii)=hzz(ii)/safe(volwt)
 
         enddo
 
      elseif (action(1:7).eq.'erradpt') then
 
         call mmgetblk('errvec',isubname,iperrvec,6*ieltno,2
     &      ,icscode)
 
c... Get edge errors from user.
 
         ieltno=nelements
         call mmgetblk('ieltary',isubname,ipieltary,ieltno,1,icscode)
         do i=1,nelements
            ieltary(i)=i
         enddo
c... Get edge errors from user.
 
         do i=1,ieltno
            ihyb=ieltary(i)
            if (itettyp(ihyb).ne.ifelmtet) then
               print*,
     &            'ERROR:  ERRADPT in 3D requires all tet mesh.'
               ierror=1
               goto 9999
            endif
         enddo
         call erradpt(ieltary,ieltno,range,errvec)
 
         call cmo_set_attinfo('frange',cmo,idata,range,cdata,2
     &      ,ierr)
 
c.... Loop over all elements in polyhedron.  For each element
c.... compute the Hessian, which will be a weighted average of the
c.... Hessians computed for each virtual tetrahedron in the element.
 
         do ii=1,ieltno
            ihyb=ii
            hxx(ii)=0.
            hxy(ii)=0.
            hxz(ii)=0.
            hyy(ii)=0.
            hyz(ii)=0.
            hzz(ii)=0.
            volwt=0.
 
c.... Loop over all virtual tetrahedra in the element.
 
            do loctet=1,ihybnumtetv(itettyp(ihyb))
 
               do i=1,6
                  eerrl(i)=errvec(i,ii)
               enddo
 
c.... Loop over all four faces and compute the sum of squares
c.... of area vectors.
 
               ssq=0.
               do i=1,4
                  loc2=ielmface1(1,i,ifelmtet)
                  loc3=ielmface1(2,i,ifelmtet)
                  loc4=ielmface1(3,i,ifelmtet)
                  i2=itet(itetv(loc2,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i3=itet(itetv(loc3,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  i4=itet(itetv(loc4,loctet,itettyp(ihyb))
     &               +itetoff(ihyb))
                  afac(1,i)=dcrosx(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(2,i)=dcrosy(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  afac(3,i)=dcrosz(x(i2),y(i2),z(i2),x(i3),
     &               y(i3),z(i3),x(i4),y(i4),z(i4))
                  ssq=ssq+afac(1,i)**2+afac(2,i)**2+afac(3,i)**2
               enddo
 
c.... Compute the volume of the tet.
 
               i1=itet( itetv(1,loctet,itettyp(ihyb)) +
     &            itetoff(ihyb))
               i2=itet( itetv(2,loctet,itettyp(ihyb)) +
     &            itetoff(ihyb))
               vol6=afac(1,1)*(x(i2)-x(i1))+
     &            afac(2,1)*(y(i2)-y(i1))+afac(3,1)*(z(i2)-z(i1))
 
c.... The Hessian is computed by summing contributions
c.... coming from each edge.  Each edge's contribution involves the
c.... product of the edge error for that edge with the two area
c.... vectors of the faces which DO NOT CONTAIN the edge.  This is
c.... then divided by the volume of the tet squared.  However, since
c.... we are computing a weighted sum of Hessians of tetrahedra, we
c.... then multiply by the weights.  The weight is equal to the
c.... volume of the tet (thus canceling one factor of volume in the
c.... denominator) multiplied by the WTTETV value in smooth.h.
 
               hxxloc=0.
               hxyloc=0.
               hxzloc=0.
               hyyloc=0.
               hyzloc=0.
               hzzloc=0.
               do i=1,6
                  i1=ielmedge1(1,i,ifelmtet)
                  i2=ielmedge1(2,i,ifelmtet)
                  hxxloc=hxxloc+eerrl(i)*
     &               2.*afac(1,i1)*afac(1,i2)
                  hxyloc=hxyloc+eerrl(i)*
     &               (afac(1,i1)*afac(2,i2)+afac(2,i1)*afac(1
     &               ,i2))
                  hxzloc=hxzloc+eerrl(i)*
     &               (afac(1,i1)*afac(3,i2)+afac(3,i1)*afac(1
     &               ,i2))
                  hyyloc=hyyloc+eerrl(i)*
     &               2.*afac(2,i1)*afac(2,i2)
                  hyzloc=hyzloc+eerrl(i)*
     &               (afac(2,i1)*afac(3,i2)+afac(3,i1)*afac(2
     &               ,i2))
                  hzzloc=hzzloc+eerrl(i)*
     &               2.*afac(3,i1)*afac(3,i2)
               enddo
 
               hxx(ii)=hxx(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hxxloc/safe(vol6)
               hxy(ii)=hxy(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hxyloc/safe(vol6)
               hxz(ii)=hxz(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hxzloc/safe(vol6)
               hyy(ii)=hyy(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hyyloc/safe(vol6)
               hyz(ii)=hyz(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hyzloc/safe(vol6)
               hzz(ii)=hzz(ii)+wttetv(loctet,itettyp(ihyb))*4.
     &            *hzzloc/safe(vol6)
 
               volwt=volwt+wttetv(loctet,itettyp(ihyb))*vol6
 
            enddo
 
c.... Now divide through the sum of Hessians by the sum of weights
c.... to get an average Hessian for the element.
 
            hxx(ii)=hxx(ii)/safe(volwt)
            hxy(ii)=hxy(ii)/safe(volwt)
            hxz(ii)=hxz(ii)/safe(volwt)
            hyy(ii)=hyy(ii)/safe(volwt)
            hyz(ii)=hyz(ii)/safe(volwt)
            hzz(ii)=hzz(ii)/safe(volwt)
 
         enddo
 
      endif
 
c.... Release temp arrays
 
 9999 continue
 
      call mmrelprt(isubname,icscode)
 
      return
      end
