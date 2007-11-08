      subroutine mega3d(cmo,mpary,mpno,ctrl,
     *   action,cfield,climit1,climit2,climit3,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     MEGA3D smooths or performs r-adaption on 3D mesh objects using
C     Minimum Error Gradient Adaption.
C
C     INPUT ARGUMENTS -
C
C         cmo - name of current mesh object
C         mpary - array of nodes to be smoothed
C         mpno - length of mpary
C         ctrl - control parameter for controlled smoothing
C         action - type of smoothing/adaption to be performed
C         cfield - block name of cmo reference field for the
C                  case of r-adaption on a field
C         climit1, -  point index boundaries
C         climit2, climit3
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/mega3d_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.22   22 Sep 2003 12:53:48   dcg
CPVCS    use ierror only for major errors
CPVCS    
CPVCS       Rev 1.21   19 Nov 2002 14:32:50   dcg
CPVCS    'fix geom option to skip calls having to do with calculating and using hessian'
CPVCS    
CPVCS       Rev 1.20   21 Mar 2002 14:12:30   dcg
CPVCS    skip call to sethessian if in geometry mode
CPVCS    
CPVCS       Rev 1.19   26 Feb 2002 12:06:58   dcg
CPVCS    pass ctrl and range variables correctly
CPVCS    
CPVCS       Rev 1.18   05 Feb 2002 10:36:40   dcg
CPVCS    remove inner loop and make it a separate subroutine
CPVCS    
CPVCS       Rev 1.17   24 Jan 2002 13:27:24   dcg
CPVCS    don't call sobolev norm except for tet meshes
CPVCS    
CPVCS       Rev 1.16   10 Jan 2002 16:50:32   kuprat
CPVCS    Uncommented call to interpolate_hessian
CPVCS    
CPVCS       Rev 1.15   07 Jan 2002 20:36:40   kuprat
CPVCS    Commented out call to INTERPOLATE_HESSIAN.
CPVCS    
CPVCS       Rev 1.14   21 Dec 2001 18:18:02   kuprat
CPVCS    Corrected output string.
CPVCS    
CPVCS       Rev 1.13   21 Dec 2001 18:12:18   kuprat
CPVCS    We now call SETHESSIAN to obtain Hessian.  We do away
CPVCS    with synthesis of new Hessian in outer loop, using 
CPVCS    INTERPOLATE_HESSIAN instead.  
CPVCS    
CPVCS       Rev 1.12   19 Dec 2001 14:16:30   kuprat
CPVCS    Put in call to EVALUATE_SOBOLEVNORM, but commented out.
CPVCS    
CPVCS       Rev 1.11   06 Nov 2001 15:40:42   kuprat
CPVCS    Pass ITETCLR on to POLYFUN.
CPVCS    
CPVCS       Rev 1.10   22 Oct 2001 11:53:36   dcg
CPVCS    print measure of error and measure of node movement
CPVCS    
CPVCS       Rev 1.9   31 Jul 2001 09:57:32   kuprat
CPVCS    We allow inner iterations different than 4.
CPVCS    
CPVCS       Rev 1.8   30 Jul 2001 15:46:58   kuprat
CPVCS    Put in finite differencing (currently disabled).  Pulled out 
CPVCS    derivative (PFXX, PFX) evaluation from FREEMOVE_NOSB.F
CPVCS    
CPVCS       Rev 1.7   12 Jun 2001 14:37:58   kuprat
CPVCS    For the 'field' option, if there is no refreshing, make sure
CPVCS    there is just one outer iteration.
CPVCS    
CPVCS       Rev 1.6   09 Apr 2001 15:21:10   kuprat
CPVCS    We now used ABSVOLTOL equal to EPSILONV.
CPVCS    
CPVCS       Rev 1.5   Wed Apr 05 13:34:44 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.4   Thu Feb 03 08:54:16 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Wed Feb 02 12:50:46 2000   dcg
CPVCS    
CPVCS       Rev 1.2   27 Jan 2000 13:17:24   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:48:10   dcg
CPVCS    
CPVCS       Rev 1.0   06 Jan 2000 12:55:06   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.6   Wed Nov 10 15:25:20 1999   dcg
CPVCS    declare xmin..., time local variables
CPVCS    get values from storage blocks
CPVCS
CPVCS       Rev 1.5   Tue Nov 25 21:18:04 1997   kuprat
CPVCS    Nonverbose unless idebug>=1.
CPVCS
CPVCS       Rev 1.4   Tue Nov 25 13:31:02 1997   dcg
CPVCS    use get_surfaces, get_regions
CPVCS
CPVCS       Rev 1.2   Fri Oct 31 10:47:50 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.1   Wed Oct 29 17:08:02 1997   kuprat
CPVCS    Restored explicit guards against tet volume collapse
CPVCS    (i.e. call to damp_pt_by_volume).
CPVCS
CPVCS       Rev 1.0   Tue Sep 02 23:07:42 1997   kuprat
CPVCS    Initial revision.
C
      implicit none
 
      integer lenptr
      parameter (lenptr=1000000)
 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'smooth.h'
      include 'machine.h'
      include 'geom_lg.h'
 
      character*132 logmess
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipicr1, icr1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
      integer imt1(lenptr)
      pointer (ipicontab, icontab)
      integer icontab(50,lenptr)
      integer itp1(lenptr)
      integer isn1(lenptr)
      integer icr1(lenptr)
      real*8 xic(lenptr)
      real*8 yic(lenptr)
      real*8 zic(lenptr)
      integer itetclr(lenptr)
      integer itet(lenptr)
      integer itetoff(lenptr)
      integer itettyp(lenptr)
 
      pointer (ipfvec,fvec),
     &   (ipinvmpary,invmpary),(ipichildary,ichildary),
     &   (ipinvchildary,invchildary),(ipieltary,ieltary),
     &   (ipreffield,reffield)
      real*8 fvec(lenptr),reffield(lenptr)
      integer mpary(lenptr),invmpary(lenptr),ichildary(lenptr),
     &   invchildary(lenptr),ieltary(lenptr)
 
      character*32 cmo,action,cmo1,climit1,climit2,climit3,blkname,
     &   cfield,geom_name
      character*32 isubname
      character*132 com
 
      pointer (ipninexact,ninexact)
      integer ninexact(lenptr)
      pointer (ipireal1,ireal1)
      integer ireal1(lenptr)
      pointer (ipiparent,iparent)
      integer iparent(lenptr)
      pointer (ipnodhyb,nodhyb),(ipnodhyboff,nodhyboff)
      integer nodhyb(lenptr),nodhyboff(lenptr)
      pointer (ipvoloff,voloff),(iplocvoloff,locvoloff),
     &   (ipivoloffoff,ivoloffoff)
      real*8 voloff(lenptr)
      integer locvoloff(lenptr),ivoloffoff(lenptr)
      pointer (ipiedgeoff,iedgeoff)
      pointer (ipiedge,iedge)
      pointer (ipiedgemat,iedgemat)
      integer iedgeoff(lenptr),iedge(lenptr),iedgemat(lenptr)
      pointer (ipxsave,xsave),(ipysave,ysave),(ipzsave,zsave)
      real*8 xsave(lenptr),ysave(lenptr),zsave(lenptr)
      pointer (iphxx,hxx),(iphxy,hxy),(iphxz,hxz),(iphyy,hyy),(iphyz,hyz
     &   ),(iphzz,hzz)
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*),range
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,crosx,crosy,crosz,
     *   xmin,xmax,ymin,ymax,zmin,zmax,time,rout,
     &   ctrl,volmin,volmax,vol6,err,
     &   tolconv_sm,pf,pfx(3),pfxx(3,3),
     &   err1,pftot,epsilonl,epsilonv,area(3),
     &   epsilona,sob
 
      integer mpno,ierror,nnodes,length,icmotype,nelements,
     &   mbndry,icscode,i,ierrw,k,niters,iter,k1,
     &   ineghess,izeromove_force,izeromove_forcvol,izeromove_free,
     &   izeromove_val,ilen,ityp,len,
     &   node,maxiter_sm,itypconv_sm,ierrdum,nextnode,
     &   mpno_old,ichildno,ieltno,nod,nod1,j,locnod,jteti,
     &   ii,ihyb,loctet,
     &   i1,i2,i3,i4,icharlnf,iout,lout,itype
      pointer(ipout,out)
      real*8 out(*)
 
      real*8 absvoltol
      character*32 cout
      character*132 cbuf
      integer ierr
c
      logical lusefd,firsttot
      parameter (lusefd=.false.)
 
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
 
 
c statement functions for the components of the cross product
c ((x2,y2,z2)-(x1,y1,z1)) x ((x3,y3,z3)-(x1,y1,z1)) .
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
      isubname = 'mega3d'
      ierror=0
 
c.... issue SETHESSIAN command to generate necessary 2nd derivatives.
 
      if (action(1:5).eq.'field') then
         write(cbuf,'(3a)') 'sethessian/',cfield,'/ ; finish'
         call dotask(cbuf,ierr)
      elseif(action(1:4).eq.'geom') then
         ierr=0
      else
         write(cbuf,'(3a)') 'sethessian/',action,'/ ; finish'
         call dotask(cbuf,ierr)
      endif
      if (ierr.ne.0) then
         ierror=ierr
         write(logmess,'(a)')
     &      'MEGA3D: error computing Hessian'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      firsttot=.true.
 
c  get info from mesh object to be smoothed
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *   ipout,lout,itype,icscode)
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,icscode)
      call cmo_get_info('nelements',cmo,
     *   nelements,length,icmotype,icscode)
      call cmo_get_info('mbndry',cmo,
     *   mbndry,length,icmotype,icscode)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,icscode)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,icscode)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,icscode)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,icscode)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,icscode)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,icscode)
      call cmo_get_info('itetclr',cmo,
     *   ipitetclr,length,icmotype,icscode)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,icscode)
      call cmo_get_info('icontab',cmo,ipicontab,length,icmotype,icscode)
      call cmo_get_info('hxx',cmo,iphxx,length,icmotype,icscode)
      call cmo_get_info('hxy',cmo,iphxy,length,icmotype,icscode)
      call cmo_get_info('hxz',cmo,iphxz,length,icmotype,icscode)
      call cmo_get_info('hyy',cmo,iphyy,length,icmotype,icscode)
      call cmo_get_info('hyz',cmo,iphyz,length,icmotype,icscode)
      call cmo_get_info('hzz',cmo,iphzz,length,icmotype,icscode)
      call cmo_get_attinfo('frange',cmo,iout,range,cout,ipout,length
     &   ,icmotype,icscode)
 
c...  get debug level info
 
      call cmo_get_info('idebug',cmo,
     *   idebug,length,icmotype,icscode)
 
c...  set and get epsilon information
 
      call setsize
      call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsilona,
     &   epsilonv)
c$$$      absvoltol=epsilonv/sqrt(epsilonr*1000.)
      absvoltol=epsilonv
 
      call get_epsilon('epsilonl', epsilonl)
 
      if (idebug.ge.1) then
         print*,'epsilonv=',epsilonv
         print*,'absvoltol=',absvoltol
         print*,'epsilonl=',epsilonl
      endif
 
c     ..................................................................
c     Get surface information for use in constrained smoothing.
c     ..................................................................
      call mmfindbk('istype',geom_name,ipistype,len,icscode)
      call mmfindbk('surfparam',geom_name,ipsurfparam,len,icscode)
      call mmfindbk('offsparam',geom_name,ipoffsparam,len,icscode)
C
      call mmgetblk('ireal1',isubname,ipireal1,nnodes,1,icscode)
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('mega3d', 'unpacktp')
 
c     ..................................................................
c     find the parents of each node.
c
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
 
c.... change mass point array to contain only parent nodes.
 
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
      call mmgetblk('ichildary',isubname,ipichildary,nnodes,1,icscode)
      call mmgetblk('invchildary',isubname,ipinvchildary,nnodes,1,
     &   icscode)
      call mmgetblk('ieltary',isubname,ipieltary,nelements,1,icscode)
 
      do i=1,nnodes
         invmpary(i)=0
         invchildary(i)=0
      enddo
 
      mpno_old=mpno
      mpno=0
      ichildno=0
      do k=1,mpno_old
         if (ireal1(mpary(k)).eq.1.or.
     &      itp1(mpary(k)).eq.ifitpcup) then
            nod=iparent(mpary(k))
            if (invmpary(nod).eq.0) then
               mpno=mpno+1
               mpary(mpno)=nod
               invmpary(nod)=mpno
               if (itp1(nod).eq.ifitpcup) then
                  nod1=isn1(nod)
                  do while (nod1.ne.nod.and.nod1.ne.0)
                     ichildno=ichildno+1
                     ichildary(ichildno)=nod1
                     invchildary(nod1)=ichildno
                     nod1=isn1(nod1)
                  enddo
               else
                  ichildno=ichildno+1
                  ichildary(ichildno)=nod
                  invchildary(nod)=ichildno
               endif
 
            endif
         endif
      enddo
 
      ieltno=0
      do 40 i=1,nelements
         do j=1,nelmnen(itettyp(i))
            if (invchildary(itet(j+itetoff(i))).ne.0) then
               ieltno=ieltno+1
               ieltary(ieltno)=i
               goto 40
            endif
         enddo
 40   continue
 
c.... derive the nodnumtetv and jtetv arrays from the
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
 
c... Get list of relevant edges.
 
      call getiedge(mpno,mpary,ieltno,ieltary,nnodes,itet,itetoff,
     &   itetclr,itettyp,isubname,ipiedge,ipiedgeoff,ipiedgemat)
 
c obtain node-hyb relation.  For a given node, the node-hyb relation
c is a list of numbers that give the hybrid elements that the
c the node belongs to AND the local node number within each element.
 
      call getnodhyb(mpno,mpary,ieltno,ieltary,nnodes,itet,
     &   itetoff,itettyp,iparent,invmpary,isubname,ipnodhyb,ipnodhyboff)
 
c... For controlled smoothing, save node positions.
      if (ctrl.ne.0.) then
         call mmgetblk('xsave',isubname,ipxsave,mpno,2,icscode)
         call mmgetblk('ysave',isubname,ipysave,mpno,2,icscode)
         call mmgetblk('zsave',isubname,ipzsave,mpno,2,icscode)
 
         do k=1,mpno
            node=mpary(k)
            xsave(k)=xic(node)
            ysave(k)=yic(node)
            zsave(k)=zic(node)
         enddo
      endif
 
      call  cmo_get_info('maxiter_sm',cmo,
     *   maxiter_sm,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
 
      niters=maxiter_sm
 
      if (action(1:5).eq.'field') then
         blkname=cfield(1:icharlnf(cfield))
         call cmo_get_info(blkname,cmo,ipreffield,ilen,ityp,icscode)
         if(icscode.ne.0) then
            ierror=icscode
            write(logmess,'(a)')
     *         'MEGA3D: bad reference field'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
         if (action(1:13).eq.'field:refresh') then
            cmo1="radapt_cmo1"
            com="cmo/copy/radapt_cmo1/"//cmo//" ; finish"
            call dotaskx3d(com,icscode)
            com='cmo/select/'//cmo//
     &         " ; finish "
            call dotaskx3d(com,icscode)
         endif
      endif
 
c.... determine the volumes of unsmoothed virtual tets.
 
      volmin=1.d99
      volmax=-1.d99
      do ii=1,ieltno
         ihyb=ieltary(ii)
         do loctet=1,ihybnumtetv(itettyp(ihyb) )
 
            i1=itet( itetv(1,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i2=itet( itetv(2,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i3=itet( itetv(3,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i4=itet( itetv(4,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            area(1)=crosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            area(2)=crosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            area(3)=crosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            vol6=area(1)*(xic(i2)-xic(i1))+
     &         area(2)*(yic(i2)-yic(i1))+area(3)*(zic(i2)-zic(i1))
            volmin=min(volmin,vol6)
            volmax=max(volmax,vol6)
         enddo
      enddo
 
      volmin=volmin/6.
      volmax=volmax/6.
 
      if (idebug.ge.1) then
         write(logmess,'(a,1pe15.7)')
     &      'min. (virtual) tet volume before smoothing=',volmin
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,1pe15.7)')
     &      'max. (virtual) tet volume before smoothing=',volmax
         call writloga('default',0,logmess,0,ierrw)
      endif
 
      call cmo_get_info('itypconv_sm',cmo,
     *   itypconv_sm,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_attinfo('tolconv_sm',cmo,iout,
     *   tolconv_sm,cout,ipout,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_r')
      call get_global('time',iout,
     *   time,cout,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_r')
 
      if (idebug.ge.1) then
         write(logmess,'(a,i5)')
     *      'max. iters=',niters
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,i5)')
     *      'itypconv_sm=',itypconv_sm
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,e14.7)')
     *      'tolconv_sm=',tolconv_sm
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,e14.7)')
     *      'time=',time
         call writloga('default',0,logmess,0,ierrw)
      endif
 
c... fvec contains adaption field values.
 
      call mmgetblk('fvec',isubname,
     *   ipfvec,ichildno,2,icscode)
 
      call mmgetblk('ninexact',isubname,ipninexact,mpno,2,icscode)
 
c.... inexactly treated constraints involve movement of nodes in
c.... a field whose gradient points to the constraint surface.
c.... the only instance of this was the kent smith test problem;
c.... currently these kinds of constraints do not occur.
 
      do i=1,mpno
         ninexact(i)=0
      enddo
 
c...  'constructor' call for setting up arrays for polyfun.
 
      call polyfun(1,action,node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,itetclr,itet,itetoff,xic,yic,zic,
     &   nnodes,nelements,iedge,iedgeoff,iedgemat,ichildary,
     &   ichildno,invchildary,imt1,epsilonv,fvec,reffield,iparent,
     &   hxx,hxy,hxz,hyy,hyz,hzz,range,pf,pfx,pfxx)
 
      call polyfun(2,action,node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,itetclr,itet,itetoff,xic,yic,zic,
     &   nnodes,nelements,iedge,iedgeoff,iedgemat,ichildary,
     &   ichildno,invchildary,imt1,epsilonv,fvec,reffield,iparent,
     &   hxx,hxy,hxz,hyy,hyz,hzz,range,pf,pfx,pfxx)
 
      if (firsttot.and.action(1:4).ne.'geom') then
         if(itettyp(1).eq.ifelmtet)
     *   call evaluate_sobolevnorm(ieltary,ieltno,itet
     &      ,itetoff,xic,yic,zic,hxx,hxy,hxz,hyy,hyz,hzz,sob)
         write(logmess,'(a,e14.7)')
     *      'actual sobolev norm estimate=',sob
         call writloga('default',0,logmess,0,ierrw)
      endif
 
      do iter=1,niters
         ineghess=0
         izeromove_force=0
         izeromove_forcvol=0
         izeromove_free=0
         izeromove_val=0
         err=0.
         pftot=0.
 
         do 30 k1=1,mpno
 
            call primestep(mpno,k)
*                  k=k1
 
            node=mpary(k)
            call  mega3d_inner_loop(action,node,lusefd,ctrl,
     &               nodhyb,nodhyboff,itp1,icr1,icontab,
     &               ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &               ipivoloffoff,itettyp,itetclr,itet,itetoff,xic
     &               ,yic,zic,nnodes,nelements,iedge,iedgeoff
     &               ,iedgemat,ichildary,ichildno,invchildary,imt1
     &               ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz
     &               ,hyy,hyz,hzz,range,pf,pfx,pfxx,xsave,ysave,
     &               zsave,ipoffsparam,ipsurfparam,ipistype,err,pftot)
C
 30      continue
 
         err=sqrt(err/mpno)
         if (firsttot) then
            write(logmess,'(a,e14.7)')
     *         'total computer functional value=',pftot/4.
            call writloga('default',0,logmess,0,ierrw)
            firsttot=.false.
         endif
 
         if (idebug.ge.1) then
            write(logmess,'(a,i4,a,e14.7)')
     *         'iteration=',iter,'; root mean square error=',err
            call writloga('default',0,logmess,0,ierrw)
 
            write(logmess,'(a,e14.7)')
     *         'total computer functional value=',pftot/4.
            call writloga('default',0,logmess,0,ierrw)
 
            write(logmess,'(6(i4,a))')
     &         ineghess,' neg hess/',izeromove_force
     &         ,' force/',izeromove_forcvol,' forcvol/',
     &         izeromove_free,' free/',
     &         izeromove_val,' val.'
            call writloga('default',0,logmess,0,ierrw)
         endif
 
         if (iter.eq.1) then
            err1=err
         else
            if (err.le.err1*.01) goto 8900
         endif
      enddo
 
 8900 continue
 
      call polyfun(5,action,node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,itetclr,itet,itetoff,xic,yic,zic,
     &   nnodes,nelements,iedge,iedgeoff,iedgemat,ichildary,
     &   ichildno,invchildary,imt1,epsilonv,fvec,reffield,iparent,
     &   hxx,hxy,hxz,hyy,hyz,hzz,range,pf,pfx,pfxx)
 
c... copy new node positions to cmo
      do k=1,mpno
         node=mpary(k)
         if (isn1(node).ne.0) then
            nextnode=isn1(node)
            do while (nextnode.ne.node)
               xic(nextnode)=xic(node)
               yic(nextnode)=yic(node)
               zic(nextnode)=zic(node)
               nextnode=isn1(nextnode)
            enddo
         endif
      enddo
 
 
 
c.... determine the volumes of smoothed virtual tets.
 
      volmin=1.d99
      volmax=-1.d99
      do ii=1,ieltno
         ihyb=ieltary(ii)
         do loctet=1,ihybnumtetv(itettyp(ihyb) )
 
            i1=itet( itetv(1,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i2=itet( itetv(2,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i3=itet( itetv(3,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            i4=itet( itetv(4,loctet,itettyp(ihyb)) + itetoff(ihyb) )
            area(1)=crosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            area(2)=crosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            area(3)=crosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &         yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            vol6=area(1)*(xic(i2)-xic(i1))+
     &         area(2)*(yic(i2)-yic(i1))+area(3)*(zic(i2)-zic(i1))
            volmin=min(volmin,vol6)
            volmax=max(volmax,vol6)
         enddo
      enddo
 
      volmin=volmin/6.
      volmax=volmax/6.
      if (idebug.ge.1) then
         write(logmess,'(a,1pe15.7)')
     &      'min. (virtual) tet volume after smoothing=',volmin
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a,1pe15.7)')
     &      'max. (virtual) tet volume after smoothing=',volmax
         call writloga('default',0,logmess,0,ierrw)
      endif
 
c.... Refresh field if desired.
 
      if (action(1:13).eq.'field:refresh') then
         com="doping/table/"//
     &      cfield(1:icharlnf(cfield))//
     &      "/set/"//
     &      climit1(1:icharlnf(climit1))//
     &      ","//
     &      climit2(1:icharlnf(climit2))//
     &      ","//
     &      climit3(1:icharlnf(climit3))//
     &      "/radapt_cmo1/"//
     &      cfield(1:icharlnf(cfield))//
     &      " ; finish "
         call dotaskx3d(com,icscode)
 
         com='cmo/release/radapt_cmo1'//
     &      " ; finish "
         call dotaskx3d(com,icscode)
         com='cmo/select/'//cmo//
     &      " ; finish "
         call dotaskx3d(com,icscode)
      endif
 
      call mmverify()
 
 9999 continue
      write(logmess,'(a,e14.7)')
     *   ' root mean square node motion',err
      call writloga('default',0,logmess,0,ierrw)
      call mmrelprt(isubname,icscode)
      return
      end
