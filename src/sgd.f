c #####################################################################
      subroutine sgd(cmo,toldamage,msmoothed,mpary_in,mpno_in,ierror)
c
c #####################################################################
c
c    PURPOSE
c
c       SGD ("Smooth minding Graph Damage")
c     takes a mesh object and smooths nodes that are in the
c     list of selected mass points and only allows movements that would
c     cause a 'graph damage' of no greater than TOLDAMAGE.
c
c    INPUT ARGUMENTS -
c
c       CMO - name of current mesh object
c       TOLDAMAGE - maximum allowable graph damage for
c                   annihilated nodes
c       TOLLENGTH - maximum allowable length of edges
c                   created by node annihilation
c       MPARY_IN - array of mass points
c       MPNO_IN - no. of mass points
c       ILAPLACE - 0=selective Laplace+movements to neighbors
c                  1=selective Laplace only.
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
c
c $Log: sgd.f,v $
c Revision 2.00  2007/11/09 20:04:03  spchu
c Import to CVS
c
CPVCS    
CPVCS       Rev 1.21   01 Mar 2002 14:45:36   dcg
CPVCS    adaptive merging
CPVCS    
CPVCS       Rev 1.20   26 Feb 2002 12:06:42   dcg
CPVCS    pass ctrl and range variables correctly
CPVCS
CPVCS       Rev 1.19   25 Feb 2002 14:12:14   dcg
CPVCS    use massage.h include file
CPVCS
CPVCS       Rev 1.18   20 Feb 2002 15:19:02   dcg
CPVCS    set isafield to false as default
CPVCS
CPVCS       Rev 1.17   05 Feb 2002 17:34:04   dcg
CPVCS    fix problems introduced with error adaption changes
CPVCS
CPVCS       Rev 1.16   05 Feb 2002 14:58:48   dcg
CPVCS    return number of nodes smoothed
CPVCS
CPVCS       Rev 1.15   05 Feb 2002 10:26:42   dcg
CPVCS    changes for adaptive smoothing
CPVCS
CPVCS       Rev 1.14   31 Jan 2002 13:10:02   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.13   17 Jan 2002 14:05:48   dcg
CPVCS    more changes for discrete mode
CPVCS
CPVCS       Rev 1.12   14 Jan 2002 17:22:10   dcg
CPVCS    add changes for discrete mode
CPVCS
CPVCS       Rev 1.12   11 Jan 2002 13:30:44   dcg
CPVCS    add changes for discrete mode
CPVCS
CPVCS       Rev 1.11   06 Jul 2000 19:00:44   kuprat
CPVCS    Corrected bug where some node relaxations may have underestimated
CPVCS    the damage.
CPVCS
CPVCS       Rev 1.10   01 Mar 2000 15:00:54   kuprat
CPVCS    We altered the call to DAMAGE_EST_2 to conform with altered
CPVCS    parameter list for this function.
CPVCS
CPVCS       Rev 1.9   Thu Nov 18 18:20:36 1999   kuprat
CPVCS     We now reduce damage allowed if inscribed radius is large, and
CPVCS    only use full user damage tolerance if inscribed radius is small
CPVCS    (and we want to try everything to increase it).
CPVCS
CPVCS       Rev 1.8   Tue Oct 12 08:20:58 1999   kuprat
CPVCS    We now smooth using inscribed radius rather than aspect ratio.
CPVCS
CPVCS       Rev 1.7   Fri May 14 11:26:32 1999   kuprat
CPVCS    Send warnings to CWARN which is set to 'bat'.  (I.e. not
CPVCS    'default', 'tty', or 'log'.)
CPVCS
CPVCS       Rev 1.6   Thu Dec 24 14:38:58 1998   kuprat
CPVCS    Commented out 'noterminate' dumps.
CPVCS
CPVCS       Rev 1.2   Wed Nov 04 13:47:54 1998   kuprat
CPVCS    Minor change to output message.
CPVCS
CPVCS       Rev 1.1   Wed Nov 04 11:14:28 1998   dcg
CPVCS    fix never used warnings
CPVCS
CPVCS       Rev 1.0   Wed Nov 04 02:45:12 1998   kuprat
CPVCS    Initial revision.
c
c #####################################################################
 
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'geom_lg.h'
      include 'smooth.h'
      include 'massage.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      character*132 logmess
      pointer (ipimt1, imt1), (ipitp1, itp1),(ipisn1, isn1),
     *  (ipicr1, icr1)
      pointer (ipxic, xic),(ipyic, yic),(ipzic, zic)
      pointer (ipitetclr, itetclr)
      integer itetclr(lenptr)
      pointer (ipitet, itet),(ipitetoff, itetoff)
      pointer (ipitettyp, itettyp),(ipicontab, icontab)
      integer icontab(50,lenptr)
      integer itp1(lenptr),isn1(lenptr),icr1(lenptr),imt1(lenptr)
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)
      integer itet(lenptr),itetoff(lenptr),itettyp(lenptr)
      pointer (ipjtet,jtet),(ipjtetoff,jtetoff)
      integer jtet(lenptr),jtetoff(lenptr)
 
      pointer (ipnodhyb,nodhyb),(ipnodhyboff,nodhyboff)
      integer nodhyb(lenptr),nodhyboff(lenptr)
      pointer (ipieltary,ieltary)
      integer ieltary(lenptr)
      pointer (ipireal1,ireal1),(ipinvmpary,invmpary),
     &   (ipichildary,ichildary),(ipinvchildary,invchildary),
     &   (ipiparent,iparent),(ipmpary,mpary)
      integer invmpary(lenptr),ichildary(lenptr),invchildary(lenptr),
     &   ireal1(lenptr),iparent(lenptr),mpary(lenptr)
      pointer (ipvoloff,voloff),(iplocvoloff,locvoloff),
     &   (ipivoloffoff,ivoloffoff)
      real*8 voloff(lenptr)
      integer locvoloff(lenptr),ivoloffoff(lenptr)
      pointer (ipiedgeoff,iedgeoff),(ipiedge,iedge),
     *  (ipiedgemat,iedgemat)
      integer iedgeoff(lenptr),iedge(lenptr),iedgemat(lenptr)
      pointer (ipfvec,fvec),(ipreffield,reffield)
      real*8 fvec(lenptr),reffield(lenptr)
      pointer (iphxx,hxx),(iphxy,hxy),(iphxz,hxz),(iphyy,hyy),
     &   (iphyz,hyz),(iphzz,hzz)
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*),range
 
      pointer (ipmcr,mcr),(ipmat,mat),
     &   (ipnearestnbr,nearestnbr),
     &   (ipinvneibr,invneibr),(ipneibr,neibr),
     &   (ipdnbr,dnbr),(ipnbrlist,nbrlist),
     &   (ipielts,ielts),(iplstale,lstale),
     *   (iptested,tested),(ipinvtested,invtested)
      integer mcr(lenptr),mat(lenptr),invneibr(lenptr),
     &   nearestnbr(lenptr),neibr(lenptr),nbrlist(lenptr),
     &   ielts(lenptr),tested(lenptr),invtested(lenptr)
      real*8  areak,dnbr(lenptr),pf,pfx(3),pfxx(3,3),distpmin,
     *   xplane(3),yplane(3),zplane(3),distp,xp,yp,zp,xelm(2),
     *   yelm(2),zelm(2),asmallnumber
      logical lstale(lenptr),lmoved, validfield
 
      integer maxlenstr
      parameter (maxlenstr=4095)
 
      character*8  cdefault, cwarn
      character*32 cmo,isubname,cfield,action,cout,
     *   geom_name
      character*132 cbuff
      parameter (asmallnumber=-1.0d+99)
 
      integer mpary_in(lenptr),mpno_in,iflag,
     &   ierror,nnodes,length,icmotype,nelements,mbndry,icscode,
     &   ieltno,i,j,node,nod1,icr,nmat,ii,lochybnod,ihyb,
     &   ityp,k,locnbr,nbr,nnbr,ilen,indx,
     &   nearnbr,k1,i2,i3,i4,minmat,maxmat,nfound,ipoint,
     &   ierrw,mpno,mpno_old,ierrdum,
     &   ichildno,nod,len_ieltary,len_mat,inc,nummat,
     &   nsdtopo,nelementss,iout,ier,
     &   len_neibr,nodek,i0,nelt,nearnbrj,
     &   nodek1,minpt,kk,icand,
     &   nef_cmo,ioppnod,len_ielts,nsmoothed,
     &   ismooth,nextnode,ifromicr,ifromitp,mpk,
     &   msmoothed,maxsweep,isweep,ntested,nnodess
 
      real*8 toldamage,dnearnbr,frac,damage,xproj,damagea,
     &   yproj,zproj,a1x,a1y,a1z,ax,ay,az,atot,avec,alg_epsilon,
     &   projmin,projmax,proj,ascend,epsln,xold,yold,zold,
     &   vtol,vtolabs,worstvol,projdamage,damage_est_2,
     &   dartol,worstar,xcen,ycen,zcen,xnew,ynew,znew,dist,
     &   synthx,synthy,synthz,omega,darl2,dcen
     &   ,toldamageused,rout,x1,y1,z1,distmin,damaged,
     *   damage_est_discrete,dist1,epsilonv,xcenadapt,ycenadapt,
     *   zcenadapt,err,pftot,xcena,ycena,zcena,v
c
      pointer (ipxics,xics),(ipyics,yics),(ipzics,zics)
      real*8 xics(*),yics(*),zics(*)
      pointer (ipitets,itets)
      integer itets(3,*)
      pointer (iplinkt, linkt),(ipsbox, sbox),(ipout,out)
      integer linkt(*)
      real*8 sbox(*),out(*)
      pointer (ipitfound,itfound),(ipisurftst,isurftst),(ipwork,work)
      integer itfound(*),isurftst(*)
      real*8 work(*)
      pointer (ipxsave,xsave),(ipysave,ysave),(ipzsave,zsave)
      real*8 xsave(*),ysave(*),zsave(*)
 
c
      parameter (alg_epsilon=1.d-10)
      logical lvalidface,lwontinvert_smooth,ltripedge,lsomereversed,
     * lusefd,goodface
      parameter (lusefd=.false.)
      character*8 eq
 
      real*8 tolcutfactor, tolimprovefactor,ctrl
      parameter (tolcutfactor=0.25,tolimprovefactor=1.1)
 
      include 'statementfunctions.h'
 
      isubname='sgd'
      cdefault='default'
      action='field'
      err=zero
      pftot=zero
      maxsweep=100
      lsomereversed=.false.
      call get_epsilon('epsilonl',epsln)
      call get_epsilon('epsilonv',epsilonv)
      validfield=.false.
 
c.... Warnings are sent to cwarn.  The choices are 'default', 'bat', and
c.... 'tty'.  ('log' is inappropriate.)  For nonexpert users, 'bat'
c.... is preferred.
 
      cwarn='bat'
      ntested=0
      ierror=0
 
      vtolabs=epsilonv*1.1
      vtol=vtolabs
 
c.... Allocate memory for arrays that cannot grow in size.  (There
c.... are a couple of arrays that have to be allocated within the
c.... following loop, since they could grow in size.)
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
 
      call mmgetblk('ireal1',isubname,ipireal1,nnodes,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
      call mmgetblk('ichildary',isubname,ipichildary,nnodes,1,icscode)
      call mmgetblk('invchildary',isubname,ipinvchildary,nnodes,1,
     &   icscode)
      call mmgetblk('mpary',isubname,ipmpary,mpno_in,1,icscode)
      call mmgetblk('mcr',isubname,ipmcr,mpno_in,1,icscode)
      call mmgetblk('nearestnbr',isubname,ipnearestnbr,
     &   mpno_in,1,icscode)
      call mmgetblk('lstale',isubname,iplstale,mpno_in,1,icscode)
      call mmgetblk('invneibr',isubname,ipinvneibr,
     &   nnodes,1,icscode)
 
      do i=1,nnodes
         invneibr(i)=0
      enddo
 
      len_ieltary=0
      len_mat=0
      len_neibr=0
      len_ielts=0
 
c.... We copy the input mass point array to a new array.  The new array
c.... will shrink as we deem certain nodes in the array to be
c.... unsuitable for annihilation.
 
      do k=1,mpno_in
         mpary(k)=mpary_in(k)
      enddo
      mpno=mpno_in
 
c.... Get info from mesh object.
 
      call cmo_get_intinfo('nnodes',cmo,
     *      nnodes,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,
     *      nelements,length,icmotype,ierror)
      call cmo_get_intinfo('ndimensions_topo',cmo,
     *      nsdtopo,length,icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,
     *      mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('idebug',cmo,
     *      idebug,length,icmotype,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *   ipout,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,
     *      ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype
     &      ,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype
     &      ,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype
     &      ,ierror)
      call cmo_get_info('icontab',cmo,ipicontab,length,icmotype
     &      ,ierror)
      call cmo_get_intinfo('faces_per_element',cmo,nef_cmo,
     &      length,icmotype,ierror)
c
c.... space for discrete surface flag
c
      call mmgetblk('isurftst',isubname,ipisurftst,nnodes
     *      ,1,icscode)
      do i=1,nnodes
         isurftst(i)=0
      enddo
C
c....  see if discrete refinement is on
c....  get temporary storage for already tried surface nodes
c....  set up kdtree for surface after getting mesh object info
c....  for surface mesh
c....
      if(isdiscrete) then
         call cmo_get_intinfo('nelements',surfcmo_name,nelementss,
     *      length,icmotype,ier)
         if(ier.ne.0) then
            call x3d_error(surfcmo_name,'cmo_get')
            go to 9999
         endif
         call cmo_get_intinfo('nnodes',surfcmo_name,nnodess,
     *      length,icmotype,ier)
         call mmgetblk('tested',isubname,iptested,nnodess,
     *      1,icscode)
         call mmgetblk('invtested',isubname,ipinvtested,nnodess,
     *      1,icscode)
         if(toldamage_discrete.ne.zero) toldamage=toldamage_discrete
         call cmo_get_info('xic',surfcmo_name,ipxics,
     *      ilen,icmotype,ier)
         call cmo_get_info('yic',surfcmo_name,ipyics,
     *      ilen,icmotype,ier)
         call cmo_get_info('zic',surfcmo_name,ipzics,
     *      ilen,icmotype,ier)
         call cmo_get_info('itet',surfcmo_name,ipitets,
     *      ilen,icmotype,ier)
C
C     ADD ATTRIBUTES FOR k-D TREE OF THE surface if needed
C
         call cmo_get_info('linkt',surfcmo_name,iplinkt,
     *      ilen,icmotype,ier)
         if(ier.ne.0) then
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'v2' //
     &      '/INT' //
     &      '/scalar/scalar/constant/permanent//2.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'linkt' //
     &      '/VINT' //
     &      '/v2/nelements//permanent/x/0.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
         endif
C
         call cmo_get_info('sbox',surfcmo_name,ipsbox,
     *      ilen,icmotype,ier)
         if(ier.ne.0) then
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'v12' //
     &      '/INT' //
     &      '/scalar/scalar/constant/permanent//12.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
            cbuff='cmo/addatt/' //
     &      '/' //
     &      'sbox' //
     &      '/VDOUBLE' //
     &      '/v12/nelements/linear/permanent/x/0.0' //
     &      ' ; finish'
            call dotaskx3d(cbuff,ierror)
            call kdtree(xics,yics,zics,itets,nelementss,
     *      linkt,sbox,ier)
         endif
c
c  get space for local arrays
c
         call mmgetblk('itfound',isubname,ipitfound,5*nelementss
     *      ,1,icscode)
         call mmgetblk('work',isubname,ipwork,nelementss
     *      ,2,icscode)
c
c  find nodes that are on surface
c
         eq='eq'
         call shttstv(xic,yic,zic,nnodes,epsln,surfcmo_name,
     *      eq,isurftst)
c
 
      endif
c
c....  test if we are using error adpation
c
      if(isafield) then
         cfield=adaption_field_name
         ctrl=zero
         call cmo_get_info('maxiter_sm',cmo,maxsweep,length,
     *      icmotype,icscode)
         call cmo_get_info(cfield,cmo,ipreffield,length,icmotype,
     *      icscode)
         if(icscode.ne.0) then
            ierror=icscode
            write(logmess,'(a)')
     *         'sgd: bad reference field'
            call writloga(cdefault,0,logmess,0,ierrw)
            goto 9999
         endif
c.... issue SETHESSIAN command to generate necessary 2nd derivatives.
         write(cbuff,'(3a)') 'sethessian/',cfield,'/ ; finish'
         call dotask(cbuff,ier)
         if (ier.ne.0) then
            ierror=ier
            write(logmess,'(a)')
     &         'sgd: error computing Hessian'
            call writloga(cdefault,0,logmess,0,ierrw)
            goto 9999
         endif
         call cmo_get_info('hxx',cmo,iphxx,length,icmotype,ierror)
         call cmo_get_info('hxy',cmo,iphxy,length,icmotype,ierror)
         call cmo_get_info('hxz',cmo,iphxz,length,icmotype,ierror)
         call cmo_get_info('hyy',cmo,iphyy,length,icmotype,ierror)
         call cmo_get_info('hyz',cmo,iphyz,length,icmotype,ierror)
         call cmo_get_info('hzz',cmo,iphzz,length,icmotype,ierror)
         call cmo_get_attinfo('frange',cmo,iout,range,cout,ipout,length
     &   ,icmotype,ierror)
c
c  check that function has a non-zero range - if not turn off
c  error based smoothing
c
         if(range.gt.epsln) then
            validfield=.true.
         endif
 
c     ..................................................................
c     Get surface information for use in constrained smoothing.
c     ..................................................................
         call mmfindbk('istype',geom_name,ipistype,length,ierror)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      endif
 
      call mmgetblk('xsave',isubname,ipxsave,mpno,2,icscode)
      call mmgetblk('ysave',isubname,ipysave,mpno,2,icscode)
      call mmgetblk('zsave',isubname,ipzsave,mpno,2,icscode)
c
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('sgd', 'unpacktp')
c
c
c     ..................................................................
c     find the parents of each node.
c
      call unpackpc(nnodes,itp1,isn1,iparent)
 
c.... change mass point array to contain only parent nodes.
 
      do i=1,nnodes
            invmpary(i)=0
            invchildary(i)=0
      enddo
 
      mpno_old=mpno
      mpno=0
      ichildno=0
      do k=1,mpno_old
            if (ireal1(mpary(k)).eq.1.or.
     &         itp1(mpary(k)).eq.ifitpcup) then
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
 
c... fvec contains adaption field values.
 
      if(validfield) then
         call mmgetblk('fvec',isubname,
     *     ipfvec,ichildno,2,icscode)
      endif
 
c
      if (mpno.le.0) goto 9999
 
c.... Compute list of elements that are involved in this computations
c.... (ie contain at least one node in CHILDARY).
 
      if (len_ieltary.eq.0) then
            len_ieltary=100+nelements
            call mmgetblk('ieltary',isubname,ipieltary,
     &         len_ieltary,1,icscode)
       elseif (len_ieltary.lt.nelements) then
            inc=100+nelements-len_ieltary
            len_ieltary=len_ieltary+inc
            call mmincblk('ieltary',isubname,ipieltary,inc,icscode)
       endif
 
       minmat=999999
       maxmat=-999999
       ieltno=0
       do  i=1,nelements
          do j=1,nelmnen(itettyp(i))
             if (invchildary(itet(j+itetoff(i))).ne.0) then
                ieltno=ieltno+1
                ieltary(ieltno)=i
                minmat=min(minmat,imt1(itet(1+itetoff(i))))
                maxmat=max(maxmat,imt1(itet(1+itetoff(i))))
                goto 40
             endif
          enddo
 40       continue
       enddo
 
       nummat=maxmat-minmat+1
       if (len_mat.eq.0) then
            len_mat=100+nummat
            call mmgetblk('mat',isubname,ipmat,len_mat,1,icscode)
       elseif (len_mat.lt.nummat) then
            inc=100+nummat-len_mat
            call mmincblk('mat',isubname,ipmat,inc,icscode)
       endif
 
       if (len_neibr.eq.0) then
            len_neibr=100
            call mmgetblk('neibr',isubname,ipneibr,len_neibr,1,icscode)
            call mmgetblk('dnbr',isubname,ipdnbr,len_neibr,2,icscode)
            call mmgetblk('nbrlist',isubname,ipnbrlist,len_neibr,1
     &         ,icscode)
       endif
       if (len_ielts.eq.0) then
            len_ielts=100
            call mmgetblk('ielts',isubname,ipielts,len_ielts,1,icscode)
       endif
c
c...  'constructor' call for setting up arrays for polyfun.
      if(validfield) then
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
      endif
 
c obtain node-hyb relation.  For a given node, the node-hyb relation
c is a list of numbers that give the hybrid elements that the
c the node belongs to AND the local node number within each element.
 
       call getnodhyb(mpno,mpary,ieltno,ieltary,nnodes,itet,
     &      itetoff,itettyp,iparent,invmpary,isubname,ipnodhyb
     &      ,ipnodhyboff)
 
c.... Define MCR which gives the total effective number of constraints
c.... on each parent node.  MCR equals the sum of geometrical
c.... constraints plus 'material constraints'.  Geometrical constraints
c.... are, for example, 'plane' where a node is constrained to lie on
c.... a plane.  'Material constraints' occur where a node has more than
c.... one child point so that reconnection must respect the interfaces
c.... between the materials.
c....
c.... Thus, if MCR=0, the point is totally unconstrained, and we
c....     consider the damage of moving it toward its neighbor to be zero.
c.... If MCR=1, the point lies on an interface or geometrical constraint
c....     and it can only be moved toward interface neighbors.
c....     A nonzero amount of damage may occur which is usually defined
c ...     to be
c....     an estimate of the 'fatness' of the polygon formed by joining
c....     the node to all its interface neighbors.
c.... If MCR=2, the point lies on an interfacial curve and it can
c....     only be merged to either one of its curve neighbors.  The
c....     damage incurred by this merge is usually equal to the height
c ...    of the
c....     triangle formed by the node and its two curve neighbors.
c.... If MCR>=3, the point is critical in defining the geometry or
c....     interface.  The damage caused by merging this point into
c....     another point would thus be equal to the distance to the
c ...     neighboring
c....     point.  Currently, this routine does not allow such a merge.
      do i=1,mpno
         mcr(i)=0
         node=mpary(i)
         if (itp1(node).eq.ifitpcup) then
               nod1=isn1(node)
               do while (nod1.ne.node.and.nod1.ne.0)
                  mcr(i)=mcr(i)+1
                  nod1=isn1(nod1)
               enddo
               mcr(i)=mcr(i)-1
         endif
 
         icr=icr1(node)
         if (icr.gt.0) then
               ifromicr=icontab(1,icr)
         else
               ifromicr=0
         endif
 
         ifromitp=0
         if (itp1(node).eq.ifitpcup) then
            nod1=isn1(node)
            do while (nod1.ne.node.and.nod1.ne.0)
               if (itp1(nod1).ge.ifitpst2.and.itp1(nod1).le.ifitpen2)
     &            then
                  ifromitp=1
               endif
               nod1=isn1(nod1)
            enddo
         elseif (itp1(node).ge.ifitpst2.and.itp1(node).le.ifitpen2)
     &      then
            ifromitp=1
         endif
 
         if (ifromitp.eq.1) then
            mcr(i)=mcr(i)+max(1,ifromicr)
         endif
 
      enddo
 
c.... Loop over all mass points for potential inclusion into
c.... smoothing list.
 
      msmoothed=0
c$$$      omega=0.1
      omega=0.25
 
      do i=1,mpno
         lstale(i)=.true.
         node=mpary(i)
      enddo
c
c.... loop over all nodes maxsweep times
c
 
      do isweep=1,maxsweep
 
         nsmoothed=0
         do i=1,mpno
            node=mpary(i)
            xsave(i)=xic(node)
            ysave(i)=yic(node)
            zsave(i)=zic(node)
         enddo
         do 10 i0=1,mpno
            call primestep(mpno,i)
 
            if (.not.lstale(i)) goto 10
 
            node=mpary(i)
            nelt=nodhyboff(i+1)-nodhyboff(i)
            if (validfield) then
               call  mega3d_inner_loop(action,node,lusefd,ctrl,
     &               nodhyb,nodhyboff,itp1,icr1,icontab,
     &               ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &               ipivoloffoff,itettyp,itetclr,itet,itetoff,xic
     &               ,yic,zic,nnodes,nelements,iedge,iedgeoff
     &               ,iedgemat,ichildary,ichildno,invchildary,imt1
     &               ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz
     &               ,hyy,hyz,hzz,range,pf,pfx,pfxx,xsave,ysave,
     &               zsave,ipoffsparam,ipsurfparam,ipistype,err,pftot)
c
c.... new coordinates are in xic(node),yic(node) and zic(node)
c
               xcenadapt=xic(node)
               ycenadapt=yic(node)
               zcenadapt=zic(node)
               xic(node)=xsave(i)
               yic(node)=ysave(i)
               zic(node)=zsave(i)
C
            endif
c... Get the materials and constraints that NODE participates in.
 
            if (itp1(node).eq.ifitpcup) then
               nod1=isn1(node)
               nmat=0
               do while (nod1.ne.node.and.nod1.ne.0)
                  nmat=nmat+1
                  mat(nmat)=imt1(nod1)
                  nod1=isn1(nod1)
               enddo
            else
               nmat=1
               mat(nmat)=imt1(node)
            endif
 
            if (len_ielts.lt.nelt) then
               inc=nelt-len_ielts+100
               len_ielts=len_ielts+inc
               call mmincblk('ielts',isubname,ipielts,inc,icscode)
            endif
 
            do j=nodhyboff(i)+1,nodhyboff(i+1)
               ii=1+(nodhyb(j)-1)/maxnen
               ihyb=ieltary(ii)
               ielts(j-nodhyboff(i))=ihyb
            enddo
c
c.... In the case of triangular grids, compute a synthetic normal at
c.... NODE to be used to determine the orientation of triangles
c.... incident upon NODE.
c
            if (nsdtopo.eq.2) then
               call synthnormal(node,nelt,ielts,iparent,itet,
     &            itetoff,xic,yic,zic,epsln,synthx,synthy,synthz
     &            ,lsomereversed)
            endif
c
c.... Get the inscribed smallest inscribed radius and
c.... smallest volume of all elements containing this node
c.... Note this routine assumes tetrahedra
c
            if (validfield) then
               dartol=asmallnumber
               if(toldamage_4d.gt.zero) then
                  toldamageused=toldamage_4d
               else
                  toldamageused=toldamage
               endif                
            else
               call polyar1(nelt,ielts,itet,itetoff,xic,yic,
     &            zic,nsdtopo,worstvol,darl2,worstar)
               dartol=worstar*tolimprovefactor
               if (worstar.lt.toldamage) then
                  toldamageused=toldamage
               else
                  toldamageused=toldamage*tolcutfactor
               endif
            endif
c.... Treat the case where the node is a 'curve' point
c.... mcr=2 for 3d, mcr=1 for 2d.
 
            if ((mcr(i).eq.2.and.nsdtopo.eq.3).or.
     *          (mcr(i).eq.1.and.nsdtopo.eq.2)) then
 
c.... Loop over elements sharing node and determine the
c.... (two) curve neighbors.
 
               xcen=0.
               ycen=0.
               zcen=0.
               nnbr=0
               do j=nodhyboff(i)+1,nodhyboff(i+1)
                  ii=1+(nodhyb(j)-1)/maxnen
                  lochybnod=nodhyb(j)-maxnen*(ii-1)
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
c... LOCHYBNOD is the the local node number of NODE in the
c... general ("hybrid") element IHYB.
c... Loop thru edges emanating from this node in this
c... element, and see if the neighboring nodes obey
c... all the constraints and share in all the materials
c... that NODE has.  Nodes that satisfy this (there should
c... be two of them) live on the same boundary curve as NODE.
 
                  do 20 k=1,nelmnee(ityp)
                     if (lochybnod.eq.ielmedge1(1,k,ityp)) then
                        locnbr=ielmedge1(2,k,ityp)
                     elseif (lochybnod.eq.ielmedge1(2,k,ityp)) then
                        locnbr=ielmedge1(1,k,ityp)
                     else
                        goto 20
                     endif
                     nbr=iparent(itet(locnbr+itetoff(ihyb)))
 
                     if (invneibr(nbr).eq.0) then
 
c.... If the neighboring node is not a curve neighbor, skip him.
 
                        if (nsdtopo.eq.2) then
                           ioppnod=6-locnbr-lochybnod
                           if (jtet(ioppnod+jtetoff(ihyb)).lt.mbndry)
     &                        goto 20
                        else
                           if (.not.ltripedge(ihyb,k,itet,itetoff
     &                        ,itettyp,iparent,jtet,jtetoff,mbndry
     &                        ,nef_cmo,icr1,icontab)) goto 20
                        endif
 
                        nnbr=nnbr+1
                        if (len_neibr.lt.nnbr) then
                           inc=nnbr-len_neibr+100
                           len_neibr=len_neibr+inc
                           call mmincblk('neibr',isubname
     &                        ,ipneibr,inc,icscode)
                           call mmincblk('dnbr',isubname
     &                        ,ipdnbr,inc,icscode)
                           call mmincblk('nbrlist',isubname
     &                        ,ipnbrlist,inc,icscode)
                        endif
                        neibr(nnbr)=nbr
                        invneibr(nbr)=nnbr
 
                        xcen=xcen+xic(nbr)
                        ycen=ycen+yic(nbr)
                        zcen=zcen+zic(nbr)
 
                     endif
 
 20               continue
 
               enddo
 
c.... Check that we have exactly two curve neighbors.
 
               if (nnbr.ne.2) then
                  write(logmess,'(a,i4,a,i6)') 'Error:  ',nnbr
     &               ,' curve neighbors at ',node
                  call writloga(cwarn,0,logmess,0,ierrw)
                  lmoved=.false.
                  goto 900
               endif
 
c.... Check damage that moving NODE would cause.  We project NODE
c.... orthogonally onto the segment between neibr(1) and neibr(2).
c.... We calculate FRAC, where
c....     XPROJ=(1-FRAC)*XIC(NEIBR(1))+FRAC*XIC(NEIBR(2))
c.... Here XPROJ is the x-coordinate of the orthogonal projection of
c.... NODE onto the segment between NEIBR(1) and NEIBR(2).  A similar
c.... statement holds for the y- and z- coordinates.
c.... If 0<=FRAC<=1, we have that the DAMAGE would be equal to the
c.... distance of the orthogonal projection.  That is, the distance
c.... between NODE and (XPROJ,YPROJ,ZPROJ).
c.... If FRAC<0, the damage is equal to the distance between NODE
c.... and NEIBR(1).
c.... If FRAC>1, the damage is equal to the distance between NODE
c.... and NEIBR(2).
 
               frac=((xic(neibr(2))-xic(neibr(1)))*
     &            (xic(node)-xic(neibr(1)))+
     &            (yic(neibr(2))-yic(neibr(1)))*
     &            (yic(node)-yic(neibr(1)))+
     &            (zic(neibr(2))-zic(neibr(1)))*
     &            (zic(node)-zic(neibr(1))))/
     &            ((xic(neibr(2))-xic(neibr(1)))*
     &            (xic(neibr(2))-xic(neibr(1)))+
     &            (yic(neibr(2))-yic(neibr(1)))*
     &            (yic(neibr(2))-yic(neibr(1)))+
     &            (zic(neibr(2))-zic(neibr(1)))*
     &            (zic(neibr(2))-zic(neibr(1))))
 
               xproj=frac*xic(neibr(2))+(one-frac)*xic(neibr(1))
               yproj=frac*yic(neibr(2))+(one-frac)*yic(neibr(1))
               zproj=frac*zic(neibr(2))+(one-frac)*zic(neibr(1))
               damage=sqrt((xic(node)-xproj)**2+
     &            (yic(node)-yproj)**2+
     &            (zic(node)-zproj)**2)
 
               xcen=xcen/nnbr
               ycen=ycen/nnbr
               zcen=zcen/nnbr
c
c.... first try midpoint of curve - check for inversion if no
c.... inversions move node to these coordinates
c.... if node to be moved is on 'discrete' surface then
c.... use nearestpoint to get set of nodes on surface that are
c.... closest to the midpoint
c.... if adaptive smoothing is turned on, project returned
c.... coordinates onto edge - see if projected coordinates
c.... are within the line segment if not skip this node
c.... in this case the previous damage estimate is valid
c
               if(validfield) then
 
                 call project_point_to_line(xic(neibr(1)),
     *             yic(neibr(1)),zic(neibr(1)),xic(neibr(2)),
     *             yic(neibr(2)),zic(neibr(2)),xcenadapt,
     *             ycenadapt,zcenadapt,xproj,yproj,zproj)
                 xelm(1)=  xic(neibr(1))
                 yelm(1)=  yic(neibr(1))
                 zelm(1)=  zic(neibr(1))
                 xelm(2)=  xic(neibr(2))
                 yelm(2)=  yic(neibr(2))
                 zelm(2)=  zic(neibr(2))
                 call inside_element(ifelmlin,xelm,yelm,zelm,
     *             xproj,yproj,zproj,iflag)
                 if(iflag.ne.0) go to 150
                 xcen=xproj
                 ycen=yproj
                 zcen=zproj
               endif
c
               if(isurftst(node).eq.1) then
                  call nearestpoint(xcen,ycen,zcen,
     *              xics,yics,zics,itets,
     *              xold,yold,zold,
     *              linkt,sbox,
     *              epsln,work,nfound,itfound,ierror)
C
                  if (nfound .eq. 0) then
                    write(logmess,'(a)') 'Error in smooth:
     &              kd tree returns no triangles'
                    call writloga(cdefault,0,logmess,0,ierror)
                    go to 9999
                  endif
c
c  loop through candidate nodes find closest which
c  does not cause inversion or damage - reject candidates
c  by setting ifound(icand) to zero
c
                 ntested=0
 125             indx=0
                 distmin=1.e+30
                 do icand=1,nfound
                    do kk=1,3
                       ipoint=itets(kk,itfound(icand))
                       if(tested(ipoint).ne.1) then
                          dist1=dlen(xic(node),yic(node),zic(node),
     *                    xics(ipoint),yics(ipoint),zics(ipoint))
                          dist=dlen(xcen,ycen,zcen,
     *                    xics(ipoint),yics(ipoint),zics(ipoint))
                          if(dist1.lt.epsln) then
                             ntested=ntested+1
                             tested(ipoint)=1
                             invtested(ntested)=ipoint
                          elseif(dist.lt.distmin) then
                             distmin=dist
                             minpt=ipoint
                             indx=icand
                          endif
                       endif
                    enddo
                 enddo
                 if(indx.eq.0) then
c       print *, 'moving edge node ',node, 'to midpoint failed'
                    go to 130
                 endif
                 ntested=ntested+1
                 tested(minpt)=1
                 invtested(ntested)=minpt
c
c.... damage in discrete case is minimum of distance from
c.... node to be moved to new position or sum of
c.... projected distances from node to be moved to line between
c.... neighbors and distance from discrete point to line
c.... if min damage is sum then since we get the
c.... closest point first looking at other nodes it will
c.... not help so try next option
c.... otherwise try one of the other points
c
                 frac=((xic(neibr(2))-xic(neibr(1)))*
     &            (xics(minpt)-xic(neibr(1)))+
     &            (yic(neibr(2))-yic(neibr(1)))*
     &            (yics(minpt)-yic(neibr(1)))+
     &            (zic(neibr(2))-zic(neibr(1)))*
     &            (zics(minpt)-zic(neibr(1))))/
     &            ((xic(neibr(2))-xic(neibr(1)))*
     &            (xic(neibr(2))-xic(neibr(1)))+
     &            (yic(neibr(2))-yic(neibr(1)))*
     &            (yic(neibr(2))-yic(neibr(1)))+
     &            (zic(neibr(2))-zic(neibr(1)))*
     &            (zic(neibr(2))-zic(neibr(1))))
 
                 xproj=frac*xic(neibr(2))+(one-frac)*xic(neibr(1))
                 yproj=frac*yic(neibr(2))+(one-frac)*yic(neibr(1))
                 zproj=frac*zic(neibr(2))+(one-frac)*zic(neibr(1))
                 damagea=sqrt((xics(minpt)-xproj)**2+
     &            (yics(minpt)-yproj)**2+
     &            (zics(minpt)-zproj)**2)
                 dist=dlen(xic(node),yic(node),zic(node),
     *                xics(minpt),yics(minpt),zics(minpt))
                 damaged=min((damage+damagea),dist)
                 if (damaged.gt.toldamageused) then
                      go to 125
                 endif
c
c.... found a discrete node to move to that has acceptable damage
c
                 x1=xics(minpt)
                 y1=yics(minpt)
                 z1=zics(minpt)
                 xold=x1
                 yold=y1
                 zold=z1
               else
c
c.... regular 'non-discrete' mode
c
                 x1=xcen
                 y1=ycen
                 z1=zcen
               endif
               if (damage.le.toldamageused) then
                  xnew=x1
                  ynew=y1
                  znew=z1
                  if (lwontinvert_smooth(node,xnew,ynew,znew,
     &               nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp,iparent
     &               ,itet,itetoff,xic,yic,zic,vtol,synthx,synthy,synthz
     &               ,lsomereversed,dartol)) then
                     nsmoothed=nsmoothed+1
                     lmoved=.true.
c      print*,node,' edge node moved to midpoint'
                     goto 900
                  endif
c      print*,node,' fail edge node moved to midpoint'
               endif
c
c.... move to midpoint (or discrete node close to midpoint)
c.... would cause inversions
c.... so now try moving a portion (omega) of the
c.... distance toward a neighbor - order the two choices in increasing
c.... distance from the new point to the midpoint
c.... check for inversion
c.... if we are doing error adaption and get to this place give up
c
 130           continue
               if(validfield) go to 150
               do kk=1,ntested
                    tested(invtested(kk))=0
                    invtested(kk)=0
               enddo
               ntested=0
               if (isurftst(node).eq.0.and.
     *            omega*damage.gt.toldamageused) then
                  lmoved=.false.
                  goto 900
               endif
 
               do j=1,nnbr
                  ismooth=neibr(j)
                  xnew=xic(node)*(1.d0-omega)+xic(ismooth)*omega
                  ynew=yic(node)*(1.d0-omega)+yic(ismooth)*omega
                  znew=zic(node)*(1.d0-omega)+zic(ismooth)*omega
                  dnbr(j)=sqrt((xcen-xnew)**2+(ycen-ynew)**2+(zcen-znew)
     &               **2)
               enddo
 
               if (dnbr(1).le.dnbr(2)) then
                  nbrlist(1)=1
                  nbrlist(2)=2
               else
                  nbrlist(1)=2
                  nbrlist(2)=1
               endif
 
               do j=1,nnbr
                  nearnbrj=nbrlist(j)
                  nearnbr=neibr(nearnbrj)
                  xnew=xic(node)*(1.d0-omega)+xic(nearnbr)*omega
                  ynew=yic(node)*(1.d0-omega)+yic(nearnbr)*omega
                  znew=zic(node)*(1.d0-omega)+zic(nearnbr)*omega
                  if(isurftst(node).eq.1) then
                     call nearestpoint(xnew,ynew,znew,
     *                 xics,yics,zics,itets,
     *                 xold,yold,zold,
     *                 linkt,sbox,
     *                 epsln,work,nfound,itfound,ierror)
C
                     if (nfound .eq. 0) then
                       write(logmess,'(a)') 'Error in smooth:
     &                 kd tree returns no triangles'
                       call writloga(cdefault,0,logmess,0,ierror)
                       go to 9999
                     endif
c
c  loop through candidate nodes find closest which
c  does not cause inversion or damage
c
 
 135                 indx=0
                     distmin=1.e+30
                     do icand=1,nfound
                        do kk=1,3
                           ipoint=itets(kk,itfound(icand))
                           if(tested(ipoint).ne.1) then
                              dist1=dlen(xic(node),yic(node),zic(node),
     *                        xics(ipoint),yics(ipoint),zics(ipoint))
                              dist=dlen(xcen,ycen,zcen,
     *                        xics(ipoint),yics(ipoint),zics(ipoint))
                              if(dist1.lt.epsln) then
                                 ntested=ntested+1
                                 tested(ipoint)=1
                                 invtested(ntested)=ipoint
                              elseif(dist.lt.distmin) then
                                 distmin=dist
                                 minpt=ipoint
                                 indx=icand
                              endif
                           endif
                        enddo
                     enddo
                     if(indx.eq.0.and.idebug.gt.0) then
                       write(logmess,'(a,i10,a)')
     *                  'moving edge node partially ',
     *                 node, ' will invert tetrahedra '
                       call writloga(cdefault,0,logmess,0,ierror)
                       write(logmess,'(a)')
     *                 'or cause unacceptable damage'
                       call writloga(cdefault,0,logmess,0,ierror)
                     endif
                     if(indx.eq.0) go to 150
                     ntested=ntested+1
                     tested(minpt)=1
                     invtested(ntested)=minpt
c
c.... damage in discrete case is minimum of distance from
c.... node to be moved to new position or difference of
c.... distance from node to be moved to line between
c.... neighbors and distance from discrete point to line
c.... if min damage is sum then since we get the
c.... closest point first looking at other nodes it will
c.... not help so try next option
c.... otherwise try one of the other points
c
                     frac=((xic(neibr(2))-xic(neibr(1)))*
     &               (xics(minpt)-xic(neibr(1)))+
     &               (yic(neibr(2))-yic(neibr(1)))*
     &               (yics(minpt)-yic(neibr(1)))+
     &               (zic(neibr(2))-zic(neibr(1)))*
     &               (zics(minpt)-zic(neibr(1))))/
     &               ((xic(neibr(2))-xic(neibr(1)))*
     &               (xic(neibr(2))-xic(neibr(1)))+
     &               (yic(neibr(2))-yic(neibr(1)))*
     &               (yic(neibr(2))-yic(neibr(1)))+
     &               (zic(neibr(2))-zic(neibr(1)))*
     &               (zic(neibr(2))-zic(neibr(1))))
 
                     xproj=frac*xic(neibr(2))+(one-frac)*xic(neibr(1))
                     yproj=frac*yic(neibr(2))+(one-frac)*yic(neibr(1))
                     zproj=frac*zic(neibr(2))+(one-frac)*zic(neibr(1))
                     damagea=sqrt((xics(minpt)-xproj)**2+
     &               (yics(minpt)-yproj)**2+
     &               (zics(minpt)-zproj)**2)
 
                     damaged=min(damage+damagea,dist)
                     x1=xics(minpt)
                     y1=yics(minpt)
                     z1=zics(minpt)
                     xold=x1
                     yold=y1
                     zold=z1
                     if (damaged.gt.toldamageused) then
                       if(damaged.eq.damage+damagea) then
                          go to 150
                       else
                          go to 135
                       endif
                     endif
                  else
                     x1=xcen
                     y1=ycen
                     z1=zcen
                  endif
                  xnew=x1
                  ynew=y1
                  znew=z1
                  if (lwontinvert_smooth(node,xnew,ynew,znew,
     &               nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp,iparent
     &               ,itet,itetoff,xic,yic,zic,vtol,synthx,synthy,synthz
     &               ,lsomereversed,dartol)) then
                     nsmoothed=nsmoothed+1
                     lmoved=.true.
c      print *, 'moved edge node partially',node
                     goto 900
                  endif
 140           continue
               enddo
c
c.... get here if cannot move the node
c
 150           lmoved=.false.
 
 900           continue
               do k=1,nnbr
                  invneibr(neibr(k))=0
               enddo
               do kk=1,ntested
                    tested(invtested(kk))=0
                    invtested(kk)=0
               enddo
               ntested=0
 
c.... Treat the case where the node is a 'surface' point.
c.... mcr=1 for 3d, mcr=0 for 2d
 
            elseif ((mcr(i).eq.1.and.nsdtopo.eq.3)
     *       .or.(mcr(i).eq.0.and.nsdtopo.eq.2)) then
 
c.... Loop over elements sharing NODE and determine the
c.... area vector pointing out from material mat(1).  This
c.... area vector is constructed by taking all the triangle
c.... area vectors from interface faces sharing NODE, and
c.... adding them up.  We later normalize this vector, giving
c.... us a good average normal for the interface.
 
c.... We currently assume the interface facets are triangular...
c.... so we restrict operation to tetrahedral elements.
 
               ax=0.
               ay=0.
               az=0.
               nnbr=0
               atot=0.
               xcen=0.
               ycen=0.
               zcen=0.
               xcena=zero
               ycena=zero
               zcena=zero
               distpmin=1.d+50
               goodface=.false.
               do 110 j=nodhyboff(i)+1,nodhyboff(i+1)
 
                  ii=1+(nodhyb(j)-1)/maxnen
                  lochybnod=nodhyb(j)-maxnen*(ii-1)
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
c.... Check that current element is material type mat(1).
 
                  if (nsdtopo.eq.3) then
                     if (imt1(itet(1+itetoff(ihyb))).ne.mat(1)) goto 110
                  endif
 
                  if (ityp.ne.ifelmtet.and.ityp.ne.ifelmtri) then
                     write(logmess,'(a)')
     &                  'SGD requires tet or tri mesh ; aborting'
                     call writloga(cdefault,0,logmess,0,ierrw)
                     ierror=1
                     goto 9999
                  endif
 
c.... Loop thru faces of element with jtet >= mbndry.  Then process only
c.... those that contain LOCHYBNOD---the local node number corresponding
c.... to NODE.
 
                  if (nsdtopo.eq.2) then
                     do k1=1,nelmnen(ityp)
                        if (lochybnod.ne.k1) then
                           nodek1=iparent(itet(k1+itetoff(ihyb)))
                           if (invneibr(nodek1).eq.0) then
                              nnbr=nnbr+1
                              if (len_neibr.lt.nnbr) then
                                 inc=nnbr-len_neibr+100
                                 len_neibr=len_neibr+inc
                                 call mmincblk('neibr',isubname
     &                              ,ipneibr,inc,icscode)
                                 call mmincblk('dnbr',isubname
     &                              ,ipdnbr,inc,icscode)
                                 call mmincblk('nbrlist',isubname
     &                              ,ipnbrlist,inc,icscode)
                              endif
                              neibr(nnbr)=nodek1
                              invneibr(nodek1)=nnbr
                              xcen=xcen+xic(nodek1)
                              ycen=ycen+yic(nodek1)
                              zcen=zcen+zic(nodek1)
c$$$                              dnbr(nnbr)=sqrt((xic(nodek1)-xic(node))**2
c$$$     &                           +(yic(nodek1)-yic(node))**2+(zic(nodek1
c$$$     &                           )-zic(node))**2)
                           endif
                        endif
                     enddo
                     i2=itet(1+itetoff(ihyb))
                     i3=itet(2+itetoff(ihyb))
                     i4=itet(3+itetoff(ihyb))
 
                     a1x=dcrosx(xic(i2),yic(i2),zic(i2),xic(i3),
     &                  yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
                     a1y=dcrosy(xic(i2),yic(i2),zic(i2),xic(i3),
     &                  yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
                     a1z=dcrosz(xic(i2),yic(i2),zic(i2),xic(i3),
     &                  yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
                     ax=ax+a1x
                     ay=ay+a1y
                     az=az+a1z
                     areak=sqrt(a1x**2+a1y**2+a1z**2)
                     atot=atot+areak
                  else
                     do 120 k=1,nelmnef(ityp)
                        if (jtet(k+jtetoff(ihyb)).ge.mbndry) then
                           lvalidface=.false.
                           do k1=1,ielmface0(k,ityp)
                              if (lochybnod.eq.ielmface1(k1,k,ityp))
     &                           then
                                 lvalidface=.true.
                              endif
                           enddo
 
                           if (lvalidface) then
 
                              do k1=1,ielmface0(k,ityp)
                                 nodek1=iparent(itet(ielmface1(k1,k
     &                                 ,ityp)+itetoff(ihyb)))
                                 xplane(k1)=xic(nodek1)
                                 yplane(k1)=yic(nodek1)
                                 zplane(k1)=zic(nodek1)
                                 if (lochybnod.ne.nodek1)then
                                    if (invneibr(nodek1).eq.0) then
                                       nnbr=nnbr+1
                                       if (len_neibr.lt.nnbr) then
                                          inc=nnbr-len_neibr+100
                                          len_neibr=len_neibr+inc
                                          call mmincblk('neibr',isubname
     &                                       ,ipneibr,inc,icscode)
                                          call mmincblk('dnbr',isubname
     &                                       ,ipdnbr,inc,icscode)
                                          call mmincblk('nbrlist'
     &                                       ,isubname,ipnbrlist,inc
     &                                       ,icscode)
                                       endif
                                       neibr(nnbr)=nodek1
                                       invneibr(nodek1)=nnbr
                                       xcen=xcen+xic(nodek1)
                                       ycen=ycen+yic(nodek1)
                                       zcen=zcen+zic(nodek1)
                                    endif
                                 endif
                              enddo
c
c....  if error adaption project node onto plane of face - see if
c....  falls inside face - if so find min distance
c....
                              if (validfield) then
                                 call point_to_plane_lg(
     *                              xcenadapt,ycenadapt,
     *                              zcenadapt,xplane,
     *                              yplane,zplane,xp,yp,zp,
     *                              distp,iflag)
                                 if(iflag.eq.0) then
                                    if(distp.lt.distpmin)then
                                       distpmin=distp
                                       xcena=xp
                                       ycena=yp
                                       zcena=zp
                                       goodface=.true.
                                    endif
                                 endif
                              endif
                              i2=itet(ielmface1(1,k,ityp)+itetoff(ihyb))
                              i3=itet(ielmface1(2,k,ityp)+itetoff(ihyb))
                              i4=itet(ielmface1(3,k,ityp)+itetoff(ihyb))
 
                              a1x=dcrosx(xic(i2),yic(i2),zic(i2)
     &                           ,xic(i3),yic(i3),zic(i3),xic(i4)
     &                           ,yic(i4),zic(i4))
                              a1y=dcrosy(xic(i2),yic(i2),zic(i2)
     &                           ,xic(i3),yic(i3),zic(i3),xic(i4)
     &                           ,yic(i4),zic(i4))
                              a1z=dcrosz(xic(i2),yic(i2),zic(i2)
     &                           ,xic(i3),yic(i3),zic(i3),xic(i4)
     &                           ,yic(i4),zic(i4))
                              ax=ax+a1x
                              ay=ay+a1y
                              az=az+a1z
                              areak=sqrt(a1x**2+a1y**2+a1z**2)
                              atot=atot+areak
                           endif
                        endif
 120                 continue
                  endif
 110           continue
               if(validfield) then
                  if(.not.goodface) go to 190
                  xcenadapt=xcena
                  ycenadapt=ycena
                  zcenadapt=zcena
               endif
 
c.... Check that magnitude of area vector is not virtually zero
c.... due to cancellation.
 
               avec=sqrt(ax**2+ay**2+az**2)
               if (avec.le.atot*alg_epsilon) then
                  ax=0.
                  ay=0.
                  az=0.
               else
                  ax=ax/avec
                  ay=ay/avec
                  az=az/avec
               endif
 
c.... Project all edges onto the normalized area vector.  Also check
c.... if any of the individual area vectors are not pointing in the
c.... same direction as the overall area vector.  If they are all
c.... pointing in the same direction, we define the damage to be
c.... PROJMAX-PROJMIN which is the maximum dot product of an edge
c.... vector with the normalized area vector MINUS the minimum
c.... dot product.  In addition to edge vectors, the zero vector
c.... is projected onto the normal; that is accomplished by
c.... initializing PROJMAX and PROJMIN both to zero.  This definition
c.... of damage represents the distance between the two closest planes
c.... orthogonal to the normal vector that bracket the interface
c.... polygon.
c....    However, if some of the area vectors are pointing in
c.... a direction opposite the average normal, then it is
c.... possible that this definition of damage is incorrect.  In this
c.... case we define the damage to be the smallest distance from
c.... NODE to a neighboring interface node.
 
               projmax=zero
               projmin=zero
               do j=1,nnbr
 
c.... Project the edges emanating from NODE
c.... onto the aggregate normal.
 
                  nbr=neibr(j)
                  proj=(xic(nbr)-xic(node))*ax+
     &               (yic(nbr)-yic(node))*ay+
     &               (zic(nbr)-zic(node))*az
                  projmax=max(projmax,proj)
                  projmin=min(projmin,proj)
               enddo
 
               projdamage=projmax-projmin
 
c.... first try the center of the polygon - check for inversion
 
               xcen=xcen/nnbr
               ycen=ycen/nnbr
               zcen=zcen/nnbr
c
c.... see if error adaption is on
c
               if(validfield) then
                  xcen=xcenadapt
                  ycen=ycenadapt
                  zcen=zcenadapt
               endif
c
c.... if node is on 'discrete surface' look for nearest discrete node
               if(isurftst(node).eq.1) then
                  call nearestpoint(xcen,ycen,zcen,
     *                 xics,yics,zics,itets,
     *                 xold,yold,zold,
     *                 linkt,sbox,
     *                 epsln,work,nfound,itfound,ierror)
C
                  if (nfound .eq. 0) then
                     write(logmess,'(a)') 'Error in smooth:
     &                 kd tree returns no triangles'
                     call writloga(cdefault,0,logmess,0,ierror)
                     go to 9999
                  endif
c
c  loop through candidate nodes find closest which
c  does not cause inversion or damage
c
 165              indx=0
                  distmin=1.e+30
                  do icand=1,nfound
                     do kk=1,3
                        ipoint=itets(kk,itfound(icand))
                        if(tested(ipoint).ne.1) then
                           dist1=dlen(xic(node),yic(node),zic(node),
     *                     xics(ipoint),yics(ipoint),zics(ipoint))
                           dist1=dlen(xcen,ycen,zcen,
     *                     xics(ipoint),yics(ipoint),zics(ipoint))
                           if(dist.lt.epsln) then
                              ntested=ntested+1
                              tested(ipoint)=1
                              invtested(ntested)=ipoint
                           elseif(dist.lt.distmin) then
                              distmin=dist
                              minpt=ipoint
                              indx=icand
                           endif
                        endif
                     enddo
                  enddo
                  if(indx.eq.0.and.idebug.gt.0) then
                     write(logmess,'(a,i10,a)')
     *                'moving surface node to midpoint',
     *                node, ' will invert tetrahedra '
                     call writloga(cdefault,0,logmess,0,ierror)
                     write(logmess,'(a)')
     *                'or cause unacceptable damage'
                     call writloga(cdefault,0,logmess,0,ierror)
                  endif
                  if(indx.eq.0) go to 170
                  ntested=ntested+1
                  tested(minpt)=1
                  invtested(ntested)=minpt
c
c.... modify projdamage in discrete case to include
c.... projection from minpt
c
                  proj=(xics(minpt)-xic(node))*ax+
     &               (yics(minpt)-yic(node))*ay+
     &               (zics(minpt)-zic(node))*az
                  projmax=max(projmax,proj)
                  projmin=min(projmin,proj)
                  projdamage=projmax-projmin
                  dist=dlen(xic(node),yic(node),zic(node),
     *                xics(minpt),yics(minpt),zics(minpt))
                  damaged=min(projdamage,dist)
                  x1=xics(minpt)
                  y1=yics(minpt)
                  z1=zics(minpt)
                  xold=x1
                  yold=y1
                  zold=z1
                  if (damaged.gt.toldamageused) then
                     if(damaged.eq.projdamage) then
                        go to 170
                     else
                        go to 165
                     endif
                  endif
               else
                  x1=xcen
                  y1=ycen
                  z1=zcen
               endif
               xnew=x1
               ynew=y1
               znew=z1
 
 
               dcen=sqrt((xic(node)-xnew)**2+(yic(node)-ynew)**2
     &            +(zic(node)-znew)**2)
               if (min(dcen,projdamage).le.toldamageused) then
                  if (lwontinvert_smooth(node,xnew,ynew,znew
     &               ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &               ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &               ,synthy,synthz,lsomereversed,dartol)) then
                     nsmoothed=nsmoothed+1
                     lmoved=.true.
c      print *,'surface node moved ',node
                     goto 910
                  endif
c      print *,'fail surface node moved ',node
               endif
 
c
c.... center won't work - try moving node toward one of the
c.... vertices of the enclosing polygon - order the new
c.... proposed locations by distance to the center
c
 170           continue
               do kk=1,ntested
                    tested(invtested(kk))=0
                    invtested(kk)=0
               enddo
               ntested=0
               do j=1,nnbr
                  ismooth=neibr(j)
                  xnew=xic(node)*(1.d0-omega)+xic(ismooth)*omega
                  ynew=yic(node)*(1.d0-omega)+yic(ismooth)*omega
                  znew=zic(node)*(1.d0-omega)+zic(ismooth)*omega
                  dnbr(j)=sqrt((xcen-xnew)**2+(ycen-ynew)**2+(zcen-znew)
     &               **2)
               enddo
 
               do j=1,nnbr
                  nbrlist(j)=j
               enddo
               ascend=1.
               call hpsort1(nnbr,dnbr,ascend,nbrlist)
c
c.... try the new locations (omega of the distance toward a
c.... polygon vertex - check for inversion and damage
c
               do j=1,nnbr
                  nearnbrj=nbrlist(j)
                  nearnbr=neibr(nearnbrj)
                  dnearnbr=sqrt((xic(node)-xic(nearnbr))**2+(yic(node)
     &               -yic(nearnbr))**2+(zic(node)-zic(nearnbr))**2)
                  damage=min(omega*dnearnbr,omega*projdamage)
                  xnew=xic(node)*(1.d0-omega)+xic(nearnbr)*omega
                  ynew=yic(node)*(1.d0-omega)+yic(nearnbr)*omega
                  znew=zic(node)*(1.d0-omega)+zic(nearnbr)*omega
                  if(isurftst(node).eq.1) then
                     call nearestpoint(xnew,ynew,znew,
     *                 xics,yics,zics,itets,
     *                 xold,yold,zold,
     *                 linkt,sbox,
     *                 epsln,work,nfound,itfound,ierror)
C
                     if (nfound .eq. 0) then
                       write(logmess,'(a)') 'Error in smooth:
     &                 kd tree returns no triangles'
                       call writloga(cdefault,0,logmess,0,ierror)
                       go to 9999
                     endif
c
c  loop through candidate nodes find closest which
c  does not cause inversion or damage
c
 185                 indx=0
                     distmin=1.e+30
                     do icand=1,nfound
                        do kk=1,3
                           ipoint=itets(kk,itfound(icand))
                           if(tested(ipoint).ne.1) then
                              dist1=dlen(xic(node),yic(node),zic(node),
     *                        xics(ipoint),yics(ipoint),zics(ipoint))
                              dist=dlen(xnew,ynew,znew,
     *                        xics(ipoint),yics(ipoint),zics(ipoint))
                              if(dist1.lt.epsln) then
                                 ntested=ntested+1
                                 tested(ipoint)=1
                                 invtested(ntested)=ipoint
                              elseif(dist.lt.distmin) then
                                 distmin=dist
                                 minpt=ipoint
                                 indx=icand
                              endif
                           endif
                        enddo
                     enddo
                     if(indx.eq.0.and.idebug.gt.0) then
                        write(logmess,'(a,i10,a)')
     *                  'moving surface node partway ',
     *                  node, ' will invert tetrahedra '
                        call writloga(cdefault,0,logmess,0,ierror)
                        write(logmess,'(a)')
     *                  'or cause unacceptable damage'
                        call writloga(cdefault,0,logmess,0,ierror)
                     endif
                     if(indx.eq.0) go to 190
                     ntested=ntested+1
                     tested(minpt)=1
                     invtested(ntested)=minpt
c
c.... modify projdamage in discrete case to include
c.... projection from minpt
c
                     proj=(xics(minpt)-xic(node))*ax+
     &                 (yics(minpt)-yic(node))*ay+
     &                 (zics(minpt)-zic(node))*az
                     projmax=max(projmax,proj)
                     projmin=min(projmin,proj)
                     projdamage=projmax-projmin
                     dist=dlen(xic(node),yic(node),zic(node),
     *                xics(minpt),yics(minpt),zics(minpt))
                     damage=min(projdamage,dist)
                     x1=xics(minpt)
                     y1=yics(minpt)
                     z1=zics(minpt)
                     xold=x1
                     yold=y1
                     zold=z1
                  else
                     x1=xnew
                     y1=ynew
                     z1=znew
                  endif
                  xnew=x1
                  ynew=y1
                  znew=z1
 
                  if (lwontinvert_smooth(node,xnew,ynew,znew
     &               ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &               ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &               ,synthy,synthz,lsomereversed,dartol)) then
                     if (damage.le.toldamageused) then
                        nsmoothed=nsmoothed+1
                        lmoved=.true.
c         print *, 'moving surface node partially first test ',node
                        goto 910
                     else
c         print *, 'fail moving surface node partially 1st test ',node
                        if(isurftst(node).eq.1) then
                          damage=min(damage,damage_est_discrete
     *                     (node,xnew,ynew,znew,
     &                      nodhyb(nodhyboff(i)+1),ieltary
     &                     ,nelt,itettyp,iparent,itet,itetoff,jtet
     &                     ,jtetoff,mbndry,xic,yic,zic,
     &                     xics,yics,zics,itets,itfound,linkt,
     &                     sbox,epsln,work))
                          if (damage.le.toldamageused) then
                            nsmoothed=nsmoothed+1
                            lmoved=.true.
c         print *, 'moving surface node partially 2nd test ',node
                            goto 910
                          endif
                        else
                          damage=min(damage,omega*damage_est_2(node
     &                     ,nearnbr,nodhyb(nodhyboff(i)+1),ieltary
     &                     ,nelt,itettyp,iparent,itet,itetoff,jtet
     &                     ,jtetoff,mbndry,xic,yic,zic,.false.))
                          if (damage.le.toldamageused) then
                            nsmoothed=nsmoothed+1
                            lmoved=.true.
c         print *, 'moving surface node partially 2nd test ',node
                            goto 910
                          endif
                        endif
c         print *, 'fail moving surface node partially 2nd test ',node
                     endif
                  endif
               enddo
c
c.... get here if can't move node
 190           lmoved=.false.
c       print *,node,' failed all attempts - inversion error'
 
 910           continue
               do k=1,nnbr
                  invneibr(neibr(k))=0
               enddo
               do kk=1,ntested
                    tested(invtested(kk))=0
                    invtested(kk)=0
               enddo
               ntested=0
c.... In the MCR=0 case where the node is a 'volume' point, we
c.... decree that DAMAGE is zero.  We still have to loop thru all
c.... the elements to find the nearest neighbor.
 
            elseif (mcr(i).eq.0) then
 
c.... We now loop thru the elements associated with NODE
c.... and we work out the center of mass (CMX,CMY,CMZ)
 
               nnbr=0
               xcen=0.
               ycen=0.
               zcen=0.
 
c.... Here we must make the assumption that our elements
c.... are tetrahedra; that's because we need element
c.... volume formulas for the CM calculation which we
c.... only bother supplying for tets.
 
               do j=nodhyboff(i)+1,nodhyboff(i+1)
                  ii=1+(nodhyb(j)-1)/maxnen
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
                  if (ityp.ne.ifelmtet.and.ityp.ne.ifelmtri) then
                     write(logmess,'(a)')
     &                'SGD requires all tet or tri mesh ; aborting'
                     call writloga(cdefault,0,logmess,0,ierrw)
                     ierror=1
                     goto 9999
                  endif
 
                  do k=1,nelmnen(ityp)
                     nodek=iparent(itet(k+itetoff(ihyb)))
                     if (nodek.ne.node) then
                        if (invneibr(nodek).eq.0) then
                           nnbr=nnbr+1
                           if (len_neibr.lt.nnbr) then
                              inc=nnbr-len_neibr+100
                              len_neibr=len_neibr+inc
                              call mmincblk('neibr',isubname
     &                           ,ipneibr,inc,icscode)
                              call mmincblk('dnbr',isubname
     &                           ,ipdnbr,inc,icscode)
                              call mmincblk('nbrlist',isubname
     &                           ,ipnbrlist,inc,icscode)
                           endif
                           neibr(nnbr)=nodek
                           invneibr(nodek)=nnbr
                           xcen=xcen+xic(nodek)
                           ycen=ycen+yic(nodek)
                           zcen=zcen+zic(nodek)
                        endif
                     endif
                  enddo
               enddo
 
c.... check for inversion
 
               xcen=xcen/nnbr
               ycen=ycen/nnbr
               zcen=zcen/nnbr
 
               xnew=xcen
               ynew=ycen
               znew=zcen
               if (lwontinvert_smooth(node,xnew,ynew,znew
     &            ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &            ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &            ,synthy,synthz,lsomereversed,dartol)) then
                  nsmoothed=nsmoothed+1
                  lmoved=.true.
                  goto 920
               endif
 
c
c.... center of mass won't work - try moving node toward
c.... one of the vertices of the enclosing polyhedron.
c.... order candidates by distance to centroid
c
               do j=1,nnbr
                  ismooth=neibr(j)
                  xnew=xic(node)*(1.d0-omega)+xic(ismooth)*omega
                  ynew=yic(node)*(1.d0-omega)+yic(ismooth)*omega
                  znew=zic(node)*(1.d0-omega)+zic(ismooth)*omega
                  dnbr(j)=sqrt((xcen-xnew)**2+(ycen-ynew)**2+(zcen-znew)
     &               **2)
               enddo
 
               do j=1,nnbr
                  nbrlist(j)=j
               enddo
               ascend=1.
               call hpsort1(nnbr,dnbr,ascend,nbrlist)
 
               do j=1,nnbr
                  nearnbrj=nbrlist(j)
                  nearnbr=neibr(nearnbrj)
                  xnew=xic(node)*(1.d0-omega)+xic(nearnbr)*omega
                  ynew=yic(node)*(1.d0-omega)+yic(nearnbr)*omega
                  znew=zic(node)*(1.d0-omega)+zic(nearnbr)*omega
                  if (lwontinvert_smooth(node,xnew,ynew,znew
     &               ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &               ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &               ,synthy,synthz,lsomereversed,dartol)) then
                     nsmoothed=nsmoothed+1
                     lmoved=.true.
                     goto 920
                  endif
               enddo
c
c.... get here if can't move a node
               lmoved=.false.
 
 920           continue
               do k=1,nnbr
                  invneibr(neibr(k))=0
               enddo
 
c.... If MCR>2, we don't try to annihilate the node.
 
            else
 
               lmoved=.false.
 
            endif
 
            if (lmoved) then

               xic(node)=xnew
               yic(node)=ynew
               zic(node)=znew
               if (isn1(node).ne.0) then
                  nextnode=isn1(node)
                  do while (nextnode.ne.node)
                     xic(nextnode)=xnew
                     yic(nextnode)=ynew
                     zic(nextnode)=znew
                     nextnode=isn1(nextnode)
                  enddo
               endif
c
c.... If a node has moved mark all other nodes
c.... in elements containing this node as stale
c.... (i.e. do not try to move those nodes)
c
               do j=nodhyboff(i)+1,nodhyboff(i+1)
                  ii=1+(nodhyb(j)-1)/maxnen
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
                  do k=1,nelmnen(ityp)
                     nod=iparent(itet(itetoff(ihyb)+k))
                     mpk=invmpary(nod)
                     if (mpk.ne.0) then
                        lstale(mpk)=.true.
                     endif
                  enddo
               enddo
            else
               lstale(i)=.false.
            endif
 
 10      continue
 
         msmoothed=msmoothed+nsmoothed
            
         if (nsmoothed.eq.0) goto 100
 
      enddo
 
      write(logmess,'(a,i6,a)') 'SGD: ',maxsweep,
     &   ' sweeps (hit max. limit)'
      call writloga(cwarn,0,logmess,0,ierrw)
 
c$$$      write(logmess,'(a,d16.8)') 'toldamageused=',toldamageused
c$$$      call writloga('default',0,logmess,0,ierrw)
c$$$
c$$$      write(logmess,'(a,a)') 'cmo=',cmo
c$$$      call writloga('default',0,logmess,0,ierrw)
c$$$
c$$$      call dotaskx3d('dump x3d x3d.sgd.noterminate ; finish',ierr)
c$$$      call dotaskx3d('dump gmv gmv.sgd.noterminate ; finish',ierr)
 
 100  continue
 
      write(logmess,'(a,i6,a,i6,a)') 'SGD: ',msmoothed,
     &   ' total relaxations on point set of ',mpno,' indept. nodes.'
      call writloga(cdefault,0,logmess,0,ierrw)
      if (validfield) call polyfun(5,action,node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,itetclr,itet,itetoff,xic,yic,zic,
     &   nnodes,nelements,iedge,iedgeoff,iedgemat,ichildary,
     &   ichildno,invchildary,imt1,epsilonv,fvec,reffield,iparent,
     &   hxx,hxy,hxz,hyy,hyz,hzz,range,pf,pfx,pfxx)
 
 9999 continue
      call mmrelprt(isubname,icscode)
c
c
c
c
      return
      end
 
      subroutine polyar1(nelt,ielts,itet,itetoff,xic,yic,
     &         zic,nsdtopo,worstvol,darl2,worstar)
 
      implicit none
 
      include 'consts.h'
 
      integer nelt,ielts(*),itet(*),itetoff(*),nsdtopo
      real*8 xic(*),yic(*),zic(*),darl2,worstar,worstvol,vol
 
      integer i,i1,i2,i3,i4
      real*8 dar,epsilonaspect
 
      include 'statementfunctions.h'
 
      darl2=0.
      worstar=1.d99
      worstvol=1.d99
 
      if (nsdtopo.eq.2) then
         do i=1,nelt
            i1=itet(1+itetoff(ielts(i)))
            i2=itet(2+itetoff(ielts(i)))
            i3=itet(3+itetoff(ielts(i)))
c$$$            call aratio_tri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &         ,zic(i2),xic(i3),yic(i3),zic(i3),dar)
            dar=dirtri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &         ,zic(i2),xic(i3),yic(i3),zic(i3))
            darl2=darl2+1./dar**2
            worstar=min(worstar,dar)
         enddo
      else
         epsilonaspect=1.d-30
         do i=1,nelt
            i1=itet(1+itetoff(ielts(i)))
            i2=itet(2+itetoff(ielts(i)))
            i3=itet(3+itetoff(ielts(i)))
            i4=itet(4+itetoff(ielts(i)))
c$$$            call aratio_tet(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &         ,zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4)
c$$$     &         ,dar,epsilonaspect)
            dar=dirtet(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &         ,zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            vol=dvol(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &         ,zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            darl2=darl2+1./dar**2
            worstar=min(worstar,dar)
            worstvol=min(worstvol,vol)
         enddo
      endif
      return
      end
 
      function lwontinvert_smooth(node,xnew,ynew,znew,nodhyb,ieltary
     &   ,nelt,itettyp,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &   ,synthy,synthz,lsomereversed,dartol)
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      integer node,nodhyb(*),ieltary(*),nelt,itettyp(*),
     &   iparent(*),itet(*),itetoff(*),j,ii,ihyb,ityp,
     &   j1,nbr
      real*8 xic(*),yic(*),zic(*),vtol,cros(3),vol,dartol,dar,
     &   dot,synthx,synthy,synthz,x(4),y(4),z(4),xnew,ynew,znew,
     &   darhere
      logical lwontinvert_smooth,lsomereversed
 
      real*8 epsilonaspect
 
      include 'statementfunctions.h'
 
      lwontinvert_smooth=.true.
 
      darhere=1.d99
 
      do 10 j=1,nelt
         ii=1+(nodhyb(j)-1)/maxnen
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)
 
         do j1=1,nelmnen(ityp)
            nbr=iparent(itet(j1+itetoff(ihyb)))
            if (nbr.eq.node) then
               x(j1)=xnew
               y(j1)=ynew
               z(j1)=znew
            else
               x(j1)=xic(nbr)
               y(j1)=yic(nbr)
               z(j1)=zic(nbr)
            endif
         enddo
 
         if(ityp.eq.ifelmtet) then
            epsilonaspect=1.d-30
            dar=dirtet(x(1),y(1),z(1),x(2),y(2)
     &         ,z(2),x(3),y(3),z(3),x(4),y(4),z(4))
            vol=dvol(x(1),y(1),z(1),x(2),y(2),z(2)
     &         ,x(3),y(3),z(3),x(4),y(4),z(4))
            if (vol.le.vtol) then
               lwontinvert_smooth=.false.
               return
            endif
            darhere=min(darhere,dar)
 
         elseif(ityp.eq.ifelmtri) then
            dar=dirtri(x(1),y(1),z(1),x(2),y(2)
     &         ,z(2),x(3),y(3),z(3))
            darhere=min(darhere,dar)
 
            if (.not.lsomereversed) then
               cros(1)=dcrosx(x(1),y(1),z(1),x(2),y(2)
     &            ,z(2),x(3),y(3),z(3))
               cros(2)=dcrosy(x(1),y(1),z(1),x(2),y(2)
     &            ,z(2),x(3),y(3),z(3))
               cros(3)=dcrosz(x(1),y(1),z(1),x(2),y(2)
     &            ,z(2),x(3),y(3),z(3))
               dot=synthx*cros(1)+synthy*cros(2)+synthz*cros(3)
               if(dot.le.0.0) then
                  lwontinvert_smooth=.false.
                  return
               endif
            endif
 
         endif
  10  continue
      if (darhere.le.dartol) then
         lwontinvert_smooth=.false.
         return
      endif
 
      return
 
      end
