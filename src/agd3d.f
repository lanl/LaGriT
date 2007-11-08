*dk,agd3d
      subroutine agd3d(cmo,toldamage,tollength,mpary_in,mpno_in
     &   ,lstrictmergelength,lignoremats,lcheckaxy,lcheckroughness
     &   ,tol_roughness_in,ierror)
c
c #####################################################################
c
c    PURPOSE
c
c       AGD3D ("Annihilate on Graph Damage in 3D")
c     takes a 3D mesh object and annihilates nodes that
c     (i) are in the list of selected mass points,
c     (ii) would cause a 'graph damage' of no greater than TOLDAMAGE if
c          they are removed, and
c     (iii) are within TOLLENGTH of a suitable neighboring node which
c           they can merge in to.
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
c       LSTRICTMERGELENGTH - interpret TOLLENGTH strictly if true
c       LIGNOREMATS - treat as uniform material if true
c       LCHECKAXY - check projected area in xy plane is > EPSILONA
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
c
c $Log: agd3d.f,v $
c Revision 2.00  2007/11/05 19:45:46  spchu
c Import to CVS
c
CPVCS    
CPVCS       Rev 1.41   29 Apr 2002 13:56:10   dcg
CPVCS    fix unassigned index error (nbr)
CPVCS
CPVCS       Rev 1.40   14 Mar 2002 10:56:18   dcg
CPVCS    remove overflow past column 72
CPVCS
CPVCS       Rev 1.39   01 Mar 2002 14:44:18   dcg
CPVCS    adaptive merging
CPVCS
CPVCS       Rev 1.38   31 Jan 2002 13:14:40   dcg
CPVCS    move damage functions to testdamage
CPVCS
CPVCS       Rev 1.37   16 Jan 2002 14:27:26   dcg
CPVCS    add function damamge_est_discrete
CPVCS
CPVCS       Rev 1.37   14 Jan 2002 17:20:00   dcg
CPVCS    add function damage_est_discrete
CPVCS
CPVCS       Rev 1.36   31 May 2001 14:27:38   kuprat
CPVCS    Fixed argument list for POLYIR.
CPVCS
CPVCS       Rev 1.35   29 May 2001 17:58:00   kuprat
CPVCS    We now refrain from merging out nodes if they would create
CPVCS    a roughness > 0.8*TOLROUGHNESS.
CPVCS
CPVCS       Rev 1.34   14 May 2001 08:44:06   kuprat
CPVCS    Put in CHECKAXY option.
CPVCS
CPVCS       Rev 1.33   04 May 2001 14:18:46   kuprat
CPVCS    Changed TOLAR to TOLIR, WORSTAR to WORSTIR, etc., reflecting the
CPVCS    change that the criterion is now inscribed radius, not aspect ratio.
CPVCS
CPVCS       Rev 1.32   07 Jul 2000 15:51:26   kuprat
CPVCS    We now use an LSTALE array to prevent us from recomputing whether a node
CPVCS    should be merged in a nbd where nothing has been changed.
CPVCS
CPVCS       Rev 1.31   01 Mar 2000 14:56:52   kuprat
CPVCS    We now can process tets with zero or negative inscribed radii
CPVCS    and we try to get their inscribed radii to be positive.
CPVCS    If LSTRICTMERGELENGTH is .true., we do not allow some merges of
CPVCS    edges of length greater than TOLLENGTH that we would have allowed
CPVCS    due to the presence of elements with small inscribed radii.
CPVCS    IF LIGNOREMATS is .true., we process the grid as if there was
CPVCS    only one material type.
CPVCS
CPVCS       Rev 1.30   Thu Nov 18 18:18:06 1999   kuprat
CPVCS    We now reduce damage allowed if inscribed radius is large, and
CPVCS    only use full user damage tolerance if inscribed radius is small
CPVCS    (and we want to try everything to increase it).  Similarly, we
CPVCS    now all inscribed radius to go down during annihilation UNLESS
CPVCS    inscribed radius is already very small (and we want to prevent
CPVCS    it from getting smaller).
CPVCS
CPVCS       Rev 1.29   Tue Oct 12 08:21:46 1999   kuprat
CPVCS    We now annihilate, paying attention to incribed radius maximization,
CPVCS    rather than aspect ratio optimization.
CPVCS
CPVCS       Rev 1.28   Thu Aug 26 13:46:24 1999   kuprat
CPVCS    Changed output mode from 'default' to 'bat' for curve
CPVCS    neighbor warnings due to excessive warns.
CPVCS
CPVCS       Rev 1.27   Fri Jan 15 13:38:26 1999   kuprat
CPVCS    Changed 'Error' to 'Stern Warning' in the case of no
CPVCS    common material type; we now all execution to continue.
CPVCS
CPVCS       Rev 1.26   Wed Jan 13 15:57:42 1999   kuprat
CPVCS    Now use SAFE in denominators in DAMAGE_EST_2.
CPVCS
CPVCS       Rev 1.25   Mon Jan 11 11:00:00 1999   kuprat
CPVCS    Changed definition of TOLLENGTH so that it is now the
CPVCS    merge length.
CPVCS
CPVCS       Rev 1.24   Mon Dec 21 13:46:08 1998   kuprat
CPVCS    We now allow merging of 'weird' curve points (i.e. those not having
CPVCS    unique successors and predecessors) to connected curve points, as
CPVCS    long as the movement required is less than TOLDAMAGE.
CPVCS
CPVCS       Rev 1.23   Wed Nov 04 11:14:16 1998   dcg
CPVCS    fix never used warnings
CPVCS
CPVCS       Rev 1.22   Wed Nov 04 02:42:08 1998   kuprat
CPVCS    Slightly changed method for calculating number of effective
CPVCS    constraints, MCR, for a node.
CPVCS
CPVCS       Rev 1.21   Fri Oct 30 15:25:14 1998   kuprat
CPVCS    We now look at the aspect ratio situation BEFORE merging
CPVCS    (via a call to POLYAR) and look for relative improvements
CPVCS    caused by potential merges.
CPVCS
CPVCS       Rev 1.20   Fri Oct 23 16:25:10 1998   kuprat
CPVCS    We now call SYNTHNORMAL to compute a synthetic normal at NODE in
CPVCS    the case of triangulated surfaces.  This allows us to pre-reject
CPVCS    node merges that would have been rejected by MERGEPTS_SIMPLEX anyway.
CPVCS    We now test that we don't created tets or triangles with bad
CPVCS    aspect ratios, and we now more aggressively try to eliminate
CPVCS    elements with small inscribed radii.
CPVCS
CPVCS       Rev 1.19   Tue Sep 15 12:55:02 1998   dcg
CPVCS    add last argument in calls to lwontinvert
CPVCS
CPVCS       Rev 1.18   Fri Aug 21 16:46:32 1998   dcg
CPVCS    make changes to allow for 2d massage
CPVCS
CPVCS       Rev 1.17   Fri Jul 31 18:19:24 1998   kuprat
CPVCS    Loosened merge criterion by allowing low-projection-
CPVCS    distance merges.
CPVCS
CPVCS       Rev 1.16   Fri Jun 19 09:40:00 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.15   Wed Jun 17 11:54:50 1998   dcg
CPVCS    move write statement about number of nodes merged
CPVCS
CPVCS       Rev 1.14   Wed Jun 10 16:56:24 1998   dcg
CPVCS    add print statement of number of nodes merged
CPVCS
CPVCS       Rev 1.13   Wed Jun 03 08:45:28 1998   dcg
CPVCS    correct loop index nelmnee should be nelmnen
CPVCS    call mergepts_simplex
CPVCS
CPVCS       Rev 1.12   Sat May 23 23:46:40 1998   kuprat
CPVCS    Now merge points with faster MERGEPTS_TET.
CPVCS
CPVCS       Rev 1.11   Sat May 23 20:33:18 1998   kuprat
CPVCS    Pass TOLDAMAGE on to RECON now.
CPVCS
CPVCS       Rev 1.10   Tue May 05 16:46:46 1998   kuprat
CPVCS    Put in 'ridge' damage estimate.
CPVCS
CPVCS       Rev 1.9   Thu Apr 30 19:41:22 1998   kuprat
CPVCS    Fixed bug where LISNBR wasn't always restored to 'all .false.'
CPVCS
CPVCS       Rev 1.8   Thu Apr 30 15:13:24 1998   kuprat
CPVCS    We now rank neighbors to a given node in order of distance, precheck
CPVCS    their viability for annihilation, and then continue down the list
CPVCS    until we find one that works.  Documentation is now out of date...
CPVCS
CPVCS       Rev 1.7   Tue Apr 28 16:26:48 1998   kuprat
CPVCS    Got rid of center-of-mass 'worstlength' estimate.  Estimate was
CPVCS    too pessimistic.  New estimate based on knowledge of actual
CPVCS    pair of nodes to be merged.
CPVCS
CPVCS       Rev 1.6   Fri Apr 10 16:53:16 1998   kuprat
CPVCS    No change.
c
c #####################################################################
 
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'massage.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      character*132 logmess,cbuf
      pointer (ipimt1, imt1),(ipitp1, itp1),(ipisn1, isn1)
      pointer (ipicr1, icr1)
      pointer (ipxic, xic),(ipyic, yic),(ipzic, zic)
      pointer (ipitetclr, itetclr)
      integer itetclr(lenptr)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff),(ipitettyp, itettyp)
      integer imt1(lenptr)
      pointer (ipicontab, icontab)
      integer icontab(50,lenptr), itp1(lenptr),isn1(lenptr)
      integer icr1(lenptr)
      real*8 xic(lenptr),yic(lenptr),zic(lenptr)
      integer itet(lenptr),itetoff(lenptr),itettyp(lenptr)
      pointer (ipjtet,jtet),(ipjtetoff,jtetoff)
      integer jtet(lenptr),jtetoff(lenptr)
      pointer (iphxx,hxx),(iphxy,hxy),(iphxz,hxz),(iphyy,hyy),
     &  (iphyz,hyz ),(iphzz,hzz)
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*)
 
      pointer (ipnodhyb,nodhyb),(ipnodhyboff,nodhyboff)
      integer nodhyb(lenptr),nodhyboff(lenptr)
      pointer (ipieltary,ieltary)
      integer ieltary(lenptr)
      pointer (ipireal1,ireal1),(ipinvmpary,invmpary),
     &   (ipichildary,ichildary),(ipinvchildary,invchildary),
     &   (ipiparent,iparent),(ipmpary,mpary),(ipirealold,irealold)
      integer invmpary(lenptr),ichildary(lenptr),invchildary(lenptr),
     &   ireal1(lenptr),iparent(lenptr),mpary(lenptr),irealold(lenptr)
      pointer (ipelts,elts),(ipedges,edges)
      integer elts(lenptr),edges(lenptr)
      pointer (ipmcr,mcr),(ipmat,mat),
     &   (ipnodelist,nodelist),
     &   (ipnearestnbr,nearestnbr),(ipworst_length,worst_length),
     &   (iplockout,lockout),(ipinvneibr,invneibr),(ipneibr,neibr),
     &   (ipdnbr,dnbr),(ipnbrlist,nbrlist),(ipimerge,imerge),
     &   (ipdnbrproj,dnbrproj),(ipielts,ielts),(iplstale,lstale)
      integer mcr(lenptr),mat(lenptr),nodelist(lenptr),
     &   nearestnbr(lenptr),neibr(lenptr),nbrlist(lenptr),
     &   imerge(2,lenptr),ielts(lenptr)
      real*8 worst_length(lenptr),areak,dnbr(lenptr),
     &   dnbrproj(lenptr)
      logical lockout(lenptr),lstale(lenptr)
      integer invneibr(lenptr)
 
      integer maxlenstr
      parameter (maxlenstr=4095)
 
      character*8  cdefault
      character*32 cmo,isubname,cdata
 
      integer mpary_in(lenptr),mpno_in,
     &   ierror,nnodes,length,icmotype,nelements,mbndry,icscode,
     &   ieltno,i,j,node,nod1,icr,nmat,ii,lochybnod,ihyb,
     &   ityp,k,locnbr,nbr,nnbr,ioff,ie1,ie2,nelts,
     &   nearnbr,k1,i2,i3,i4,minmat,maxmat,j1,kk,
     &   nanni,ianni,ierr,ierrw,mpno,mpno_old,ierrdum,
     &   ichildno,nod,len_ieltary,len_mat,inc,nummat,
     &   merge_lives,merge_dies,nsdtopo,jj,
     &   len_neibr,nodek,i0,nelt,nearnbrj,
     &   nmrg,nodek1,nnbrk,nnbrk1,nnbr1,ip1,ip2,
     &   nef_cmo,ioppnod,len_ielts,ifromitp,ifromicr,ntrip,mtrip,idata
 
      real*8 toldamage,tollength,dnearnbr,frac,damage,xproj,
     &   yproj,zproj,a1x,a1y,a1z,ax,ay,az,atot,avec,alg_epsilon,
     &   projmin,projmax,proj,ascend,
     &   eps,vtol,projdamage,damage_est_2,
     &   xk1,yk1,zk1,elen,
     &   xk2,yk2,zk2,xk3,yk3,zk3,
     &   dprojnearnbr,xk4,yk4,zk4,dir,dirtol,worstir,
     &   synthx,synthy,synthz,toldamageused,dltol,tolroughness
     &   ,tol_roughness_in,epsilonv_save,epsilona,toldamage_in
      parameter (alg_epsilon=1.d-10)
      logical lsame,lvalidface,lwontinvert,ltripedge,lsomereversed,
     &   lstrictmergelength,lignoremats,lcheckaxy,lcheckroughness
 
      real*8 tolcutfactor,tolimprovefactor_up,tolimprovefactor_down
      parameter (tolcutfactor=0.25,tolimprovefactor_up=1.000001
     &   ,tolimprovefactor_down=0.5)
 
      pointer (ipgsynth,gsynth)
      real*8 gsynth(3,*)
      integer len_gsynth
 
      include 'statementfunctions.h'
 
      isubname='agd3d'
 
      len_gsynth=0
      cdefault='default'
      ierror=0
      toldamage_in=toldamage
 
      tolroughness=0.8*tol_roughness_in
 
      call get_epsilon('epsilonl',dltol)
      call get_epsilon('epsilonv',epsilonv_save)
      call get_epsilon('epsilona',epsilona)
      vtol=-1.d99
      call cmo_set_attinfo('epsilonv',cmo,idata,vtol,cdata,2,icscode)
 
c.... Allocate memory for arrays that cannot grow in size.  (There
c.... are a couple of arrays that have to be allocated within the
c.... following loop, since they could grow in size.)
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
 
      call mmgetblk('ireal1',isubname,ipireal1,nnodes,1,icscode)
      call mmgetblk('irealold',isubname,ipirealold,nnodes,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
      call mmgetblk('ichildary',isubname,ipichildary,nnodes,1,icscode)
      call mmgetblk('invchildary',isubname,ipinvchildary,nnodes,1,
     &   icscode)
      call mmgetblk('mpary',isubname,ipmpary,mpno_in,1,icscode)
      call mmgetblk('mcr',isubname,ipmcr,mpno_in,1,icscode)
      call mmgetblk('nodelist',isubname,ipnodelist,mpno_in,1,icscode)
      call mmgetblk('lockout',isubname,iplockout,nnodes,1,icscode)
      call mmgetblk('lstale',isubname,iplstale,nnodes,1,icscode)
      call mmgetblk('nearestnbr',isubname,ipnearestnbr,
     &   mpno_in,1,icscode)
      call mmgetblk('worst_length',isubname,ipworst_length,
     &   mpno_in,2,icscode)
      call mmgetblk('invneibr',isubname,ipinvneibr,
     &   nnodes,1,icscode)
      call mmgetblk('imerge',isubname,ipimerge,
     &   2*nnodes,1,icscode)
      call mmgetblk('elts',isubname,ipelts,100,1,icscode)
      call mmgetblk('edges',isubname,ipedges,100,1,icscode)
 
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
 
c.... Initialize IREALOLD to 2.  Eventually, when each element of
c.... IREAL is equal to each element of IREALOLD, then the outer
c.... annihilation loop is not dudding out anymore points and we
c.... break out.  The initialization to 2 guarantees that the
c.... arrays won't match to begin with.
 
      do i=1,nnodes
         irealold(i)=2
      enddo
 
      do i=1,nnodes
         lockout(i)=.false.
         lstale(i)=.true.
      enddo
 
c.... Outer iteration loop for annihilating nodes.  On each iteration of
c.... the outer loop, a set of nodes that do not interfere with each
c.... other is annihilated.  Since this changes the mesh object, each
c.... outer loop iteration includes getting fresh pointers for the
c.... mesh object and recalculating the relevant geometric quantities.
 
      do while (mpno.gt.0)
 
c.... Get info from mesh object.
 
         call cmo_get_info('nnodes',cmo,
     *      nnodes,length,icmotype,ierror)
         call cmo_get_info('nelements',cmo,
     *      nelements,length,icmotype,ierror)
         call cmo_get_info('ndimensions_topo',cmo,
     *      nsdtopo,length,icmotype,ierror)
         call cmo_get_info('mbndry',cmo,
     *      mbndry,length,icmotype,ierror)
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
         call cmo_get_info('faces_per_element',cmo,nef_cmo,
     &      length,icmotype,ierror)
         if(isafield) then
c.... issue SETHESSIAN command to generate necessary 2nd derivatives.
 
            write(cbuf,'(3a)') 'sethessian/',adaption_field_name,
     &        '/ ; finish'
            call dotask(cbuf,ierror)
            if (ierror.ne.0) then
               write(logmess,'(a)')
     &         'CEE_CHAIN: error computing Hessian'
               call writloga('default',0,logmess,0,ierrw)
               goto 9999
            endif
            call cmo_get_info('hxx',cmo,iphxx,length,icmotype,ierror)
            call cmo_get_info('hxy',cmo,iphxy,length,icmotype,ierror)
            call cmo_get_info('hxz',cmo,iphxz,length,icmotype,ierror)
            call cmo_get_info('hyy',cmo,iphyy,length,icmotype,ierror)
            call cmo_get_info('hyz',cmo,iphyz,length,icmotype,ierror)
            call cmo_get_info('hzz',cmo,iphzz,length,icmotype,ierror)
         endif
 
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
         call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
         if(ierrdum.ne.0) call x3d_error('agd3d', 'unpacktp')
c
c
c
c
         lsame=.true.
         do i=1,nnodes
            if (ireal1(i).ne.irealold(i)) lsame=.false.
         enddo
 
         if (lsame) goto 9999
 
         do i=1,nnodes
            irealold(i)=ireal1(i)
         enddo
 
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
         do 40 i=1,nelements
            do j=1,nelmnen(itettyp(i))
               if (invchildary(itet(j+itetoff(i))).ne.0) then
                  ieltno=ieltno+1
                  ieltary(ieltno)=i
                  minmat=min(minmat,imt1(itet(1+itetoff(i))))
                  maxmat=max(maxmat,imt1(itet(1+itetoff(i))))
                  goto 40
               endif
            enddo
 40      continue
 
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
            call mmgetblk('dnbrproj',isubname,ipdnbrproj,len_neibr,
     &         2,icscode)
            call mmgetblk('nbrlist',isubname,ipnbrlist,len_neibr,1
     &         ,icscode)
         endif
         if (len_ielts.eq.0) then
            len_ielts=100
            call mmgetblk('ielts',isubname,ipielts,len_ielts,1,icscode)
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
c....     consider the damage of merging it to its neighbor to be zero.
c.... If MCR=1, the point lies on an interface or geometrical constraint
c....     and it can only be merged to interface neighbors.
c....     A nonzero amount of damage may occur which is usually defined
c to be
c....     an estimate of the 'fatness' of the polygon formed by joining
c....     the node to all its interface neighbors.
c.... If MCR=2, the point lies on an interfacial curve and it can
c....     only be merged to either one of its curve neighbors.  The
c....     damage incurred by this merge is usually equal to the height
c of the
c....     triangle formed by the node and its two curve neighbors.
c.... If MCR>=3, the point is critical in defining the geometry or
c....     interface.  The damage caused by merging this point into
c....     another point would thus be equal to the distance to the
c neighboring
c....     point.  Currently, this routine does not allow such a merge.
 
         do i=1,mpno
            mcr(i)=0
            node=mpary(i)
 
            if (.not.lignoremats) then
               if (itp1(node).eq.ifitpcup) then
                  nod1=isn1(node)
                  do while (nod1.ne.node.and.nod1.ne.0)
                     mcr(i)=mcr(i)+1
                     nod1=isn1(nod1)
                  enddo
                  mcr(i)=mcr(i)-1
               endif
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
     &               then
                     ifromitp=1
                  endif
                  nod1=isn1(nod1)
               enddo
            elseif (itp1(node).ge.ifitpst2.and.itp1(node).le.ifitpen2)
     &         then
               ifromitp=1
            endif
 
            if (ifromitp.eq.1) then
               mcr(i)=mcr(i)+max(1,ifromicr)
            endif
 
         enddo
 
c.... For the surface case, if LCHECKROUGHNESS is true,
c.... compute a synthetic normal at all points.  Then
c.... potentially new edges will be projected onto the
c.... synthetic normals at the endpoints.  For efficiency
c.... of computation, we keep and do not update the synthetic
c.... normal values inside of a single sweep.
 
         if ((nsdtopo.eq.2).and.lcheckroughness) then
            call getgsynth(eps,isubname,ieltary,ieltno,iparent,itet
     &         ,itetoff,invmpary,mpno,xic,yic,zic,ipgsynth,len_gsynth)
         endif
 
c.... Loop over all mass points for potential inclusion into
c.... annihilation list.
 
         do i=1,mpno
            if (lstale(mpary(i))) then
               lockout(mpary(i))=.false.
            else
               lockout(mpary(i))=.true.
            endif
         enddo
         nanni=0
         do 10 i0=1,mpno
            call primestep(mpno,i)
            node=mpary(i)
            if (lockout(node)) goto 10
            nelt=nodhyboff(i+1)-nodhyboff(i)
 
c... Get the materials and constraints that NODE participates in.
 
            if (.not.lignoremats) then
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
            endif
 
c.... In the case of triangular grids, compute a synthetic normal at
c.... NODE to be used to determine the orientation of triangles
c.... incident upon NODE.
 
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
 
            if (nsdtopo.eq.2) then
               call synthnormal(node,nelt,ielts,iparent,itet,
     &            itetoff,xic,yic,zic,eps,synthx,synthy,synthz
     &            ,lsomereversed)
            endif
 
            call polyir(nelt,ielts,itet,itetoff,xic,yic,
     &         zic,nsdtopo,lcheckaxy,worstir)
            toldamageused=toldamage_in
            if(isafield) then
               if(toldamage_4d.gt.zero) toldamageused=toldamage_4d
            endif
            if (worstir.ge.toldamageused) then
               toldamageused=toldamageused*tolcutfactor
               dirtol=worstir*tolimprovefactor_down
            elseif (worstir.ge.dltol) then
               dirtol=worstir*tolimprovefactor_up
            elseif (worstir.ge.-dltol) then
               dirtol=-2*dltol
            else
               dirtol=2*worstir
            endif
 
c.... Treat the MCR=2 case where the node is a 'curve' point.
 
            if ((mcr(i).eq.2.and.nsdtopo.eq.3).or.
     *          (mcr(i).eq.1.and.nsdtopo.eq.2)) then
 
c.... Loop over elements sharing node and determine the
c.... (two) curve neighbors.
 
               nnbr=0
               do j=nodhyboff(i)+1,nodhyboff(i+1)
                  ii=1+(nodhyb(j)-1)/maxnen
                  lochybnod=nodhyb(j)-maxnen*(ii-1)
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
                  xk1=xic(iparent(itet(1+itetoff(ihyb))))
                  yk1=yic(iparent(itet(1+itetoff(ihyb))))
                  zk1=zic(iparent(itet(1+itetoff(ihyb))))
 
                  xk2=xic(iparent(itet(2+itetoff(ihyb))))
                  yk2=yic(iparent(itet(2+itetoff(ihyb))))
                  zk2=zic(iparent(itet(2+itetoff(ihyb))))
 
                  xk3=xic(iparent(itet(3+itetoff(ihyb))))
                  yk3=yic(iparent(itet(3+itetoff(ihyb))))
                  zk3=zic(iparent(itet(3+itetoff(ihyb))))
 
                  if (nsdtopo.eq.2) then
                     dir=dirtri(xk1,yk1,zk1,xk2,yk2,zk2,xk3,yk3,zk3)
                  else
                     xk4=xic(iparent(itet(4+itetoff(ihyb))))
                     yk4=yic(iparent(itet(4+itetoff(ihyb))))
                     zk4=zic(iparent(itet(4+itetoff(ihyb))))
 
                     dir=dirtet(xk1,yk1,zk1,xk2,yk2,zk2,xk3,yk3,zk3,
     &                  xk4,yk4,zk4)
                  endif
 
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
     &                        ,nef_cmo,icr1,icontab,lignoremats)) goto
     &                        20
                        endif
 
                        nnbr=nnbr+1
                        if (len_neibr.lt.nnbr) then
                           inc=nnbr-len_neibr+100
                           len_neibr=len_neibr+inc
                           call mmincblk('neibr',isubname
     &                        ,ipneibr,inc,icscode)
                           call mmincblk('dnbr',isubname
     &                        ,ipdnbr,inc,icscode)
                           call mmincblk('dnbrproj',isubname
     &                        ,ipdnbrproj,inc,icscode)
                           call mmincblk('nbrlist',isubname
     &                        ,ipnbrlist,inc,icscode)
                        endif
                        neibr(nnbr)=nbr
                        invneibr(nbr)=nnbr
                        if(isafield) then
c
c... get the 4d distance which includes the edge error
c... find an element to which this edge belongs
c... use that to find all elements surrounding the edge
c... if any edge emanating from node has error > toldamage_4d
c... do not merge this node
c
                           do kk=1,nelmnee(ityp)
                              ie1=ielmedge1(1,kk,ityp)
                              ie2=ielmedge1(2,kk,ityp)
                              ioff=itetoff(ihyb)
                              ip1=iparent(itet(ie1+ioff))
                              ip2=iparent(itet(ie2+ioff))
                              if ((nbr.eq.ip1.and.
     &                           node.eq.ip2).or.
     &                           (nbr.eq.ip2.and.
     &                           node.eq.ip1)) then
                                 call get_elements_on_edge(ihyb,kk,
     *                             nelts,ipelts,
     *                             ipedges,ipitetoff,ipjtetoff,
     *                             ipitet,ipjtet,
     *                             ipitettyp,ipiparent,nef_cmo,mbndry)
                                 call edgefun_lg(nelts,ipelts,ipedges,
     &                             itettyp,itet,itetoff,xic,yic,zic,
     &                             hxx,hxy,hxz,hyy,hyz,hzz,elen)
                                 if(elen.gt.toldamage_4d) then
                                   do jj=1,nnbr
                                      nbr=neibr(jj)
                                      invneibr(nbr)=0
                                   enddo
                                   go to 10
                                 endif
                                 dnbr(nnbr)=sqrt((
     &                             xic(nbr)-xic(node))**2+
     &                            (yic(nbr)-yic(node))**2+
     &                            (zic(nbr)-zic(node))**2+elen**2)
                                 go to 19
                              endif
                           enddo
                        else
                           dnbr(nnbr)=sqrt((xic(nbr)-xic(node))**2+
     &                      (yic(nbr)-yic(node))**2+
     &                      (zic(nbr)-zic(node))**2)
                        endif
19                      dnbrproj(nnbr)=1.d99
 
                     endif
 
                     nnbr1=invneibr(nbr)
                     dnbrproj(nnbr1)=min(dnbrproj(nnbr1),dir)
 
 20               continue
 
               enddo
 
c.... Check that we have exactly two curve neighbors.
 
               if (nnbr.ne.2) then
                  write(logmess,'(a,i10,a,i10)')
     &               'Warning:  ',nnbr,' curve neighbors at ',node
                  call writloga('bat',0,logmess,0,ierrw)
                  if (nnbr.eq.0) goto 900
               endif
 
c.... Check damage that annihilating NODE would cause.  We project NODE
c.... orthogonally onto the segment between neibr(1) and neibr(2).
c.... We calculate FRAC, where
c....     XPROJ=(1-FRAC)*XIC(NEIBR(1))+FRAC*XIC(NEIBR(2))
c.... Here XPROJ is the x-coordinate of the orthogonal projection of
c.... NODE onto the segment between NEIBR(1) and NEIBR(2).  A similar
c.... statement holds for the y- and z- coordinates.
c.... If 0<=FRAC<=1, we have that the DAMAGE would be equal to the
c.... distance of the orthogonal projection.  That is, the distance
c.... between NODE and (XPROJ,YPROJ,ZPROJ).  In a previous version, we
c.... had that if FRAC<0, the damage was equal to the distance between NODE
c.... and NEIBR(1) and if FRAC>1, the damage was equal to the distance
c.....between NODE and NEIBR(2).  We now simply take the distance
c.... between NODE and (XPROJ,YPROJ,ZPROJ), because in these ``folded
c.... -over'' cases, we want to encourage node annihilation.
 
               if (nnbr.eq.2) then
 
                  frac=((xic(neibr(2))-xic(neibr(1)))*
     &               (xic(node)-xic(neibr(1)))+
     &               (yic(neibr(2))-yic(neibr(1)))*
     &               (yic(node)-yic(neibr(1)))+
     &               (zic(neibr(2))-zic(neibr(1)))*
     &               (zic(node)-zic(neibr(1))))/
     &               safe(((xic(neibr(2))-xic(neibr(1)))*
     &               (xic(neibr(2))-xic(neibr(1)))+
     &               (yic(neibr(2))-yic(neibr(1)))*
     &               (yic(neibr(2))-yic(neibr(1)))+
     &               (zic(neibr(2))-zic(neibr(1)))*
     &               (zic(neibr(2))-zic(neibr(1)))))
 
                  xproj=frac*xic(neibr(2))+(one-frac)*xic(neibr(1))
                  yproj=frac*yic(neibr(2))+(one-frac)*yic(neibr(1))
                  zproj=frac*zic(neibr(2))+(one-frac)*zic(neibr(1))
                  projdamage=sqrt((xic(node)-xproj)**2+
     &               (yic(node)-yproj)**2+
     &               (zic(node)-zproj)**2)
                  if (projdamage.gt.toldamageused) goto 900
 
               else
 
                  projdamage=1.d99
 
               endif
 
               do j=1,nnbr
                  nbrlist(j)=j
               enddo
               ascend=1.
               call hpsort1(nnbr,dnbr,ascend,nbrlist)
 
               do j=1,nnbr
                  nearnbrj=nbrlist(j)
                  nearnbr=neibr(nearnbrj)
                  dnearnbr=dnbr(nearnbrj)
                  dprojnearnbr=dnbrproj(nearnbrj)
                  if (projdamage.le.toldamageused.or.
     &               dnearnbr.le.toldamageused) then
                     if (dnearnbr.le.tollength.or.
     &                  ((.not.lstrictmergelength).and.(dprojnearnbr.le
     &                  .tollength/10.))) then
                        if (lwontinvert(i,node,invmpary,nearnbr
     &                     ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &                     ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &                     ,synthy,synthz,lsomereversed,lcheckaxy
     &                     ,epsilona,lcheckroughness,tolroughness,gsynth
     &                     ,dirtol))then
                           nanni=nanni+1
                           nodelist(nanni)=i
                           nearestnbr(i)=nearnbr
                           worst_length(i)=dnearnbr
                           goto 900
                        endif
                     endif
                  endif
               enddo
 
 900           continue
               do k=1,nnbr
                  invneibr(neibr(k))=0
               enddo
 
c.... Treat the MCR=1 case where the node is a 'surface' point.
 
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
               do 110 j=nodhyboff(i)+1,nodhyboff(i+1)
 
                  ii=1+(nodhyb(j)-1)/maxnen
                  lochybnod=nodhyb(j)-maxnen*(ii-1)
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
c.... Check that current element is material type mat(1).
 
                  if (nsdtopo.eq.3) then
                     if (.not.lignoremats) then
                        if (imt1(itet(1+itetoff(ihyb))).ne.mat(1)) goto
     &                     110
                     endif
                  endif
 
                  if (ityp.ne.ifelmtet.and.ityp.ne.ifelmtri) then
                     write(logmess,'(a)')
     &                  'AGD3D requires tet or tri mesh ; aborting'
                     call writloga('default',0,logmess,0,ierrw)
                     ierror=1
                     goto 9999
                  endif
 
c.... Loop thru faces of element with jtet >= mbndry.  Then process only
c.... those that contain LOCHYBNOD---the local node number corresponding
c.... to NODE.
                  xk1=xic(iparent(itet(1+itetoff(ihyb))))
                  yk1=yic(iparent(itet(1+itetoff(ihyb))))
                  zk1=zic(iparent(itet(1+itetoff(ihyb))))
 
                  xk2=xic(iparent(itet(2+itetoff(ihyb))))
                  yk2=yic(iparent(itet(2+itetoff(ihyb))))
                  zk2=zic(iparent(itet(2+itetoff(ihyb))))
 
                  xk3=xic(iparent(itet(3+itetoff(ihyb))))
                  yk3=yic(iparent(itet(3+itetoff(ihyb))))
                  zk3=zic(iparent(itet(3+itetoff(ihyb))))
 
 
                  if (nsdtopo.eq.2) then
                     dir=dirtri(xk1,yk1,zk1,xk2,yk2,zk2,xk3,yk3,zk3)
                  else
                     xk4=xic(iparent(itet(4+itetoff(ihyb))))
                     yk4=yic(iparent(itet(4+itetoff(ihyb))))
                     zk4=zic(iparent(itet(4+itetoff(ihyb))))
 
                     dir=dirtet(xk1,yk1,zk1,xk2,yk2,zk2,xk3,yk3,zk3,
     &                  xk4,yk4,zk4)
                  endif
 
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
                                 call mmincblk('dnbrproj',isubname
     &                              ,ipdnbrproj,inc,icscode)
                                 call mmincblk('nbrlist',isubname
     &                              ,ipnbrlist,inc,icscode)
                              endif
                              neibr(nnbr)=nodek1
                              invneibr(nodek1)=nnbr
                              if(isafield) then
c
c... get the 4d distance which includes the edge error
c... find an element to which this edge belongs
c... use that to find all elements surrounding the edge
c
                                 do kk=1,nelmnee(ityp)
                                   ie1=ielmedge1(1,kk,ityp)
                                   ie2=ielmedge1(2,kk,ityp)
                                   ioff=itetoff(ihyb)
                                   ip1=iparent(itet(ie1+ioff))
                                   ip2=iparent(itet(ie2+ioff))
                                   if ((nodek1.eq.ip1.and.
     &                               node.eq.ip2).or.
     &                               (nodek1.eq.ip2.and.
     &                               node.eq.ip1)) then
                                     call get_elements_on_edge(ihyb,
     *                                kk,nelts,ipelts,
     *                                ipedges,ipitetoff,ipjtetoff,
     *                                ipitet,ipjtet,ipitettyp,
     *                                ipiparent,nef_cmo,mbndry)
                                     call edgefun_lg(nelts,ipelts,
     &                                ipedges,itettyp,itet,itetoff,
     &                                xic,yic,zic,
     &                                hxx,hxy,hxz,hyy,hyz,hzz,elen)
                                     if(elen.gt.toldamage_4d) then
                                       do jj=1,nnbr
                                          nbr=neibr(jj)
                                          invneibr(nbr)=0
                                       enddo
                                       go to 10
                                     endif
                                     nbr=neibr(nnbr)
                                     dnbr(nnbr)=sqrt((
     &                                 xic(nbr)-xic(node))**2+
     &                                (yic(nbr)-yic(node))**2+
     &                                (zic(nbr)-zic(node))**2+elen**2)
                                     go to 39
                                   endif
                                 enddo
                              else
                                dnbr(nnbr)=sqrt(
     *                                    (xic(nodek1)-xic(node))**2
     &                           +(yic(nodek1)-yic(node))**2+(zic(nodek1
     &                           )-zic(node))**2)
                              endif
 39                           dnbrproj(nnbr)=1.d99
                           endif
                           nnbrk1=invneibr(nodek1)
                           dnbrproj(nnbrk1)=min(dnbrproj(nnbrk1),dir)
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
                        if ((lignoremats.and.(jtet(k+jtetoff(ihyb)).eq
     &                     .mbndry)).or.((.not.lignoremats).and.(jtet(k
     &                     +jtetoff(ihyb)).ge.mbndry))) then
                           lvalidface=.false.
                           do k1=1,ielmface0(k,ityp)
                              if (lochybnod.eq.ielmface1(k1,k,ityp))
     &                           then
                                 lvalidface=.true.
                              endif
                           enddo
 
                           if (lvalidface) then
 
                              do k1=1,ielmface0(k,ityp)
                                 if (lochybnod.ne.ielmface1(k1,k,ityp)
     &                              )then
                                    nodek1=iparent(itet(ielmface1(k1,k
     &                                 ,ityp)+itetoff(ihyb)))
                                    if (invneibr(nodek1).eq.0) then
                                      nnbr=nnbr+1
                                      if (len_neibr.lt.nnbr) then
                                          inc=nnbr-len_neibr+100
                                          len_neibr=len_neibr+inc
                                          call mmincblk('neibr',isubname
     &                                       ,ipneibr,inc,icscode)
                                          call mmincblk('dnbr',isubname
     &                                       ,ipdnbr,inc,icscode)
                                          call mmincblk('dnbrproj'
     &                                       ,isubname,ipdnbrproj,inc
     &                                       ,icscode)
                                          call mmincblk('nbrlist'
     &                                       ,isubname,ipnbrlist,inc
     &                                       ,icscode)
                                      endif
                                      neibr(nnbr)=nodek1
                                      invneibr(nodek1)=nnbr
                                      if(isafield) then
c
c... get the 4d distance which includes the edge error
c... find an element to which this edge belongs
c... use that to find all elements surrounding the edge
c
                                       do kk=1,nelmnee(ityp)
                                        ie1=ielmedge1(1,kk,ityp)
                                        ie2=ielmedge1(2,kk,ityp)
                                        ioff=itetoff(ihyb)
                                        ip1=iparent(itet(ie1+ioff))
                                        ip2=iparent(itet(ie2+ioff))
                                        if ((nodek1.eq.ip1.and.
     &                                   node.eq.ip2).or.
     &                                   (nodek1.eq.ip2.and.
     &                                   node.eq.ip1)) then
                                         call get_elements_on_edge(ihyb,
     *                                    kk,nelts,ipelts,
     *                                    ipedges,ipitetoff,ipjtetoff,
     *                                    ipitet,ipjtet,ipitettyp,
     *                                    ipiparent,nef_cmo,mbndry)
                                         call edgefun_lg(nelts,ipelts,
     &                                    ipedges,itettyp,
     &                                    itet,itetoff,xic,yic,zic,
     &                                    hxx,hxy,hxz,hyy,hyz,hzz,elen)
                                         if(elen.gt.toldamage_4d) then
                                            do jj=1,nnbr
                                              nbr=neibr(jj)
                                              invneibr(nbr)=0
                                            enddo
                                            go to 10
                                         endif
                                         nbr=neibr(nnbr)
                                         dnbr(nnbr)=sqrt((
     &                                    xic(nbr)-xic(node))**2+
     &                                    (yic(nbr)-yic(node))**2+
     &                                    (zic(nbr)-zic(node))**2+
     &                                    elen**2)
                                         go to 49
                                        endif
                                       enddo
                                      else
                                       dnbr(nnbr)=sqrt((xic(nodek1)-
     &                                    xic(node))**2+
     &                                    (yic(nodek1)-yic(node))**2
     &                                    +(zic(nodek1)-zic(node))**2)
                                      endif
                                      dnbrproj(nnbr)=1.d99
  49                                endif
                                    nnbrk1=invneibr(nodek1)
                                    dnbrproj(nnbrk1)=
     &                                 min(dnbrproj(nnbrk1),dir)
                                 endif
                              enddo
 
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
 
               projmax=0.d0
               projmin=0.d0
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
 
c.... We now sort the neighbors in increasing order
c.... using WORST_LENGTH as a key.  Thus, we will try to merge
c.... NODE to the nearest feasible neighbor.
 
c$$$               do j=1,nnbr
c$$$                  print*,' dproj(',j,')=',dnbrproj(j)
c$$$               enddo
 
               do j=1,nnbr
                  nbrlist(j)=j
               enddo
               ascend=1.
               call hpsort1(nnbr,dnbr,ascend,nbrlist)
 
               do j=1,nnbr
                  nearnbrj=nbrlist(j)
                  nearnbr=neibr(nearnbrj)
                  dnearnbr=dnbr(nearnbrj)
                  damage=min(dnearnbr,projdamage)
                  dprojnearnbr=dnbrproj(nearnbrj)
                  if (dnearnbr.le.tollength.or.
     &               ((.not.lstrictmergelength).and.(dprojnearnbr.le
     &               .tollength/10.))) then
                     if (lwontinvert(i,node,invmpary,nearnbr
     &                  ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &                  ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &                  ,synthy,synthz,lsomereversed,lcheckaxy,epsilona
     &                  ,lcheckroughness,tolroughness,gsynth,dirtol))
     &                  then
                        if (damage.le.toldamageused) then
                           nanni=nanni+1
                           nodelist(nanni)=i
                           nearestnbr(i)=nearnbr
                           worst_length(i)=dnearnbr
                           goto 910
                        else
                           damage=min(damage,damage_est_2(node,nearnbr,
     &                        nodhyb(nodhyboff(i)+1),ieltary,nelt,
     &                        itettyp,iparent,itet,itetoff,jtet,jtetoff,
     &                        mbndry,xic,yic,zic,lignoremats))
                           if (damage.le.toldamageused) then
                              nanni=nanni+1
                              nodelist(nanni)=i
                              nearestnbr(i)=nearnbr
                              worst_length(i)=dnearnbr
                              goto 910
                           endif
                        endif
                     endif
                  endif
               enddo
 910           continue
               do k=1,nnbr
                  invneibr(neibr(k))=0
               enddo
 
c.... In the MCR=0 case where the node is a 'volume' point, we
c.... decree that DAMAGE is zero.  We still have to loop thru all
c.... the elements to find the nearest neighbor.
 
            elseif (mcr(i).eq.0) then
 
c.... We now loop thru the elements associated with NODE
c.... and we work out the center of mass (CMX,CMY,CMZ)
 
               nnbr=0
 
c.... Here we must make the assumption that our elements
c.... are tetrahedra; that's because we need element
c.... volume formulas for the CM calculation which we
c.... only bother supplying for tets.
 
               do j=nodhyboff(i)+1,nodhyboff(i+1)
                  ii=1+(nodhyb(j)-1)/maxnen
                  lochybnod=nodhyb(j)-maxnen*(ii-1)
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
                  xk1=xic(iparent(itet(1+itetoff(ihyb))))
                  yk1=yic(iparent(itet(1+itetoff(ihyb))))
                  zk1=zic(iparent(itet(1+itetoff(ihyb))))
 
                  xk2=xic(iparent(itet(2+itetoff(ihyb))))
                  yk2=yic(iparent(itet(2+itetoff(ihyb))))
                  zk2=zic(iparent(itet(2+itetoff(ihyb))))
 
                  xk3=xic(iparent(itet(3+itetoff(ihyb))))
                  yk3=yic(iparent(itet(3+itetoff(ihyb))))
                  zk3=zic(iparent(itet(3+itetoff(ihyb))))
 
                  xk4=xic(iparent(itet(4+itetoff(ihyb))))
                  yk4=yic(iparent(itet(4+itetoff(ihyb))))
                  zk4=zic(iparent(itet(4+itetoff(ihyb))))
 
                  dir=dirtet(xk1,yk1,zk1,xk2,yk2,zk2,xk3,yk3,zk3,
     &               xk4,yk4,zk4)
 
                  if (ityp.ne.ifelmtet.and.ityp.ne.ifelmtri) then
                     write(logmess,'(a)')
     &                'AGD3D requires all tet or tri mesh ; aborting'
                     call writloga('default',0,logmess,0,ierrw)
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
                              call mmincblk('dnbrproj',isubname
     &                           ,ipdnbrproj,inc,icscode)
                              call mmincblk('nbrlist',isubname
     &                           ,ipnbrlist,inc,icscode)
                           endif
                           neibr(nnbr)=nodek
                           invneibr(nodek)=nnbr
                           if(isafield) then
c
c... get the 4d distance which includes the edge error
c... find an element to which this edge belongs
c... use that to find all elements surrounding the edge
c
                            do kk=1,nelmnee(ityp)
                              ie1=ielmedge1(1,kk,ityp)
                              ie2=ielmedge1(2,kk,ityp)
                              ioff=itetoff(ihyb)
                              ip1=iparent(itet(ie1+ioff))
                              ip2=iparent(itet(ie2+ioff))
                              if ((nodek.eq.ip1.and.
     &                           node.eq.ip2).or.
     &                           (nodek.eq.ip2.and.
     &                           node.eq.ip1)) then
                                 call get_elements_on_edge(ihyb,kk,
     *                             nelts,ipelts,
     *                             ipedges,ipitetoff,ipjtetoff,
     *                             ipitet,ipjtet,
     *                             ipitettyp,ipiparent,nef_cmo,mbndry)
                                 call edgefun_lg(nelts,ipelts,ipedges,
     &                             itettyp,itet,itetoff,xic,yic,zic,
     &                             hxx,hxy,hxz,hyy,hyz,hzz,elen)
                                 if(elen.gt.toldamage_4d) then
                                    do jj=1,nnbr
                                       nbr=neibr(jj)
                                       invneibr(nbr)=0
                                    enddo
                                    go to 10
                                 endif
                                 nbr=neibr(nnbr)
                                 dnbr(nnbr)=sqrt((
     &                             xic(nbr)-xic(node))**2+
     &                            (yic(nbr)-yic(node))**2+
     &                            (zic(nbr)-zic(node))**2+elen**2)
                                 go to 59
                              endif
                            enddo
                           else
                            dnbr(nnbr)=sqrt((xic(nodek)-xic(node))**2+
     &                        (yic(nodek)-yic(node))**2+
     &                        (zic(nodek)-zic(node))**2)
c$$$                                    print*,'node=',node,';mcr=',mcr(i),
c$$$     &                                 'init d(',nnbr,')=',dnbr(nnbr)
                           endif
 59                        dnbrproj(nnbr)=1.d99
                        endif
                        nnbrk=invneibr(nodek)
                        dnbrproj(nnbrk)=min(dnbrproj(nnbrk),dir)
                     endif
                  enddo
               enddo
 
c.... We now sort the nodes in the annihilation list in increasing order
c.... using WORST_LENGTH as a key.  Thus, we will try to annihilate
c.... at first nodes that will produce the shortest edges in the wake
c.... of their annihilation.
 
c$$$               do j=1,nnbr
c$$$                  print*,'dproj(',j,')=',dnbrproj(j)
c$$$               enddo
 
               do j=1,nnbr
                  nbrlist(j)=j
               enddo
               ascend=1.
               call hpsort1(nnbr,dnbr,ascend,nbrlist)
 
               do j=1,nnbr
                  nearnbrj=nbrlist(j)
                  nearnbr=neibr(nearnbrj)
                  dnearnbr=dnbr(nearnbrj)
                  dprojnearnbr=dnbrproj(nearnbrj)
                  if (dnearnbr.le.tollength.or.
     &               ((.not.lstrictmergelength).and.(dprojnearnbr.le
     &               .tollength/10.))) then
                     if (lwontinvert(i,node,invmpary,nearnbr
     &                  ,nodhyb(nodhyboff(i)+1),ieltary,nelt,itettyp
     &                  ,iparent,itet,itetoff,xic,yic,zic,vtol,synthx
     &                  ,synthy,synthz,lsomereversed,lcheckaxy,epsilona
     &                  ,lcheckroughness,tolroughness,gsynth,dirtol))
     &                  then
                        nanni=nanni+1
                        nodelist(nanni)=i
                        nearestnbr(i)=nearnbr
                        worst_length(i)=dnearnbr
                        goto 920
                     endif
                  endif
               enddo
 920           continue
               do k=1,nnbr
                  invneibr(neibr(k))=0
               enddo
 
c.... If MCR>2, we don't try to annihilate the node.
 
            else
 
            endif
 
            if (nanni.gt.0.and.nodelist(max(1,nanni)).eq.i) then
               do j=nodhyboff(i)+1,nodhyboff(i+1)
                  ii=1+(nodhyb(j)-1)/maxnen
                  lochybnod=nodhyb(j)-maxnen*(ii-1)
                  ihyb=ieltary(ii)
                  ityp=itettyp(ihyb)
 
                  do j1=1,nelmnen(ityp)
                     nbr=iparent(itet(j1+itetoff(ihyb)))
                     lstale(nbr)=.true.
                     lockout(nbr)=.true.
                  enddo
               enddo
            else
               lstale(mpary(i))=.false.
            endif
 
 10      continue
 
         if (nanni.eq.0) goto 9999
 
         nmrg=0
 
            do 600 ianni=1,nanni
               i=nodelist(ianni)
               node=mpary(i)
 
 
 
c.... Since MERGEPTS_TET doesn't accept parent type nodes, we
c.... must find representative children for the parents.
 
               if (itp1(node).eq.ifitpcup) then
                  merge_dies=isn1(node)
               else
                  merge_dies=node
               endif
               ntrip=1
               do while (((itp1(node).eq.ifitpcup).and.(ntrip.lt.10000)
     &            .and.(merge_dies.ne.node)).or.
     &            ((itp1(node).ne.ifitpcup).and.(ntrip.eq.1)))
 
                  if (itp1(nearestnbr(i)).eq.ifitpcup) then
                     merge_lives=isn1(nearestnbr(i))
                  else
                     merge_lives=nearestnbr(i)
                  endif
                  mtrip=1
                  do while (((itp1(nearestnbr(i)).eq.ifitpcup).and
     &               .(mtrip.lt.10000).and.(merge_lives.ne.nearestnbr(i)
     &               )).or.
     &               ((itp1(nearestnbr(i)).ne.ifitpcup).and.(mtrip.eq.1)
     &                  ))
 
                     if (imt1(merge_lives).eq.imt1(merge_dies)) then
                        nmrg=nmrg+1
                        imerge(1,nmrg)=merge_lives
                        imerge(2,nmrg)=merge_dies
                        goto 600
                     endif
 
                     merge_lives=isn1(merge_lives)
                     mtrip=mtrip+1
                  enddo
 
                  merge_dies=isn1(merge_dies)
                  ntrip=ntrip+1
               enddo
 
               write(logmess,'(2a)')
     &            'Stern Warning:  merge_lives doesn''t contain ',
     &            'material of merge_dies'
               call writloga('default',0,logmess,0,ierrw)
 
 600        continue
 
            do i=1,nnodes
               if (invneibr(i).ne.0) then
                  print*,'error!!!'
                  print*,'i=',i
                  call termcode()
               endif
            enddo
 
c.... If the following condition is true, nodes were written to the
c.... command buffer CBUF and we execute the merge command using CBUF.
c.... We then break out of the loop and go to the beginning of the
c.... subroutine and refresh pointers and look for more nodes to
c.... merge.  If the following condition is false, there is no
c.... node to merge and we exit.
 
            if (nmrg.gt.0) then
               call mergepts_simplex(imerge,nmrg,cmo,ierr)
               write(logmess,'(a,i8,a)')'Merged ',nmrg,' nodes.'
               call writloga(cdefault,0,logmess,0,ierr)
               goto 8000
            else
               goto 9999
            endif
 
 8000    continue
      enddo
 
 9999 continue
 
      call mmrelprt(isubname,icscode)
 
      call cmo_set_attinfo('epsilonv',cmo,idata,epsilonv_save,cdata
     &   ,2,icscode)
 
      return
      end
 
      subroutine polyir(nelt,ielts,itet,itetoff,xic,yic,
     &         zic,nsdtopo,lcheckaxy,worstir)
 
      implicit none
 
      include 'consts.h'
 
      logical lcheckaxy
      integer nelt,ielts(*),itet(*),itetoff(*),nsdtopo
      real*8 xic(*),yic(*),zic(*),worstir
 
      integer i,i1,i2,i3,i4
      real*8 dir,epsilonaspect
 
      include 'statementfunctions.h'
 
      worstir=1.d99
 
      if (nsdtopo.eq.2) then
         do i=1,nelt
            i1=itet(1+itetoff(ielts(i)))
            i2=itet(2+itetoff(ielts(i)))
            i3=itet(3+itetoff(ielts(i)))
c$$$            call aratio_tri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &         ,zic(i2),xic(i3),yic(i3),zic(i3),dar)
            if (lcheckaxy) then
               dir=dirtri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &            ,zic(i2),xic(i3),yic(i3),zic(i3))
               worstir=min(worstir,dir)
            else
               dir=dszirtri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &            ,zic(i2),xic(i3),yic(i3),zic(i3))
               worstir=min(worstir,dir)
            endif
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
            dir=dirtet(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &         ,zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            worstir=min(worstir,dir)
         enddo
      endif
      return
      end
 
