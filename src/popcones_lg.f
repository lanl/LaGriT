*dk,popcones_lg
      subroutine popcones_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        POPCONES_LG assesses the correctness of the topology in the
C        neighborhood of points and edges in a LaGriT mesh
C        ("tangent cone topology") and fixes the topology if
C        it is incorrect ("pops the cones").
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: popcones_lg.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   07 Jan 2002 13:50:56   dcg
CPVCS     add error return argument to refine_edge_add_tet call
CPVCS
CPVCS       Rev 1.5   Wed Nov 10 14:44:10 1999   dcg
CPVCS    make xmin,xmax,ymim,ymax,zmin,zmax local variables
CPVCS
CPVCS       Rev 1.4   Mon Jul 12 13:57:32 1999   kuprat
CPVCS    Put rmpoint/compress within iteration loop.
CPVCS
CPVCS       Rev 1.3   Fri Jun 18 16:24:02 1999   kuprat
CPVCS    Corrected error where protected components were assigned
CPVCS    component numbers.
CPVCS
CPVCS       Rev 1.2   Fri Apr 30 15:47:30 1999   kuprat
CPVCS    We now print out bad tang. cones whether or not 'list' is
CPVCS     given as an argument, but only try to correct them if
CPVCS    'list' is not given.  I.e. 'list' means 'only list'.
CPVCS
CPVCS       Rev 1.1   Thu Apr 15 17:02:46 1999   kuprat
CPVCS    Corrected distance of candidate point to edge.
CPVCS
CPVCS       Rev 1.0   Sun Apr 11 23:28:54 1999   kuprat
CPVCS    Initial revision.
C
C #####################################################################
c
c   FORMAT:
c
c   POPCONES / CUT_LENGTH / ifirst,ilast,istride / [list]
c
************************************************************************
 
      implicit none
 
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
 
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
      integer imt1(*)
      pointer (ipicontab, icontab)
      integer icontab(50,*)
      integer itp1(*)
      integer isn1(*)
      integer icr1(*)
      real*8 xic(*)
      real*8 yic(*)
      real*8 zic(*)
      integer itetclr(*)
      integer itet(*)
      integer itetoff(*)
      integer itettyp(*)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(*),jtetoff(*)
 
      pointer (ipisurfelt,isurfelt)
      integer isurfelt(*),len_isurfelt
 
      pointer (ipiedges_first_,iedges_first_)
      integer iedges_first_(*),len_iedges_first_
 
      pointer (ipiedges_first,iedges_first)
      integer iedges_first(*),len_iedges_first
 
      pointer (iplink,link)
      integer link(*),len_link
 
      pointer (ipiedges_,iedges_)
      integer iedges_(*),len_iedges_
 
      pointer (ipiedges,iedges)
      integer iedges(*),len_iedges
 
      pointer (ipiedgisurfelt,iedgisurfelt)
      integer iedgisurfelt(2,*),len_iedgisurfelt
 
      pointer (ipnodhyb,nodhyb)
      integer nodhyb(*),len_nodhyb
 
      pointer (ipnodhyboff,nodhyboff)
      integer nodhyboff(*),len_nodhyboff
 
      pointer (ipieltary,ieltary)
      integer ieltary(*),len_ieltary
 
      pointer (ipinvieltary,invieltary)
      integer invieltary(*),len_invieltary
 
      pointer (ipmpary,mpary)
      integer mpary(*),len_mpary
 
      pointer (ipiparent,iparent)
      integer iparent(*),len_iparent
 
      pointer (ipinvmpary,invmpary)
      integer invmpary(*),len_invmpary
 
      pointer (ipireal1,ireal1)
      integer ireal1(*),len_ireal1
 
      pointer (ipmatary,matary)
      integer matary(*),len_matary
 
      pointer (ipinvmatary,invmatary)
      integer invmatary(*),len_invmatary
 
      pointer (ipisurfeltstack,isurfeltstack)
      integer isurfeltstack(*),len_isurfeltstack
 
      pointer (iplockout,lockout)
      logical lockout(*)
      integer len_lockout
 
      pointer (ipitadd,itadd)
      integer itadd(*),len_itadd
 
      pointer (ipieadd,ieadd)
      integer ieadd(*),len_ieadd
 
      pointer (ipiadd,iadd)
      integer iadd(*),len_iadd
 
      pointer (ipitpadd,itpadd)
      integer itpadd(*),len_itpadd
 
      pointer (ipicradd,icradd)
      integer icradd(*),len_icradd
 
      pointer (ipxadd,xadd)
      real*8 xadd(*)
      integer len_xadd
 
      pointer (ipyadd,yadd)
      real*8 yadd(*)
      integer len_yadd
 
      pointer (ipzadd,zadd)
      real*8 zadd(*)
      integer len_zadd
 
      pointer (ipieltstack,ieltstack)
      integer ieltstack(*),len_ieltstack
 
      pointer (ipnewbcomplist,newbcomplist)
      integer newbcomplist(*),len_newbcomplist
 
      pointer (iplfoundbcomp,lfoundbcomp)
      logical lfoundbcomp(*)
      integer len_lfoundbcomp
 
      pointer (ipnewprotmatlist,newprotmatlist)
      integer newprotmatlist(*),len_newprotmatlist
 
      pointer (iplfoundprotmat,lfoundprotmat)
      logical lfoundprotmat(*)
      integer len_lfoundprotmat
 
      pointer (iprcomp,rcomp)
      real*8  rcomp(*)
      integer len_rcomp
 
      pointer (iplfoundcomp,lfoundcomp)
      logical lfoundcomp(*)
      integer len_lfoundcomp
 
      pointer (ipiedgehyb,iedgehyb)
      integer iedgehyb(*),len_iedgehyb
 
      pointer (ipinvlochyb,invlochyb)
      integer invlochyb(*),len_invlochyb
 
      pointer (ipicomp,icomp)
      integer icomp(*),len_icomp
 
      pointer (ipicompadj,icompadj)
      integer icompadj(*),len_icompadj
 
      pointer (ipicompadjptr,icompadjptr)
      integer icompadjptr(*),len_icompadjptr
 
      pointer (ipibcomplist,ibcomplist)
      integer ibcomplist(*),len_ibcomplist
 
      pointer (ipiprotmatlist,iprotmatlist)
      integer iprotmatlist(*),len_iprotmatlist
 
      pointer (ipifeature,ifeature)
      integer ifeature(*),len_ifeature
 
      pointer (ipifeatureptr,ifeatureptr)
      integer ifeatureptr(*),len_ifeatureptr
 
      pointer (iptheta,theta)
      real*8  theta(*)
      integer len_theta
 
      pointer (ipiprm,iprm)
      integer iprm(*),len_iprm
 
      pointer (ipiproc,iproc)
      integer iproc(*),len_iproc
 
      pointer (ipiprocedge,iprocedge)
      logical iprocedge(*)
      integer len_iprocedge
 
      pointer (iprnodhyb,rnodhyb)
      real*8  rnodhyb(*)
      integer len_rnodhyb
 
      integer imsginsave(3),msgtypesave(3)
      real*8 xmsginsave(3)
      character*32 cmsginsave(3)
 
      logical lrefine,locrefine,lrecolor,locrecolor,llist,
     &   lsamecons_lg,lalltouch,ledgesok,lneed_to_order,lcand,
     &   lprinted_edge_banner,lprinted_vertex_banner
 
      real*8 epsilonl,epsilona,epsilonv,cut_length,rsurf(50)
      real*8 xmin,xmax,ymin,ymax,zmin,zmax
 
      character*8 cglobal, cdefault
      character*32 cmo,isubname,psetname
      character*132 logmess
 
      integer ierror,nnodes,length,icmotype,nelements,mbndry,icscode
     &   ,ieltno,i,j,node,nod1,ihyb,ityp,k,j1,mpno,mpno_old,ierrdum,nod
     &   ,nef_cmo,jtetj,nsurfelt,iedg,nod2,minpar,maxpar
     &   ,next,numedges,nodek,
     &   ierrw,matmax,imat,nmat,jteti,ifac,k1,ibcomp,jtetopp,
     &   itop,iopp,ihybj,ifacj,itypj,nbcomp,ip1,ip2,i2,nnodes_old,
     &   nadd,node1,maxadj,notused,nfoundbcomp,nfoundprot,ncomp,
     &   isprev,iscurr,nconbnd,len1,len2,nodj1,jtetcurr,icurr,ihyb1
     &   ,ityp1,nodek1,itrip,flag
 
      logical lcompresswherepossible
      parameter (lcompresswherepossible=.true.)
 
      integer locdebug,maxtrip
      parameter (locdebug=0,maxtrip=10)
 
      len_isurfelt=0
      len_iedges_first_=0
      len_iedges_first=0
      len_link=0
      len_iedges_=0
      len_iedges=0
      len_iedgisurfelt=0
      len_nodhyb=0
      len_nodhyboff=0
      len_ieltary=0
      len_invieltary=0
      len_mpary=0
      len_iparent=0
      len_invmpary=0
      len_ireal1=0
      len_matary=0
      len_invmatary=0
      len_isurfeltstack=0
      len_lockout=0
      len_itadd=0
      len_ieadd=0
      len_iadd=0
      len_itpadd=0
      len_icradd=0
      len_xadd=0
      len_yadd=0
      len_zadd=0
      len_ieltstack=0
      len_newbcomplist=0
      len_lfoundbcomp=0
      len_newprotmatlist=0
      len_lfoundprotmat=0
      len_rcomp=0
      len_lfoundcomp=0
      len_iedgehyb=0
      len_invlochyb=0
      len_icomp=0
      len_icompadj=0
      len_icompadjptr=0
      len_ibcomplist=0
      len_iprotmatlist=0
      len_ifeature=0
      len_ifeatureptr=0
      len_theta=0
      len_iprm=0
      len_iproc=0
      len_iprocedge=0
      len_rnodhyb=0
 
      if (locdebug.ge.1) then
         call dotaskx3d('dump/gmv/gmvprepopcones/3dmesh/ascii ; finish',
     &      ierror)
         call dotaskx3d('dump/lagrit/lgprepopcones ; finish',
     &      ierror)
      endif
 
      isubname = 'popcones_lg'
      cglobal='global'
      cdefault='default'
      ierror=0
 
      call get_epsilon('epsilonl',epsilonl)
 
C  Check that user has specified a valid mesh object.
 
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'POPCONES_LG: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      if (nwds.lt.2.or.msgtype(2).le.0.or.cmsgin(2)(1:5).eq.'-def-')
     &   then
         call setsize()
         call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsilona,epsilonv)
         cut_length=.01*sqrt((xmax-xmin)**2+(ymax-ymin)**2+
     &      (zmax-zmin)**2)
      else
         call test_argument_type(1,2,2,imsgin,xmsgin,cmsgin,msgtype,
     &      nwds)
         cut_length=xmsgin(2)
      endif
 
      llist=.false.
      if (nwds.lt.6.or.msgtype(6).le.0.or.cmsgin(6)(1:5).eq.'-def-')
     &   then
      else
         call test_argument_type(1,3,6,imsgin,xmsgin,cmsgin,msgtype,
     *      nwds)
         if (cmsgin(6)(1:4).eq.'list') then
            llist=.true.
         endif
      endif
 
c.... Save PSET info, since DOTASKX3D destroys it.
 
      if (nwds.ge.5) then
         do i=1,3
            imsginsave(i)=imsgin(i+2)
            xmsginsave(i)=xmsgin(i+2)
            cmsginsave(i)=cmsgin(i+2)
            msgtypesave(i)=msgtype(i+2)
         enddo
      else
         msgtypesave(1)=1
         imsginsave(1)=1
         imsginsave(2)=0
         imsginsave(3)=0
      endif
C
      ledgesok=.false.
 
      do itrip=1,maxtrip
 
         lprinted_edge_banner=.false.
         lprinted_vertex_banner=.false.
 
c.... Get info from mesh object.
 
         call cmo_get_info('nnodes',cmo,
     *      nnodes,length,icmotype,ierror)
         call cmo_get_info('nelements',cmo,
     *      nelements,length,icmotype,ierror)
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
         call cmo_get_info ('nconbnd',cmo,nconbnd,length,icmotype,ierror
     &      )
 
         if (nnodes.gt.len_ireal1) call mm_ovall('ireal1',isubname,
     &      ipireal1,nnodes,100,len_ireal1,1,icscode)
 
         if (nnodes.gt.len_iparent) call mm_ovall('iparent',isubname,
     &      ipiparent,nnodes,100,len_iparent,1,icscode)
 
         if (nnodes.gt.len_invmpary) call mm_ovall('invmpary',isubname,
     &      ipinvmpary,nnodes,100,len_invmpary,1,icscode)
 
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
         call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
         if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
 
c     ..................................................................
c     find the parents of each node.
c
         call unpackpc(nnodes,itp1,isn1,iparent)
 
         if (nnodes.gt.len_mpary) call mm_ovall('mpary',isubname,
     &      ipmpary,nnodes,100,len_mpary,1,icscode)
 
         call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &      ipmpary,mpno,psetname,ierror)
 
c.... We derive MATARY, the array of materials from the incoming PSET
c.... in MPARY.  We then convert MPARY to all parent points (and proper
c.... parents at that, because interior points cannot have bad tangent
c.... cones).  On each iteration, we add to MPARY any new points that
c.... were created by refinement that have material type in MATARY, and
c.... we again convert that to an all-parent set.
 
         matmax=0
         do i=1,nnodes
            if (ireal1(i).eq.1) then
               matmax=max(matmax,imt1(i))
            endif
         enddo
 
         if (matmax.gt.len_invmatary) call mm_ovall('invmatary',isubname
     &      ,ipinvmatary,matmax,0,len_invmatary,1,icscode)
 
         if (matmax.gt.len_matary) call mm_ovall('matary',isubname,
     &      ipmatary,matmax,0,len_matary,1,icscode)
 
         do i=1,matmax
            invmatary(i)=0
         enddo
 
         nmat=0
         do i=1,mpno
            node=mpary(i)
            if (ireal1(node).eq.1) then
               imat=imt1(node)
               if (invmatary(imat).eq.0) then
                  nmat=nmat+1
                  matary(nmat)=imat
                  invmatary(imat)=nmat
               endif
            endif
         enddo
 
c.... change mass point array to contain only parent nodes.
 
         do i=1,nnodes
            invmpary(i)=0
         enddo
 
         mpno_old=mpno
         mpno=0
         do k=1,mpno_old
            if (ireal1(mpary(k)).eq.1.or.
     &         itp1(mpary(k)).eq.ifitpcup) then
               nod=iparent(mpary(k))
c... The following IF statement guarantees that the parent
c... is a PROPER parent... that is we only put in MPARY
c... parents that have at least two children.
               if (itp1(nod).eq.ifitpcup) then
                  if (invmpary(nod).eq.0) then
                     mpno=mpno+1
                     mpary(mpno)=nod
                     invmpary(nod)=mpno
                     if (itp1(nod).eq.ifitpcup) then
                        nod1=isn1(nod)
                        do while (nod1.ne.nod.and.nod1.ne.0)
                           invmpary(nod1)=mpno
                           nod1=isn1(nod1)
                        enddo
                     endif
                  endif
               endif
            endif
         enddo
 
         if (mpno.le.0) goto 9999
 
         if (mpno.gt.len_isurfelt) call mm_ovall('isurfelt',isubname,
     &      ipisurfelt,mpno,100,len_isurfelt,1,icscode)
 
         if (mpno.gt.len_ieltary) call mm_ovall('ieltary',isubname,
     &      ipieltary,mpno,100,len_ieltary,1,icscode)
 
         if (nelements.gt.len_invieltary) call mm_ovall('invieltary'
     &      ,isubname,ipinvieltary,nelements,100,len_invieltary,1
     &      ,icscode)
 
c.... Compute list of elements that are involved in these computations
c.... (ie contain at least one node in MPARY).
 
         ieltno=0
         do 40 i=1,nelements
            do j=1,nelmnen(itettyp(i))
               if (invmpary(itet(j+itetoff(i))).ne.0) then
                  ieltno=ieltno+1
                  if (ieltno.gt.len_ieltary) call mm_ovall('ieltary'
     &               ,isubname,ipieltary,ieltno,100+mpno,len_ieltary,1
     &               ,icscode)
                  ieltary(ieltno)=i
                  goto 40
               endif
            enddo
 40      continue
 
c... Compute inverse of IELTARY relation.
 
         do i=1,nelements
            invieltary(i)=0
         enddo
         do i=1,ieltno
            invieltary(ieltary(i))=i
         enddo
 
c obtain node-hyb relation.  For a given node, the node-hyb relation
c is a list of numbers that give the hybrid elements that the
c the node belongs to AND the local node number within each element.
 
         call getnodhyb(mpno,mpary,ieltno,ieltary,nnodes,itet,
     &      itetoff,itettyp,iparent,invmpary,isubname,ipnodhyb
     &      ,ipnodhyboff)
 
         maxadj=0
         do i=1,mpno
            maxadj=max(maxadj,nodhyboff(i+1)-nodhyboff(i))
         enddo
 
c.... Order entries in NODHYB.
 
         if (maxadj.gt.len_rnodhyb) call mm_ovall('rnodhyb'
     &      ,isubname,iprnodhyb,maxadj,100,len_rnodhyb,2,icscode)
 
         do i=1,mpno
            do j=1,nodhyboff(i+1)-nodhyboff(i)
               rnodhyb(j)=nodhyb(nodhyboff(i)+j)
            enddo
            call hpsort(nodhyboff(i+1)-nodhyboff(i),rnodhyb)
            do j=1,nodhyboff(i+1)-nodhyboff(i)
               nodhyb(nodhyboff(i)+j)=rnodhyb(j)
            enddo
         enddo
 
c.... Order entries in ICONTAB if necessary.
 
         lneed_to_order=.false.
         do i=1,nconbnd
            isprev=icontab(3,i)
            do j=2,icontab(1,i)
               iscurr=icontab(2+j,i)
               if (isprev.ge.iscurr) lneed_to_order=.true.
               isprev=iscurr
            enddo
         enddo
         if (lneed_to_order) then
            print*,'Reordering ICONTAB entries.'
            do i=1,nconbnd
               do j=1,icontab(1,i)
                  rsurf(j)=icontab(j+2,i)
               enddo
               call hpsort(icontab(1,i),rsurf)
               do j=1,icontab(1,i)
                  icontab(j+2,i)=rsurf(j)
               enddo
            enddo
         endif
 
c.... Create ISURFELT array of external boundary elements (usually
c triangles)
c.... by looping through volume elements and peeling off faces.
c.... At the same time, compile IEDGES---the list of external surface
c edges.
c.... These are stored as (I<J); i.e., there is
c.... unique entry for each such edge between specified nodes I and J,
c.... and node I is taken to be the one with lower global index.
 
 
         if (nnodes.gt.len_iedges_first_) call mm_ovall('iedges_first_'
     &      ,isubname,ipiedges_first_,nnodes,100,len_iedges_first_,1
     &      ,icscode)
         do i=1,nnodes
            iedges_first_(i)=0
         enddo
 
         next=1
 
         nsurfelt=0
         do i=1,ieltno
            ihyb=ieltary(i)
            ityp=itettyp(ihyb)
            do 10 j=1,nelmnef(ityp)
               jtetj=jtet(j+jtetoff(ihyb))
               if (jtetj.ne.mbndry) goto 10
               lcand=.false.
               do j1=1,ielmface0(j,ityp)
                  nodj1=itet(itetoff(ihyb)+ielmface1(j1,j,ityp))
                  if (invmpary(nodj1).ne.0) lcand=.true.
               enddo
               if (.not.lcand) goto 10
               nsurfelt=nsurfelt+1
               if (nsurfelt.gt.len_isurfelt) call mm_ovall('isurfelt'
     &            ,isubname,ipisurfelt,nsurfelt,100+mpno,len_isurfelt,1
     &            ,icscode)
               isurfelt(nsurfelt)=(ihyb-1)*nef_cmo+j
               do j1=1,ielmface0(j,ityp)
                  iedg=ielmface2(j1,j,ityp)
                  nod1=iparent(itet(itetoff(ihyb)+ielmedge1(1,iedg,ityp)
     &               ))
                  nod2=iparent(itet(itetoff(ihyb)+ielmedge1(2,iedg,ityp)
     &               ))
                  maxpar=max(nod1,nod2)
                  minpar=min(nod1,nod2)
 
                  if (next.gt.len_link) call mm_ovall('link'
     &               ,isubname,iplink,next,mpno+100,len_link,1,icscode)
                  if (next.gt.len_iedges_) call mm_ovall('iedges_'
     &               ,isubname,ipiedges_,next,mpno+100,len_iedges_,1
     &               ,icscode)
                  call insertll(iedges_first_,ipiedges_,iplink,
     &               len_iedges_first_,notused,minpar,maxpar,next)
 
               enddo
 10         continue
         enddo
         numedges=next-1
 
         if (numedges.gt.len_iedges) call mm_ovall('iedges',isubname,
     &      ipiedges,numedges,100,len_iedges,1,icscode)
 
         if (nnodes+1.gt.len_iedges_first) call mm_ovall('iedges_first'
     &      ,isubname,ipiedges_first,nnodes+1,100,len_iedges_first,1
     &      ,icscode)
 
         call orderll(iedges_first_,iedges_,link,nnodes,
     &      iedges_first,iedges)
 
c.... Loop thru surface elements and create edge-element relation
c.... IEDGISURFELT.
 
         if (2*numedges.gt.len_iedgisurfelt) call mm_ovall
     &      ('iedgisurfelt',isubname,ipiedgisurfelt,2*numedges,200
     &      ,len_iedgisurfelt,1,icscode)
 
         do i=1,numedges
            iedgisurfelt(1,i)=0
            iedgisurfelt(2,i)=0
         enddo
 
c.... This following piece of code establishes the
c.... surface edge--> surface element relation.
c.... It is written to be valid for hybrid elements
c.... in 3-D, but is not valid for 2-D, because
c.... "surface-elements" in 2-D are edges, and so
c.... the analog of 3-D surface edges is surface points.
 
         do i=1,nsurfelt
            jteti=isurfelt(i)
            ihyb=1+(jteti-1)/nef_cmo
            ifac=jteti-nef_cmo*(ihyb-1)
            ityp=itettyp(ihyb)
            do 100 k=1,ielmface0(ifac,ityp)
               iedg=ielmface2(k,ifac,ityp)
               nod1=iparent(itet(ielmedge1(1,iedg,ityp)+itetoff(ihyb)))
               nod2=iparent(itet(ielmedge1(2,iedg,ityp)+itetoff(ihyb)))
               maxpar=max(nod1,nod2)
               minpar=min(nod1,nod2)
               do k1=iedges_first(minpar),iedges_first(minpar+1)-1
                  if (iedges(k1).eq.maxpar) then
                     if (iedgisurfelt(1,k1).eq.0) then
                        iedgisurfelt(1,k1)=i
                     elseif (iedgisurfelt(2,k1).eq.0) then
                        iedgisurfelt(2,k1)=i
                     else
c                        if(locdebug.gt.0)
                        if(.true.)
     &                     print*,'Warning! Edge ',k1
     &                     ,' has >2 triangles'
                     endif
                     goto 100
                  endif
               enddo
 100        continue
         enddo
 
c.... Loop thru ISURFELT and look for connected sets of surface elements
c ,
c.... defining virtual materials (negative material numbers) that are
c.... written into JTET over the usual MBNDRY value.
 
         if (100.gt.len_isurfeltstack) call mm_ovall('isurfeltstack'
     &      ,isubname,ipisurfeltstack,100,0,len_isurfeltstack,1,icscode)
 
         ibcomp=0
 
         do 110 i=1,nsurfelt
            jteti=isurfelt(i)
            jtetopp=jtet(jteti)
            if (jtetopp.ne.mbndry) goto 110
            ibcomp=ibcomp-1
 
            jtet(jteti)=ibcomp
 
c... Put on stack
            itop=1
            isurfeltstack(itop)=i
 
            do while (itop.gt.0)
 
c.... Pop stack
 
               icurr=isurfeltstack(itop)
               jtetcurr=isurfelt(icurr)
               itop=itop-1
               ihyb=1+(jtetcurr-1)/nef_cmo
               ifac=jtetcurr-nef_cmo*(ihyb-1)
               ityp=itettyp(ihyb)
 
               do 120 k=1,ielmface0(ifac,ityp)
                  iedg=ielmface2(k,ifac,ityp)
                  nod1=iparent(itet(ielmedge1(1,iedg,ityp)+itetoff(ihyb)
     &               ))
                  nod2=iparent(itet(ielmedge1(2,iedg,ityp)+itetoff(ihyb)
     &               ))
                  maxpar=max(nod1,nod2)
                  minpar=min(nod1,nod2)
                  do j=iedges_first(minpar),iedges_first(minpar+1)-1
                     if (iedges(j).eq.maxpar) then
                        if (iedgisurfelt(1,j).eq.icurr) then
                           iopp=iedgisurfelt(2,j)
                           if (iopp.ne.0) then
                              goto 130
                           else
                              goto 120
                           endif
                        elseif (iedgisurfelt(2,j).eq.icurr) then
                           iopp=iedgisurfelt(1,j)
                           if (iopp.ne.0) then
                              goto 130
                           else
                              goto 120
                           endif
                        else
                           print*
     &                        ,'Can''t find surf elt'
     &                        ,' touching surf edge !'
                           stop
                        endif
                     endif
                  enddo
                  print*,'Warning! Can''t find surface edge !'
                  stop
 130              continue
                  jtetj=isurfelt(iopp)
                  if (jtet(jtetj).eq.mbndry) then
                     ihybj=1+(jtetj-1)/nef_cmo
                     ifacj=jtetj-nef_cmo*(ihybj-1)
                     itypj=itettyp(ihybj)
                     if (lsamecons_lg(ihyb,ifac,ityp,ihybj,ifacj,itypj
     &                  ,maxpar,minpar,iparent,itet,itetoff,icontab,icr1
     &                  ))then
                        jtet(jtetj)=ibcomp
                        itop=itop+1
                        if (itop.gt.len_isurfeltstack) call mm_ovall
     &                     ('isurfeltstack',isubname,ipisurfeltstack
     &                     ,itop,100,len_isurfeltstack,1,icscode)
                        isurfeltstack(itop)=iopp
                     endif
                  endif
 120           continue
            enddo
 110     continue
         nbcomp=abs(ibcomp)
 
c.... Iniialize Lock-out list.
 
         if (mpno.gt.len_lockout) call mm_ovall('lockout',isubname,
     &      iplockout,mpno,100,len_lockout,1,icscode)
 
         do i=1,mpno
            lockout(i)=.false.
         enddo
 
         nadd=0
         lrefine=.false.
         lrecolor=.false.
 
c.... Obtain storage for arrays used to obtain and represent information
c.... about the tangent cones at points and edges.
 
         if (maxadj.gt.len_ieltstack) call mm_ovall('ieltstack',isubname
     &      ,ipieltstack,maxadj,100,len_ieltstack,1,icscode)
 
         if (maxadj*nef_cmo.gt.len_newbcomplist) call mm_ovall
     &      ('newbcomplist',isubname,ipnewbcomplist,maxadj*nef_cmo,100
     &      ,len_newbcomplist,1,icscode)
 
         if (maxadj*nef_cmo.gt.len_lfoundbcomp) call mm_ovall
     &      ('lfoundbcomp',isubname,iplfoundbcomp,maxadj*nef_cmo,100
     &      ,len_lfoundbcomp,1,icscode)
 
         if (maxadj*nef_cmo.gt.len_newprotmatlist) call mm_ovall
     &      ('newprotmatlist',isubname,ipnewprotmatlist,maxadj*nef_cmo
     &      ,100,len_newprotmatlist,1,icscode)
 
         if (maxadj*nef_cmo.gt.len_lfoundprotmat) call mm_ovall
     &      ('lfoundprotmat',isubname,iplfoundprotmat,maxadj*nef_cmo,100
     &      ,len_lfoundprotmat,1,icscode)
 
         if (maxadj.gt.len_rcomp) call mm_ovall('rcomp',isubname,
     &      iprcomp,maxadj,100,len_rcomp,2,icscode)
 
         if (maxadj.gt.len_lfoundcomp) call mm_ovall('lfoundcomp'
     &      ,isubname,iplfoundcomp,maxadj,100,len_lfoundcomp,1,icscode)
 
         if (maxadj.gt.len_iedgehyb) call mm_ovall('iedgehyb',isubname,
     &      ipiedgehyb,maxadj,100,len_iedgehyb,1,icscode)
 
         if (ieltno.gt.len_invlochyb) call mm_ovall('invlochyb',isubname
     &      ,ipinvlochyb,ieltno,100,len_invlochyb,1,icscode)
 
         if (maxadj.gt.len_icomp) call mm_ovall('icomp',isubname,
     &      ipicomp,maxadj,100,len_icomp,1,icscode)
 
         if (maxadj**2/2.gt.len_icompadj) call mm_ovall('icompadj'
     &      ,isubname,ipicompadj,maxadj**2/2,100,len_icompadj,1,icscode)
 
         if (maxadj+1.gt.len_icompadjptr) call mm_ovall('icompadjptr'
     &      ,isubname,ipicompadjptr,maxadj+1,100,len_icompadjptr,1
     &      ,icscode)
 
         if (maxadj*nef_cmo.gt.len_ibcomplist) call mm_ovall
     &      ('ibcomplist',isubname,ipibcomplist,maxadj*nef_cmo,100
     &      ,len_ibcomplist,1,icscode)
 
         if (maxadj*nef_cmo.gt.len_iprotmatlist) call mm_ovall
     &      ('iprotmatlist',isubname,ipiprotmatlist,maxadj*nef_cmo,100
     &      ,len_iprotmatlist,1,icscode)
 
         if (maxadj.gt.len_ifeature) call mm_ovall('ifeature',isubname,
     &      ipifeature,maxadj,100,len_ifeature,1,icscode)
 
         if (maxadj+1.gt.len_ifeatureptr) call mm_ovall('ifeatureptr'
     &      ,isubname,ipifeatureptr,maxadj+1,100,len_ifeatureptr,1
     &      ,icscode)
 
         if (maxadj.gt.len_theta) call mm_ovall('theta',isubname,
     &      iptheta,maxadj,100,len_theta,2,icscode)
 
         if (maxadj.gt.len_iprm) call mm_ovall('iprm',isubname,
     &      ipiprm,maxadj,100,len_iprm,1,icscode)
 
         if (nnodes.gt.len_iproc) call mm_ovall('iproc',isubname,
     &      ipiproc,nnodes,100,len_iproc,1,icscode)
 
         if (nnodes.gt.len_iprocedge) call mm_ovall('iprocedge',isubname
     &      ,ipiprocedge,nnodes,100,len_iprocedge,1,icscode)
 
         do i=1,ieltno
            invlochyb(i)=0
         enddo
         do i=1,nnodes
            iproc(i)=0
            iprocedge(i)=.false.
         enddo
 
c.... Loop through edges, finding and fixing those with bad tangent
c.... cones.  To save compute time, if we find no bad tangent cones,
c.... we set LEDGESOK=.true. and never check edges again, even if
c.... refinement and recoloring may be subsequently performed to
c.... correct tangent cones at points.
 
c$$$      if (ledgesok) goto 350
 
         do 300 i=1,mpno
            if (lockout(i)) goto 300
            ip1=mpary(i)
            do j=nodhyboff(i)+1,nodhyboff(i+1)
               ihyb=ieltary(1+(nodhyb(j)-1)/maxnen)
               ityp=itettyp(ihyb)
               do k=1,nelmnen(ityp)
                  ip2=iparent(itet(itetoff(ihyb)+k))
                  i2=invmpary(ip2)
                  if ((i2.gt.i).and.(.not.lockout(i2)).and.(.not
     &               .iprocedge(ip2))) then
                     iprocedge(ip2)=.true.
                     len1=nodhyboff(i+1)-nodhyboff(i)
                     len2=nodhyboff(i2+1)-nodhyboff(i2)
                     call gettangcone_lg(xic,yic,zic,itet,itetoff,ip1
     &                  ,ip2,nodhyb(nodhyboff(i)+1),len1
     &                  ,nodhyb(nodhyboff(i2)+1),len2,ieltary,itettyp
     &                  ,jtet,jtetoff,mbndry,invieltary,itetclr
     &                  ,invmatary,nef_cmo,lalltouch,ieltstack
     &                  ,nfoundbcomp,newbcomplist,lfoundbcomp,nfoundprot
     &                  ,newprotmatlist,lfoundprotmat,ncomp,rcomp
     &                  ,lfoundcomp,iedgehyb,invlochyb,icomp,icompadj
     &                  ,icompadjptr,ibcomplist,iprotmatlist,ifeature
     &                  ,ifeatureptr,theta)
 
                     if (.not.lalltouch) then
                        if (.not.lprinted_edge_banner) then
                           write(logmess,'(2a)')
     &                        'POPCONES_LG: Edges with illegal tangent',
     &                        ' cone connectivity:'
                           call writloga('default',0,logmess,0,ierrw)
                           lprinted_edge_banner=.true.
                        endif
                        write(logmess,'(i8,x,i8)') ip1,ip2
                        call writloga('default',0,logmess,0,ierrw)
 
                        if (.not.llist) then
                           call correcttangcone_lg(isubname,ip1,ip2,xic
     &                        ,yic,zic,epsilonl,cut_length,itet,itetoff
     &                        ,itetclr,itp1,isn1,imt1,icr1,icontab
     &                        ,iparent,itettyp,mpno,ieltary
     &                        ,nodhyb(nodhyboff(i)+1),iedgehyb,iproc
     &                        ,iprm,ifeature,ifeatureptr,icompadj
     &                        ,icompadjptr,ncomp,theta,nadd,ipitadd
     &                        ,len_itadd,ipieadd,len_ieadd,ipiadd
     &                        ,len_iadd,ipitpadd,len_itpadd,ipicradd
     &                        ,len_icradd,ipxadd,len_xadd,ipyadd
     &                        ,len_yadd,ipzadd,len_zadd,locrefine
     &                        ,locrecolor,ierror)
 
                           lrefine=lrefine.or.locrefine
                           lrecolor=lrecolor.or.locrecolor
 
c.... Lockout nodes in neighbourhood of I and I2.
 
                           if (locrefine.or.locrecolor) then
 
                              do j1=nodhyboff(i)+1,nodhyboff(i+1)
                                 ihyb1=ieltary(1+(nodhyb(j1)-1)/maxnen)
                                 ityp1=itettyp(ihyb1)
                                 do k1=1,nelmnen(ityp1)
                                    nodek1=iparent(itet(itetoff(ihyb1)
     &                                 +k1))
                                    if (invmpary(nodek1).ne.0) then
                                       lockout(invmpary(nodek1))=.true.
                                    endif
                                 enddo
                              enddo
 
                              do j1=nodhyboff(i2)+1,nodhyboff(i2+1)
                                 ihyb1=ieltary(1+(nodhyb(j1)-1)/maxnen)
                                 ityp1=itettyp(ihyb1)
                                 do k1=1,nelmnen(ityp1)
                                    nodek1=iparent(itet(itetoff(ihyb1)
     &                                 +k1))
                                    if (invmpary(nodek1).ne.0) then
                                       lockout(invmpary(nodek1))=.true.
                                    endif
                                 enddo
                              enddo
                              goto 290 ! Break out of inner loop, since I lockedout
                           endif
                        endif
                     endif
                  endif
               enddo
            enddo
 290        continue
            do j=nodhyboff(i)+1,nodhyboff(i+1)
               ihyb=ieltary(1+(nodhyb(j)-1)/maxnen)
               ityp=itettyp(ihyb)
               do k=1,nelmnen(ityp)
                  ip2=iparent(itet(itetoff(ihyb)+k))
                  iprocedge(ip2)=.false.
               enddo
            enddo
 300     continue
 
         if (.not.(lrecolor.or.lrefine)) ledgesok=.true.
 
c.... Loop through points, finding and fixing those with bad tangent
c.... cones.
 
 350     continue
 
         do 400 i=1,mpno
            if (lockout(i)) goto 400
 
            ip1=mpary(i)
            ip2=0
            len1=nodhyboff(i+1)-nodhyboff(i)
            len2=0
 
            call gettangcone_lg(xic,yic,zic,itet,itetoff,ip1,ip2
     &         ,nodhyb(nodhyboff(i)+1),len1,notused,len2,ieltary,itettyp
     &         ,jtet,jtetoff,mbndry,invieltary,itetclr,invmatary,nef_cmo
     &         ,lalltouch,ieltstack,nfoundbcomp,newbcomplist,lfoundbcomp
     &         ,nfoundprot,newprotmatlist,lfoundprotmat,ncomp,rcomp
     &         ,lfoundcomp,iedgehyb,invlochyb,icomp,icompadj,icompadjptr
     &         ,ibcomplist,iprotmatlist,ifeature,ifeatureptr,theta)
 
            if (.not.lalltouch) then
               if (.not.lprinted_vertex_banner) then
                  write(logmess,'(2a)')
     &               'POPCONES_LG: Vertices with illegal tangent cone '
     &               ,'connectivity:'
                  call writloga('default',0,logmess,0,ierrw)
                  lprinted_vertex_banner=.true.
               endif
               write(logmess,'(i8)') ip1
               call writloga('default',0,logmess,0,ierrw)
 
               if (.not.llist) then
                  call correcttangcone_lg(isubname,ip1,ip2,xic,yic,zic
     &               ,epsilonl,cut_length,itet,itetoff,itetclr,itp1,isn1
     &               ,imt1,icr1,icontab,iparent,itettyp,mpno,ieltary
     &               ,nodhyb(nodhyboff(i)+1),iedgehyb,iproc,iprm
     &               ,ifeature,ifeatureptr,icompadj,icompadjptr,ncomp
     &               ,theta,nadd,ipitadd,len_itadd,ipieadd,len_ieadd
     &               ,ipiadd,len_iadd,ipitpadd,len_itpadd,ipicradd
     &               ,len_icradd,ipxadd,len_xadd,ipyadd,len_yadd,ipzadd
     &               ,len_zadd,locrefine,locrecolor,ierror)
                  lrefine=lrefine.or.locrefine
                  lrecolor=lrecolor.or.locrecolor
 
c.... Lockout nodes in neighbourhood of I.
 
                  if (locrefine.or.locrecolor) then
 
                     do j=nodhyboff(i)+1,nodhyboff(i+1)
                        ihyb=ieltary(1+(nodhyb(j)-1)/maxnen)
                        ityp=itettyp(ihyb)
                        do k=1,nelmnen(ityp)
                           nodek=iparent(itet(itetoff(ihyb)+k))
                           if (invmpary(nodek).ne.0) then
                              lockout(invmpary(nodek))=.true.
                           endif
                        enddo
                     enddo
                  endif
               endif
            endif
 400     continue
 
c.... Restore JTET boundary values.
 
         do i=1,ieltno
            ihyb=ieltary(i)
            ityp=itettyp(ihyb)
            do j=1,nelmnef(ityp)
               if (jtet(jtetoff(ihyb)+j).lt.0) then
                  jtet(jtetoff(ihyb)+j)=mbndry
               endif
            enddo
         enddo
 
         if (llist) goto 9999
 
c.... Perform refinement if necessary.
 
         if (lrefine) then
            nnodes_old=nnodes
            call refine_fix_add(cmo,nadd,ipitadd,ipieadd,ipiadd,
     &         ipitpadd,ipicradd)
            call refine_edge_add_tet(cmo,nadd,ipitadd,ipieadd,
     &         iadd,xadd,yadd,zadd,flag)
c.... Fix up ITP1, ICR1.
 
            call cmo_get_info('nnodes',cmo,
     *         nnodes,length,icmotype,ierror)
            call cmo_get_info('itp1',cmo,
     *         ipitp1,length,icmotype,ierror)
            call cmo_get_info('icr1',cmo,
     *         ipicr1,length,icmotype,ierror)
            call cmo_get_info('imt1',cmo,
     *         ipimt1,length,icmotype,ierror)
            call cmo_get_info('isn1',cmo,
     *         ipisn1,length,icmotype,ierror)
            do i = 1,nadd
               if (iadd(i).gt.0) then
                  node=iadd(i)
                  if (isn1(node).eq.0) then
                     itp1(node)=itpadd(i)
                     icr1(node)=icradd(i)
                  else
                     icr1(node)=icradd(i)
                     if (itp1(node).ne.ifitpcup) then
                        itp1(node)=itpadd(i)
                     endif
                     node1=isn1(node)
                     do while (node1.ne.node)
                        icr1(node1)=icradd(i)
                        if (itp1(node1).ne.ifitpcup) then
                           itp1(node1)=itpadd(i)
                        endif
                        node1=isn1(node1)
                     enddo
                  endif
               endif
            enddo
         endif
 
c.... Now execute LaGriT commands to redefine parent child system and
c.... ITP1 based on new tet colors.
 
            if (lrecolor) then
               nnodes_old=nnodes
               if (locdebug.ge.1) then
                  call dotaskx3d
     &               ('dump/gmv/gmvrecolor/3dmesh/ascii ; finish',ierror
     &               )
                  call dotaskx3d('dump/lagrit/lgrecolor ; finish',ierror
     &               )
               endif
 
               call dotaskx3d('resetpts / parents ; finish', ierror)
               call dotaskx3d('geniee ; finish', ierror)
               call dotaskx3d('resetpts / itp ; finish', ierror)
               call dotaskx3d('settets / color_points ; finish', ierror)
               if (locdebug.ge.1) then
                  call dotaskx3d('dump/lagrit/lgpostrecolor ; finish'
     &               ,ierror)
               endif
c$$$         call dotaskx3d('rmtetless ; finish', ierror)
 
            endif
 
            if (.not.(lrecolor.or.lrefine)) goto 9999
 
c.... Large grids demand point compression WITHIN the iteration loop.
 
            if (lcompresswherepossible) then
               call dotaskx3d('rmpoint/compress; finish',ierror)
            endif
 
         enddo
 
 9999    continue
 
         call mmrelprt(isubname,icscode)
 
         return
         end
 
      function lsamecons_lg(ihybc,ifacc,itypc,ihybd,ifacd,itypd,ipara
     &   ,iparb,iparent,itet,itetoff,icontab,icr1)
 
      implicit none
 
      include 'local_element.h'
      include 'chydro.h'
 
      logical lsamecons_lg
      integer ihybc,ifacc,ihybd,ifacd,ipara,iparb,icontab(50,*)
      integer i,itypc,lnod,nod,iparent(*),itet(*),itetoff(*),ioppc,
     &   itypd,ioppd,nlistab,listab(50),nlistabc,listabc(50),
     &   nlistabd,listabd(50),iposb,icra,icrb,isa,iposc,isab,icrc,
     &   iposd,icrd,icr1(*)
 
      icra=icr1(ipara)
      icrb=icr1(iparb)
      do i=1,ielmface0(ifacc,itypc)
         lnod=ielmface1(i,ifacc,itypc)
         nod=iparent(itet(lnod+itetoff(ihybc)))
         if (nod.ne.ipara.and.nod.ne.iparb) goto 10
      enddo
 10   continue
      ioppc=nod
      icrc=icr1(ioppc)
 
      do i=1,ielmface0(ifacd,itypd)
         lnod=ielmface1(i,ifacd,itypd)
         nod=iparent(itet(lnod+itetoff(ihybd)))
         if (nod.ne.ipara.and.nod.ne.iparb) goto 20
      enddo
 20   continue
      ioppd=nod
      icrd=icr1(ioppd)
 
c.... We now find the intersection list of the constraints
c.... on ipara and iparb.  We assume that the lists for these
c.... nodes are in ascending order.
 
      nlistab=0
      if (icra.ne.0.and.icrb.ne.0) then
         iposb=1
         do i=1,icontab(1,icra)
            isa=icontab(2+i,icra)
 30         continue
            if (icontab(2+iposb,icrb).lt.isa) then
               iposb=iposb+1
               if (iposb.gt.icontab(1,icrb)) then
                  goto 40
               else
                  goto 30
               endif
            elseif (icontab(2+iposb,icrb).eq.isa) then
               nlistab=nlistab+1
               listab(nlistab)=isa
            endif
         enddo
      endif
 
 40   continue
 
c.... We now find the intersection list of the constraints
c.... on ipara and iparb and iparc.
 
      nlistabc=0
      if (icrc.ne.0) then
         iposc=1
         do i=1,nlistab
            isab=listab(i)
 50         continue
            if (icontab(2+iposc,icrc).lt.isab) then
               iposc=iposc+1
               if (iposc.gt.icontab(1,icrc)) then
                  goto 60
               else
                  goto 50
               endif
            elseif (icontab(2+iposc,icrc).eq.isab) then
               nlistabc=nlistabc+1
               listabc(nlistabc)=isab
            endif
         enddo
      endif
 
 60   continue
 
c.... We now find the intersection list of the constraints
c.... on ipara and iparb and ipard.
 
      nlistabd=0
      if (icrd.ne.0) then
         iposd=1
         do i=1,nlistab
            isab=listab(i)
 70         continue
            if (icontab(2+iposd,icrd).lt.isab) then
               iposd=iposd+1
               if (iposd.gt.icontab(1,icrd)) then
                  goto 80
               else
                  goto 70
               endif
            elseif (icontab(2+iposd,icrd).eq.isab) then
               nlistabd=nlistabd+1
               listabd(nlistabd)=isab
            endif
         enddo
      endif
 
 80   continue
 
c... If LISTABC and LISTABD are the same, LSAMECONS_LG is .true., else
c... it is .false.
 
      if (nlistabd.ne.nlistabc) then
         lsamecons_lg=.false.
         return
      endif
 
      lsamecons_lg=.true.
      do i=1,nlistabc
         if (listabd(i).ne.listabc(i)) lsamecons_lg=.false.
      enddo
 
      return
      end
 
      subroutine gettangcone_lg(xic,yic,zic,itet,itetoff,ip1,ip2,nodhyb1
     &   ,len1,nodhyb2,len2,ieltary,itettyp,jtet,jtetoff,mbndry
     &   ,invieltary,itetclr,invmatary,nef_cmo,lalltouch,ieltstack
     &   ,nfoundbcomp,newbcomplist,lfoundbcomp,nfoundprot,newprotmatlist
     &   ,lfoundprotmat,ncomp,rcomp,lfoundcomp,iedgehyb,invlochyb,icomp
     &   ,icompadj,icompadjptr,ibcomplist,iprotmatlist,ifeature
     &   ,ifeatureptr,theta)
 
      implicit none
 
      include 'local_element.h'
 
      integer ip1,ip2,nodhyb1(*),nodhyb2(*),len1,len2,ieltstack(*),
     &   newbcomplist(*),newprotmatlist(*),iedgehyb(*),invlochyb(*),
     &   icomp(*),icompadj(*),icompadjptr(*),ibcomplist(*),
     &   nfoundprot,iprotmatlist(*),ifeature(*),ifeatureptr(*),
     &   ieltary(*),itettyp(*),iedg,jtet(*),jtetoff(*),mbndry
     &   ,nfoundbcomp,newbcomp,invieltary(*),nef_cmo,iadjelt,imat,
     &   itetclr(*),invmatary(*),ncomp,itet(*),itetoff(*)
      logical lalltouch,lfoundbcomp(*),lfoundprotmat(*),lfoundcomp(*)
      real*8 rcomp(*),theta(*),xic(*),yic(*),zic(*)
 
      integer len12,lenlochyb,i,ii,icurrcomp,itop,ielt,iloc,iloc2,ihyb,
     &   ityp,j,k,lnod,jtetj,newprot
      logical l1onface,l2onface
      real*8 ascend
 
c.... If ip2=0, we are getting the tangent cones at the point ip1.
c.... Otherwise, we are getting the tangent cones at the edge ip1-ip2.
 
c.... If edge case, collide jtet list
 
      if (ip2.ne.0) call getedghyb_lg(ieltary,itettyp,nodhyb1,len1
     &   ,nodhyb2,len2,iedgehyb,len12)
 
      if (ip2.eq.0) then
         lenlochyb=len1
      else
         lenlochyb=len12
      endif
 
      do i=1,lenlochyb
         if (ip2.eq.0) then
            ii=1+(nodhyb1(i)-1)/maxnen
            invlochyb(ii)=i
         else
            ii=1+(iedgehyb(i)-1)/maxnee2
            invlochyb(ii)=i
         endif
      enddo
 
      lalltouch=.true.
      icurrcomp=0
      do i=1,lenlochyb
         icomp(i)=0
      enddo
      nfoundbcomp=0
      nfoundprot=0
      icompadjptr(1)=1
      do 100 i=1,lenlochyb
 
         if (icomp(i).ne.0) goto 100
 
         if (ip2.eq.0) then
            ihyb=ieltary(1+(nodhyb1(i)-1)/maxnen)
         else
            ihyb=ieltary(1+(iedgehyb(i)-1)/maxnee2)
         endif
         imat=itetclr(ihyb)
         if (invmatary(imat).eq.0) goto 100
 
         newbcomp=0
         newprot=0
         icurrcomp=icurrcomp+1
         theta(icurrcomp)=0.
         icomp(i)=icurrcomp
 
c... Put on stack
         itop=1
         ieltstack(itop)=i
 
         do while (itop.gt.0)
 
c.... Pop stack
 
            ielt=ieltstack(itop)
            itop=itop-1
            if (ip2.eq.0) then
               ii=1+(nodhyb1(ielt)-1)/maxnen
               iloc=nodhyb1(ielt)-maxnen*(ii-1)
               iloc2=0
               ihyb=ieltary(ii)
               ityp=itettyp(ihyb)
               call incrangle_lg(xic,yic,zic,itet,itetoff
     &            ,theta(icurrcomp),iloc,iloc2,ihyb,ityp)
            else
               ii=1+(iedgehyb(ielt)-1)/maxnee2
               ihyb=ieltary(ii)
               ityp=itettyp(ihyb)
               iedg=iedgehyb(ielt)-maxnee2*(ii-1)
               iloc=ielmedge1(1,iedg,ityp)
               iloc2=ielmedge1(2,iedg,ityp)
               call incrangle_lg(xic,yic,zic,itet,itetoff
     &            ,theta(icurrcomp),iloc,iloc2,ihyb,ityp)
            endif
 
            do 130 j=1,nelmnef(ityp)
               l1onface=.false.
               do k=1,ielmface0(j,ityp)
                  lnod=ielmface1(k,j,ityp)
                  if (lnod.eq.iloc) l1onface=.true.
               enddo
               if (.not.l1onface) goto 130
               if (ip2.ne.0) then
                  l2onface=.false.
                  do k=1,ielmface0(j,ityp)
                     lnod=ielmface1(k,j,ityp)
                     if (lnod.eq.iloc2) l2onface=.true.
                  enddo
                  if (.not.l2onface) goto 130
               endif
 
c.... Try to transit thru this face which shares the point or edge
c.... endpoints of the tangent cone.
 
               jtetj=jtet(jtetoff(ihyb)+j)
 
c.... Process correctly the cases of (i) adjacency to a boundary
c.... component, (ii) adjacency to the current material (therefore
c.... in the current component), (iii) adjacency to different material not
c.... in matary, and (iv) adjacency to a different material in matary.
 
               if (jtetj.eq.mbndry) then
                  print*,'POPCONES_LG: not supposed to happen!'
                  stop
 
c.... Adjacency to a boundary component...
 
               elseif (jtetj.lt.0) then
 
c.... If bcomp is in list of known adjacent bcomp, check off that
c.... it is adjacent to this particular component.  If bcomp is
c.... not in the list, put it in the list of new bcomp's.
 
                  do k=1,nfoundbcomp
                     if (ibcomplist(k).eq.jtetj) then
                        lfoundbcomp(k)=.true.
                        goto 130
                     endif
                  enddo
                  do k=1,newbcomp
                     if (newbcomplist(k).eq.jtetj) goto 130
                  enddo
                  newbcomp=newbcomp+1
                  newbcomplist(newbcomp)=jtetj
 
               else
 
c.... Adjacency to the current material
 
                  if (jtetj.lt.mbndry) then
                     ii=invlochyb(invieltary(1+(jtetj-1)/nef_cmo))
                     if (icomp(ii).eq.0) then
                        icomp(ii)=icurrcomp
                        itop=itop+1
                        ieltstack(itop)=ii
                     endif
 
c.... Adjacency to another material
 
                  else
                     iadjelt=1+(jtetj-mbndry-1)/nef_cmo
                     imat=itetclr(iadjelt)
 
                     if (invmatary(imat).eq.0) then
 
c.... Protected material case.  If imat is in list of known adjacent
c.... protected materials, check off that
c.... it is adjacent to this particular component.  If imat is
c.... not in the list, put it in the list of new imat's.
 
                        do k=1,nfoundprot
                           if (iprotmatlist(k).eq.imat) then
                              lfoundprotmat(k)=.true.
                              goto 130
                           endif
                        enddo
                        do k=1,newprot
                           if (newprotmatlist(k).eq.imat) goto 130
                        enddo
                        newprot=newprot+1
                        newprotmatlist(newprot)=imat
 
                     else
 
c.... Adjacent regular material case.  If this element has a
c.... component number, record this adjacency.
 
                        ii=invlochyb(invieltary(iadjelt))
                        if (icomp(ii).ne.0) then
                           lfoundcomp(icomp(ii))=.true.
                        endif
 
                     endif
                  endif
               endif
 130        continue
 
         enddo
 
c.... We check if all the adjacencies where found while processing this
c.... component.  If some are missing, the tangent cones do not all
c.... touch each other, LALLTOUCH is set to .false., and the tangent
c.... cones are illegal with respect to the Plateau criteria.
 
         do k=1,nfoundbcomp
            if (.not.lfoundbcomp(k)) then
               lalltouch=.false.
            endif
         enddo
         if (icurrcomp.gt.1.and.newbcomp.gt.0) lalltouch=.false.
         do k=1,newbcomp
           ibcomplist(nfoundbcomp+k)=newbcomplist(k)
         enddo
         nfoundbcomp=nfoundbcomp+newbcomp
         do k=1,nfoundbcomp
            lfoundbcomp(k)=.false.
         enddo
 
         do k=1,nfoundprot
            if (.not.lfoundprotmat(k)) then
               lalltouch=.false.
            endif
         enddo
         if (icurrcomp.gt.1.and.newprot.gt.0) lalltouch=.false.
         do k=1,newprot
           iprotmatlist(nfoundprot+k)=newprotmatlist(k)
         enddo
         nfoundprot=nfoundprot+newprot
         do k=1,nfoundprot
            lfoundprotmat(k)=.false.
         enddo
 
         icompadjptr(icurrcomp+1)=icompadjptr(icurrcomp)
         do k=1,icurrcomp-1
            if (lfoundcomp(k)) then
               icompadj(icompadjptr(icurrcomp+1))=k
               icompadjptr(icurrcomp+1)=icompadjptr(icurrcomp+1)+1
            else
               lalltouch=.false.
            endif
         enddo
         do k=1,icurrcomp
            lfoundcomp(k)=.false.
         enddo
 100  continue
      ncomp=icurrcomp
 
c.... Create IFEATURE array containing lists of elements in each
c.... feature (tangent cone).
 
      if (ncomp.gt.0) then
         do i=1,lenlochyb
            rcomp(i)=icomp(i)
            ifeature(i)=i
         enddo
         ascend=1.d0
         call hpsort1(lenlochyb,rcomp,ascend,ifeature)
         icurrcomp=1
         i=1
         do while(icomp(ifeature(i)).lt.icurrcomp)
            i=i+1
         enddo
         ifeatureptr(icurrcomp)=i
         do i=1,lenlochyb
            do while (icomp(ifeature(i)).gt.icurrcomp)
               icurrcomp=icurrcomp+1
               ifeatureptr(icurrcomp)=i
            enddo
         enddo
         ifeatureptr(ncomp+1)=lenlochyb+1
      else
         ifeatureptr(1)=lenlochyb+1
         lalltouch=.true.
      endif
 
c.... Clear INVLOCHYB array.
 
      do i=1,lenlochyb
         if (ip2.eq.0) then
            ii=1+(nodhyb1(i)-1)/maxnen
            invlochyb(ii)=0
         else
            ii=1+(iedgehyb(i)-1)/maxnee2
            invlochyb(ii)=0
         endif
      enddo
 
 
      return
      end
 
      subroutine correcttangcone_lg(cprtname,ip1,ip2,xic,yic,zic
     &   ,epsilonl,cut_length,itet,itetoff,itetclr,itp1,isn1,imt1,icr1
     &   ,icontab,iparent,itettyp,mpno,ieltary,nodhyb,iedgehyb,iproc
     &   ,iprm,ifeature,ifeatureptr,icompadj,icompadjptr,ncomp,theta
     &   ,nadd,ipitadd,len_itadd,ipieadd,len_ieadd,ipiadd,len_iadd
     &   ,ipitpadd,len_itpadd,ipicradd,len_icradd,ipxadd,len_xadd,ipyadd
     &   ,len_yadd,ipzadd,len_zadd,locrefine,locrecolor,ierror)
 
c.... Tangent cone connectivity is incorrect and we take the
c.... course of attempting to correct connectivity by detaching
c.... one of the cones from the point.  Specifically, we take
c.... the material whose cone has smallest angle and recolor with
c.... the adjacent material with largest angle.  If edge lengths
c.... are too long that recoloring of tets violates conservation
c.... of color-volume by too much, we instead emit refinement
c.... data and refrain from recoloring for this iteration.
 
      implicit none
 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
 
      character*32 cprtname
 
      pointer (ipitadd,itadd)
      integer itadd(*),len_itadd
 
      pointer (ipieadd,ieadd)
      integer ieadd(*),len_ieadd
 
      pointer (ipiadd,iadd)
      integer iadd(*),len_iadd
 
      pointer (ipitpadd,itpadd)
      integer itpadd(*),len_itpadd
 
      pointer (ipicradd,icradd)
      integer icradd(*),len_icradd
 
      pointer (ipxadd,xadd)
      real*8 xadd(*)
      integer len_xadd
 
      pointer (ipyadd,yadd)
      real*8 yadd(*)
      integer len_yadd
 
      pointer (ipzadd,zadd)
      real*8 zadd(*)
      integer len_zadd
 
      integer ierror,ifeature(*),ifeatureptr(*),ip1,ip2,nodhyb(*)
     &   ,iedgehyb(*),ieltary(*),itettyp(*),iparent(*),itet(*),itetoff(
     &   *),iproc(*),itp1(*),isn1(*),imt1(*),icr1(*),icontab(50,*),nadd
     &   ,mpno,itetclr(*),iprm(*),icompadj(*),icompadjptr(*),ncomp
 
      logical locrefine,locrecolor
      real*8 xic(*),yic(*),zic(*),epsilonl,cut_length,theta(*)
 
      integer iwinningmat,ilosingcomp,nhybincomp,icompoff,i,ii,iloc,ihyb
     &   ,ityp,iedg,iloc2,j,iparnear,iparfar
     &   ,nodek,ktrip,matk,matk1,nodek1,k1trip,iposnear,k,isfar,icscode
     &   ,iparopp
      real*8 dist,distp,frac,fracproj,fracused,distx,disty,distz,
     &   basevecx,basevecy,basevecz,candvecx,candvecy,candvecz,basesq,
     &   dot
 
      ierror=0
      locrefine=.false.
      locrecolor=.false.
 
      call getrecolorwinlose_lg(ip1,ip2,itetclr,ieltary,nodhyb
     &   ,iedgehyb,ifeature,ifeatureptr,icompadj,icompadjptr,iprm,theta
     &   ,ncomp,iwinningmat,ilosingcomp)
 
      if (ilosingcomp.eq.0) return
 
      nhybincomp=ifeatureptr(ilosingcomp+1)-ifeatureptr(ilosingcomp)
      icompoff=ifeatureptr(ilosingcomp)-1
 
c... Loop through edges of elements in losing component.  Determine
c... which ones have to be refined.  (If none, we will recolor
c... all the elements with the winning material.)
 
      do i=1,nhybincomp
         if (ip2.eq.0) then
            ii=1+(nodhyb(ifeature(i+icompoff))-1)/maxnen
            iloc=nodhyb(ifeature(i+icompoff))-maxnen*(ii-1)
            ihyb=ieltary(ii)
            ityp=itettyp(ihyb)
         else
            ii=1+(iedgehyb(ifeature(i+icompoff))-1)/maxnee2
            ihyb=ieltary(ii)
            ityp=itettyp(ihyb)
            iedg=iedgehyb(ifeature(i+icompoff))-maxnee2*(ii-1)
            iloc=ielmedge1(1,iedg,ityp)
            iloc2=ielmedge1(2,iedg,ityp)
         endif
         do 100 j=1,nelmnee(ityp)
            if (ip2.eq.0) then
               if (ielmedge1(1,j,ityp).eq.iloc) then
                  iparnear=ip1
                  iparfar=iparent(itet(itetoff(ihyb)+ielmedge1(2,j,ityp)
     &               ))
               elseif (ielmedge1(2,j,ityp).eq.iloc) then
                  iparnear=ip1
                  iparfar=iparent(itet(itetoff(ihyb)+ielmedge1(1,j,ityp)
     &               ))
               else
                  goto 100
               endif
            else
               if (iedg.eq.j) goto 100
               if (ielmedge1(1,j,ityp).eq.iloc) then
                  iparnear=iparent(itet(itetoff(ihyb)+iloc))
                  iparfar=iparent(itet(itetoff(ihyb)+ielmedge1(2,j,ityp)
     &               ))
                  iparopp=iparent(itet(itetoff(ihyb)+iloc2))
               elseif (ielmedge1(1,j,ityp).eq.iloc2) then
                  iparnear=iparent(itet(itetoff(ihyb)+iloc2))
                  iparfar=iparent(itet(itetoff(ihyb)+ielmedge1(2,j,ityp)
     &               ))
                  iparopp=iparent(itet(itetoff(ihyb)+iloc))
               elseif (ielmedge1(2,j,ityp).eq.iloc) then
                  iparnear=iparent(itet(itetoff(ihyb)+iloc))
                  iparfar=iparent(itet(itetoff(ihyb)+ielmedge1(1,j,ityp)
     &               ))
                  iparopp=iparent(itet(itetoff(ihyb)+iloc2))
               elseif (ielmedge1(2,j,ityp).eq.iloc2) then
                  iparnear=iparent(itet(itetoff(ihyb)+iloc2))
                  iparfar=iparent(itet(itetoff(ihyb)+ielmedge1(1,j,ityp)
     &               ))
                  iparopp=iparent(itet(itetoff(ihyb)+iloc))
               else
                  goto 100
               endif
            endif
 
c.... We inspect the tag array IPROC.  If IPROC(parent node)=1 or 3
c.... (i.e. low order bit set), then we already processed the edge from
c.... IP1 to the parent node.  If IPROC(parent node)=2 or 3
c.... (i.e. high order bit set), then we already processed the edge from
c.... IP2 to the parent node.
 
            if (ip2.eq.0) then
               if (iproc(iparfar).eq.1) goto 100
               iproc(iparfar)=1
            else
               if (iparnear.eq.ip1) then
                  if (iproc(iparfar).eq.1.or.iproc(iparfar).eq.3) goto
     &               100
                  if (iproc(iparfar).eq.0) then
                     iproc(iparfar)=1
                  elseif (iproc(iparfar).eq.2) then
                     iproc(iparfar)=3
                  endif
               else
                  if (iproc(iparfar).eq.2.or.iproc(iparfar).eq.3) goto
     &               100
                  if (iproc(iparfar).eq.0) then
                     iproc(iparfar)=2
                  elseif (iproc(iparfar).eq.1) then
                     iproc(iparfar)=3
                  endif
               endif
            endif
 
            if (ip2.eq.0) then
               dist=sqrt((xic(iparfar)-xic(iparnear))**2+
     &            (yic(iparfar)-yic(iparnear))**2+
     &            (zic(iparfar)-zic(iparnear))**2)
 
            else
               basevecx=xic(iparopp)-xic(iparnear)
               basevecy=yic(iparopp)-yic(iparnear)
               basevecz=zic(iparopp)-zic(iparnear)
               candvecx=xic(iparfar)-xic(iparnear)
               candvecy=yic(iparfar)-yic(iparnear)
               candvecz=zic(iparfar)-zic(iparnear)
               basesq=basevecx**2+basevecy**2+basevecz**2
               dot=candvecx*basevecx+candvecy*basevecy+candvecz*basevecz
               fracproj=dot/basesq
               fracused=max(zero,min(one,fracproj))
               distx=candvecx-fracused*basevecx
               disty=candvecy-fracused*basevecy
               distz=candvecz-fracused*basevecz
               dist=sqrt(distx**2+disty**2+distz**2)
            endif
 
c... If the edge length is REALLY tiny, don't refine it under any
c circumstances.
 
            if (dist.le.epsilonl) goto 100
 
c... If the edge length is less than CUT_LENGTH, don't refine it
c provided
c... we can verify that IPARFAR does not contain any constraints or
c... materials that are foreign to IPARNEAR.
 
            if (dist.le.cut_length) then
               if (itp1(iparfar).eq.ifitpcup) then
                  nodek=isn1(iparfar)
                  if (itp1(iparnear).ne.ifitpcup) then
                     print*,'Near is not parent!'
                     stop
                  endif
                  ktrip=1
                  do while (nodek.ne.iparfar.and.ktrip.lt.10000)
                     matk=imt1(nodek)
                     nodek1=isn1(iparnear)
                     k1trip=1
                     do while (nodek1.ne.iparnear.and.k1trip.lt.10000)
                        matk1=imt1(nodek1)
                        if (matk1.eq.matk) goto 110
                        nodek1=isn1(nodek1)
                     enddo
                     if (k1trip.ge.10000) then
                        print*,'Bad isn sequence at ',iparnear
                        ierror=1
                        goto 9999
                     endif
                     goto 200   ! mat subset not true
 110                 nodek=isn1(nodek)
                  enddo
                  if (ktrip.ge.10000) then
                     print*,'Bad isn sequence at ',iparfar
                     ierror=1
                     goto 9999
                  endif
               endif
 
               if (icr1(iparfar).ne.0.and.icr1(iparfar).ne.icr1(iparnear
     &            )) then
 
                  if (icr1(iparnear).eq.0) goto 200 ! constraint subset not true.
 
                  iposnear=1
 
                  do k=1,icontab(1,icr1(iparfar))
                     isfar=icontab(k+2,icr1(iparfar))
 140                 continue
                     if (icontab(2+iposnear,icr1(iparnear)).lt.isfar)
     &                  then
                        iposnear=iposnear+1
                        if (iposnear.gt.icontab(1,icr1(iparnear))) then
                           goto 200 ! constraint subset not true.
                        else
                           goto 140
                        endif
                     elseif(icontab(2+iposnear,icr1(iparnear)).gt.isfar)
     &                     then ! constraint subset not true.
                        goto 200
                     endif
                  enddo
               endif
c....If we made it to here, we have edge length less than cut_length and
c
c....IPARFAR doesn't contain any materials or constraints foreign to
c....IPARNEAR, so no refinement.
               goto 100
            endif
 
c.... Emit refinement data for edge.
 200        continue
            locrefine=.true.
            nadd=nadd+1
 
            if (nadd.gt.len_itadd) call mm_ovall('itadd',cprtname,
     &         ipitadd,nadd,100+mpno,len_itadd,1,icscode)
            if (nadd.gt.len_ieadd) call mm_ovall('ieadd',cprtname,
     &         ipieadd,nadd,100+mpno,len_ieadd,1,icscode)
            if (nadd.gt.len_iadd) call mm_ovall('iadd',cprtname,
     &         ipiadd,nadd,100+mpno,len_iadd,1,icscode)
            if (nadd.gt.len_itpadd) call mm_ovall('itpadd',cprtname,
     &         ipitpadd,nadd,100+mpno,len_itpadd,1,icscode)
            if (nadd.gt.len_icradd) call mm_ovall('icradd',cprtname,
     &         ipicradd,nadd,100+mpno,len_icradd,1,icscode)
            if (nadd.gt.len_xadd) call mm_ovall('xadd',cprtname,
     &         ipxadd,nadd,100+mpno,len_xadd,2,icscode)
            if (nadd.gt.len_yadd) call mm_ovall('yadd',cprtname,
     &         ipyadd,nadd,100+mpno,len_yadd,2,icscode)
            if (nadd.gt.len_zadd) call mm_ovall('zadd',cprtname,
     &         ipzadd,nadd,100+mpno,len_zadd,2,icscode)
 
            itadd(nadd)=ihyb
            ieadd(nadd)=j
            iadd(nadd)=0
            distp=min(cut_length,0.5d0*dist)
            frac=distp/dist
            xadd(nadd)=xic(iparfar)*frac+xic(iparnear)*(1.d0-frac)
            yadd(nadd)=yic(iparfar)*frac+yic(iparnear)*(1.d0-frac)
            zadd(nadd)=zic(iparfar)*frac+zic(iparnear)*(1.d0-frac)
 100     continue
      enddo
 
      if (.not.locrefine) then
         locrecolor=.true.
         do i=1,nhybincomp
            if (ip2.eq.0) then
               ii=1+(nodhyb(ifeature(i+icompoff))-1)/maxnen
               ihyb=ieltary(ii)
            else
               ii=1+(iedgehyb(ifeature(i+icompoff))-1)/maxnee2
               ihyb=ieltary(ii)
            endif
            itetclr(ihyb)=iwinningmat
         enddo
      endif
 
 9999 continue
 
c.... Clear IPROC.
 
      do i=1,nhybincomp
         if (ip2.eq.0) then
            ii=1+(nodhyb(ifeature(i+icompoff))-1)/maxnen
            ihyb=ieltary(ii)
            ityp=itettyp(ihyb)
         else
            ii=1+(iedgehyb(ifeature(i+icompoff))-1)/maxnee2
            ihyb=ieltary(ii)
            ityp=itettyp(ihyb)
         endif
         do j=1,nelmnen(ityp)
            iproc(iparent(itet(itetoff(ihyb)+j)))=0
         enddo
      enddo
 
      return
      end
 
      subroutine getrecolorwinlose_lg(ip1,ip2,itetclr,ieltary,nodhyb
     &   ,iedgehyb,ifeature,ifeatureptr,icompadj,icompadjptr,iprm,theta
     &   ,ncomp,iwinningmat,ilosingcomp)
 
      implicit none
 
      include 'local_element.h'
 
      integer iwinningmat,ilosingcomp,ncomp,iprm(*),icompadj(*)
     &   ,icompadjptr(*),ip1,ip2,nodhybind,nodhyb(*),iedgehyb(*)
     &   ,ifeature(*),ifeatureptr(*),ieltary(*),itetclr(*)
      real*8 theta(*)
 
      integer i,iwinningcomp,j,icandcomp,k,ii,ihyb,iedghybind
      real*8 ascend,thetamax
 
      do i=1,ncomp
         iprm(i)=i
      enddo
      ascend=1.0d0
      call hpsort1(ncomp,theta,ascend,iprm)
 
      do i=1,ncomp
         ilosingcomp=iprm(i)
         thetamax=-1.d+99
         iwinningcomp=0
         if (icompadjptr(ilosingcomp).ne.icompadjptr(ilosingcomp+1))
     &      then
            do j=icompadjptr(ilosingcomp),icompadjptr(ilosingcomp+1)-1
               icandcomp=icompadj(j)
               if (theta(icandcomp).gt.thetamax) then
                  thetamax=theta(icandcomp)
                  iwinningcomp=icandcomp
               endif
            enddo
         endif
         do j=ilosingcomp+1,ncomp
            do k=icompadjptr(j),icompadjptr(j+1)-1
               if (icompadj(k).eq.ilosingcomp) then
                  icandcomp=j
                  if (theta(icandcomp).gt.thetamax) then
                     thetamax=theta(icandcomp)
                     iwinningcomp=icandcomp
                  endif
               endif
            enddo
         enddo
         if (iwinningcomp.ne.0) then
            if (ip2.eq.0) then
               nodhybind=ifeature(ifeatureptr(iwinningcomp))
               ii=1+(nodhyb(nodhybind)-1)/maxnen
               ihyb=ieltary(ii)
               iwinningmat=itetclr(ihyb)
            else
               iedghybind=ifeature(ifeatureptr(iwinningcomp))
               ii=1+(iedgehyb(iedghybind)-1)/maxnee2
               ihyb=ieltary(ii)
               iwinningmat=itetclr(ihyb)
            endif
            return
         endif
      enddo
 
      print*,'Weird adjacency means norecolor at ',ip1,ip2
      ilosingcomp=0
      iwinningmat=0
 
      return
      end
 
      subroutine getedghyb_lg(ieltary,itettyp,nodhyb1,len1,nodhyb2,len2
     &   ,iedgehyb,len12)
 
      implicit none
 
      include 'local_element.h'
 
      integer nodhyb1(*),len1,nodhyb2(*),len2,iedgehyb(*)
     &   ,ieltary(*),itettyp(*)
 
      integer len12,ipos2,i,it1,it2,loc1,ihyb1,ityp1,loc2,iv1
     &   ,iv2,j
 
c.... We assume that nodhyb1, nodhyb2 give data in ascending order.
 
      len12=0
      ipos2=1
      do 50 i=1,len1
         it1=1+(nodhyb1(i)-1)/maxnen
 
 30      continue
         it2=1+(nodhyb2(ipos2)-1)/maxnen
         if (it2.lt.it1) then
            ipos2=ipos2+1
            if (ipos2.gt.len2) then
               goto 40
            else
               goto 30
            endif
         elseif (it2.eq.it1) then
            len12=len12+1
            loc1=nodhyb1(i)-maxnen*(it1-1)
            ihyb1=ieltary(it1)
            ityp1=itettyp(ihyb1)
            loc2=nodhyb2(ipos2)-maxnen*(it2-1)
            do j=1,nelmnee(ityp1)
               iv1=ielmedge1(1,j,ityp1)
               iv2=ielmedge1(2,j,ityp1)
               if ((loc1.eq.iv1.and.loc2.eq.iv2).or.(loc1.eq
     &            .iv2.and.loc2.eq.iv1)) then
                  iedgehyb(len12)=(it1-1)*maxnee2+j
                  goto 50
               endif
            enddo
            print*,'Error: getedghyb_lg'
            stop
         endif
 50   continue
 
 40   continue
      return
      end
 
      subroutine incrangle_lg(xic,yic,zic,itet,itetoff,totangle,iloc
     &   ,iloc2,ihyb,ityp)
 
c.... If ILOC2=0, this subroutine increments TOTANGLE by the solid
c.... angle at local node ILOC in tetrahedron IHYB.  If ILOC2 is
c.... nonzero, then we instead increment TOTANGLE by the
c.... dihedral angle at edge ILOC-ILOC2 in tetrahedron IHYB.
c.... Only these two kinds of angles involving 3D tets are
c.... supported at this time.  I.e. modifications are
c.... required for non 3-D or non-simplicial elements.
 
      implicit none
 
      include 'consts.h'
      include 'local_element.h'
 
      real*8 totangle,xic(*),yic(*),zic(*)
      integer iloc,iloc2,ihyb,ityp,itet(*),itetoff(*)
 
      integer i,j,noda,nodb,nodc
      real*8 xn(maxnef),yn(maxnef),zn(maxnef),rlen,dot12,dot23,dot31
     &   ,solidangle,dihangle
 
      real*8 pi
      parameter (pi=3.1415926535897932d0)
 
      include 'statementfunctions.h'
 
      if (iloc2.eq.0) then
 
c.... Solid angle case.  Find the three faces that contain ILOC,
c.... compute their outward normals, N_1, N_2, N_3.  Then solid angle
c.... is
c....     acos(-N_1*N_2) + acos(-N_2*N_3) + acos(-N_3*N_1) - pi.
 
c.... Due to clever ordering, the three faces required are
c.... {1,2,3,4}\ILOC.
 
         i=0
         do 10 j=1,nelmnef(ityp)
            if (iloc.eq.j) goto 10
            i=i+1
            noda=itet(itetoff(ihyb)+ielmface1(1,j,ityp))
            nodb=itet(itetoff(ihyb)+ielmface1(2,j,ityp))
            nodc=itet(itetoff(ihyb)+ielmface1(3,j,ityp))
            xn(i)=dcrosx(xic(noda),yic(noda),zic(noda),
     &         xic(nodb),yic(nodb),zic(nodb),
     &         xic(nodc),yic(nodc),zic(nodc))
            yn(i)=dcrosy(xic(noda),yic(noda),zic(noda),
     &         xic(nodb),yic(nodb),zic(nodb),
     &         xic(nodc),yic(nodc),zic(nodc))
            zn(i)=dcrosz(xic(noda),yic(noda),zic(noda),
     &         xic(nodb),yic(nodb),zic(nodb),
     &         xic(nodc),yic(nodc),zic(nodc))
            rlen=sqrt(xn(i)**2+yn(i)**2+zn(i)**2)
            xn(i)=xn(i)/safe(rlen)
            yn(i)=yn(i)/safe(rlen)
            zn(i)=zn(i)/safe(rlen)
 10      continue
         dot12=xn(1)*xn(2)+yn(1)*yn(2)+zn(1)*zn(2)
         dot23=xn(2)*xn(3)+yn(2)*yn(3)+zn(2)*zn(3)
         dot31=xn(3)*xn(1)+yn(3)*yn(1)+zn(3)*zn(1)
         solidangle=acos(-dot12)+acos(-dot23)+acos(-dot31)-pi
         totangle=totangle+solidangle
      else
 
c.... Dihedral angle case.  Find the two faces that contain ILOC and
c.... ILOC2.  Compute two outward normals N_1, N_2.  Then dihedral
c.... angle is
c....           acos(-N_1*N_2).
 
c.... Due to clever ordering, the two faces required are
c.... {1,2,3,4}\ILOC\ILOC2.
 
         i=0
         do 20 j=1,nelmnef(ityp)
            if (iloc.eq.j.or.iloc2.eq.j) goto 20
            i=i+1
            noda=itet(itetoff(ihyb)+ielmface1(1,j,ityp))
            nodb=itet(itetoff(ihyb)+ielmface1(2,j,ityp))
            nodc=itet(itetoff(ihyb)+ielmface1(3,j,ityp))
            xn(i)=dcrosx(xic(noda),yic(noda),zic(noda),
     &         xic(nodb),yic(nodb),zic(nodb),
     &         xic(nodc),yic(nodc),zic(nodc))
            yn(i)=dcrosy(xic(noda),yic(noda),zic(noda),
     &         xic(nodb),yic(nodb),zic(nodb),
     &         xic(nodc),yic(nodc),zic(nodc))
            zn(i)=dcrosz(xic(noda),yic(noda),zic(noda),
     &         xic(nodb),yic(nodb),zic(nodb),
     &         xic(nodc),yic(nodc),zic(nodc))
            rlen=sqrt(xn(i)**2+yn(i)**2+zn(i)**2)
            xn(i)=xn(i)/safe(rlen)
            yn(i)=yn(i)/safe(rlen)
            zn(i)=zn(i)/safe(rlen)
 20      continue
         dot12=xn(1)*xn(2)+yn(1)*yn(2)+zn(1)*zn(2)
         dihangle=acos(-dot12)
         totangle=totangle+dihangle
      endif
      return
      end
