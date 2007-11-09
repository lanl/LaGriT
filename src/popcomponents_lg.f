*dk,popcomponents_lg
      subroutine popcomponents_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        POPCOMPONENTS_LG computes the topology of a LaGriT mesh
C        (connected material volumes, surfaces bounding volumes,
C        lines bounding surfaces, and special points bounding
C        lines).  A topological component that is 'small' (less than a
C        volume, surface, or length tolerance) is 'popped' (eliminated
C        through a combination of edge refinement and element
C        recoloring) if either (i) it consists of pieces which have
C        velocities (according to a specified CMO velocity field) which
C        indicate that the component will collapse in the next time
C        TOLDT, or (ii) the velocity field is absent or indicates the component
C        is nonexpanding AND the linear spatial dimension of the
C        component is less than TOL_SMALL_DIST AND the incident solid
C        angles of components adjacent to the given component indicate
C        that a topological change should occur (i.e. it would tend to
C        disappear in a very short time under the action of mean
C        curvature motion and surface tension present in a soap froth).
C
C        General LaGriT users may find that the action of this routine
C        will have a pleasing affect on the mesh, eliminating tiny
C        components that would be a nuisance in many mesh applications.
C        Also, general users that would prefer to leave the mesh
C        unchanged can still obtain a useful printout of the
C        topological components and their connections to each other
C        using the 'LIST' option below.
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
C        $Log: popcomponents_lg.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.11   08 Feb 2006 14:35:32   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.10   07 Jan 2002 13:59:56   dcg
CPVCS    fix typo
CPVCS
CPVCS       Rev 1.9   07 Jan 2002 13:53:10   dcg
CPVCS     add error return argument to refine_edge_add_tet call
CPVCS
CPVCS       Rev 1.8   10 Oct 2001 16:23:46   kuprat
CPVCS    Corrected bug where we didn't disregard volumes outside
CPVCS    of a non-full pset of points.
CPVCS
CPVCS       Rev 1.7   Wed Dec 08 03:48:58 1999   kuprat
CPVCS    We now look for SKELETONONLY and output skeleton in this case.
CPVCS
CPVCS       Rev 1.6   Mon Dec 06 11:03:50 1999   kuprat
CPVCS    New LOCVERBOSITY variable to control screen output.
CPVCS
CPVCS       Rev 1.5   Wed Dec 01 16:07:22 1999   kuprat
CPVCS    Fixed IELTSTACK memory management error.  Added effective 'MIT'
CPVCS    boundary treatment---this encourages non-breakthrough of grains
CPVCS    to boundary and generally decreases number of surviving grains.
CPVCS
CPVCS       Rev 1.4   Wed Nov 10 14:44:40 1999   dcg
CPVCS    make xmin,xmax,ymin,ymax,zmin,zmax local variables
CPVCS
CPVCS       Rev 1.3   Tue Sep 28 13:21:20 1999   kuprat
CPVCS    Due to observed events where a 'doomed' component can
CPVCS    actually slightly increase in measure, we have liberalized
CPVCS    the annihilation criterion to tolerate very small
CPVCS    positive growth rates and still allow annihilation.
CPVCS
CPVCS       Rev 1.2   Mon Sep 27 23:09:02 1999   kuprat
CPVCS    Changed 'stop' to 'warning' in case of edge-multiply-connected
CPVCS    volumes in DMAXDEPTH_LG.
CPVCS
CPVCS       Rev 1.1   Wed Sep 22 13:51:38 1999   kuprat
CPVCS    Added DMAXDEPTH_LG and fixed bug in GETNEXTILINESEG.
CPVCS
CPVCS       Rev 1.0   Thu Aug 26 14:42:06 1999   kuprat
CPVCS    Initial revision.
C
C #####################################################################
c
c   FORMAT:
c
c   POPCOMPONENTS / CUT_LENGTH / TOL_SMALL_DIST / TOLDT / VELFIELD /
c              ifirst,ilast,istride / [skeletononly] / [cmoskeleton]
c
C  If criteria involving TOL_SMALL_DIST, TOLDT, or the velocity field
C  VELFIELD are satisfied, we will eliminate a component.  The
C  elimination process may involve incisions around the component of
C  length less than or equal to CUT_LENGTH.  If SKELETONONLY is specified,
C  we output the topological 'skeleton' structure of the mesh to CMOSKELETON and
C  take no action.
C
************************************************************************
 
      implicit none
 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
 
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
 
      integer imsginsave(3), msgtypesave(3)
      real*8 xmsginsave(3)
      character*32 cmsginsave(3)
 
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
      pointer (ipvels,vels)
      real*8 vels(3,*)
 
c.... Local pointered arrays.
 
      pointer (ipmpary,mpary)
      integer mpary(*),len_mpary
 
      pointer (ipireal1,ireal1)
      integer ireal1(*),len_ireal1
 
      pointer (ipiedges_first,iedges_first)
      integer iedges_first(*),len_iedges_first
 
      pointer (ipiedges,iedges)
      integer iedges(*),len_iedges
 
      pointer (ipiedgisurfelt,iedgisurfelt)
      integer iedgisurfelt(*),len_iedgisurfelt
 
      pointer (ipieltary,ieltary)
      integer ieltary(*),len_ieltary
 
      pointer (ipinvieltary,invieltary)
      integer invieltary(*),len_invieltary
 
      pointer (ipiparent,iparent)
      integer iparent(*),len_iparent
 
      pointer (ipinvmpary,invmpary)
      integer invmpary(*),len_invmpary
 
      pointer (ipinvmatary,invmatary)
      integer invmatary(*),len_invmatary
 
      pointer (ipmatary,matary)
      integer matary(*),len_matary
 
      pointer (ipicomp,icomp)
      integer icomp(*),len_icomp
 
      pointer (ipieltstack,ieltstack)
      integer ieltstack(*),len_ieltstack
 
      pointer (ipisurfelt_,isurfelt_)
      integer isurfelt_(*),len_isurfelt_
 
      pointer (ipisurfeltsrc_,isurfeltsrc_)
      integer isurfeltsrc_(*),len_isurfeltsrc_
 
      pointer (ipiprm,iprm)
      integer iprm(*),len_iprm
 
      pointer (ipiprm1,iprm1)
      integer iprm1(*),len_iprm1
 
      pointer (ipisurfelt,isurfelt)
      integer isurfelt(*),len_isurfelt
 
      pointer (ipisurfeltsrc,isurfeltsrc)
      integer isurfeltsrc(2,*),len_isurfeltsrc
 
      pointer (ipifeatureptr,ifeatureptr)
      integer ifeatureptr(*),len_ifeatureptr
 
      pointer (ipifeature,ifeature)
      integer ifeature(*),len_ifeature
 
      pointer (ipisrccomp,isrccomp)
      integer isrccomp(*),len_isrccomp
 
      pointer (ipibndcomp,ibndcomp)
      integer ibndcomp(*),len_ibndcomp
 
      integer md
      parameter (md=3)
      pointer (ipikey,ikey)
      integer ikey(md,*),len_ikey
 
      pointer (ipilinedge,ilinedge)
      integer ilinedge(*),len_ilinedge
 
      pointer (ipilinedgeptr,ilinedgeptr)
      integer ilinedgeptr(*),len_ilinedgeptr
 
      pointer (ipilinedgesrc,ilinedgesrc)
      integer ilinedgesrc(*),len_ilinedgesrc
 
      pointer (ipisrc,isrc)
      integer isrc(*),len_isrc
 
      pointer (ipiforward,iforward)
      integer iforward(*),len_iforward
 
      pointer (ipibackward,ibackward)
      integer ibackward(*),len_ibackward
 
      pointer (ipiboundary,iboundary)
      integer iboundary(*),len_iboundary
 
      pointer (ipiboundaryptr,iboundaryptr)
      integer iboundaryptr(*),len_iboundaryptr
 
      pointer (ipinvboundary,invboundary)
      integer invboundary(*),len_invboundary
 
      pointer (ipinvboundaryptr,invboundaryptr)
      integer invboundaryptr(*),len_invboundaryptr
 
      pointer (iplockout,lockout)
      logical lockout(*)
      integer len_lockout
 
      pointer (ipicategory,icategory)
      integer icategory(*),len_icategory
 
      pointer (ipvolume,volume)
      real*8 volume(*)
      integer len_volume
 
      pointer (ipdiameter,diameter)
      real*8 diameter(*)
      integer len_diameter
 
      pointer (ipdvoldt,dvoldt)
      real*8 dvoldt(*)
      integer len_dvoldt
 
      pointer (ipestcolltime,estcolltime)
      real*8 estcolltime(*)
      integer len_estcolltime
 
      pointer (ipinvicompnod,invicompnod)
      integer invicompnod(*),len_invicompnod
 
      pointer (ipicompnod,icompnod)
      integer icompnod(*),len_icompnod
 
      pointer (ipivcomp,ivcomp)
      integer ivcomp(*),len_ivcomp
 
      pointer (ipthetaminus,thetaminus)
      real*8 thetaminus(*)
      integer len_thetaminus
 
      pointer (ipthetaplus,thetaplus)
      real*8 thetaplus(*)
      integer len_thetaplus
 
      pointer (ipicompstack,icompstack)
      integer icompstack(*),len_icompstack
      character*32 cicompstack
 
      pointer (ipicodimstack,icodimstack)
      integer icodimstack(*),len_icodimstack
      character*32 cicodimstack
 
      pointer (ipipluslist,ipluslist)
      integer ipluslist(*),len_ipluslist
      character*32 cipluslist
 
      pointer (ipiminuslist,iminuslist)
      integer iminuslist(*),len_iminuslist
      character*32 ciminuslist
 
      pointer (ipicomphyb,icomphyb)
      integer icomphyb(*),len_icomphyb
 
      pointer (iplistout,listout)
      integer listout(*),len_listout
      character*32 clistout
 
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
 
      pointer (ipinvifeature,invifeature)
      integer invifeature(*),len_invifeature
 
      pointer (ipdir,dir)
      real*8 dir(*)
      integer len_dir
 
      pointer (ipdepth,depth)
      real*8 depth(*)
      integer len_depth
 
c.... Local pointered arrays accessed in an older style.
 
      pointer (ipnodhyb,nodhyb)
      integer nodhyb(*)
 
      pointer (ipnodhyboff,nodhyboff)
      integer nodhyboff(*)
 
      integer len_iout,len_ktet,len_iout_first,len_icompmat,len_icompicr
     &   ,len_isrcnew,len_itarglist,len_vecx,len_vecy,len_vecz
 
      logical lrefine,locrefine,lrecolor,locrecolor,lskeletononly,
     &   lsamecons_lg,lneed_to_order,lbarely
 
      real*8 epsilonl,epsilona,epsilonv,cut_length,tolsmalldist
 
      character*8 cglobal, cdefault
      character*32 cmo,cprtname,psetname,cmoskeleton,cvels,isubname
      character*132 logmess
 
      integer ierror,nnodes,length,icmotype,nelements,mbndry,icscode,flag
     &   ,ieltno,i,j,node,nod1,ihyb,ityp,k,j1,mpno,ierrdum,nod,nef_cmo
     &   ,jtetj,nsurfelt,iedg,nod2,minpar,maxpar,next,numedges,ierrw
     &   ,matmax,imat,nmat,ifac,k1,itop,iopp,ihybj,ifacj,itypj,i2
     &   ,nadd,node1,ncomp,isprev,iscurr,nconbnd,jtetcurr,flag
     &   ,icurr,itrip,ivcompstart,iscompstart,ilcompstart,isppcompstart
     &   ,icurrcomp,nsurfelt_,ielt,isurfeltcurr,ifacopp,iclrcurr
     &   ,iclropp,isurfeltwrite,icand,iprevsurfelt,nextfeature,itemp(10)
     &   ,ipos,ncompdata,numilinedge,i1,numspecial_,i1orig,i2orig,ndeg
     &   ,nforward,i3,nbackward,iprev,numspecial,ivol,nod3,nod4,ncompnod
     &   ,nodk,ncompnod_,icodim,npluslist,nminuslist,ncomphyb
     &   ,ilosingcomp,ilosingmat,iwinningcomp,iwinningmat,iplusmin
     &   ,iminusmin,numbdryloser,numbdrycand,ncompstack,iposcand
     &   ,itargcodim,nlistout,icharlnf,iposlist,jbegin,jend,nodbegin
     &   ,itmp,nelts,jvol,jsurf,iprevikey1,iprevikey2,jcmp,jclr
 
      logical lsamesurf,lspecial,lvels_exist,lchange,laccept
 
      real*8 ascend,x21,y21,z21,x31,y31,z31,x41,y41,z41,volcurr,v21x
     &   ,v21y,v21z,v31x,v31y,v31z,v41x,v41y,v41z,dvoldtcurr
     &   ,areax,areay,areaz,areacurr,ratex,ratey,ratez,dadtcurr
     &   ,rlengthcurr,dldtcurr,toldt,thetaplusmin
     &   ,thetaminusmin,dimx,dimy,dimz,dimmax,dimmin,dimmed,dmaxdepth_lg
     &   ,fracincrease,diamadj,voladj
 
      real*8 xmin,ymin,zmin,xmax,ymax,zmax
 
      real*8 dnearfactor
      parameter (dnearfactor=10.d0)
 
      logical lcompresswherepossible
      parameter (lcompresswherepossible=.true.)
 
      integer locdebug,locverbosity,maxtrip
      parameter (locdebug=0,locverbosity=1,maxtrip=10)
 
      real*8 a2,b2,c2,a3,b3,c3,a4,b4,c4,det
 
      real*8 crosx,crosy,crosz,x1,y1,z1,x2,y2,z2,x3,y3,z3
 
c.... statement functions for the components of the cross product
c.... ((x2,y2,z2)-(x1,y1,z1)) x ((x3,y3,z3)-(x1,y1,z1)) .
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
c.... statement function for 3x3 determinant.
 
      det(a2,b2,c2,a3,b3,c3,a4,b4,c4) = a2*(b3*c4-b4*c3)+
     & a3*(b4*c2-b2*c4)+a4*(b2*c3-b3*c2)
 
      len_mpary=0
      len_ireal1=0
      len_invmatary=0
      len_matary=0
      len_iparent=0
      len_invmpary=0
      len_ieltary=0
      len_invieltary=0
      len_icomp=0
      len_isurfelt_=0
      len_isurfeltsrc_=0
      len_iprm=0
      len_iprm1=0
      len_isurfelt=0
      len_isurfeltsrc=0
      len_ifeatureptr=0
      len_ifeature=0
      len_ikey=0
      len_iedges=0
      len_iedges_first=0
      len_iedgisurfelt=0
      len_isrccomp=0
      len_ibndcomp=0
      len_ieltstack=0
      len_ilinedgeptr=0
      len_ilinedge=0
      len_ilinedgesrc=0
      len_isrc=0
      len_iforward=0
      len_ibackward=0
      len_iboundaryptr=0
      len_iboundary=0
      len_invboundaryptr=0
      len_invboundary=0
      len_lockout=0
      len_icategory=0
      len_volume=0
      len_diameter=0
      len_dvoldt=0
      len_estcolltime=0
      len_ivcomp=0
      len_invicompnod=0
      len_icompnod=0
      len_thetaminus=0
      len_thetaplus=0
      len_icompstack=0
      len_icodimstack=0
      len_ipluslist=0
      len_iminuslist=0
      len_icomphyb=0
      len_listout=0
      len_itadd=0
      len_ieadd=0
      len_iadd=0
      len_itpadd=0
      len_icradd=0
      len_xadd=0
      len_yadd=0
      len_zadd=0
      len_iout=0
      len_ktet=0
      len_iout_first=0
      len_isrcnew=0
      len_icompmat=0
      len_icompicr=0
      len_itarglist=0
      len_vecx=0
      len_vecy=0
      len_vecz=0
      len_invifeature=0
      len_dir=0
      len_depth=0
      cicompstack='icompstack'
      cicodimstack='icodimstack'
      clistout='listout'
      cipluslist='ipluslist'
      ciminuslist='iminuslist'
 
      isubname = 'popcomponents_lg'
      cprtname = 'popcomponents_lg'
      cglobal='global'
      cdefault='default'
      ierror=0
 
c.... If we return with XDUM103=0., we are signalling that this
c.... subroutine has made no changes to the mesh.  If XDUM103=1.,
c.... we are signalling that it has.
 
      xdum103=zero
 
C  Check that user has specified a valid mesh object.
 
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'POPCOMPONENTS_LG: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      call setsize()
      call getsize(xmin,xmax,ymin,ymax,zmin,zmax,epsilona,epsilonv)
      dimx=xmax-xmin
      dimy=ymax-ymin
      dimz=zmax-zmin
      dimmax=max(dimx,dimy,dimz)
      dimmin=min(dimx,dimy,dimz)
      dimmed=dimx+dimy+dimz-dimmax-dimmin
 
      call get_epsilon('epsilonl',epsilonl)
 
      if (nwds.lt.2.or.msgtype(2).le.0.or.cmsgin(2)(1:5).eq.'-def-')
     &   then
         cut_length=.01*sqrt(dimx**2+dimy**2+dimz**2)
      else
         call test_argument_type(1,2,2,imsgin,xmsgin,cmsgin,msgtype,
     &      nwds)
         cut_length=xmsgin(2)
      endif
 
      if (nwds.lt.3.or.msgtype(3).le.0.or.cmsgin(3)(1:5).eq.'-def-')
     &   then
         tolsmalldist=.01*sqrt(dimx**2+dimy**2+dimz**2)
      else
         call test_argument_type(1,2,3,imsgin,xmsgin,cmsgin,msgtype,
     &      nwds)
         tolsmalldist=xmsgin(3)
      endif
 
      if (nwds.lt.4.or.msgtype(4).le.0.or.cmsgin(4)(1:5).eq.'-def-')
     &   then
         toldt=0.
      else
         call test_argument_type(1,2,4,imsgin,xmsgin,cmsgin,msgtype,
     &      nwds)
         toldt=xmsgin(4)
      endif
 
      if (nwds.lt.5.or.msgtype(5).le.0.or.cmsgin(5)(1:5).eq.'-def-')
     &   then
         cvels='vels'
         call cmo_get_info(cvels,cmo,ipvels,length,icmotype,ierror)
         if (ierror.eq.0) then
            lvels_exist=.true.
         else
            lvels_exist=.false.
         endif
      else
         cvels=cmsgin(5)(1:icharlnf(cmsgin(5)))
         call cmo_get_info(cvels,cmo,ipvels,length,icmotype,ierror)
         if (ierror.eq.0) then
            lvels_exist=.true.
         else
            lvels_exist=.false.
            write(logmess,'(a)')
     &         'POPCOMPONENTS_LG: ',cvels,' not a valid velocity field.'
            call writloga('default',0,logmess,0,ierrw)
         endif
      endif
 
c.... Save PSET info, since DOTASKX3D destroys it.
 
      if (nwds.ge.8) then
         do i=1,3
            imsginsave(i)=imsgin(i+5)
            xmsginsave(i)=xmsgin(i+5)
            cmsginsave(i)=cmsgin(i+5)
            msgtypesave(i)=msgtype(i+5)
         enddo
      else
         msgtypesave(1)=1
         imsginsave(1)=1
         imsginsave(2)=0
         imsginsave(3)=0
      endif
 
      lskeletononly=.false.
      if (nwds.lt.9.or.msgtype(9).le.0.or.cmsgin(9)(1:5).eq.'-def-')
     &   then
      else
         call test_argument_type(1,3,9,imsgin,xmsgin,cmsgin,msgtype,
     *      nwds)
         if (cmsgin(9)(1:12).eq.'skeletononly') then
            lskeletononly=.true.
            if (nwds.lt.10.or.msgtype(10).le.0.or.cmsgin(10)(1:5).eq
     &         .'-def-')then
               cmoskeleton='cmoskeleton'
            else
               call test_argument_type(1,3,10,imsgin,xmsgin,cmsgin
     &            ,msgtype,nwds)
               cmoskeleton=cmsgin(10)
            endif
         endif
      endif
 
c.... Main loop.  In each pass we possibly recolor or refine
c.... mesh in several (nonoverlapping) places to "pop
c.... nonphysical components."
 
      if (locdebug.ge.1) then
         call dotaskx3d
     &      ('dump/gmv/gmvprepopcomponents/3dmesh/ascii ; finish',ierror
     &      )
         call dotaskx3d('dump/x3d/x3dprepopcomponents ; finish',
     &      ierror)
      endif
 
      do itrip=1,maxtrip
 
c$$$         if (itrip.ne.1) cut_length=1.d6
 
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
         call cmo_get_info('vels',cmo,ipvels,length,icmotype,ierror)
         if (ierror.eq.0) then
            lvels_exist=.true.
         else
            lvels_exist=.false.
         endif
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
 
         if (nnodes.gt.len_mpary) call mm_ovall('mpary',cprtname,
     &      ipmpary,nnodes,100,len_mpary,1,icscode)
 
         call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &      ipmpary,mpno,psetname,ierror)
 
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
         if (nnodes.gt.len_ireal1) call mm_ovall('ireal1',cprtname,
     &      ipireal1,nnodes,100,len_ireal1,1,icscode)
 
         call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
         if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
 
c.... We derive MATARY, the array of materials from the incoming PSET
c.... in MPARY.
 
         matmax=0
         do i=1,nnodes
            if (ireal1(i).eq.1) then
               matmax=max(matmax,imt1(i))
            endif
         enddo
 
         if (matmax.gt.len_invmatary) call mm_ovall('invmatary',cprtname
     &      ,ipinvmatary,matmax,0,len_invmatary,1,icscode)
 
         if (matmax.gt.len_matary) call mm_ovall('matary',cprtname,
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
 
c     ..................................................................
c     find the parents of each node.
c
         if (nnodes.gt.len_iparent) call mm_ovall('iparent',cprtname,
     &      ipiparent,nnodes,100,len_iparent,1,icscode)
 
         call unpackpc(nnodes,itp1,isn1,iparent)
 
c.... change mass point array to contain all parent nodes in
c.... materials in MATARY.
 
         if (nnodes.gt.len_invmpary) call mm_ovall('invmpary',cprtname,
     &      ipinvmpary,nnodes,100,len_invmpary,1,icscode)
 
         do i=1,nnodes
            invmpary(i)=0
         enddo
 
         mpno=0
         do k=1,nnodes
            if (ireal1(k).eq.1.and.
     &         invmatary(imt1(k)).ne.0) then
               nod=iparent(k)
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
         enddo
 
         if (mpno.le.0) goto 9999
 
c.... Compute list of elements that are involved in these computations
c.... (ie contain at least one node in MPARY).
 
         ieltno=0
         do 40 i=1,nelements
            do j=1,nelmnen(itettyp(i))
               if (invmpary(itet(j+itetoff(i))).ne.0) then
                  ieltno=ieltno+1
                  if (ieltno.gt.len_ieltary) call mm_ovall('ieltary'
     &               ,cprtname,ipieltary,ieltno,100+mpno,len_ieltary,1
     &               ,icscode)
                  ieltary(ieltno)=i
                  goto 40
               endif
            enddo
 40      continue
 
c... Compute inverse of IELTARY relation.
 
         if (nelements.gt.len_invieltary) call mm_ovall('invieltary'
     &      ,cprtname,ipinvieltary,nelements,100,len_invieltary,1
     &      ,icscode)
 
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
     &      itetoff,itettyp,iparent,invmpary,cprtname,ipnodhyb
     &      ,ipnodhyboff)
 
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
            if (locverbosity.ge.1) print*,'Reordering ICONTAB entries.'
            do i=1,nconbnd
               call hpsorti(icontab(1,i),icontab(3,i))
            enddo
         endif
 
 
c.... Loop thru tetrahedra and look for connected sets of material type,
c.... defining the first part of IFEATURE.  Also compute list of
c.... "surface elements" (i.e. interface triangles) in preparation
c.... for the second part of IFEATURE:  connected sets of triangles.
 
         ivcompstart=1
         icurrcomp=ivcompstart-1
         nsurfelt_=0
         if (ieltno.gt.len_icomp) call mm_ovall('icomp',cprtname
     &      ,ipicomp,ieltno,100,len_icomp,1,icscode)
         do i=1,ieltno
            icomp(i)=0
         enddo
 
         do 1010 i=1,ieltno
            if (icomp(i).ne.0) goto 1010
 
c... There is a buffer layer of protected material elements around
c... the nonprotected (active) region.  Make sure this component
c... is of an active material.
            imat=itetclr(ieltary(i))
            if (invmatary(imat).eq.0) goto 1010
 
            icurrcomp=icurrcomp+1
            icomp(i)=icurrcomp
 
c... Put on stack
            itop=1
 
            if (itop.gt.len_ieltstack) call mm_ovall('ieltstack'
     &         ,cprtname,ipieltstack,itop,1000,len_ieltstack,1,icscode)
 
            ieltstack(itop)=i
 
            do while (itop.gt.0)
 
c... Pop stack
 
               ielt=ieltstack(itop)
               itop=itop-1
               ihyb=ieltary(ielt)
               ityp=itettyp(ihyb)
 
               do j=1,nelmnef(ityp)
                  jtetj=jtet(j+jtetoff(ihyb))
 
                  if (jtetj.eq.mbndry) then
                     isurfeltcurr=(ihyb-1)*nef_cmo+j
                     nsurfelt_=nsurfelt_+1
                     if (nsurfelt_.gt.len_isurfelt_) call mm_ovall
     &                  ('isurfelt_',cprtname,ipisurfelt_,nsurfelt_,mpno
     &                  +100,len_isurfelt_,1,icscode)
                     if (nsurfelt_.gt.len_isurfeltsrc_) call mm_ovall
     &                  ('isurfeltsrc_',cprtname,ipisurfeltsrc_
     &                  ,nsurfelt_,mpno+100,len_isurfeltsrc_,1,icscode)
                     isurfelt_(nsurfelt_)=isurfeltcurr
                     isurfeltsrc_(nsurfelt_)=icurrcomp
                  elseif (jtetj.gt.mbndry) then
                     iopp=1+(jtetj-mbndry-1)/nef_cmo
                     ifacopp=jtetj-mbndry-(iopp-1)*nef_cmo
                     iclrcurr=itetclr(ihyb)
                     iclropp=itetclr(iopp)
                     if (iclrcurr.gt.iclropp) then
                        isurfeltwrite=(ihyb-1)*nef_cmo+j
                     else
                        isurfeltwrite=jtetj-mbndry
                     endif
                     nsurfelt_=nsurfelt_+1
                     if (nsurfelt_.gt.len_isurfelt_) call mm_ovall
     &                  ('isurfelt_',cprtname,ipisurfelt_,nsurfelt_,mpno
     &                  +100,len_isurfelt_,1,icscode)
                     if (nsurfelt_.gt.len_isurfeltsrc_) call mm_ovall
     &                  ('isurfeltsrc_',cprtname,ipisurfeltsrc_
     &                  ,nsurfelt_,mpno+100,len_isurfeltsrc_,1,icscode)
                     isurfelt_(nsurfelt_)=isurfeltwrite
                     isurfeltsrc_(nsurfelt_)=icurrcomp
                  else
                     iopp=1+(jtetj-1)/nef_cmo
                     ifacopp=jtetj-(iopp-1)*nef_cmo
                     icand=invieltary(iopp)
                     if (icand.ne.0) then
                        if (icomp(icand).eq.0) then
                           icomp(icand)=icurrcomp
                           itop=itop+1
                           if (itop.gt.len_ieltstack) call mm_ovall
     &                        ('ieltstack',cprtname,ipieltstack,itop
     &                        ,1000,len_ieltstack,1,icscode)
                           ieltstack(itop)=icand
                        endif
                     endif
                  endif
               enddo
            enddo
 1010    continue
 
c.... Order isurfelt array so that duplicate entries can be
c.... merged into single entries.
 
         if (nsurfelt_.gt.len_iprm) call mm_ovall('iprm',cprtname,
     &      ipiprm,nsurfelt_,100,len_iprm,1,icscode)
         do i=1,nsurfelt_
            iprm(i)=i
         enddo
         ascend=1.d0
         call hpsortip(nsurfelt_,isurfelt_,ascend,iprm)
 
         nsurfelt=0
         iprevsurfelt=0
         do i=1,nsurfelt_
            if (isurfelt_(iprm(i)).ne.iprevsurfelt) then
               nsurfelt=nsurfelt+1
               if (nsurfelt.gt.len_isurfelt) call mm_ovall
     &            ('isurfelt',cprtname,ipisurfelt,nsurfelt,mpno
     &            +100,len_isurfelt,1,icscode)
               if (2*nsurfelt.gt.len_isurfeltsrc) call mm_ovall
     &            ('isurfeltsrc',cprtname,ipisurfeltsrc
     &            ,2*nsurfelt,2*mpno+200,len_isurfeltsrc,1,icscode)
               isurfelt(nsurfelt)=isurfelt_(iprm(i))
               iprevsurfelt=isurfelt_(iprm(i))
               isurfeltsrc(1,nsurfelt)=isurfeltsrc_(iprm(i))
               isurfeltsrc(2,nsurfelt)=0
            else
               isurfeltsrc(2,nsurfelt)=isurfeltsrc_(iprm(i))
            endif
         enddo
 
c.... Now load connected volume components into IFEATURE.
 
         if (ieltno.gt.len_iprm) call mm_ovall('iprm',cprtname,
     &      ipiprm,ieltno,100,len_iprm,1,icscode)
         do i=1,ieltno
            iprm(i)=i
         enddo
         ascend=1.d0
         call hpsortip(ieltno,icomp,ascend,iprm)
 
         if (icurrcomp+1.gt.len_ifeatureptr) call mm_ovall
     &      ('ifeatureptr',cprtname,ipifeatureptr,icurrcomp+1
     &      ,100,len_ifeatureptr,1,icscode)
         icurrcomp=ivcompstart-1
         nextfeature=1
         do i=1,ieltno
            if (icomp(iprm(i)).ne.0) then
               do while (icomp(iprm(i)).gt.icurrcomp)
                  icurrcomp=icurrcomp+1
                  ifeatureptr(icurrcomp)=nextfeature
               enddo
               if (nextfeature.gt.len_ifeature) call mm_ovall('ifeature'
     &            ,cprtname,ipifeature,nextfeature,mpno+100,len_ifeature
     &            ,1,icscode)
               ifeature(nextfeature)=ieltary(iprm(i))
               nextfeature=nextfeature+1
            endif
         enddo
         iscompstart=icurrcomp+1
         ifeatureptr(iscompstart)=nextfeature
 
c.... Loop thru ISURFELT and look for connected sets boundary triangles,
c.... defining the second part of IFEATURE.
 
c.... First, compile set of edges bounding triangles in IEDG and
c.... then edge-triangle relation in IEDGISURFELT which will allow us
c.... to move from triangle to triangle so that we can obtain the
c.... connected sets of boundary triangles.
 
         next=1
 
         do i=1,nsurfelt
            isurfeltcurr=isurfelt(i)
            ihyb=1+(isurfeltcurr-1)/nef_cmo
            ifac=isurfeltcurr-(ihyb-1)*nef_cmo
            ityp=itettyp(ihyb)
            do j1=1,ielmface0(ifac,ityp)
               iedg=ielmface2(j1,ifac,ityp)
               nod1=iparent(itet(itetoff(ihyb)+ielmedge1(1,iedg,ityp)))
               nod2=iparent(itet(itetoff(ihyb)+ielmedge1(2,iedg,ityp)))
               maxpar=max(nod1,nod2)
               minpar=min(nod1,nod2)
 
               if (3*next.gt.len_ikey) call mm_ovall('ikey',cprtname,
     &            ipikey,3*next,3*mpno+300,len_ikey,1,icscode)
               ikey(1,next)=minpar
               ikey(2,next)=maxpar
               ikey(3,next)=i
               next=next+1
 
            enddo
         enddo
         numedges=next-1
 
         call hpsortim(numedges,3,md,itemp,ikey)
 
         if (numedges.gt.len_iedges) call mm_ovall('iedges',cprtname,
     &      ipiedges,numedges,100,len_iedges,1,icscode)
 
         if (numedges.gt.len_iedgisurfelt) call mm_ovall('iedgisurfelt'
     &      ,cprtname,ipiedgisurfelt,numedges,100,len_iedgisurfelt,1
     &      ,icscode)
 
         if (nnodes+1.gt.len_iedges_first) call mm_ovall('iedges_first'
     &      ,cprtname,ipiedges_first,nnodes+1,100,len_iedges_first,1
     &      ,icscode)
 
         ipos=1
         do i=1,nnodes
            iedges_first(i)=ipos
            do while (ipos.le.numedges.and.ikey(1,ipos).eq.i)
               iedges(ipos)=ikey(2,ipos)
               iedgisurfelt(ipos)=ikey(3,ipos)
               ipos=ipos+1
            enddo
         enddo
         iedges_first(nnodes+1)=ipos
 
c.... Loop thru ISURFELT and look for connected sets of surface elements
 
         ncompdata=0
         numilinedge=0
         icurrcomp=iscompstart-1
 
         if (nsurfelt.gt.len_icomp) call mm_ovall('icomp',cprtname,
     &      ipicomp,nsurfelt,100,len_icomp,1,icscode)
         do i=1,nsurfelt
            icomp(i)=0
         enddo
         do 110 i=1,nsurfelt
            if (icomp(i).ne.0) goto 110
 
            icurrcomp=icurrcomp+1
 
            icomp(i)=icurrcomp
 
c... Register the source component / boundary component
c... relationship in ISRCCOMP and IBNDCOMP.
 
            ncompdata=ncompdata+1
            if (ncompdata+1.gt.len_isrccomp) call mm_ovall('isrccomp'
     &         ,cprtname,ipisrccomp,ncompdata+1,1000,len_isrccomp,1
     &         ,icscode)
            if (ncompdata+1.gt.len_ibndcomp) call mm_ovall('ibndcomp'
     &         ,cprtname,ipibndcomp,ncompdata+1,1000,len_ibndcomp,1
     &         ,icscode)
            isrccomp(ncompdata)=isurfeltsrc(1,i)
            ibndcomp(ncompdata)=icurrcomp
            if (isurfeltsrc(2,i).ne.0) then
               ncompdata=ncompdata+1
               isrccomp(ncompdata)=isurfeltsrc(2,i)
               ibndcomp(ncompdata)=icurrcomp
            endif
 
c... Put on stack
            itop=1
            ieltstack(itop)=i
 
            do while (itop.gt.0)
 
c.... Pop stack
 
               icurr=ieltstack(itop)
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
 
                  jbegin=0
                  jend=0
                  do j=iedges_first(minpar),iedges_first(minpar+1)-1
                     if (iedges(j).eq.maxpar) then
                        jend=j
                        if (jbegin.eq.0) jbegin=j
                     endif
                  enddo
                  if (jbegin.eq.0) then
                     print*,'Warning! Can''t find surface edge !'
                     call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &                  ,icscode)
                     stop
                  endif
                  if (jbegin.eq.jend) then
                     print*,'Deg. 1 edge!!'
                     call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &                  ,icscode)
                     stop
                  endif
                  if (jbegin+2.lt.jend) then
                     if (locverbosity.ge.1) print*,'Warning! Deg. ',jend
     &                  -jbegin+1,' edge'
                  endif
                  if (jbegin+1.ne.jend) then
                     iopp=-1
                  else
                     if ((iedgisurfelt(jbegin).ne.icurr).and
     &                  .(iedgisurfelt(jend).ne.icurr)) then
                        print*,'Surf. elt not in edge!!'
                        call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &                     ,icscode)
                        stop
                     else
                        iopp=iedgisurfelt(jbegin)+iedgisurfelt(jend)
     &                     -icurr
                     endif
                  endif
 
                  if (iopp.eq.-1) then
                     lsamesurf=.false.
                  else
                     jtetj=isurfelt(iopp)
                     ihybj=1+(jtetj-1)/nef_cmo
                     ifacj=jtetj-nef_cmo*(ihybj-1)
                     itypj=itettyp(ihybj)
 
                     lsamesurf=lsamecons_lg(ihyb,ifac,ityp,ihybj,ifacj
     &                  ,itypj,maxpar,minpar,iparent,itet,itetoff
     &                  ,icontab,icr1)
                  endif
                  if (lsamesurf) then
                     if (icomp(iopp).eq.0) then
                        icomp(iopp)=icurrcomp
                        itop=itop+1
                        if (itop.gt.len_ieltstack) call mm_ovall
     &                     ('ieltstack',cprtname,ipieltstack
     &                     ,itop,100,len_ieltstack,1,icscode)
                        ieltstack(itop)=iopp
                     endif
                  else
                     numilinedge=numilinedge+2
                     i1=invmpary(minpar)
                     i2=invmpary(maxpar)
                     if (i1.eq.0.or.i2.eq.0) then
                        print*,'Warning!!  Inverse map not defined'//
     &                     'on ilinedge ',minpar,'/',maxpar
                     else
 
                        if (3*numilinedge.gt.len_ikey) call mm_ovall
     &                     ('ikey',cprtname,ipikey,3*numilinedge,mpno
     &                     +300,len_ikey,1,icscode)
                        ikey(1,numilinedge-1)=i1
                        ikey(2,numilinedge-1)=i2
                        ikey(3,numilinedge-1)=icurrcomp
 
                        ikey(1,numilinedge)=i2
                        ikey(2,numilinedge)=i1
                        ikey(3,numilinedge)=icurrcomp
 
                     endif
                  endif
 120           continue
            enddo
 110     continue
 
c.... Now load connected surface components into IFEATURE.
 
         if (nsurfelt.gt.len_iprm) call mm_ovall('iprm',cprtname,
     &      ipiprm,nsurfelt,100,len_iprm,1,icscode)
         do i=1,nsurfelt
            iprm(i)=i
         enddo
         ascend=1.d0
         call hpsortip(nsurfelt,icomp,ascend,iprm)
 
         if (icomp(iprm(1)).lt.1) then
            print*,'Error!  Untagged elements!!'
            call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &         ,icscode)
            stop
         endif
 
         if (icurrcomp+1.gt.len_ifeatureptr) call mm_ovall
     &      ('ifeatureptr',cprtname,ipifeatureptr,icurrcomp+1
     &      ,100,len_ifeatureptr,1,icscode)
         icurrcomp=iscompstart-1
 
         do i=1,nsurfelt
            if (icomp(iprm(i)).gt.icurrcomp) then
               icurrcomp=icurrcomp+1
               ifeatureptr(icurrcomp)=nextfeature
            endif
            if (nextfeature.gt.len_ifeature) call mm_ovall('ifeature'
     &         ,cprtname,ipifeature,nextfeature,mpno+100,len_ifeature
     &         ,1,icscode)
            ifeature(nextfeature)=isurfelt(iprm(i))
            nextfeature=nextfeature+1
         enddo
         ilcompstart=icurrcomp+1
 
c.... Sort intersection lines.
 
         call hpsortim(numilinedge,3,md,itemp,ikey)
 
         if (mpno+1.gt.len_ilinedgeptr) call mm_ovall
     &      ('ilinedgeptr',cprtname,ipilinedgeptr,mpno+1
     &      ,100,len_ilinedgeptr,1,icscode)
         if (numilinedge.gt.len_ilinedge) call mm_ovall('ilinedge'
     &      ,cprtname,ipilinedge,numilinedge,100,len_ilinedge
     &      ,1,icscode)
         if (numilinedge.gt.len_ilinedgesrc) call mm_ovall('ilinedgesrc'
     &      ,cprtname,ipilinedgesrc,numilinedge,100,len_ilinedgesrc
     &      ,1,icscode)
         ipos=1
         do i=1,mpno
            ilinedgeptr(i)=ipos
            do while (ipos.le.numilinedge.and.ikey(1,ipos).eq.i)
               if (ipos.gt.len_ilinedge) call mm_ovall('ilinedge'
     &            ,cprtname,ipilinedge,ipos,mpno+100,len_ifeature
     &            ,1,icscode)
               ilinedge(ipos)=ikey(2,ipos)
               ilinedgesrc(ipos)=ikey(3,ipos)
               ipos=ipos+1
            enddo
         enddo
         ilinedgeptr(mpno+1)=ipos
 
c.... Loop thru intersection line edges and look for connnected
c.... intersection lines.  Stick these lines into the third part of
c.... IFEATURE.
 
         icurrcomp=ilcompstart-1
         if (numilinedge.gt.len_icomp) call mm_ovall ('icomp',cprtname
     &      ,ipicomp,numilinedge,100,len_icomp,1,icscode)
         do i=1,numilinedge
            icomp(i)=0
         enddo
 
         numspecial_=0
         do i=1,mpno
            i1orig=i
            j=ilinedgeptr(i)
            do 210 while (j.lt.ilinedgeptr(i+1))
               if (icomp(j).ne.0) then
                  j=j+1
                  goto 210
               endif
               i2orig=ilinedge(j)
               icurrcomp=icurrcomp+1
               icomp(j)=icurrcomp
               ndeg=1
 
               if (ndeg.gt.len_isrc) call mm_ovall ('isrc',cprtname
     &            ,ipisrc,ndeg,100,len_isrc,1,icscode)
               isrc(ndeg)=ilinedgesrc(j)
               j=j+1
               do while((ilinedge(j).eq.i2orig).and.(j.lt
     &            .ilinedgeptr(i+1)))
                  if (isrc(ndeg).ne.ilinedgesrc(j)) then
                     ndeg=ndeg+1
                     if (ndeg.gt.len_isrc) call mm_ovall ('isrc'
     &                  ,cprtname,ipisrc,ndeg,100,len_isrc,1,icscode)
                     isrc(ndeg)=ilinedgesrc(j)
                  endif
                  icomp(j)=icurrcomp
                  j=j+1
               enddo
 
c... Register the source component / boundary component
c... relationship in ISRCCOMP and IBNDCOMP.
 
               if (ncompdata+ndeg.gt.len_isrccomp) call mm_ovall
     &            ('isrccomp',cprtname,ipisrccomp,ncompdata+ndeg,1000
     &            ,len_isrccomp,1,icscode)
               if (ncompdata+ndeg.gt.len_ibndcomp) call mm_ovall
     &            ('ibndcomp',cprtname,ipibndcomp,ncompdata+ndeg,1000
     &            ,len_ibndcomp,1,icscode)
               do k1=1,ndeg
                  ncompdata=ncompdata+1
                  isrccomp(ncompdata)=isrc(k1)
                  ibndcomp(ncompdata)=icurrcomp
               enddo
 
c.... Cross off redundant edge
 
               k=ilinedgeptr(i2orig)
               do while (k.lt.ilinedgeptr(i2orig+1).and.ilinedge(k).ne
     &            .i1orig)
                  k=k+1
               enddo
               do while (k.lt.ilinedgeptr(i2orig+1).and.ilinedge(k).eq
     &            .i1orig)
                  icomp(k)=icurrcomp
                  k=k+1
               enddo
 
c.... Move in 'forward' direction and string together interline.
 
               nforward=1
               if (nforward+1.gt.len_iforward) call mm_ovall
     &            ('iforward',cprtname,ipiforward,nforward+1,100
     &            ,len_iforward,1,icscode)
               iforward(1)=i1orig
               iforward(2)=i2orig
 
               i1=i1orig
               i2=i2orig
 
 111           continue
               call getnextilineseg(i1,i2,ndeg,isrc,ilinedgeptr
     &            ,ilinedge,ilinedgesrc,icomp,icurrcomp,cprtname
     &            ,len_isrcnew,lspecial,i3)
               if (i3.ne.0) then
                  i1=i2
                  i2=i3
                  nforward=nforward+1
                  if (nforward+1.gt.len_iforward) call mm_ovall
     &               ('iforward',cprtname,ipiforward,nforward+1,100
     &               ,len_iforward,1,icscode)
                  iforward(nforward+1)=i3
                  goto 111
               endif
 
               nbackward=0
               if (lspecial) then
                  numspecial_=numspecial_+1
                  if (3*numspecial_.gt.len_ikey) call mm_ovall
     &               ('ikey',cprtname,ipikey,3*numspecial_,300
     &               ,len_ikey,1,icscode)
                  ikey(1,numspecial_)=i2
                  ikey(2,numspecial_)=icurrcomp
                  i1=i2orig
                  i2=i1orig
 
 112              continue
 
                  call getnextilineseg(i1,i2,ndeg,isrc,ilinedgeptr
     &               ,ilinedge,ilinedgesrc,icomp,icurrcomp,cprtname
     &               ,len_isrcnew,lspecial,i3)
                  if (i3.ne.0) then
                     i1=i2
                     i2=i3
                     nbackward=nbackward+1
                     if (nbackward.gt.len_ibackward) call mm_ovall
     &                  ('ibackward',cprtname,ipibackward,nbackward,100
     &                  ,len_ibackward,1,icscode)
                     ibackward(nbackward)=i3
                     goto 112
                  endif
                  if (lspecial) then
                     numspecial_=numspecial_+1
                     if (3*numspecial_.gt.len_ikey) call mm_ovall
     &                  ('ikey',cprtname,ipikey,3*numspecial_,300
     &                  ,len_ikey,1,icscode)
                     ikey(1,numspecial_)=i2
                     ikey(2,numspecial_)=icurrcomp
                  else
                     print*,'Strange interline loop!'
                     call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &                  ,icscode)
                     stop
                  endif
               endif
 
c.... Write component into ifeature
 
               if (icurrcomp.gt.len_ifeatureptr) call mm_ovall
     &            ('ifeatureptr',cprtname,ipifeatureptr,icurrcomp
     &            ,1000,len_ifeatureptr,1,icscode)
 
               ifeatureptr(icurrcomp)=nextfeature
 
               if (nextfeature+nbackward+nforward+1.gt.len_ifeature)
     &            call mm_ovall('ifeature',cprtname,ipifeature
     &            ,nextfeature+nbackward+nforward+1,mpno+100
     &            ,len_ifeature,1,icscode)
 
               do k1=nbackward,1,-1
                  ifeature(nextfeature)=mpary(ibackward(k1))
                  nextfeature=nextfeature+1
               enddo
               do k1=1,nforward+1
                  ifeature(nextfeature)=mpary(iforward(k1))
                  nextfeature=nextfeature+1
               enddo
 
 210        continue
         enddo
 
         isppcompstart=icurrcomp+1
 
c.... Sort special point data.
 
         call hpsortim(numspecial_,2,md,itemp,ikey)
 
         icurrcomp=isppcompstart-1
         iprev=0
         do i=1,numspecial_
            if (ikey(1,i).ne.iprev) then
               iprev=ikey(1,i)
               icurrcomp=icurrcomp+1
               if (icurrcomp+1.gt.len_ifeatureptr) call mm_ovall
     &            ('ifeatureptr',cprtname,ipifeatureptr,icurrcomp+1
     &            ,1000,len_ifeatureptr,1,icscode)
               ifeatureptr(icurrcomp)=nextfeature
               if (nextfeature.gt.len_ifeature)
     &            call mm_ovall('ifeature',cprtname,ipifeature
     &            ,nextfeature,1000,len_ifeature,1,icscode)
               ifeature(nextfeature)=mpary(ikey(1,i))
               nextfeature=nextfeature+1
            endif
            ncompdata=ncompdata+1
            if (ncompdata.gt.len_isrccomp)
     &         call mm_ovall('isrccomp',cprtname,ipisrccomp
     &         ,ncompdata,1000,len_isrccomp,1,icscode)
            if (ncompdata.gt.len_ibndcomp)
     &         call mm_ovall('ibndcomp',cprtname,ipibndcomp
     &         ,ncompdata,1000,len_ibndcomp,1,icscode)
            isrccomp(ncompdata)=ikey(2,i)
            ibndcomp(ncompdata)=icurrcomp
         enddo
         ncomp=icurrcomp
         numspecial=ncomp-isppcompstart+1
         ifeatureptr(ncomp+1)=nextfeature
 
c.... Order (isrccomp,ibndcomp) to form IBOUNDARY relation.
 
         if (3*ncompdata.gt.len_ikey) call mm_ovall
     &      ('ikey',cprtname,ipikey,3*ncompdata,300
     &      ,len_ikey,1,icscode)
         do i=1,ncompdata
            ikey(1,i)=isrccomp(i)
            ikey(2,i)=ibndcomp(i)
         enddo
         call hpsortim(ncompdata,2,md,itemp,ikey)
 
c.... Eliminate redundent entries, if necessary.
 
         ipos=1
         iprevikey1=ikey(1,1)
         iprevikey2=ikey(2,1)
         do i=2,ncompdata
            if ((ikey(1,i).ne.iprevikey1).or.(ikey(2,i).ne.iprevikey2)
     &         )then
               ipos=ipos+1
               ikey(1,ipos)=ikey(1,i)
               ikey(2,ipos)=ikey(2,i)
               iprevikey1=ikey(1,i)
               iprevikey2=ikey(2,i)
            endif
         enddo
         ncompdata=ipos
 
         if (ncomp+1.gt.len_iboundaryptr) call mm_ovall
     &      ('iboundaryptr',cprtname,ipiboundaryptr,ncomp+1
     &      ,1000,len_iboundaryptr,1,icscode)
         ipos=1
         do i=1,ncomp
            iboundaryptr(i)=ipos
            do while (ipos.le.ncompdata.and.ikey(1,ipos).eq.i)
               if (ipos.gt.len_iboundary) call mm_ovall
     &            ('iboundary',cprtname,ipiboundary,ipos
     &            ,1000,len_iboundary,1,icscode)
               iboundary(ipos)=ikey(2,ipos)
               ipos=ipos+1
            enddo
         enddo
         iboundaryptr(ncomp+1)=ipos
 
c.... If necessary, interchange boundary (special point) components
c.... of interlines, such that first special point coincides
c.... with beginning of line.
 
         do i=ilcompstart,isppcompstart-1
            if (iboundaryptr(i+1).eq.iboundaryptr(i)+2) then
               nod1=ifeature(ifeatureptr(iboundary(iboundaryptr(i))))
               nod2=ifeature(ifeatureptr(iboundary(iboundaryptr(i)+1)))
               nodbegin=ifeature(ifeatureptr(i))
               if (nodbegin.ne.nod1) then
                  if (nodbegin.ne.nod2) then
                     print*,'Error! Beginning node not in boundary'//
     &                  ' for (interline) component ',i
                     call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &                  ,icscode)
                     stop
                  else
                     itmp=iboundary(iboundaryptr(i))
                     iboundary(iboundaryptr(i))=iboundary(iboundaryptr(i
     &                  )+1)
                     iboundary(iboundaryptr(i)+1)=itmp
                  endif
               endif
            endif
         enddo
 
c.... Reverse the keys, i.e. order (ibndcomp,isrccomp) to form
c.... INVBOUNDARY relation.
 
         do i=1,ncompdata
            isrccomp(i)=ikey(1,i)
            ibndcomp(i)=ikey(2,i)
         enddo
         do i=1,ncompdata
            ikey(2,i)=isrccomp(i)
            ikey(1,i)=ibndcomp(i)
         enddo
         call hpsortim(ncompdata,2,md,itemp,ikey)
 
         if (ncomp+1.gt.len_invboundaryptr) call mm_ovall
     &      ('invboundaryptr',cprtname,ipinvboundaryptr,ncomp+1
     &      ,1000,len_invboundaryptr,1,icscode)
         ipos=1
         do i=1,ncomp
            invboundaryptr(i)=ipos
            do while (ipos.le.ncompdata.and.ikey(1,ipos).eq.i)
               if (ipos.gt.len_invboundary) call mm_ovall
     &            ('invboundary',cprtname,ipinvboundary,ipos
     &            ,1000,len_invboundary,1,icscode)
               invboundary(ipos)=ikey(2,ipos)
               ipos=ipos+1
            enddo
         enddo
         invboundaryptr(ncomp+1)=ipos
 
c.... Create Line (Skeleton) CMO and exit if LSKELETONONLY=.true.
 
         if (lskeletononly) then
            call createcmoline(cmoskeleton,xic,yic,zic,itp1,icr1,isn1
     &         ,iboundary,iboundaryptr,ifeature,ifeatureptr,ilcompstart
     &         ,isppcompstart,ncomp)
            goto 9999
         endif
 
c.... Initialize Lock-out list.
 
         if (mpno.gt.len_lockout) call mm_ovall('lockout',cprtname,
     &      iplockout,mpno,100,len_lockout,1,icscode)
 
         do i=1,mpno
            lockout(i)=.false.
         enddo
 
         if (ieltno.gt.len_invifeature) call mm_ovall('invifeature'
     &      ,cprtname,ipinvifeature,ieltno,100,len_invifeature,1,icscode
     &      )
 
c.... Initially zero entire INVIFEATURE array.
 
         do i=1,ieltno
            invifeature(i)=0
         enddo
 
         nadd=0
         lrefine=.false.
         lrecolor=.false.
 
c.... Loop thru volume elements and compute volume, volume collapse rate
c.... and estimated collapse time for each connected volume set.
 
         if (ncomp.gt.len_volume) call mm_ovall('volume',cprtname,
     &      ipvolume,ncomp,100,len_volume,2,icscode)
         do i=1,ncomp
            volume(i)=0.
         enddo
         if (ilcompstart-1.gt.len_diameter) call mm_ovall('diameter'
     &      ,cprtname,ipdiameter,ilcompstart-1,100,len_diameter,2
     &      ,icscode)
         if (lvels_exist) then
            if (ncomp.gt.len_dvoldt) call mm_ovall('dvoldt',cprtname,
     &         ipdvoldt,ncomp,100,len_dvoldt,2,icscode)
            if (ncomp.gt.len_estcolltime) call mm_ovall('estcolltime'
     &         ,cprtname,ipestcolltime,ncomp,100,len_estcolltime,2
     &         ,icscode)
            do i=1,ncomp
               dvoldt(i)=0.
               estcolltime(i)=0.
            enddo
         endif
 
         if (ieltno.gt.len_ivcomp) call mm_ovall('ivcomp',cprtname,
     &      ipivcomp,ieltno,100,len_ivcomp,1,icscode)
         do i=1,ieltno
            ivcomp(i)=0
         enddo
         do i=ivcompstart,iscompstart-1
            do j=ifeatureptr(i),ifeatureptr(i+1)-1
               ivcomp(invieltary(ifeature(j)))=i
            enddo
         enddo
 
         do i=1,ieltno
 
            ivol=ivcomp(i)
            if (ivol.gt.0) then
               ihyb=ieltary(i)
               nod1=itet(itetoff(ihyb)+1)
               nod2=itet(itetoff(ihyb)+2)
               nod3=itet(itetoff(ihyb)+3)
               nod4=itet(itetoff(ihyb)+4)
 
               x21=xic(nod2)-xic(nod1)
               y21=yic(nod2)-yic(nod1)
               z21=zic(nod2)-zic(nod1)
               x31=xic(nod3)-xic(nod1)
               y31=yic(nod3)-yic(nod1)
               z31=zic(nod3)-zic(nod1)
               x41=xic(nod4)-xic(nod1)
               y41=yic(nod4)-yic(nod1)
               z41=zic(nod4)-zic(nod1)
 
               volcurr=one6th*det(x21,y21,z21,x31,y31,z31,x41,y41,z41)
 
               volume(ivol)=volume(ivol)+volcurr
 
               if (lvels_exist) then
                  v21x=vels(1,nod2)-vels(1,nod1)
                  v21y=vels(2,nod2)-vels(2,nod1)
                  v21z=vels(3,nod2)-vels(3,nod1)
                  v31x=vels(1,nod3)-vels(1,nod1)
                  v31y=vels(2,nod3)-vels(2,nod1)
                  v31z=vels(3,nod3)-vels(3,nod1)
                  v41x=vels(1,nod4)-vels(1,nod1)
                  v41y=vels(2,nod4)-vels(2,nod1)
                  v41z=vels(3,nod4)-vels(3,nod1)
 
                  dvoldtcurr=one6th*
     &               (det(v21x,v21y,v21z,x31,y31,z31,x41,y41,z41)+
     &               det(x21,y21,z21,v31x,v31y,v31z,x41,y41,z41)+
     &               det(x21,y21,z21,x31,y31,z31,v41x,v41y,v41z))
 
                  dvoldt(ivol)=dvoldt(ivol)+dvoldtcurr
               endif
            endif
         enddo
 
         do i=ivcompstart,iscompstart-1
            if (lvels_exist) then
               if (dvoldt(i).lt.zero) then
                  estcolltime(i)=-volume(i)/dvoldt(i)
               elseif (dvoldt(i).eq.zero) then
                  estcolltime(i)=1.d98
               else
                  estcolltime(i)=1.d99
               endif
            endif
         enddo
 
         icodim=0
         do i=ivcompstart,iscompstart-1
            diameter(i)=volume(i)/(dimmin*dimmed)
            if (diameter(i).le.dnearfactor*tolsmalldist) then
               nelts=ifeatureptr(i+1)-ifeatureptr(i)
               if (locverbosity.ge.2) print*,'Volume component ',i
     &            ,':  old diameter=',diameter(i)
               diameter(i)=dmaxdepth_lg(icodim,nelts
     &            ,ifeature(ifeatureptr(i)),invifeature,invieltary,itet
     &            ,itetoff,xic,yic,zic,nnodes,mbndry,ipdir,len_dir
     &            ,ipdepth,len_depth,itettyp,jtet,jtetoff,ipieltstack
     &            ,len_ieltstack,md,ikey,iedges,iedges_first
     &            ,iedgisurfelt,nef_cmo,iparent,cprtname)
               if (locverbosity.ge.2) print*,'new diameter=',diameter(i)
            endif
         enddo
 
c.... Loop thru surface elements and compute area, area collapse rate,
c.... and estimated collapse time for each surface set.
 
         do i=iscompstart,ilcompstart-1
            do j=ifeatureptr(i),ifeatureptr(i+1)-1
               jtetcurr=ifeature(j)
               ihyb=1+(jtetcurr-1)/nef_cmo
               ifac=jtetcurr-nef_cmo*(ihyb-1)
               ityp=itettyp(ihyb) ! just ornamental---type must be tetrahedron
               nod1=itet(itetoff(ihyb)+ielmface1(1,ifac,ityp))
               nod2=itet(itetoff(ihyb)+ielmface1(2,ifac,ityp))
               nod3=itet(itetoff(ihyb)+ielmface1(3,ifac,ityp))
               x21=xic(nod2)-xic(nod1)
               y21=yic(nod2)-yic(nod1)
               z21=zic(nod2)-zic(nod1)
               x31=xic(nod3)-xic(nod1)
               y31=yic(nod3)-yic(nod1)
               z31=zic(nod3)-zic(nod1)
 
               areax=half*crosx(zero,zero,zero,x21,y21,z21,x31,y31,z31)
               areay=half*crosy(zero,zero,zero,x21,y21,z21,x31,y31,z31)
               areaz=half*crosz(zero,zero,zero,x21,y21,z21,x31,y31,z31)
 
               areacurr=sqrt(areax**2+areay**2+areaz**2)
 
               volume(i)=volume(i)+areacurr
 
               if (lvels_exist) then
                  v21x=vels(1,nod2)-vels(1,nod1)
                  v21y=vels(2,nod2)-vels(2,nod1)
                  v21z=vels(3,nod2)-vels(3,nod1)
                  v31x=vels(1,nod3)-vels(1,nod1)
                  v31y=vels(2,nod3)-vels(2,nod1)
                  v31z=vels(3,nod3)-vels(3,nod1)
 
                  ratex=half*(crosx(zero,zero,zero,v21x,v21y,v21z,x31
     &               ,y31,z31)+crosx(zero,zero,zero,x21,y21,z21,v31x
     &               ,v31y,v31z))
                  ratey=half*(crosy(zero,zero,zero,v21x,v21y,v21z,x31
     &               ,y31,z31)+crosy(zero,zero,zero,x21,y21,z21,v31x
     &               ,v31y,v31z))
                  ratez=half*(crosz(zero,zero,zero,v21x,v21y,v21z,x31
     &               ,y31,z31)+crosz(zero,zero,zero,x21,y21,z21,v31x
     &               ,v31y,v31z))
 
                  dadtcurr=(ratex*areax+ratey*areay+ratez*areaz)
     &               /areacurr
 
                  dvoldt(i)=dvoldt(i)+dadtcurr
               endif
            enddo
         enddo
 
         if (lvels_exist) then
            do i=iscompstart,ilcompstart-1
               if (dvoldt(i).lt.zero) then
                  estcolltime(i)=-volume(i)/dvoldt(i)
               elseif (dvoldt(i).eq.zero) then
                  estcolltime(i)=1.d98
               else
                  estcolltime(i)=1.d99
               endif
            enddo
         endif
 
         icodim=1
         do i=iscompstart,ilcompstart-1
            diameter(i)=volume(i)/dimmed
            if (diameter(i).le.dnearfactor*tolsmalldist) then
               nelts=ifeatureptr(i+1)-ifeatureptr(i)
               if (locverbosity.ge.2) then
                  print*,'Surface component ',i,'; + colors='
                  do j=invboundaryptr(i),invboundaryptr(i+1)-1
                     jcmp=invboundary(j)
                     jclr=itetclr(ifeature(ifeatureptr(jcmp)))
                     print*,jclr
                  enddo
               endif
               if (locverbosity.ge.2) print*,'Old diameter=',diameter(i)
               diameter(i)=dmaxdepth_lg(icodim,nelts
     &            ,ifeature(ifeatureptr(i)),invifeature,invieltary,itet
     &            ,itetoff,xic,yic,zic,nnodes,mbndry,ipdir,len_dir
     &            ,ipdepth,len_depth,itettyp,jtet,jtetoff,ipieltstack
     &            ,len_ieltstack,md,ikey,iedges,iedges_first
     &            ,iedgisurfelt,nef_cmo,iparent,cprtname)
               if (locverbosity.ge.2) print*,'new diameter=',diameter(i)
            endif
         enddo
 
c.... Loop thru interline edges and compute length, length contraction
c.... rate, and estimated collapse time for each intersection line.
 
         do i=ilcompstart,isppcompstart-1
            do j=ifeatureptr(i),ifeatureptr(i+1)-2
               nod1=ifeature(j)
               nod2=ifeature(j+1)
               x21=xic(nod2)-xic(nod1)
               y21=yic(nod2)-yic(nod1)
               z21=zic(nod2)-zic(nod1)
               rlengthcurr=sqrt(x21**2+y21**2+z21**2)
               volume(i)=volume(i)+rlengthcurr
 
               if (lvels_exist) then
                  v21x=vels(1,nod2)-vels(1,nod1)
                  v21y=vels(2,nod2)-vels(2,nod1)
                  v21z=vels(3,nod2)-vels(3,nod1)
 
                  dldtcurr=(x21*v21x+y21*v21y+z21*v21z)/rlengthcurr
 
                  dvoldt(i)=dvoldt(i)+dldtcurr
               endif
            enddo
         enddo
 
         if (lvels_exist) then
            do i=ilcompstart,isppcompstart-1
               if (dvoldt(i).lt.zero) then
                  estcolltime(i)=-volume(i)/dvoldt(i)
               elseif (dvoldt(i).eq.zero) then
                  estcolltime(i)=1.d98
               else
                  estcolltime(i)=1.d99
               endif
            enddo
         endif
 
         length=iscompstart-ivcompstart
         if (length.gt.len_icategory) call mm_ovall('icategory',cprtname
     &      ,ipicategory,length,100,len_icategory,1,icscode)
 
c.... Initially zero all elements of ICATEGORY.
 
         do i=ivcompstart,iscompstart-1
            icategory(i)=0
         enddo
         if (mpno.gt.len_invicompnod) call mm_ovall('invicompnod'
     &      ,cprtname,ipinvicompnod,mpno,100,len_invicompnod,1,icscode)
 
c.... Initially zero entire INVICOMPNOD array.
 
         do i=1,mpno
            invicompnod(i)=0
         enddo
 
c.... We now sort the volumes using ESTCOLLTIME as a key.  Thus, we
c.... will try to eliminate first connected volumes whose estimated
c.... collapse is the most imminent.  However, any nonexpanding
c.... volume that is sufficiently small will be considered for
c.... elimination.
 
         length=iscompstart-ivcompstart
         if (length.gt.len_iprm) call mm_ovall('iprm',cprtname,
     &      ipiprm,length,100,len_iprm,1,icscode)
         do i=ivcompstart,iscompstart-1
            iprm(i)=i
         enddo
 
         if (lvels_exist) then
            ascend=1.
            call hpsort1(iscompstart-ivcompstart,estcolltime,ascend
     &         ,iprm(ivcompstart))
         else
            ascend=1.
            call hpsort1(iscompstart-ivcompstart,volume,ascend
     &         ,iprm(ivcompstart))
         endif
 
         do 300 i=ivcompstart,iscompstart-1
            icurr=iprm(i)
 
            if (lvels_exist) then
 
               if (dvoldt(icurr).gt.zero) then
                  fracincrease=dvoldt(icurr)/volume(icurr)*toldt
                  diamadj=diameter(icurr)*fracincrease
               else
                  diamadj=0.d0
               endif
 
               if (diameter(icurr)+diamadj.gt.tolsmalldist.and
     &            .estcolltime(icurr).gt.toldt)goto 300
            else
               if (diameter(icurr).gt.tolsmalldist) goto 310
            endif
 
c.... Form list of nodes in this component.  Also, if any node
c.... has been locked out, skip this component.
 
            ncompnod=0
            do j=ifeatureptr(icurr),ifeatureptr(icurr+1)-1
               ihyb=ifeature(j)
               ityp=itettyp(ihyb)
               do k=1,nelmnen(ityp)
                  nodk=invmpary(iparent(itet(itetoff(ihyb)+k)))
                  if (nodk.ne.0) then
                     if (lockout(nodk)) goto 300
                     ncompnod=ncompnod+1
                     if (ncompnod.gt.len_icompnod) call mm_ovall
     &                  ('icompnod',cprtname,ipicompnod,ncompnod,1000
     &                  ,len_icompnod,1,icscode)
                     icompnod(ncompnod)=nodk
                  endif
               enddo
            enddo
 
            call hpsorti(ncompnod,icompnod)
 
            ncompnod_=ncompnod
 
            ncompnod=1
            iprev=icompnod(1)
 
            do j=2,ncompnod_
               if (iprev.ne.icompnod(j)) then
                  iprev=icompnod(j)
                  ncompnod=ncompnod+1
                  if (ncompnod.gt.len_icompnod) call mm_ovall
     &               ('icompnod',cprtname,ipicompnod,ncompnod,1000
     &               ,len_icompnod,1,icscode)
                  icompnod(ncompnod)=icompnod(j)
               endif
            enddo
 
            icodim=0
 
            call getplusminus(icodim,icurr,cipluslist,ipipluslist
     &         ,len_ipluslist,cprtname,npluslist,ciminuslist
     &         ,ipiminuslist,len_iminuslist,nminuslist,cicompstack
     &         ,ipicompstack,len_icompstack,cicodimstack,ipicodimstack
     &         ,len_icodimstack,iboundaryptr,iboundary,invboundaryptr
     &         ,invboundary)
 
            if (nminuslist.eq.0) goto 300
 
c.... Inverse icompnod relation.
 
            do j=1,ncompnod
               invicompnod(icompnod(j))=j
            enddo
 
c... Now define ICATEGORY to be the inverse map of both IMINUSLIST and
c... IPLUSLIST, in the sense that ICATEGORY(IPLUSLIST(I))=I , 1<=I<=NPLUSLIST
c... and ICATEGORY(IMINUSLIST(I))=-I , 1<=I<=NMINUSLIST.  The name
c... ICATEGORY stems from the fact that the sign of ICATEGORY(IVC) tells
c... which category volume component IVC is in.
 
            do j=1,npluslist
               icategory(ipluslist(j))=j
            enddo
            do j=1,nminuslist
               icategory(iminuslist(j))=-j
            enddo
 
            if (nminuslist.gt.len_thetaminus) call mm_ovall('thetaminus'
     &         ,cprtname,ipthetaminus,nminuslist,100,len_thetaminus,2
     &         ,icscode)
            if (npluslist.gt.len_thetaplus) call mm_ovall('thetaplus'
     &         ,cprtname,ipthetaplus,npluslist,100,len_thetaplus,2
     &         ,icscode)
 
            call getcompnbd(icodim,cprtname,ipicomphyb,len_icomphyb
     &         ,ncomphyb,icompnod,invicompnod,ncompnod,nodhyb,nodhyboff
     &         ,xic,yic,zic,npluslist,nminuslist,ipluslist,iminuslist
     &         ,thetaplus,thetaminus,iboundaryptr,iboundary
     &         ,invboundaryptr,invboundary,ifeature,ifeatureptr,invmpary
     &         ,ipicompstack,cicompstack,len_icompstack,ipicodimstack
     &         ,cicodimstack,len_icodimstack,len_itarglist,len_vecx
     &         ,len_vecy,len_vecz,nef_cmo,itet,itetoff,itettyp,iparent
     &         ,mpary)
 
            ascend=-1.
            if (nminuslist.gt.len_iprm1) call mm_ovall('iprm1',cprtname,
     &         ipiprm1,nminuslist,100,len_iprm1,1,icscode)
            do j=1,nminuslist
               iprm1(j)=j
            enddo
            call hpsort1(nminuslist,thetaminus,ascend,iprm1)
 
            ilosingcomp=icurr
            ihyb=ifeature(ifeatureptr(ilosingcomp))
            ilosingmat=itetclr(ihyb)
 
            iwinningcomp=iminuslist(iprm1(1))
            ihyb=ifeature(ifeatureptr(iwinningcomp))
            iwinningmat=itetclr(ihyb)
 
            if (locverbosity.ge.2) then
               print*,'Volume: ICURR=',icurr
               print*,'iminuslist(1:nminuslist)=',(iminuslist(j1),j1=1
     &            ,nminuslist)
               print*,'ipluslist(1:npluslist)=',(ipluslist(j1),j1=1
     &            ,npluslist)
               print*,'-volumes:',(volume(iminuslist(j1)),j1=1
     &            ,nminuslist)
               print*,'+volumes:',(volume(ipluslist(j1)),j1=1,npluslist)
               print*,'-thetas/4pi:',(thetaminus(j1)/(4*
     &            3.1415926535897932d0),j1=1,nminuslist)
               print*,'+thetas/4pi:',(thetaplus(j1)/(4*
     &            3.1415926535897932d0),j1=1,npluslist)
            endif
 
            call correctcompnbd(icodim,locrecolor,locrefine,ncomphyb
     &         ,icomphyb,ieltary,itettyp,iparent,invicompnod,invmpary
     &         ,mpary,mpno,ncompnod,icompnod,xic,yic,zic,epsilonl,itp1
     &         ,isn1,imt1,icr1,icontab,itet,itetoff
     &         ,cut_length,nadd,ipikey,len_ikey,ipitadd,len_itadd
     &         ,ipieadd,len_ieadd,ipiadd,len_iadd,ipitpadd,len_itpadd
     &         ,ipicradd,len_icradd,cprtname,ipxadd,len_xadd,ipyadd
     &         ,len_yadd,ipzadd,len_zadd,lockout,ivcomp,nef_cmo
     &         ,iwinningmat,ilosingcomp,itetclr,icurr,ifeatureptr
     &         ,ifeature,len_iout,len_ktet,len_iout_first
     &         ,len_icompmat,len_icompicr)
 
            lrefine=lrefine.or.locrefine
            lrecolor=lrecolor.or.locrecolor
 
c.... Rezero only those elements of INVICOMPNOD, ICATEGORY that need
c.... to be rezeroed.
 
            do j=1,ncompnod
               invicompnod(icompnod(j))=0
            enddo
 
            do j=1,npluslist
               icategory(ipluslist(j))=0
            enddo
            do j=1,nminuslist
               icategory(iminuslist(j))=0
            enddo
 
 300     continue
 310     continue
 
c.... We now sort the surfaces using ESTCOLLTIME as a key.  Thus, we
c.... will try to eliminate first connected surfaces whose estimated
c.... collapse is the most imminent.  However, any nonexpanding
c.... surface that is sufficiently small will be considered for
c.... elimination.
 
         length=ilcompstart-iscompstart
         if (length.gt.len_iprm) call mm_ovall('iprm',cprtname,
     &      ipiprm,length,100,len_iprm,1,icscode)
         do i=iscompstart,ilcompstart-1
            iprm(i)=i
         enddo
 
         if (lvels_exist) then
            ascend=1.
            call hpsort1(ilcompstart-iscompstart,estcolltime,ascend
     &         ,iprm(iscompstart))
         else
            ascend=1.
            call hpsort1(ilcompstart-iscompstart,volume,ascend
     &         ,iprm(iscompstart))
         endif
 
         do 400 i=iscompstart,ilcompstart-1
            icurr=iprm(i)
 
            if (lvels_exist) then
 
               if (dvoldt(icurr).gt.zero) then
                  fracincrease=dvoldt(icurr)/volume(icurr)*toldt
                  diamadj=diameter(icurr)*fracincrease
               else
                  diamadj=0.d0
               endif
 
               if (diameter(icurr)+diamadj.gt.tolsmalldist.and
     &            .estcolltime(icurr).gt.toldt)goto 400
            else
               if (diameter(icurr).gt.tolsmalldist) goto 410
            endif
 
c.... Form list of nodes in this component.  Also, if any node
c.... has been locked out, skip this component.
 
            ncompnod=0
            do j=ifeatureptr(icurr),ifeatureptr(icurr+1)-1
               jtetcurr=ifeature(j)
               ihyb=1+(jtetcurr-1)/nef_cmo
               ifac=jtetcurr-nef_cmo*(ihyb-1)
               ityp=itettyp(ihyb) ! just ornamental---type must be tetrahedron here
               do k=1,ielmface0(ifac,ityp)
                  nodk=invmpary(iparent(itet(itetoff(ihyb)
     &               +ielmface1(k,ifac,ityp))))
                  if (nodk.ne.0) then
                     if (lockout(nodk)) goto 400
                     ncompnod=ncompnod+1
                     if (ncompnod.gt.len_icompnod) call mm_ovall
     &                  ('icompnod',cprtname,ipicompnod,ncompnod,1000
     &                  ,len_icompnod,1,icscode)
                     icompnod(ncompnod)=nodk
                  endif
               enddo
            enddo
 
c.... Special strategy:  If elimination of current component is
c.... 'barely' justified, loop through bounding volumes and make sure no
c.... bounding volume is 'nearly' about to be eliminated.  ('Barely'
c.... and 'nearly' means within DNEARFACTOR of tolerance.)  If so, it
c.... would be better (computationally much cheaper) to wait for the
c.... volume to be eliminated, and so to pass on this surface at this
c.... time.
 
            lbarely=.false.
            if (lvels_exist) then
               if (diameter(icurr).ge.tolsmalldist/dnearfactor.and
     &            .estcolltime(icurr).ge.toldt/dnearfactor) lbarely=
     &            .true.
            else
               if (diameter(icurr).ge.tolsmalldist/dnearfactor)
     &            lbarely=.true.
            endif
 
            if (lbarely) then
               do j=invboundaryptr(icurr),invboundaryptr(icurr+1)-1
                  jvol=invboundary(j)
                  if (lvels_exist) then
                     if (diameter(jvol).le.dnearfactor*tolsmalldist.or
     &                  .estcolltime(jvol).le.dnearfactor*toldt) goto
     &                  400
                  else
                     if (diameter(jvol).le.dnearfactor*tolsmalldist)
     &                  goto 400
                  endif
               enddo
            endif
 
            call hpsorti(ncompnod,icompnod)
 
            ncompnod_=ncompnod
 
            ncompnod=1
            iprev=icompnod(1)
 
            do j=2,ncompnod_
               if (iprev.ne.icompnod(j)) then
                  iprev=icompnod(j)
                  ncompnod=ncompnod+1
                  if (ncompnod.gt.len_icompnod) call mm_ovall
     &               ('icompnod',cprtname,ipicompnod,ncompnod,1000
     &               ,len_icompnod,1,icscode)
                  icompnod(ncompnod)=icompnod(j)
               endif
            enddo
 
            icodim=1
 
            call getplusminus(icodim,icurr,cipluslist,ipipluslist
     &         ,len_ipluslist,cprtname,npluslist,ciminuslist
     &         ,ipiminuslist,len_iminuslist,nminuslist,cicompstack
     &         ,ipicompstack,len_icompstack,cicodimstack,ipicodimstack
     &         ,len_icodimstack,iboundaryptr,iboundary,invboundaryptr
     &         ,invboundary)
 
            if (nminuslist.eq.0) goto 400
 
c.... Inverse icompnod relation.
 
            do j=1,ncompnod
               invicompnod(icompnod(j))=j
            enddo
 
c... Now define ICATEGORY to be the inverse map of both IMINUSLIST and
c... IPLUSLIST, in the sense that ICATEGORY(IPLUSLIST(I))=I , 1<=I<=NPLUSLIST
c... and ICATEGORY(IMINUSLIST(I))=-I , 1<=I<=NMINUSLIST.  The name
c... ICATEGORY stems from the fact that the sign of ICATEGORY(IVC) tells
c... which category volume component IVC is in.
 
            do j=1,npluslist
               icategory(ipluslist(j))=j
            enddo
            do j=1,nminuslist
               icategory(iminuslist(j))=-j
            enddo
 
            if (nminuslist.gt.len_thetaminus) call mm_ovall('thetaminus'
     &         ,cprtname,ipthetaminus,nminuslist,100,len_thetaminus,2
     &         ,icscode)
            if (npluslist.gt.len_thetaplus) call mm_ovall('thetaplus'
     &         ,cprtname,ipthetaplus,npluslist,100,len_thetaplus,2
     &         ,icscode)
 
            call getcompnbd(icodim,cprtname,ipicomphyb,len_icomphyb
     &         ,ncomphyb,icompnod,invicompnod,ncompnod,nodhyb,nodhyboff
     &         ,xic,yic,zic,npluslist,nminuslist,ipluslist,iminuslist
     &         ,thetaplus,thetaminus,iboundaryptr,iboundary
     &         ,invboundaryptr,invboundary,ifeature,ifeatureptr,invmpary
     &         ,ipicompstack,cicompstack,len_icompstack,ipicodimstack
     &         ,cicodimstack,len_icodimstack,len_itarglist,len_vecx
     &         ,len_vecy,len_vecz,nef_cmo,itet,itetoff,itettyp,iparent
     &         ,mpary)
 
            length=max(npluslist,nminuslist)
            if (length.gt.len_iprm1) call mm_ovall('iprm1',cprtname,
     &         ipiprm1,length,100,len_iprm1,1,icscode)
 
            ascend=-1.
            do j=1,npluslist
               iprm1(j)=j
            enddo
            call hpsort1(npluslist,thetaplus,ascend,iprm1)
            iplusmin=iprm1(npluslist)
            thetaplusmin=thetaplus(iplusmin)
 
            ascend=-1.
            do j=1,nminuslist
               iprm1(j)=j
            enddo
            call hpsort1(nminuslist,thetaminus,ascend,iprm1)
            iminusmin=iprm1(nminuslist)
            thetaminusmin=thetaminus(iminusmin)
 
            lchange=.false.
 
            if (lvels_exist) then
               if (estcolltime(icurr).le.toldt) then
                  lchange=.true.
               else
                  if (thetaplusmin.lt.thetaminusmin) then
                     lchange=.true.
                  endif
               endif
            else
               if (thetaplusmin.lt.thetaminusmin) then
                  lchange=.true.
               endif
            endif
 
            if (.not.lchange) goto 390
 
            ilosingcomp=ipluslist(iplusmin)
            ihyb=ifeature(ifeatureptr(ilosingcomp))
            ilosingmat=itetclr(ihyb)
 
            numbdryloser=iboundaryptr(ilosingcomp+1)
     &         -iboundaryptr(ilosingcomp)
 
            if (locverbosity.ge.2) then
               print*,'Surface: ICURR=',icurr
               print*,'iminuslist(1:nminuslist)=',(iminuslist(j1),j1=1
     &            ,nminuslist)
               print*,'ipluslist(1:npluslist)=',(ipluslist(j1),j1=1
     &            ,npluslist)
               print*,'-volumes:',(volume(iminuslist(j1)),j1=1
     &            ,nminuslist)
               print*,'+volumes:',(volume(ipluslist(j1)),j1=1,npluslist)
               print*,'-thetas/4pi:',(thetaminus(j1)/(4*
     &            3.1415926535897932d0),j1=1,nminuslist)
               print*,'+thetas/4pi:',(thetaplus(j1)/(4*
     &            3.1415926535897932d0),j1=1,npluslist)
            endif
 
            do 380 j=1,nminuslist
               icand=iminuslist(iprm1(j))
 
c... We assume that iboundary and invboundary are in ascending order.
 
               numbdrycand=iboundaryptr(icand+1)-iboundaryptr(icand)
 
c... We check that ICAND and ILOSINGCOMP at least one common
c... boundary component.
 
               ncompstack=0
               if (numbdryloser.ne.0.and.numbdrycand.ne.0) then
                  iposcand=iboundaryptr(icand)
                  do k=iboundaryptr(ilosingcomp)
     &               ,iboundaryptr(ilosingcomp+1)-1
 30                  continue
                     if (iboundary(iposcand).lt.iboundary(k)) then
                        iposcand=iposcand+1
                        if (iposcand.ge.iboundaryptr(icand+1)) then
                           goto 45
                        else
                           goto 30
                        endif
                     elseif (iboundary(iposcand).eq.iboundary(k))
     &                     then
                        ncompstack=ncompstack+1
                        if (ncompstack.gt.len_icompstack) call mm_ovall
     &                     ('icompstack',cprtname,ipicompstack
     &                     ,ncompstack,100,len_icompstack,1,icscode)
                        if (ncompstack.gt.len_icodimstack) call mm_ovall
     &                     ('icodimstack',cprtname,ipicodimstack
     &                     ,ncompstack,100,len_icodimstack,1,icscode)
                        icompstack(ncompstack)=iboundary(k)
                        icodimstack(ncompstack)=1
                     endif
                  enddo
               endif
 
 45            continue
 
               if (ncompstack.gt.0) then
                  itargcodim=icodim+1
                  call gettargcodimcomps(ncompstack,cicompstack
     &               ,ipicompstack,len_icompstack,cicodimstack
     &               ,ipicodimstack,len_icodimstack,itargcodim,cprtname
     &               ,clistout,iplistout,len_listout,nlistout
     &               ,iboundaryptr,iboundary,invboundaryptr,invboundary)
                  if (nlistout.gt.0) then
 
c.... If ICAND and ILOSINGCOMP has one or more common boundary components, we
c.... we check that at least one is 'valid' in that it's own boundary
c.... is part of the boundary of ICURR.  (I.e., the common boundary
c.... component is not an irrelevant intersection not close to ICURR.)
 
                     iposlist=1
                     laccept=.false.
                     do k=iboundaryptr(icurr),iboundaryptr(icurr+1)-1
 31                     continue
                        if (listout(iposlist).lt.iboundary(k)) then
                           iposlist=iposlist+1
                           if (iposlist.gt.nlistout) then
                              goto 46
                           else
                              goto 31
                           endif
                        elseif (listout(iposlist).eq.iboundary(k)) then
                           laccept=.true.
                           goto 46
                        endif
                     enddo
 
 46                  continue
 
                     if (.not.laccept) goto 380
 
                     iwinningcomp=icand
                     ihyb=ifeature(ifeatureptr(iwinningcomp))
                     iwinningmat=itetclr(ihyb)
 
                     call correctcompnbd(icodim,locrecolor,locrefine
     &                  ,ncomphyb,icomphyb,ieltary,itettyp,iparent
     &                  ,invicompnod,invmpary,mpary,mpno,ncompnod
     &                  ,icompnod,xic,yic,zic,epsilonl,itp1,isn1,imt1
     &                  ,icr1,icontab,itet,itetoff
     &                  ,cut_length,nadd,ipikey,len_ikey,ipitadd
     &                  ,len_itadd,ipieadd,len_ieadd,ipiadd,len_iadd
     &                  ,ipitpadd,len_itpadd,ipicradd,len_icradd
     &                  ,cprtname,ipxadd,len_xadd,ipyadd,len_yadd,ipzadd
     &                  ,len_zadd,lockout,ivcomp,nef_cmo,iwinningmat
     &                  ,ilosingcomp,itetclr,icurr,ifeatureptr,ifeature
     &                  ,len_iout,len_ktet,len_iout_first,len_icompmat
     &                  ,len_icompicr)
 
                     lrefine=lrefine.or.locrefine
                     lrecolor=lrecolor.or.locrecolor
 
                     goto 390
                  endif
               endif
 380        continue
 390        continue
 
c.... Rezero only those elements of INVICOMPNOD, ICATEGORY that need
c.... to be rezeroed.
 
            do j=1,ncompnod
               invicompnod(icompnod(j))=0
            enddo
 
            do j=1,npluslist
               icategory(ipluslist(j))=0
            enddo
            do j=1,nminuslist
               icategory(iminuslist(j))=0
            enddo
 
 400     continue
 410     continue
 
c.... We now sort the interlines using ESTCOLLTIME as a key.  Thus, we
c.... will try to eliminate first connected lines whose estimated
c.... collapse is the most imminent.  However, any nonexpanding
c.... lines that are sufficiently small will be considered for
c.... elimination.
 
         length=isppcompstart-ilcompstart
         if (length.gt.len_iprm) call mm_ovall('iprm',cprtname,
     &      ipiprm,length,100,len_iprm,1,icscode)
         do i=ilcompstart,isppcompstart-1
            iprm(i)=i
         enddo
 
         if (lvels_exist) then
            ascend=1.
            call hpsort1(isppcompstart-ilcompstart,estcolltime,ascend
     &         ,iprm(ilcompstart))
         else
            ascend=1.
            call hpsort1(isppcompstart-ilcompstart,volume,ascend
     &         ,iprm(ilcompstart))
         endif
 
         do 500 i=ilcompstart,isppcompstart-1
            icurr=iprm(i)
 
            if (lvels_exist) then
 
               if (dvoldt(icurr).gt.zero) then
                  voladj=dvoldt(icurr)*toldt
               else
                  voladj=0.d0
               endif
 
               if (volume(icurr)+voladj.gt.tolsmalldist.and
     &            .estcolltime(icurr).gt.toldt)goto 500
            else
               if (volume(icurr).gt.tolsmalldist) goto 510
            endif
 
c.... Form list of nodes in this component.  Also, if any node
c.... has been locked out, skip this component.
 
            ncompnod=0
            do j=ifeatureptr(icurr),ifeatureptr(icurr+1)-2
               nod=invmpary(ifeature(j))
               if (nod.eq.0) then
                  print*,'Error!  Special point not in MPARY.'
                  call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &               ,icscode)
                  stop
               endif
               if (lockout(nod)) goto 500
               ncompnod=ncompnod+1
               if (ncompnod.gt.len_icompnod) call mm_ovall
     &            ('icompnod',cprtname,ipicompnod,ncompnod,1000
     &            ,len_icompnod,1,icscode)
               icompnod(ncompnod)=nod
            enddo
            i1=ifeatureptr(icurr)
            i2=ifeatureptr(icurr+1)-1
            if (ifeature(i1).ne.ifeature(i2)) then
               nod=invmpary(ifeature(i2))
               if (nod.eq.0) then
                  print*,'Error!  Special point not in MPARY.'
                  call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &               ,icscode)
                  stop
               endif
               if (lockout(nod)) goto 500
               ncompnod=ncompnod+1
               if (ncompnod.gt.len_icompnod) call mm_ovall
     &            ('icompnod',cprtname,ipicompnod,ncompnod,1000
     &            ,len_icompnod,1,icscode)
               icompnod(ncompnod)=nod
            endif
 
c.... Special strategy:  If elimination of current component is
c.... 'barely' justified, loop through bounding surfaces and make sure no
c.... bounding surface is 'nearly' about to be eliminated.  ('Barely'
c.... and 'nearly' means within DNEARFACTOR of tolerance.)  If so, it
c.... would be better (computationally much cheaper) to wait for the
c.... surface to be eliminated, and so to pass on this interline at this
c.... time.
 
            lbarely=.false.
            if (lvels_exist) then
               if (volume(icurr).ge.tolsmalldist/dnearfactor.and
     &            .estcolltime(icurr).ge.toldt/dnearfactor) lbarely=
     &            .true.
            else
               if (volume(icurr).ge.tolsmalldist/dnearfactor)
     &            lbarely=.true.
            endif
 
            if (lbarely) then
               do j=invboundaryptr(icurr),invboundaryptr(icurr+1)-1
                  jsurf=invboundary(j)
                  if (lvels_exist) then
                     if (diameter(jsurf).le.dnearfactor*tolsmalldist.or
     &                  .estcolltime(jsurf).le.dnearfactor*toldt) goto
     &                  500
                  else
                     if (diameter(jsurf).le.dnearfactor*tolsmalldist)
     &                  goto 500
                  endif
               enddo
            endif
 
            icodim=2
 
            call getplusminus(icodim,icurr,cipluslist,ipipluslist
     &         ,len_ipluslist,cprtname,npluslist,ciminuslist
     &         ,ipiminuslist,len_iminuslist,nminuslist,cicompstack
     &         ,ipicompstack,len_icompstack,cicodimstack,ipicodimstack
     &         ,len_icodimstack,iboundaryptr,iboundary,invboundaryptr
     &         ,invboundary)
 
            if (nminuslist.eq.0) goto 500
 
c.... Inverse icompnod relation.
 
            do j=1,ncompnod
               invicompnod(icompnod(j))=j
            enddo
 
c... Now define ICATEGORY to be the inverse map of both IMINUSLIST and
c... IPLUSLIST, in the sense that ICATEGORY(IPLUSLIST(I))=I , 1<=I<=NPLUSLIST
c... and ICATEGORY(IMINUSLIST(I))=-I , 1<=I<=NMINUSLIST.  The name
c... ICATEGORY stems from the fact that the sign of ICATEGORY(IVC) tells
c... which category volume component IVC is in.
 
            do j=1,npluslist
               icategory(ipluslist(j))=j
            enddo
            do j=1,nminuslist
               icategory(iminuslist(j))=-j
            enddo
 
            if (nminuslist.gt.len_thetaminus) call mm_ovall('thetaminus'
     &         ,cprtname,ipthetaminus,nminuslist,100,len_thetaminus,2
     &         ,icscode)
            if (npluslist.gt.len_thetaplus) call mm_ovall('thetaplus'
     &         ,cprtname,ipthetaplus,npluslist,100,len_thetaplus,2
     &         ,icscode)
 
            call getcompnbd(icodim,cprtname,ipicomphyb,len_icomphyb
     &         ,ncomphyb,icompnod,invicompnod,ncompnod,nodhyb,nodhyboff
     &         ,xic,yic,zic,npluslist,nminuslist,ipluslist,iminuslist
     &         ,thetaplus,thetaminus,iboundaryptr,iboundary
     &         ,invboundaryptr,invboundary,ifeature,ifeatureptr,invmpary
     &         ,ipicompstack,cicompstack,len_icompstack,ipicodimstack
     &         ,cicodimstack,len_icodimstack,len_itarglist,len_vecx
     &         ,len_vecy,len_vecz,nef_cmo,itet,itetoff,itettyp,iparent
     &         ,mpary)
 
            length=max(npluslist,nminuslist)
            if (length.gt.len_iprm1) call mm_ovall('iprm1',cprtname,
     &         ipiprm1,length,100,len_iprm1,1,icscode)
 
            ascend=-1.
            do j=1,npluslist
               iprm1(j)=j
            enddo
            call hpsort1(npluslist,thetaplus,ascend,iprm1)
            iplusmin=iprm1(npluslist)
            thetaplusmin=thetaplus(iplusmin)
 
            ascend=-1.
            do j=1,nminuslist
               iprm1(j)=j
            enddo
            call hpsort1(nminuslist,thetaminus,ascend,iprm1)
            iminusmin=iprm1(nminuslist)
            thetaminusmin=thetaminus(iminusmin)
 
            lchange=.false.
 
            if (lvels_exist) then
               if (estcolltime(icurr).le.toldt) then
                  lchange=.true.
               else
                  if (thetaplusmin.lt.thetaminusmin) then
                     lchange=.true.
                  endif
               endif
            else
               if (thetaplusmin.lt.thetaminusmin) then
                  lchange=.true.
               endif
            endif
 
            if (.not.lchange) goto 490
 
            ilosingcomp=ipluslist(iplusmin)
            ihyb=ifeature(ifeatureptr(ilosingcomp))
            ilosingmat=itetclr(ihyb)
 
            numbdryloser=iboundaryptr(ilosingcomp+1)
     &         -iboundaryptr(ilosingcomp)
 
            if (locverbosity.ge.2) then
               print*,'Interline: ICURR=',icurr
               print*,'iminuslist(1:nminuslist)=',(iminuslist(j1),j1=1
     &            ,nminuslist)
               print*,'ipluslist(1:npluslist)=',(ipluslist(j1),j1=1
     &            ,npluslist)
               print*,'-volumes:',(volume(iminuslist(j1)),j1=1
     &            ,nminuslist)
               print*,'+volumes:',(volume(ipluslist(j1)),j1=1,npluslist)
               print*,'-thetas/4pi:',(thetaminus(j1)/(4*
     &            3.1415926535897932d0),j1=1,nminuslist)
               print*,'+thetas/4pi:',(thetaplus(j1)/(4*
     &            3.1415926535897932d0),j1=1,npluslist)
            endif
 
            do 480 j=1,nminuslist
               icand=iminuslist(iprm1(j))
 
c... We assume that iboundary and invboundary are in ascending order.
 
               numbdrycand=iboundaryptr(icand+1)-iboundaryptr(icand)
 
               ncompstack=0
               if (numbdryloser.ne.0.and.numbdrycand.ne.0) then
                  iposcand=iboundaryptr(icand)
                  do k=iboundaryptr(ilosingcomp)
     &               ,iboundaryptr(ilosingcomp+1)-1
 35                  continue
                     if (iboundary(iposcand).lt.iboundary(k)) then
                        iposcand=iposcand+1
                        if (iposcand.ge.iboundaryptr(icand+1)) then
                           goto 55
                        else
                           goto 35
                        endif
                     elseif (iboundary(iposcand).eq.iboundary(k))
     &                     then
                        ncompstack=ncompstack+1
                        icompstack(ncompstack)=iboundary(k)
                        icodimstack(ncompstack)=1
                     endif
                  enddo
               endif
 
 55            continue
 
               if (ncompstack.gt.0) then
                  itargcodim=icodim+1
                  call gettargcodimcomps(ncompstack,cicompstack
     &               ,ipicompstack,len_icompstack,cicodimstack
     &               ,ipicodimstack,len_icodimstack,itargcodim,cprtname
     &               ,clistout,iplistout,len_listout,nlistout
     &               ,iboundaryptr,iboundary,invboundaryptr,invboundary)
                  if (nlistout.gt.0) then
 
c.... If ICAND and ILOSINGCOMP has one or more common boundary components, we
c.... we check that at least one is 'valid' in that it's own boundary
c.... is part of the boundary of ICURR.  (I.e., the common boundary
c.... component is not an irrelevant intersection not close to ICURR.)
 
                     iposlist=1
                     laccept=.false.
                     do k=iboundaryptr(icurr),iboundaryptr(icurr+1)-1
 32                     continue
                        if (listout(iposlist).lt.iboundary(k)) then
                           iposlist=iposlist+1
                           if (iposlist.gt.nlistout) then
                              goto 47
                           else
                              goto 32
                           endif
                        elseif (listout(iposlist).eq.iboundary(k)) then
                           laccept=.true.
                           goto 47
                        endif
                     enddo
 
 47                  continue
 
                     if (.not.laccept) goto 480
 
                     iwinningcomp=icand
                     ihyb=ifeature(ifeatureptr(iwinningcomp))
                     iwinningmat=itetclr(ihyb)
 
                     call correctcompnbd(icodim,locrecolor,locrefine
     &                  ,ncomphyb,icomphyb,ieltary,itettyp,iparent
     &                  ,invicompnod,invmpary,mpary,mpno,ncompnod
     &                  ,icompnod,xic,yic,zic,epsilonl,itp1,isn1,imt1
     &                  ,icr1,icontab,itet,itetoff
     &                  ,cut_length,nadd,ipikey,len_ikey,ipitadd
     &                  ,len_itadd,ipieadd,len_ieadd,ipiadd,len_iadd
     &                  ,ipitpadd,len_itpadd,ipicradd,len_icradd
     &                  ,cprtname,ipxadd,len_xadd,ipyadd,len_yadd,ipzadd
     &                  ,len_zadd,lockout,ivcomp,nef_cmo,iwinningmat
     &                  ,ilosingcomp,itetclr,icurr,ifeatureptr,ifeature
     &                  ,len_iout,len_ktet,len_iout_first,len_icompmat
     &                  ,len_icompicr)
 
                     lrefine=lrefine.or.locrefine
                     lrecolor=lrecolor.or.locrecolor
 
                     goto 490
                  endif
               endif
 480        continue
 490        continue
 
c.... Rezero only those elements of INVICOMPNOD, ICATEGORY that need
c.... to be rezeroed.
 
            do j=1,ncompnod
               invicompnod(icompnod(j))=0
            enddo
 
            do j=1,npluslist
               icategory(ipluslist(j))=0
            enddo
            do j=1,nminuslist
               icategory(iminuslist(j))=0
            enddo
 
 500     continue
 510     continue
 
 
c.... Signal that we made changes to the mesh, if necessary.
 
         if (lrecolor.or.lrefine) xdum103=one
 
c.... Perform refinement if necessary.
 
         if (lrefine) then
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
 
c.... Check INVICOMPNOD, ICATEGORY, and INVIFEATURE did not acquire nonzeros.
 
         do i=1,mpno
            if (invicompnod(i).ne.0) then
               print*,'Error!  INVICOMPNOD acquired nonzeros.'
               call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &            ,icscode)
               stop
            endif
         enddo
         do i=ivcompstart,iscompstart-1
            if (icategory(i).ne.0) then
               print*,'Error!  ICATEGORY acquired nonzeros.'
               call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &            ,icscode)
               stop
            endif
         enddo
         do i=1,ieltno
            if (invifeature(i).ne.0) then
               print*,'Error!  INVIFEATURE acquired nonzeros.'
               call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &            ,icscode)
               stop
            endif
         enddo
 
c.... Now execute LaGriT commands to redefine parent child system and
c.... ITP1 based on new tet colors.
 
         if (lrecolor) then
            if (locdebug.ge.1) then
               call dotaskx3d
     &            ('dump/gmv/gmvrecolor/3dmesh/ascii ; finish',ierror
     &            )
               call dotaskx3d('dump/x3d/x3drecolor ; finish',ierror
     &            )
            endif
 
            call dotaskx3d('resetpts / parents ; finish', ierror)
            call dotaskx3d('geniee ; finish', ierror)
            call dotaskx3d('resetpts / itp ; finish', ierror)
            call dotaskx3d('settets / color_points ; finish', ierror)
            if (locdebug.ge.1) then
               call dotaskx3d('dump/x3d/x3dpostrecolor ; finish'
     &            ,ierror)
            endif
c$$$         call dotaskx3d('rmtetless ; finish', ierror)
 
         endif
 
         if (.not.(lrecolor.or.lrefine)) goto 9999
 
c.... Large grids demand point compression WITHIN the iteration loop.
 
         if (lcompresswherepossible) then
            call dotaskx3d('rmpoint/compress; finish',ierror)
         endif
      enddo
 
 9999 continue
 
      call mmrelprt(cprtname,icscode)
 
      return
      end
 
      subroutine getplusminus(icodim,icmp,cipluslist,ipipluslist
     &   ,len_ipluslist,cprtname,npluslist,ciminuslist,ipiminuslist
     &   ,len_iminuslist,nminuslist,cicompstack,ipicompstack
     &   ,len_icompstack,cicodimstack,ipicodimstack,len_icodimstack
     &   ,iboundaryptr,iboundary,invboundaryptr,invboundary)
 
      implicit none
 
      integer icodim,icmp,npluslist,nminuslist
     &   ,iboundaryptr(*),iboundary(*),invboundaryptr(*)
     &   ,invboundary(*)
 
c.... Pointered arrays passed in from partition CPRTNAME.
 
      pointer (ipipluslist,ipluslist)
      integer ipluslist(*),len_ipluslist
      character*(*) cipluslist
 
      pointer (ipiminuslist,iminuslist)
      integer iminuslist(*),len_iminuslist
      character*(*) ciminuslist
 
      pointer (ipicompstack,icompstack)
      integer icompstack(*),len_icompstack
      character*(*) cicompstack
 
      pointer (ipicodimstack,icodimstack)
      integer icodimstack(*),len_icodimstack
      character*(*) cicodimstack
 
      integer icscode,itop,itargcodim,i,iposp,nminuslist_
 
      integer locverbosity
      parameter (locverbosity=1)
 
      character*(*) cprtname
 
c.... 'Plus' list.  Put ICMP on stack and get list of associated volumes.
 
      if (icodim.eq.0) then
         npluslist=1
         if (npluslist.gt.len_ipluslist) call mm_ovall(cipluslist
     &      ,cprtname,ipipluslist,npluslist,100,len_ipluslist,1,icscode
     &      )
         ipluslist(npluslist)=icmp
      else
         itop=1
         if (itop.gt.len_icompstack) call mm_ovall(cicompstack,cprtname
     &      ,ipicompstack,itop,100,len_icompstack,1,icscode)
         if (itop.gt.len_icodimstack) call mm_ovall(cicodimstack
     &      ,cprtname,ipicodimstack,itop,100,len_icodimstack,1,icscode)
         icompstack(itop)=icmp
         icodimstack(itop)=icodim
 
         itargcodim=0
         call gettargcodimcomps(itop,cicompstack,ipicompstack
     &      ,len_icompstack,cicodimstack,ipicodimstack,len_icodimstack
     &      ,itargcodim,cprtname,cipluslist,ipipluslist,len_ipluslist
     &      ,npluslist,iboundaryptr,iboundary,invboundaryptr,invboundary
     &      )
      endif
 
c.... 'Minus' list.  Put iboundary(ICMP) on stack and get list of assoc.
c.... volumes.  Then subtract 'plus' list.
 
      itop=0
      do i=iboundaryptr(icmp),iboundaryptr(icmp+1)-1
         itop=itop+1
         if (itop.gt.len_icompstack) call mm_ovall(cicompstack,cprtname
     &      ,ipicompstack,itop,100,len_icompstack,1,icscode)
         if (itop.gt.len_icodimstack) call mm_ovall(cicodimstack
     &      ,cprtname,ipicodimstack,itop,100,len_icodimstack,1,icscode)
         icompstack(itop)=iboundary(i)
         icodimstack(itop)=icodim+1
      enddo
      itargcodim=0
      call gettargcodimcomps(itop,cicompstack,ipicompstack
     &   ,len_icompstack,cicodimstack,ipicodimstack,len_icodimstack
     &   ,itargcodim,cprtname,ciminuslist,ipiminuslist,len_iminuslist
     &   ,nminuslist,iboundaryptr,iboundary,invboundaryptr,invboundary)
      iposp=1
      nminuslist_=nminuslist
      nminuslist=0
      do i=1,nminuslist_
         do while (iposp.le.npluslist.and.iminuslist(i).gt
     &      .ipluslist(iposp))
            iposp=iposp+1
         enddo
         if (iposp.gt.npluslist) then
            nminuslist=nminuslist+1
            iminuslist(nminuslist)=iminuslist(i)
         else
            if (iminuslist(i).lt.ipluslist(iposp)) then
               nminuslist=nminuslist+1
               iminuslist(nminuslist)=iminuslist(i)
            elseif (iminuslist(i).eq.ipluslist(iposp)) then
               iposp=iposp+1
            endif
         endif
      enddo
 
c.... Check:  IPLUSLIST should have been a subset of initial IMINUSLIST.
c.... Exception is if ICMP had no boundary.
 
      if (nminuslist_.ne.nminuslist+npluslist) then
         if (nminuslist_.gt.0) then
            print*
     &         ,'Error:  Improper IMINUSLIST/IPLUSLIST decomposition.'
            call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &         ,icscode)
            stop
         else
c... Fallback position??  Recolor with largest VOL IPLUSLIST comp.??
c... Probably best to do nothing and let something happen to
c... small components that have sole boundary ICMP.
            if (locverbosity.ge.1) print*,'Component ',icmp
     &         ,' has no boundary.'
         endif
      endif
 
      return
      end
 
      subroutine gettargcodimcomps(itop,cicompstack,ipicompstack
     &   ,len_icompstack,cicodimstack,ipicodimstack,len_icodimstack
     &   ,itargcodim,cprtname,clistout,iplistout,len_listout,nlistout
     &   ,iboundaryptr,iboundary,invboundaryptr,invboundary)
 
      implicit none
 
      integer itop,itargcodim,nlistout,iboundaryptr(*),iboundary(*)
     &   ,invboundaryptr(*),invboundary(*)
 
c.... Pointered arrays passed in from partition CPRTNAME.
 
      pointer (iplistout,listout)
      integer listout(*),len_listout
 
      pointer (ipicompstack,icompstack)
      integer icompstack(*),len_icompstack
 
      pointer (ipicodimstack,icodimstack)
      integer icodimstack(*),len_icodimstack
 
      integer icurr,icurrcodim,icscode,i,nlistout_,iprev
 
      integer locverbosity
      parameter (locverbosity=1)
 
      character*(*) cprtname,clistout,cicompstack,cicodimstack
 
      nlistout=0
 
      do while (itop.gt.0)
 
c.... Pop stack
 
      icurr=icompstack(itop)
      icurrcodim=icodimstack(itop)
      itop=itop-1
 
      if (icurrcodim.eq.itargcodim) then
         nlistout=nlistout+1
         if (nlistout.gt.len_listout) call mm_ovall(clistout
     &      ,cprtname,iplistout,nlistout,100,len_listout,1,icscode)
         listout(nlistout)=icurr
      elseif (icurrcodim.gt.itargcodim) then
         do i=invboundaryptr(icurr),invboundaryptr(icurr+1)-1
            itop=itop+1
            if (itop.gt.len_icompstack) call mm_ovall(cicompstack
     &         ,cprtname,ipicompstack,itop,100,len_icompstack,1,icscode)
            if (itop.gt.len_icodimstack) call mm_ovall(cicodimstack
     &         ,cprtname,ipicodimstack,itop,100,len_icodimstack,1
     &         ,icscode)
            icompstack(itop)=invboundary(i)
            icodimstack(itop)=icurrcodim-1
         enddo
      else
         do i=iboundaryptr(icurr),iboundaryptr(icurr+1)-1
            itop=itop+1
            if (itop.gt.len_icompstack) call mm_ovall(cicompstack
     &         ,cprtname,ipicompstack,itop,100,len_icompstack,1,icscode)
            if (itop.gt.len_icodimstack) call mm_ovall(cicodimstack
     &         ,cprtname,ipicodimstack,itop,100,len_icodimstack,1
     &         ,icscode)
            icompstack(itop)=iboundary(i)
            icodimstack(itop)=icurrcodim+1
         enddo
      endif
 
      enddo
 
c.... Sort the list and eliminate duplicates.
 
      if (nlistout.gt.0) then
 
         call hpsorti(nlistout,listout)
 
         nlistout_=nlistout
 
         nlistout=1
         iprev=listout(1)
 
         do i=2,nlistout_
            if (iprev.ne.listout(i)) then
               iprev=listout(i)
               nlistout=nlistout+1
               listout(nlistout)=listout(i)
            endif
         enddo
 
      else
 
         if (locverbosity.ge.1) print*
     &      ,'GETTARGCODIMCOMPS:  Warning:  No target components!'
 
      endif
 
      return
      end
 
      subroutine getcompnbd(icodim,cprtname,ipicomphyb,len_icomphyb
     &   ,ncomphyb,icompnod,invicompnod,ncompnod,nodhyb,nodhyboff,xic
     &   ,yic,zic,npluslist,nminuslist,ipluslist,iminuslist,thetaplus
     &   ,thetaminus,iboundaryptr,iboundary,invboundaryptr,invboundary
     &   ,ifeature,ifeatureptr,invmpary,ipicompstack,cicompstack
     &   ,len_icompstack,ipicodimstack,cicodimstack,len_icodimstack
     &   ,len_itarglist,len_vecx,len_vecy,len_vecz,nef_cmo,itet,itetoff
     &   ,itettyp,iparent,mpary)
 
      implicit none
 
      include 'local_element.h'
 
      integer ncomphyb,icompnod(*),ncompnod,nodhyb(*),nodhyboff(*)
     &   ,npluslist,nminuslist,ipluslist(*),iminuslist(*)
     &   ,iboundaryptr(*),iboundary(*),invboundaryptr(*),invboundary(*)
     &   ,ifeature(*),ifeatureptr(*),invmpary(*),invicompnod(*),icodim
     &   ,len_itarglist,len_vecx,len_vecy,len_vecz,nef_cmo,itet(*)
     &   ,itetoff(*),itettyp(*),iparent(*),mpary(*)
 
      real*8 xic(*),yic(*),zic(*),thetaplus(*),thetaminus(*),thetaest_lg
 
c.... Pointered arrays passed in from partition CPRTNAME.
 
      pointer (ipicompstack,icompstack)
      integer icompstack(*),len_icompstack
      character*(*) cicompstack
 
      pointer (ipicodimstack,icodimstack)
      integer icodimstack(*),len_icodimstack
      character*(*) cicodimstack
 
      pointer (ipicomphyb,icomphyb)
      integer icomphyb(*),len_icomphyb
 
      integer i,j,nodi,icscode
 
      character*(*) cprtname
 
c.... We form the list of all tets pointwise incident on a given component
c.... (the "neighborhood" of the component).  The we compute the
c.... solid angles of each incident volume component.  If the component
c.... were only a point, then the solid angle for a given
c.... incident component INC would equal the sum of the solid angles in the
c.... set of tetrahedra in INC that are incident on the component.  (For each
c.... tet, the solid angle is of course measured at the vertex located
c.... at the component.)  However, since the component is not a point, the definition
c.... of solid angle must be generalized, and this generalization is
c.... certainly nonunique.  What is done is that we throw out all
c.... tetrahedra in a component that have more than one vertex in the component.
c.... The idea is that by extending the tetrahedra incident upon the component
c.... so that they intersect a sphere centred on the component, only the
c.... tets contributing a face-intersection with the sphere (i.e.
c.... only those tets intersecting the component at one point) will contribute
c.... a positive intersection area, and hence a positive solid angle.
c.... This is at best a rough argument, because the extended tetrahedra
c.... can overlap.
 
c.... Form list of tets pointwise incident on the component.
 
      ncomphyb=0
      do i=1,ncompnod
         nodi=icompnod(i)
         do j=nodhyboff(nodi)+1,nodhyboff(nodi+1)
            ncomphyb=ncomphyb+1
            if (ncomphyb.gt.len_icomphyb) call mm_ovall('icomphyb'
     &         ,cprtname,ipicomphyb,ncomphyb,100,len_icomphyb,1,icscode)
            icomphyb(ncomphyb)=nodhyb(j)
         enddo
      enddo
 
      call hpsorti(ncomphyb,icomphyb)
 
c.... We loop through all volumes incident on the components and obtain
c.... list of interlines that (i) border the volume and (ii) have
c.... exactly one endpoint in the component.  We estimate the solid
c.... angle of the volume by the 'spread' of the unit vectors formed
c.... by moving tangent to the interlines at their incident endpoints.
 
      if (icodim.eq.0) then
         thetaplus(1)=0.d0
      else
         do i=1,npluslist
            thetaplus(i)=thetaest_lg(ipluslist(i),cprtname,xic,yic,zic
     &         ,iboundaryptr,iboundary,invboundaryptr,invboundary
     &         ,invicompnod,ifeature,ifeatureptr,invmpary,ipicompstack
     &         ,cicompstack,len_icompstack,ipicodimstack,cicodimstack
     &         ,len_icodimstack,len_itarglist,len_vecx,len_vecy
     &         ,len_vecz,nef_cmo,itet,itetoff,itettyp,iparent,mpary
     &         ,icompnod,icodim)
         enddo
      endif
      do i=1,nminuslist
         thetaminus(i)=thetaest_lg(iminuslist(i),cprtname,xic,yic,zic
     &      ,iboundaryptr,iboundary,invboundaryptr,invboundary
     &      ,invicompnod,ifeature,ifeatureptr,invmpary,ipicompstack
     &      ,cicompstack,len_icompstack,ipicodimstack,cicodimstack
     &      ,len_icodimstack,len_itarglist,len_vecx,len_vecy,len_vecz
     &      ,nef_cmo,itet,itetoff,itettyp,iparent,mpary,icompnod,icodim)
      enddo
 
      return
      end
 
      subroutine correctcompnbd(icodim,locrecolor,locrefine,ncomphyb
     &   ,icomphyb,ieltary,itettyp,iparent,invicompnod,invmpary,mpary
     &   ,mpno,ncompnod,icompnod,xic,yic,zic,epsilonl,itp1,isn1,imt1
     &   ,icr1,icontab,itet,itetoff,cut_length
     &   ,nadd,ipikey,len_ikey,ipitadd,len_itadd,ipieadd,len_ieadd
     &   ,ipiadd,len_iadd,ipitpadd,len_itpadd,ipicradd,len_icradd
     &   ,cprtname,ipxadd,len_xadd,ipyadd,len_yadd,ipzadd,len_zadd
     &   ,lockout,ivcomp,nef_cmo,iwinningmat,ilosingcomp,itetclr,icmp
     &   ,ifeatureptr,ifeature,len_iout,len_ktet,len_iout_first
     &   ,len_icompmat,len_icompicr)
 
c... This subroutine corrects a small nonexpanding topological
c... component by either refining a small neighbourhood of the
c... component (collapsing surface and interline case only) or
c... recoloring the component and a small neighbourhood.
c... (The additional small neighbourhood is recolored only in
c... the surface and interline collapse case, which is why
c... refinement might only occur in these cases.)  If refinements
c... are necessary, no recoloring occurs.  If no refinement
c... is necessary, recoloring using the color in the 'minus list'
c... with the largest solid angle is undertaken.  Only materials
c... in the 'plus list' are recolored.
 
      implicit none
 
      include 'local_element.h'
      include 'chydro.h'
 
      integer icodim,ncomphyb,icomphyb(*),ieltary(*),itettyp(*),iparent(
     &   *),invicompnod(*),invmpary(*),mpary(*),mpno,ncompnod,icompnod(
     &   *),itp1(*),isn1(*),imt1(*),icr1(*),icontab(50,*)
     &   ,itet(*),itetoff(*),nadd,ivcomp(*)
     &   ,itetclr(*),nef_cmo,iwinningmat,ilosingcomp,iedg,icmp
     &   ,ifeatureptr(*),ifeature(*)
 
      logical locrecolor,locrefine,lockout(*)
 
      real*8 xic(*),yic(*),zic(*),epsilonl,cut_length
 
c.... Pointered arrays passed in from partition CPRTNAME.
 
      integer md
      parameter (md=3)
      pointer (ipikey,ikey)
      integer ikey(md,*),len_ikey
 
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
 
c.... Pointered arrays passed in partition CPRTNAME but only
c.... used locally.
 
      pointer (ipiout,iout)
      integer iout(*),len_iout
      save ipiout
 
      pointer (ipktet,ktet)
      integer ktet(*),len_ktet
      save ipktet
 
      pointer (ipiout_first,iout_first)
      integer iout_first(*),len_iout_first
      save ipiout_first
 
      pointer (ipicompmat,icompmat)
      integer icompmat(*),len_icompmat
      save ipicompmat
 
      pointer (ipicompicr,icompicr)
      integer icompicr(*),len_icompicr
      save ipicompicr
 
      integer ninout,i,ielt,iloc,ihyb,ityp,j,iloc1,iloc2,nod1,nod2,nodc
     &   ,icscode,itemp(10),ipos,iparin,iparout,k,nodek,ktrip,matk
     &   ,ierror,iposin,isout,jtetcurr,ifac,i1,i2,i3,nod,j1,nmat,inod
     &   ,imat,nmat_,iprev,ncr,icr,icrj,ncr_
 
      logical lsomeloser,lcontainsforeign
 
      real*8 edgelength,distmin,qx,qy,qz,x1,y1,z1,x2,y2,z2,x3
     &   ,y3,z3,dist,distpttri,distptedg,frac
 
      integer locverbosity
      parameter (locverbosity=1)
 
      character*(*) cprtname
 
      locrecolor=.false.
      locrefine=.false.
 
      if (icodim.eq.0) goto 10
 
c.... Compute "in-out-ktet" relation for the component.  That is,
c.... For each node IN the component give all neighboring nodes OUT of the
c.... component, and for each IN-OUT pair (i.e. for each edge LEAVING the component),
c.... provide a list of tetrahedra-local edges.  This list is generated
c.... by looping through all the tetrahedra incident upon the component.
 
      ninout=0
      do i=1,ncomphyb
         ielt=1+(icomphyb(i)-1)/maxnen
         iloc=icomphyb(i)-maxnen*(ielt-1)
         ihyb=ieltary(ielt)
         ityp=itettyp(ihyb)
         do j=1,nelmnee(ityp)
            iloc1=ielmedge1(1,j,ityp)
            iloc2=ielmedge1(2,j,ityp)
            nod1=iparent(itet(itetoff(ihyb)+iloc1))
            nod2=iparent(itet(itetoff(ihyb)+iloc2))
            if (iloc1.eq.iloc) then
               nodc=invicompnod(invmpary(nod1))
               if (invmpary(nod2).eq.0) then
                  ninout=ninout+1
                  if (3*ninout.gt.len_ikey) call mm_ovall
     &               ('ikey',cprtname,ipikey,3*ninout,300,len_ikey,1
     &               ,icscode)
                  ikey(1,ninout)=nodc
                  ikey(2,ninout)=nod2
                  ikey(3,ninout)=j+(ielt-1)*maxnee2
               elseif (invicompnod(invmpary(nod2)).eq.0) then
                  ninout=ninout+1
                  if (3*ninout.gt.len_ikey) call mm_ovall
     &               ('ikey',cprtname,ipikey,3*ninout,300,len_ikey,1
     &               ,icscode)
                  ikey(1,ninout)=nodc
                  ikey(2,ninout)=nod2
                  ikey(3,ninout)=j+(ielt-1)*maxnee2
               endif
            elseif (iloc2.eq.iloc) then
               nodc=invicompnod(invmpary(nod2))
               if (invmpary(nod1).eq.0) then
                  ninout=ninout+1
                  if (3*ninout.gt.len_ikey) call mm_ovall
     &               ('ikey',cprtname,ipikey,3*ninout,300,len_ikey,1
     &               ,icscode)
                  ikey(1,ninout)=nodc
                  ikey(2,ninout)=nod1
                  ikey(3,ninout)=j+(ielt-1)*maxnee2
               elseif (invicompnod(invmpary(nod1)).eq.0) then
                  ninout=ninout+1
                  if (3*ninout.gt.len_ikey) call mm_ovall
     &               ('ikey',cprtname,ipikey,3*ninout,300,len_ikey,1
     &               ,icscode)
                  ikey(1,ninout)=nodc
                  ikey(2,ninout)=nod1
                  ikey(3,ninout)=j+(ielt-1)*maxnee2
               endif
            endif
         enddo
      enddo
      call hpsortim(ninout,3,md,itemp,ikey)
 
      if (ninout.gt.len_iout) call mm_ovall
     &   ('iout',cprtname,ipiout,ninout,100,len_iout,1,icscode)
      if (ninout.gt.len_ktet) call mm_ovall
     &   ('ktet',cprtname,ipktet,ninout,100,len_ktet,1,icscode)
      do j=1,ninout
         iout(j)=ikey(2,j)
         ktet(j)=ikey(3,j)
      enddo
 
      if (ncompnod+1.gt.len_iout_first) call mm_ovall('iout_first'
     &   ,cprtname,ipiout_first,ncompnod+1,100,len_iout_first,1
     &   ,icscode)
 
      ipos=1
      do i=1,ncompnod
         iout_first(i)=ipos
         do while (ipos.le.ninout.and.ikey(1,ipos).eq.i)
            ipos=ipos+1
         enddo
      enddo
      iout_first(ncompnod+1)=ipos
 
      nmat=0
      do i=1,ncompnod
         iparin=mpary(icompnod(i))
         if (itp1(iparin).eq.ifitpcup) then
            inod=isn1(iparin)
            ktrip=0
            do while (inod.ne.iparin.and.ktrip.le.10000)
               ktrip=ktrip+1
               imat=imt1(inod)
               nmat=nmat+1
               if (nmat.gt.len_icompmat) call mm_ovall('icompmat'
     &            ,cprtname,ipicompmat,nmat,100,len_icompmat,1,icscode)
               icompmat(nmat)=imat
               inod=isn1(inod)
            enddo
            if (ktrip.gt.10000) then
               print*,'Bad isn sequence at ',iparout
               ierror=1
               goto 9999
            endif
         else
            imat=imt1(iparin)
            nmat=nmat+1
            if (nmat.gt.len_icompmat) call mm_ovall('icompmat'
     &         ,cprtname,ipicompmat,nmat,100,len_icompmat,1,icscode)
            icompmat(nmat)=imat
         endif
      enddo
 
      call hpsorti(nmat,icompmat)
 
      nmat_=nmat
      nmat=1
      iprev=icompmat(1)
      do i=2,nmat_
         if (icompmat(i).gt.iprev) then
            nmat=nmat+1
            icompmat(nmat)=icompmat(i)
            iprev=icompmat(i)
         endif
      enddo
 
      ncr=0
      do i=1,ncompnod
         iparin=mpary(icompnod(i))
         icr=icr1(iparin)
         if (icr.ne.0) then
            do j=1,icontab(1,icr)
               icrj=icontab(2+j,icr)
               ncr=ncr+1
               if (ncr.gt.len_icompicr) call mm_ovall('icompicr'
     &            ,cprtname,ipicompicr,ncr,100,len_icompicr,1,icscode)
               icompicr(ncr)=icrj
            enddo
         endif
      enddo
 
      if (ncr.gt.0) then
 
         call hpsorti(ncr,icompicr)
 
         ncr_=ncr
         ncr=1
         iprev=icompicr(1)
         do i=2,ncr_
            if (icompicr(i).gt.iprev) then
               ncr=ncr+1
               icompicr(ncr)=icompicr(i)
               iprev=icompicr(i)
            endif
         enddo
 
      endif
 
      do 20 i=1,ncompnod
         iparin=mpary(icompnod(i))
         iparout=0
         j=iout_first(i)
 
 30      continue
 
         do while (j.lt.iout_first(i+1).and.iout(j).eq.iparout)
            j=j+1
         enddo
         if (j.eq.iout_first(i+1)) then
            goto 20
         else
            iparout=iout(j)
         endif
 
c$$$c.... Loop through tets around edge between to determine
c$$$c.... if there are any tets with material in the 'plus list'
c$$$c.... which will be recolored.  If not, refinement of this
c$$$c.... edge is not necessary.
c$$$
c$$$            k=j
c$$$            lsomepluslist=.false.
c$$$            do while (k.lt.iout_first(i+1).and.iout(k).eq.iparout)
c$$$               ielt=1+(ktet(k)-1)/maxnee2
c$$$               icat=icategory(ivcomp(ielt))
c$$$               if (icat.gt.0) lsomepluslist=.true.
c$$$               k=k+1
c$$$            enddo
c$$$            if (.not.lsomepluslist) goto 30
 
c.... Loop through tets around edge between to determine
c.... if there are any tets in the 'losing component' which
c.... will be recolored.  If not, refinement of this edge is not
c.... necessary.
 
         k=j
         lsomeloser=.false.
         do while (k.lt.iout_first(i+1).and.iout(k).eq.iparout)
            ielt=1+(ktet(k)-1)/maxnee2
            if (ivcomp(ielt).eq.ilosingcomp) lsomeloser=.true.
            k=k+1
         enddo
         if (.not.lsomeloser) goto 30
 
c.... If edge is extremely short, refine it under no circumstances.
 
         edgelength=sqrt((xic(iparin)-xic(iparout))**2+(yic(iparin)
     &      -yic(iparout))**2+(zic(iparin)-zic(iparout))**2)
         if (edgelength.le.epsilonl) goto 30
 
         lcontainsforeign=.true.
 
c... If IPAROUT contains any materials or constraints foreign
c... to ICMP, we refine edge, even if it is short.
 
         if (itp1(iparout).eq.ifitpcup) then
            nodek=isn1(iparout)
            ktrip=0
            do while (nodek.ne.iparout.and.ktrip.le.10000)
               ktrip=ktrip+1
               matk=imt1(nodek)
               do j1=1,nmat
                  if (icompmat(j1).eq.matk) goto 110
               enddo
               goto 200         ! mat subset not true
 110           nodek=isn1(nodek)
            enddo
            if (ktrip.gt.10000) then
               print*,'Bad isn sequence at ',iparout
               ierror=1
               goto 9999
            endif
         endif
 
         if (icr1(iparout).ne.0) then
 
            if (ncr.eq.0) goto 200   ! constraint subset not true.
 
            iposin=1
 
            do k=1,icontab(1,icr1(iparout))
               isout=icontab(k+2,icr1(iparout))
 140           continue
               if (icompicr(iposin).lt.isout) then
                  iposin=iposin+1
                  if (iposin.gt.ncr) then
                     goto 200   ! constraint subset not true.
                  else
                     goto 140
                  endif
               elseif(icompicr(iposin).gt.isout) then
                  goto 200      ! constraint subset not true.
               endif
            enddo
         endif
 
c....If we made it to here, we have concluded that pointwise material
c.... and constraint inclusion is satisfied.
 
         lcontainsforeign=.false.
 
 200     continue
 
c.... Next, we determine the distance of iparout from the component.
c.... If the distance is more than CUT_LENGTH, we must refine.
 
         if (icodim.eq.1) then
 
            distmin=1.d99
            qx=xic(iparout)
            qy=yic(iparout)
            qz=zic(iparout)
            do j1=ifeatureptr(icmp),ifeatureptr(icmp+1)-1
               jtetcurr=ifeature(j1)
               ihyb=1+(jtetcurr-1)/nef_cmo
               ifac=jtetcurr-nef_cmo*(ihyb-1)
               ityp=itettyp(ihyb) ! just ornamental---type must be tetrahedron
               i1=itet(itetoff(ihyb)+ielmface1(1,ifac,ityp))
               i2=itet(itetoff(ihyb)+ielmface1(2,ifac,ityp))
               i3=itet(itetoff(ihyb)+ielmface1(3,ifac,ityp))
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               x3=xic(i3)
               y3=yic(i3)
               z3=zic(i3)
 
               dist=distpttri(qx,qy,qz,x1,y1,z1,x2,y2,z2,x3,y3,z3)
               if (dist.lt.distmin)distmin=dist
            enddo
 
         else if (icodim.eq.2) then
 
            distmin=1.d99
            qx=xic(iparout)
            qy=yic(iparout)
            qz=zic(iparout)
            do j1=ifeatureptr(icmp),ifeatureptr(icmp+1)-2
               i1=ifeature(j1)
               i2=ifeature(j1+1)
               x1=xic(i1)
               y1=yic(i1)
               z1=zic(i1)
               x2=xic(i2)
               y2=yic(i2)
               z2=zic(i2)
               dist=distptedg(qx,qy,qz,x1,y1,z1,x2,y2,z2)
               if (dist.lt.distmin)distmin=dist
            enddo
 
         endif
 
         if ((.not.lcontainsforeign).and.(distmin.le.1.1*cut_length))
     &      goto 30
 
c.... Emit refinement data for edge.
 
         locrefine=.true.
         nadd=nadd+1
 
         if (nadd.gt.len_itadd) call mm_ovall('itadd',cprtname,
     &      ipitadd,nadd,100+mpno,len_itadd,1,icscode)
         if (nadd.gt.len_ieadd) call mm_ovall('ieadd',cprtname,
     &      ipieadd,nadd,100+mpno,len_ieadd,1,icscode)
         if (nadd.gt.len_iadd) call mm_ovall('iadd',cprtname,
     &      ipiadd,nadd,100+mpno,len_iadd,1,icscode)
         if (nadd.gt.len_itpadd) call mm_ovall('itpadd',cprtname,
     &      ipitpadd,nadd,100+mpno,len_itpadd,1,icscode)
         if (nadd.gt.len_icradd) call mm_ovall('icradd',cprtname,
     &      ipicradd,nadd,100+mpno,len_icradd,1,icscode)
         if (nadd.gt.len_xadd) call mm_ovall('xadd',cprtname,
     &      ipxadd,nadd,100+mpno,len_xadd,2,icscode)
         if (nadd.gt.len_yadd) call mm_ovall('yadd',cprtname,
     &      ipyadd,nadd,100+mpno,len_yadd,2,icscode)
         if (nadd.gt.len_zadd) call mm_ovall('zadd',cprtname,
     &      ipzadd,nadd,100+mpno,len_zadd,2,icscode)
 
         ielt=(ktet(j)-1)/maxnee2+1
         iedg=ktet(j)-(ielt-1)*maxnee2
         ihyb=ieltary(ielt)
 
         if (locverbosity.ge.1) print*,'nadd=',nadd,'; iparin/iparout='
     &      ,iparin,'/',iparout
 
         itadd(nadd)=ihyb
         ieadd(nadd)=iedg
         iadd(nadd)=0
 
         frac=min(0.5d0,cut_length/distmin)
         xadd(nadd)=xic(iparout)*frac+xic(iparin)*(1.d0-frac)
         yadd(nadd)=yic(iparout)*frac+yic(iparin)*(1.d0-frac)
         zadd(nadd)=zic(iparout)*frac+zic(iparin)*(1.d0-frac)
 
         k=j
         do while(k.lt.iout_first(i+1).and.iout(k).eq.iparout)
            ielt=1+(ktet(k)-1)/maxnee2
            ihyb=ieltary(ielt)
            ityp=itettyp(ihyb)
            do j1=1,nelmnen(ityp)
               nod=invmpary(itet(itetoff(ihyb)+j1))
               if (nod.ne.0) lockout(nod)=.true.
            enddo
            k=k+1
         enddo
 
         goto 30
 
 20   continue
 
 10   continue
 
      if (.not.locrefine) then
         locrecolor=.true.
         do i=1,ncomphyb
            ielt=1+(icomphyb(i)-1)/maxnen
            ihyb=ieltary(ielt)
            ityp=itettyp(ihyb)
            if (ivcomp(ielt).eq.ilosingcomp) then
               itetclr(ihyb)=iwinningmat
               do j=1,nelmnen(ityp)
                  nod=invmpary(itet(itetoff(ihyb)+j))
                  if (nod.ne.0) lockout(nod)=.true.
               enddo
            endif
         enddo
      endif
 
 9999 continue
 
      return
      end
 
 
      function distptedg(qx,qy,qz,x1,y1,z1,x2,y2,z2)
 
      implicit none
 
      include 'consts.h'
 
      real*8 qx,qy,qz,x1,y1,z1,x2,y2,z2,distptedg
 
      real*8 xic(2),yic(2),zic(2),ax,ay,az,bx,by,bz,denom,t,xs,ys,zs
     &   ,dist1,dist2
 
      xic(1)=x1
      yic(1)=y1
      zic(1)=z1
      xic(2)=x2
      yic(2)=y2
      zic(2)=z2
 
      ax=x2-x1
      ay=y2-y1
      az=z2-z1
      bx=qx-x1
      by=qy-y1
      bz=qz-z1
      denom=ax*ax+ay*ay+az*az
      t=(ax*bx+ay*by+az*bz)/denom
      if (t .gt. zero .and. t .lt. one) then
         xs=t*x2+(1.0-t)*x1
         ys=t*y2+(1.0-t)*y1
         zs=t*z2+(1.0-t)*z1
         distptedg=sqrt( (qx-xs)**2+(qy-ys)**2+(qz-zs)**2 )
      else
         dist1=sqrt((qx-x1)**2+(qy-y1)**2+(qz-z1)**2)
         dist2=sqrt((qx-x2)**2+(qy-y2)**2+(qz-z2)**2)
         distptedg=min(dist1,dist2)
      endif
      return
      end
 
      function distpttri(qx,qy,qz,x1,y1,z1,x2,y2,z2,x3,y3,z3)
 
      implicit none
 
      include 'consts.h'
 
      real*8 distpttri,qx,qy,qz,x1,y1,z1,x2,y2,z2,x3,y3,z3
 
      integer iprev3,next3,ii,i,nedges,i1,i2,iface
 
      real*8 xic(3),yic(3),zic(3),dist,ax,ay,az,bx,by,bz,denom,t,xs,ys
     &   ,zs,tx,ty,tz,sgndist,area,tnorm
 
      real*8 alargenumber
      parameter (alargenumber=1.d99)
 
      include 'statementfunctions.h'
 
      iprev3(ii)=mod(ii+1,3)+1
      next3(ii)=mod(ii,3)+1
 
      xic(1)=x1
      yic(1)=y1
      zic(1)=z1
      xic(2)=x2
      yic(2)=y2
      zic(2)=z2
      xic(3)=x3
      yic(3)=y3
      zic(3)=z3
 
 
      distpttri=alargenumber
 
c.... Distance to each of the triangle's vertices.
 
      do i=1,3
         dist=sqrt((xic(i)-qx)**2+(yic(i)-qy)**2+(zic(i)-qz)**2)
         distpttri=min(distpttri,dist)
      enddo
 
c.... Distance to each of the triangle's edges, provided it is 'legal'.
 
      nedges=0
      do i=1,3
         i1=next3(i)
         i2=iprev3(i)
         ax=xic(i2)-xic(i1)
         ay=yic(i2)-yic(i1)
         az=zic(i2)-zic(i1)
         bx=qx-xic(i1)
         by=qy-yic(i1)
         bz=qz-zic(i1)
         denom=ax*ax+ay*ay+az*az
         t=(ax*bx+ay*by+az*bz)/denom
         if (t .gt. zero .and. t .lt. one) then
            nedges=nedges+1
            xs=t*xic(i2)+(1.0-t)*xic(i1)
            ys=t*yic(i2)+(1.0-t)*yic(i1)
            zs=t*zic(i2)+(1.0-t)*zic(i1)
            dist=sqrt( (qx-xs)**2+(qy-ys)**2+(qz-zs)**2 )
            distpttri=min(distpttri,dist)
         endif
      enddo
 
c.... Distance to the triangle's face, provided it is 'legal'.
 
      if (nedges.gt.1) then
         tx=dcrosx(xic(1),yic(1),zic(1),xic(2),yic(2),zic(2),xic(3)
     &      ,yic(3),zic(3))
         ty=dcrosy(xic(1),yic(1),zic(1),xic(2),yic(2),zic(2),xic(3)
     &      ,yic(3),zic(3))
         tz=dcrosz(xic(1),yic(1),zic(1),xic(2),yic(2),zic(2),xic(3)
     &      ,yic(3),zic(3))
         tnorm=sqrt(tx**2+ty**2+tz**2)
         tx=tx/tnorm
         ty=ty/tnorm
         tz=tz/tnorm
 
         iface=1
         sgndist=(qx-x1)*tx+(qy-y1)*ty+(qz-z1)*tz
 
         xs=qx-tx*sgndist
         ys=qy-ty*sgndist
         zs=qz-tz*sgndist
 
         do i=1,3
            i1=i
            i2=next3(i)
            area=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &         ,xs,ys,zs)*tx+dcrosy(xic(i1),yic(i1),zic(i1),xic(i2)
     &         ,yic(i2),zic(i2),xs,ys,zs)*ty+dcrosz(xic(i1),yic(i1)
     &         ,zic(i1),xic(i2),yic(i2),zic(i2),xs,ys,zs)*tz
            if (area.le.zero) iface=0
         enddo
         if (iface .eq. 1) then
            distpttri=min(distpttri,abs(sgndist))
         endif
      endif
 
      return
      end
 
      subroutine getnextilineseg(i1,i2,ndeg,isrc,ilinedgeptr,ilinedge
     &   ,ilinedgesrc,icomp,icurrcomp,cprtname,len_isrcnew,lspecial,i3)
 
c.... We seek a successor node I3 that follows I1 and I2.
c.... If there is not a unique successor node, then I2 is
c.... a special point.
 
      implicit none
 
      integer i1,i2,ndeg,isrc(*),ilinedgeptr(*),ilinedge(*),i3,icomp(*)
     &   ,icurrcomp,ilinedgesrc(*)
 
      logical lspecial
 
c.... Pointered arrays passed in partition CPRTNAME but only
c.... used locally.
 
      pointer (ipisrcnew,isrcnew)
      integer isrcnew(*),len_isrcnew
      save ipisrcnew
 
      integer ndata,idata(0:2),k,iloc(2),ibegin,iend,ndegnew,icscode
 
      logical lsame
 
      character*32 cprtname
 
      lspecial=.false.
 
      ndata=0
      idata(0)=0
      do k=ilinedgeptr(i2),ilinedgeptr(i2+1)-1
         if (ilinedge(k).ne.idata(ndata)) then
            ndata=ndata+1
            if (ndata.gt.2) goto 10
            idata(ndata)=ilinedge(k)
            iloc(ndata)=k
         endif
      enddo
 
 10   continue
 
c....If there is no unique successor, return.
      if (ndata.ne.2) then
         i3=0
         lspecial=.true.
         return
      endif
 
      if (idata(1).ne.i1) then
         i3=idata(1)
         ibegin=iloc(1)
         iend=iloc(2)-1
      else
         i3=idata(2)
         ibegin=iloc(2)
         iend=ilinedgeptr(i2+1)-1
      endif
 
c.... If the new segment already is tagged as having the current
c.... component, then the interline is a closed loop.  We
c.... return with I3=0 since we've already processed it before;
c.... we return with LSPECIAL=F, since I2 is not a special point.
      if (icomp(ibegin).eq.icurrcomp) then
         i3=0
         lspecial=.false.
         return
      endif
 
c.... If the new segment already is tagged as having a component
c.... unequal to the current component, I2 is a special point.
      if (icomp(ibegin).ne.0) then
         i3=0
         lspecial=.true.
         return
      endif
 
c.... If the set of incident surfaces for I1-I2 and I2-I3 differ,
c.... then I2 is a special point.
      ndegnew=1
      if (ndegnew.gt.len_isrcnew) call mm_ovall('isrcnew'
     &   ,cprtname,ipisrcnew,ndegnew,100,len_isrcnew,1
     &   ,icscode)
      isrcnew(ndegnew)=ilinedgesrc(ibegin)
      do k=ibegin+1,iend
         if (isrcnew(ndegnew).ne.ilinedgesrc(k)) then
            ndegnew=ndegnew+1
            if (ndegnew.gt.len_isrcnew) call mm_ovall('isrcnew'
     &         ,cprtname,ipisrcnew,ndegnew,100,len_isrcnew,1
     &         ,icscode)
            isrcnew(ndegnew)=ilinedgesrc(k)
         endif
      enddo
      if (ndeg.ne.ndegnew) then
         i3=0
         lspecial=.true.
         return
      endif
      lsame=.true.
      do k=1,ndeg
         if (isrc(k).ne.isrcnew(k)) lsame=.false.
      enddo
      if (.not.lsame) then
         i3=0
         lspecial=.true.
         return
      endif
 
c.... The incident surfaces are the same, and so I3 is the continuation
c.... of the interline.  We now brand I2-I3 and I3-I2 with ICURRCOMP and
c.... return I3 as the successor.
 
      do k=ibegin,iend
         icomp(k)=icurrcomp
      enddo
 
      do k=ilinedgeptr(i3),ilinedgeptr(i3+1)-1
         if (ilinedge(k).eq.i2) then
            icomp(k)=icurrcomp
         endif
      enddo
      lspecial=.false.
 
      return
      end
 
      subroutine createcmoline(cmoline,xic3d,yic3d,zic3d,itp3d,icr3d
     &   ,isn3d,iboundary,iboundaryptr,ifeature,ifeatureptr,ilcompstart
     &   ,isppcompstart,ncomp)
 
      implicit none
 
      include 'local_element.h'
      include 'chydro.h'
 
      real*8 xic3d(*),yic3d(*),zic3d(*)
      integer itp3d(*),icr3d(*),iboundary(*),iboundaryptr(*),ifeature(*)
     &   ,ifeatureptr(*),ilcompstart,isppcompstart,ncomp,isn3d(*)
 
      pointer (ipxic,xic)
      real*8 xic(*)
 
      pointer (ipyic,yic)
      real*8 yic(*)
 
      pointer (ipzic,zic)
      real*8 zic(*)
 
      pointer (ipimt1,imt1)
      integer imt1(*)
 
      pointer (ipitp1,itp1)
      integer itp1(*)
 
      pointer (ipicr1,icr1)
      integer icr1(*)
 
      pointer (ipisn1,isn1)
      integer isn1(*)
 
      pointer (ipitet,itet)
      integer itet(*)
 
      pointer (ipitetoff,itetoff)
      integer itetoff(*)
 
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
 
      pointer (ipitetclr,itetclr)
      integer itetclr(*)
 
      pointer (ipimap,imap)
      integer imap(*)
 
      character*132 cbuf
      character*32 cmo,cmoline
 
      integer ierror,iexisterror,nseg,nelements,nodbegin,nodend,nnodes
     &   ,icscode,i,nod,iparnod,j,inod,icolor
     &   ,nnodesbegin,length,icmotype,nspp,nnodes_apriori
     &   ,nelements_apriori
 
      integer ncolormax
      parameter (ncolormax=20)
      logical lusedcolor(ncolormax),lfoundcolor
 
      call cmo_get_name(cmo,ierror)
 
      call cmo_exist(cmoline,iexisterror)
      if (iexisterror.ne.0) then
         write(cbuf,*) 'cmo/create/',cmoline,'/// line ; finish'
         call dotaskx3d (cbuf,ierror)
         write(cbuf,*)
     &      'cmo/addatt//imap/VINT/scalar/nnodes//temporary/agx/0'//
     &      ' ; finish'
         call dotaskx3d (cbuf,ierror)
      else
         call cmo_set_name(cmoline,icscode)
      endif
 
c.... A priori computation of nnodes and nelements.
 
      nnodes=0
      nelements=0
      do i=ilcompstart,isppcompstart-1
         nseg=ifeatureptr(i+1)-1-ifeatureptr(i)
         nelements=nelements+nseg
         nodbegin=ifeature(ifeatureptr(i))
         nodend=ifeature(ifeatureptr(i+1)-1)
         if (nodbegin.ne.nodend) then
            nnodes=nnodes+nseg+1
         else
            nnodes=nnodes+nseg
         endif
      enddo
 
      nspp=ncomp-isppcompstart+1
      nnodes=nnodes+nspp
 
      call cmo_set_info('nnodes',cmoline,nnodes,1,1,icscode)
      call cmo_set_info('nelements',cmoline,nelements,1,1,icscode)
      nnodes_apriori=nnodes
      nelements_apriori=nelements
 
      call cmo_newlen(cmoline,icscode)
 
      call cmo_get_info('imt1',cmoline,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmoline,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmoline,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmoline,ipisn1,length,icmotype,ierror)
      call cmo_get_info('imap',cmoline,ipimap,length,icmotype,ierror)
      call cmo_get_info('xic',cmoline,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmoline,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmoline,ipzic,length,icmotype,ierror)
      call cmo_get_info('itet',cmoline,ipitet,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmoline,ipitetoff,length,icmotype
     &   ,ierror)
      call cmo_get_info('itettyp',cmoline,ipitettyp,length,icmotype
     &   ,ierror)
      call cmo_get_info('itetclr',cmoline,ipitetclr,length,icmotype
     &   ,ierror)
 
      nnodes=0
      nelements=0
 
c.... Each special point gets a parent node.
 
      do i=isppcompstart,ncomp
         iparnod=ifeature(ifeatureptr(i))
         nnodes=nnodes+1
         xic(nnodes)=xic3d(iparnod)
         yic(nnodes)=yic3d(iparnod)
         zic(nnodes)=zic3d(iparnod)
         isn1(nnodes)=nnodes
         imt1(nnodes)=ncolormax
         itp1(nnodes)=ifitpcup
         icr1(nnodes)=icr3d(iparnod)
         imap(nnodes)=iparnod
      enddo
 
c.... Loop thru interlines.
 
      do i=ilcompstart,isppcompstart-1
 
         if (iboundaryptr(i+1).gt.iboundaryptr(i)) then
 
c.... Line has a boundary and so add first line point to
c.... parent/child complex of first boundary point.
 
            do j=1,ncolormax-1
               lusedcolor(j)=.false.
            enddo
            do j=iboundaryptr(i),iboundaryptr(i+1)-1
               nod=iboundary(j)-isppcompstart+1
               iparnod=ifeature(ifeatureptr(iboundary(j)))
               inod=isn1(nod)
               do while (inod.ne.nod)
                  lusedcolor(imt1(inod))=.true.
                  inod=isn1(inod)
               enddo
            enddo
            icolor=1
            lfoundcolor=.false.
            do while (icolor.le.ncolormax-1.and.(.not.lfoundcolor))
               if (.not.lusedcolor(icolor)) then
                  lfoundcolor=.true.
               else
                  icolor=icolor+1
               endif
            enddo
            if (icolor.ge.ncolormax) then
               print*,'Error!  Ran out of colors for line cmo!'
               call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &            ,icscode)
               stop
            endif
 
            nod=iboundary(iboundaryptr(i))-isppcompstart+1
            inod=nod
            do while (isn1(inod).ne.nod)
               inod=isn1(inod)
            enddo
            iparnod=ifeature(ifeatureptr(i))
            nnodes=nnodes+1
            xic(nnodes)=xic3d(iparnod)
            yic(nnodes)=yic3d(iparnod)
            zic(nnodes)=zic3d(iparnod)
            if (itp3d(iparnod).eq.ifitpcup) then
               itp1(nnodes)=itp3d(isn3d(iparnod))
            else
               itp1(nnodes)=itp3d(iparnod)
            endif
            icr1(nnodes)=icr3d(iparnod)
            imt1(nnodes)=icolor
            imap(nnodes)=iparnod
            isn1(inod)=nnodes
            isn1(nnodes)=nod
            nnodesbegin=nnodes
         else
 
c.... Line has no boundary, so add non-parent point for first point
c.... of line.
 
            icolor=1
            nnodes=nnodes+1
            iparnod=ifeature(ifeatureptr(i)+1)
            xic(nnodes)=xic3d(iparnod)
            yic(nnodes)=yic3d(iparnod)
            zic(nnodes)=zic3d(iparnod)
            if (itp3d(iparnod).eq.ifitpcup) then
               itp1(nnodes)=itp3d(isn3d(iparnod))
            else
               itp1(nnodes)=itp3d(iparnod)
            endif
            icr1(nnodes)=icr3d(iparnod)
            imt1(nnodes)=icolor
            isn1(nnodes)=0
            imap(nnodes)=iparnod
            nnodesbegin=nnodes
         endif
 
c.... Loop thru all segments (except for the last one) and
c.... add element and non-parent point corresponding to the
c.... segment and the 'far' endpoint of the segment.
 
         do j=ifeatureptr(i),ifeatureptr(i+1)-3
            nelements=nelements+1
            itetclr(nelements)=icolor
            itetoff(nelements)=2*(nelements-1)
            itet(1+itetoff(nelements))=nnodes
            itet(2+itetoff(nelements))=nnodes+1
            itettyp(nelements)=ifelmlin
            nnodes=nnodes+1
            iparnod=ifeature(j+1)
            xic(nnodes)=xic3d(iparnod)
            yic(nnodes)=yic3d(iparnod)
            zic(nnodes)=zic3d(iparnod)
            if (itp3d(iparnod).eq.ifitpcup) then
               itp1(nnodes)=itp3d(isn3d(iparnod))
            else
               itp1(nnodes)=itp3d(iparnod)
            endif
            icr1(nnodes)=icr3d(iparnod)
            imt1(nnodes)=icolor
            isn1(nnodes)=0
            imap(nnodes)=iparnod
         enddo
 
c.... Add element corresponding to the last segment.
 
         nelements=nelements+1
         itetclr(nelements)=icolor
         itetoff(nelements)=2*(nelements-1)
         itettyp(nelements)=ifelmlin
         itet(1+itetoff(nelements))=nnodes
 
c.... If line has second boundary point (i.e. is open), add
c.... endpoint into parent/child complex of boundary point.
 
         if (ifeature(ifeatureptr(i+1)-1).ne
     &      .ifeature(ifeatureptr(i))) then
            itet(2+itetoff(nelements))=nnodes+1
            nod=iboundary(iboundaryptr(i)+1)-isppcompstart+1
            inod=nod
            do while (isn1(inod).ne.nod)
               inod=isn1(inod)
            enddo
            iparnod=ifeature(ifeatureptr(i+1)-1)
            nnodes=nnodes+1
            xic(nnodes)=xic3d(iparnod)
            yic(nnodes)=yic3d(iparnod)
            zic(nnodes)=zic3d(iparnod)
            if (itp3d(iparnod).eq.ifitpcup) then
               itp1(nnodes)=itp3d(isn3d(iparnod))
            else
               itp1(nnodes)=itp3d(iparnod)
            endif
            icr1(nnodes)=icr3d(iparnod)
            imt1(nnodes)=icolor
            imap(nnodes)=iparnod
            isn1(inod)=nnodes
            isn1(nnodes)=nod
         else
            itet(2+itetoff(nelements))=nnodesbegin
         endif
      enddo
 
      if (nnodes.ne.nnodes_apriori.or.nelements.ne.nelements_apriori)
     &   then
         print*,'CREATECMOLINE: Disparity in predicted/actual cmo size.'
         call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &      ,icscode)
         stop
      endif
 
c.... Leave CMOLINE as the Current Mesh Object.
c$$$      call cmo_set_name(cmo,icscode)
 
      return
      end
 
      function thetaest_lg(icmp,cprtname,xic,yic,zic,iboundaryptr
     &   ,iboundary,invboundaryptr,invboundary,invicompnod,ifeature
     &   ,ifeatureptr,invmpary,ipicompstack,cicompstack,len_icompstack
     &   ,ipicodimstack,cicodimstack,len_icodimstack,len_itarglist
     &   ,len_vecx,len_vecy,len_vecz,nef_cmo,itet,itetoff,itettyp
     &   ,iparent,mpary,icompnod,icodim)
 
c.... This function estimates the solid angle that component ICMP
c.... subtends with the component (whose nodes are in ICOMPNOD)
c.... at the 'origin'.
 
      implicit none
 
      include 'local_element.h'
 
      real*8 thetaest_lg,xic(*),yic(*),zic(*)
      integer icmp,iboundaryptr(*),iboundary(*),invboundaryptr(*)
     &   ,invboundary(*),invicompnod(*),ifeature(*),ifeatureptr(*)
     &   ,invmpary(*),iparent(*),itet(*),itetoff(*),itettyp(*),nef_cmo
     &   ,mpary(*),icompnod(*),icodim
      character*(*) cprtname
 
c.... Pointered arrays in partition CPRTNAME
 
      pointer (ipicompstack,icompstack)
      integer icompstack(*),len_icompstack
      character*(*) cicompstack
 
      pointer (ipicodimstack,icodimstack)
      integer icodimstack(*),len_icodimstack
      character*(*) cicodimstack
 
c.... Arrays on CPRTNAME but only used locally.
 
      pointer (ipitarglist,itarglist)
      integer itarglist(*),len_itarglist
      character*32 citarglist
      save ipitarglist,citarglist
 
      pointer (ipvecx,vecx)
      real*8 vecx(*)
      integer len_vecx
      save ipvecx
 
      pointer (ipvecy,vecy)
      real*8 vecy(*)
      integer len_vecy
      save ipvecy
 
      pointer (ipvecz,vecz)
      real*8 vecz(*)
      integer len_vecz
      save ipvecz
 
      real*8 pi
      parameter (pi=3.1415926535897932d0)
 
      real*8 eps
      parameter (eps=1.d-10)
 
      integer itop,i,ipoint0,ipoint1,iline,node,mpnode,nvec,itargcodim
     &   ,nitarglist,icscode,icurr,nadjvol,j,k,jtetcurr,ihyb,ifac,ityp
     &   ,nodek,nincsurf
      real*8 vecavex,vecavey,vecavez,vnorm,cosave
 
      integer locverbosity
      parameter (locverbosity=1)
 
      data citarglist/'itarglist'/
 
      itop=1
      icompstack(1)=icmp
      icodimstack(1)=0
      itargcodim=2
      call gettargcodimcomps(itop,cicompstack,ipicompstack
     &   ,len_icompstack,cicodimstack,ipicodimstack,len_icodimstack
     &   ,itargcodim,cprtname,citarglist,ipitarglist
     &   ,len_itarglist,nitarglist,iboundaryptr,iboundary
     &   ,invboundaryptr,invboundary)
 
      nvec=0
      vecavex=0.d0
      vecavey=0.d0
      vecavez=0.d0
      if (locverbosity.ge.2) print*,'Volume:',icmp
      do 10 i=1,nitarglist
         ipoint0=0
         ipoint1=0
         iline=itarglist(i)
         node=ifeature(ifeatureptr(iline))
         mpnode=invmpary(node)
         if (mpnode.ne.0) then
            if (invicompnod(mpnode).ne.0) then
               ipoint0=node
               ipoint1=ifeature(ifeatureptr(iline)+1)
            endif
         endif
         node=ifeature(ifeatureptr(iline+1)-1)
         mpnode=invmpary(node)
         if (mpnode.ne.0) then
            if (invicompnod(mpnode).ne.0) then
               if (ipoint0.ne.0) goto 10
               ipoint0=node
               ipoint1=ifeature(ifeatureptr(iline+1)-2)
            endif
         endif
         if (ipoint0.eq.0) goto 10
         nvec=nvec+1
         if (nvec.gt.len_vecx) call mm_ovall('vecx',cprtname,
     &      ipvecx,nvec,100,len_vecx,2,icscode)
         if (nvec.gt.len_vecy) call mm_ovall('vecy',cprtname,
     &      ipvecy,nvec,100,len_vecy,2,icscode)
         if (nvec.gt.len_vecz) call mm_ovall('vecz',cprtname,
     &      ipvecz,nvec,100,len_vecz,2,icscode)
         vecx(nvec)=xic(ipoint1)-xic(ipoint0)
         vecy(nvec)=yic(ipoint1)-yic(ipoint0)
         vecz(nvec)=zic(ipoint1)-zic(ipoint0)
         vnorm=max(eps,sqrt(vecx(nvec)**2+vecy(nvec)**2+vecz(nvec)**2))
         vecx(nvec)=vecx(nvec)/vnorm
         vecy(nvec)=vecy(nvec)/vnorm
         vecz(nvec)=vecz(nvec)/vnorm
         vecavex=vecavex+vecx(nvec)
         vecavey=vecavey+vecy(nvec)
         vecavez=vecavez+vecz(nvec)
         if (locverbosity.ge.2) then
            print*,'vec',nvec,';ipoint0,ipoint1=',ipoint0,ipoint1
            print*,'xyz0',xic(ipoint0),yic(ipoint0),zic(ipoint0)
            print*,'xyz1',xic(ipoint1),yic(ipoint1),zic(ipoint1)
         endif
 10   continue
      if (nvec.eq.0) then
         if (locverbosity.ge.1) print*,'THETAEST_LG: NVEC=0 for ICMP='
     &      ,icmp
         thetaest_lg=4.0d0*pi
         return
      endif
      vnorm=max(eps,sqrt(vecavex**2+vecavey**2+vecavez**2))
      vecavex=vecavex/vnorm
      vecavey=vecavey/vnorm
      vecavez=vecavez/vnorm
      cosave=0.d0
      do i=1,nvec
         cosave=cosave+vecx(i)*vecavex+vecy(i)*vecavey+vecz(i)*vecavez
      enddo
      cosave=cosave/nvec
      thetaest_lg=2.d0*pi*(1.d0-cosave)
 
c.... If ICMP has a bounding surface that only bounds ICMP and no
c.... other volume, then ICMP is on the boundary.  If ICOMPNOD
c.... represents the nodes of a collapsing volume (ICODIM=0), then for each such
c.... bounding surface (which 'counts' by being incident on ICOMPNOD)
c.... we credit solid angle for ICMP by a very large amount (1.e4).
c.... This forces the component deemed to have 'largest solid angle'
c.... to be in the class of components with a maximal number
c.... of boundary touches.
 
      if (icodim.eq.0) then
 
         itop=1
         icompstack(1)=icmp
         icodimstack(1)=0
         itargcodim=1
         call gettargcodimcomps(itop,cicompstack,ipicompstack
     &      ,len_icompstack,cicodimstack,ipicodimstack,len_icodimstack
     &      ,itargcodim,cprtname,citarglist,ipitarglist
     &      ,len_itarglist,nitarglist,iboundaryptr,iboundary
     &      ,invboundaryptr,invboundary)
 
         nincsurf=0
 
         do 20 i=1,nitarglist
            icurr=itarglist(i)
            nadjvol=invboundaryptr(icurr+1)-invboundaryptr(icurr)
            if (nadjvol.eq.1) then
 
c.... Component ICURR is on the boundary, but we only count it
c.... for purposes of solid angle increase if some node in ICURR
c.... is also in ICOMPNOD.  Otherwise, ICURR is an irrelevant
c.... surface that is away from the action.
 
               do j=ifeatureptr(icurr),ifeatureptr(icurr+1)-1
                  jtetcurr=ifeature(j)
                  ihyb=1+(jtetcurr-1)/nef_cmo
                  ifac=jtetcurr-nef_cmo*(ihyb-1)
                  ityp=itettyp(ihyb)
                  do k=1,ielmface0(ifac,ityp)
                     nodek=itet(itetoff(ihyb)+ielmface1(k,ifac,ityp))
                     mpnode=invmpary(iparent(nodek))
                     if (mpnode.ne.0) then
                        if (invicompnod(mpnode).ne.0) then
                           nincsurf=nincsurf+1
                           goto 20
                        endif
                     endif
                  enddo
               enddo
            endif
 20      continue
 
c$$$      if (nincsurf.gt.0) then
c$$$         thetaest_lg=thetaest_lg*(nincsurf+1)
c$$$         print*,'boundary volume ',icmp
c$$$     &      ,' had solid angle increased to ',thetaest_lg
c$$$     &      ,' due to surfaces near node ',
c$$$     &      mpary(icompnod(1))
c$$$      endif
 
         if (nincsurf.gt.0) then
            thetaest_lg=thetaest_lg + 1.e4*nincsurf
            if (locverbosity.ge.2) print*,'boundary volume ',icmp
     &         ,' had solid angle increased to ',thetaest_lg
     &         ,' due to surfaces near node ',
     &         mpary(icompnod(1))
         endif
 
      endif
 
      return
      end
 
      function dmaxdepth_lg(icodim,nelts,ifeature,invifeature,invieltary
     &   ,itet,itetoff,xic,yic,zic,nnodes,mbndry,ipdir,len_dir,ipdepth
     &   ,len_depth,itettyp,jtet,jtetoff,ipieltstack,len_ieltstack,md
     &   ,ikey,iedges,iedges_first,iedgisurfelt,nef_cmo,iparent,cprtname
     &   )
 
      implicit none
 
      include 'consts.h'
      include 'local_element.h'
 
      real*8 xic(*),yic(*),zic(*)
      integer md,itettyp(*),jtet(*),jtetoff(*),ikey(md,*),iedges(*)
     &   ,iedges_first(*),iedgisurfelt(*),nef_cmo,iparent(*),icodim
     &   ,ifeature(*),invifeature(*),invieltary(*),itet(*),itetoff(*)
     &   ,mbndry,nelts
 
      pointer (ipdir,dir)
      real*8 dir(*)
      integer len_dir
 
      pointer (ipieltstack,ieltstack)
      integer ieltstack(*),len_ieltstack
 
      pointer (ipdepth,depth)
      real*8 depth(*)
      integer len_depth
 
      integer itemp(10),icscode,i,ihyb,i1,i2,i3,i4,ielt,ityp,j,jtetj
     &   ,itop,icurr,j1,jtetj1,ieltopp,iopp,jteti,ifac,next,isurfeltcurr
     &   ,iedg,nod1,nod2,maxpar,minpar,numedges,ipos,nnodes,k,jbegin
     &   ,jend,jtetcurr,k1
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,depthposs,dmaxdepth_lg
      character*32 cprtname
 
      integer locverbosity
      parameter (locverbosity=1)
 
      include 'statementfunctions.h'
 
      if (icodim.eq.0) then
 
c.... Volume component case.
 
c.... Loop over elements in component and compute inscribed
c.... radii of elements.  Also define inverse mapping INVIFEATURE,
c.... and initialize depth of all elements to infinity.
 
         if (nelts.gt.len_dir) call mm_ovall('dir',cprtname,
     &      ipdir,nelts,100,len_dir,2,icscode)
         if (nelts.gt.len_depth) call mm_ovall('depth',cprtname,
     &      ipdepth,nelts,100,len_depth,2,icscode)
 
         do i=1,nelts
            ihyb=ifeature(i)
            invifeature(invieltary(ihyb))=i
            i1=itet(itetoff(ihyb)+1)
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            i2=itet(itetoff(ihyb)+2)
            x2=xic(i2)
            y2=yic(i2)
            z2=zic(i2)
            i3=itet(itetoff(ihyb)+3)
            x3=xic(i3)
            y3=yic(i3)
            z3=zic(i3)
            i4=itet(itetoff(ihyb)+4)
            x4=xic(i4)
            y4=yic(i4)
            z4=zic(i4)
            dir(i)=dirtet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
            depth(i)=1.d99
         enddo
 
c.... Loop over elements and then loop over faces of elements, looking
c.... for a boundary face.
 
         do 10 i=1,nelts
            ielt=ifeature(i)
            ityp=itettyp(ielt)
            do j=1,nelmnef(ityp)
               jtetj=jtet(jtetoff(ielt)+j)
               if (jtetj.ge.mbndry) then
 
c.... We've found a boundary face.  So set the depth of the associated
c.... element to the inscribed radius of the element.  Start an
c.... element stack with this element.
 
                  depth(i)=dir(i)
                  itop=1
 
                  if (itop.gt.len_ieltstack) call mm_ovall('ieltstack'
     &               ,cprtname,ipieltstack,itop,1000,len_ieltstack,1
     &               ,icscode)
 
                  ieltstack(itop)=i
 
c.... While the stack is nonempty, pop the next element off the stack.
 
                  do while (itop.gt.0)
                     icurr=ieltstack(itop)
                     ielt=ifeature(icurr)
                     ityp=itettyp(ielt)
                     itop=itop-1
 
c.... Loop over the faces of the current element and see if there is
c.... an element on the other side of the face.
 
                     do j1=1,nelmnef(ityp)
                        jtetj1=jtet(jtetoff(ielt)+j1)
                        if (jtetj1.lt.mbndry) then
                           ieltopp=1+(jtetj1-1)/nef_cmo
                           iopp=invifeature(invieltary(ieltopp))
                           if (iopp.eq.0) then
                              print*,'DMAXDEPTH_LG:  ERROR:  '
     &                           ,'Element not in component!'
                              call dotaskx3d
     &                           ('dump/lagrit/lgdebug ; finish',icscode
     &                           )
                              stop
                           endif
 
c.... Check if traversing from the current element to the new element
c.... results in a shallower depth for the new element.  If so,
c.... register this new shallower depth in the DEPTH array and
c.... put the new element on the element stack.
 
                           depthposs=depth(icurr)+dir(icurr)+dir(iopp)
                           if (depthposs.lt.depth(iopp)) then
                              depth(iopp)=depthposs
                              itop=itop+1
                              if (itop.gt.len_ieltstack) call mm_ovall
     &                           ('ieltstack',cprtname,ipieltstack,itop
     &                           ,1000,len_ieltstack,1,icscode)
                              ieltstack(itop)=iopp
                           endif
                        endif
                     enddo
                  enddo
                  goto 10
               endif
            enddo
 10      continue
 
         dmaxdepth_lg=0.
         do i=1,nelts
            dmaxdepth_lg=max(dmaxdepth_lg,depth(i))
         enddo
 
c.... Clear INVIFEATURE.
 
         do i=1,nelts
            ihyb=ifeature(i)
            invifeature(invieltary(ihyb))=0
         enddo
 
      elseif (icodim.eq.1) then
 
         if (nelts.gt.len_dir) call mm_ovall('dir',cprtname,
     &      ipdir,nelts,100,len_dir,2,icscode)
         if (nelts.gt.len_depth) call mm_ovall('depth',cprtname,
     &      ipdepth,nelts,100,len_depth,2,icscode)
 
         do i=1,nelts
            jteti=ifeature(i)
            ihyb=1+(jteti-1)/nef_cmo
            ifac=jteti-nef_cmo*(ihyb-1)
            ityp=itettyp(ihyb)
            i1=itet(itetoff(ihyb)+ielmface1(1,ifac,ityp))
            i2=itet(itetoff(ihyb)+ielmface1(2,ifac,ityp))
            i3=itet(itetoff(ihyb)+ielmface1(3,ifac,ityp))
            x1=xic(i1)
            y1=yic(i1)
            z1=zic(i1)
            x2=xic(i2)
            y2=yic(i2)
            z2=zic(i2)
            x3=xic(i3)
            y3=yic(i3)
            z3=zic(i3)
            dir(i)=dirtri(x1,y1,z1,x2,y2,z2,x3,y3,z3)
            depth(i)=1.d99
         enddo
 
c.... Compile set of edges bounding triangles in IEDG and
c.... then edge-triangle relation in IEDGISURFELT which will allow us
c.... to move from triangle to triangle.
 
         next=1
 
         do i=1,nelts
            isurfeltcurr=ifeature(i)
            ihyb=1+(isurfeltcurr-1)/nef_cmo
            ifac=isurfeltcurr-(ihyb-1)*nef_cmo
            ityp=itettyp(ihyb)
            do j1=1,ielmface0(ifac,ityp)
               iedg=ielmface2(j1,ifac,ityp)
               nod1=iparent(itet(itetoff(ihyb)+ielmedge1(1,iedg,ityp)))
               nod2=iparent(itet(itetoff(ihyb)+ielmedge1(2,iedg,ityp)))
               maxpar=max(nod1,nod2)
               minpar=min(nod1,nod2)
 
               ikey(1,next)=minpar
               ikey(2,next)=maxpar
               ikey(3,next)=i
               next=next+1
 
            enddo
         enddo
         numedges=next-1
 
         call hpsortim(numedges,3,md,itemp,ikey)
 
         ipos=1
         do i=1,nnodes
            iedges_first(i)=ipos
            do while (ipos.le.numedges.and.ikey(1,ipos).eq.i)
               iedges(ipos)=ikey(2,ipos)
               iedgisurfelt(ipos)=ikey(3,ipos)
               ipos=ipos+1
            enddo
         enddo
         iedges_first(nnodes+1)=ipos
 
c.... Loop over triangles and then loop over edges of triangles, looking
c.... for an interline edge.
 
         do 20 i=1,nelts
            jteti=ifeature(i)
            ihyb=1+(jteti-1)/nef_cmo
            ifac=jteti-nef_cmo*(ihyb-1)
            ityp=itettyp(ihyb)
 
            do k=1,ielmface0(ifac,ityp)
               iedg=ielmface2(k,ifac,ityp)
               nod1=iparent(itet(ielmedge1(1,iedg,ityp)+itetoff(ihyb)))
               nod2=iparent(itet(ielmedge1(2,iedg,ityp)+itetoff(ihyb)))
               maxpar=max(nod1,nod2)
               minpar=min(nod1,nod2)
 
               jbegin=0
               jend=0
               do j=iedges_first(minpar),iedges_first(minpar+1)-1
                  if (iedges(j).eq.maxpar) then
                     jend=j
                     if (jbegin.eq.0) jbegin=j
                  endif
               enddo
               if (jbegin.eq.0) then
                  print*,'Warning! Can''t find surface edge !'
                  call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &               ,icscode)
                  stop
               endif
               if (jbegin+1.lt.jend) then
                  if (locverbosity.ge.1) print*,'Warning! Deg. ',jend
     &               -jbegin+1,' edge in DMAXDEPTH_LG'
               endif
               if (jbegin+1.ne.jend) then
 
c.... We've found an interline edge.  So set the depth of the associated
c.... triangle to the inscribed radius of the triangle.  Start an
c.... element stack with this triangle.
 
                  depth(i)=dir(i)
                  itop=1
                  if (itop.gt.len_ieltstack) call mm_ovall('ieltstack'
     &               ,cprtname,ipieltstack,itop,1000,len_ieltstack,1
     &               ,icscode)
                  ieltstack(itop)=i
 
c.... While the stack is nonempty, pop the next triangle off the stack.
 
                  do while (itop.gt.0)
                     icurr=ieltstack(itop)
                     jtetcurr=ifeature(icurr)
                     itop=itop-1
                     ihyb=1+(jtetcurr-1)/nef_cmo
                     ifac=jtetcurr-nef_cmo*(ihyb-1)
                     ityp=itettyp(ihyb)
 
c.... Loop over the faces of the current triangle and see if there is
c.... a triangle on the other side of the face.
 
                     do k1=1,ielmface0(ifac,ityp)
                        iedg=ielmface2(k1,ifac,ityp)
                        nod1=iparent(itet(ielmedge1(1,iedg,ityp)
     &                     +itetoff(ihyb)))
                        nod2=iparent(itet(ielmedge1(2,iedg,ityp)
     &                     +itetoff(ihyb)))
                        maxpar=max(nod1,nod2)
                        minpar=min(nod1,nod2)
 
                        jbegin=0
                        jend=0
                        do j=iedges_first(minpar),iedges_first(minpar+1)
     &                     -1
                           if (iedges(j).eq.maxpar) then
                              jend=j
                              if (jbegin.eq.0) jbegin=j
                           endif
                        enddo
                        if (jbegin.eq.0) then
                           print*,'Warning! Can''t find surface edge !'
                           call dotaskx3d('dump/lagrit/lgdebug ; finish'
     &                        ,icscode)
                           stop
                        endif
 
c.... Comment out the following 'if' block if there are excessive
c.... warnings.
 
                        if (jbegin+1.lt.jend) then
                           if (locverbosity.ge.1) print*
     &                        ,'warning! degree ',jend-jbegin+1
     &                        ,' edge in DMAXDEPTH_LG'
                        endif
                        if (jbegin+1.eq.jend) then
                           if ((iedgisurfelt(jbegin).ne.icurr).and
     &                        .(iedgisurfelt(jend).ne.icurr)) then
                              print*,'Surf. elt not in edge!!'
                              call dotaskx3d
     &                           ('dump/lagrit/lgdebug ; finish',icscode
     &                           )
                              stop
                           else
                              iopp=iedgisurfelt(jbegin)
     &                           +iedgisurfelt(jend)-icurr
                           endif
 
c.... Check if traversing from the current triangle to the new triangle
c.... results in a shallower depth for the new triangle.  If so,
c.... register this new shallower depth in the DEPTH array and
c.... put the new triangle on the element stack.
 
                           depthposs=depth(icurr)+dir(icurr)+dir(iopp)
                           if (depthposs.lt.depth(iopp)) then
                              depth(iopp)=depthposs
                              itop=itop+1
                              if (itop.gt.len_ieltstack) call mm_ovall
     &                           ('ieltstack',cprtname,ipieltstack,itop
     &                           ,1000,len_ieltstack,1,icscode)
                              ieltstack(itop)=iopp
                           endif
                        endif
                     enddo
                  enddo
                  goto 20
               endif
            enddo
 20      continue
 
         dmaxdepth_lg=0.
         do i=1,nelts
            dmaxdepth_lg=max(dmaxdepth_lg,depth(i))
         enddo
 
      endif
 
      return
      end
 
 
