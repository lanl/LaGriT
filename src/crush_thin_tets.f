      subroutine crush_thin_tets(imsgin,xmsgin,cmsgin,msgtype,nwds
     &   ,ierror)
C#######################################################################
C     
C     PURPOSE -
C     
c     CRUSH_THIN_TETS loops thru tets of a volume mesh 
c     and looks for tets that, relative to the characteristic
c     length established by the normalized root mean square length
c     of the edges, are
c     thinner than TOLCRUSH.  In the case that 'thinness' is defined
c     as the minimum tet altitude, then this 'relative length' 
c     (which is 'min tet altitude' / sqrt('sum of squares of edge lengths'/9) )
c     is actually an aspect ratio, because it goes to zero iff
c     the tet is degenerate and has a maximal value of 1 for a regular
c     tet.  (Note: denominator is '9' not '6' so that overall expression
c     evaluates to '1' for a regular tetrahedron.)
c     However, given the relative length scale, we can measure thinness
c     not just of the altitudes, but of any distance measurement in the 
c     tet.  In particular there are four type of 'thin' situations 
c     possible for a bad tet and we take appropriate actions for each case:
c     
c     (1) If an element has relative edge length shorter than TOLCRUSH, it
c     is merged.
c     (2) If a normal point-to-edge projection has relative length < TOLCRUSH,
c     a node is added to the edge, so that a type (1) merge can take
c     place.
c     (3) If a normal point-to-face projection has relative length < TOLCRUSH,
c     a node is added to the face, so that a type (1) merge can take 
c     place.
c     (4) If a normal mutual diagonal-to-diagonal projection has
c     relative length
c     < TOLCRUSH, a node is added to one diagonal, so that a type (2)
c     situation is created (which leads to another refinement and a
c     type (1) merge).
c
c     Since there are situations when a type (1) action would be barely
c     rejected on tolerance, but the more complicated type (2) action
c     would be barely accepted on tolerance, we loosen the tolerance
c     slightly for type (1) actions relative to type (2) actions to
c     avoid this.
c     Similarly, there are situations when a type (2) action would be
c     barely rejected on tolerance, but the more complicated type (3) 
c     or type (4) actions would be barely accepted on tolerance.
c     To avoid this we loosen the tolerance of type (2) actions relative
c     to type (3) and type(4) actions.
c     Currently we are using these effective tolerances (on relative
c     length):
c        type 1 : 1.2*TOLCRUSH
c        type 2 : 1.1*TOLCRUSH
c        type 3 : TOLCRUSH
c        type 4 : TOLCRUSH
C     
C     FORMAT: CRUSH_THIN_TETS/MESH_OBJECT/[TOLCRUSH]/
c     [pset,get,psetname]
C     
C     INPUT ARGUMENTS -
C     
C     imsgin()  - Integer array of command input tokens
C     xmsgin()  - Real array of command input tokens
C     cmsgin()  - Character array of command input tokens
C     msgtype() - Integer array of command input token types
C     nwds      - Number of command input tokens
C     
C     OUTPUT ARGUMENTS -
C     
C     ierror - ERROR FLAG
C     
C     CHANGE HISTORY -
C
C        $Log: crush_thin_tets.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   14 Jun 2007 07:57:24   tam
CPVCS    Initial revision.
CPVCS    
CPVCS    
CPVCS    Initial Version from kuprat.
CPVCS    
C     Revision 1.15  2007/02/09 05:45:31  kuprat
C     made nnodes bump-up clearer
C
C     Revision 1.14  2007/01/17 06:16:18  kuprat
C     prefer merging away old point in 01
C
C     Revision 1.13  2007/01/16 06:00:40  kuprat
C     prefigure validity of merges by itp
C
C     Revision 1.12  2007/01/13 03:42:51  kuprat
C     latest version
C
C     Revision 1.11  2007/01/08 03:05:10  kuprat
C     Only emit pairs of merge requests
C
C     Revision 1.10  2007/01/05 20:11:00  kuprat
C     changed default tolcrush to 0.1
C
C     Revision 1.9  2007/01/05 05:25:09  kuprat
C     prefigure benefits of potential moves
C
C     Revision 1.8  2006/12/17 03:47:30  kuprat
C     corrected use of cmo_newlen
C
C     Revision 1.7  2006/12/12 01:37:29  kuprat
C     latest save
C
C     Revision 1.6  2006/12/10 04:54:35  kuprat
C     order tets by mmsar
C
C     Revision 1.5  2006/12/10 04:20:23  kuprat
C     latest undebugged version
C
C     Revision 1.4  2006/12/08 04:37:39  kuprat
C     save of unfinished revision
C
C     Revision 1.3  2006/11/22 08:45:19  kuprat
C     corrected more bugs
C     
C     Revision 1.2  2006/11/21 05:13:44  kuprat
C     corrected to refine_fix_faceadd
C     
C     Revision 1.1  2006/11/21 05:08:26  kuprat
C     Initial revision
C     
C     
C#######################################################################
C     
      implicit none

      include 'chydro.h'
      include 'local_element.h'

      integer nwds,ierror
      real*8 xmsgin(nwds)
      integer imsgin(nwds), msgtype(nwds)
      character*(*) cmsgin(nwds)
      character*32 cmo,isubname
      integer icharlnf
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      integer itet(*),itetoff(*)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(*),jtetoff(*)
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipicr1,icr1)
      integer icr1(*)
      pointer (ipisn1,isn1)
      integer isn1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipitetclr,itetclr)
      integer itetclr(*)
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      
      pointer (ipelts1,elts1)
      integer elts1(*)
      pointer (ipelts2,elts2)
      integer elts2(*)
      pointer (ipeltse1,eltse1)
      integer eltse1(*)
      pointer (ipedges1,edges1)
      integer edges1(*)
      pointer (ipeltse2,eltse2)
      integer eltse2(*)
      pointer (ipedges2,edges2)
      integer edges2(*)

      integer nelements,i,node1,mbndry,j
      real*8 tolcrush
      integer imsginsave(20),msgtypesave(20)
      real*8 xmsginsave(20)
      character*32 cmsginsave(20)
      
      pointer (ipchanged,changed)
      logical changed(*)
      pointer (ipinvmpary,invmpary)
      integer invmpary(*)
      pointer (ipmpary,mpary)
      integer mpary(*)
      pointer (ipiparent,iparent)
      integer iparent(*)
      integer par(4)
      logical process
      pointer (ipiadd_bis,iadd_bis)
      integer iadd_bis(*)
      pointer (ipiadd_tr,iadd_tr)
      integer iadd_tr(*)
      pointer (ipieadd,ieadd)
      integer ieadd(*)
      pointer (ipifadd,ifadd)
      integer ifadd(*)
      pointer (ipnodeadd_tr,nodeadd_tr)
      integer nodeadd_tr(3,*)
      pointer (ipitadd_bis,itadd_bis)
      integer itadd_bis(*)
      pointer (ipitadd_tr,itadd_tr)
      integer itadd_tr(*)
      pointer (ipxadd_bis,xadd_bis)
      real*8 xadd_bis(*)
      pointer (ipyadd_bis,yadd_bis)
      real*8 yadd_bis(*)
      pointer (ipzadd_bis,zadd_bis)
      real*8 zadd_bis(*)
      pointer (ipxadd_tr,xadd_tr)
      real*8 xadd_tr(*)
      pointer (ipyadd_tr,yadd_tr)
      real*8 yadd_tr(*)
      pointer (ipzadd_tr,zadd_tr)
      real*8 zadd_tr(*)
      pointer (ipireal1,ireal1)
      integer ireal1(*)
      pointer (ipitpadd,itpadd)
      integer itpadd(*)
      pointer (ipicradd,icradd)
      integer icradd(*)
      pointer (ipimerge,imerge)
      integer imerge(2,*)
      pointer (ipiprm,iprm)
      integer iprm(*)
      pointer (iprelwidth,relwidth)
      real*8 relwidth(*)
      integer length,icmotype,ierrw,icscode
      logical did01,did02,did11,match,did00
      character*132 logmess,cbuff
      character*32 psetname
      integer nmrg,trip,maxtrip,nnodes
     &   ,mpno,nbis,ntrisect,ierrdum,mpno_old,k,nodeorig
     &   ,nod,flag,node,ict,ntrisect_used,nef,idies,ilives
     &   ,nnodes_actual
      real*8 scalelen
      parameter (maxtrip=10)
      integer len_changed,len_mpary,len_ireal1
     &   ,len_iparent,len_invmpary,len_xadd_bis,len_yadd_bis
     &   ,len_zadd_bis,len_ieadd,len_itadd_bis,len_iadd_bis,len_xadd_tr
     &   ,len_yadd_tr,len_zadd_tr,len_ifadd,len_itadd_tr
     &   ,len_iadd_tr,len_nodeadd_tr,len_itpadd,len_icradd,len_imerge
     &   ,len_iprm,len_relwidth
      logical itsttp
      real*8 relative_width,ascend,ssq,tolrelwidth_orig
      integer n00,n01,n02,n11,i1,i2,i3,nd(4),ii,minextra,idata,jj,
     &   ivoronoi_orig,itype
      character*32 cdata
      real*8 epsilonv_save,vtol

      pointer (ipout,out)
      real*8 rout,out(*)
      integer iout
      character*32 cout
C     
C########################################################################
C     ierror - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C     1 IF THERE IS AN ERROR)
      isubname = 'crush_thin_tets'

      n00=0
      n01=0
      n02=0
      n11=0

      ierror = 0
      
c     Initialize local array length counters to zero.
      len_changed=0
      len_mpary=0
      len_ireal1=0
      len_iparent=0
      len_invmpary=0
      len_xadd_bis=0
      len_yadd_bis=0
      len_zadd_bis=0
      len_ieadd=0
      len_itadd_bis=0
      len_iadd_bis=0
      len_xadd_tr=0
      len_yadd_tr=0
      len_zadd_tr=0
      len_ifadd=0
      len_itadd_tr=0
      len_nodeadd_tr=0
      len_itpadd=0
      len_icradd=0
      len_imerge=0
      len_iprm=0
      len_relwidth=0

c.... Get 3d mesh object
      cmo=cmsgin(2)(1:icharlnf(cmsgin(2)))

      if((cmo(1:5).eq.'-cmo-') .or.
     &   (cmo(1:5).eq.'-def-').or.
     &   nwds.lt.2) then
C     
C.... Use the Current Mesh Object.
C     
         call cmo_get_name(cmo,ierror)
      endif

c...  Select cmo
      call cmo_select(cmo,ierror)

      if (nwds.ge.3) then
         tolcrush=xmsgin(3)
      else
         tolcrush=0.1d0
      endif
      
c.... Save PSET info, since DOTASKX3D destroys it.
      
      if (nwds.ge.6) then
         do i=1,3
            imsginsave(i)=imsgin(i+3)
            xmsginsave(i)=xmsgin(i+3)
            cmsginsave(i)=cmsgin(i+3)
            msgtypesave(i)=msgtype(i+3)
         enddo
      else
         msgtypesave(1)=1
         imsginsave(1)=1
         imsginsave(2)=0
         imsginsave(3)=0
      endif
      
      call cmo_get_intinfo('faces_per_element',cmo,nef,
     &   length,icmotype,ierror)

c     Set epsilonv to very small, because this program
c     will guard against negative volumes under merges.
      call get_epsilon('epsilonv',epsilonv_save)
      vtol=epsilonv_save*1.d-6
      call cmo_set_attinfo('epsilonv',cmo,idata,vtol,cdata,2,icscode)

c     Main loop.  In each pass we possibly merge or refine mesh in
c     several (nonoverlapping) places to 'crush thin tets'.

      nmrg=-1
      nbis=-1
      ntrisect=-1
      trip=0

      call mmgetblk('elts1',isubname,ipelts1,1000,1,icscode)
      call mmgetblk('elts2',isubname,ipelts2,1000,1,icscode)
      call mmgetblk('eltse1',isubname,ipeltse1,1000,1,icscode)
      call mmgetblk('edges1',isubname,ipedges1,1000,1,icscode)
      call mmgetblk('eltse2',isubname,ipeltse2,1000,1,icscode)
      call mmgetblk('edges2',isubname,ipedges2,1000,1,icscode)

      call cmo_get_attinfo('tolrelwidth',cmo,iout,rout,cout,
     &   ipout,length,itype,ierror)
      tolrelwidth_orig=rout
      call cmo_get_intinfo('ivoronoi',cmo,ivoronoi_orig,length,icmotype
     &   ,ierror)

      write (cbuff,'(a,d16.8,a)') 'cmo/setatt//tolrelwidth//// ',
     &   tolcrush,' ; finish'
      call dotaskx3d(cbuff,ierror)

      write (cbuff,'(a)') 'cmo/setatt//ivoronoi////-4 ; finish'
      call dotaskx3d(cbuff,ierror)

      do while ((trip.lt.maxtrip).and.((nmrg.ne.0).or.
     &   (nbis.ne.0).or.(ntrisect.ne.0)))
         trip=trip+1

         write(cbuff,*) 'quality ; finish'
         call dotaskx3d(cbuff,ierror)

c... Do recon every trip.
         write(cbuff,*) 'recon ; finish'
C CWG         call dotaskx3d(cbuff,ierror)

c... Get some more space so we can name the nodes we are going to create.
         call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
         nnodes_actual=nnodes
         minextra=0.1d0*nnodes
         minextra=max(minextra,1000)
         nnodes=nnodes+minextra
         call cmo_set_info('nnodes',cmo,nnodes,1,1,ierror)
         call cmo_newlen(cmo,ierror)

         call cmo_get_info('nelements',cmo,nelements,length,icmotype
     &      ,ierror)
         call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror
     &      )
         call cmo_get_stdptrs(cmo,ipimt1,ipitp1,ipicr1,ipisn1,ipxic
     &      ,ipyic,ipzic,ipitetclr,ipitettyp,ipitetoff,ipjtetoff
     &      ,ipitet,ipjtet,ierror)

         if (nnodes.gt.len_mpary) call mm_ovall('mpary',isubname,
     &      ipmpary,nnodes,100,len_mpary,1,icscode)
         call getmpary(imsginsave,xmsginsave,cmsginsave,msgtypesave,
     &      ipmpary,mpno,psetname,ierror)
         
c     1) do we have a real point?
c     ireal1() =  0 ==> not a real point.
c     ireal1() =  1 ==> a real point.
c     
         if (nnodes.gt.len_ireal1) call mm_ovall('ireal1',isubname,
     &      ipireal1,nnodes,100,len_ireal1,1,icscode)
         
         call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum
     &      )
         if(ierrdum.ne.0) call x3d_error(isubname, 'unpacktp')
         
         if (nnodes.gt.len_changed) call mm_ovall('changed',isubname
     &      ,ipchanged,nnodes,100,len_changed,1,icscode)
         
         do i=1,nnodes
            changed(i)=.false.
         enddo
         
c     ..................................................................
c     find the parents of each node.
c     
         if (nnodes.gt.len_iparent) call mm_ovall('iparent',isubname,
     &      ipiparent,nnodes,100,len_iparent,1,icscode)
         
         call unpackpc(nnodes,itp1,isn1,iparent)
         
c.... change mass point array to contain all parent nodes
         
         if (nnodes.gt.len_invmpary) call mm_ovall('invmpary'
     &      ,isubname,ipinvmpary,nnodes,100,len_invmpary,1,icscode)
         
         do i=1,nnodes
            invmpary(i)=0
         enddo
         
         mpno_old=mpno
         mpno=0
         do k=1,mpno_old
            if (ireal1(mpary(k)).eq.1.or.
     &         itp1(mpary(k)).eq.ifitpcup) then
               nodeorig=mpary(k)
               nod=iparent(mpary(k))
               if (invmpary(nod).eq.0) then
                  mpno=mpno+1
                  mpary(mpno)=nod
                  invmpary(nod)=mpno
                  invmpary(nodeorig)=mpno
               endif
            endif
         enddo
         
         nbis=0
         ntrisect=0
         nmrg=0
c... Consider elements in order of increasing aspect ratio
         if (nelements.gt.len_iprm) call mm_ovall('iprm'
     &      ,isubname,ipiprm,nelements,100,len_iprm,1,icscode)
         if (nelements.gt.len_relwidth) call mm_ovall('relwidth'
     &      ,isubname,iprelwidth,nelements,100,len_relwidth,2,icscode)
         do i=1,nelements
            iprm(i)=i
            do j=1,nelmnen(ifelmtet)
               nd(j)=itet(itetoff(i)+j)
            enddo
            relwidth(i)=relative_width(xic(nd(1))
     &         ,yic(nd(1)),zic(nd(1)),xic(nd(2)),yic(nd(2))
     &         ,zic(nd(2)),xic(nd(3)),yic(nd(3)),zic(nd(3))
     &         ,xic(nd(4)),yic(nd(4)),zic(nd(4)))
         enddo
         ascend=1.0d0
         call hpsort1(nelements,relwidth,ascend,iprm)
         do 1000 ii=1,nelements
            i=iprm(ii)
c If aspect ratio exceeds 1.2*tolcrush (loose aspect ratio tolerance),
c then no more tets need be considered in this pass.
c$$$            if (relwidth(i).gt.1.2d0*tolcrush) goto 1001
            if (relwidth(i).gt.tolcrush) goto 1001
            if (ii.le.5) then
               print*,'processing tet ',i,'; relwidth=',relwidth(i)
            endif
c Get scale length for tet.
c Scale length is sqrt of (sum of squares of edge lengths/9)
            ssq=0.d0
            do k=1,nelmnee(ifelmtet)
               i1=ielmedge1(1,k,ifelmtet)
               i2=ielmedge1(2,k,ifelmtet)
               ssq=ssq+(xic(i1)-xic(i2))**2+(yic(i1)-yic(i2))**2
     &            +(zic(i1)-zic(i2))**2
            enddo
            scalelen=sqrt(ssq/9.d0)

            if (nbis+2.gt.len_xadd_bis) then 
               call mm_ovall('xadd_bis',isubname,ipxadd_bis,nbis+2
     &            ,100,len_xadd_bis,2,icscode)
               call mm_ovall('yadd_bis',isubname,ipyadd_bis,nbis+2
     &            ,100,len_yadd_bis,2,icscode)
               call mm_ovall('zadd_bis',isubname,ipzadd_bis,nbis+2
     &            ,100,len_zadd_bis,2,icscode)
               call mm_ovall('ieadd',isubname,ipieadd,nbis+2
     &            ,100,len_ieadd,1,icscode)
               call mm_ovall('itadd_bis',isubname,ipitadd_bis,nbis+2
     &            ,100,len_itadd_bis,1,icscode)
               call mm_ovall('iadd_bis',isubname,ipiadd_bis,nbis+2
     &            ,100,len_iadd_bis,1,icscode)
            endif
            if (ntrisect+1.gt.len_xadd_tr) then 
               call mm_ovall('xadd_tr',isubname,ipxadd_tr,ntrisect+1
     &            ,100,len_xadd_tr,2,icscode)
               call mm_ovall('yadd_tr',isubname,ipyadd_tr,ntrisect+1
     &            ,100,len_yadd_tr,2,icscode)
               call mm_ovall('zadd_tr',isubname,ipzadd_tr,ntrisect+1
     &            ,100,len_zadd_tr,2,icscode)
               call mm_ovall('ifadd',isubname,ipifadd,ntrisect+1
     &            ,100,len_ifadd,1,icscode)
               call mm_ovall('itadd_tr',isubname,ipitadd_tr,ntrisect
     &            +1,100,len_itadd_tr,1,icscode)
               call mm_ovall('iadd_tr',isubname,ipiadd_tr,ntrisect+1
     &            ,100,len_iadd_tr,1,icscode)
            endif
            if (3*(ntrisect+1).gt.len_nodeadd_tr) call mm_ovall
     &         ('nodeadd_tr',isubname,ipnodeadd_tr,3*(ntrisect+1),300
     &         ,len_nodeadd_tr,1,icscode)
            if (2*(nmrg+2).gt.len_imerge) call mm_ovall
     &         ('imerge',isubname,ipimerge,2*(nmrg+2),200
     &         ,len_imerge,1,icscode)

c...  We process this element only if all 4 nodes are in pset
            process=.true.
            do j=1,4
               par(j)=iparent(itet(itetoff(i)+j))
c$$$               process=process.and.(invmpary(par(j)).ne.0)
            enddo
c...  We process this element only if no node is `changed'
            do j=1,4
               process=process.and.(.not.changed(par(j)))
            enddo

            if (.not.process) then
               goto 1000
            endif

c     Check if this element has any edge shorter than tolcrush.
c     Merging this edge would be a '00': a projection from a zero
c     dimensional entity
c     (a point) onto another zero dimensional entity.  If this
c     possibility is detected, do nothing here.  (We anticipate that
c     this edge will be merged later by agd3d.)

            call find_00(tolcrush,scalelen,i,itet,itetoff,jtet,jtetoff
     &         ,itp1,itettyp,iparent,xic,yic,zic,nmrg,imerge
     &         ,ipelts1,ipelts2,nef,mbndry,changed,did00)

            if (did00) then
               goto 1000
            endif

c     Check if we can project a point onto an edge with distance less
c     than tolcrush.  If so, schedule a bisection of the edge (at the
c     proj.
c     point).  This is a '01': a projection from a zero dimensional
c     entity
c     (a point) onto a one dimensional entity (edge).  
            
            call find_01(tolcrush,scalelen,i,ipelts1,ipeltse1,ipedges1
     &         ,ipitetoff,ipjtetoff,ipitet,ipjtet,itp1,ipitettyp
     &         ,ipiparent,nef,mbndry,xic,yic,zic,nbis,xadd_bis,yadd_bis
     &         ,zadd_bis,ieadd,itadd_bis,iadd_bis,imerge,nmrg
     &         ,nnodes_actual,changed,did01)
            
            if (did01) then
               goto 1000
            endif

c     Check if we can project a point onto a face with distance less
c     than
c     tolcrush.  If so, schedule a trisection of the face (at the proj.
c     point).  
c     This is a '02': a projection from a zero dimensional entity
c     (a point) onto a two dimensional entity (face).  
            
            if (.false.) then
            call find_02(tolcrush,scalelen,i,itet,itetoff,xic,yic
     &         ,zic,ntrisect,xadd_tr,yadd_tr,zadd_tr,ifadd
     &         ,itadd_tr,did02)
            
            if (did02) then
               n02=n02+1
               i1=iparent(itet(itetoff(i)+ielmface1(1,ifadd(ntrisect)
     &            ,itettyp(i))))
               i2=iparent(itet(itetoff(i)+ielmface1(2,ifadd(ntrisect)
     &            ,itettyp(i))))
               i3=iparent(itet(itetoff(i)+ielmface1(3,ifadd(ntrisect)
     &            ,itettyp(i))))
               nnodes_actual=nnodes_actual+1
               idies=nnodes_actual
               ilives=itet(itetoff(i)+ifadd(ntrisect))
               iadd_tr(ntrisect)=-nnodes_actual
               do j=1,ielmface0(ifadd(ntrisect),ifelmtet)
                  nodeadd_tr(j,ntrisect)=iparent(itet(itetoff(i)
     &               +ielmface1(j,ifadd(ntrisect),ifelmtet)))
               enddo
               call mark_facenbd(changed,nef,itet,itetoff,jtet
     &            ,jtetoff,itettyp,iparent,mbndry,i,ifadd(ntrisect))
c We'll now schedule a merge
               nmrg=nmrg+1
               imerge(1,nmrg)=ilives
               imerge(2,nmrg)=idies
c We'll now schedule a merge
               nmrg=nmrg+1
               imerge(1,nmrg)=idies
               imerge(2,nmrg)=ilives
               goto 1000
            endif
            endif

c     Check if we can project a tet edge normally onto a diagonally
c     opposite tet edge with distance less than
c     tolcrush.  If so, schedule a bisection of one of the edges (at the
c     projection point).
c     This is a '11': a projection from a one dimensional entity
c     (an edge) onto another one dimensional entity.  
            
            call find_11(tolcrush,scalelen,i,ipeltse1,ipedges1,ipeltse2
     &         ,ipedges2,ipitetoff,ipjtetoff,ipitet,ipjtet,itp1
     &         ,ipitettyp,ipiparent,nef,mbndry,xic,yic,zic,nbis,xadd_bis
     &         ,yadd_bis,zadd_bis,ieadd,itadd_bis,iadd_bis,imerge,nmrg
     &         ,nnodes_actual,changed,did11)
            
            if (did11) then
               goto 1000
            endif
 1000    enddo 

 1001    continue
c... We increased nnodes so that we could specifically name the 
c... created nodes (so that we could include them in merge lists
c... if necessary).  Now trim the length of the cmo to just 
c... barely include the new named points.  (Any additional child
c... points colocated at the named points will have space made
c... for them in the mesh object by settets (which is called
c... by refine_edge_add_tet).
         if (nnodes.ne.nnodes_actual) then
            nnodes=nnodes_actual
            call cmo_set_info('nnodes',cmo,nnodes,1,1,ierror)
            call cmo_newlen(cmo,icscode)
         endif
         if (nbis.gt.0) then 
            if (nbis.gt.len_itpadd) call mm_ovall('itpadd',isubname
     &         ,ipitpadd,nbis,100,len_itpadd,1,icscode)
            if (nbis.gt.len_icradd) call mm_ovall('icradd',isubname
     &         ,ipicradd,nbis,100,len_icradd,1,icscode)
            call refine_fix_add(cmo,nbis,ipitadd_bis,ipieadd
     &         ,ipiadd_bis,ipitpadd,ipicradd)
c$$$            print*,'calling refine_edge_add_tet with nbis=',nbis
            call refine_edge_add_tet(cmo,nbis,ipitadd_bis,ipieadd
     &         ,iadd_bis,xadd_bis,yadd_bis,zadd_bis,flag)
            call cmo_newlen(cmo,icscode)
c.... Fix up ITP1, ICR1.
            
            call cmo_get_info('nnodes',cmo,
     *         nnodes,length,icmotype,ierror)
            call cmo_get_info('itp1',cmo,
     *         ipitp1,length,icmotype,ierror)
            call cmo_get_info('icr1',cmo,
     *         ipicr1,length,icmotype,ierror)
            call cmo_get_info('isn1',cmo,
     *         ipisn1,length,icmotype,ierror)
            do i = 1,nbis
               if (iadd_bis(i).gt.0) then
                  node=iadd_bis(i)
                  if (isn1(node).eq.0) then
                     itp1(node)=itpadd(i)
                     icr1(node)=icradd(i)
                  else
                     icr1(node)=icradd(i)
                     if (itp1(node).ne.ifitpcup) then
                        itp1(node)=itpadd(i)
                     endif
c     
c     check if node type is not interface then reset isn1
c     
                     if(itsttp('intrface',itpadd(i))) then
                        node1=isn1(node)
                        ict=0
                        do while (node1.ne.node)
                           icr1(node1)=icradd(i)
                           if (itp1(node1).ne.ifitpcup) then
                              itp1(node1)=itpadd(i)
                           endif
                           node1=isn1(node1)
                           ict=ict+1
                           if(ict.gt.10000) then
                              write(logmess,'
     &                           ("Bad parent/child chain "
     &                           ,"in crush_thin_tets (bisect)")')
                              call writloga('default',0,logmess,0
     &                           ,icscode)
                              isn1(node)=0
                              node1=node
                           endif
                        enddo
                     else
                        itp1(node)=itpadd(i)
                        icr1(node)=icradd(i)
                        isn1(node)=0
                     endif
                  endif
               endif
            enddo
         endif

         if (ntrisect.gt.0) then
c...  Check these are really the right faces, because edge bisection
c     could have in principle changed tet ordering.
            ntrisect_used=0
            do j=1,ntrisect
               match=.true.
               do k=1,ielmface0(ifadd(j),ifelmtet)
                  if (nodeadd_tr(k,j).ne
     &               .iparent(itet(itetoff(itadd_tr(j))+ielmface1(k
     &               ,ifadd(j),ifelmtet)))) match=.false. 
               enddo
               if (match) then
                  ntrisect_used=ntrisect_used+1
                  itadd_tr(ntrisect_used)=itadd_tr(j)
                  ifadd(ntrisect_used)=ifadd(j)
                  iadd_tr(ntrisect_used)=iadd_tr(j)
                  xadd_tr(ntrisect_used)=xadd_tr(j)
                  yadd_tr(ntrisect_used)=yadd_tr(j)
                  zadd_tr(ntrisect_used)=zadd_tr(j)
               else
                  print*,'Face on tet ',itadd_tr(j), 'not refined.'
               endif
            enddo
            if (ntrisect_used.gt.0) then 
               if (ntrisect_used.gt.len_itpadd) call mm_ovall
     &            ('itpadd',isubname,ipitpadd,ntrisect_used,100
     &            ,len_itpadd,1,icscode)
               if (ntrisect_used.gt.len_icradd) call mm_ovall
     &            ('icradd',isubname,ipicradd,ntrisect_used,100
     &            ,len_icradd,1,icscode)
C CWG               call refine_fix_faceadd(cmo,ntrisect_used,ipitadd_tr
C CWG     &            ,ipifadd,ipiadd_tr,ipitpadd,ipicradd)
               print *, 'Commented out call refine_fix_faceadd'
               print *, 'Should never get here.'
               call refine_face_add_tet(cmo,ntrisect_used,ipitadd_tr
     &            ,ipifadd,ipiadd_tr,xadd_tr,yadd_tr,zadd_tr)
               call cmo_newlen(cmo,icscode)
c.... Fix up ITP1, ICR1.
               
               call cmo_get_info('nnodes',cmo,
     *            nnodes,length,icmotype,ierror)
               call cmo_get_info('itp1',cmo,
     *            ipitp1,length,icmotype,ierror)
               call cmo_get_info('icr1',cmo,
     *            ipicr1,length,icmotype,ierror)
               call cmo_get_info('isn1',cmo,
     *            ipisn1,length,icmotype,ierror)
               do i = 1,ntrisect_used
                  if (iadd_tr(i).gt.0) then
                     node=iadd_tr(i)
                     if (isn1(node).eq.0) then
                        itp1(node)=itpadd(i)
                        icr1(node)=icradd(i)
                     else
                        icr1(node)=icradd(i)
                        if (itp1(node).ne.ifitpcup) then
                           itp1(node)=itpadd(i)
                        endif
c     
c     check if node type is not interface then reset isn1
c     
                        if(itsttp('intrface',itpadd(i))) then
                           node1=isn1(node)
                           ict=0
                           do while (node1.ne.node)
                              icr1(node1)=icradd(i)
                              if (itp1(node1).ne.ifitpcup) then
                                 itp1(node1)=itpadd(i)
                              endif
                              node1=isn1(node1)
                              ict=ict+1
                              if(ict.gt.10000) then
                                 write(logmess,'
     &                              ("Bad parent/child chain "
     &                              ,"in crush_thin_tets (trisect)")'
     &                              )
                                 call writloga('default',0,logmess,0
     &                              ,icscode)
                                 isn1(node)=0
                                 node1=node
                              endif
                           enddo
                        else
                           itp1(node)=itpadd(i)
                           icr1(node)=icradd(i)
                           isn1(node)=0
                        endif
                     endif
                  endif
               enddo
            endif
         endif
         if (nmrg.gt.0) then
            write(logmess,'(a,i8,a)') 'Merging ',nmrg,' nodes.'
            call writloga('default',0,logmess,0,ierrw)
c$$$            print*,'Merge list has ',nmrg,' elements:'
c$$$            do j=1,nmrg
c$$$               print*,imerge(1,j),imerge(2,j)
c$$$            enddo
            call mergepts_simplex(imerge,nmrg,cmo,icscode)
c$$$            call cmo_get_stdptrs(cmo,ipimt1,ipitp1,ipicr1,ipisn1,ipxic
c$$$     &         ,ipyic,ipzic,ipitetclr,ipitettyp,ipitetoff,ipjtetoff
c$$$     &         ,ipitet,ipjtet,ierror)
c$$$            call cmo_get_info('nelements',cmo,nelements,length,icmotype
c$$$     &         ,ierror)
c$$$            do 99 k=1,nelements
c$$$               do j=1,4
c$$$                  if (itp1(itet(itetoff(k)+j)).eq.ifitpmrg) then
c$$$                     print*,'Tet ',k,' has mrg type!'
c$$$                     print*,'Nodes:',(itet(itetoff(k)+jj),jj=1,4)
c$$$                     print*,'Types:',(itp1(itet(itetoff(k)+jj)),jj=1,4)
c$$$                  endif
c$$$               enddo
c$$$ 99         enddo
         endif
      enddo

      call mmrelprt(isubname,icscode)

c... restore original values of these variables...

      call cmo_set_attinfo('epsilonv',cmo,idata,epsilonv_save,cdata
     &   ,2,icscode)

      write (cbuff,'(a,d16.8,a)') 'cmo/setatt//tolrelwidth//// ',
     &   tolrelwidth_orig,' ; finish'
      call dotaskx3d(cbuff,ierror)

      write (cbuff,'(a,i4,a)') 'cmo/setatt//ivoronoi//// ',
     &   ivoronoi_orig,' ; finish'
      call dotaskx3d(cbuff,ierror)

      write(cbuff,*) 'quality ; finish'
      call dotaskx3d(cbuff,ierror)

      return
      end
 
      subroutine find_00(tolcrush,scalelen,it,itet,itetoff,jtet,jtetoff
     &   ,itp1,itettyp,iparent,xic,yic,zic,nmrg,imerge
     &   ,ipelts1,ipelts2,nef,mbndry,changed,did00)

      implicit none

      include 'local_element.h'

      real*8 tolcrush,scalelen,xic(*),yic(*),zic(*)
      logical changed(*)

      integer it,itet(*),itetoff(*),nmrg,imerge(2,*),itp1(*)
     &   ,itettyp(*),nef,mbndry,iparent(*),jtetoff(*)
     &   ,jtet(*)
      logical did00

      logical mergeable

      integer i1,i2,j,loc1,loc2,p1,p2,i,k
     &   ,jj,nold,nnew,oldlist(4,2000),newlist(4,2000)
      real*8 d,x0,y0
     &   ,z0,orig_minwidth,min_over_tetlist,mrg1_minwidth,mrg2_minwidth
      logical mrg1_feasible,mrg2_feasible

      pointer (ipelts1,elts1)
      integer elts1(*)
      pointer (ipelts2,elts2)
      integer elts2(*)

      did00=.false.
      do j=1,6
         loc1=ielmedge1(1,j,ifelmtet)
         loc2=ielmedge1(2,j,ifelmtet)
         i1=itet(itetoff(it)+loc1)
         i2=itet(itetoff(it)+loc2)
         p1=iparent(i1)
         p2=iparent(i2)
         d=sqrt((xic(i1)-xic(i2))**2+(yic(i1)-yic(i2))**2+(zic(i1)
     &      -zic(i2))**2)

c... We inflate tolerance here to avoid cases where barely avoiding a 
c... merge leads to insertion of a point close to a vertex or edge.
c$$$         if (d.lt.1.2d0*tolcrush*scalelen) then
         if (d.lt.tolcrush*scalelen) then
            call get_00_tets(it,j,nold,oldlist,nnew,newlist,ipelts1
     &         ,ipelts2,itet,itetoff,jtet,jtetoff,itettyp,iparent,nef
     &         ,mbndry)
            x0=0.d0
            y0=0.d0
            z0=0.d0
            orig_minwidth=min_over_tetlist(nold,oldlist,xic,yic,zic,x0
     &         ,y0,z0)
c... What happens when node 1 merges into node 2?
            if (mergeable(itp1(i2),itp1(i1))) then
               x0=xic(i2)
               y0=yic(i2)
               z0=zic(i2)
               mrg1_minwidth=min_over_tetlist(nnew,newlist,xic,yic,zic
     &            ,x0,y0,z0)
            else
               mrg1_minwidth=-1.d50
            endif
c... What happens when node 2 merges into node 1?
            if (mergeable(itp1(i1),itp1(i2))) then
               x0=xic(i1)
               y0=yic(i1)
               z0=zic(i1)
               mrg2_minwidth=min_over_tetlist(nnew,newlist,xic,yic,zic
     &            ,x0,y0,z0)
            else
               mrg2_minwidth=-1.d50
            endif
            
c.... Schedule best merge if it improves minwidth.
            if (mrg1_minwidth.gt.mrg2_minwidth) then
               if (mrg1_minwidth.gt.1.01d0*orig_minwidth) then
                  did00=.true.
                  nmrg=nmrg+1
                  imerge(1,nmrg)=i2
                  imerge(2,nmrg)=i1
               endif
            else                  
               if (mrg2_minwidth.gt.1.01d0*orig_minwidth) then
                  did00=.true.
                  nmrg=nmrg+1
                  imerge(1,nmrg)=i1
                  imerge(2,nmrg)=i2
               endif
            endif
            if (did00) then
c Mark nodes in old list
               do jj=1,nold
                  do k=1,4
                     changed(iparent(oldlist(k,jj)))=.true.
                  enddo
               enddo
               return
            endif
         endif
      enddo
           
      return
      end

      function relative_width(xica,yica,zica,xicb,yicb,zicb,xicc
     &   ,yicc,zicc,xicd,yicd,zicd)

      implicit none

      include 'local_element.h'

      real*8 relative_width,xica,yica,zica,xicb,yicb,zicb,xicc
     &   ,yicc,zicc,xicd,yicd,zicd
      real*8 xic(4),yic(4),zic(4),ssq,scalelen,salt,nx,ny,nz
     &   ,norm
      integer i,i1,i2,i3

      include 'consts.h'
      include 'statementfunctions.h'

      xic(1)=xica
      yic(1)=yica
      zic(1)=zica
      xic(2)=xicb
      yic(2)=yicb
      zic(2)=zicb
      xic(3)=xicc
      yic(3)=yicc
      zic(3)=zicc
      xic(4)=xicd
      yic(4)=yicd
      zic(4)=zicd
c Scale length is sqrt of (sum of squares of edge lengths/9)
      ssq=0.d0
      do i=1,nelmnee(ifelmtet)
         i1=ielmedge1(1,i,ifelmtet)
         i2=ielmedge1(2,i,ifelmtet)
         ssq=ssq+(xic(i1)-xic(i2))**2+(yic(i1)-yic(i2))**2
     &      +(zic(i1)-zic(i2))**2
      enddo
      scalelen=sqrt(ssq/9.d0)
c Min signed altitude
      salt=1.d50
      do i=1,nelmnef(ifelmtet)
         i1=ielmface1(1,i,ifelmtet)
         i2=ielmface1(2,i,ifelmtet)
         i3=ielmface1(3,i,ifelmtet)
         nx=-dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         ny=-dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         nz=-dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         norm=sqrt(nx**2+ny**2+nz**2)
         salt=min(salt,(nx*(xic(i)-xic(i1))+ny*(yic(i)-yic(i1))+nz
     &      *(zic(i)-zic(i1)))/safe(norm))
      enddo
      relative_width=salt/scalelen
      return
      end

      subroutine find_01(tolcrush,scalelen,it,ipelts1,ipeltse1,ipedges1
     &   ,ipitetoff,ipjtetoff,ipitet,ipjtet,itp1,ipitettyp,ipiparent,nef
     &   ,mbndry,xic,yic,zic,nadd,xadd,yadd,zadd,ieadd,itadd,iadd
     &   ,imerge,nmrg,nnodes_actual,changed,did01)

      implicit none

      include 'local_element.h'

      integer imerge(2,*),nmrg,itp1(*)
      real*8 tolcrush,scalelen,xadd(*),yadd(*),zadd(*),xic(*),yic(*)
     &   ,zic(*)

      pointer (ipelts1,elts1)
      integer elts1(*)

      pointer (ipeltse1,eltse1)
      integer eltse1(*)

      pointer (ipedges1,edges1)
      integer edges1(*)

      pointer (ipitetoff,itetoff)
      integer itetoff(*)
      
      pointer (ipjtetoff,jtetoff)
      integer jtetoff(*)

      pointer (ipitet,itet)
      integer itet(*)
      
      pointer (ipjtet,jtet)
      integer jtet(*)

      pointer (ipitettyp,itettyp)
      integer itettyp(*)

      pointer (ipiparent,iparent)
      integer iparent(*)

      integer it,nadd,ieadd(*),itadd(*),iadd(*)
      logical did01,changed(*)
      
      logical mergeable
      integer itp_result,itp12

      real*8 dotvw,lenw2,lambda,projx,projy,projz,distproj
     $   ,x0,y0,z0,orig_minwidth,min_over_tetlist
     &   ,mrg0_minwidth,mrgproj_minwidth
      integer k,i,fsum,j,jj,jedge,i1,i2,i0,nold,nnew
     &   ,oldlist(4,2000),newlist(4,2000),nef,mbndry,loc0,nnodes_actual
      logical mrg0_feasible,mrgproj_feasible

      include 'consts.h'
      include 'statementfunctions.h'

      did01=.false.
      do i=1,nelmnef(ifelmtet)
         fsum=ielmface1(1,i,ifelmtet)+ielmface1(2,i,ifelmtet)
     &      +ielmface1(3,i,ifelmtet)
         do 100 j=1,ielmface0(i,ifelmtet)
            jedge=ielmface2(j,i,ifelmtet)
            i1=itet(itetoff(it)+ielmedge1(1,jedge,ifelmtet))
            i2=itet(itetoff(it)+ielmedge1(2,jedge,ifelmtet))
            loc0=fsum-ielmedge1(1,jedge,ifelmtet)-ielmedge1(2,jedge
     &         ,ifelmtet)
            i0=itet(itetoff(it)+loc0)
c.... Project i0 onto i1-i2 and obtain lambda such that
c.... xic(proj(i0))=(1-lambda)*xic(i1)+lambda*xic(i2)
            dotvw=(xic(i0)-xic(i1))*(xic(i2)-xic(i1))+(yic(i0)-yic(i1))
     &         *(yic(i2)-yic(i1))+(zic(i0)-zic(i1))*(zic(i2)-zic(i1)) 
            lenw2=(xic(i2)-xic(i1))**2+(yic(i2)-yic(i1))**2+(zic(i2)
     &         -zic(i1))**2 
            lambda=dotvw/safe(lenw2)
c...  Projection must be in interior of segment
            if ((lambda.le.0.d0) .or. (lambda.ge.1.d0)) goto 100

            projx=(1.d0-lambda)*xic(i1)+lambda*xic(i2)
            projy=(1.d0-lambda)*yic(i1)+lambda*yic(i2)
            projz=(1.d0-lambda)*zic(i1)+lambda*zic(i2)

            distproj=sqrt((xic(i0)-projx)**2+(yic(i0)-projy)**2+(zic(i0)
     &         -projz)**2)
c...  Distance to projected point must be less than tolerance.  We
c...  change tolerance a bit to avoid case that projected point is too
c...  close to one of the endpoints of the edge.
c$$$            if (distproj.le.1.1d0*tolcrush*scalelen) then
            if (distproj.le.tolcrush*scalelen) then

               call get_01_tets(it,loc0,jedge,ipelts1,ipeltse1,ipedges1
     &            ,ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent
     &            ,nef,mbndry,nold,oldlist,nnew,newlist)
               x0=0.d0
               y0=0.d0
               z0=0.d0
               orig_minwidth=min_over_tetlist(nold,oldlist,xic,yic,zic
     &            ,x0,y0,z0)

               itp12=itp_result(itp1(i1),itp1(i2))
c...  What happens when node i0 merges into projection point?
               if (mergeable(itp12,itp1(i0))) then
                  x0=projx
                  y0=projy
                  z0=projz
                  mrg0_minwidth=min_over_tetlist(nnew,newlist,xic,yic
     &               ,zic,x0,y0,z0)
               else
                  mrg0_minwidth=-1.d50
               endif
c...  What happens when projection point merges into node i0
               if (mergeable(itp1(i0),itp12)) then
                  x0=xic(i0)
                  y0=yic(i0)
                  z0=zic(i0)
                  mrgproj_minwidth=min_over_tetlist(nnew,newlist,xic,yic
     &               ,zic,x0,y0,z0)
               else
                  mrgproj_minwidth=-1.d50
               endif
c.... We force the projected point to not be mergeable away.
c$$$               mrgproj_minwidth=-1.d50

c.... If it is possible to merge away i0 and improve the mesh, do that.
c.... Failing that, if it is possible to merge away the projected point
c.... (thus effectively performing a flip) and improve the mesh, do that. 

c$$$               if (mrg0_minwidth.gt.mrgproj_minwidth) then
                  if (mrg0_minwidth.gt.1.01d0*orig_minwidth) then
                     did01=.true.
                     nadd=nadd+1
                     xadd(nadd)=projx
                     yadd(nadd)=projy
                     zadd(nadd)=projz
                     ieadd(nadd)=jedge
                     itadd(nadd)=it
                     nnodes_actual=nnodes_actual+1
                     iadd(nadd)=-nnodes_actual
                     i1=iparent(itet(itetoff(it)+ielmedge1(1,ieadd(nadd)
     &                  ,itettyp(it))))
                     i2=iparent(itet(itetoff(it)+ielmedge1(2,ieadd(nadd)
     &                  ,itettyp(it))))
                     nmrg=nmrg+1
                     imerge(1,nmrg)=nnodes_actual
                     imerge(2,nmrg)=i0
c$$$                  endif
               else
                  if (mrgproj_minwidth.gt.1.01d0*orig_minwidth) then
                     did01=.true.
                     nadd=nadd+1
                     xadd(nadd)=projx
                     yadd(nadd)=projy
                     zadd(nadd)=projz
                     ieadd(nadd)=jedge
                     itadd(nadd)=it
                     nnodes_actual=nnodes_actual+1
                     iadd(nadd)=-nnodes_actual
                     i1=iparent(itet(itetoff(it)+ielmedge1(1,ieadd(nadd)
     &                  ,itettyp(it))))
                     i2=iparent(itet(itetoff(it)+ielmedge1(2,ieadd(nadd)
     &                  ,itettyp(it))))
                     nmrg=nmrg+1
                     imerge(1,nmrg)=i0
                     imerge(2,nmrg)=nnodes_actual
                  endif
               endif
               if (did01) then
c     Mark nodes in old list
                  do jj=1,nold
                     do k=1,4
                        changed(iparent(oldlist(k,jj)))=.true.
                     enddo
                  enddo
                  return
               endif
            endif
 100     enddo
      enddo
      return
      end

      subroutine find_02(tolcrush,scalelen,it,itet,itetoff,xic,yic,zic
     &   ,nadd,xadd,yadd,zadd,ifadd,itadd,did02)
      
      implicit none

      include 'local_element.h'

      real*8 tolcrush,scalelen,xadd(*),yadd(*),zadd(*),xic(*),yic(*)
     &   ,zic(*)
      integer it,itet(*),itetoff(*),nadd,ifadd(*),itadd(*)
      logical did02

      real*8 dmin,ax,ay,az,norm,nx,ny,nz,vx,vy,vz,sdistproj,projx,projy
     $     ,projz,weight1,weight2,weight3,projxmin,projymin,projzmin
      integer i,i0,i1,i2,i3,fmin
                  
      include 'consts.h'
      include 'statementfunctions.h'

      did02=.false.
      dmin=1.d50
      do i=1,nelmnef(ifelmtet)
         i1=itet(itetoff(it)+ielmface1(1,i,ifelmtet))
         i2=itet(itetoff(it)+ielmface1(2,i,ifelmtet))
         i3=itet(itetoff(it)+ielmface1(3,i,ifelmtet))
         i0=itet(itetoff(it)+i)
C...  Note: ordering convention for ielmface1 means that {i,i1,i2,i3}
C...  ={1,2,3,4} (in some order).
         ax=-dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         ay=-dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         az=-dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         norm=sqrt(ax**2+ay**2+az**2)
         nx=ax/safe(norm)
         ny=ay/safe(norm)
         nz=az/safe(norm)
         vx=xic(i0)-xic(i1)
         vy=yic(i0)-yic(i1)
         vz=zic(i0)-zic(i1)
         sdistproj=vx*nx+vy*ny+vz*nz
         projx=xic(i0)-nx*sdistproj
         projy=yic(i0)-ny*sdistproj
         projz=zic(i0)-nz*sdistproj
         ax=-dcrosx(projx,projy,projz,xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         ay=-dcrosy(projx,projy,projz,xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         az=-dcrosz(projx,projy,projz,xic(i2),yic(i2),zic(i2)
     &      ,xic(i3),yic(i3),zic(i3))
         weight1=(nx*ax+ny*ay+nz*az)/safe(norm)
         ax=-dcrosx(xic(i1),yic(i1),zic(i1),projx,projy,projz
     &      ,xic(i3),yic(i3),zic(i3))
         ay=-dcrosy(xic(i1),yic(i1),zic(i1),projx,projy,projz
     &      ,xic(i3),yic(i3),zic(i3))
         az=-dcrosz(xic(i1),yic(i1),zic(i1),projx,projy,projz
     &      ,xic(i3),yic(i3),zic(i3))
         weight2=(nx*ax+ny*ay+nz*az)/safe(norm)
         ax=-dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,projx,projy,projz)
         ay=-dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,projx,projy,projz)
         az=-dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &      ,projx,projy,projz)
         weight3=(nx*ax+ny*ay+nz*az)/safe(norm)
         if ((weight1.gt.0.d0).and.(weight2.gt.0.d0).and.(weight3.gt.0
     &      .d0).and.(sdistproj.lt.dmin)) then         
            dmin=sdistproj
            fmin=i
            projxmin=projx
            projymin=projy
            projzmin=projz
         endif
      enddo
c...  Distance to projected point must be less than tolerance.
      if (dmin.le.tolcrush*scalelen) then
         did02=.true.
         nadd=nadd+1
         xadd(nadd)=projxmin
         yadd(nadd)=projymin
         zadd(nadd)=projzmin
         ifadd(nadd)=fmin
         itadd(nadd)=it
      endif
      return
      end

      subroutine find_11(tolcrush,scalelen,it,ipeltse1,ipedges1,ipeltse2
     &   ,ipedges2,ipitetoff,ipjtetoff,ipitet,ipjtet,itp1,ipitettyp
     &   ,ipiparent,nef,mbndry,xic,yic,zic,nadd,xadd,yadd,zadd,ieadd
     &   ,itadd,iadd,imerge,nmrg,nnodes_actual,changed,did11)

c The 3 diagonal pairs of edges are as follows:
c Edge 1 (nodes 1,2) pairs with edge 6 (nodes 3,4)
c Edge 2 (nodes 1,3) pairs with edge 5 (nodes 2,4)
c Edge 3 (nodes 1,4) pairs with edge 4 (nodes 2,3)
c We thus note the pattern:  edge I pairs with edge 7-I

c Let an edge be described by the pair (x1,t1) where x1 is an endpoint
c of the edge and t1 is a unit vector pointing to the other endpoint
c of the edge.  Similarly, let (x2,t2) describe the 'diagonally
c opposite' edge.  Then there are values (lambda, mu) so that 
c x1+lambda*t1 and x2+mu*t2 form a line segment that is 
c orthogonal to both t1 and t2.  If 0<lambda<LENGTH(x1,t1) and 
c 0<mu<LENGTH(x2,t2), then this segment intersects the 
c diagonally opposite edges in their interior.  Assuming this,
c and assuming the length of the segment is sufficiently small,
c we schedule the first segment for bisection at x1proj=x1+lambda*t1.

c Using elementary vector algebra, it can be shown that 
c if vperp1=t1-t2(t2,t1) (where (t1,t2) means dot product) and
c    vperp2=t2-t1(t1,t2), then
c    ||vperp1|| = ||vperp2|| and 
c    lambda=(vperp1,x2-x1)/||vperp1||^2 and 
c    mu=(vperp2,x1-x2)/||vperp2||^2

      implicit none

      include 'local_element.h'

      pointer (ipeltse1,eltse1)
      integer eltse1(*)
 
      pointer (ipedges1,edges1)
      integer edges1(*)

      pointer (ipeltse2,eltse2)
      integer eltse2(*)

      pointer (ipedges2,edges2)
      integer edges2(*)
      
      pointer (ipitetoff,itetoff)
      integer itetoff(*)
      
      pointer (ipjtetoff,jtetoff)
      integer jtetoff(*)

      pointer (ipitet,itet)
      integer itet(*)
      
      pointer (ipjtet,jtet)
      integer jtet(*)

      pointer (ipitettyp,itettyp)
      integer itettyp(*)

      pointer (ipiparent,iparent)
      integer iparent(*)

      real*8 tolcrush,scalelen,xadd(*),yadd(*),zadd(*),xic(*),yic(*)
     &   ,zic(*)
      integer it,nadd,ieadd(*),itadd(*),iadd(*)
     &   ,imerge(2,*),nnodes_actual,itp1(*)
      logical did11,changed(*)

      logical mergeable
      integer itp_result,itp1proj,itp2proj

      real*8 len1,t1x,t1y,t1z,len2,t2x,t2y,t2z,dot,vperp1x
     $     ,vperp1y,vperp1z,vperp2x,vperp2y,vperp2z,vperpsq,lambda,mu
     $     ,x1proj,y1proj,z1proj,x2proj,y2proj,z2proj,distproj,x0
     &   ,y0,z0,orig_minwidth,min_over_tetlist,mrg1_minwidth
     &   ,mrg2_minwidth
      integer i,i1,i1opp,i2,i2opp,k,nef,mbndry,nold,nnew,nmrg,j
      logical mrg1_feasible, mrg2_feasible
      integer newlist(4,2000),oldlist(4,2000)

      include 'consts.h'
      include 'statementfunctions.h'

      did11=.false.
      do i=1,3
         i1=itet(itetoff(it)+ielmedge1(1,i,ifelmtet))
         i1opp=itet(itetoff(it)+ielmedge1(2,i,ifelmtet))
         i2=itet(itetoff(it)+ielmedge1(1,7-i,ifelmtet))
         i2opp=itet(itetoff(it)+ielmedge1(2,7-i,ifelmtet))
         len1=sqrt((xic(i1opp)-xic(i1))**2+(yic(i1opp)-yic(i1))**2
     &      +(zic(i1opp)-zic(i1))**2)
         t1x=(xic(i1opp)-xic(i1))/len1
         t1y=(yic(i1opp)-yic(i1))/len1
         t1z=(zic(i1opp)-zic(i1))/len1
         len2=sqrt((xic(i2opp)-xic(i2))**2+(yic(i2opp)-yic(i2))**2
     &      +(zic(i2opp)-zic(i2))**2)
         t2x=(xic(i2opp)-xic(i2))/len2
         t2y=(yic(i2opp)-yic(i2))/len2
         t2z=(zic(i2opp)-zic(i2))/len2
         dot=t1x*t2x+t1y*t2y+t1z*t2z
         vperp1x=t1x-t2x*dot
         vperp1y=t1y-t2y*dot
         vperp1z=t1z-t2z*dot
         vperp2x=t2x-t1x*dot
         vperp2y=t2y-t1y*dot
         vperp2z=t2z-t1z*dot
         vperpsq=vperp1x*vperp1x+vperp1y*vperp1y+vperp1z*vperp1z
         lambda=(vperp1x*(xic(i2)-xic(i1))+vperp1y*(yic(i2)-yic(i1))
     &      +vperp1z*(zic(i2)-zic(i1)))/safe(vperpsq)
         mu=(vperp2x*(xic(i1)-xic(i2))+vperp2y*(yic(i1)-yic(i2))
     &      +vperp2z*(zic(i1)-zic(i2)))/safe(vperpsq)
         if ((lambda.gt.0.d0).and.(lambda.lt.len1).and.(mu.gt.0.d0).and
     &      .(mu.lt.len2)) then 
            x1proj=xic(i1)+lambda*t1x
            y1proj=yic(i1)+lambda*t1y
            z1proj=zic(i1)+lambda*t1z
            x2proj=xic(i2)+mu*t2x
            y2proj=yic(i2)+mu*t2y
            z2proj=zic(i2)+mu*t2z
            distproj=sqrt((x1proj-x2proj)**2+(y1proj-y2proj)**2+(z1proj
     &         -z2proj)**2)
            if (distproj.lt.tolcrush*scalelen) then 
               call get_11_tets(it,i,ipeltse1,ipedges1,ipeltse2,ipedges2
     &            ,ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent
     &            ,nef,mbndry,nold,oldlist,nnew,newlist)
               x0=0.d0
               y0=0.d0
               z0=0.d0
               orig_minwidth=min_over_tetlist(nold,oldlist,xic,yic,zic
     &            ,x0,y0,z0)

               itp1proj=itp_result(itp1(i1),itp1(i1opp))
               itp2proj=itp_result(itp1(i2),itp1(i2opp))
c...  What happens when proj1 merges into proj2?
               if (mergeable(itp2proj,itp1proj)) then
                  x0=x2proj
                  y0=y2proj
                  z0=z2proj
                  mrg1_minwidth=min_over_tetlist(nnew,newlist,xic,yic
     &               ,zic,x0,y0,z0)
               else
                  mrg1_minwidth=-1.d50
               endif
c...  What happens when proj2 merges into proj1?
               if (mergeable(itp1proj,itp2proj)) then 
                  x0=x1proj
                  y0=y1proj
                  z0=z1proj
                  mrg2_minwidth=min_over_tetlist(nnew,newlist,xic,yic
     &               ,zic,x0,y0,z0)
               else
                  mrg2_minwidth=-1.d50
               endif

c Put better merge into schedule, if it is feasible.
               if (mrg1_minwidth.gt.mrg2_minwidth) then
                  if (mrg1_minwidth.gt.1.01d0*orig_minwidth) then
                     did11=.true.
c$$$                     print*,'11 event at tet ',it
                     nadd=nadd+1
                     xadd(nadd)=x1proj
                     yadd(nadd)=y1proj
                     zadd(nadd)=z1proj
                     ieadd(nadd)=i
                     itadd(nadd)=it
                     nnodes_actual=nnodes_actual+1
                     iadd(nadd)=-nnodes_actual
c$$$                     print*,'Bisecting edge between nodes :',i1,i1opp
                     nadd=nadd+1
                     xadd(nadd)=x2proj
                     yadd(nadd)=y2proj
                     zadd(nadd)=z2proj
                     ieadd(nadd)=7-i
                     itadd(nadd)=it
                     nnodes_actual=nnodes_actual+1
                     iadd(nadd)=-nnodes_actual
c$$$                     print*,'Bisecting edge between nodes :',i2,i2opp
                     nmrg=nmrg+1
                     imerge(1,nmrg)=nnodes_actual
                     imerge(2,nmrg)=nnodes_actual-1
c$$$                     print*,'Scheduling merge  ',imerge(2,nmrg)
c$$$     &                  ,' into node ',imerge(1,nmrg)
                  endif
               else
                  if (mrg2_minwidth.gt.1.01d0*orig_minwidth) then
                     did11=.true.
c$$$                     print*,'11 event at tet ',it
                     nadd=nadd+1
                     xadd(nadd)=x1proj
                     yadd(nadd)=y1proj
                     zadd(nadd)=z1proj
                     ieadd(nadd)=i
                     itadd(nadd)=it
                     nnodes_actual=nnodes_actual+1
                     iadd(nadd)=-nnodes_actual
c$$$                     print*,'Bisecting edge between nodes :',i1,i1opp
                     nadd=nadd+1
                     xadd(nadd)=x2proj
                     yadd(nadd)=y2proj
                     zadd(nadd)=z2proj
                     ieadd(nadd)=7-i
                     itadd(nadd)=it
                     nnodes_actual=nnodes_actual+1
                     iadd(nadd)=-nnodes_actual
c$$$                     print*,'Bisecting edge between nodes :',i2,i2opp
                     nmrg=nmrg+1
                     imerge(1,nmrg)=nnodes_actual-1
                     imerge(2,nmrg)=nnodes_actual
c$$$                     print*,'Scheduling merge  ',imerge(2,nmrg)
c$$$     &                  ,' into node ',imerge(1,nmrg)
                  endif
               endif
               if (did11) then
c     Mark nodes in old list
                  do j=1,nold
                     do k=1,4
                        changed(iparent(oldlist(k,j)))=.true.
                     enddo
                  enddo
                  return
               endif
            endif
         endif
      enddo
      return
      end

      subroutine mark_edgenbd(changed,nef,itet,itetoff,jtet,jtetoff
     &   ,itettyp,iparent,mbndry,t0,e0)

c... Note: this subroutine not called, but is a template for
c... a more efficient 'get_elements_around_edge'

      implicit none

      include 'local_element.h'

c     This subroutine marks all nodes (setting 'changed' to .true.) that
c     are
c     part of any tet incident on the specified tet edge.

      logical changed(*)
      integer itet(*),itetoff(*),jtet(*),jtetoff(*),itettyp(*),t0,e0
     &   ,mbndry,iparent(*),nef

      integer i1,i2,p1,p2,i,node,fi,jteti,itopp,fopp,j,itop,estack(2)
     $     ,fstack(2),pj1,pj2,fcurr,jtetopp,itcurr,jedge
      logical keepgoing

c.... Nodes of element edge
      i1=iparent(itet(itetoff(t0)+ielmedge1(1,e0,itettyp(t0))))
      i2=iparent(itet(itetoff(t0)+ielmedge1(2,e0,itettyp(t0))))
      p1=min(i1,i2)
      p2=max(i1,i2)
c.... Mark nodes in root tet
      do i=1,nelmnen(itettyp(t0))
         node=itet(itetoff(t0)+i)
         changed(iparent(node))=.true.
      enddo
c.... Put any neighbors on stack
      itop=0
      do i=1,2
         fi=ielmedge2(i,e0,itettyp(t0))
         jteti=jtet(jtetoff(t0)+fi)
         if (jteti.ne.mbndry) then
            if (jteti.gt.mbndry) jteti=jteti-mbndry
            itopp=1+(jteti-1)/nef
            fopp=jteti-(itopp-1)*nef
            do j=1,nelmnen(itettyp(itopp))
               node=itet(itetoff(itopp)+j)
               changed(iparent(node))=.true.
            enddo
            itop=itop+1
            estack(itop)=itopp
            fstack(itop)=fopp
         endif
      enddo
      do while (itop.gt.0)
         itcurr=estack(itop)
         fcurr=fstack(itop)
         itop=itop-1
         keepgoing=.true.
         do while (keepgoing)
            do j=1,ielmface0(fcurr,itettyp(itcurr))
               jedge=ielmface2(j,fcurr,itettyp(itcurr))
               pj1=iparent(itet(itetoff(itcurr)+ielmedge1(1,jedge
     &            ,itettyp(itcurr))))
               pj2=iparent(itet(itetoff(itcurr)+ielmedge1(2,jedge
     &            ,itettyp(itcurr))))
               if (min(pj1,pj2).eq.p1.and.max(pj1,pj2).eq.p2) then
                  goto 10
               endif
            enddo
            print*,'MARK_EDGENBD: Error!'
            stop
 10         if (fcurr.eq.ielmedge2(1,jedge,itettyp(itcurr))) then
               fopp=ielmedge2(2,jedge,itettyp(itcurr))
            else
               fopp=ielmedge2(1,jedge,itettyp(itcurr))
            endif
            jtetopp=jtet(jtetoff(itcurr)+fopp)
            keepgoing=.false.
            if (jtetopp.ne.mbndry) then
               if (jtetopp.gt.mbndry) jtetopp=jtetopp-mbndry
               itopp=1+(jtetopp-1)/nef
               fopp=jtetopp-(itopp-1)*nef
c...  The only way itopp could have already been visited is that 
c...  it is sitting on the stack.  Note:  the stack can have
c...  a maximum of one element on it at this point.
               if (itop.eq.1) then
                  if (itopp.eq.estack(1)) return
               endif
               keepgoing=.true.
               do j=1,nelmnen(itettyp(itopp))
                  node=itet(itetoff(itopp)+j)
                  changed(iparent(node))=.true.
               enddo
               itcurr=itopp
               fcurr=fopp
            endif
         enddo
      enddo
      return
      end
            
      subroutine mark_facenbd(changed,nef,itet,itetoff,jtet,jtetoff
     &   ,itettyp,iparent,mbndry,t0,f0) 

      implicit none

      include 'local_element.h'

c     This subroutine marks all nodes (setting 'changed' to .true.) that
c     are
c     part of any tet incident on the specified tet face.

      logical changed(*)
      integer itet(*),itetoff(*),jtet(*),jtetoff(*),t0,f0,mbndry
     $     ,iparent(*),itettyp(*),nef

      integer i,jtetf,itopp,node

      do i=1,nelmnen(itettyp(t0))
         node=itet(itetoff(t0)+i)
         changed(iparent(node))=.true.
      enddo
      jtetf=jtet(jtetoff(t0)+f0)
      if (jtetf.ne.mbndry) then
         if (jtetf.gt.mbndry) jtetf=jtetf-mbndry
         itopp=1+(jtetf-1)/nef
         do i=1,nelmnen(itettyp(itopp))
            node=itet(itetoff(itopp)+i)
            changed(iparent(node))=.true.
         enddo
      endif
      return
      end

      subroutine get_00_tets(it,ie,nold,oldlist,nnew,newlist,ipelts1
     &   ,ipelts2,itet,itetoff,jtet,jtetoff,itettyp,iparent,nef,mbndry)

      implicit none

      include 'local_element.h'

      integer newlist(4,*),nnew
      integer oldlist(4,*),nold
      pointer (ipelts1,elts1)
      integer elts1(*)
      pointer (ipelts2,elts2)
      integer elts2(*)
      integer it,ie,itet(*),itetoff(*),jtet(*),jtetoff(*),itettyp(*)
     &   ,iparent(*),nef,mbndry

      integer loc1,loc2,par1,par2,nelts1,nelts2,ierrw,tetshare,i,locnod1
     &   ,locnod2,j,nod
      character*132 logmess

      loc1=ielmedge1(1,ie,ifelmtet)
      loc2=ielmedge1(2,ie,ifelmtet)
      par1=iparent(itet(itetoff(it)+loc1))
      par2=iparent(itet(itetoff(it)+loc2))

c     This subroutine gets the list of tets potentially affected when
c     two nodes on an edge are merged together. ('old list')
c     What is also returned is a list of new elements with 'node 0' in an
c     element's node list signifying the node is 'the surviving node'.

c.... Obtain tets around first point.            
      call get_elements_around_node(it,loc1,nelts1,ipelts1,itetoff
     &   ,jtetoff,itet,jtet,itettyp,iparent,nef,mbndry)
c.... Obtain tets around second point.            
      call get_elements_around_node(it,loc2,nelts2,ipelts2,itetoff
     &   ,jtetoff,itet,jtet,itettyp,iparent,nef,mbndry)
c.... Combine into one list.  (Duplicates ok)
      if ((nelts1.gt.1000).or.(nelts2.gt.1000)) then
         write(logmess,'(a)')
     &      'GET_00_TETS:  >1000 incident elts... aborting'
         call writloga('default',0,logmess,0,ierrw)
         stop
      endif

c     Compile 'old' and 'new' tet lists.
      nold=0
      nnew=0
      tetshare=0
c Loop thru tets surrounding first point
      do i=1,nelts1
         nold=nold+1
c Loop through nodes of this tet.  Copy into orig. tet node list.
c For new tet node list, designate central point with zero label,
c Unless second central node exists, in which case skip this element.
         locnod1=0
         locnod2=0
         do j=1,4
            nod=itet(itetoff(elts1(i))+j)
            if (iparent(nod).eq.par1) locnod1=j
            if (iparent(nod).eq.par2) locnod2=j
            oldlist(j,nold)=nod
         enddo
         if (locnod2.eq.0) then
            nnew=nnew+1
            do j=1,4
               
               if (j.eq.locnod1) then
                  newlist(j,nnew)=0
               else
                  nod=itet(itetoff(elts1(i))+j)
                  newlist(j,nnew)=nod
               endif
            enddo
         endif
      enddo
c Loop thru tets surrounding second point
      do 200 i=1,nelts2
c Loop through nodes of this tet.  Copy into orig. tet node list.
c For new tet node list, designate central point with zero label,
c Unless second central node exists, in which case skip this element.
         locnod1=0
         locnod2=0
         do j=1,4
            nod=itet(itetoff(elts2(i))+j)
            if (iparent(nod).eq.par1) then
c... Old list already contains this tet; new list shouldn't have this tet
               goto 200
            endif
            if (iparent(nod).eq.par2) locnod2=j
         enddo
         nold=nold+1
         nnew=nnew+1
         do j=1,4
            nod=itet(itetoff(elts2(i))+j)
            oldlist(j,nold)=nod
            if (j.eq.locnod2) then
               newlist(j,nnew)=0
            else
               newlist(j,nnew)=nod
            endif
         enddo   
 200  enddo

      return 
      end

      subroutine get_01_tets(it,loc0,ie,ipelts1,ipeltse1,ipedges1
     &   ,ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef
     &   ,mbndry,nold,oldlist,nnew,newlist)

      implicit none

      include 'local_element.h'

      integer it,loc0,ie,nold,oldlist(4,*),nnew,newlist(4,*),nef,mbndry

      pointer (ipelts1,elts1)
      integer elts1(*)

      pointer (ipeltse1,eltse1)
      integer eltse1(*)

      pointer (ipedges1,edges1)
      integer edges1(*)

      pointer (ipitetoff,itetoff)
      integer itetoff(*)
   
      pointer (ipjtetoff,jtetoff)
      integer jtetoff(*)

      pointer (ipitet,itet)
      integer itet(*)
  
      pointer (ipjtet,jtet)
      integer jtet(*)

      pointer (ipitettyp,itettyp)
      integer itettyp(*)

      pointer (ipiparent,iparent)
      integer iparent(*)

      integer par0,tetshare1,tetshare2,neltse1,nelts1,i,j,nod,ierrw
      character*132 logmess

      logical found_par0

c     Parent of node
      par0=iparent(itet(itetoff(it)+loc0))

c     This subroutine gets the list of tets that would arise when a
c     specified tet edge is bisected and a specified tet node
c     (originally sharing a face with the specified tet edge) is then
c     merged onto the newly created node on the tet edge.
c     What is returned is a list of elements with 'node 0' in an
c     element's node list signifying the node is 'the new node'.
c     Also returned is the set of elements containing the edge or the
c     node before any creation/merge action is taken.

c     First get the elements around the node
      call get_elements_around_node(it,loc0,nelts1,ipelts1,itetoff
     &         ,jtetoff,itet,jtet,itettyp,iparent,nef,mbndry)
      
c     Now get the elements around the edge
      call get_elements_on_edge(it,ie,neltse1,ipeltse1,ipedges1,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry)

      if ((nelts1.gt.1000).or.(neltse1.gt.1000)) then
         write(logmess,'(a)')
     &      'GET_01_TETS:  >1000 incident elts... aborting'
         call writloga('default',0,logmess,0,ierrw)
         stop
      endif
      
c     Compile the list of elements sharing the node or edge
c     originally...

c     Copy in the set of elements sharing the edge.

      nold=0
      nnew=0
      tetshare1=0
      tetshare2=0
      do i=1,neltse1
         nold=nold+1
c Loop through nodes of this tet.  Copy into orig. tet node list.
c If we encounter 'par0' then tet has no progeny in the new tet list.
c Otherwise, tet has two progeny which we insert in new tet list
         found_par0=.false.
         do j=1,4
            nod=itet(itetoff(eltse1(i))+j)
            oldlist(j,nold)=nod
            if (iparent(nod).eq.par0) found_par0=.true.
         enddo
         if (.not.found_par0) then
c First progeny:  first edge node changed to new node
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(eltse1(i))+j)
               if (j.eq.ielmedge1(1,edges1(i),ifelmtet)) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
c Second progeny:  second edge node changed to new node
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(eltse1(i))+j)
               if (j.eq.ielmedge1(2,edges1(i),ifelmtet)) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
         else
c     Note this tet so we can exclude it in the 'new' list when looping
c     through the node tet list.
            if (tetshare1.eq.0) then
               tetshare1=eltse1(i)
            else
               tetshare2=eltse1(i)
            endif
         endif
      enddo
c Copy in set of tets associated with node.  Exclude shared tets.
      do i=1,nelts1
         if (elts1(i).ne.tetshare1.and.elts1(i).ne.tetshare2) then
            nold=nold+1
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(elts1(i))+j)
               oldlist(j,nold)=nod
               if (iparent(nod).eq.par0) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
         endif
      enddo
      return
      end

      subroutine get_11_tets(it,ie,ipeltse1,ipedges1,ipeltse2,ipedges2
     &   ,ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef
     &   ,mbndry,nold,oldlist,nnew,newlist)

      implicit none 

      include 'local_element.h'

      integer it,ie,nold,oldlist(4,*),nnew,newlist(4,*),nef,mbndry

      pointer (ipeltse1,eltse1)
      integer eltse1(*)

      pointer (ipedges1,edges1)
      integer edges1(*)

      pointer (ipeltse2,eltse2)
      integer eltse2(*)

      pointer (ipedges2,edges2)
      integer edges2(*)

      pointer (ipitetoff,itetoff)
      integer itetoff(*)
   
      pointer (ipjtetoff,jtetoff)
      integer jtetoff(*)

      pointer (ipitet,itet)
      integer itet(*)
  
      pointer (ipjtet,jtet)
      integer jtet(*)

      pointer (ipitettyp,itettyp)
      integer itettyp(*)

      pointer (ipiparent,iparent)
      integer iparent(*)

      integer neltse1,neltse2,ierrw,tetshare,i,j,nod
      character*132 logmess

c     This subroutine gets the list of tets that would arise when a
c     one tet edge is bisected and then the diagonally opposite tet
c     edge in the same tet is bisected and then the two newly
c     created nodes are merged together (thus resulting in the 
c     net creation of one 'new node').
c     What is returned is a list of new elements with 'node 0' in an
c     element's node list signifying the node is 'the new node'.
c     Also returned is the set of elements containing either edge
c     before any creation/merge action is taken.

c     Get the elements around first edge ie.
      call get_elements_on_edge(it,ie,neltse1,ipeltse1,ipedges1,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry)
c     Get the elements around diag opposite edge.  According to 
c     our tetedge conventions, this is always edge 7-ie.
      call get_elements_on_edge(it,7-ie,neltse2,ipeltse2,ipedges2,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry)

      if ((neltse1.gt.1000).or.(neltse2.gt.1000)) then
         write(logmess,'(a)')
     &      'GET_11_TETS:  >1000 incident elts... aborting'
         call writloga('default',0,logmess,0,ierrw)
         stop
      endif
      
c     Compile the list of elements sharing the node or edge
c     originally...

c     Copy in the set of elements sharing the edge ie.

      nold=0
      nnew=0
      tetshare=0
      do i=1,neltse1
         nold=nold+1
c Loop through nodes of this tet.  Copy into orig. tet node list.
c For new tet node list, each tet has two progeny, except for 
c tet 'it' which as zero progeny.
         do j=1,4
            nod=itet(itetoff(eltse1(i))+j)
            oldlist(j,nold)=nod
         enddo
         if (eltse1(i).ne.it) then
c First progeny:  first edge node changed to new node
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(eltse1(i))+j)
               if (j.eq.ielmedge1(1,edges1(i),ifelmtet)) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
c Second progeny:  second edge node changed to new node
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(eltse1(i))+j)
               if (j.eq.ielmedge1(2,edges1(i),ifelmtet)) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
         endif
      enddo
c Copy in set of tets associated with edge 7-ie.  Exclude tet 'it'.
      do i=1,neltse2
         if (eltse2(i).ne.it) then
c Loop through nodes of this tet.  Copy into orig. tet node list.
c For new tet node list, each tet has two progeny.
            nold=nold+1
            do j=1,4
               nod=itet(itetoff(eltse2(i))+j)
               oldlist(j,nold)=nod
            enddo
c First progeny:  first edge node changed to new node
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(eltse2(i))+j)
               if (j.eq.ielmedge1(1,edges2(i),ifelmtet)) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
c Second progeny:  second edge node changed to new node
            nnew=nnew+1
            do j=1,4
               nod=itet(itetoff(eltse2(i))+j)
               if (j.eq.ielmedge1(2,edges2(i),ifelmtet)) then
                  newlist(j,nnew)=0
               else
                  newlist(j,nnew)=nod
               endif
            enddo
         endif
      enddo
      return 
      end

      function min_over_tetlist(ntets,tetlist,xic,yic,zic,x0,y0,z0)

      implicit none

      real*8 min_over_tetlist,xic(*),yic(*),zic(*),x0,y0,z0
      integer ntets,tetlist(4,*)

      integer i
      real*8 xica,yica,zica,xicb,yicb,zicb,xicc,yicc,zicc,xicd,yicd,zicd
      real*8 relative_width

      min_over_tetlist=1.d50
      do i=1,ntets
         if (tetlist(1,i).eq.0) then
            xica=x0
            yica=y0
            zica=z0
         else
            xica=xic(tetlist(1,i))
            yica=yic(tetlist(1,i))
            zica=zic(tetlist(1,i))
         endif
         if (tetlist(2,i).eq.0) then
            xicb=x0
            yicb=y0
            zicb=z0
         else
            xicb=xic(tetlist(2,i))
            yicb=yic(tetlist(2,i))
            zicb=zic(tetlist(2,i))
         endif
         if (tetlist(3,i).eq.0) then
            xicc=x0
            yicc=y0
            zicc=z0
         else
            xicc=xic(tetlist(3,i))
            yicc=yic(tetlist(3,i))
            zicc=zic(tetlist(3,i))
         endif
         if (tetlist(4,i).eq.0) then
            xicd=x0
            yicd=y0
            zicd=z0
         else
            xicd=xic(tetlist(4,i))
            yicd=yic(tetlist(4,i))
            zicd=zic(tetlist(4,i))
         endif

         min_over_tetlist=min(min_over_tetlist,relative_width(xica,yica
     &      ,zica,xicb,yicb,zicb,xicc,yicc,zicc,xicd,yicd,zicd))
      enddo
      return
      end

      function itp_result(itpa,itpb)

      implicit none

c We predict point type of a node created on an edge between nodes
c with point types itpa, itpb.
c
c We assume we are in the "0,2,10,12" universe of point types.

      integer itp_result,itpa,itpb

c We map itp to a two-vector V, each of whose components is ordered.
c The BND (first) component is 1 if the node is on the boundary
c and 0 if the node is in the interior.  The MAT (second) component is
c given the number of incident materials.  Since we only have itp
c available here, we distinguish only between one material (V(MAT)=1)
c and more than one material (V(MAT)=2).
c
c We then assume that the resultant value of V(midpoint node)
c is the componentwise minimum of V(itpa), V(itpb).  This is then
c mapped back to the "0,2,10,12" universe.

      integer va(2),vb(2),vab(2)
      integer bnd,mat
      parameter (bnd=1,mat=2)

      if (itpa.eq.0) then
         va(bnd)=0
         va(mat)=1
      elseif (itpa.eq.2) then
         va(bnd)=0
         va(mat)=2
      elseif (itpa.eq.10) then
         va(bnd)=1
         va(mat)=1
      elseif (itpa.eq.12) then
         va(bnd)=1
         va(mat)=2
      else
         print*,'Error!  ITPA=',itpa
         itp_result=0
         return
      endif
      if (itpb.eq.0) then
         vb(bnd)=0
         vb(mat)=1
      elseif (itpb.eq.2) then
         vb(bnd)=0
         vb(mat)=2
      elseif (itpb.eq.10) then
         vb(bnd)=1
         vb(mat)=1
      elseif (itpb.eq.12) then
         vb(bnd)=1
         vb(mat)=2
      else
         print*,'Error!  ITPB=',itpb
         itp_result=0
         return
      endif

      vab(bnd)=min(va(bnd),vb(bnd))
      vab(mat)=min(va(mat),vb(mat))

      if (vab(bnd).eq.0) then
         if (vab(mat).eq.1) then
            itp_result=0
         else
            itp_result=2
         endif
      else
         if (vab(mat).eq.1) then
            itp_result=10
         else
            itp_result=12
         endif
      endif
      return
      end

      function mergeable(itplives,itpdies)

      implicit none

c We predict if a node with point type itpdies can be merged into 
c a node with point type itplives.
c
c We assume we are in the "0,2,10,12" universe of point types.

      logical mergeable
      integer itplives,itpdies

c We map itp to a two-vector V, each of whose components is ordered.
c The BND (first) component is 1 if the node is on the boundary
c and 0 if the node is in the interior.  The MAT (second) component is
c given the number of incident materials.  Since we only have itp
c available here, we distinguish only between one material (V(MAT)=1)
c and more than one material (V(MAT)=2).
c
c We then assume that we must have V(dies) less than or equal to V(lives),
c componentwise, in order for the merge to be feasible.

      integer vlives(2),vdies(2)
      integer bnd,mat
      parameter (bnd=1,mat=2)

      if (itplives.eq.0) then
         vlives(bnd)=0
         vlives(mat)=1
      elseif (itplives.eq.2) then
         vlives(bnd)=0
         vlives(mat)=2
      elseif (itplives.eq.10) then
         vlives(bnd)=1
         vlives(mat)=1
      elseif (itplives.eq.12) then
         vlives(bnd)=1
         vlives(mat)=2
      else
         print*,'Error!  ITPLIVES=',itplives
         mergeable=.false.
         return
      endif
      if (itpdies.eq.0) then
         vdies(bnd)=0
         vdies(mat)=1
      elseif (itpdies.eq.2) then
         vdies(bnd)=0
         vdies(mat)=2
      elseif (itpdies.eq.10) then
         vdies(bnd)=1
         vdies(mat)=1
      elseif (itpdies.eq.12) then
         vdies(bnd)=1
         vdies(mat)=2
      else
         print*,'Error!  ITPDIES=',itpdies
         mergeable=.false.
         return
      endif
      if (vdies(bnd).le.vlives(bnd)) then
         if (vdies(mat).le.vlives(mat)) then
            mergeable=.true.
         else
            mergeable=.false.
         endif
      else
         mergeable=.false.
      endif
      return
      end
