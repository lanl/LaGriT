      subroutine cer_chain(cmo,tollength,toldamage,mpary_in,mpno_in,
     *   inclusive,psetname,cmode,ierror)
c
c #####################################################################
c
c    PURPOSE
c
c       CER_CHAIN ("Create on roughness")
c     takes a mesh object and bisects edges that
c     (i) have both endpoints in the list of selected mass points, and
c     (ii)have the distance from one endpoint to the tangent plane
c         of the other endpoint > tollength
c
c     The process is recursive in that new nodes are added to the
c     list of mass points, meaning that newly created edges can
c     be refined until all the edges in the mesh satisfy the
c     criterion.  Also, all edge refinement obeys
c     the 'Rivara Principle' which is that a refined edge should
c     be the longest edge in ANY element that contains it.  This
c     leads to recursive refinement with nondegrading element
c     aspect ratios.
c
c    INPUT ARGUMENTS -
c
c       CMO - name of current mesh object
c       TOLLENGTH - maximum allowable distance to tangent plane
c       TOLDAMAGE - max. damage (used by recon)
c       MPARY_IN - array of mass points
c       MPNO_IN - no. of mass points
C       INCLUSIVE - 1 means inclusive - edge is a refine candidate
C                    if either node is in pset,
C                  - 0 means exclusive - edge is a refine candidate
C                    if both nodes are in pset
c       PSETNAME - name of pset for rivara refinement - if -def-
C                  then all nodes are in the pset
c       CMODE - rivara method 'full' means use full chain type
C                             'truncated' means stop at end of
c                                         pset
c                             'boundary' means only refine boundary
c                                         edges
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
c
C $Log: cer_chain.f,v $
C Revision 2.00  2007/11/05 19:45:47  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.12   07 Jan 2002 14:00:56   dcg
CPVCS    add error return argument to refine_edge_add_tet call
CPVCS
CPVCS       Rev 1.11   29 May 2001 17:55:10   kuprat
CPVCS    We commented out the RECON after refinement.
CPVCS
CPVCS       Rev 1.10   22 May 2001 16:39:00   dcg
CPVCS    fix loop limits if 2d mesh
CPVCS
CPVCS       Rev 1.9   07 May 2001 16:34:00   dcg
CPVCS    make it work for 2D meshes
CPVCS
CPVCS       Rev 1.8   Thu Apr 06 08:16:00 2000   dcg
CPVCS      remove get_info_i calls
CPVCS
CPVCS       Rev 1.7   Tue Sep 07 17:05:22 1999   dcg
CPVCS    fix error when working with a pset
CPVCS
CPVCS       Rev 1.6   Mon Jun 14 17:35:00 1999   dcg
CPVCS    change argument in call to refine_face_add to ipiadd so as to
CPVCS    be able to use call to refine_fix_add from refine_face_add
CPVCS
CPVCS       Rev 1.5   Wed Mar 17 10:52:44 1999   dcg
CPVCS    set pset membership of child nodes in refine_edge_add
CPVCS
CPVCS       Rev 1.4   Tue Feb 23 15:19:16 1999   dcg
CPVCS    check for matching colors of elements when accepting
CPVCS    contributions for the tangent plane
CPVCS
CPVCS       Rev 1.2   Thu Dec 24 13:40:20 1998   dcg
CPVCS    fix parent/child chains and divide by zero problems
CPVCS
CPVCS       Rev 1.1   Thu Dec 03 12:41:48 1998   dcg
CPVCS    change call to getedges to return only edges on surface
c
c #####################################################################
 
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      character*132 logmess
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipisetwd, isetwd)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
      pointer (ipitetclr, itetclr)
      integer itp1(lenptr)
      integer icr1(lenptr)
      integer isn1(lenptr)
      integer isetwd(lenptr)
      real*8 xic(lenptr)
      real*8 yic(lenptr)
      real*8 zic(lenptr)
      integer itet(lenptr)
      integer itetoff(lenptr)
      integer itettyp(lenptr),itetclr(lenptr)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(lenptr),jtetoff(lenptr)
 
      pointer (ipireal1,ireal1),(ipinvmpary,invmpary),
     &   (ipiparent,iparent),(ipmpary,mpary),(ipiedges,iedges),
     &   (ipiedges_first,iedges_first),(ipiedge_element,iedge_element),
     &   (ipelen,elen),(ipielist,ielist),(ipitadd,itadd),
     &   (ipieadd,ieadd),(ipiadd,iadd),(ipxadd,xadd),(ipyadd,yadd),
     &   (ipzadd,zadd),(ipicradd,icradd),
     &   (ipitpadd,itpadd),(ipmpary1,mpary1)
      integer invmpary(lenptr),ireal1(lenptr),iparent(lenptr),
     &   mpary(lenptr),iedges(lenptr),iedges_first(lenptr),
     &   iedge_element(lenptr),ielist(lenptr),itadd(lenptr),
     &   ieadd(lenptr),iadd(lenptr),icradd(lenptr),itpadd(lenptr),
     &   mpary1(lenptr)
      real*8 elen(lenptr),xadd(lenptr),yadd(lenptr),zadd(lenptr)
 
      pointer (ipelts,elts)
      integer elts(lenptr)
      pointer (ipedges,edges)
      integer edges(lenptr)
      character*8 ich1,ich2
      character*32 cmo,isubname,psetname,cmode
 
 
      integer mpary_in(lenptr),mpno_in,loc1,loc2,ipar1,ipar2,flag,
     &   ierror,nnodes,length,icmotype,nelements,icscode,mpno1,
     &   i,j,nod1,ityp,k,nef_cmo,minpar,maxpar,lenlist,
     &   mpno,mpno_old,ierrdum,numedges,nod2,nadd,ipos,
     &   ielt,iedg,ic1,ic2,inclusive,
     &   nnodes_old,
     &   ierrw,len_mpno,len_nnodes,inc,len_edges,len_elist,
     &   len_elements,node,node1,mbndry,i2,
     &   nelts,nrealnodes,previousmaxedge,nsd,
     *   iloc1,iloc2,it,l,ie,m
      logical lsomereversed
 
      real*8 ascend,tollength,toldamage,
     &   a1,a2,b1,b2,c1,c2,d1,d2,epsilonl
 
      isubname = 'cer_chain'
      ich1='pset'
      ich2='get'
      ierror=0
 
c.... Initialize to zero the values of scalars used for keeping track
c.... of the lengths of dynamically managed arrays.
 
      len_mpno=0
      len_nnodes=0
      len_edges=0
      len_elist=0
      len_elements=0
c....
c.... Create temporary storage for elements that share an edge and
c.... their local edge numbers
c....
      call mmgetblk('elts',isubname,ipelts,100,1,icscode)
      call mmgetblk('edges',isubname,ipedges,100,1,icscode)
c
c.... We copy the input mass point array to a new array.  The new array
c.... will grow as new points are created.  Allocate an array for
c.... nodes on the surface
c
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      len_mpno=nnodes
      call mmgetblk('mpary',isubname,ipmpary,len_mpno,1,icscode)
      call mmgetblk('mpary1',isubname,ipmpary1,len_mpno,1,icscode)
c
      do k=1,mpno_in
         mpary(k)=mpary_in(k)
      enddo
      mpno=mpno_in
c
c.... Outer iteration loop for creating nodes.  On each iteration of
c.... the outer loop, a set of edges that do not interfere with each
c.... other is bisected.  Since this changes the mesh object, each
c.... outer loop iteration includes getting fresh pointers for the
c.... mesh object and recalculating the relevant geometric quantities.
 
 1    continue
 
c.... Get info from mesh object.
 
      call get_epsilon('epsilonl', epsilonl)
      call cmo_get_intinfo('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,
     *   mbndry,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,length,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_intinfo('faces_per_element',cmo,nef_cmo,
     &   length,icmotype,ierror)
      call cmo_get_intinfo('ndimensions_topo',cmo,nsd,length,icmotype,
     &   ierror)
c
      if (psetname(1:5).eq.'-def-') then
         do j=1,mpno_in
            mpary1(j)=j
         enddo
         mpno1=mpno_in
      else
         call pntlimc(ich1,ich2,psetname,ipmpary1,mpno1,
     *                                nnodes,isetwd,itp1)
      endif
 
c.... Get memory for arrays that should have length NNODES.
 
      if (len_nnodes.eq.0) then
         len_nnodes=1000+nnodes
         call mmgetblk('ireal1',isubname,ipireal1,len_nnodes,1,icscode)
         call mmgetblk('invmpary',isubname,ipinvmpary,len_nnodes,1
     &      ,icscode)
         call mmgetblk('iedges_first',isubname,ipiedges_first,len_nnodes
     &      ,1,icscode)
         call mmgetblk('iparent',isubname,ipiparent,
     &      len_nnodes,1,icscode)
      elseif (len_nnodes.le.nnodes) then
         inc=1000+nnodes-len_nnodes
         len_nnodes=len_nnodes+inc
         call mmincblk('ireal1',isubname,ipireal1,inc,icscode)
         call mmincblk('invmpary',isubname,ipinvmpary,inc,icscode)
         call mmincblk('iedges_first',isubname,ipiedges_first,inc
     &      ,icscode)
         call mmincblk('iparent',isubname,ipiparent,inc
     &      ,icscode)
      endif
 
c        1) do we have a real point?
c             ireal1() =  0 ==> not a real point.
c             ireal1() =  1 ==> a real point.
c
      call unpacktp("allreal","set",nnodes,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error('cel', 'unpacktp')
c
c     find the parents of each node.
c
      call unpackpc(nnodes,itp1,isn1,iparent)
c
c.... Use invmpary to store all valid nodes - if the chain takes
c.... us beyond the valid area stop and do refinement on longest
c.... edge in valid spac
c
      do i=1,nnodes
         invmpary(i)=0
      enddo
      do i=1,mpno1
         invmpary(mpary1(i))=1
      enddo
      do i=1,mpno1
         mpary1(i)=iparent(mpary1(i))
      enddo
      do i=1,nnodes
         mpary(i)=iparent(mpary(i))
      enddo
c
c.... Obtain mpnode-mpnode relation.  For a given mass point node
c.... ("mpnode") that appears in the list of mass points MPARY,
c.... the mpnode-mpnode relation is a list of neighbouring mpnodes.
c.... This is thus the list of "exclusive" edges
c.... incident on the set of mass points.  (The "inclusive" edges
c.... would not require that BOTH endpoints of the edge reside
c.... in MPARY.)  It is here assumed that in the case of
c.... parent-child points, only the parent appears in MPARY.
c.... for full rivara get all edges
c
      nrealnodes=0
      call getedges(mpary1,mpno1,nnodes,nelements,itet,itetoff,
     &     itettyp,iparent,isubname,inclusive,ipiedges,iedges_first)
      numedges=iedges_first(nnodes+1)-1
c
c.... Compute the edge-"any element" relation.  Given an
c.... edge in IEDGES, this relation gives the index of
c.... SOME element that contains this edge.  This element
c.... will be used as a 'seed' for determining the set
c.... of elements that share a given edge.  In fact, similar to
c.... the JTET relation, we store a ``hybrid'' piece of information
c.... associated with each edge which is
c....
c....   (elt-1)*maxnee2+locedge
c....
c.... That is, we take the element number, subtract 1 from it, multiply it
c.... by MAXNEE2 (the largest number of edges an element can have),
c.... and add the local edge number that the given edge appears as
c.... in the element.  In this way, we know not only an element that
c.... contains the edge, but the edge's local position in that element.
 
      if (len_edges.eq.0) then
         len_edges=1000+numedges
         call mmgetblk('iedge_element',isubname,ipiedge_element
     &      ,len_edges,1,icscode)
         call mmgetblk('elen',isubname,ipelen,len_edges,2,icscode)
         call mmgetblk('ielist',isubname,ipielist,len_edges,1,icscode)
      elseif (len_edges.le.numedges) then
         inc=1000+numedges-len_edges
         len_edges=len_edges+inc
         call mmincblk('iedge_element',isubname,ipiedge_element,inc
     &      ,icscode)
         call mmincblk('elen',isubname,ipelen,inc,icscode)
         call mmincblk('ielist',isubname,ipielist,inc,icscode)
      endif
 
      do k=1,numedges
         iedge_element(k)=0
      enddo
      do i=1,nelements
        ityp=itettyp(i)
        do j=1,nelmnef(ityp)
          m=ielmface0(j,ityp)
          if(nsd.eq.2) m=1
          do k=1,m
            if(jtet(jtetoff(i)+j).ge.mbndry.or.nsd.eq.2) then
              ie=ielmface2(k,j,ityp)
              loc1=ielmedge1(1,ie,ityp)
              loc2=ielmedge1(2,ie,ityp)
              ipar1=iparent(itet(loc1+itetoff(i)))
              ipar2=iparent(itet(loc2+itetoff(i)))
              minpar=min(ipar1,ipar2)
              maxpar=max(ipar1,ipar2)
              do l=iedges_first(minpar),iedges_first(minpar+1)-1
                if (maxpar.eq.iedges(l)) then
                   if (iedge_element(l).eq.0) then
                     iedge_element(l)=(i-1)*maxnee2+ie
                   endif
                endif
              enddo
            endif
          enddo
        enddo
      enddo
 
c.... Loop over IEDGES and compute ELEN, the length of the edge.
c.... Get the tangent planes at each enpoint
c.... If the distance from the endpoint to the tangent plane of
c.... the other endpoint > tollength then refine this edge
 
      lenlist=0
      do i=1,nnodes
         nod1=iparent(i)
         do k=iedges_first(i),iedges_first(i+1)-1
           if(iedge_element(k).ne.0) then
             it= 1+(iedge_element(k)-1)/maxnee2
             ityp=itettyp(it)
             do j=1,nelmnen(ityp)
               if(nod1.eq.iparent(itet(itetoff(it)+j))) iloc1=j
             enddo
             i2=iedges(k)
             nod2=iparent(i2)
             do j=1,nelmnen(ityp)
               if(nod2.eq.iparent(itet(itetoff(it)+j)))iloc2=j
             enddo
             d1=0.0
             d2=0.0
             if (nsd.eq.3) then
                call tangent_plane(xic,yic,zic,it,iloc1,invmpary,
     *             itetoff,jtetoff,
     *             itet,jtet,itettyp,itetclr,iparent,nef_cmo,
     *             mbndry,toldamage,a1,b1,c1,d1)
             else
                call get_elements_around_node(it,iloc1,
     *             nelts,ipelts,
     *             itetoff,jtetoff,itet,jtet,itettyp,iparent,
     *             nef_cmo,mbndry)
                call synthnormal(nod1,nelts,elts,iparent,itet,itetoff,
     *             xic,yic,zic,epsilonl,a1,b1,c1,
     *             lsomereversed)
                   d1=-a1*xic(nod1)-b1*yic(nod1)-c1*zic(nod1)
             endif
             if(abs(a1**2+b1**2+c1**2).le.epsilonr) then
                  d1=0.0
             else
                  d1=abs (a1*xic(i2)+
     *                 b1*yic(i2)+
     *                 c1*zic(i2)+d1)/
     *                 sqrt(a1**2+b1**2+c1**2)
             endif
             if(d1.lt.tollength) then
                if (nsd.eq.3) then
                  call tangent_plane(xic,yic,zic,it,iloc2,invmpary,
     *               itetoff,jtetoff,itet,jtet,itettyp,itetclr,iparent,
     *               nef_cmo,mbndry,toldamage,a2,b2,c2,d2)
                else
                   call get_elements_around_node(it,iloc2,
     *                nelts,ipelts,
     *                itetoff,jtetoff,itet,jtet,itettyp,iparent,
     *                nef_cmo,mbndry)
                   call synthnormal(nod2,nelts,elts,iparent,itet,
     *                itetoff,xic,yic,zic,epsilonl,a2,b2,c2,
     *                lsomereversed)
                   d2=-a2*xic(nod2)-b2*yic(nod2)-c2*zic(nod2)
                endif
               if(abs(a2**2+b2**2+c2**2).le.epsilonr) then
                  d2=0.0
               else
                  d2=abs((a2*xic(i)+b2*yic(i)+c2*zic(i)+d2)/
     *              sqrt(a2**2+b2**2+c2**2))
               endif
             endif
             if (d1.gt.tollength.or.d2.gt.tollength) then
               lenlist=lenlist+1
               ielist(lenlist)=k
               elen(lenlist)=sqrt((xic(i)-xic(i2))**2+
     *                            (yic(i)-yic(i2))**2+
     *                            (zic(i)-zic(i2))**2)
             endif
           endif
         enddo
      enddo
 
c.... If LENLIST is zero, no edges are to be refined, and we are done.
      if (lenlist.eq.0) goto 9999
 
c.... We now sort the edges in IELIST in reverse order
c.... using ELEN as a key.  Thus, we will try to bisect the
c.... longest edges first.  This will assure the 'Rivara Principle'
c.... of not refining any element edge that is not the
c.... longest edge in that element.
 
      ascend=-1.
      call hpsort1(lenlist,elen,ascend,ielist)
 
c.... Allocate memory for 'add' arrays.
 
      if (len_elist.eq.0) then
         len_elist=1000+lenlist
         call mmgetblk('itadd',isubname,ipitadd,len_elist,1,icscode)
         call mmgetblk('ieadd',isubname,ipieadd,len_elist,1,icscode)
         call mmgetblk('iadd',isubname,ipiadd,len_elist,1,icscode)
         call mmgetblk('itpadd',isubname,ipitpadd,len_elist,1,icscode)
         call mmgetblk('icradd',isubname,ipicradd,len_elist,1,icscode)
         call mmgetblk('xadd',isubname,ipxadd,len_elist,2,icscode)
         call mmgetblk('yadd',isubname,ipyadd,len_elist,2,icscode)
         call mmgetblk('zadd',isubname,ipzadd,len_elist,2,icscode)
      elseif (len_elist.le.lenlist) then
         inc=1000+lenlist-len_elist
         len_elist=len_elist+inc
         call mmincblk('itadd',isubname,ipitadd,inc,icscode)
         call mmincblk('ieadd',isubname,ipieadd,inc,icscode)
         call mmincblk('iadd',isubname,ipiadd,inc,icscode)
         call mmincblk('itpadd',isubname,ipitpadd,inc,icscode)
         call mmincblk('icradd',isubname,ipicradd,inc,icscode)
         call mmincblk('xadd',isubname,ipxadd,inc,icscode)
         call mmincblk('yadd',isubname,ipyadd,inc,icscode)
         call mmincblk('zadd',isubname,ipzadd,inc,icscode)
      endif
 
c.... Loop thru node list, and assemble a list of edges for
c.... refinement.
 
      nadd=0
      do ipos=1,lenlist
         previousmaxedge=0
         i=ielist(ipos)
            ielt=1+(iedge_element(i)-1)/maxnee2
            iedg=iedge_element(i)-maxnee2*(ielt-1)
50          ityp=itettyp(ielt)
            ipar1=itet(ielmedge1(1,iedg,ityp)+itetoff(ielt))
            ipar2=itet(ielmedge1(2,iedg,ityp)+itetoff(ielt))
            if(nsd.eq.3) then
               call get_elements_on_edge(ielt,iedg,nelts,ipelts,
     *           ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *           ipitettyp,ipiparent, nef_cmo,mbndry)
            elseif(nsd.eq.2) then
               call get_elements_on_edge2d(ielt,iedg,nelts,ipelts,
     *           ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *           ipitettyp,ipiparent, nef_cmo,mbndry)
            endif
c.... Add this edge to refinement list
            nadd=nadd+1
            itadd(nadd)=ielt
            ieadd(nadd)=iedg
            iadd(nadd)=0
            ic1=itet(ielmedge1(1,iedg,ityp)+itetoff(ielt))
            ic2=itet(ielmedge1(2,iedg,ityp)+itetoff(ielt))
            ipar1=iparent(ic1)
            ipar2=iparent(ic2)
            minpar=min(ipar1,ipar2)
            maxpar=max(ipar1,ipar2)
            xadd(nadd)=half*(xic(ic1)+xic(ic2))
            yadd(nadd)=half*(yic(ic1)+yic(ic2))
            zadd(nadd)=half*(zic(ic1)+zic(ic2))
  60     continue
      enddo
 
c.... If the set of mass points is less than the total number of real
c.... points, it is possible that there are LENLIST edges longer than
c.... the tolerance, but we are unable to bisect them, because they
c.... are not the longest edges in their respective elements and the
c.... longer edges do not have both endpoints in the set of mass points.
c.... In this case, print a warning and exit.
 
      if (nadd.eq.0) then
         write(logmess,'(a,i7,a)') 'Unable to bisect ',lenlist,
     &      ' edges due to longest edge condition.'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
      nnodes_old=nnodes
      if(nsd.eq.3) then
         call refine_fix_add(cmo,nadd,ipitadd,ipieadd,ipiadd,
     &   ipitpadd,ipicradd)
         call refine_edge_add_tet(cmo,nadd,ipitadd,ipieadd,
     &   iadd,xadd,yadd,zadd,flag)
c.... Fix up ITP1, ICR1.
 
         call cmo_get_info('nnodes',cmo,
     *      nnodes,length,icmotype,ierror)
         call cmo_get_info('itp1',cmo,
     *      ipitp1,length,icmotype,ierror)
         call cmo_get_info('icr1',cmo,
     *      ipicr1,length,icmotype,ierror)
         call cmo_get_info('isn1',cmo,
     *      ipisn1,length,icmotype,ierror)
         call cmo_get_info('isetwd',cmo,
     *      ipisetwd,length,icmotype,ierror)
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
      elseif(nsd.eq.2) then
c
c convert edge numbers to face numbers
c
         do j=1,nadd
            if(ieadd(j).eq.1) then
               ieadd(j)=3
            elseif(ieadd(j).eq.3) then
               ieadd(j)=1
            endif
         enddo
         call refine_face_add_tri(cmo,nadd,ipitadd,ipieadd,
     &   ipiadd,xadd,yadd,zadd)
      endif
C
      call cmo_get_info('ivoronoi',cmo,
     &   ivoronoi,length,icmotype,icscode)
C
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
C
      if (psetname(1:5).eq.'-def-') then
         mpno_old=mpno
         mpno=mpno+nnodes-nnodes_old
         mpno1=mpno
         if (len_mpno.le.mpno) then
            inc=1000+mpno-len_mpno
            len_mpno=len_mpno+inc
            call mmincblk('mpary',isubname,ipmpary,inc,icscode)
            call mmincblk('mpary1',isubname,ipmpary1,inc,icscode)
         endif
         do j=mpno_old+1,mpno
            mpary(j)=j-mpno_old+nnodes_old
            mpary1(j)=j-mpno_old+nnodes_old
         enddo
      else
         mpno_old=mpno
         if (len_mpno.le.nnodes) then
            inc=nnodes-len_mpno+1000
            len_mpno=nnodes+1000
            call mmincblk('mpary',isubname,ipmpary,inc,icscode)
            call mmincblk('mpary1',isubname,ipmpary1,inc,icscode)
         endif
         call pntlimc(ich1,ich2,psetname,ipmpary,mpno1,
     *                                nnodes,isetwd,itp1)
      endif
c$$$      write(logmess,'(a,e16.8,a)')'recon/0/',toldamage
c$$$     &      ,'/ ; finish'
c$$$       call dotaskx3d(logmess,icscode)
 
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
