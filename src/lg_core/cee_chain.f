      subroutine cee_chain(cmo,toldamage,
     *   mpary_in,mpno_in,inclusivein,psetname,epsilonl,cfield,ierror)
c
c #####################################################################
c
c    PURPOSE
c
c       CEE_CHAIN ("Create on Edge Error")
c     takes a mesh object and bisects edges that
c     (i) have both endpoints in the list of selected mass points, and
c     (ii) have edge error in the top pc_refine percent
c          or have edge error > error_refine
c     (e.g.) if pc_refine is 10 then then 10% of the edges will
c     be bisected - the ones with the worst error.
c
c     All edge refinement obeys
c     the 'Rivara Principle' which is that a refined edge should
c     be the longest edge in ANY element that contains it.  This
c     leads to recursive refinement with nondegrading element
c     aspect ratios.
c
c    INPUT ARGUMENTS -
c
c       CMO - name of current mesh object
c       ERROR_CUT_OFF - percent of edges to be refined
c       TOLDAMAGE - max. damage (used by recon)
c       MPARY_IN - array of mass points
c       MPNO_IN - no. of mass points
C       INCLUSIVE - 1 means inclusive - edge is a refine candidate
C                    if either node is in pset,
C                  - 0 means exclusive - edge is a refine candidate
C                    if both nodes are in pset
c       PSETNAME - name of pset for rivara refinement - if -def-
C                  then all nodes are in the pset
c       CFIELD - name of field to use for adaptive refinement
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
c
C $Log: cee_chain.f,v $
C Revision 2.00  2007/11/05 19:45:47  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   01 Mar 2002 14:45:42   dcg
CPVCS    adaptive merging
CPVCS    
CPVCS       Rev 1.4   25 Feb 2002 14:09:52   dcg
CPVCS    allow absolute error cut off or percent of edges
CPVCS    to refine
CPVCS    
CPVCS       Rev 1.3   20 Feb 2002 16:30:14   dcg
CPVCS    fix memory managements error for '..add' arrays
CPVCS    
CPVCS       Rev 1.2   07 Feb 2002 13:40:14   dcg
CPVCS    do rivara chain not just longest edge at
CPVCS    end of chain which is what previous version did
CPVCS    
CPVCS       Rev 1.1   06 Feb 2002 13:54:38   dcg
CPVCS    just do one iteration
CPVCS    
CPVCS       Rev 1.0   31 Jan 2002 13:11:34   dcg
CPVCS    Initial revision.
c
c #####################################################################
 
      implicit none
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'
      include 'massage.h'
 
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
      integer itp1(lenptr)
      integer icr1(lenptr)
      integer isn1(lenptr)
      integer isetwd(lenptr)
      real*8 xic(lenptr)
      real*8 yic(lenptr)
      real*8 zic(lenptr)
      integer itet(lenptr)
      integer itetoff(lenptr)
      integer itettyp(lenptr)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(lenptr),jtetoff(lenptr)
 
      pointer (ipireal1,ireal1),(ipinvmpary,invmpary),
     &   (ipiparent,iparent),(ipmpary,mpary),(ipiedges,iedges),
     &   (ipiedges_first,iedges_first),(ipiedge_element,iedge_element),
     &   (ipelen,elen),(ipielist,ielist),(ipitadd,itadd),
     &   (ipieadd,ieadd),(ipiadd,iadd),(ipxadd,xadd),(ipyadd,yadd),
     &   (ipzadd,zadd),(ipvisited,visited),(ipicradd,icradd),
     &   (ipitpadd,itpadd),(ipmpary1,mpary1)
      integer invmpary(lenptr),ireal1(lenptr),iparent(lenptr),
     &   mpary(lenptr),iedges(lenptr),iedges_first(lenptr),
     &   iedge_element(lenptr),ielist(lenptr),itadd(lenptr),
     &   ieadd(lenptr),iadd(lenptr),icradd(lenptr),itpadd(lenptr),
     &   mpary1(lenptr)
      real*8 elen(lenptr),xadd(lenptr),yadd(lenptr),zadd(lenptr)
      logical visited(lenptr)
      pointer (iphxx,hxx),(iphxy,hxy),(iphxz,hxz),(iphyy,hyy),(iphyz,hyz
     &   ),(iphzz,hzz)
      real*8 hxx(*),hxy(*),hxz(*),hyy(*),hyz(*),hzz(*)
      pointer (ipelts,elts)
      integer elts(lenptr)
      pointer (ipedges,edges)
      integer edges(lenptr)
 
      character*8 cglobal, cdefault,ich1,ich2
      character*32 cmo,isubname,psetname,cfield
 
      integer mpary_in(lenptr),mpno_in,loc1,loc2,ipar1,ipar2,flag,
     &   ierror,nnodes,length,icmotype,nelements,icscode,
     &   i,j,nod1,nod2,ityp,k,nef_cmo,minpar,maxpar,lenlist,
     &   mpno,mpno_old,ierrdum,numedges,nadd,ipos,niter,
     &   ichildno,nod,ielt,iedg,ic1,ic2,inclusive,
     &   jmaxpar,jminpar,j2,nnodes_old,
     &   ierrw,len_mpno,len_nnodes,inc,len_edges,len_elist,
     &   len_elements,node,node1,mbndry,i1,i2,ityp1,ict,inclusivein,
     &   maxelt,maxedge,nelts,nrealnodes,previousmaxedge,nsd,
     &   iminpar,imaxpar,numleft
       logical itsttp
      real*8 ascend,toldamage,distmax,epsilonl,
     &   dist,dist1
 
      isubname = 'cee_chain'
      cglobal='global'
      cdefault='default'
      ich1='pset'
      ich2='get'
      ierror=0
      inclusive=inclusivein
 
c.... Initialize to zero the values of scalars used for keeping track
c.... of the lengths of dynamically managed arrays.
 
      len_mpno=0
      len_nnodes=0
      len_edges=0
      len_elist=0
      len_elements=0
      niter=0
c....
c.... Create temporary storage for elements that share an edge and
c.... their local edge numbers
c....
      call mmgetblk('elts',isubname,ipelts,100,1,icscode)
      call mmgetblk('edges',isubname,ipedges,100,1,icscode)
 
c.... We copy the input mass point array to a new array.  The new array
c.... will grow as new points are created.  Allocate
 
      len_mpno=1000+mpno_in
      call mmgetblk('mpary',isubname,ipmpary,len_mpno,1,icscode)
 
      do k=1,mpno_in
         mpary(k)=mpary_in(k)
      enddo
      mpno=mpno_in
 
c.... Outer iteration loop for creating nodes.  On each iteration of
c.... the outer loop, a set of edges that do not interfere with each
c.... other is bisected.  Since this changes the mesh object, each
c.... outer loop iteration includes getting fresh pointers for the
c.... mesh object and recalculating the relevant geometric quantities.
 
 1    continue
 
c.... Get info from mesh object.
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,
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
      call cmo_get_info('faces_per_element',cmo,nef_cmo,
     &   length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('ndimensions_topo',cmo,nsd,length,icmotype,
     &   ierror)
 
c.... Get memory for arrays that should have length NNODES.
 
      if (len_nnodes.eq.0) then
         len_nnodes=1000+nnodes
         call mmgetblk('ireal1',isubname,ipireal1,len_nnodes,1,icscode)
         call mmgetblk('invmpary',isubname,ipinvmpary,len_nnodes,1
     &      ,icscode)
         call mmgetblk('mpary1',isubname,ipmpary1,len_nnodes,1
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
         call mmincblk('mpary1',isubname,ipmpary1,inc,icscode)
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
      if(ierrdum.ne.0) call x3d_error('cee', 'unpacktp')
 
c     ..................................................................
c     find the parents of each node.
c
      call unpackpc(nnodes,itp1,isn1,iparent)
 
      call cmo_get_info('hxx',cmo,iphxx,length,icmotype,ierror)
      call cmo_get_info('hxy',cmo,iphxy,length,icmotype,ierror)
      call cmo_get_info('hxz',cmo,iphxz,length,icmotype,ierror)
      call cmo_get_info('hyy',cmo,iphyy,length,icmotype,ierror)
      call cmo_get_info('hyz',cmo,iphyz,length,icmotype,ierror)
      call cmo_get_info('hzz',cmo,iphzz,length,icmotype,ierror)
 
 
c.... change mass point array to replace child by parent nodes.
 
      do i=1,nnodes
         invmpary(i)=0
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
            endif
         endif
      enddo
 
c.... Use invmpary to store all valid nodes - if the chain takes
c.... us beyond the valid area stop and do refinement on longest
c.... edge in valid spac
c
      do i=1,nnodes
         invmpary(i)=0
      enddo
      do i=1,mpno_old
         invmpary(mpary(i))=1
      enddo
 
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
c....
c.... Build edges data structure
c.... for each node iedges_first(node) is the index into iedges.
c.... iedges then at this index and up to iedges_first(node+1)
c.... contains the second node of an edge - the second node
c.... is always numerically greater than 'node' which is the
c.... node number of the first node in the edge - hence the
c.... number of edges eminating from 'node' whose other end
c.... point is numerically greater is iedges_first(node+1)-
c.... iedges_first(node)
c.... numedges is the total number of edges.
c
      do i=1,nnodes
            if (ireal1(i).eq.1.or.itp1(i).eq.ifitpcup) then
               nrealnodes=nrealnodes+1
               mpary1(nrealnodes)=i
            endif
      enddo
      call getedges(mpary1,nrealnodes,nnodes,nelements,itet,
     &      itetoff,itettyp,iparent,isubname,inclusive,ipiedges,
     &      iedges_first)
      numedges=iedges_first(nnodes+1)-1
 
 
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
         call mmgetblk('visited',isubname,ipvisited,len_edges,1
     &      ,icscode)
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
         call mmincblk('visited',isubname,ipvisited,inc,icscode)
      endif
 
      do k=1,numedges
         iedge_element(k)=0
      enddo
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnee(ityp)
            loc1=ielmedge1(1,j,ityp)
            loc2=ielmedge1(2,j,ityp)
            ipar1=iparent(itet(loc1+itetoff(i)))
            ipar2=iparent(itet(loc2+itetoff(i)))
            minpar=min(ipar1,ipar2)
            maxpar=max(ipar1,ipar2)
            do k=iedges_first(minpar),iedges_first(minpar+1)-1
               if (maxpar.eq.iedges(k)) then
                  if (iedge_element(k).eq.0) then
                     iedge_element(k)=(i-1)*maxnee2+j
                  endif
               endif
            enddo
         enddo
      enddo
 
c.... Loop over edges and calculate edge relative error for
c.... each edges - error is the max error for edge over
c.... the elements containing the edge
 
      lenlist=0
      do i=1,nnodes
         nod1=iparent(i)
         nod2=iparent(iedges(k))
         dist=sqrt((xic(nod1)-xic(nod2))**2+
     *                (yic(nod1)-yic(nod2))**2+
     *                (zic(nod1)-zic(nod2))**2)
         if(dist.gt.epsilonl*10.d0) then
           do k=iedges_first(i),iedges_first(i+1)-1
              lenlist=lenlist+1
              ielist(lenlist)=k
              ielt=1+(iedge_element(k)-1)/maxnee2
              iedg=iedge_element(k)-maxnee2*(ielt-1)
              if(nsd.eq.3) then
                 call get_elements_on_edge(ielt,iedg,nelts,ipelts,
     *          ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *          ipitettyp,ipiparent, nef_cmo,mbndry)
              elseif(nsd.eq.2) then
                 call get_elements_on_edge2d(ielt,iedg,nelts,ipelts,
     *            ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *            ipitettyp,ipiparent, nef_cmo,mbndry)
              endif
              call edgefun_lg(nelts,ipelts,ipedges,
     &          itettyp,itet,itetoff,xic,yic,zic,
     &          hxx,hxy,hxz,hyy,hyz,hzz,elen(k))
           enddo
         endif
       enddo
c
c.... Get the worst edges
c.... Sort edges
       ascend=-1.
       call hpsort1(numedges,elen,ascend,ielist)
       if(pc_refine.ne.zero) then
          lenlist = pc_refine*numedges/100.d0
       else
          lenlist=0
          do i=1,numedges
             if(elen(i).gt.error_refine) lenlist=lenlist+1
          enddo
       endif
                
 
c.... If LENLIST is zero, no edges are to be refined, and we are done.
      if (lenlist.eq.0) goto 9999
 
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
 
      do i=1,len_edges
         visited(i)=.false.
      enddo
 
c.... Loop thru node list, and assemble a list of edges for
c.... refinement.
 
      nadd=0
      numleft=lenlist
      do while (numleft.gt.0)
      do ipos=1,lenlist
         previousmaxedge=0
         i=ielist(ipos)
         if (.not.visited(i)) then
            ielt=1+(iedge_element(i)-1)/maxnee2
            iedg=iedge_element(i)-maxnee2*(ielt-1)
50          ityp=itettyp(ielt)
            ipar1=itet(ielmedge1(1,iedg,ityp)+itetoff(ielt))
            ipar2=itet(ielmedge1(2,iedg,ityp)+itetoff(ielt))
            if(nsd.eq.3) then
               call get_elements_on_edge(ielt,iedg,nelts,ipelts,
     *        ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *        ipitettyp,ipiparent, nef_cmo,mbndry)
            elseif(nsd.eq.2) then
               call get_elements_on_edge2d(ielt,iedg,nelts,ipelts,
     *        ipedges,ipitetoff,ipjtetoff,ipitet,ipjtet,
     *        ipitettyp,ipiparent, nef_cmo,mbndry)
            endif
c.... Get length of edge
            ipar1=iparent(ipar1)
            ipar2=iparent(ipar2)
            dist=sqrt((xic(ipar1)-xic(ipar2))**2+
     *                (yic(ipar1)-yic(ipar2))**2+
     *                (zic(ipar1)-zic(ipar2))**2)
            iminpar=min(ipar1,ipar2)
            imaxpar=max(ipar1,ipar2)
c.... Find max of all neighborhood edges
            maxelt=0
            maxedge=0
            distmax=0.0
            do j=1,nelts
               ityp1=itettyp(elts(j))
               do k=1,nelmnee(ityp1)
                  if(elts(j).ne.ielt.or.iedg.ne.k) then
                     i1=itet(ielmedge1(1,k,ityp)+
     *                  itetoff(elts(j)))
                     i2=itet(ielmedge1(2,k,ityp)+
     *                  itetoff(elts(j)))
                     i1=iparent(i1)
                     i2=iparent(i2)
c.... Check if edge has already been visited
                     jminpar=min(i1,i2)
                     jmaxpar=max(i1,i2)
                     do j2=iedges_first(jminpar),
     &                 iedges_first(jminpar+1)-1
                       if (iedges(j2).eq.jmaxpar) then
                          if(visited(j2).and. j2.ne.previousmaxedge)
     *                       go to 52
                       endif
                     enddo
                     dist1=sqrt((xic(i1)-xic(i2))**2+
     *                          (yic(i1)-yic(i2))**2+
     *                          (zic(i1)-zic(i2))**2)
                     if(dist1.gt.distmax) then
                        distmax=dist1
                        maxelt=elts(j)
                        maxedge=k

                     endif
                  endif
 52               continue
               enddo
            enddo
c.... See if candidate edge > longest in neighborhood
 55         if(distmax.gt.(1.0001*dist)) then
               ielt=maxelt
               iedg=maxedge
               go to 50
            else
c.... Add this edge to refinement list
c.... Mark this edge as done - skip it hence forth
               do j2=iedges_first(iminpar),
     &            iedges_first(iminpar+1)-1
                  if (iedges(j2).eq.imaxpar) then
                      visited(j2)=.true.
                      previousmaxedge=j2
                  endif
               enddo
               nadd=nadd+1
               if (len_elist.le.nadd) then
                  inc=1000+nadd-len_elist
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
 

            endif
         endif
  60     continue
      enddo
c
c.... check if any edges left
c
      numleft=0
      do i=1,lenlist
        if(.not.visited(ielist(i))) numleft=numleft+1
      enddo
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
c
c check if node type is not interface then reset isn1
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
                           write(logmess,'("Bad parent/child chain ",
     *                      "in cee_chain")')
                           call writloga('default',0,logmess,0,icscode)
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
      if (psetname(1:5).eq.'-def-'.or.psetname.eq.' ') then
         mpno_old=mpno
         mpno=mpno+nnodes-nnodes_old
         if (len_mpno.le.mpno) then
            inc=1000+mpno-len_mpno
            len_mpno=len_mpno+inc
            call mmincblk('mpary',isubname,ipmpary,inc,icscode)
         endif
         do j=mpno_old+1,mpno
            mpary(j)=j-mpno_old+nnodes_old
         enddo
      else
         mpno_old=mpno
         if (len_mpno.le.nnodes) then
            inc=nnodes-len_mpno
            len_mpno=nnodes
            call mmincblk('mpary',isubname,ipmpary,inc,icscode)
         endif
         call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
         call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierror)
         call pntlimc(ich1,ich2,psetname,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif
      niter=niter+1
 
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
