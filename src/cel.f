*dk,cel
      subroutine cel(cmo,tollength,toldamage,mpary_in,mpno_in,
     *   inclusive,psetname,ierror)
c
c #####################################################################
c
c    PURPOSE
c
c       CEL ("Create on Edge Length")
c     takes a mesh object and bisects edges that
c     (i) have both endpoints in the list of selected mass points, and
c     (ii) have length greater than TOLLENGTH.
c
c     The process is recursive in that new nodes are added to the
c     list of mass points, meaning that newly created edges can
c     be refined until all the edges in the mesh have length
c     less than TOLLENGTH.  Also, all edge refinement obeys
c     the 'Rivara Principle' which is that a refined edge should
c     be the longest edge in ANY element that contains it.  This
c     leads to recursive refinement with nondegrading element
c     aspect ratios.
c
c    INPUT ARGUMENTS -
c
c       CMO - name of current mesh object
c       TOLLENGTH - maximum allowable length of edges
c       TOLDAMAGE - max. damage (used by recon)
c       MPARY_IN - array of mass points
c       MPNO_IN - no. of mass points
C       INCLUSIVE - 1 means inclusive - edge is a refine candidate
C                    if either node is in pset,
C                  - 0 means exclusive - edge is a refine candidate
C                    if both nodes are in pset
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
c
c $Log: cel.f,v $
c Revision 2.00  2007/11/05 19:45:47  spchu
c Import to CVS
c
CPVCS    
CPVCS       Rev 1.16   07 Jan 2002 14:03:30   dcg
CPVCS    add error return argument to refine_edge_add_tet call
CPVCS    stop iterating if refine returns an error
CPVCS
CPVCS       Rev 1.15   Mon Nov 16 10:47:02 1998   kuprat
CPVCS    Removed unused variable declaration.
CPVCS
CPVCS       Rev 1.14   Mon Nov 16 10:41:36 1998   kuprat
CPVCS    Commented out call to 'recon'.
CPVCS
CPVCS       Rev 1.13   Wed Sep 23 16:59:54 1998   kuprat
CPVCS    Turned on version control logging by correcting 'LOG' typo.
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
     &   (ipzadd,zadd),(iplvisited,lvisited),(ipicradd,icradd),
     &   (ipitpadd,itpadd),(ipipset,ipset)
      integer invmpary(lenptr),ireal1(lenptr),iparent(lenptr),
     &   mpary(lenptr),iedges(lenptr),iedges_first(lenptr),
     &   iedge_element(lenptr),ielist(lenptr),itadd(lenptr),
     &   ieadd(lenptr),iadd(lenptr),icradd(lenptr),itpadd(lenptr)
      real*8 elen(lenptr),xadd(lenptr),yadd(lenptr),zadd(lenptr),
     &   ipset(lenptr)
      logical lvisited(lenptr)
 
      pointer (iplskip,lskip)
      logical lskip(lenptr)
 
      integer maxelt
      parameter (maxelt=50)
      integer ieltlist(maxelt)
 
      character*8 cglobal, cdefault, ich1,ich2
      character*32 cmo,isubname, psetname
c$$$      character*132 cbuf
 
      integer mpary_in(lenptr),mpno_in,loc1,loc2,ipar1,ipar2,flag,
     &   ierror,nnodes,length,icmotype,nelements,mbndry,icscode,
     &   i,j,nod1,ityp,k,j1,nef_cmo,minpar,maxpar,lenlist,
     &   mpno,mpno_old,ierrdum,numedges,nod2,nadd,ipos,
     &   ichildno,nod,ielt,iedg,ic1,ic2,inclusive,
     &   leneltlist,iter,icurrelt,icurredge,jtetj,nextelt,jedg,
     &   jc1,jc2,jpar1,jpar2,jmaxpar,jminpar,jelt,j2,nnodes_old,
     &   ierrw,len_mpno,len_nnodes,inc,len_edges,len_elist,
     &   len_elements,node,node1
 
      real*8 ascend,tollength,rlong,rlen,toldamage
 
      isubname = 'cel'
      cglobal='global'
      cdefault='default'
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
 
c     ..................................................................
c     find the parents of each node.
c
      call unpackpc(nnodes,itp1,isn1,iparent)
 
c.... change mass point array to contain only parent nodes.
 
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
 
c.... Obtain mpnode-mpnode relation.  For a given mass point node
c.... ("mpnode") that appears in the list of mass points MPARY,
c.... the mpnode-mpnode relation is a list of neighbouring mpnodes.
c.... This is thus the list of "exclusive" edges
c.... incident on the set of mass points.  (The "inclusive" edges
c.... would not require that BOTH endpoints of the edge reside
c.... in MPARY.)  It is here assumed that in the case of
c.... parent-child points, only the parent appears in MPARY.
 
      call getedges(mpary,mpno,nnodes,nelements,itet,itetoff,
     &   itettyp,iparent,isubname,inclusive,ipiedges,iedges_first)
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
         call mmgetblk('iedge_element',isubname,ipiedge_element
     &      ,len_edges,1,icscode)
         call mmgetblk('elen',isubname,ipelen,len_edges,2,icscode)
         call mmgetblk('lskip',isubname,iplskip,len_edges,1,icscode)
         call mmgetblk('ielist',isubname,ipielist,len_edges,1,icscode)
      elseif (len_edges.le.numedges) then
         inc=1000+numedges-len_edges
         len_edges=len_edges+inc
         call mmincblk('iedge_element',isubname,ipiedge_element,inc
     &      ,icscode)
         call mmincblk('elen',isubname,ipelen,inc,icscode)
         call mmincblk('lskip',isubname,iplskip,inc,icscode)
         call mmincblk('ielist',isubname,ipielist,inc,icscode)
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
 
c.... Loop over IEDGES and compute ELEN, the length of the edge.
c.... If ELEN > TOLLENGTH we place the edge in IELIST.
 
      lenlist=0
      do i=1,nnodes
         nod1=i
         do k=iedges_first(i),iedges_first(i+1)-1
            nod2=iedges(k)
            elen(k)=sqrt((xic(nod1)-xic(nod2))**2+
     &         (yic(nod1)-yic(nod2))**2+
     &         (zic(nod1)-zic(nod2))**2)
            if (elen(k).gt.tollength) then
               lenlist=lenlist+1
               ielist(lenlist)=k
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
 
c.... We set the array LSKIP to .false.  Whenever we bisect
c.... an edge we set the LSKIP value to .true. for all edges
c.... that are 'adjacent' to the bisected edge in the sense
c.... that they share a mutual element.
c.... In this way, by only bisecting edges with a
c.... .false. LSKIP value, we insure that the bisections
c.... do not interfere with each other.
 
      do i=1,numedges
         lskip(i)=.false.
      enddo
 
c.... Allocate memory for 'add' arrays.
 
      if (len_elist.eq.0) then
         len_elist=1000+lenlist
         call mmgetblk('itadd',isubname,ipitadd,len_elist,1,icscode)
         call mmgetblk('ieadd',isubname,ipieadd,len_elist,1,icscode)
         call mmgetblk('iadd',isubname,ipiadd,len_elist,1,icscode)
         call mmgetblk('itpadd',isubname,ipitpadd,len_elist,1,icscode)
         call mmgetblk('icradd',isubname,ipicradd,len_elist,1,icscode)
         call mmgetblk('ipset',isubname,ipipset,len_elist,1,icscode)
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
         call mmincblk('ipset',isubname,ipipset,inc,icscode)
         call mmincblk('xadd',isubname,ipxadd,inc,icscode)
         call mmincblk('yadd',isubname,ipyadd,inc,icscode)
         call mmincblk('zadd',isubname,ipzadd,inc,icscode)
      endif
      if (len_elements.eq.0) then
         len_elements=1000+nelements
         call mmgetblk('lvisited',isubname,iplvisited,len_elements,1
     &      ,icscode)
      elseif (len_elements.le.nelements) then
         inc=1000+nelements-len_elements
         len_elements=len_elements+inc
         call mmincblk('lvisited',isubname,iplvisited,inc,icscode)
      endif
 
      do i=1,nelements
         lvisited(i)=.false.
      enddo
 
c.... Loop thru element list, and set to .true. the LSKIP value for each
c.... which is not the longest edge in that element.
 
      do i=1,nelements
         ityp=itettyp(i)
         rlong=0.d0
         do j=1,nelmnee(ityp)
            loc1=ielmedge1(1,j,ityp)
            loc2=ielmedge1(2,j,ityp)
            ipar1=iparent(itet(loc1+itetoff(i)))
            ipar2=iparent(itet(loc2+itetoff(i)))
            rlong=max(rlong,sqrt((xic(ipar1)-xic(ipar2))**2+
     &         (yic(ipar1)-yic(ipar2))**2+
     &         (zic(ipar1)-zic(ipar2))**2))
         enddo
         do j=1,nelmnee(ityp)
            loc1=ielmedge1(1,j,ityp)
            loc2=ielmedge1(2,j,ityp)
            ipar1=iparent(itet(loc1+itetoff(i)))
            ipar2=iparent(itet(loc2+itetoff(i)))
            rlen=sqrt((xic(ipar1)-xic(ipar2))**2+
     &         (yic(ipar1)-yic(ipar2))**2+
     &         (zic(ipar1)-zic(ipar2))**2)
            if (rlen.lt.rlong) then
               minpar=min(ipar1,ipar2)
               maxpar=max(ipar1,ipar2)
               do k=iedges_first(minpar),iedges_first(minpar+1)-1
                  if (maxpar.eq.iedges(k)) then
                     lskip(k)=.true.
                  endif
               enddo
            endif
         enddo
      enddo
 
c.... Loop thru node list, and assemble a list of edges for
c.... refinement.
 
      nadd=0
      do ipos=1,lenlist
         i=ielist(ipos)
         if (.not.lskip(i)) then
            ielt=1+(iedge_element(i)-1)/maxnee2
            iedg=iedge_element(i)-maxnee2*(ielt-1)
            ityp=itettyp(ielt)
 
            nadd=nadd+1
            itadd(nadd)=ielt
            ieadd(nadd)=iedg
            iadd(nadd)=0
            ic1=itet(ielmedge1(1,iedg,ityp)+itetoff(ielt))
            ic2=itet(ielmedge1(2,iedg,ityp)+itetoff(ielt))
            ipset(nadd)=iand(isetwd(ic1),isetwd(ic2))
            ipar1=iparent(ic1)
            ipar2=iparent(ic2)
            minpar=min(ipar1,ipar2)
            maxpar=max(ipar1,ipar2)
            xadd(nadd)=half*(xic(ic1)+xic(ic2))
            yadd(nadd)=half*(yic(ic1)+yic(ic2))
            zadd(nadd)=half*(zic(ic1)+zic(ic2))
            if (idebug.ge.1) then
               write(logmess,'(i5,a,1x,1pe20.13)') ipos,': dist=',
     &                sqrt((xic(ic1)-xic(ic2))**2+
     &                (yic(ic1)-yic(ic2))**2+
     &                (zic(ic1)-zic(ic2))**2)
               call writloga('default',0,logmess,0,ierrw)
            endif
 
c.... Loop around edge to compile list of elements sharing that edge.
c.... Outer loop has length of two, signifying that we might have to
c.... change direction looping around the edge if we encounter an
c.... outer boundary.
 
            leneltlist=1
            ieltlist(1)=ielt
            lvisited(ielt)=.true.
 
            do iter=1,2
               icurrelt=ielt
               icurredge=iedg
 500           continue
               do 510 j=1,nelmnef(itettyp(icurrelt))
                  do j1=1,ielmface0(j,itettyp(icurrelt))
                     if (ielmface2(j1,j,itettyp(icurrelt)).eq
     &                  .icurredge) then
                        jtetj=jtet(j+jtetoff(icurrelt))
                        if (jtetj.eq.mbndry) then
                           goto 510
                        elseif (jtetj.gt.mbndry) then
                           nextelt=1+(jtetj-mbndry-1)/nef_cmo
                        else
                           nextelt=1+(jtetj-1)/nef_cmo
                        endif
                        if (.not.lvisited(nextelt)) then
                           lvisited(nextelt)=.true.
                           leneltlist=leneltlist+1
                           if (leneltlist.gt.maxelt) then
                              write(logmess,'(a)') 'More than ', maxelt,
     &                           'elements sharing an edge!'
                              call writloga('default',0,logmess,0,ierrw)
                              stop
                           endif
                           icurrelt=nextelt
                           ieltlist(leneltlist)=nextelt
                           do jedg=1,nelmnee(itettyp(icurrelt))
                              jc1=itet(
     &                           ielmedge1(1,jedg,itettyp(icurrelt))
     &                           +itetoff(icurrelt))
                              jc2=itet(
     &                           ielmedge1(2,jedg,itettyp(icurrelt))
     &                           +itetoff(icurrelt))
                              jpar1=iparent(jc1)
                              jpar2=iparent(jc2)
                              jmaxpar=max(jpar1,jpar2)
                              jminpar=min(jpar1,jpar2)
                              if (jminpar.eq.minpar.and.jmaxpar.eq
     &                           .maxpar) then
                                 icurredge=jedg
                                 goto 500
                              endif
                           enddo
                           write(logmess,'(a)') 'CEL:Topological error'
                           call writloga('default',0,logmess,0,ierrw)
                           write(logmess,'(a,2i8)') 'minpar/maxpar=',
     &                        minpar,maxpar
                           call writloga('default',0,logmess,0,ierrw)
                           write(logmess,'(a,i8)')
     &                        'cannot match element ',icurrelt
                           call writloga('default',0,logmess,0,ierrw)
                           stop
                        endif
                     endif
                  enddo
 510           continue
            enddo
 
c.... Now go thru the list of elements that contain the refined edge and
c.... give all edges appearing in the refinement list that are part of
c.... those elements a LSKIP value of .true.  In this way, we will avoid
c.... conflicting refinements.
 
            do j=1,leneltlist
               jelt=ieltlist(j)
 
c.... Clear LVISITED(JELT).  After this loop is completed, LVISITED will
c.... have all elements false and will be ready for re-use.
 
               lvisited(jelt)=.false.
 
               do j1=1,nelmnee(itettyp(jelt))
                  jc1=itet(ielmedge1(1,j1,itettyp(jelt))+
     &               itetoff(jelt))
                  jc2=itet(ielmedge1(2,j1,itettyp(jelt))+
     &               itetoff(jelt))
                  jpar1=iparent(jc1)
                  jpar2=iparent(jc2)
                  jmaxpar=max(jpar1,jpar2)
                  jminpar=min(jpar1,jpar2)
                  do j2=iedges_first(jminpar),
     &               iedges_first(jminpar+1)-1
                     if (iedges(j2).eq.jmaxpar) then
                        lskip(j2)=.true.
                     endif
                  enddo
               enddo
            enddo
         endif
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
      call refine_fix_add(cmo,nadd,ipitadd,ipieadd,ipiadd,
     &   ipitpadd,ipicradd)
      call refine_edge_add_tet(cmo,nadd,ipitadd,ipieadd,
     &   iadd,xadd,yadd,zadd,flag)
 
c.... Fix up ITP1, ICR1 values.
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,
     *   ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,
     *   ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,
     *   ipisn1,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *   ipisetwd,length,icmotype,ierror)
      do i = 1,nadd
         if (iadd(i).gt.0) then
            node=iadd(i)
            isetwd(node)=ipset(i)
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
C
c$$$      write(cbuf,'(a,e16.8,a)')'recon/0/',toldamage
c$$$     &   ,'/ ; finish'
c$$$      call dotaskx3d(cbuf,ierr)
c$$$C
c$$$      call cmo_get_info('nnodes',cmo,
c$$$     *   nnodes,length,icmotype,ierror)
c$$$C
      if (psetname(1:5).eq.'-def-') then
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
         if (len_mpno.le.mpno+nadd) then
            inc=1000+mpno+nadd-len_mpno
            len_mpno=len_mpno+inc
            call mmincblk('mpary',isubname,ipmpary,inc,icscode)
         endif
         call pntlimc(ich1,ich2,psetname,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif
      if(flag.ne.0) go to 9999
      goto 1
 
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
 
