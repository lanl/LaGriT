*dk,nbrsmooth
c -----------------------------------------------------------------
 
	subroutine nbrsmooth(cmo,mpary,mpno,rlxwt,ntimes,nwttry,useisn
     &  ,extnbr)
 
C -----------------------------------------------------------------
C
C  PURPOSE
C Smooth by moving nodes towards average position of neighbors.
C Limit definition of neighbor to those points connected by an edge
C  of an element to a node with a 'subset' of contraints/type
C  on the node of those on the neighbor and edge.
C Optionally check that the materials on the node are also a subset
C  of those on the neighbor and connecting edge.
C
C constraints:
c (1) above neighbor constraints
C (2) resultant element volumes must be positive
C
C caveats:
C (1) limited to tets for now
C (2) repeated iterations flatten curved surfaces, and shrink spheres
C (3) constaints do not know about analytic shapes
C
C
C  INPUT
C
C  cmo    name of mesh object
C  mpary  array of nodes numbers to smooth
C  mpno   number of nodes to smooth
C  rlxwt  weight for under-relaxed Laplacian smoothing
C         [recommended value: 0.5]
C  ntimes number of smoothing iterations
C         [recommended value: 1-5]
C  nwttry number of attempts to not tangle mesh by halving the
C         smoothing weights
C         [recommended value: 1-5]
C  useisn flag  0  ignore parent/child chains
C                  (interface node positions smoothed
C                   based on all interface neighbors)
C               1  use parent/child relationship
C                  (interface node positions smoothed
C                   based on only those neighbors
C                   who are connected along multimaterial edges
C                   with all the materials of the node)
C         [recommended value: 1; unlisted values same as 1]
C  extnbr flag  0  neighbors must be in mpary array
C               1  neighbors can be not in mpary array
C         [recommended value: 1; unlisted values same as 1]
C
C Rule for under-relaxed Laplacian smoothing is
C     x -> (1-rlxwt)*x + rlxwt*[ av_neighbor_position ]
C If a volume is inverted, I modify this to try nwttry times to
C reduce the smoothing weight of the nodes defining this volume by
C     rlxwt -> local_rlxwt*rlxwt,
C where local_rlxwt=rlxwt unless c rlxwt>1 or rlxwt<0,
C in which case local_rlxwt is set to 0.5 to avoid iterations blowing up.
C If locally reducing the weight fails to keep volumes positive
C after ntimes tries, I set the smoothing weight to zero on nodes
C defining inverted tets, and if this fails the smoothing is aborted.
C
C      CHANGE HISTORY -
C
C $Log: nbrsmooth.f,v $
C Revision 2.00  2007/11/05 19:46:02  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   14 Dec 2000 14:56:22   jtg
CPVCS    fixed error occuring when no neighbors
CPVCS    
CPVCS       Rev 1.4   26 Jan 2000 16:57:24   jtg
CPVCS    now accepts mbndry=0
CPVCS    
CPVCS       Rev 1.3   Mon Dec 21 18:51:22 1998   jtg
CPVCS    dfloat to changed to dble
CPVCS    
CPVCS       Rev 1.2   Fri Oct 23 13:08:30 1998   dcg
CPVCS    replace lenptr with '*' - DEC compiler complaint
CPVCS
CPVCS       Rev 1.1   Wed Oct 21 01:09:52 1998   jtg
CPVCS    forgot to add PVCS log - corrected
c -----------------------------------------------------------------
c23456789012345678901234567890123456789012345678901234567890123456789012
 
      implicit none
 
      include 'local_element.h'
 
      include 'chydro.h'	
      integer mpno
      integer mpary(mpno)
 
      integer nwttry,useisn,extnbr
      real*8 rlxwt
 
      pointer (ip_itet,itet),(ip_imt1,imt1),(ip_itp1,itp1)
     &  ,(ip_isn1,isn1),(ip_icr1,icr1),(ip_icontab,icontab)
     &  ,(ip_iparent,iparent),(ip_jtet,jtet),(ip_itettyp,itettyp)
      integer itet(4,*),imt1(*),itp1(*)
     &  ,isn1(*),icr1(*),icontab(50,*)
     &  ,iparent(*),jtet(4,*),itettyp(*)
c
      pointer (ip_xic,xic),(ip_yic,yic),(ip_zic,zic)
     &  ,(ip_dx,dx),(ip_dy,dy),(ip_dz,dz),(ip_weight,weight)
      real*8 xic(*),yic(*),zic(*)
     &  ,dx(*),dy(*),dz(*),weight(*)
c
      pointer (ip_nbr1,nbr1),(ip_nbor,nbor),(ip_link,link)
     &  ,(ip_neigh,neigh),(ip_neigh_off,neigh_off)
     &  ,(ip_neigh_n,neigh_n)
      integer nbr1(*),nbor(*),link(*)
     &  ,neigh(*),neigh_off(*)
     &  ,neigh_n(*)
c
      integer ntimes,itime,ii
     &  ,nnodes,nelements,nconbnd,mbndry
     &  ,length,itype,ierr,pos_vol,nwt,n_nbrs,length_add
     &  ,i,j,k,iel,i1(2),i2(2)
     &  ,ip1,ip2,ic1,ic2,jt1,jt2,is1,is2
c
      pointer (ip_child_nbr1,child_nbr1),(ip_child_nbor,child_nbor)
     &  ,(ip_child_link,child_link)
      integer child_nbr1(*),child_nbor(*),child_link(*)
      integer child_n_nbrs,child_length,n_dud
C
      real*8 xxx,rrr,x1(4),y1(4),z1(4),vvv,local_rlxwt
      character*32 cmo,isubname
      character*132 logmess
      real*8 zero,one,half,one_sixth
c
      zero=0.0d0
      one=1.0d0
      one_sixth=1.d0/6.0d0
      half=0.5d0
 
c -----------------------------------------------------------------
c -----------------------------------------------------------------
 
      isubname='nbrsmooth'
 
      call cmo_get_info('nnodes',cmo,nnodes,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info nnodes'
      call cmo_get_info('nelements',cmo,nelements,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info nelements'
      call cmo_get_info('itp1',cmo,ip_itp1,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info itp1'
      call cmo_get_info('imt1',cmo,ip_imt1,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info imt1'
      call cmo_get_info('isn1',cmo,ip_isn1,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info isn1'
      call cmo_get_info('icr1',cmo,ip_icr1,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info icr1'
      call cmo_get_info('itet',cmo,ip_itet,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info itet'
      call cmo_get_info('jtet',cmo,ip_jtet,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info jtet'
      call cmo_get_info('itettyp',cmo,ip_itettyp,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info itettyp'
      call cmo_get_info('xic',cmo,ip_xic,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info xic'
      call cmo_get_info('yic',cmo,ip_yic,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info yic'
      call cmo_get_info('zic',cmo,ip_zic,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info zic'
      call cmo_get_info('mbndry',cmo,mbndry,length,itype,ierr)
      if (ierr.ne.0) mbndry=0
      call cmo_get_info('nconbnd',cmo,nconbnd,length,itype,ierr)
      if (ierr.ne.0) then
	 nconbnd=0
      elseif (nconbnd.gt.0) then
	 call cmo_get_info('icontab',cmo,ip_icontab,length,itype,ierr)
	 if (ierr.ne.0) stop 'NBRSMOOTH: cmo_get_info icontab'
      endif
      itype=1
      length=nnodes
c   (use neigh_n to hold temp info...)
      call mmggetbk('neigh_n',isubname,ip_neigh_n,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk neigh_n'
      itype=2
      length=nnodes
      call mmggetbk('dx',isubname,ip_dx,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk dx'
      call mmggetbk('dy',isubname,ip_dy,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk dy'
      call mmggetbk('dz',isubname,ip_dz,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk dz'
      itype=2
      length=nnodes
      call mmggetbk('weight',isubname,ip_weight,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk weight'
      itype=1
      length=nnodes
      call mmggetbk("iparent",isubname,ip_iparent,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk iparent'
      call unpackpc(nnodes,itp1,isn1,iparent)
c
      itype=1
      length=nnodes
      call mmggetbk('nbr1',isubname,ip_nbr1,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk nbr1'
      if (useisn.ne.0) then
         call mmggetbk('child_nbr1',isubname,ip_child_nbr1
     &    ,length,itype,ierr)
         if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk child_nbr1'
      endif
      length=24*mpno+1000
      call mmggetbk('link',isubname,ip_link,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk link'
      call mmggetbk('nbor',isubname,ip_nbor,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk nbor'
      if (useisn.ne.0) then
         child_length=length
         call mmggetbk('child_link',isubname,ip_child_link
     &		,child_length,itype,ierr)
         if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk child_link'
         call mmggetbk('child_nbor',isubname,ip_child_nbor
     &		,child_length,itype,ierr)
         if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk child_nbor'
      else
          child_length=100
c   bigger than 24 so don't try to incr length..
      endif
 
c--------------------------------------------------------------------
c	 ........... (create neighbor table) .............
c
c initialize arrays for neighbor table
c nbr1 is "masked"
      do i=1,nnodes
	 nbr1(i)=-1
      enddo
      n_nbrs=0
      if (useisn.ne.0) then
         do i=1,nnodes
            child_nbr1(i)=0
         enddo
      endif
      child_n_nbrs=0
c
c unmask nbr1 for parents of points in mpary
      do ii=1,mpno
         i=iparent(mpary(ii))
         nbr1(i)=0
      enddo
c
      length_add=mpno+1000
c
c loop through elements to fill in neighbor table
      do iel=1,nelements
         if(itettyp(iel).ne.ifelmtet) then
            write(logmess,'(a)')
     *     'laplace smoothing only implemented on 3D tet meshes'
            call writloga('default',0,logmess,0,ierr)
            go to 9999
          endif
c
c  increment arrays if necessary
         if (n_nbrs+24.gt.length) then
            length=length+length_add
            call mmggetbk('link',isubname,ip_link,length,itype,ierr)
            if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk link'
            call mmggetbk('nbor',isubname,ip_nbor,length,itype,ierr)
            if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk nbor'
         endif
         if (child_n_nbrs+24.gt.child_length) then
            child_length=child_length+length_add
            call mmggetbk('child_link',isubname,ip_child_link
     &			,child_length,itype,ierr)
            if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk child_link'
            call mmggetbk('child_nbor',isubname,ip_child_nbor
     &			,child_length,itype,ierr)
            if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk child_nbor'
         endif
c
C  loop through edges of element iel
c   define endpoints (i1) and connected tets (i2)
c   at some point this should be generalized from "elmedge/elmface"...
c   as this is the only part of the code which is specific to tets
c   except dimension for itet,jtet assumed (4,nelements)
c   rather than using itetoff,etc
c   and formula for volume in volume check
         do k=1,6
            if (k.ge.1.and.k.le.4) then
               i1(1)=k
               i1(2)=1+mod(k,4)
               i2(1)=1+mod(k+1,4)
               i2(2)=1+mod(k+2,4)
            elseif (k.eq.5) then
               i1(1)=1
               i1(2)=3
               i2(1)=2
               i2(2)=4
c  if (k.eq.6) then
            else
               i1(1)=2
               i1(2)=4
               i2(1)=1
               i2(2)=3
            endif
            do i=1,2
               i1(i)=itet(i1(i),iel)
               i2(i)=jtet(i2(i),iel)
            enddo
 
c   loop through endpoints of edge k of element iel
            do i=1,2
c
c    check if OK re mask
c     (only unmasked nodes have neighbors)
               ip1=iparent(i1(i))
               ip2=iparent(i1(3-i))
               if (nbr1(ip1).lt.0) goto 250 ! not OK
               if (nbr1(ip2).lt.0 .and. extnbr.eq.0) goto 250 ! not OK
c
c    check if OK re itp
c     (point type of (child) node must be "lower" than neighbor)
c     Note: should switch these tests to proper "ifitp..." from chydro.h
               ip1=itp1(i1(i))
               ip2=itp1(i1(3-i))
               if (ip1.eq.0 .or. ip1.eq.ip2) then
                  continue
               elseif (ip1.eq.2
     &			.and.(ip2.eq.12.or.ip2.eq.13.or.ip2.eq.15)) then
                  continue
               elseif ((ip1.eq.10.or.ip1.eq.11)
     &			.and.(ip2.eq.12.or.ip2.eq.13)) then
                  continue
               elseif ((ip1.eq.12.or.ip1.eq.13).and.ip2.eq.15) then
                  continue
               elseif (ip1.eq.14.and.ip2.eq.15) then
                  continue
               else
c  not OK
                  goto 250
               endif
 
c    check if OK re jtet
c     (interface nodes only neighbors if edge is interface edge)
               if (ip1.ne.0) then ! ip1 is still point type
                  jt1=i2(1)
                  jt2=i2(2)
                  if ((jt1.lt.mbndry.and.jt2.lt.mbndry.and.mbndry.gt.0)
     &               .or.(jt1.gt.0.and.jt2.gt.0.and.mbndry.eq.0) ) then
                     goto 250
                  endif
               endif
 
c    check if OK re icr values
c     (constraints of node must be subset of constraints of neighbor)
c     Note: similar to material check, do I need to check that the
c     "bond constraints" also same or higher order? Or is the jtet check
c     that it's an interface bond enough? Answer is I probably should
c     check, but I don't know what the appropriate bond or face-based
c     variable to check the "bond constraint" is. -> DCG??
c     Probably in most cases the "useisn" check for "bond materials"
c     would insure the "bond constraints" are also consistent.
c     (Fails only if have interface bond connecting points on same
c      constraint surface with the bond not in the constraint surface,
c      and with the useisn flag on, the interface the bond lies on would
c      have to have all the same materials as the constraint surface.
c      A virtual interface on a curved surface between different
c      boundary conditions in the same material is an example requiring
c      one to check if the "bond constraints" are consistent
c      with the node constraints to properly assign neighbors and
c      avoid a "2nd neighbor" connection between points on the virtual
c      interface through either constraint surface.)
               ic1=icr1(i1(i))
               ic2=icr1(i1(3-i))
               if (ic1.eq.0 .or. ic1.eq.ic2) then
                  continue
               elseif (nconbnd.gt.0.and.ic1.gt.0.and.ic2.gt.0) then
                  do is1=1,icontab(1,ic1)
                     j=icontab(2+is1,ic1)
                     do is2=1,icontab(1,ic2)
                        if (icontab(2+is2,ic2).eq.j) goto 210
                     enddo
                     goto 250
210                  continue
                  enddo
               else
                  goto 250
               endif
 
c    if useisn flag on, check if OK re isn
c     (only neighbors if node materials subset of neighbor materials;
c     bond material check performed afterwards, using "child neighbors")
               if (useisn.ne.0) then
                  ip1=iparent(i1(i))
                  ip2=iparent(i1(3-i))
                  if (isn1(ip1).eq.0) then
                     continue
                  elseif (isn1(ip2).eq.0) then
                     goto 250
                  else
                     is1=isn1(ip1)
                     do while (is1.ne.ip1)
                        is2=isn1(ip2)
                        do while (is2.ne.ip2)
                           if (imt1(is1).eq.imt1(is2)) goto 220
                           is2=isn1(is2)
                        enddo
                        goto 250
220                     is1=isn1(is1)
                     enddo
                  endif
               endif
 
c    all checks OK: add i1(3-i) parent position to i1(i) parent neighbor table
               ip1=iparent(i1(i))
               ip2=iparent(i1(3-i))
               if (nbr1(ip1).eq.0) then
                  n_nbrs=n_nbrs+1
                  nbr1(ip1)=n_nbrs
                  nbor(n_nbrs)=ip2
                  link(n_nbrs)=0
c nbr1>0 as check would have failed if <0
               else
                  j=nbr1(ip1)
                  if (nbor(j).eq.ip2) goto 230
                  do while(link(j).ne.0)
                     j=link(j)
                     if (nbor(j).eq.ip2) goto 230
                  enddo
                  n_nbrs=n_nbrs+1
                  nbor(n_nbrs)=ip2
                  link(j)=n_nbrs
                  link(n_nbrs)=0
230               continue
               endif
 
c    if useisn flag on,
c    also add i1(3-i) child position to i1(i) child neighbor table
c    Note: ONLY for multimatl-multimatl connections
c     are child neighbors necessary to do multimatl edge check
              if (useisn.ne.0.and.isn1(ip1).gt.0) then
                  ip1=i1(i)
                  ip2=i1(3-i)
                  if (child_nbr1(ip1).eq.0) then
                     child_n_nbrs=child_n_nbrs+1
                     child_nbr1(ip1)=child_n_nbrs
                     child_nbor(child_n_nbrs)=ip2
                     child_link(child_n_nbrs)=0
                  else
                     j=child_nbr1(ip1)
                     if (child_nbor(j).eq.ip2) goto 240
                     do while(child_link(j).ne.0)
                        j=child_link(j)
                        if (child_nbor(j).eq.ip2) goto 240
                     enddo
                     child_n_nbrs=child_n_nbrs+1
                     child_nbor(child_n_nbrs)=ip2
                     child_link(j)=child_n_nbrs
                     child_link(child_n_nbrs)=0
240                  continue
                  endif
               endif
 
250            continue
 
C    end loop on endpoints
            enddo
 
c   end loop on edges
         enddo
 
c  end loop on elements
      enddo
 
c  if useisn flag on,
c  need to recheck multimatl-multimatl connections re edge materials
      if (useisn.ne.0) then
         n_dud=0
         do ip1=1,nnodes
            if (isn1(ip1).gt.0) then
               k=nbr1(ip1)
               is1=0
               do while (k.gt.0)
c                   isn1(ip2) must also be >0 if no error above
c                   => multimatl-multimatl connection
c                   => check that for every child of ip1,
c                   there is a child_nbor whose parent is ip2;
c                   and if not, remove from parent nbor table
                  ip2=nbor(k)
                  i=isn1(ip1)
                  do while (i.ne.ip1)
                     j=child_nbr1(i)
                     do while (j.ne.0)
                        if (iparent(child_nbor(j)).eq.ip2) goto 205
                        j=child_link(j)
                     enddo
                     n_dud=n_dud+1
                     k=link(k)
                     if (is1.eq.0) then
                        nbr1(ip1)=k
                     else
                        link(is1)=k
                     endif
                     goto 206
205                  continue
                     i=isn1(i)
c  end loop on isn1 chain of ip1
                  enddo
                  is1=k
                  k=link(k)
206               continue
               enddo
            endif
         enddo
         n_nbrs=n_nbrs-n_dud
      else
         n_dud=0
      endif
c
 
c redo linked list nbor into ordered list neigh:
c  actually could use nbor table and link, but create neigh table
c  as I wanted to look at for debugging: might eventually want to remove
c  to make more efficient?
      itype=1
      length=n_nbrs+1
      call mmggetbk('neigh',isubname,ip_neigh,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk neigh'
      itype=1
      length=nnodes
      call mmggetbk('neigh_off',isubname,ip_neigh_off
     &		,length,itype,ierr)
      if (ierr.ne.0) stop 'NBRSMOOTH: mmggetbk neigh_off'
 
      j=0
      do i=1,nnodes
         neigh_n(i)=0
         neigh_off(i)=j
         k=nbr1(i)
         do while (k.gt.0)
            j=j+1
            neigh_n(i)=neigh_n(i)+1
            neigh(j)=nbor(k)
            k=link(k)
        enddo
      enddo
 
c--------------------------------------------------------------------
ccc
cccc ........... (test nbr groups) .............
ccc
ccc	is1=0 ! so not used, but keep code here for debugging -> switch to idebug?
ccc
ccc	if (is1.gt.0) then
ccc
ccc	if (useisn.ne.0) write(*,*) 'removed ',n_dud,' nbrs as bad bond color'
ccc
ccc	is2=0
ccc	do i=1,nnodes
ccc	  if (neigh_n(i).gt.is2) is2=neigh_n(i)
ccc	enddo
ccc	do j=1,is2
ccc	 k=0
ccc	 do i=1,nnodes
ccc	  if (neigh_n(i).eq.j) k=k+1
ccc	 enddo
ccc	 if (k.gt.0) then
ccc	  write(*,'(a,i5,a,i8)') '# sites w ',j,' nbrs: ',k
ccc	 endif
ccc	enddo
ccc
cccc  just count number in isn1 chain vs "burn"??
cccc  also, should make attribute so can view,
cccc  and need to add neigh_type back to declarations
ccc	do i=1,nnodes
ccc	  neigh_type(i)=0
ccc	  if (itp1(i).ge.10.and.itp1(i).lt.20)
ccc     &		neigh_type(i)=neigh_type(i)+1
ccc	  if (icr1(i).ne.0)
ccc     &		neigh_type(i)=neigh_type(i)+icontab(1,icr1(i))
ccc	  if (isn1(i).ne.0) then
ccc	    j=isn1(i)
ccc	    if (itp1(i).eq.41.and.itp1(j).ge.10.and.itp1(j).lt.20)
ccc     &		neigh_type(i)=neigh_type(i)+1
ccc	    k=0
ccc	    do while (j.ne.i.and.j.ne.0)
ccc	     k=k+1
ccc	     j=isn1(j)
ccc	    enddo
ccc	    neigh_type(i)=neigh_type(i)+k
ccc	  endif
ccc	enddo
cccc   still not quite "spin" as icr....
ccc
ccc	endif
ccc
c--------------------------------------------------------------------
 
c ........... (local smooth - multi matl) .............
c   assuming that itp,icr checks on neigh table enough to fix that
c   can't arbitrarily move interior,exterior bdry defining nodes...
 
c check that the rlxwt won't lead to the iterative procedure blowing up
      local_rlxwt=rlxwt
      if (rlxwt.lt.zero .or. rlxwt.ge.one) local_rlxwt=half
 
c do the smooth the desired number of times
      do itime=1,ntimes
 
c   initialize weights and average neighbor positions for this itime of smoothing
c   Note: neigh_n already masked by mpary, and mpary could contain multiple children
         do i = 1,nnodes
            dx(i)=zero
            dy(i)=zero
            dz(i)=zero
            if (neigh_n(i).gt.0) then
               do j=1,neigh_n(i)
                  k=neigh(j+neigh_off(i))
                  dx(i)=dx(i)+xic(k)
                  dy(i)=dy(i)+yic(k)
                  dz(i)=dz(i)+zic(k)
               enddo
               dx(i)=dx(i)/dble(neigh_n(i))
               dy(i)=dy(i)/dble(neigh_n(i))
               dz(i)=dz(i)/dble(neigh_n(i))
               weight(i)=rlxwt
            else
               weight(i)=zero
            endif
         enddo
         nwt=nwttry
 
c   check volumes at new location and flag nodes bordering inverted elements
300      pos_vol=1
         do iel=1,nelements
            do j=1,4
              k=iparent(itet(j,iel))
	      xxx=one-weight(k)
	      rrr=weight(k)
	      x1(j)=xxx*xic(k)+rrr*dx(k)
	      y1(j)=xxx*yic(k)+rrr*dy(k)
	      z1(j)=xxx*zic(k)+rrr*dz(k)
	    enddo
	    vvv = ( (x1(4)-x1(1))*((y1(2)-y1(1))*(z1(3)-z1(1))
     &					-(z1(2)-z1(1))*(y1(3)-y1(1)))
     &		+(y1(4)-y1(1))*((z1(2)-z1(1))*(x1(3)-x1(1))
     &					-(x1(2)-x1(1))*(z1(3)-z1(1)))
     &		+(z1(4)-z1(1))*((x1(2)-x1(1))*(y1(3)-y1(1))
     &					-(y1(2)-y1(1))*(x1(3)-x1(1)))
     &		) * one_sixth
	    if (vvv.le.zero) then
               pos_vol=0
               do j=1,4
                  k=iparent(itet(j,iel))
                  neigh_n(k)=-iabs(neigh_n(k))
               enddo
            endif
         enddo
 
c   if inverting volume and if haven't tried to many times,
c    try to locally reduce weight for smoothing (and then go back to volume check);
c   otherwise punt
         if (pos_vol.eq.0) then
             nwt=nwt-1
             if (nwt.gt.0) then
                do i=1,nnodes
                   if (neigh_n(i).lt.0) then
                      neigh_n(i)=-neigh_n(i)
                      weight(i)=local_rlxwt*weight(i)
                   endif
                enddo
                goto 300
             elseif (nwt.eq.0) then
                do i=1,nnodes
                   if (neigh_n(i).lt.0) then
                      neigh_n(i)=-neigh_n(i)
                      weight(i)=zero
                   endif
                enddo
                goto 300
             else
                do i=1,nnodes
                   if (neigh_n(i).lt.0) neigh_n(i)=-neigh_n(i)
                   weight(i)=zero
                enddo
c  no need to reset or interpolate
                goto 400
             endif
          endif
 
c   reset parent positions (children done at end):
c   move parent node to weighted current location plus av of neighbors
c   Note: neigh_n already masked by mpary, and mpary could contain multiple children
          do i=1,nnodes
c  should be poisitive, but to be safe...
             if (neigh_n(i).ne.0) then
                neigh_n(i)=iabs(neigh_n(i))
                xxx=one-weight(i)
                rrr=weight(i)
                xic(i)=xxx*xic(i)+rrr*dx(i)
                yic(i)=xxx*yic(i)+rrr*dy(i)
                zic(i)=xxx*zic(i)+rrr*dz(i)
             endif
          enddo
 
c  ........... (interpolate) .............
c ADD INTERPOLATION IF NEEDED HERE...
c   could store "orig position" and interpolate at end, or interpolate here....
 
 
c   end loop on ntimes
      enddo
400   continue
 
c--------------------------------------------------------------------
c  ........... (fix child nodes) .............
 
c   spread new position of parent to child nodes
      do i=1,nnodes
         if (neigh_n(i).gt.0.and.itp1(i).eq.ifitpcup) then
            j=isn1(i)
            do while (j.ne.i.and.j.ne.0)
               xic(j)=xic(i)
               yic(j)=yic(i)
               zic(j)=zic(i)
ccc		 ! ! only if these are added as attributes re debugging...
ccc	         ! neigh_n(j)=neigh_n(i)
ccc	         ! dx(j)=dx(i)
ccc	         ! dy(j)=dy(i)
ccc	         ! dz(j)=dz(i)
ccc	         ! weight(j)=weight(i)
               j=isn1(j)
            enddo
         endif
      enddo
 
c--------------------------------------------------------------------
 
c  ........... (interpolate) .............
c ADD INTERPOLATION IF NEEDED HERE...
 
c--------------------------------------------------------------------
 
c  ........... (clean up/return) .............
9999  continue
c  release temp memory ............
      call mmrelprt(isubname,ierr)
      return
 
      end
 
c--------------------------------------------------------------------
 
