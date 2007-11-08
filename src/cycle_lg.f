      subroutine cycle_lg(nwds,msgtype,cmsgin,
     *    imsgin,xmsgin,ierror)
c
c #####################################################################
c
c    PURPOSE
c
c       collect info needed for volume conserving smoothing to pass
c     to worker routine
c  smooth/position/network/[pset,get,name]/[#iterations]/
c         [ weight]/[off]
c
c    INPUT ARGUMENTS -
c
c
c    OUTPUT ARGUMENTS -
c
c       IERROR - error return
c
c    CHANGE HISTORY -
c
C $Log:   /pvcs.config/t3d/src/cycle_lg.f_a  $
CPVCS    
CPVCS       Rev 1.17   19 Sep 2003 13:08:02   dcg
CPVCS    set nconbnd to zero if attribute does not exist
CPVCS    
CPVCS       Rev 1.16   14 Mar 2001 13:35:08   dcg
CPVCS    get rid of upper case
CPVCS    
CPVCS       Rev 1.15   10 Jan 2001 14:41:06   dcg
CPVCS    remove duplicate declaration
CPVCS    
CPVCS       Rev 1.14   18 Oct 2000 16:45:56   dcg
CPVCS    look for edges that are both interior and constrained
CPVCS    check if nodes in cycle are in the same set
CPVCS    if not smooth as a line - not as a patch
CPVCS    
CPVCS       Rev 1.13   16 Oct 2000 17:10:02   dcg
CPVCS    remove tests on icr
CPVCS    assume pset has restricted operation to desired subset
CPVCS    
CPVCS       Rev 1.10   26 Sep 2000 11:23:36   dcg
CPVCS    call lower_d if structures are not there
CPVCS    
CPVCS       Rev 1.9   05 Sep 2000 12:43:48   dcg
CPVCS    add tests on length of cycle
CPVCS    
CPVCS       Rev 1.8   05 Sep 2000 11:46:16   dcg
CPVCS    
CPVCS       Rev 1.4   24 Aug 2000 15:45:22   dcg
CPVCS    fix bug in setting index for nx3
CPVCS    
CPVCS       Rev 1.3   22 Aug 2000 10:12:36   dcg
c
c #####################################################################
 
      implicit none
      include 'local_element.h'
      include 'chydro.h'
      include 'consts.h'
 
      integer ierror,nwds
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtype(nwds)
      real*8 xmsgin(nwds)
      character*132 logmess
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      pointer (ipimt1, imt1)
      pointer (ipicr1, icr1)
      pointer (ipicontab, icontab)
      pointer (ipisetwd, isetwd)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      pointer (ipitettyp, itettyp)
      pointer (ipitet_3d, itet_3d),(ipjtet_3d,jtet_3d)
      pointer (ipitetoff_3d, itetoff_3d),
     *        (ipitettyp_3d, itettyp_3d),
     *        (ipjtetoff_3d, jtetoff_3d)
      pointer (ipitetclr_3d, itetclr_3d)
      integer itp1(*),imt1(*),icr1(*),icontab(50,*)
      integer isn1(*)
      real*8 xic(*)
      real*8 yic(*)
      real*8 zic(*)
      integer itet(*)
      integer itetoff(*)
      integer itet_3d(*),jtet_3d(*),jtetoff_3d(*),isetwd(*)
      integer itetoff_3d(*),itettyp_3d(*)
      integer itettyp(*),itetclr_3d(*)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      integer jtet(*),jtetoff(*)
      pointer (ipiseed,iseed),(ipiseede,iseede),(ipelts,elts),
     *   (ipiteto,iteto),(ipnodeidx,nodeidx),(iptriples,triples)
      integer iseed(*),iseede(*),elts(*),iteto(5,*),nodeidx(*)
      pointer (ipiparent,iparent),(ipivalid,ivalid)
      integer iparent(*),ivalid(*)
      pointer (ipmpary,mpary)
      integer mpary(*)
      pointer (ipicrvals,icrvals)
      integer icrvals(2,*)
      real*8 weight,volelm,vol(10)
      integer node1,node2,ip1,ip2,i,k,nmat1,nmat2,kt,iedge,nl,
     * nloop,nnodes_3d,icmotype,length,nelements_3d,nnodes,
     *  nelements,mbndry,nef,nef_3d,ityp,j,n,n1,n2,
     * ncandidates,numneighbors,icscode,it,nx1(2),nx2(2),num,
     * kkt,nelts,jt,nn1,nn2,nxtelm,nxtface,iface,ncommon,numdiff,
     * nsave1,nsave2,icount,ntriples,ii,jj,ktt,kface,mattst,ns1,
     * ns2,nwdsact,istart,iend,istride,mpno,maxlen,
     * numinvert,icharlnf,lower_d_flag,ic1,ic2,sameconstraint_lg,
     * nconbnd,nicrn1,nicrn2,icrn1,icrn2,npred,nsuc,node3
 
      logical test_face_lg,needpredsuc
      integer candidates(200),neignode(200),neighbors(200),
     *  mtrls(10),triangle(3,200),triples(2,*)
      real*8 savec(6)
      real*8 xcycle(200,2),ycycle(200,2),zcycle(200,2),coord(6),
     *  xv(4),yv(4),zv(4),r(10)
      logical inversioncheck
      character*32 ich1,ich2,ich3
c
      character*32 cmo,isubname,psetname
      include 'statementfunctions.h'
c
cc
      isubname = 'cycle_lg'
      maxlen=200
      ierror=0
c
c.... Weight goes from 0 to 1 - used by smoothedge
c.... default number of interations is 5, weight is 1.
c.... default is inversion check off
      inversioncheck=.true.
      weight=1.
      nloop=5
      nwdsact=nwds
      psetname='nonexxxx'
      istart=1
      iend=0
      istride=0
      if(msgtype(nwds).eq.3.and.nwds.gt.3) then
         length=icharlnf(cmsgin(nwds))
         if (cmsgin(nwds)(1:length).eq.'nocheck') then
           inversioncheck=.false.
           nwdsact= nwdsact-1
         elseif(cmsgin(nwds)(1:length).eq.'check')then
           nwdsact= nwdsact-1
         elseif(msgtype(nwds).eq.3.and.cmsgin(nwds).eq.'-def-') then
           nwdsact= nwdsact-1
         endif
      endif
      if(nwdsact.gt.4) then
         if(msgtype(nwdsact).eq.2.and.msgtype(nwdsact-1).eq.1) then
            weight=xmsgin(nwdsact)
            nloop=imsgin(nwdsact-1)
            nwdsact= nwdsact-2
         elseif(msgtype(nwdsact).eq.1) then
            nloop=imsgin(nwdsact)
            nwdsact= nwdsact-1
         elseif(msgtype(nwdsact).eq.2) then
            weight=xmsgin(nwdsact)
            nwdsact= nwdsact-1
         endif
      endif
      if(nwds.ge.6) then
         if(msgtype(6).eq.3) then
            psetname=cmsgin(6)
         elseif(msgtype(6).eq.1) then
            istart=imsgin(4)
            iend=imsgin(5)
            istride=imsgin(6)
         endif
      endif
 
c....
c.... Get surface mesh
c....
      call cmo_get_name(cmo,ierror)
 
c
c.... Get info from mesh object.
      call cmo_get_intinfo('nnodes',cmo,
     *   nnodes_3d,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,
     *   nelements_3d,length,icmotype,ierror)
      lower_d_flag=0
      call cmo_get_intinfo('lower_d_flag',cmo
     &    ,lower_d_flag,length,icmotype,ierror)
 
      if(ierror.ne.0.or.lower_d_flag.ne.1) then
c
c.... get the lower d stuff
c
         call dotask('lower_d/create;finish',ierror)
         if(ierror.ne.0) then
            call x3d_error(' error getting lower-d info in ',
     *        isubname)
            go to 9999
         endif
      endif
      call cmo_get_intinfo('d1_nnodes',cmo,
     *   nnodes,length,icmotype,ierror)
      call cmo_get_intinfo('d1_nelements',cmo,
     *   nelements,length,icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,
     *   mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('d1_nef_cmo',cmo,
     *   nef,length,icmotype,ierror)
      call cmo_get_intinfo('faces_per_element',cmo,
     *   nef_3d,length,icmotype,ierror)
      call cmo_get_intinfo('idebug',cmo,
     *   idebug,length,icmotype,ierror)
      call cmo_get_intinfo('nconbnd',cmo,
     *   nconbnd,length,icmotype,ierror)
      if(ierror.ne.0) nconbnd=0
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('icontab',cmo,ipicontab,
     *     length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierror)
      call cmo_get_info('d1_itet',cmo,ipitet,length,icmotype,ierror)
 
      call cmo_get_info('d1_itetoff',cmo,ipitetoff,length,icmotype,
     *  ierror)
      call cmo_get_info('d1_jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_info('d1_jtetoff',cmo,ipjtetoff,length,icmotype,
     *  ierror)
      call cmo_get_info('d1_itettyp',cmo,ipitettyp,length,icmotype,
     * ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr_3d,length,icmotype,
     *  ierror)
      call cmo_get_info('itet',cmo,ipitet_3d,length,icmotype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff_3d,length,icmotype,
     *  ierror)
      call cmo_get_info('jtet',cmo,ipjtet_3d,length,icmotype,ierror)
      call cmo_get_info('jtetoff',cmo,ipjtetoff_3d,length,icmotype,
     *  ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp_3d,length,icmotype,
     *  ierror)
 
      call mmgetblk('iparent',isubname,ipiparent,
     &      nnodes_3d,1,icscode)
      call mmgetblk('iseed',isubname,ipiseed,
     &      nnodes_3d,1,icscode)
      call mmgetblk('iseede',isubname,ipiseede,
     &      nnodes_3d,1,icscode)
      call mmgetblk('nodeidx',isubname,ipnodeidx,
     &      nnodes_3d,1,icscode)
      call mmgetblk('mpary',isubname,ipmpary,
     &      nnodes_3d,1,icscode)
      call mmgetblk('ivalid',isubname,ipivalid,
     &      nnodes_3d,1,icscode)
      call mmgetblk('elts',isubname,ipelts,
     &     200,1,icscode)
      call mmgetblk('iteto',isubname,ipiteto,
     &      nelements*5,1,icscode)
      call mmgetblk('triples',isubname,iptriples,
     &      nnodes*2,1,icscode)
      call mmgetblk('icrvals',isubname,ipicrvals,
     &      max(nconbnd,1)*2,1,icscode)
c
c     find the parents of each node.
c
      call unpackpc(nnodes_3d,itp1,isn1,iparent)
c
c  find valid nodes
c
      if (psetname.eq.'nonexxxx') then
         call pntlimn(istart,iend,istride,ipmpary,mpno,
     *                                nnodes_3d,isetwd,itp1)
      else
         ich1='pset'
         ich2='get'
         ich3=psetname
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes_3d,isetwd,itp1)
      endif
      do i=1,nnodes_3d
         ivalid(i)=0
      enddo
c
      do i=1,mpno
         ivalid(mpary(i))=1
      enddo
c
c  for each node find a tet that contains it
c
      do i=1,nelements_3d
         ityp=itettyp_3d(i)
         do j=1,nelmnen(ityp)
            iseed(itet_3d(itetoff_3d(i)+j))=i
            iseede(itet_3d(itetoff_3d(i)+j))=j
         enddo
      enddo
c
c   Make the matrix on node connections (i.e. the edge matrix)
c
      call make_edge_matrix_lg()
c
c  Make a list of faces ordered by node number to use
c  to determine if 3 nodes are a face
c
      call order_faces_lg(itet,iteto,nodeidx,iparent,nelements,
     *  nnodes,itet_3d,itetclr_3d,nelements_3d)
c
cc  Find all interior edges and pass edges and cycles of
C  nodes that surround end points of edges to smooth routine.
c
c
c  Loop through number of iterations
      do nl=1,nloop
        numinvert=0
        ntriples=0
C  Loop through all elements and all 3 edges on each element.
        do iedge=1,3
          do kt=1,nelements
            call primestep(nelements,it)
            nx1(1)=0
            nx2(1)=0
            nx1(2)=0
            nx2(2)=0
            node1=itet(itetoff(it)+ielmedge1(1,iedge,3))
            node2=itet(itetoff(it)+ielmedge1(2,iedge,3))
            ip1=iparent(node1)
            ip2=iparent(node2)
            if(itp1(node1).eq.ifitpcup) node1=isn1(node1)
            if(itp1(node2).eq.ifitpcup) node2=isn1(node2)
            if(ivalid(node1).eq.0.or.ivalid(node2).eq.0) go to 49
c  Point types must match
            if(itp1(node1).ne.itp1(node2)) go to 49
c  Constraints if they exist must match
            ic1=icr1(node1)
            ic2=icr1(node2)
            if(ic1.ne.0.and.itp1(node1).le.ifitpen1) then
               if(ic2.ne.0.and.itp1(node2).le.ifitpen1) then
                  if(sameconstraint_lg(ic1,ic2,icontab).ne.0)
     *              go to 49
               endif
            endif
c  For edge two the nodes are node 1 to node 3 (need 3 to 1 )
c  to avoid duplicate edges and skipping others
            if(iedge.eq.2) then
               i=node1
               node1=node2
               node2=i
               i=ip1
               ip1=ip2
               ip2=i
            endif
c  check that nodes have the same materials
c  skip edge if different - if more than 3 materials bail out
c  if more than 2 materials on exterior boundary bail out
            nmat1=1
            mtrls(1)=imt1(ip1)
            if(itp1(ip1).eq.ifitpcup.and.
     x         itp1(ip2).eq.ifitpcup) then
               nmat1=0
               nmat2=0
               i=ip1
               j=isn1(i)
               do while (j.ne.0..and.j.ne.i)
                  nmat1=nmat1+1
                  mtrls(nmat1)=imt1(j)
                  if(nmat1.gt.3) go to 49
                  if(nmat1.eq.3.and.itp1(node1).eq.12) go to 49
                  j=isn1(j)
               enddo
               i=ip2
               j=isn1(i)
               do while (j.ne.0..and.j.ne.i)
                  nmat2=nmat2+1
                  do n=1,nmat1
                     if(imt1(j).eq.mtrls(n)) go to 5
                  enddo
                  go to 49
 5                j=isn1(j)
               enddo
               if(nmat2.ne.nmat1) go to 49
            elseif ((itp1(ip1).eq.ifitpcup.and.
     x               itp1(ip2).ne.ifitpcup).or.
     x              (itp1(ip1).ne.ifitpcup.and.
     x               itp1(ip2).eq.ifitpcup)) then
                go to 49
             endif
c  If number of materials is 2 and nodes are interior interface
c  can use just one cycle - if nodes are interior interface and
c  number of materials is greater than three skip this edge
            if(itp1(node1).eq.2.and.itp1(node2).eq.2.
     *         and.nmat1.eq.2) then
               nmat1=1
            elseif(itp1(node1).eq.2.and.itp1(node2).eq.2.
     *         and.nmat1.gt.3) then
               go to 49
            endif
c  If both end points are type 12 then edge must be a triple edge
c  i.e. has at least 3 faces coming off
c  if both end points are type2 and number of materials=2 then
c  confirm that this is a triple edge
c  We process those that have exactly 3 faces - confirmed by
c  a jtet loop of length 3
           if((itp1(node1).eq.12.and.itp1(node2).eq.12.
     *         and.nmat1.eq.2).or.
     *        (itp1(node1).eq.2.and.itp1(node2).eq.2.
     *         and.nmat1.eq.3)) then
               if(iedge.eq.1) iface=3
               if(iedge.eq.2) iface=2
               if(iedge.eq.3) iface=1
               jt=jtet(jtetoff(it)+iface)
               if(jt.eq.mbndry) go to 49
               if(jt.gt.mbndry) jt=jt-mbndry
               nxtelm=1+(jt-1)/nef
               nxtface=jt-(nxtelm-1)*nef
               i=jtet(jtetoff(nxtelm)+nxtface)
               if(i.eq.mbndry) go to 49
               if(i.gt.mbndry) i=i-mbndry
               ii=1+(i-1)/nef
               jj=i-(ii-1)*nef
               if(ii.eq.it.and.jj.eq.iface) go to 49
            endif
c  Each interior edge will appear twice - process only one occurrance
            if(ip1.gt.ip2) go to 49
c  If nmat1>1 then edge might appear more than twice - check if
c  processed otherwise add it to the list
            if(nmat1.ne.1) then
               do i=1,ntriples
                  if(ip1.eq.triples(1,i).and.ip2.eq.triples(2,i))
     *              go to 49
               enddo
               ntriples=ntriples+1
               triples(1,ntriples)=ip1
               triples(2,ntriples)=ip2
            endif
c loop through number of materials ( must be <= 3)
c if three materials then must be interior edge - only need 2 cycles
            do n=1,min(2,nmat1)
c  Find common nodes to this edge
              call commonneigbors_lg(ip1,ip2,numneighbors,
     *            neighbors,ierror)
              if(numneighbors.gt.maxlen) then
                 write(logmess,9006)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
c  must find at least two common neighbors for this edge
c  and this material
c  Now select those nodes that match the material
              if(nmat1.gt.1)call filterneigbors_lg(numneighbors,
     *            neighbors,mtrls(n),imt1,isn1,ierror)
              if(numneighbors.lt.2.or.(
     *           numneighbors.eq.2 .and.
     *           neighbors(1).eq.neighbors(2))) then
                 write(logmess,9000) 'bad edge ',ip1,ip2,mtrls(n)
 9000            format(a,3i10)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
c  if more than two nodes are found, we
c  want to chose faces that are the correct color
              n1=1
              n2=2
              ncommon=numneighbors
              if(nmat1.eq.1) then
                 mattst=imt1(node1)
              else
                 mattst=mtrls(n)
               endif
              if(numneighbors.gt.2) then
               if(iedge.eq.1) iface=3
               if(iedge.eq.2) iface=2
               if(iedge.eq.3) iface=1
               ktt=it
               kface=iface
6              do i=1,numneighbors
                  if(iparent(itet(itetoff(ktt)+kface)).eq.
     *               neighbors(i)) then
 
                     if(test_face_lg(ip1,ip2,iparent(neighbors(i)),
     *                            iteto,nodeidx,mattst)) then
                        n1=i
                        jt=jtet(jtetoff(ktt)+kface)
                        if(jt.eq.mbndry) go to 49
                        if(jt.gt.mbndry) jt=jt-mbndry
                        nxtelm=1+(jt-1)/nef
                        nxtface=jt-(nxtelm-1)*nef
                        do j=1,numneighbors
                           if(iparent(itet(itetoff(nxtelm)+nxtface))
     *                     .eq. neighbors(j)) then
                           if(test_face_lg(ip1,ip2,
     *                            iparent(neighbors(j)),
     *                            iteto,nodeidx,mattst)) then
                               n2=j
                               go to 8
                           else
                              jt=jtet(jtetoff(nxtelm)+nxtface)
                              if(jt.eq.mbndry) go to 49
                              if(jt.gt.mbndry) jt=jt-mbndry
                              nxtelm=1+(jt-1)/nef
                              nxtface=jt-(nxtelm-1)*nef
                              do k=1,numneighbors
                                 if(iparent(itet(itetoff(nxtelm)+
     *                              nxtface))
     *                             .eq. neighbors(k)) then
                                    if(test_face_lg(ip1,ip2,
     *                                 iparent(neighbors(k)),
     *                                 iteto,nodeidx,mattst)) then
                                       n2=k
                                       go to 8
                                    endif
                                 endif
                              enddo
                           endif
                           endif
                        enddo
                        kkt=jtet(jtetoff(ktt)+kface)
                        if(kkt.eq.mbndry) go to 49
                        if(kkt.gt.mbndry) kkt=kkt-mbndry
                        ktt=1+(kkt-1)/nef
                        kface=kkt-(ktt-1)*nef
                     endif
 
                   endif
                enddo
c
c  try face neighbor tets till get back to the beginning
                kkt=jtet(jtetoff(ktt)+kface)
                if(kkt.eq.mbndry) go to 49
                if(kkt.gt.mbndry) kkt=kkt-mbndry
                ktt=1+(kkt-1)/nef
                kface=kkt-(ktt-1)*nef
                if(ktt.eq.it)go to 49
                go to 6
              endif
c
c if there are constraints on interior edges make sure
c constraints of special points are subsets of constaints
c on edge nodes
c if the constraints on special points are different from
c each other - we will have to check all nodes in cycle
c to see if we can find two that contain all the edge node
c constraints - then we will call these matching nodes the
c predecessor and sucesssor nodes for a special 'line' not
c 'patch' type smoothing
c
              needpredsuc=.false.
              nicrn1=0
              nicrn2=0
              if(ic1.ne.0.and.itp1(node1).le.ifitpen1
     *          .and.nmat1.eq.1.and.icontab(1,ic1).gt.1) then
                 node3=neighbors(n1)
                 if(itp1(node3).eq.ifitpcup) node3=isn1(node3)
                 icrn1=icr1(node3)
                 if(icrn1.ne.0) then
                    nicrn1=icontab(1,icrn1)
                    do j=1,nicrn1
                       icrvals(1,j)=icontab(2+j,icrn1)
                    enddo
                 endif
              endif
              if(ic1.ne.0.and.itp1(node1).le.ifitpen1
     *          .and.nmat1.eq.1.and.icontab(1,ic1).gt.1) then
                 node3=neighbors(n2)
                 if(itp1(node3).eq.ifitpcup) node3=isn1(node3)
                 icrn2=icr1(node3)
                 if(icrn2.ne.0) then
                    nicrn2=icontab(1,icrn2)
                    do j=1,nicrn2
                       icrvals(1,j)=icontab(2+j,icrn2)
                    enddo
                 endif
              endif
              npred=0
              nsuc=0
              if(nicrn1.ne.nicrn2) then
                 needpredsuc=.true.
              elseif(nicrn1.gt.0.and.nicrn2.gt.0) then
                 if(sameconstraint_lg(icrn1,icrn2,icontab)
     *                   .ne.0) then
                    needpredsuc=.true.
                 else
                    needpredsuc=.false.
                 endif
              endif
c  build list of candidates = all neighbors of node1 and/or node2
c  but not equal to node1 or node2 - duplicates are needed except
c  for duplicates of n1,n2 (remember n1,n2 are indices to the
c  special shared nodes.
 8            ncandidates=0
              call get_neighbors_lg(ip1,num,neignode,ierror)
              if(num.gt.maxlen) then
                 write(logmess,9006)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
              if(nmat1.gt.1)call filterneigbors_lg(num,
     *            neignode,mtrls(n),imt1,isn1,ierror)
              do i=1,num
                if(neignode(i).ne.ip1.and.neignode(i).ne.ip2) then
                   ncandidates=ncandidates+1
                   if(ncandidates.gt.maxlen) then
                      write(logmess,9006)
                      call writloga('default',0,logmess,0,ierror)
                      go to 49
                   endif
                   candidates(ncandidates)=neignode(i)
                endif
              enddo
c  if 3 material interior edge or 2 material external edge
c  need predecessor and successor nodes on triple line
c  nsave1 is predecessor node, nsave2 is successor node.
              if(nmat1.ne.1.and.n.eq.1)
     *            call find_triple_neigbors_lg(
     *            node1,ip2,num,neignode,
     *            imt1,isn1,itp1,jtet,jtetoff,
     *            itet,itetoff,itettyp,iparent,nef,
     *            mbndry,ipelts,it,
     *            nmat1,mtrls,nsave1,ierror)
              call get_neighbors_lg(ip2,num,neignode,ierror)
              if(num.gt.maxlen) then
                 write(logmess,9006)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
              if(nmat1.gt.1)call filterneigbors_lg(num,
     *            neignode,mtrls(n),imt1,isn1,ierror)
              do i=1,num
                if(neignode(i).ne.ip1.and.neignode(i).ne.ip2) then
                   do j=1,ncandidates
                      if(neignode(i).eq.candidates(j).and.(
     *                   neignode(i).eq.neighbors(n1).or.
     *                   neignode(i).eq.neighbors(n2))) go to 12
                   enddo
                   ncandidates=ncandidates+1
                   if(ncandidates.gt.maxlen) then
                      write(logmess,9006)
                      call writloga('default',0,logmess,0,ierror)
                      go to 49
                   endif
 9006              format (' maximum candidates for cycle > 200'
     *                     ,' will skip edge')
                   candidates(ncandidates)=neignode(i)
                endif
 12           continue
              enddo
              if(nmat1.ne.1.and.n.eq.1)
     *            call find_triple_neigbors_lg(
     *            node2,ip1,num,neignode,
     *            imt1,isn1,itp1,jtet,jtetoff,
     *            itet,itetoff,itettyp,iparent,nef,
     *            mbndry,ipelts,it,
     *            nmat1,mtrls,nsave2,ierror)
c
c  build cycle(s) of nodes around edge --
c  start at first special shared node neighbors(n1)
c  as use up entries in candidates array mark them out
c
              xcycle(1,n)=xic(neighbors(n1))
              ycycle(1,n)=yic(neighbors(n1))
              zcycle(1,n)=zic(neighbors(n1))
              triangle(1,1)=ip1
              triangle(2,1)=ip2
              triangle(3,1)=neighbors(n1)
              if(neighbors(n1).eq.nsave1.and.nmat1.ne.1
     *                  .and.n.eq.1) ns1=1
              if(neighbors(n1).eq.nsave2.and.nmat1.ne.1
     *                  .and.n.eq.1) ns2=1
              call markout_lg(ncandidates,candidates,neighbors(n1))
              nn1=ip1
              nn2=neighbors(n1)
              nx1(n)=1
c
c  follow cycle by looking for common neighbors of
c  one of the original edge nodes(nn1) and the last member
c  of the cycle(nn2).  Switch to second original node
c  when second special shared node is encountered
c
 10           call commonneigbors_lg(nn1,nn2,numneighbors,
     *            neignode,ierror)
              if(numneighbors.gt.maxlen) then
                 write(logmess,9006)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
              if(nmat1.gt.1)call filterneigbors_lg(numneighbors,
     *            neignode,mtrls(n),imt1,isn1,ierror)
 
c  throw out nodes that are not in face list
              i=1
              do while(i.le.numneighbors)
                 if(.not.test_face_lg(iparent(nn1),
     *                      neignode(i) ,iparent(nn2),
     *                       iteto,nodeidx,mattst)) then
                    call deleteitem_lg(numneighbors,neignode,i)
                 elseif(ip1.eq.neignode(i).or.
     *              ip2.eq.neignode(i)) then
                    call deleteitem_lg(numneighbors,neignode,i)
                 else
                    i=i+1
                 endif
              enddo
c
c  delete marked out items
c
              i=1
              do while(i.le.numneighbors)
                 do j=1,ncandidates
                    if(-candidates(j).eq.neignode(i)) then
                       do k=j,ncandidates
                          if(candidates(k).eq.neignode(i)) go to 111
                       enddo
                       call deleteitem_lg(numneighbors,neignode,i)
                       go to 11
                    endif
                 enddo
  111            continue
c
c  check that this is a new triangle
c
                 do k=1,nx1(n)
                     icount=0
                     do kkt=1,3
                         if(triangle(kkt,k).eq.nn1.or.
     *                      triangle(kkt,k).eq.nn2.or.
     *                      triangle(kkt,k).eq.neignode(i))
     *                      icount=icount+1
                    enddo
                    if (icount.eq.3) then
                       call deleteitem_lg(numneighbors,neignode,i)
                       go to 11
                    endif
                 enddo
                 i=i+1
  11             continue
              enddo
              numdiff=numneighbors
c  if no possibilities we might be done or there might be
c  an error
              if(numdiff.le.0) then
c
c must be done if can't find anything else in the list of
c candidates -  check that cycle closes
c
                 if(.not.test_face_lg(iparent(neighbors(n1)),
     *                       ip2,iparent(nn2),
     *                       iteto,nodeidx,mattst)) then
                    write(logmess,9001) ip1,ip2,nn1,nn2
 9001               format('cycle not closed - give up ',4i10)
                    call writloga('default',0,logmess,0,ierror)
                    go to 49
                 endif
                 go to 16
              elseif(numdiff.ge.2) then
                 write(logmess,9002)ip1,ip2,
     *             (neignode(k),k=1,numdiff)
 9002            format('too many choices - give up',8i10)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
              do i=1,ncandidates
                do j=1,numneighbors
                   if(candidates(i).eq.neignode(j))then
                      nx1(n)=nx1(n)+1
c  save triangle so we can check for duplicates later
                      triangle(1,nx1(n))=nn1
                      triangle(2,nx1(n))=nn2
                      triangle(3,nx1(n))=candidates(i)
                      if(neignode(j).eq.neighbors(n2)) then
                         nx2(n)=nx1(n)
                         nn1=ip2
                      endif
c  save index of predecessor and successor nodes
                      if(candidates(i).eq.nsave1.and.nmat1.ne.1
     *                  .and.n.eq.1) ns1=nx1(n)
                      if(candidates(i).eq.nsave2.and.nmat1.ne.1
     *                  .and.n.eq.1) ns2=nx1(n)
c  if need predecessor and sucessor for case with constrained
c  interior edges that are not triple lines then check here
                      if(needpredsuc.and.nmat1.eq.1) then
                         node3=candidates(i)
                         if(itp1(node3).eq.ifitpcup) node3=isn1(node3)
                         if(sameconstraint_lg(ic1,icr1(node3),icontab)
     *                      .ge.0) then
                            if(npred.eq.0)then
                               npred=nx1(n)
                            else
                               nsuc=nx1(n)
                            endif
                         endif
                      endif
c  add to cycle
                      xcycle(nx1(n),n)=xic(candidates(i))
                      ycycle(nx1(n),n)=yic(candidates(i))
                      zcycle(nx1(n),n)=zic(candidates(i))
                      candidates(i)=-candidates(i)
                      nn2=neignode(j)
                      go to 10
                   endif
 14                continue
                enddo
               enddo
 15           continue
 
c
c must be done if can't find anything else in the list of
c candidates -  check that cycle closes
c
              if(.not.test_face_lg(iparent(neighbors(n1)),
     *                       ip2,iparent(nn2),
     *                       iteto,nodeidx,mattst)) then
                 write(logmess,9003) ip1,ip2,iedge,kt
 9003            format('cycle not closed - give up',4i10)
                 call writloga('default',0,logmess,0,ierror)
                 go to 49
              endif
 16           continue
              enddo
c
c  done with both cycles
c
              coord(1)=xic(ip1)
              coord(2)=yic(ip1)
              coord(3)=zic(ip1)
              coord(4)=xic(ip2)
              coord(5)=yic(ip2)
              coord(6)=zic(ip2)
c
c  check volumes
         if(idebug.gt.1) then
           do k=1,6
             vol(k)=0.
             r(k)=0.
           enddo
           do jt=1,nelements_3d
             do i=1,4
              xv(i)=xic(itet_3d(itetoff_3d(jt)+i))
              yv(i)=yic(itet_3d(itetoff_3d(jt)+i))
              zv(i)=zic(itet_3d(itetoff_3d(jt)+i))
             enddo
             volelm=dvol(xv(1),yv(1),zv(1),xv(2),yv(2),zv(2),
     *        xv(3),yv(3),zv(3),xv(4),yv(4),zv(4))
             vol(itetclr_3d(jt))=vol(itetclr_3d(jt))+volelm
             vol(6)=vol(6)+volelm
           enddo
           do k=1,6
             r(k)=vol(k)
           enddo
         endif
 
c
c  check that nx1 and nx2 are valid
c
              do n=1,min(2,nmat1)
                if(nx1(n).le.0.or.nx2(n).le.0) go to 49
              enddo
c  call routine to calculate new coordinates
              if(nmat1.eq.1) then
                 if(needpredsuc) then
                    call smoothedgea(nx2(1)+1,nx1(1)-nx2(1)+3,
     *                coord,xcycle(1,1),
     *                ycycle(1,1),zcycle(1,1),npred,nsuc,
     *                weight,ierror)
                 else
                    call smoothedge(nx2(1)+1,nx1(1)-nx2(1)+3,
     *                coord,xcycle(1,1),
     *                ycycle(1,1),zcycle(1,1),weight,ierror)
                 endif
              else
                 call smoothedgetriple(nx2(1)+1,nx1(1)-nx2(1)+3,
     *                      nx2(2)+1,nx1(2)-nx2(2)+3,coord,
     *                      xcycle(1,1),ycycle(1,1),zcycle(1,1),
     *                      xcycle(1,2),ycycle(1,2),zcycle(1,2),
     *                      ns1,ns2,
     *                      weight,ierror)
              endif
 
c
c  Move node edges and children
c
              savec(1)=xic(node1)
              savec(2)=yic(node1)
              savec(3)=zic(node1)
              savec(4)=xic(node2)
              savec(5)=yic(node2)
              savec(6)=zic(node2)
              xic(ip1)=coord(1)
              yic(ip1)=coord(2)
              zic(ip1)=coord(3)
              xic(ip2)=coord(4)
              yic(ip2)=coord(5)
              zic(ip2)=coord(6)
              i=ip1
              j=isn1(i)
              do while (j.ne.0..and.j.ne.i)
                     xic(j)=coord(1)
                     yic(j)=coord(2)
                     zic(j)=coord(3)
                     j=isn1(j)
              enddo
              i=ip2
              j=isn1(i)
              do while (j.ne.0..and.j.ne.i)
                     xic(j)=coord(4)
                     yic(j)=coord(5)
                     zic(j)=coord(6)
                     j=isn1(j)
              enddo
c
c  check volumes
c
         if(idebug.gt.1) then
            do k=1,6
              vol(k)=0.
            enddo
            do jt=1,nelements_3d
              do i=1,4
                xv(i)=xic(itet_3d(itetoff_3d(jt)+i))
                yv(i)=yic(itet_3d(itetoff_3d(jt)+i))
                zv(i)=zic(itet_3d(itetoff_3d(jt)+i))
              enddo
              volelm=dvol(xv(1),yv(1),zv(1),xv(2),yv(2),zv(2),
     *         xv(3),yv(3),zv(3),xv(4),yv(4),zv(4))
 
              vol(itetclr_3d(jt))=vol(itetclr_3d(jt))+volelm
              vol(6)=vol(6)+volelm
            enddo
            do k=1,6
 
             if(abs(vol(k)-r(k)).gt.1.e-12*r(k)) then
               write(logmess,9004)ip1,ip2,r(k),vol(k)
 9004          format(' unconserved volume ',2i10,1x,2e20.12)
               call writloga('default',0,logmess,0,ierror)
             endif
            enddo
          endif
              if (.not.inversioncheck) go to 1000
 
c
c  check for inverted elements
c
              call get_elements_around_node(iseed(node1),
     *               iseede(node1),nelts,ipelts,itetoff_3d,
     *               jtetoff_3d,itet_3d,jtet_3d,itettyp_3d,
     *               iparent,nef_3d,mbndry)
              do kkt=1,nelts
                     do i=1,4
                        xv(i)=xic(
     *                     itet_3d(itetoff_3d(elts(kkt))+i))
                        yv(i)=yic(
     *                     itet_3d(itetoff_3d(elts(kkt))+i))
                        zv(i)=zic(
     *                     itet_3d(itetoff_3d(elts(kkt))+i))
                     enddo
                     call volume_element(5,xv,yv,zv,volelm)
                     if(volelm.lt.0.0) then
                        numinvert=numinvert+1
                        xic(ip1)=savec(1)
                        yic(ip1)=savec(2)
                        zic(ip1)=savec(3)
                        xic(ip2)=savec(4)
                        yic(ip2)=savec(5)
                        zic(ip2)=savec(6)
                        i=ip1
                        j=isn1(i)
                        do while (j.ne.0..and.j.ne.i)
                           xic(j)=savec(1)
                           yic(j)=savec(2)
                           zic(j)=savec(3)
                           j=isn1(j)
                        enddo
                        i=ip2
                        j=isn1(i)
                        do while (j.ne.0..and.j.ne.i)
                           xic(j)=savec(4)
                           yic(j)=savec(5)
                           zic(j)=savec(6)
                          j=isn1(j)
                        enddo
                        go to  49
                     endif
              enddo
              call get_elements_around_node(iseed(node2),
     *               iseede(node2),nelts,ipelts,itetoff_3d,
     *               jtetoff_3d,itet_3d,jtet_3d,itettyp_3d,
     *               iparent,nef_3d,mbndry)
                  do kkt=1,nelts
                     do i=1,4
                        xv(i)=xic(
     *                     itet_3d(itetoff_3d(elts(kkt))+i))
                        yv(i)=yic(
     *                     itet_3d(itetoff_3d(elts(kkt))+i))
                        zv(i)=zic(
     *                     itet_3d(itetoff_3d(elts(kkt))+i))
                     enddo
                     call volume_element(5,xv,yv,zv,volelm)
                     if(volelm.lt.0.0) then
                        numinvert=numinvert+1
                        xic(ip1)=savec(1)
                        yic(ip1)=savec(2)
                        zic(ip1)=savec(3)
                        xic(ip2)=savec(4)
                        yic(ip2)=savec(5)
                        zic(ip2)=savec(6)
                        i=ip1
                        j=isn1(i)
                        do while (j.ne.0..and.j.ne.i)
                           xic(j)=savec(1)
                           yic(j)=savec(2)
                           zic(j)=savec(3)
                           j=isn1(j)
                        enddo
                        i=ip2
                        j=isn1(i)
                        do while (j.ne.0..and.j.ne.i)
                           xic(j)=savec(4)
                           yic(j)=savec(5)
                           zic(j)=savec(6)
                          j=isn1(j)
                        enddo
                        go to  49
                     endif
              enddo
 1000       continue
c
 49         continue
          enddo
        enddo
        if(numinvert.gt.0) then
           write(logmess,9005)'would have inverted ',
     *       numinvert,' elements'
 9005      format( a,i10,a)
           call writloga('default',0,logmess,0,ierror)
        endif
      enddo
 9999 continue
      call mmrelprt(isubname,icscode)
      call delete_edge_matrix_lg
      return
      end
c
c
      subroutine markout_lg(n,list,item)
c  set an entry in a list to its negative
      implicit none
      integer n,list(*),item,i
      do i=1,n
        if(list(i).eq.item)then
          list(i)=-list(i)
        endif
      enddo
      return
      end
c
c
      function finditem_lg(n,list,item)
c  return the index to an item in a list given the item.
      implicit none
      integer finditem_lg,n,list(*),item,i
      do i=1,n
        if(list(i).eq.item)then
          finditem_lg=i
        endif
      enddo
      return
      end
c
c
      subroutine deleteitem_lg(n,list,index)
c  delete and item from a list given its index
      implicit none
      integer n,list(*),index,i
      if (index.eq.n) then
         n=n-1
      else
        do i=index,n-1
          list(i)=list(i+1)
        enddo
        n=n-1
      endif
      return
      end
c
c
      subroutine delete_edge_matrix_lg()
c
      integer icscode
      character*32 matname
      matname='edge_matrix'
      call mmrelprt(matname,icscode)
      return
      end
c
c
      subroutine make_edge_matrix_lg()
c
c  use the utility matbld1 to make a packed
c  edge connection matrix
c  irowcnt(node) is the number of nodes connected to node
c  irowoff(node) is the offset to the list of nodes
c    connected to node
c  icolmat(irowoff(node)+1...) is the list of nodes
c    connected to node
c
      pointer (ipisort, isort)
      pointer (ipicolmat, icolmat)
      pointer (ipirowmat, irowmat)
      pointer (ipirowcnt, irowcnt)
      pointer (ipirowoff, irowoff)
      pointer (ipisendnn, isendnn)
      integer isort(*),isendnn(*)
      integer icolmat(*), irowmat(*)
      integer irowcnt(*), irowoff(*)
      pointer (ipiparent, iparent)
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer isetwd(*)
      integer imt1(*), itp1(*),
     *        icr1(*), isn1(*),
     *        itet(3,*), jtet(3,*)
      integer itet1(*), jtet1(*)
      real*8   xic(*), yic(*), zic(*)
      integer iparent(*)
      pointer (ipitetp, itetp1)
      integer itetp1(*)
      pointer (ipitetp, itetp)
      integer itetp(3,*)
      integer nnodes,length,icmotype,ierror,nelements,mbndry,nsdtopo,
     *  nen,nef,n,nconn,icscode,index,i,n12,nsdgeom,idebug,ipackopt,
     *  idiag,it,nnodes_3d
       character*32 cmo,isubname,matname
      parameter (nconn=6)
      integer lconn(2,nconn)
      data lconn / 1, 2,
     *             2, 1,
     *             1, 3,
     *             3, 1,
     *             2, 3,
     *             3, 2 /
      isubname='make_edge_matrix'
      matname='edge_matrix'
      call cmo_get_name(cmo,ierror)
      call cmo_get_intinfo('d1_nnodes',cmo,nnodes,length,icmotype,
     *  ierror)
      call cmo_get_intinfo('nnodes',cmo,nnodes_3d,length,icmotype,
     *  ierror)
      call cmo_get_intinfo('d1_nelements',cmo,nelements,length,icmotype,
     *  ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_intinfo('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      call cmo_get_intinfo('ndimensions_geom',cmo,
     *                  nsdgeom,length,icmotype,ierror)
      call cmo_get_intinfo('d1_nen_cmo',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_intinfo('d1_nef_cmo',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('d1_itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('d1_jtet',cmo,ipjtet,length,icmotype,ierror)
      call cmo_get_intinfo('idebug',cmo,idebug,length,icmotype,ierror)
C
C     ..................................................................
C     SET THE MATRIX PACKING FLAG:  =0 ==> THE SPARSE MATRIX PATTERN
C                                             IS PADDED TO A CONSTANT
C                                             COLUMN WIDTH.
C                                   =1 ==> THE SPARSE MATRIX PATTERN
C                                             IS FULLY COMPRESSED WITH
C                                             OFFSETS TO THE ROWS.
C
      ipackopt=1
C     ..................................................................
C     SET THE MATRIX DIAGONAL FLAG: =0 ==> DON'T INCLUDE THE DIAGONAL
C                                             ELEMENTS IN THE MATRIX.
C                                   =1 ==> INCLUDE THE DIAGONAL
C                                             ELEMENTS IN THE MATRIX.
C
      idiag=0
C     ..................................................................
 
C     ..................................................................
C     ALLOCATE SOME TEMPARY ARRAYS.
C
      length=nconn*nelements+idiag*nnodes
      call mmgetblk("isort",isubname,ipisort,length,1,icscode)
      call mmgetblk("icolmat",matname,ipicolmat,length,1,icscode)
      call mmgetblk("irowmat",isubname,ipirowmat,length,1,icscode)
      call mmgetblk("isendnn",isubname,ipisendnn,length,1,icscode)
      length=nnodes_3d
      call mmgetblk("irowcnt",matname,ipirowcnt,length,1,icscode)
      call mmgetblk("irowoff",matname,ipirowoff,length,1,icscode)
      length=nen*nelements
      call mmgetblk("itetp",isubname,ipitetp,length,1,icscode)
C     ..................................................................
C     FIND THE PARENTS OF EACH NODE. replace children with parents in
c     connectivity matrix
C
      length=nnodes_3d
      call mmgetblk('iparent',isubname,ipiparent,length,1,icscode)
      call unpackpc(nnodes_3d,itp1,isn1,iparent)
      do it=1,nelements
         index=nen*(it-1)
         do i=1,nen
            itetp1(index+i)=iparent(itet1(index+i))
         enddo
      enddo
C
C     BUILD SOME MATRIX ARRAYS FROM THE TET LIST.
C
      n12=nconn*nelements+idiag*nnodes
      do i=1,n12
         irowmat(i)=0
         icolmat(i)=0
         isendnn(i)=0
      enddo
      do i=1,nnodes_3d
         irowcnt(i)=0
         irowoff(i)=0
      enddo
C
      call matbld1(idiag,ipackopt,
     *             nsdtopo,nen,nef,
     *             nconn,lconn,
     *             nnodes_3d,nelements,itetp1,
     *             irowmat,icolmat,isort,isendnn,
     *             irowcnt,irowoff)
c fix up offsets
      irowoff(1)=0
      do n=2,nnodes_3d
         irowoff(n)=irowoff(n-1)+irowcnt(n-1)
      enddo
      call mmrelprt(isubname,icscode)
      return
      end
c
c
      subroutine commonneigbors_lg(node1,node2,numneighbors,
     *  neighbors,ierror)
c
c  use the edge matrix to find common neighbors to node1
c  and node2
c
      implicit none
      integer node1,node2,numneighbors,i,j,
     *  neighbors(*),num1,num2,ierror,length,icscode
      pointer (ipicolmat,icolmat),(ipirowcnt,irowcnt),
     *  (ipirowoff,irowoff)
      integer icolmat(*),irowcnt(*),irowoff(*)
      character*32 matname
      matname='edge_matrix'
      call mmfindbk('icolmat',matname,ipicolmat,length,icscode)
      if(ierror.ne.0) go to 9999
      call mmfindbk('irowcnt',matname,ipirowcnt,length,icscode)
      if(ierror.ne.0) go to 9999
      call mmfindbk('irowoff',matname,ipirowoff,length,icscode)
      if(ierror.ne.0) go to 9999
      numneighbors=0
      num1=irowcnt(node1)
      num2=irowcnt(node2)
      do i=1,num1
         do j=1,num2
            if(icolmat(irowoff(node1)+i).eq.
     *         icolmat(irowoff(node2)+j)) then
               numneighbors=numneighbors+1
               neighbors(numneighbors)=icolmat(irowoff(node2)+j)
            endif
         enddo
      enddo
      ierror=0
      return
 9999 ierror=1
      return
      end
c
c
      subroutine get_neighbors_lg(node1,numneighbors,
     *  neighbors,ierror)
c
c  use the edge matrix to retrieve all neighbors of node1
c
      implicit none
      integer node1,numneighbors,i,
     *  neighbors(*),ierror,length,icscode
      pointer (ipicolmat,icolmat),(ipirowcnt,irowcnt),
     *  (ipirowoff,irowoff)
      integer icolmat(*),irowcnt(*),irowoff(*)
      character*32 matname
      matname='edge_matrix'
      call mmfindbk('icolmat',matname,ipicolmat,length,icscode)
      if(ierror.ne.0) go to 9999
      call mmfindbk('irowcnt',matname,ipirowcnt,length,icscode)
      if(ierror.ne.0) go to 9999
      call mmfindbk('irowoff',matname,ipirowoff,length,icscode)
      if(ierror.ne.0) go to 9999
      numneighbors=irowcnt(node1)
      do i=1,numneighbors
          neighbors(i)=icolmat(irowoff(node1)+i)
      enddo
      ierror=0
      return
 9999 ierror=1
      return
      end
c
      function is_neighbor_lg(node1,node2,ierror)
c
c return true if node1 and node2 are neighbors
c use the edge matrix
c
      implicit none
      logical is_neighbor_lg
      integer node1,node2,i,num,
     *  ierror,length,icscode
      pointer (ipicolmat,icolmat),(ipirowcnt,irowcnt),
     *  (ipirowoff,irowoff)
      integer icolmat(*),irowcnt(*),irowoff(*)
      character*32 matname
      matname='edge_matrix'
      call mmfindbk('icolmat',matname,ipicolmat,length,icscode)
      if(ierror.ne.0) go to 9999
      call mmfindbk('irowcnt',matname,ipirowcnt,length,icscode)
      if(ierror.ne.0) go to 9999
      call mmfindbk('irowoff',matname,ipirowoff,length,icscode)
      if(ierror.ne.0) go to 9999
      num=irowcnt(node1)
      do i=1,num
          if(icolmat(irowoff(node1)+i).eq.node2) then
             is_neighbor_lg=.true.
             ierror=0
             go to 9998
          endif
      enddo
9999  is_neighbor_lg=.false.
      ierror=1
 9998 return
      end
c
c
      subroutine order_faces_lg(itet,iteto,nodeidx,
     *   iparent,nelements,nnodes,itet_3d,itetclr_3d,
     *    nelements_3d)
c
c  make a 3x number of faces list that contains the
c  nodes number of the face, such that itet0(1,face)=
c  minimum node number of face, itet0(3,face)= maximum
c  node number of face, and itet0(2,face) = other node.
c  also make a list of offsets into this list
c  such that nodeidx(node) points to the first entry
c  in iteto that has node as the 1st of the triplet of
c  node numbers.
c
      implicit none
      include 'local_element.h'
      integer itet(3,*),iteto(5,*),nodeidx(*),iparent(*),
     *  nelements,imin,imax,imid,m,nodetst,icscode,nnodes
      integer i,j,idxmn,idxmx,k,ipt(3),nelements_3d
      pointer (ipitemp,itemp)
      integer itemp(5,*),itet_3d(4,*),itetclr_3d(*)
      character*32 isubname
      isubname='order_faces'
      call mmgetblk('itemp',isubname,ipitemp,nelements*3,1,
     * icscode)
      do i=1,nelements
         imin=iparent(itet(1,i))
         imid=iparent(itet(1,i))
         imax=iparent(itet(1,i))
         idxmx=1
         idxmn=1
         do j=2,3
            if(iparent(itet(j,i)).gt.imax) then
               idxmx=j
               imax=iparent(itet(j,i))
            endif
            if(iparent(itet(j,i)).lt.imin) then
               idxmn=j
               imin=iparent(itet(j,i))
            endif
         enddo
         if(idxmn+idxmx.eq.3) imid=3
         if(idxmn+idxmx.eq.4) imid=2
         if(idxmn+idxmx.eq.5) imid=1
         iteto(1,i)=imin
         iteto(2,i)=iparent(itet(imid,i))
         iteto(3,i)=imax
         iteto(4,i)=0
         iteto(5,i)=0
      enddo
      m=5
      call hpsortim(nelements,m,m,itemp,iteto)
      call mmrelprt(isubname,icscode)
c
c  find offsets
c
      do i=1,nnodes
         nodeidx(i)=0
      enddo
      nodetst=0
      do i=1,nelements
         if(iteto(1,i).gt.nodetst) then
             nodetst=iteto(1,i)
             nodeidx(nodetst)=i
         endif
      enddo
c
c  now set face material from 3d mesh
c  assume tets
c
      do i=1,nelements_3d
         do j=1,4
            do k=1,3
               ipt(k)=iparent(itet_3d(ielmface1(k,j,5),i))
            enddo
            imin=ipt(1)
            imid=ipt(1)
            imax=ipt(1)
            idxmx=1
            idxmn=1
            do k=2,3
               if(ipt(k).gt.imax) then
                  imax=ipt(k)
                  idxmx=k
               endif
               if(ipt(k).lt.imin) then
                  imin=ipt(k)
                  idxmn=k
               endif
            enddo
            if(idxmn+idxmx.eq.3) imid=3
            if(idxmn+idxmx.eq.4) imid=2
            if(idxmn+idxmx.eq.5) imid=1
            imid=ipt(imid)
            k=nodeidx(imin)
            do while (imin.eq.iteto(1,k))
               if (imin.eq.iteto(1,k).and.imid.eq.iteto(2,k).and.
     *            imax.eq.iteto(3,k)) then
                  if(iteto(4,k).eq.0) then
                    iteto(4,k) =itetclr_3d(i)
                  else
                    iteto(5,k) =itetclr_3d(i)
                  endif
                  go to 80
               endif
               k=k+1
            enddo
 80      continue
         enddo
      enddo
      return
      end
c
c
      function test_face_lg(n1,n2,n3,iteto,nodeidx,mtrl)
c
c use data structures made by order_face to see if
c a triplet of nodes consitute a face in the mesh
c
      implicit none
      logical test_face_lg
      integer n1,n2,n3,iteto(5,*),nodeidx(*),
     *  imin,imax,imid,i,mtrl
      imin = min(n1,n2,n3)
      imax = max(n1,n2,n3)
      if (n1.gt.imin.and.n1.lt.imax)  imid=n1
      if (n2.gt.imin.and.n2.lt.imax)  imid=n2
      if (n3.gt.imin.and.n3.lt.imax)  imid=n3
      i=nodeidx(imin)
      do while (imin.eq.iteto(1,i))
         if (imin.eq.iteto(1,i).and.imid.eq.iteto(2,i).and.
     *       imax.eq.iteto(3,i).and.
     *       (mtrl.eq.iteto(4,i).or.mtrl.eq.iteto(5,i))) then
            test_face_lg =.true.
            return
         endif
         i=i+1
      enddo
      test_face_lg=.false.
      return
      end
c
c
      subroutine filterneigbors_lg(numneighbors,
     *            neighbors,mtrl,imt1,isn1,ierror)
c
c  limit the input list of nodes to those nodes whose
c  material number is the same as the input number(mtrl)
c  or to nodes whose isn chains contain a node of the
c  requisite material
c
      implicit none
      integer numneighbors,neighbors(*),mtrl,imt1(*),
     *  ierror,i,j,n ,numnew,isn1(*)
      ierror=0
      numnew=numneighbors
      do n=numneighbors,1,-1
         i=neighbors(n)
         j=isn1(i)
         if(j.eq.0.and.imt1(i).eq.mtrl) go to 10
         do while (j.ne.0..and.j.ne.i)
             if(imt1(j).eq.mtrl) go to 10
             j=isn1(j)
         enddo
c
c  didn't find material so eliminate this from list
c
         numnew=numnew-1
c         if (n.ne.numnew) then
           do j=n,numnew
             neighbors(j)=neighbors(j+1)
           enddo
c         endif
 10      continue
      enddo
      numneighbors=numnew
      return
      end
c
c
      subroutine find_triple_neigbors_lg(node,ip1,numneighbors,
     *            neighbors,imt1,isn1,itp1,jtet,jtetoff,
     *            itet,itetoff,itettyp,iparent,nef,
     *            mbndry,ipelts,it,
     *            nmat,mtrls,tripneig,ierror)
c
c  find node that preceeds the edge node-ip1 in the sense
c  that this node is the correct type and separated
c  the correct materials.
c
      implicit none
      pointer (ipelts,elts)
      include 'local_element.h'
      include 'chydro.h'
      integer numneighbors,neighbors(*),node,imt1(*),
     *  ierror,i,j,n ,isn1(*),itp1(*),jtet(*),jtetoff(*),
     *  nmat,mtrls(*),k,nelts,elts(*),ii,jj,
     *  nef,mbndry,icount,it,ipin,itet(*),
     *   itetoff(*),itettyp(*),iparent(*),iface,ip1,
     *   jt,nxtelm,nxtface,kkt,tripneig,nodechild
      ierror=0
      do n=1,numneighbors
c  skip other original edge node
         if(iparent(ip1).eq.iparent(neighbors(n))) go to 6
         nodechild=neighbors(n)
         if(itp1(neighbors(n)).eq.ifitpcup)
     *      nodechild=isn1(neighbors(n))
         if(itp1(node).eq.12.and.itp1(nodechild).ne.12)
     *     go to 6
         if(itp1(node).eq.2.and.itp1(nodechild).ne.2.and.
     *      itp1(nodechild).ne.12)
     *     go to 6
c  right node type check materials
         icount=0
         do k=1,nmat
            i=neighbors(n)
            j=isn1(i)
            do while (j.ne.0..and.j.ne.i)
                if(imt1(j).eq.mtrls(k)) then
                   icount=icount+1
                   go to 5
                endif
                j=isn1(j)
            enddo
            go to 6
 5          continue
         enddo
c  found a node with the correct number of materials
         if (icount.eq.nmat) then
 
c  right materials and type check if triple edge
c  find local node number
           ipin=0
           i=0
           do while (ipin.eq.0.and.i.le.3)
             i=i+1
             if(iparent(itet(itetoff(it)+i)).eq. iparent(node))
     *          ipin=i
           enddo
           call get_elements_around_node(it,ipin,nelts,ipelts,
     *     itetoff,jtetoff,itet,jtet,itettyp,iparent,nef,
     *     mbndry)
c  loop through elements to find edge in question
           tripneig=neighbors(n)
           do i=1,nelts
              do j=1,3
                 if((iparent(itet(itetoff(elts(i))+ielmedge1(1,j,3)))
     *            .eq.iparent(tripneig).and.
     *            iparent(itet(itetoff(elts(i))+ielmedge1(2,j,3)))
     *            .eq.iparent(node)).or.
     *             (iparent(itet(itetoff(elts(i))+ielmedge1(2,j,3)))
     *            .eq.iparent(tripneig).and.
     *            iparent(itet(itetoff(elts(i))+ielmedge1(1,j,3)))
     *            .eq.iparent(node))) then
                     if(j.eq.1)iface=3
                     if(j.eq.2)iface=2
                     if(j.eq.3)iface=1
c  see if this is a triple edge - must have jtet loop of length 3
                     jt=jtet(jtetoff(elts(i))+iface)
                     if(jt.eq.mbndry) go to 4
                     if(jt.gt.mbndry) jt=jt-mbndry
                     nxtelm=1+(jt-1)/nef
                     nxtface=jt-(nxtelm-1)*nef
                     kkt=jtet(jtetoff(nxtelm)+nxtface)
                     if(kkt.eq.mbndry) go to 4
                     if(kkt.gt.mbndry) kkt=kkt-mbndry
                     ii=1+(kkt-1)/nef
                     jj=kkt-(ii-1)*nef
                     if(ii.ne.elts(i).or.
     *                    jj.ne.iface) go to 9999
                 endif
              enddo
 4            continue
           enddo
 7         continue
         endif
 6       continue
      enddo
      go to 9999
 9998 ierror=1
 9999 continue
      return
      end
      function sameconstraint_lg(ic1,ic2,icontab)
      implicit none
      integer sameconstraint_lg
      integer ic1,ic2,icontab(50,*),i,j,inum1,inum2
c check to see if ic1 and ic2 are in the same class
c return -1 if ic1 not contained in ic2
c return 0 if constraint classes are exactly the same
c return 1 if ic1 contained in ic2 but not exactly the same
      sameconstraint_lg=-1
      inum1=icontab(1,ic1)
      inum2=icontab(1,ic2)
      if(inum1.gt.inum2) go to 9999
      sameconstraint_lg=0
      if(inum1.ne .inum2) sameconstraint_lg=1
      do i=1,inum1
         do j=1,inum2
            if(icontab(2+i,ic1).eq.icontab(2+j,ic2)) go to 10
         enddo
         sameconstraint_lg=-1
         go to 9999
 10      continue
      enddo
 9999 continue
      return
      end
