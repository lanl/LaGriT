      subroutine refine_interface_elements_lg(imsgin,xmsgin,
     *   cmsgin,msgtype,nwds,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     This routine refines interior edges of tetrahedral all of
C     whose nodes lie on an interface
C
c    refine/interface///edge/pset,get,psetname////[inclusive|exclusive]
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/refine_interface_elements_lg.f_a  $
CPVCS    
CPVCS       Rev 1.3   Thu Apr 06 13:53:40 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.2   Wed Aug 11 11:12:36 1999   dcg
CPVCS    use npoints as length for iseedtet not nelements
CPVCS
CPVCS       Rev 1.1   Mon Feb 22 20:42:36 1999   nnc
CPVCS    Deleted duplicate type specification.
CPVCS
CPVCS       Rev 1.0   Wed Feb 10 11:22:08 1999   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
      include 'local_element.h'
C
      integer nwds, imsgin(nwds),msgtype(nwds), ierror
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C
      pointer (ipitp1, itp1(*))
      pointer (ipisn1, isn1(*))
      pointer (ipisetwd, isetwd(*))
      pointer (ipxic, xic(*))
      pointer (ipyic, yic(*))
      pointer (ipzic, zic(*))
      integer itp1,isn1,isetwd
      real*8 xic,yic,zic
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetclr, itetclr)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(*), itetclr(*),
     *        itetoff(*), jtetoff(*)
C
      pointer (ipitadd, itadd)
      pointer (ipieadd, ieadd)
      integer itadd(*), ieadd(*)
      pointer (ipiadd, iadd)
      integer iadd(*)
      pointer (ipipt1, ipt1)
      pointer (ipipt2, ipt2)
      integer ipt1(*),ipt2(*)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(*), yadd(*), zadd(*)
      pointer (ipiseedtet, iseedtet)
      integer iseedtet(*)
      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipielts, ielts)
      integer ielts(*)
      pointer (ipiedges, iedges)
      integer iedges(*)
      pointer (ipintp,intp)
      integer intp(*)
      pointer (ipmpary,mpary)
      integer mpary(*)
C
      integer length,icscode,itype,npoints,ntets,ilen,mbndry,
     *   nef,i,nf,it,k,naddmax,isum,i1,jp1,jp2,inc,
     *   ityp,nadd,nelts,j,mpno,
     *   ip1,ip2,ip3,ipointi,ipointj,jmin,jmax
      character*32 cmo,isubname,ich1,ich2,ich3
      character*8 copt
      logical linclusive
c
      isubname='refine_interface'
      linclusive=.true.
      nadd=0
c
c  pick up pairs of nodes and find tet and edges they belong to
c  will use call to refine_edge_add
c
c     get memory for refine subroutine
      length=100
      naddmax=100
      call mmgetblk('itadd',isubname,ipitadd,length,1,icscode)
      call mmgetblk('ieadd',isubname,ipieadd,length,1,icscode)
      call mmgetblk('iadd',isubname,ipiadd,length,1,icscode)
      call mmgetblk('xadd',isubname,ipxadd,length,2,icscode)
      call mmgetblk('yadd',isubname,ipyadd,length,2,icscode)
      call mmgetblk('zadd',isubname,ipzadd,length,2,icscode)
      call mmgetblk('ipt1',isubname,ipipt1,length,1,icscode)
      call mmgetblk('ipt2',isubname,ipipt2,length,1,icscode)
 
c  get mesh object information
      call cmo_get_name(cmo,icscode)
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,ntets,ilen,itype,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,icscode)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,icscode)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,icscode)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,icscode)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,icscode)
c
c  get memory for seed array and parent array and tets surrounding node
      length=npoints
      call mmgetblk('iseedtet',isubname,ipiseedtet,length,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,
     &      length,1,icscode)
      call mmgetblk('mpary',isubname,ipmpary,
     &      length,1,icscode)
      call mmgetblk('intp',isubname,ipintp,
     &      length,1,icscode)
      length=100
      call mmgetblk('ielts',isubname,ipielts,
     &      length,1,icscode)
      call mmgetblk('iedges',isubname,ipiedges,
     &      length,1,icscode)
c  get parents
      call unpackpc(npoints,itp1,isn1,iparent)
c  get interface nodes
      copt='set'
      call unpacktp('intrface',copt,npoints,ipitp1,ipintp,icscode)
c  get point set if restricted
      ich1=' '
      ich2=' '
      ich3=' '
      mpno=0
      if(msgtype(6).eq.1) then
         ip1=imsgin(6)
         ip2=imsgin(7)
         ip3=imsgin(8)
C
         call cmo_get_info('ipointi',cmo,
     *                   ipointi,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('ipointj',cmo,
     *                   ipointj,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
         if(ip1.eq.0) ip1=ipointi
         if(ip2.eq.0) ip2=ipointj
         if(ip3.eq.0) ip3=1
         ich3='-def-'
         call pntlimn(ip1,ip2,ip3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      else
         ich1=cmsgin(6)
         ich2=cmsgin(7)
         ich3=cmsgin(8)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                npoints,isetwd,itp1)
      endif
c  modify intp such that intp(i) will be 2 if i is in point set and
c  an interface node, 1 if an interface node not in point set
c  and 0 if neither
      do i=1,mpno
         if(intp(mpary(i)).ne.0) intp(mpary(i))=2
      enddo
c  set inclusive flag
      if(nwds.ge.12) then
         if(msgtype(12).eq.3.and.cmsgin(12)(1:9).eq.'exclusive')
     *       linclusive=.false.
      endif
c  fill iseedtet
      do i=1,ntets
         ityp = itettyp(i)
         do nf = 1, nelmnef(ityp)
            k=itet1(itetoff(i)+nf)
            iseedtet(k)=i
            iseedtet(iparent(k))=i
         enddo
      enddo
C
C  loop through tets looking for tets all of whose node are on the
C  interface and are in pset
c  check element type - must be tet
c  all nodes must be interface nodes (intp /= 0)
C
      do it=1,ntets
         ityp=itettyp(it)
         if(ityp.ne.ifelmtet) go to 200
         isum=0
         do j=1,4
            i1=intp(itet1(itetoff(it)+j))
            if(i1.eq.0) go to 200
            isum=isum+i1
         enddo
         if(linclusive.and.isum.le.4) go to 200
         if(.not.linclusive.and.isum.ne.8) go to 200
c  all tests pass now look for interior edges
         do j=1,nelmnee(ityp)
            call get_elements_on_edge(it,j,nelts,ipielts,ipiedges,
     *            ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,
     *            ipiparent,nef, mbndry)
c  interior edges (not on interface ) will have only one
C  material surrounding them
c
            do k=2,nelts
                if(itetclr(ielts(k)).ne.itetclr(ielts(1))) go to 150
            enddo
 
c  this is interior edge, check if already in list from previously
c  found tet
            jp1=itet1(itetoff(it)+ielmedge1(1,j,ityp))
            jp2=itet1(itetoff(it)+ielmedge1(2,j,ityp))
            jmin=min(iparent(jp1),iparent(jp2))
            jmax=max(iparent(jp1),iparent(jp2))
            do k=1,nadd
                  if(jmin.eq.ipt1(k).and.jmax.eq.ipt2(k)) go to 150
            enddo
c  add this edge to refinement list
            nadd=nadd+1
            if(nadd.gt.naddmax) then
                  inc=100
                  naddmax=naddmax+100
                  call mmincblk('itadd',isubname,ipitadd,inc,icscode)
                  call mmincblk('ieadd',isubname,ipieadd,inc,icscode)
                  call mmincblk('iadd',isubname,ipiadd,inc,icscode)
                  call mmincblk('xadd',isubname,ipxadd,inc,icscode)
                  call mmincblk('yadd',isubname,ipyadd,inc,icscode)
                  call mmincblk('zadd',isubname,ipzadd,inc,icscode)
                  call mmincblk('ipt1',isubname,ipipt1,inc,icscode)
                  call mmincblk('ipt2',isubname,ipipt2,inc,icscode)
            endif
            itadd(nadd)=it
            ieadd(nadd)=j
            iadd(nadd)=0
            xadd(nadd)=(xic(jmin)+xic(jmax))/2.
            yadd(nadd)=(yic(jmin)+yic(jmax))/2.
            zadd(nadd)=(zic(jmin)+zic(jmax))/2.
            ipt1(nadd)=jmin
            ipt2(nadd)=jmax
 150        continue
         enddo
 200     continue
      enddo
c send edges off to be refined
      if(nadd.gt.0) call refine_edge_add(cmo,nadd,ipitadd,ipieadd,
     *      ipiadd,ipxadd,ipyadd,ipzadd)
      call mmrelprt(isubname,icscode)
      return
      end
