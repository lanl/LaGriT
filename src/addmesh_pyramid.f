*dk addmesh_pyramid
      subroutine addmesh_pyramid(cmotet,cmohex,ier)
C
C#############################################################
C
C     PURPOSE
C     THIS ROUTINE CONSTRUCTS PYRAMID ELEMENTS TO JOIN A HEX
C     MESH WITH A TET MESH.  THE MESH BOUNDARY NODES MUST
C     MATCH EXACTLY.
C    ON EXIT THE TET MESH WILL HAVE BEEN REPLACED BY A
C    HYBRID MESH WHICH WILL CONTAIN THE ADDED PYRAMID ELEMENTS
C    ALSO EDGE COMMON TO THE PAIR OF TETS WITH MATCH THE
C    OPPOSING HEX WILL HAVE BEEN REFINED.
C
C     INPUT
C     cmohex - name of hexmesh
C     cmotet - name of tetmesh
C
C     OUTPUT
C     cmotet - name of modified tet mesh ( will have refined
C            boundary tets and added pyramid elements.
C     ier    - error return 0 means no errors.
C
C $Log: addmesh_pyramid.f,v $
C Revision 2.00  2007/11/05 19:45:46  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   25 Aug 2000 11:07:38   dcg
CPVCS    use kdtree to find matching hex/tet faces
CPVCS    
CPVCS       Rev 1.7   Thu Jan 28 13:52:56 1999   dcg
CPVCS    change test for degenerate faces
CPVCS
CPVCS       Rev 1.6   Mon Feb 09 13:55:24 1998   dcg
CPVCS    make master cmo into a hybrid cmo after the
CPVCS    call to refine - not before
CPVCS
CPVCS       Rev 1.5   Fri Nov 07 13:05:48 1997   dcg
CPVCS    correct point order of pyramids to get positive volume
CPVCS    elements
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:38:50 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Tue Oct 29 11:50:20 1996   dcg
CPVCS    ignore degenerate faces of hexes
CPVCS
CPVCS       Rev 1.2   Mon Jul 29 15:02:44 1996   dcg
CPVCS    Original library version
C#############################################################
C
      implicit none
      include 'local_element.h'
C
      integer npyr,nnhex,nfhex,ihx,ilen,ity,ier,nf,nnodes,
     *    ntet_match,nftet,it,nft,mbndry_tet,mbndry_hex,ityp,
     *    imatch,i,j,i1,i2,n,k,ipt(3),iph(4),ntetnew,j1,j2,
     *    itmatch(2),ifmatch(2),ip4(2),isave(4),ityph,ntet,nhex,
     *    nntet,kk,ierror,strlen,icharlnf,ierror2,itt,numfound
      real*8 eps
      character*32 isubname,cmotet,cmohex
      character*132 cmdmess
      pointer (ipipyr,ipyr)
      integer ipyr(4,1000000)
      pointer (ipimt,imt)
      pointer (ipitptet,itptet)
      integer imt(1000000), itptet(1000000)
      pointer (ipitphex,itphex)
      integer itphex(1000000)
      pointer (ipihex,ihex)
      pointer (ipihexoff,ihexoff)
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      pointer (ipitettyp,itettyp)
      pointer (ipihextyp,ihextyp)
      pointer (ipjhex,jhex)
      pointer (ipjhexoff,jhexoff)
      pointer (ipjtet,jtet)
      pointer (ipjtetoff,jtetoff)
      pointer (ipxtet,xtet)
      pointer (ipytet,ytet)
      pointer (ipztet,ztet)
      pointer (ipxhex,xhex)
      pointer (ipyhex,yhex)
      pointer (ipzhex,zhex)
      real*8 xtet(1000000),ytet(1000000),ztet(1000000),xhex(1000000),
     * yhex(1000000),zhex(1000000)
      integer itettyp(1000000),ihextyp(1000000),itet(1000000),
     * jtet(1000000),jhex(1000000),itetoff(1000000),jtetoff(1000000),
     * ihexoff(1000000),jhexoff(1000000),ihex(1000000)
C
      pointer (ipiadd,iadd)
      pointer (ipieadd,ieadd)
      pointer (ipitadd,itadd)
      pointer (ipxadd,xadd)
      pointer (ipyadd,yadd)
      pointer (ipzadd,zadd)
      pointer (ipzadd_new,zadd_new)
      pointer (ipxadd_new,xadd_new)
      pointer (ipyadd_new,yadd_new)
      real*8 xadd_new(1000000),yadd_new(1000000)
      real*8 xadd(1000000),yadd(1000000),zadd(1000000),zadd_new(1000000)
      integer itadd(1000000),ieadd(1000000),iadd(1000000)
C     K-d tree type stuff
      pointer (iplinkto,linkto)
      pointer (ipsboxo,sboxo)
      pointer (ipitfound,itfound)
      integer linkto(*),itfound(*)
      real*8 sboxo(2,3,*)
      real*8 xnodes1(12),ynodes1(12),znodes1(12)
C
C ############################################################
 
C
      isubname='addmesh_pyramid'
      ier=0
C
C   get tet mesh info
C
      call cmo_get_info('nelements',cmotet,ntet,ilen,ity,ier)
      call cmo_get_info('nnodes',cmotet,nnodes,ilen,ity,ier)
      call cmo_get_info('mbndry',cmotet,mbndry_tet,ilen,ity,ier)
      call cmo_get_info('nodes_per_element',cmotet,nntet,ilen,ity,ier)
      call cmo_get_info('faces_per_element',cmotet,nftet,ilen,ity,ier)
      call cmo_get_info('itettyp',cmotet,ipitettyp,ilen,ity,ier)
      call cmo_get_info('itet',cmotet,ipitet,ilen,ity,ier)
      call cmo_get_info('itetoff',cmotet,ipitetoff,ilen,ity,ier)
      call cmo_get_info('jtetoff',cmotet,ipjtetoff,ilen,ity,ier)
      call cmo_get_info('jtet',cmotet,ipjtet,ilen,ity,ier)
      call cmo_get_info('xic',cmotet,ipxtet,ilen,ity,ier)
      call cmo_get_info('yic',cmotet,ipytet,ilen,ity,ier)
      call cmo_get_info('zic',cmotet,ipztet,ilen,ity,ier)
      call cmo_get_info('itp1',cmotet,ipitptet,ilen,ity,ier)
C
C    get hex mesh information
C
      call cmo_get_info('nelements',cmohex,nhex,ilen,ity,ier)
      call cmo_get_info('mbndry',cmohex,mbndry_hex,ilen,ity,ier)
      call cmo_get_info('nodes_per_element',cmohex,nnhex,ilen,ity,ier)
      call cmo_get_info('faces_per_element',cmohex,nfhex,ilen,ity,ier)
      call cmo_get_info('itettyp',cmohex,ipihextyp,ilen,ity,ier)
      call cmo_get_info('itet',cmohex,ipihex,ilen,ity,ier)
      call cmo_get_info('itetoff',cmohex,ipihexoff,ilen,ity,ier)
      call cmo_get_info('jtetoff',cmohex,ipjhexoff,ilen,ity,ier)
      call cmo_get_info('jtet',cmohex,ipjhex,ilen,ity,ier)
      call cmo_get_info('xic',cmohex,ipxhex,ilen,ity,ier)
      call cmo_get_info('yic',cmohex,ipyhex,ilen,ity,ier)
      call cmo_get_info('zic',cmohex,ipzhex,ilen,ity,ier)
      call cmo_get_info('itp1',cmohex,ipitphex,ilen,ity,ier)
C
C   allocate temporary storage
C
      k=ntet/2+1
      call mmgetblk('itadd',isubname,ipitadd,k,1,ier)
      call mmgetblk('ipadd',isubname,ipiadd,k,1,ier)
      call mmgetblk('ieadd',isubname,ipieadd,k,1,ier)
      call mmgetblk('xadd',isubname,ipxadd,k,1,ier)
      call mmgetblk('yadd',isubname,ipyadd,k,1,ier)
      call mmgetblk('zadd',isubname,ipzadd,k,1,ier)
      call mmgetblk('xadd_new',isubname,ipxadd_new,k,1,ier)
      call mmgetblk('yadd_new',isubname,ipyadd_new,k,1,ier)
      call mmgetblk('zadd_new',isubname,ipzadd_new,k,1,ier)
      call mmgetblk('ipyr',isubname,ipipyr,k*4,1,ier)
C
      call get_epsilon('epsilonl',eps)
      npyr=0
C
C     Find out if the tet cmo has a K-d tree built already, if it
C     doesn't, build one for it via the kdtree command.
c
      ierror = 0
      strlen = icharlnf(cmotet)
      call cmo_get_info('linkt',cmotet,iplinkto,ilen,ity,ierror)
      call cmo_get_info('sbox',cmotet,ipsboxo,ilen,ity,ierror2)
      if((ierror.ne.0).or.(ierror2.ne.0)) then
         ierror = 0
         strlen = icharlnf(cmotet)
         cmdmess = 'cmo/select/' // cmotet(1:strlen) // '; finish'
         call dotaskx3d(cmdmess,ierror)
         cmdmess = 'kdtree/build; finish'
         call dotaskx3d(cmdmess,ierror)
         call cmo_get_info('linkt',cmotet,iplinkto,ilen,ity,ierror)
         call cmo_get_info('sbox',cmotet,ipsboxo,ilen,ity,ierror)
      endif
c
C     Allocate memory for the kdtree search results
c
      call mmgetblk('itfound',isubname,ipitfound,ntet,1,ierror)
C
C    look for hex elements with boundary faces that pair up with
C    two boundary tet elements
C
C
C   begin loop on hex elements
C
      do ihx=1,nhex
         if(ihextyp(ihx).ne.ifelmhex) go to 200
         ityph=ihextyp(ihx)
C
C   only look at hexes with boundary faces
         do nf=1,nfhex
            ntet_match=0
            if(jhex(jhexoff(ihx)+nf).eq.mbndry_hex) then
C
C   skip degenerate faces
C
               do i=1,ielmface0(nf,ityph)-1
                  k=ihex(ihexoff(ihx)+ielmface1(i,nf,ityph))
                  do j=i+1,ielmface0(nf,ityph)
                     kk=ihex(ihexoff(ihx)+ielmface1(j,nf,ityph))
                     if ((abs(xhex(k)-xhex(kk)).lt.eps) .and.
     *                   (abs(yhex(k)-yhex(kk)).lt.eps) .and.
     *                   (abs(zhex(k)-zhex(kk)).lt.eps)) go to 195
                  enddo
               enddo

c
C        Run through the nodes in the current element and create a
C        bounding box that specifies the search area for the
C        k-D tree subroutine.
C
 15            do j = 1,ielmface0(nf,ityph)
                  k = ihex(ihexoff(ihx)+ielmface1(j,nf,ityph))
                  xnodes1(j) = xhex(k)
                  ynodes1(j) = yhex(k)
                  znodes1(j) = zhex(k)
               enddo
C
C        Find the elements in the tet mesh that
C        are "close" to the element in question using the k-D tree.
c
               call kDtreeselect(ifelmqud,xnodes1,ynodes1,
     &          znodes1,linkto,sboxo,numfound,itfound,ierror)
C
C   begin loop on potential matching tet elements
C
 
               do itt = 1,numfound
                  it=itfound(itt)
                  if(itettyp(it).ne.ifelmtet) go to 190
                  ityp=itettyp(it)
C
C   begin loop on tet element faces
C
                  do nft=1,nftet
                     if (jtet(jtetoff(it)+nft).ne.mbndry_tet)
     *                       go to 180
C
C  look for 3 tet points matching 3 of 4 hex points
C
                     imatch=0
                     do i=1,ielmface0(nft,ityp)
                        ipt(i)=itet(itetoff(it)+ielmface1(i,nft,ityp))
                        do j=1,ielmface0(nf,ityph)
                          iph(j)=ihex(ihexoff(ihx)+
     *                        ielmface1(j,nf,ityph))
                           if(abs(xtet(ipt(i))-xhex(iph(j))).le.eps
     *                        .and.
     *                        abs(ytet(ipt(i))-yhex(iph(j))).le.eps
     *                        .and.
     *                        abs(ztet(ipt(i))-zhex(iph(j))).le.eps) 
     *                        then
                              imatch=imatch+1
                              isave(j)=ipt(i)
                           endif
                        enddo
                     enddo
C
C  found match so save points for later and accumulate lists for refine
C  reset point types on pyramid base to zero
C
                     if (imatch.eq.3) then
                         ntet_match=ntet_match+1
                         itmatch(ntet_match)=it
                         ifmatch(ntet_match)=nft
                         if (ntet_match.eq.2) then
                            npyr=npyr+1
                            ipyr(1,npyr)=isave(1)
                            ipyr(2,npyr)=isave(2)
                            ipyr(3,npyr)=isave(3)
                            ipyr(4,npyr)=isave(4)
                            do k=1,4
                               itphex(iph(k))=0
                            enddo
                            itadd(npyr)=itmatch(1)
                            iadd(npyr)=nnodes+npyr
C
C  find edge for refine
C
                            do i=1,nelmnee(ityp)
                               i1=itet(itetoff(itmatch(1))+
     *                                       ielmedge1(1,i,ityp))
                               i2=itet(itetoff(itmatch(1))+
     *                                       ielmedge1(2,i,ityp))
                               do k=1,nelmnee(ityp)
                                  j1=itet(itetoff(itmatch(2))+
     *                                       ielmedge1( 1,k,ityp))
                                  j2=itet(itetoff(itmatch(2))+
     *                                       ielmedge1( 2,k,ityp))
                                 n=0
                                  do j=1,ielmface0(nf,ityph)
                                     if (((i1.eq.j1.and.i2.eq.j2).or.
     *                                 (i1.eq.j2.and.i2.eq.j1)).and.
     *                                 (i1.eq.isave(j).or.i2.eq.
     *                                              isave(j))) n=n+1
                                     if (n.ge.2) then
                                        ieadd(npyr)=i
                                        xadd(npyr)=0.5*(xtet(i1)+
     *                                    xtet(i2))
                                        yadd(npyr)=0.5*(ytet(i1)+
     *                                    ytet(i2))                                 
                                        zadd(npyr)=0.5*(ztet(i1)+
     *                                    ztet(i2))
                                        go to  50
                                     endif
                                  enddo
                               enddo
                            enddo
                            npyr=npyr-1
                            go to 185
 
C  calculate new postion for this point which will become pyramid peak
C  make it 1/10 the distance from refined point to midpoint of line
C  connecting 4th points of matching tets
C
 50                        do k=1,2
                              ip4(k)=0
                              do j=1,nntet
                                 do i=1,ielmface0(ifmatch(k),ityp)
                                    ipt(i)=itet(itetoff(itmatch(k))+
     *                                   ielmface1(i,ifmatch(k),ityp))
                                    if(itet(itetoff(itmatch(k))+j)
     *                                   .eq.ipt(i))go to 100
                                 enddo
                                 ip4(k)=itet(itetoff(itmatch(k))+j)
 100                             continue
                              enddo
                           enddo
                           if (ip4(1).eq.0.or.ip4(2).eq.0) then
                              npyr=npyr-1
                              go to 185
                           endif
                           xadd_new(npyr)=.05*(xtet(ip4(1))+
     *                                 xtet(ip4(2)))+
     *                                 .9*xadd(npyr)
                           yadd_new(npyr)=.05*(ytet(ip4(1))+
     *                                 ytet(ip4(2)))+
     *                                 .9*yadd(npyr)
                           zadd_new(npyr)=.05*(ztet(ip4(1))+
     *                                 ztet(ip4(2)))+
     *                                 .9*zadd(npyr)
                           go to 195
                        else
c  
c  found one matching tet for this hex face
c  look for the next
c
                           go to 185
                        endif
                     endif
c 
c  get next tet face
c
 180                 continue
                  enddo
c
c  get next tet
c
 185              continue
               enddo
 190           continue
             endif
c
c  get next hex face
c
 195        continue
         enddo
c
c  get next hex
c
 200     continue
      enddo
C
C  send off edges to be refined
C
      if (npyr.eq.0) go to 9999
      call cmo_set_info('faces_per_element',cmotet,nftet,
     *        1,1,ier)
      call cmo_set_info('nodes_per_element',cmotet,nntet,
     *        1,1,ier)
C
C  add nodes to be refined
C
      nnodes=nnodes+npyr
      call cmo_set_info('nnodes',cmotet,nnodes,1,1,ier)
      call cmo_newlen(cmotet,ier)
      call cmo_get_info('xic',cmotet,ipxtet,ilen,ity,ier)
      call cmo_get_info('yic',cmotet,ipytet,ilen,ity,ier)
      call cmo_get_info('zic',cmotet,ipztet,ilen,ity,ier)
      call cmo_get_info('imt1',cmotet,ipimt,ilen,ity,ier)
      call cmo_get_info('itp1',cmotet,ipitptet,ilen,ity,ier)
      do i=1,npyr
         xtet(iadd(i))=xadd(i)
         ytet(iadd(i))=yadd(i)
         ztet(iadd(i))=zadd(i)
         imt(iadd(i))=
     *      imt(itet(itetoff(itadd(i)+ielmedge1(1,ieadd(i),ityp))))
         itptet(iadd(i))=0
      enddo
      call refine_edge_add(cmotet,npyr,ipitadd,ipieadd,ipiadd,
     *         ipxadd,ipyadd,ipzadd)
C
C   move point added by refine and add pyramids to tet grid
C
      call cmo_get_info('nelements',cmotet,ntet,ilen,ity,ier)
      ntetnew=ntet+npyr
      call cmo_set_info('nelements',cmotet,ntetnew,1,1,ier)
      call cmo_set_info('faces_per_element',cmotet,nelmnef(ifelmhyb),
     *        1,1,ier)
      call cmo_set_info('nodes_per_element',cmotet,nelmnen(ifelmhyb),
     *        1,1,ier)
      call cmo_set_info('edges_per_element',cmotet,nelmnee(ifelmhyb),
     *        1,1,ier)
      call cmo_newlen(cmotet,ier)
      call cmo_get_info('itet',cmotet,ipitet,ilen,ity,ier)
      call cmo_get_info('itetoff',cmotet,ipitetoff,ilen,ity,ier)
      call cmo_get_info('jtetoff',cmotet,ipjtetoff,ilen,ity,ier)
      call cmo_get_info('itettyp',cmotet,ipitettyp,ilen,ity,ier)
      do i=1,npyr
        itettyp(ntet+i)=ifelmpyr
        itetoff(ntet+i)=itetoff(ntet+i-1)+nelmnen(itettyp(ntet+i-1))
        jtetoff(ntet+i)=jtetoff(ntet+i-1)+nelmnef(itettyp(ntet+i-1))
        itet(itetoff(ntet+i)+1)=ipyr(1,i)
        itet(itetoff(ntet+i)+2)=ipyr(2,i)
        itet(itetoff(ntet+i)+3)=ipyr(3,i)
        itet(itetoff(ntet+i)+4)=ipyr(4,i)
        itet(itetoff(ntet+i)+5)=iadd(i)
         xtet(iadd(i))=xadd_new(i)
         ytet(iadd(i))=yadd_new(i)
         ztet(iadd(i))=zadd_new(i)
      enddo
C
 9999 continue
C
      call addmesh_merge(cmotet,cmohex,ier)
C
      call mmrelprt(isubname,ier)
      return
      end
