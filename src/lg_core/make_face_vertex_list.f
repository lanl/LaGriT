      subroutine make_face_vertex_list()
C
C
C#######################################################################
C
C     PURPOSE -
C
C        Convert LaGriT data structures to face, vertex lists.
C
 
C     INPUT ARGUMENTS -
C
C       none
C
C     OUTPUT ARGUMENTS -
C
C        none
C
C     CHANGE HISTORY -
C
C        $Log: make_face_vertex_list.f,v $
C        Revision 2.00  2007/11/05 19:46:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   16 Jun 2000 14:10:50   dcg
C
C#######################################################################
C
      implicit none
      include 'local_element.h'
C
C
C#######################################################################
C
      character*32 isubname, cmo,lists,cout
c
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      pointer (ipitet,itet),(ipitp,itp1),(ipisn,isn1)
      pointer (ipjtet,jtet)
      integer itet(*),jtet(*),itp1(*),isn1(*)
      pointer (ipitetoff,itetoff)
      pointer (ipjtetoff,jtetoff)
      integer itetoff(*),jtetoff(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic),(ipout,out),
     *   (ipiparent,iparent)
      real*8 xic(*),yic(*),zic(*),out(*),xcoord(8),ycoord(8),
     *  zcoord(8),epsilonl,epsilonv,volume1,volume2,dist1,
     *  distance_lg,dist2,volume3,volume4,ascend
      pointer (ipidrfaces,idrfaces), (ipidrverts,idrverts),
     * (ipfaces,faces),(ipverts,verts),(ipfacetoface,facetoface),
     * (ipfacetocell,facetocell)
      integer idrfaces(*),idrverts(*),faces(*),verts(*),
     *  facetoface(*), facetocell(*),iparent(*)
      integer icscode, nelements,nnodes,mbndry,leni,itype,nf,it,j,k,
     *  nfaces,nef,nverts,jt,iout,jf,jtt,ip1,ip2,ip3,ip4,
     *  ktt,kt,new(6),np(6),kf,lt,newfaces,jf2,ierror,jp1,jp2,newnode,
     *  oldnode,jp3,jp4,np1,np2,new1,new2,jf3,lf,ie,l,i,jtype,nfound,
     *  nelementsnew,ncandidates,m,icount
      pointer (ippreceed,preceed),(ipfollow,follow),(ipinsert,insert),
     *  (ipsum,sum),(ipindex,index)
      integer preceed(*),follow(*),insert(*),index(*),sum(*),
     *   isort(100),jsort(100),num(100)
C
      isubname='make_lists'
      lists='lists_lg'
      ascend=1.0
c
C -- get lagrit data
c
      call cmo_get_name(cmo,icscode)
C
      call cmo_get_intinfo('nnodes',cmo,nnodes,leni,itype,icscode)
      call cmo_get_intinfo('nelements',cmo,nelements,leni,itype,
     *  icscode)
      call cmo_get_intinfo('faces_per_element',cmo,nef,leni,itype,
     *  icscode)
      call cmo_get_intinfo('mbndry',cmo,mbndry,leni,itype,icscode)
      call cmo_get_info('itp1',cmo,ipitp,leni,itype,icscode)
      call cmo_get_info('isn1',cmo,ipisn,leni,itype,icscode)
      call cmo_get_info('itet',cmo,ipitet,leni,itype,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,itype,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,leni,itype,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,itype,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,itype,icscode)
      call cmo_get_info('xic',cmo,ipxic,leni,itype,icscode)
      call cmo_get_info('yic',cmo,ipyic,leni,itype,icscode)
      call cmo_get_info('zic',cmo,ipzic,leni,itype,icscode)
      call cmo_get_attinfo('epsilonl',cmo,iout,epsilonl,cout,ipout
     *    ,leni,itype,icscode)
      call cmo_get_attinfo('epsilonv',cmo,iout,epsilonv,cout,ipout
     *    ,leni,itype,icscode)
c
c  get memory for lists
 
      call mmgetblk('idrfaces',lists,ipidrfaces,nelements+1,1,icscode)
      call mmgetblk('idrverts',lists,ipidrverts,nelements*20,1,icscode)
      call mmgetblk('faces',lists,ipfaces,nelements*20,1,icscode)
      call mmgetblk('facetoface',lists,ipfacetoface,nelements*20,1,
     *  icscode)
      call mmgetblk('facetocell',lists,ipfacetocell,nelements*20,1,
     *  icscode)
      call mmgetblk('verts',lists,ipverts,nelements*100,1,icscode)
      call mmgetblk('preceed',isubname,ippreceed,nelements*20,1,icscode)
      call mmgetblk('follow',isubname,ipfollow,nelements*20,1,icscode)
      call mmgetblk('insert',isubname,ipinsert,nelements*20,1,icscode)
      call mmgetblk('iparent',isubname,ipiparent,nnodes,1,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
      nelementsnew=0
 
c
c  loop thru cells
      idrfaces(1)=1
      idrverts(1)=1
      nfaces=0
      nverts=0
      ncandidates=0
      do it=1,nelements
         itype=itettyp(it)
c
c  skip zero volume elements completely
c
         if(itype.eq.ifelmtri.or.itype.eq.ifelmpri) then
            do k=1,nelmnen(jtype)
               xcoord(k)=xic(itet(itetoff(it)+k))
               ycoord(k)=yic(itet(itetoff(it)+k))
               zcoord(k)=zic(itet(itetoff(it)+k))
            enddo
            call volume_element(itype,xcoord,ycoord,zcoord,
     *            volume1)
            if(volume1.lt.epsilonv*10.) go to 70
         endif
         nelementsnew=nelementsnew+1
         nf=nelmnef(itype)
         idrfaces(it+1)=idrfaces(it)+nf
         newfaces=0
c
c  loop thru faces
         do j=1,nf
            nfaces=nfaces+1
            faces(idrfaces(it)-1+j+newfaces)=nfaces
            idrverts(nfaces+1)=idrverts(nfaces)+ielmface0(j,itype)
c
c  set face to cell info
            facetocell(nfaces)=it
c
c  loop through nodes on face
            do k=1,ielmface0(j,itype)
               verts(idrverts(nfaces)+k-1)=itet(itetoff(it)+
     *              ielmface1(k,j,itype))
               nverts=nverts+1
            enddo
c
c  check for hanging node faces by testing for zero volume
c  triangle or prism neighbor
c
            jtt=jtet(jtetoff(it)+j)
            if(jtt.eq.mbndry) go to 50
            if(jtt.gt.mbndry)  jtt=jtt-mbndry
            jt=1+(jtt-1)/nef
            jf=jtt-nef*(jt-1)
            jtype=itettyp(jt)
            if(jtype.eq.ifelmtri.or.jtype.eq.ifelmpri) then
               do k=1,nelmnen(jtype)
                  xcoord(k)=xic(itet(itetoff(jt)+k))
                  ycoord(k)=yic(itet(itetoff(jt)+k))
                  zcoord(k)=zic(itet(itetoff(jt)+k))
               enddo
               call volume_element(jtype,xcoord,ycoord,zcoord,
     *            volume1)
               if(volume1.gt.epsilonv*10.) go to 50
c
c  neighbor is zero volume see if this face (edge) is the 'big'
c  face (edge) of the zero volume element.  If so add faces to
c  original element (it).
c  do triangle case first
c
               if(jtype.eq.ifelmtri) then
                  ip1=itet(itetoff(jt)+ielmface1(1,jf,jtype))
                  ip2=itet(itetoff(jt)+ielmface1(2,jf,jtype))
                  dist1=distance_lg(xic(ip1),yic(ip1),zic(ip1),
     *               xic(ip2),yic(ip2),zic(ip2),ierror)
                  jf2=jf+1
                  if(jf2.eq.4)jf2=1
                  jp1=itet(itetoff(jt)+ielmface1(1,jf2,jtype))
                  jp2=itet(itetoff(jt)+ielmface1(2,jf2,jtype))
                  dist2=distance_lg(xic(jp1),yic(jp1),zic(jp1),
     *               xic(jp2),yic(jp2),zic(jp2),ierror)
                  if(abs(dist1-2.*dist2).gt.epsilonl*100.) go to 50
c
c  big face (edge) so add facet
c
                  newfaces=newfaces+1
                  newnode=jp1
                  if(iparent(newnode).eq.iparent(ip1).or.
     *               iparent(newnode).eq.iparent(ip2)) newnode=jp2
                  idrfaces(it+1)=idrfaces(it+1)+1
                  nfaces=nfaces+1
                  faces(idrfaces(it)+j+newfaces)=nfaces
                  idrverts(nfaces)=idrverts(nfaces-1)+ielmface0(j,itype)
                  idrverts(nfaces+1)=idrverts(nfaces)+ielmface0(j,itype)
                  oldnode=verts(idrverts(nfaces-1)+ielmface0(j,itype)-1)
                  verts(idrverts(nfaces)-1)=newnode
                  verts(idrverts(nfaces))=newnode
                  verts(idrverts(nfaces)+1)=oldnode
                  facetocell(nfaces)=it
                  nverts=nverts+2
c
c  now do prism case -- quad faces of prism are local faces 3,4,5
c  if another quad face neighbor of this prism is another prism
c  then this face generates four faces for the polygon
c  otherwise it generates two - must figure out which edge is
c  bifurcated.
c  First see if we are on 'big' quad face
c
               elseif(jtype.eq.ifelmpri) then
                  ip1=itet(itetoff(jt)+ielmface1(1,jf,jtype))
                  ip2=itet(itetoff(jt)+ielmface1(2,jf,jtype))
                  ip3=itet(itetoff(jt)+ielmface1(3,jf,jtype))
                  ip4=itet(itetoff(jt)+ielmface1(4,jf,jtype))
                  do k=1,nelmnen(itype)
                     xcoord(k)=xic(itet(itetoff(jt)+
     *                       ielmface1(k,jf,jtype)))
                     ycoord(k)=yic(itet(itetoff(jt)+
     *                       ielmface1(k,jf,jtype)))
                     zcoord(k)=zic(itet(itetoff(jt)+
     *                       ielmface1(k,jf,jtype)))
                  enddo
                  call volume_element(ifelmqud,xcoord,ycoord,
     *               zcoord,volume1)
                  jf2=jf+1
                  if(jf2.eq.6)jf2=3
                  jp1=itet(itetoff(jt)+ielmface1(1,jf2,jtype))
                  jp2=itet(itetoff(jt)+ielmface1(2,jf2,jtype))
                  jp3=itet(itetoff(jt)+ielmface1(3,jf2,jtype))
                  jp4=itet(itetoff(jt)+ielmface1(4,jf2,jtype))
                  do k=1,nelmnen(itype)
                     xcoord(k)=xic(itet(itetoff(jt)+
     *                       ielmface1(k,jf2,jtype)))
                     ycoord(k)=yic(itet(itetoff(jt)+
     *                       ielmface1(k,jf2,jtype)))
                     zcoord(k)=zic(itet(itetoff(jt)+
     *                       ielmface1(k,jf2,jtype)))
                  enddo
                  call volume_element(ifelmqud,xcoord,ycoord,
     *               zcoord,volume2)
                  if(abs(volume1-2.*volume2).gt.epsilonv*100.) go to 50
c
c  big face (edge) so add facet(s)
c
                  jf2=jf+1
                  if(jf2.eq.6)jf2=3
                  ktt=jtet(jtetoff(jt)+jf2)
                  if(ktt.eq.mbndry) go to 50
                  if(ktt.gt.mbndry) ktt=ktt-mbndry
                  kt=1+(ktt-1)/nef
                  kf=ktt-nef*(kt-1)
                  if(itettyp(kt).ne.ifelmpri) then
c
c  only bifurcated -- figure out which direction (which edge is split)
c  look at area of triangles of endpoints of edges and new node -
c  area will be zero if node is on edge
c
                    if(jf.eq.3)then
                        np1=3
                        np2=6
                    elseif(jf.eq.4)then
                        np1=1
                        np2=4
                    elseif(jf.eq.5)then
                        np1=2
                        np2=5
                    endif
                    xcoord(1)=xic(itet(itetoff(it)+
     *                 ielmface1(1,j,itype)))
                    ycoord(1)=yic(itet(itetoff(it)+
     *                 ielmface1(1,j,itype)))
                    zcoord(1)=zic(itet(itetoff(it)+
     *                 ielmface1(1,j,itype)))
                    xcoord(2)=xic(itet(itetoff(it)+
     *                 ielmface1(2,j,itype)))
                    ycoord(2)=yic(itet(itetoff(it)+
     *                 ielmface1(2,j,itype)))
                    zcoord(2)=zic(itet(itetoff(it)+
     *                 ielmface1(2,j,itype)))
                    xcoord(3)=xic(itet(itetoff(jt)+
     *                 np1))
                    ycoord(3)=yic(itet(itetoff(jt)+
     *                 np1))
                    zcoord(3)=zic(itet(itetoff(jt)+
     *                 np1))
                    call volume_element(ifelmtri,xcoord,ycoord,
     *                 zcoord,volume1)
                    xcoord(3)=xic(itet(itetoff(jt)+
     *                 np2))
                    ycoord(3)=yic(itet(itetoff(jt)+
     *                 np2))
                    zcoord(3)=zic(itet(itetoff(jt)+
     *                 np2))
                    call volume_element(ifelmtri,xcoord,ycoord,
     *                 zcoord,volume2)
                    xcoord(2)=xic(itet(itetoff(it)+
     *                 ielmface1(4,j,itype)))
                    ycoord(2)=yic(itet(itetoff(it)+
     *                 ielmface1(4,j,itype)))
                    zcoord(2)=zic(itet(itetoff(it)+
     *                 ielmface1(4,j,itype)))
                    xcoord(3)=xic(itet(itetoff(jt)+
     *                 np1))
                    ycoord(3)=yic(itet(itetoff(jt)+
     *                 np1))
                    zcoord(3)=zic(itet(itetoff(jt)+
     *                 np1))
                    call volume_element(ifelmtri,xcoord,ycoord,
     *                 zcoord,volume3)
                    xcoord(3)=xic(itet(itetoff(jt)+
     *                 np2))
                    ycoord(3)=yic(itet(itetoff(jt)+
     *                 np2))
                    zcoord(3)=zic(itet(itetoff(jt)+
     *                 np2))
                    call volume_element(ifelmtri,xcoord,ycoord,
     *                 zcoord,volume4)
                    if(volume1.lt.epsilonv*10..or.volume2.lt.
     *                 epsilonv*10.) then
c  spilt is between node 1 and node 2 of original face
                       if(volume1.lt.epsilonv*10.) then
                          new1=itet(itetoff(jt)+np1)
                          new2=itet(itetoff(jt)+np2)
                       elseif(volume2.lt.epsilonv*10.) then
                          new1=itet(itetoff(jt)+np2)
                          new2=itet(itetoff(jt)+np1)
                       endif
                       ncandidates=ncandidates+1
                       preceed(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(1,j,itype))
                       follow(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(2,j,itype))
                       insert(ncandidates)=new1
                       ncandidates=ncandidates+1
                       preceed(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(3,j,itype))
                       follow(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(4,j,itype))
                       insert(ncandidates)=new2
                       newfaces=newfaces+1
                       idrfaces(it+1)=idrfaces(it+1)+1
                       nfaces=nfaces+1
                       faces(idrfaces(it)+j+newfaces-1)=nfaces
                       idrverts(nfaces)=idrverts(nfaces-1)+
     *                    ielmface0(j,itype)
                       idrverts(nfaces+1)=idrverts(nfaces)+
     *                    ielmface0(j,itype)
                       verts(idrverts(nfaces)+1)=
     *                  verts(idrverts(nfaces-1)+1)
                       verts(idrverts(nfaces)+2)=
     *                  verts(idrverts(nfaces-1)+2)
                       verts(idrverts(nfaces-1)+1)=new1
                       verts(idrverts(nfaces-1)+2)=new2
                       verts(idrverts(nfaces))=new1
                       verts(idrverts(nfaces)+3)=new2
                       facetocell(nfaces)=it
                       nverts=nverts+4
                    elseif(volume3.lt.epsilonv*10..or.volume4.lt.
     *                 epsilonv*10.) then
c  spilt is between node 1 and node 4 of original face
                       if(volume3.lt.epsilonv*10.) then
                          new1=itet(itetoff(jt)+np1)
                          new2=itet(itetoff(jt)+np2)
                       elseif(volume4.lt.epsilonv*10.) then
                          new1=itet(itetoff(jt)+np2)
                          new2=itet(itetoff(jt)+np1)
                       endif
                       ncandidates=ncandidates+1
                       preceed(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(1,j,itype))
                       follow(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(4,j,itype))
                       insert(ncandidates)=new1
                       ncandidates=ncandidates+1
                       preceed(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(2,j,itype))
                       follow(ncandidates)=itet(itetoff(it)+
     *                             ielmface1(3,j,itype))
                       insert(ncandidates)=new2
                       newfaces=newfaces+1
                       idrfaces(it+1)=idrfaces(it+1)+1
                       nfaces=nfaces+1
                       faces(idrfaces(it)+j+newfaces-1)=nfaces
                       idrverts(nfaces)=idrverts(nfaces-1)+
     *                    ielmface0(j,itype)
                       idrverts(nfaces+1)=idrverts(nfaces)+
     *                    ielmface0(j,itype)
                       verts(idrverts(nfaces)+2)=
     *                  verts(idrverts(nfaces-1)+2)
                       verts(idrverts(nfaces)+3)=
     *                  verts(idrverts(nfaces-1)+3)
                       verts(idrverts(nfaces-1)+2)=new2
                       verts(idrverts(nfaces-1)+3)=new1
                       verts(idrverts(nfaces))=new1
                       verts(idrverts(nfaces)+1)=new2
                       facetocell(nfaces)=it
                       nverts=nverts+4
                    endif
c
c  need three new faces - 5 new vertices
c  get other two vertices from first prism
c
                  else
                    if(jf.eq.3)then
                        np(1)=itetoff(jt)+3
                        np(2)=itetoff(jt)+6
                    elseif(jf.eq.4)then
                        np(1)=itetoff(jt)+1
                        np(2)=itetoff(jt)+4
                    elseif(jf.eq.5)then
                        np(1)=itetoff(jt)+2
                        np(2)=itetoff(jt)+5
                    endif
c  get next two vertices from next prism
                    if(kf.eq.3)then
                        np(3)=itetoff(kt)+3
                        np(4)=itetoff(kt)+6
                    elseif(kf.eq.4)then
                        np(3)=itetoff(kt)+1
                        np(4)=itetoff(kt)+4
                    elseif(kf.eq.5)then
                        np(3)=itetoff(kt)+2
                        np(4)=itetoff(kt)+5
                    endif
c  get last two vertices from last prism
                    jf3=jf2+1
                    if(jf3.eq.6)jf3=3
                    ktt=jtet(jtetoff(jt)+jf3)
                    if(ktt.eq.mbndry) go to 50
                    if(ktt.gt.mbndry) ktt=ktt-mbndry
                    lt=1+(ktt-1)/nef
                    lf=ktt-nef*(kt-1)
                    if(kf.eq.3)then
                        np(5)=itetoff(lt)+3
                        np(6)=itetoff(lt)+6
                    elseif(kf.eq.4)then
                        np(5)=itetoff(lt)+1
                        np(6)=itetoff(lt)+4
                    elseif(kf.eq.5)then
                        np(5)=itetoff(lt)+2
                        np(6)=itetoff(lt)+5
                    endif
c
c  find which vertex is midpoint of which edge
c  loop thru edges then thru candidate nodes
c
                    nfound=0
                    do k=1,ielmface0(j,itype)
                       ie=ielmface2(k,j,itype)
                       ip1=itet(itetoff(it)+ielmedge1(1,ie,itype))
                       ip2=itet(itetoff(it)+ielmedge1(2,ie,itype))
                       xcoord(1)=xic(ip1)
                       ycoord(1)=yic(ip1)
                       zcoord(1)=zic(ip1)
                       xcoord(2)=xic(ip2)
                       ycoord(2)=yic(ip2)
                       zcoord(2)=zic(ip2)
                       do l=1,6
                          if(np(l).ne.0) then
                             xcoord(3)=xic(itet(np(l)))
                             ycoord(3)=yic(itet(np(l)))
                             zcoord(3)=zic(itet(np(l)))
                             call volume_element(ifelmtri,xcoord,
     *                         ycoord,zcoord,volume1)
                             if(volume1.lt.epsilonv*100.) then
                                nfound=nfound+1
                                new(k)=itet(np(l))
                                np(l)=0
                                ncandidates=ncandidates+1
                                preceed(ncandidates)=ip1
                                follow(ncandidates)=ip2
                                insert(ncandidates)=new(k)
                             endif
                          endif
                       enddo
 15                    continue
                    enddo
                    do l=1,6
                       if(np(l).ne.0) new(5)=itet(np(l))
                    enddo
                    if(nfound.lt.4) go to 50
                    newfaces=newfaces+1
                    ip1=itet(itetoff(it)+ielmface1(1,j,itype))
                    ip2=itet(itetoff(it)+ielmface1(2,j,itype))
                    ip3=itet(itetoff(it)+ielmface1(3,j,itype))
                    ip4=itet(itetoff(it)+ielmface1(4,j,itype))
                    idrfaces(it+1)=idrfaces(it+1)+3
                    nfaces=nfaces+1
                    faces(idrfaces(it)+j+newfaces-1)=nfaces
                    idrverts(nfaces)=idrverts(nfaces-1)+
     *                    ielmface0(j,itype)
                    idrverts(nfaces+1)=idrverts(nfaces)+
     *                    ielmface0(j,itype)
                    verts(idrverts(nfaces-1))=ip1
                    verts(idrverts(nfaces-1)+1)=new(1)
                    verts(idrverts(nfaces-1)+2)=new(5)
                    verts(idrverts(nfaces-1)+3)=new(4)
                    verts(idrverts(nfaces))=new(1)
                    verts(idrverts(nfaces)+1)=ip2
                    verts(idrverts(nfaces)+2)=new(2)
                    verts(idrverts(nfaces)+3)=new(5)
                    facetocell(nfaces)=it
                    nfaces=nfaces+1
                    newfaces=newfaces+1
                    faces(idrfaces(it)+j+newfaces-1)=nfaces
                    idrverts(nfaces)=idrverts(nfaces-1)+
     *                    ielmface0(j,itype)
                    idrverts(nfaces+1)=idrverts(nfaces)+
     *                    ielmface0(j,itype)
                    verts(idrverts(nfaces))=new(4)
                    verts(idrverts(nfaces)+1)=new(5)
                    verts(idrverts(nfaces)+2)=new(3)
                    verts(idrverts(nfaces)+3)=ip4
                    facetocell(nfaces)=it
                    nfaces=nfaces+1
                    newfaces=newfaces+1
                    faces(idrfaces(it)+j+newfaces-1)=nfaces
                    idrverts(nfaces)=idrverts(nfaces-1)+
     *                    ielmface0(j,itype)
                    idrverts(nfaces+1)=idrverts(nfaces)+
     *                    ielmface0(j,itype)
                    verts(idrverts(nfaces))=new(5)
                    verts(idrverts(nfaces)+1)=new(2)
                    verts(idrverts(nfaces)+2)=ip3
                    verts(idrverts(nfaces)+3)=new(3)
                    facetocell(nfaces)=it
                    nverts=nverts+12
                  endif
               endif
            endif
 50         continue
c  end loop on faces
         enddo
 70      continue
c  end loop on elements
      enddo
c
c  look at all the triplets formed by the bisected edges
c  we have the node numbers of the bisected edges and the
c  node number of the bisection node -- if any edges
c  on any faces are still unbisected, fix them up here
c
      do it=1,nelementsnew
         do i=1,ncandidates
            do j=idrfaces(it),idrfaces(it+1)-1
               do k=idrverts(j),idrverts(j+1)-1
                  if((verts(k).eq.preceed(i).and.verts(k+1).eq.
     *                follow(i)).or.(verts(k).eq.follow(i).and.
     *                verts(k+1).eq.preceed(i))) then
c
c  bisection node must be inserted here
c
                     nverts=nverts+1
                     do l=nverts,k+2,-1
                        verts(l)=verts(l-1)
                     enddo
                     verts(k+1)=insert(i)
                     do l=j+1,nfaces+1
                        idrverts(l)=idrverts(l)+1
                     enddo
                     go to 90
                  endif
               enddo
 90            continue
            enddo
         enddo
c  end loop on elements
      enddo
 
C
      call mmrelprt(isubname,ierror)
 
c
c  loop through and fill in neighbor info
c  sort on sum of vertex numbers of faces
c
      call mmgetblk('sum',isubname,ipsum,nfaces+1,1,ierror)
      call mmgetblk('index',isubname,ipindex,nfaces+1,1,ierror)
      do it=1,nelementsnew
         do j=idrfaces(it),idrfaces(it+1)-1
            index(j)=j
            sum(faces(j))=0
            do k=idrverts(j),idrverts(j+1)-1
               sum(faces(j))=sum(faces(j))+verts(k)
            enddo
         enddo
      enddo
      call hpsortip(nfaces,sum,ascend,index)
      icount=0
      do nf=2,nfaces
         if(sum(faces(index(nf))).eq.sum(faces(index(nf-1)))) then
            icount=icount+1
         else
            if(icount.lt.1) then
               facetoface(faces(index(nf-1)))=0
            elseif(icount.eq.1) then
               facetoface(faces(index(nf-1)))=faces(index(nf-2))
               facetoface(faces(index(nf-2)))=faces(index(nf-1))
               icount=0
            else
               icount=icount+1
               do k=1,icount
                  num(k)=idrverts(faces(index(nf-icount-1+k))+1)-
     *                   idrverts(faces(index(nf-icount-1+k)))
               enddo
               do k=1,icount
                  if(num(k).eq.0) go to 120
                  do l=1,num(k)
                     isort(l)=l
                  enddo
                  call hpsortip(num(k),verts(idrverts(faces(
     *                index(nf-icount-1+k)))),ascend,isort)
                  do l=k+1,icount
                     if(num(l).eq.0) go to 110
                     if(num(l).ne.num(k)) goto 110
                     do m=1,num(l)
                        jsort(m)=m
                     enddo
                     call hpsortip(num(l),verts(idrverts(faces(
     *                   index(nf-icount-1+l)))),ascend,jsort)
                     do m=1,num(l)
                        if(verts(idrverts(faces(index(nf-icount-1+k)))
     *                    +isort(m)-1).ne.
     *                     verts(idrverts(faces(index(nf-icount-1+l)))
     *                    +jsort(m)-1))
     *                     go to 110
                     enddo
c
c  this pair matches
c
                     facetoface(faces(index(nf-icount-1+k)))=
     *                    faces(index(nf-icount-1+l))
                     facetoface(faces(index(nf-icount-1+l)))=
     *                    faces(index(nf-icount-1+k))
                     num(k)=0
                     num(l)=0
                     go to 120
 110                 continue
                  enddo
c
c  no match must be a single
c
                  facetoface(faces(index(nf-icount-1+k)))=0
 120              continue
               enddo
               icount=0
            endif
         endif
      enddo
      call mmrelprt(isubname,ierror)
c
      print *,nfaces,nverts,nelementsnew
      do it=1,nelementsnew
         print *,'it ',it,idrfaces(it)
         do j=idrfaces(it),idrfaces(it+1)-1
            print *,'face ',j,idrverts(j),facetoface(j),facetocell(j)
            print *,(verts(k),k=idrverts(j),idrverts(j+1)-1)
         enddo
      enddo
      call mmrelprt(isubname,ierror)
      return
      end
