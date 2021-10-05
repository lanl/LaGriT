*dk,refineed
      subroutine refine_edge()
C
C     CHANGE HISTORY -
C
C        $Log: refine_edge.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   05 May 2000 15:22:42   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 16:59:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Mon Jun 03 15:13:08 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.6   Tue Apr 30 11:27:44 1996   dcg
CPVCS    replace literals in argument lists with consts variables
CPVCS
CPVCS       Rev 1.5   11/07/95 17:24:18   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.4   09/29/95 09:14:08   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.3   06/05/95 10:37:00   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.2   05/26/95 13:17:46   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.1   03/17/95 21:11:40   het
CPVCS    Add the model and dictionary calles
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:50   pvcs
CPVCS    Original version.
C
      implicit none

      include 'consts.h'
C
      integer nplen,nvalues
      parameter (nplen=10000000)
      parameter (nvalues=2)
C
      pointer (ipitetclr, itetclr(*))
      pointer (ipitetoff, itetoff(*))
      pointer (ipjtetoff, jtetoff(*))
      pointer (ipitettyp, itettyp(*))
      integer itetclr,itetoff,jtetoff,itettyp

      pointer (ipitet, itet)
      pointer (ipjtet,  jtet)
      integer itet(4,*), jtet(4,*)

      pointer (ipitflag, itflag)
      integer itflag(*)

      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      integer itetnn(4,*),itetnn1(4,*),itetnn2(4,*)

      pointer (ipiedge_tet, iedge_tet)
      pointer (ipiedge_face, iedge_face)
      pointer (ipiedge_edge, iedge_edge)
      integer iedge_tet(*), iedge_face(*),
     *        iedge_edge(*)
C
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      integer list_sink(*), list_source(nvalues,*)

      integer ierror,ier,ierrw,ics,icscode
      integer npoints,length,icmotype,ntets,mbndry,nen,nef,
     *        icount,ibound,i,i1,i2,i3,i4,it,nedge,iedge,
     *        j,j1,j2,j3,l,l1,l2,l3,lit,li,lj,k1,k2,k3
      integer lenitetclr,lenitettyp,lenitetoff,lenjtetoff,
     *        lenitet,lenjtet,lenxic,lenyic,lenzic
      integer nadd1,iedgeiter,npointsnew,ntetsnew,nedge_save,
     *        nelementsmm,ntetsinc,nnodesmm
      integer k,kf,ke,kt,irefine,npointsinc,inc,inc1,inc2,
     *        kflast,ktlast,kelast,jcount,idum,
     *        itstart,itlast,ifstart,ielast,iestart,iflast

      real*8 xdotmin,xdot,xdotl,ds23,ds2i,ds3i
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,xint,yint,zint

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
C
      pointer (ipxweight_source, xweight_source)
      real*8 xweight_source(nvalues,*)
C
      integer itetface0(4), itetface1(4,4)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 1,
     *                 1, 4, 3, 2,
     *                 1, 2, 4, 3,
     *                 1, 3, 2, 4 /
      integer itetface2(3,3,4)
      data itetface2 / 3, 4, 2,
     *                         4, 2, 3,
     *                         2, 3, 4,
     *                         4, 3, 1,
     *                         3, 1, 4,
     *                         1, 4, 3,
     *                         2, 4, 1,
     *                         4, 1, 2,
     *                         1, 2, 4,
     *                         3, 2, 1,
     *                         2, 1, 3,
     *                         1, 3, 2 /
      integer itetface3(2,3,4)
      data itetface3 / 2, 1,
     *                 3, 1,
     *                 4, 1,
     *                 1, 1,
     *                 4, 3,
     *                 3, 2,
     *                 1, 2,
     *                 2, 3,
     *                 4, 2,
     *                 1, 3,
     *                 3, 3,
     *                 2, 2 /

      real*8 crosx1,crosy1,crosz1,volume
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)

C
      character*132 logmess
      character*32 cmo, cmolength
      character*32 isubname, iblknam, iprtnam
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     BEGIN begin

      isubname='refine_edge'
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      icount=0
      do it=1,ntets
         do i=1,4
            if(jtet(i,it).ge.mbndry) icount=icount+1
         enddo
      enddo
      if(icount.eq.0) then
         write(logmess,'(a)')
     *      "No boundary face in the geometry"
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      length=3*icount
      call mmgetblk("iedgetet",isubname,ipiedge_tet,length,1,icscode)
      call mmgetblk("iedgefac",isubname,ipiedge_face,length,1,icscode)
      call mmgetblk("iedgeedg",isubname,ipiedge_edge,length,1,icscode)
      length=ntets
      call mmgetblk("itflag",isubname,ipitflag,length,1,icscode)
      length=4*ntets
      call mmgetblk("itetnn" ,isubname,ipitetnn ,length,1,icscode)
      call mmgetblk("itetnn1",isubname,ipitetnn1,length,1,icscode)
      call mmgetblk("itetnn2",isubname,ipitetnn2,length,1,icscode)
      do it=1,ntets
         do i=1,4
            itetnn(i,it)=itet(i,it)
            if(jtet(i,it).ge.mbndry) then
               itetnn1(i,it)=0
               itetnn2(i,it)=0
            else
               itetnn1(i,it)=1+(jtet(i,it)-1)/4
               itetnn2(i,it)=jtet(i,it)-4*(itetnn1(i,it)-1)
            endif
         enddo
      enddo
      xdotmin=1.0
      nedge=0
      do it=1,ntets
         do i=1,4
            if(itetnn1(i,it).le.0.or.itetnn1(i,it).gt.ntets) then
               do j=1,3
                  j1=itet(itetface2(3,j,i),it)
                  j2=itet(itetface2(1,j,i),it)
                  j3=itet(itetface2(2,j,i),it)
                  xdot=(xic(j2)-xic(j1))*(xic(j3)-xic(j1)) +
     *                 (yic(j2)-yic(j1))*(yic(j3)-yic(j1)) +
     *                 (zic(j2)-zic(j1))*(zic(j3)-zic(j1))
                  if(xdot.lt.-1.0e-10) then
                     if(nedge.gt.0) then
                        do l=1,nedge
                           lit=iedge_tet(l)
                           li=iedge_face(l)
                           lj=iedge_edge(l)
                           l1=itet(itetface2(3,lj,li),lit)
                           l2=itet(itetface2(1,lj,li),lit)
                           l3=itet(itetface2(2,lj,li),lit)
                           if((l2.eq.j2.and.l3.eq.j3).or.
     *                        (l2.eq.j3.and.l3.eq.j2)) then
C*****                              cx1=crosx1(j1,j2,j3)
C*****                              cy1=crosy1(j1,j2,j3)
C*****                              cz1=crosz1(j1,j2,j3)
C*****                              cx2=crosx1(l1,l2,l3)
C*****                              cy2=crosy1(l1,l2,l3)
C*****                              cz2=crosz1(l1,l2,l3)
C*****                              xdotc=cx1*cx2+cy1*cy2+cz1*cz2
C*****                              if(abs(1.0-xdotc).lt.1.0e-10) then
C*****                                 iedge_tet(l)=iedge_tet(nedge)
C*****                                 iedge_face(l)=iedge_face(nedge)
C*****                                 iedge_edge(l)=iedge_edge(nedge)
C*****                                 nedge=nedge-1
C*****                                 goto 5
C*****                              endif
                              xdotl=(xic(l2)-xic(l1))*(xic(l3)-xic(l1))+
     *                              (yic(l2)-yic(l1))*(yic(l3)-yic(l1))+
     *                              (zic(l2)-zic(l1))*(zic(l3)-zic(l1))
                              if(xdot.lt.xdotl) then
                                 iedge_tet(l)=it
                                 iedge_face(l)=i
                                 iedge_edge(l)=j
                              endif
                              goto 5
                           endif
                        enddo
                     endif
                     nedge=nedge+1
                     iedge_tet(nedge)=it
                     iedge_face(nedge)=i
                     iedge_edge(nedge)=j
   5                 continue
                  endif
               enddo
            endif
         enddo
      enddo
C
      length=nedge
      call mmgetblk('list_sink',isubname,iplist_sink,length,1,icscode)
      length=nvalues*nedge
      call mmgetblk('list_source',isubname,iplist_source,length,1,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      nadd1=0
      iedgeiter=0
  10  continue
      iedgeiter=iedgeiter+1
      write(logmess,'(a,2i10)')
     *   "Edge iteration: ",iedgeiter,nedge
      call writloga('default',0,logmess,0,ierrw)
      do it=1,ntets
         itflag(it)=0
      enddo
      irefine=0
      npointsnew=npoints
      ntetsnew=ntets
      nedge_save=0
      do iedge=1,nedge
         it=iedge_tet(iedge)
         if(itflag(it).ne.0) goto 130
         i=iedge_face(iedge)
         j=iedge_edge(iedge)
         j1=itet(itetface2(3,j,i),it)
         j2=itet(itetface2(1,j,i),it)
         j3=itet(itetface2(2,j,i),it)
         x1=xic(j1)
         y1=yic(j1)
         z1=zic(j1)
         x2=xic(j2)-x1
         y2=yic(j2)-y1
         z2=zic(j2)-z1
         x3=xic(j3)-x1
         y3=yic(j3)-y1
         z3=zic(j3)-z1
         call int_edge(zero,zero,zero,x2,y2,z2,x3,y3,z3,xint,yint,zint)
         ds23=sqrt((x3-x2)**2+(y3-y2)**2+(z3-z2)**2)
         ds2i=sqrt((xint-x2)**2+(yint-y2)**2+(zint-z2)**2)
         ds3i=sqrt((xint-x3)**2+(yint-y3)**2+(zint-z3)**2)
         xint=xint+x1
         yint=yint+y1
         zint=zint+z1
         icount=0
         ibound=0
         itstart=it
         ifstart=i
         iestart=j
         itlast=itstart
         iflast=ifstart
         ielast=iestart
 100     continue
            kt=1+(jtet(iflast,itlast)-1)/4
            kf=jtet(iflast,itlast)-4*(kt-1)
            ke=0
            do k=1,3
               k1=itet(itetface2(3,k,kf),kt)
               k2=itet(itetface2(1,k,kf),kt)
               k3=itet(itetface2(2,k,kf),kt)
               if((k2.eq.j2.and.k3.eq.j3).or.
     *            (k2.eq.j3.and.k3.eq.j2)) then
                  ke=k
               endif
            enddo
            if(itflag(kt).ne.0) then
               nedge_save=nedge_save+1
               iedge_tet(nedge_save)=it
               iedge_face(nedge_save)=i
               iedge_edge(nedge_save)=j
               goto 130
            endif
            if(kt.eq.itstart) then
               goto 110
            elseif(kt.gt.ntets) then
               if(ibound.eq.0) then
                  itstart=itlast
                  ifstart=itetface3(1,ielast,iflast)
                  iestart=0
                  do k=1,3
                     k1=itet(itetface2(3,k,ifstart),itstart)
                     k2=itet(itetface2(1,k,ifstart),itstart)
                     k3=itet(itetface2(2,k,ifstart),itstart)
                     if((k2.eq.j2.and.k3.eq.j3).or.
     *                  (k2.eq.j3.and.k3.eq.j2)) then
                        iestart=k
                     endif
                  enddo
                  icount=0
                  ibound=1
                  itlast=itstart
                  iflast=ifstart
                  ielast=iestart
                  goto 100
               elseif(ibound.eq.1) then
                  goto 110
               else
                  write(logmess,'(a,4i10)')
     *               'Impossible number of boundary faces',it,i,kt,kf
                  call writloga('default',0,logmess,0,ierrw)
               endif
            else
               kf=itetface3(1,ke,kf)
C*****         ke=itetface3(2,ke,kf)
               ke=0
               do k=1,3
                  k1=itet(itetface2(3,k,kf),kt)
                  k2=itet(itetface2(1,k,kf),kt)
                  k3=itet(itetface2(2,k,kf),kt)
                  if((k2.eq.j2.and.k3.eq.j3).or.
     *               (k2.eq.j3.and.k3.eq.j2)) then
                     ke=k
                  endif
              enddo
            endif
            icount=icount+1
            itlast=kt
            iflast=kf
            ielast=ke
            j1=itet(itetface2(3,ke,kf),kt)
            j2=itet(itetface2(1,ke,kf),kt)
            j3=itet(itetface2(2,ke,kf),kt)
            goto 100
 110     continue
         irefine=irefine+1
         call mmfindbk('xic',cmo,ipxic,length,icscode)
         if((npointsnew+1).gt.length) then
            npointsinc=npointsnew+1000
            call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
            call mmgetlen(ipitetclr,nelementsmm,icscode)
            call cmo_set_info('nelements',cmo,nelementsmm,1,1,ierror)
            call cmo_newlen(cmo,ierror)
            call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ier)
            call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ier)
            call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ier)
         endif
         npointsnew=npointsnew+1
C
         nadd1=nadd1+1
         list_sink(nadd1)=npointsnew
         list_source(1,nadd1)=j2
         list_source(2,nadd1)=j3
         xweight_source(1,nadd1)=ds2i
         xweight_source(2,nadd1)=ds3i
C
         jcount=0
         ktlast=itstart
         kflast=ifstart
         kelast=iestart
         j1=itet(itetface2(3,kelast,kflast),ktlast)
         j2=itet(itetface2(1,kelast,kflast),ktlast)
         j3=itet(itetface2(2,kelast,kflast),ktlast)
 120     continue
            kt=1+(jtet(kflast,ktlast)-1)/4
            kf=jtet(kflast,ktlast)-4*(kt-1)
            ke=0
            do k=1,3
               k1=itet(itetface2(3,k,kf),kt)
               k2=itet(itetface2(1,k,kf),kt)
               k3=itet(itetface2(2,k,kf),kt)
               if((k2.eq.j2.and.k3.eq.j3).or.
     *            (k2.eq.j3.and.k3.eq.j2)) then
                  ke=k
               endif
            enddo
            itflag(ktlast)=1
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+1).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *          icmotype,ierror)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,lenitetclr,icmotype,ier)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,lenitettyp,icmotype,ier)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,lenitetoff,icmotype,ier)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,lenjtetoff,icmotype,ier)
               call cmo_get_info('itet',cmo,
     *                           ipitet,lenitet,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,lenjtet,icmotype,ierror)
               call mmgetnam(ipitflag,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                       ics)
               do idum=ntetsnew+1,ntetsnew+inc
                  itflag(idum)=0
               enddo
               inc1=nen*inc
               call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                       ics)
               inc2=nef*inc
               call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                       ics)
               call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
               call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                       ics)
            endif
            i1=itet(kflast,ktlast)
            i2=itet(itetface2(3,kelast,kflast),ktlast)
            i3=itet(itetface2(1,kelast,kflast),ktlast)
            i4=itet(itetface2(2,kelast,kflast),ktlast)
            ntetsnew=ntetsnew+1
            itetclr(ntetsnew)=itetclr(ktlast)
            itettyp(ntetsnew)=itettyp(ktlast)
            itetoff(ntetsnew)=nen*(ntetsnew-1)
            jtetoff(ntetsnew)=nef*(ntetsnew-1)
            itetnn(1,ntetsnew)=i1
            itetnn(2,ntetsnew)=i2
            itetnn(3,ntetsnew)=npointsnew
            itetnn(4,ntetsnew)=i4
            itetnn1(1,ntetsnew)=-1
            itetnn1(2,ntetsnew)=-1
            itetnn1(3,ntetsnew)=-1
            itetnn1(4,ntetsnew)=-1
            itetnn2(1,ntetsnew)=-1
            itetnn2(2,ntetsnew)=-1
            itetnn2(3,ntetsnew)=-1
            itetnn2(4,ntetsnew)=-1
            itetnn(1,ktlast)=i1
            itetnn(2,ktlast)=i2
            itetnn(3,ktlast)=i3
            itetnn(4,ktlast)=npointsnew
            itetnn1(1,ktlast)=-1
            itetnn1(2,ktlast)=-1
            itetnn1(3,ktlast)=-1
            itetnn1(4,ktlast)=-1
            itetnn2(1,ktlast)=-1
            itetnn2(2,ktlast)=-1
            itetnn2(3,ktlast)=-1
            itetnn2(4,ktlast)=-1
            if(jcount.lt.icount) then
               kf=itetface3(1,ke,kf)
C*****         ke=itetface3(2,ke,kf)
               ke=0
               do k=1,3
                  k1=itet(itetface2(3,k,kf),kt)
                  k2=itet(itetface2(1,k,kf),kt)
                  k3=itet(itetface2(2,k,kf),kt)
                  if((k2.eq.j2.and.k3.eq.j3).or.
     *               (k2.eq.j3.and.k3.eq.j2)) then
                     ke=k
                  endif
               enddo
               ktlast=kt
               kflast=kf
               kelast=ke
               j1=itet(itetface2(3,kelast,kflast),ktlast)
               j2=itet(itetface2(1,kelast,kflast),ktlast)
               j3=itet(itetface2(2,kelast,kflast),ktlast)
               jcount=jcount+1
               goto 120
            endif
 130     continue
      enddo
      write(logmess,'(a,i10,a,i10)')
     *   "Edge-refined tets: old=",ntets," new=",ntetsnew
      call writloga('default',0,logmess,0,ierrw)
      if(ntetsnew.gt.ntets) then
         do it=1,ntets
            if(itflag(it).ne.0) then
               do i=1,4
                  kt=1+(jtet(i,it)-1)/4
                  kf=jtet(i,it)-4*(kt-1)
                  if(kt.le.ntets) then
                     itetnn1(kf,kt)=-1
                     itetnn2(kf,kt)=-1
                  endif
                  itetnn1(i,it)=-1
                  itetnn2(i,it)=-1
               enddo
            endif
         enddo
         npoints=npointsnew
         ntets=ntetsnew
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
         call geniee(itetnn,itetnn1,itetnn2,4,4,ntets,npoints,
     *        3,npoints,ntets)
         do it=1,ntets
            do i=1,4
               itet(i,it)=itetnn(i,it)
               if(itetnn1(i,it).gt.0.and.itetnn1(i,it).le.ntets) then
                  jtet(i,it)=4*(itetnn1(i,it)-1)+itetnn2(i,it)
               else
                  jtet(i,it)=mbndry
               endif
            enddo
         enddo
      endif
      if(nedge_save.gt.0) then
         nedge=nedge_save
         goto 10
      endif
      goto 9999
 9999 continue
C
      call cmo_get_name(cmo,ierror)
C
      cmolength='nnodes'
      call cmo_interpolate(cmo,cmo,
     *                     cmolength,
     *                     nadd1,nvalues,
     *                     list_sink,list_source,xweight_source,
     *                     ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_interpolate',ier)
C
      if (npoints .le. 0) then
         call x3d_error(isubname,'calling set mesh with 0 nodes.')
         call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      endif
      if (ntets .le. 0) then
         call x3d_error(isubname,'calling set mesh with 0 tets.')
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      endif
C
      call mmrelprt(isubname,icscode)
C
      return
      end
