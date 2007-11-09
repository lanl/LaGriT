*dk,refinefc
      subroutine refine_face()
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/refine_face.f_a  $
CPVCS    
CPVCS       Rev 1.7   05 May 2000 15:33:58   dcg
CPVCS    refresh mbndry value after cmo_newlen
CPVCS
CPVCS       Rev 1.6   Fri Jan 22 15:46:46 1999   dcg
CPVCS    Define the tolerance xst2 to be 1.d-9
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:59:24 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   11/07/95 17:24:40   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.3   09/29/95 09:14:10   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.2   06/05/95 10:36:54   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.1   05/26/95 13:17:52   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:00   pvcs
CPVCS    Original version.
C
       implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitet, itet)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet)
      pointer (ipjtet, jtet1)
      integer itet(4,1000000), jtet(4,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      dimension   xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipitflag, itflag)
      pointer (ipitetnn, itetnn)
      pointer (ipitetnn1, itetnn1)
      pointer (ipitetnn2, itetnn2)
      pointer (ipkfix, kfix)
      pointer (ipkfix, kfix1)
      pointer (ipxfix, xfix)
      dimension xfix(4,1000000)
      integer kfix(4,1000000), kfix1(4*1000000)
      integer itflag(1000000),
     *        itetnn(4,1000000), itetnn1(4,1000000), itetnn2(4,1000000)
C
      parameter (nvalues=3)
      pointer (iplist_sink, list_sink)
      pointer (iplist_source, list_source)
      pointer (ipxweight_source, xweight_source)
      integer list_sink(10000000), list_source(nvalues,10000000)
      real*8 xweight_source(nvalues,10000000)
C
      character*32 cmo, cmolength
      character*32 isubname, iblknam, iprtnam
C
      integer itetface0(4), itetface1(4,4)
c  a tolerance
      real*8 xst2
      data xst2/1.0d-9/
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
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
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
      isubname='refine_face'
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
      xxsmall=1.0d-30
      length=ntets
      call mmgetblk("itflag",isubname,ipitflag,length,2,icscode)
      length=4*ntets
      call mmgetblk("itetnn" ,isubname,ipitetnn ,length,2,icscode)
      call mmgetblk("itetnn1",isubname,ipitetnn1,length,2,icscode)
      call mmgetblk("itetnn2",isubname,ipitetnn2,length,2,icscode)
      call mmgetblk("kfix",isubname,ipkfix,length,2,icscode)
      call mmgetblk("xfix",isubname,ipxfix,length,2,icscode)
C
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
      do it=1,ntets
         itflag(it)=0
      enddo
      irefine=0
      npointsnew=npoints
      ntetsnew=ntets
      do it=1,ntets
         do j=1,4
            kfix(j,it)=0
         enddo
      enddo
      do 10 i=1,ntets
         xa=xic(itet(1,i))
         ya=yic(itet(1,i))
         za=zic(itet(1,i))
         xb=xic(itet(2,i))-xa
         yb=yic(itet(2,i))-ya
         zb=zic(itet(2,i))-za
         xc=xic(itet(3,i))-xa
         yc=yic(itet(3,i))-ya
         zc=zic(itet(3,i))-za
         xd=xic(itet(4,i))-xa
         yd=yic(itet(4,i))-ya
         zd=zic(itet(4,i))-za
         xn=crosx(xb,yb,zb,xc,yc,zc)
         yn=crosy(xb,yb,zb,xc,yc,zc)
         zn=crosz(xb,yb,zb,xc,yc,zc)
         x2=crosx(xn,yn,zn,xb,yb,zb)
         y2=crosy(xn,yn,zn,xb,yb,zb)
         z2=crosz(xn,yn,zn,xb,yb,zb)
         q=-0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *          (x2*xc+y2*yc+z2*zc+xxsmall)
         xl=q*x2+0.5*xb
         yl=q*y2+0.5*yb
         zl=q*z2+0.5*zb
         d=-0.5*(xd*xd+yd*yd+zd*zd)
         q=-(xd*xl+yd*yl+zd*zl+d)/(xd*xn+yd*yn+zd*zn+xxsmall)
         vol=xn*xd+yn*yd+zn*zd
         xv=q*xn+xl+xa
         yv=q*yn+yl+ya
         zv=q*zn+zl+za
         dist=(xv-xa)**2+(yv-ya)**2+(zv-za)**2
C********j1=cvmgn(jtet(1,i),1,jtet(1,i)-mbndry)
C********j1=cvmgm(j1,j1-mbndry,j1-mbndry)
         if((jtet(1,i)-mbndry).ne.0) then
            j1=jtet(1,i)
         else
            j1=1
         endif
         if((j1-mbndry).lt.0) then
            j1=j1
         else
            j1=j1-mbndry
         endif
         xfix(1,i)=(xic(itet1(j1))-xv)**2+
     *             (yic(itet1(j1))-yv)**2+
     *             (zic(itet1(j1))-zv)**2
C********j1=cvmgn(jtet(2,i),1,jtet(2,i)-mbndry)
C********j1=cvmgm(j1,j1-mbndry,j1-mbndry)
         if((jtet(2,i)-mbndry).ne.0) then
            j1=jtet(2,i)
         else
            j1=1
         endif
         if((j1-mbndry).lt.0) then
            j1=j1
         else
            j1=j1-mbndry
         endif
         xfix(2,i)=(xic(itet1(j1))-xv)**2+
     *             (yic(itet1(j1))-yv)**2+
     *             (zic(itet1(j1))-zv)**2
C********j1=cvmgn(jtet(3,i),1,jtet(3,i)-mbndry)
C********j1=cvmgm(j1,j1-mbndry,j1-mbndry)
         if((jtet(3,i)-mbndry).ne.0) then
            j1=jtet(3,i)
         else
            j1=1
         endif
         if((j1-mbndry).lt.0) then
            j1=j1
         else
            j1=j1-mbndry
         endif
         xfix(3,i)=(xic(itet1(j1))-xv)**2+
     *             (yic(itet1(j1))-yv)**2+
     *             (zic(itet1(j1))-zv)**2
C********j1=cvmgn(jtet(4,i),1,jtet(4,i)-mbndry)
C********j1=cvmgm(j1,j1-mbndry,j1-mbndry)
         if((jtet(4,i)-mbndry).ne.0) then
            j1=jtet(4,i)
         else
            j1=1
         endif
         if((j1-mbndry).lt.0) then
            j1=j1
         else
            j1=j1-mbndry
         endif
         xfix(4,i)=(xic(itet1(j1))-xv)**2+
     *             (yic(itet1(j1))-yv)**2+
     *             (zic(itet1(j1))-zv)**2
         ifc=4*(i-1)
C********kfix(1,i)=cvmgp(ifc+1,0,dist-xfix(1,i)-xst2*dist)
C********kfix(2,i)=cvmgp(ifc+2,0,dist-xfix(2,i)-xst2*dist)
C********kfix(3,i)=cvmgp(ifc+3,0,dist-xfix(3,i)-xst2*dist)
C********kfix(4,i)=cvmgp(ifc+4,0,dist-xfix(4,i)-xst2*dist)
C********kfix(1,i)=cvmgm(kfix(1,i),-i,-vol)
         if(dist-xfix(1,i)-xst2*dist .ge. 0.0) then
            kfix(1,i)=ifc+1
         else
            kfix(1,i)=0
         endif
         if(dist-xfix(2,i)-xst2*dist .ge. 0.0) then
            kfix(2,i)=ifc+2
         else
            kfix(2,i)=0
         endif
         if(dist-xfix(3,i)-xst2*dist .ge. 0.0) then
            kfix(3,i)=ifc+3
         else
            kfix(3,i)=0
         endif
         if(dist-xfix(4,i)-xst2*dist .ge. 0.0) then
            kfix(4,i)=ifc+4
         else
            kfix(4,i)=0
         endif
         if(-vol.lt.0.0) then
            kfix(1,i)=kfix(1,i)
         else
            kfix(1,i)=-i
         endif
         do k=1,4
            if(jtet(k,i).gt.0.and.jtet(k,i).lt.mbndry) then
               jt=1+(jtet(k,i)-1)/4
               jf=jtet(k,i)-4*(jt-1)
               iclrt1=itetclr(i)
               iclrt2=itetclr(jt)
               if(iclrt1.ne.iclrt2) then
                  if(iclrt1.ne.0.and.iclrt2.ne.0) then
C*****               kfix(k,i)=0
                  endif
               endif
             endif
          enddo
  10  continue
      nface1=0
      do it=1,ntets
         do j=1,4
            jt=itetnn1(j,it)
            jf=itetnn2(j,it)
            i1=itet(jf,jt)
            i2=itet(itetface1(1,jf),jt)
            i3=itet(itetface1(2,jf),jt)
            i4=itet(itetface1(3,jf),jt)
            if(jt.gt.0.and.jt.le.ntets) then
               if(itetclr(it).ne.itetclr(jt)) then
                  if(kfix(j,it).gt.0) then
                     if(nface1.gt.0) then
                        do l=1,nface1
                           kt=1+(kfix1(l)-1)/4
                           kf=kfix1(l)-4*(kt-1)
                           j1=itet(kf,kt)
                           j2=itet(itetface1(1,kf),kt)
                           j3=itet(itetface1(2,kf),kt)
                           j4=itet(itetface1(3,kf),kt)
                           if((i2.eq.j2.and.i3.eq.j3.and.i4.eq.j4).or.
     *                        (i2.eq.j3.and.i3.eq.j4.and.i4.eq.j2).or.
     *                        (i2.eq.j4.and.i3.eq.j2.and.i4.eq.j3)) then
                              goto 5
                           endif
                        enddo
                     endif
                     nface1=nface1+1
                     kfix1(nface1)=kfix(j,it)
   5                 continue
                  endif
               endif
            endif
         enddo
      enddo
      goto 7
      nface1=0
      do it=1,ntets
         do j=1,4
            kfix(j,it)=4*(it-1)+j
            jt=itetnn1(j,it)
            jf=itetnn2(j,it)
            i1=itet(jf,jt)
            i2=itet(itetface1(1,jf),jt)
            i3=itet(itetface1(2,jf),jt)
            i4=itet(itetface1(3,jf),jt)
            if(jt.gt.0.and.jt.le.ntets) then
               if(nface1.gt.0) then
                  do l=1,nface1
                     kt=1+(kfix1(l)-1)/4
                     kf=kfix1(l)-4*(kt-1)
                     j1=itet(kf,kt)
                     j2=itet(itetface1(1,kf),kt)
                     j3=itet(itetface1(2,kf),kt)
                     j4=itet(itetface1(3,kf),kt)
                     if((i2.eq.j2.and.i3.eq.j3.and.i4.eq.j4).or.
     *                  (i2.eq.j3.and.i3.eq.j4.and.i4.eq.j2).or.
     *                  (i2.eq.j4.and.i3.eq.j2.and.i4.eq.j3)) then
                        goto 6
                     endif
                  enddo
               endif
               nface1=nface1+1
               kfix1(nface1)=kfix(j,it)
   6           continue
            endif
         enddo
      enddo
   7  continue
      ifaceiter=0
C
      length=nface1
      call mmgetblk('list_sink',isubname,iplist_sink,length,2,icscode)
      length=nvalues*nface1
      call mmgetblk('list_source',isubname,iplist_source,length,2,
     *              icscode)
      call mmgetblk('xweight_source',isubname,ipxweight_source,length,2,
     *              icscode)
C
      nadd1=0
  11  continue
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
      do it=1,ntets
         itflag(it)=0
      enddo
      npointsnew=npoints
      ntetsnew=ntets
        ifaceiter=ifaceiter+1
        write(logmess,'(a,2i10)') "Face iteration: ",ifaceiter,nface1
        call writloga('default',0,logmess,0,ierrw)
      do it=1,ntets
         itflag(it)=0
      enddo
      write(logmess,'(a,i10)')
     *   "Interface face that need reconnecting: ",nface1
      call writloga('default',0,logmess,0,ierrw)
      if(nface1.ne.0) then
         nface1_save=0
         do iface=1,nface1
            it=1+(kfix1(iface)-1)/4
            if=kfix1(iface)-4*(it-1)
            i1=itet(if,it)
            i2=itet(itetface1(1,if),it)
            i3=itet(itetface1(2,if),it)
            i4=itet(itetface1(3,if),it)
            jt=1+(jtet(if,it)-1)/4
            jf=jtet(if,it)-4*(jt-1)
            i5=itet(jf,jt)
            if(it.lt.0.or.jt.lt.0) then
               write(logmess,'(a,2i10)') "Error1: ",it,jt
               call writloga('default',0,logmess,0,ierrw)
               stop
            endif
            if(it.gt.ntets.or.jt.gt.ntets) then
               goto 20
            endif
            if(itflag(it).eq.0.and.itflag(jt).eq.0) then
               irefine=irefine+1
               call mmfindbk('xic',cmo,ipxic,length,icscode)
               if((npointsnew+1).gt.length) then
                  npointsinc=npointsnew+1000
                  call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
                  call mmgetlen(ipitetclr,nelementsmm,icscode)
                  call cmo_set_info('nelements',cmo,nelementsmm,1,1,ier)
                  call cmo_newlen(cmo,ierror)
                  call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                    icmotype,ierror)
                  call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ier)
                  call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ier)
                  call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ier)
               endif
               npointsnew=npointsnew+1
C
               nadd1=nadd1+1
               list_sink(nadd1)=npointsnew
               list_source(1,nadd1)=i2
               list_source(2,nadd1)=i3
               list_source(3,nadd1)=i4
               xweight_source(1,nadd1)=1.0
               xweight_source(2,nadd1)=1.0
               xweight_source(3,nadd1)=1.0
C
               call mmgetlen(ipitflag,length,icscode)
               if((ntetsnew+4).gt.length) then
                  inc=1000
                  ntetsinc=ntetsnew+inc
                  call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
                  call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
                  call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
                  call cmo_newlen(cmo,ierror)
                  call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *                    icmotype,ierror)
                  call cmo_get_info('itetclr',cmo,
     *                              ipitetclr,lenitetclr,icmotype,ier)
                  call cmo_get_info('itettyp',cmo,
     *                              ipitettyp,lenitettyp,icmotype,ier)
                  call cmo_get_info('itetoff',cmo,
     *                              ipitetoff,lenitetoff,icmotype,ier)
                  call cmo_get_info('jtetoff',cmo,
     *                              ipjtetoff,lenjtetoff,icmotype,ier)
                  call cmo_get_info('itet',cmo,
     *                              ipitet,lenitet,icmotype,ierror)
                  call cmo_get_info('jtet',cmo,
     *                              ipjtet,lenjtet,icmotype,ierror)
                  call mmgetnam(ipitflag,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitflag,inc,
     *                          ics)
                  do idum=ntetsnew+1,ntetsnew+inc
                     itflag(idum)=0
                  enddo
                  inc1=nen*inc
                  call mmgetnam(ipitetnn,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn,inc1,
     *                          ics)
                  inc2=nef*inc
                  call mmgetnam(ipitetnn1,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn1,inc2,
     *                          ics)
                  call mmgetnam(ipitetnn2,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipitetnn2,inc2,
     *                          ics)
                  call mmgetnam(ipkfix,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipkfix,inc2,
     *                          ics)
                  call mmgetnam(ipxfix,iblknam,iprtnam,ics)
                  call mmincblk(iblknam,iprtnam,ipxfix,inc2,
     *                          ics)
               endif
               itflag(it)=1
               itetnn(1,it)=i1
               itetnn(2,it)=i2
               itetnn(3,it)=i3
               itetnn(4,it)=npointsnew
               itetnn1(1,it)=-1
               itetnn1(2,it)=-1
               itetnn1(3,it)=-1
               itetnn1(4,it)=-1
               itetnn2(1,it)=-1
               itetnn2(2,it)=-1
               itetnn2(3,it)=-1
               itetnn2(4,it)=-1
               ntetsnew=ntetsnew+1
                  itetclr(ntetsnew)=itetclr(it)
                  itettyp(ntetsnew)=itettyp(it)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn(1,ntetsnew)=i1
                  itetnn(2,ntetsnew)=i4
                  itetnn(3,ntetsnew)=i2
                  itetnn(4,ntetsnew)=npointsnew
                  itetnn1(1,ntetsnew)=-1
                  itetnn1(2,ntetsnew)=-1
                  itetnn1(3,ntetsnew)=-1
                  itetnn1(4,ntetsnew)=-1
                  itetnn2(1,ntetsnew)=-1
                  itetnn2(2,ntetsnew)=-1
                  itetnn2(3,ntetsnew)=-1
                  itetnn2(4,ntetsnew)=-1
               ntetsnew=ntetsnew+1
                  itetclr(ntetsnew)=itetclr(jt)
                  itettyp(ntetsnew)=itettyp(jt)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn(1,ntetsnew)=i5
                  itetnn(2,ntetsnew)=i2
                  itetnn(3,ntetsnew)=i4
                  itetnn(4,ntetsnew)=npointsnew
                  itetnn1(1,ntetsnew)=-1
                  itetnn1(2,ntetsnew)=-1
                  itetnn1(3,ntetsnew)=-1
                  itetnn1(4,ntetsnew)=-1
                  itetnn2(1,ntetsnew)=-1
                  itetnn2(2,ntetsnew)=-1
                  itetnn2(3,ntetsnew)=-1
                  itetnn2(4,ntetsnew)=-1
               ntetsnew=ntetsnew+1
                  itetclr(ntetsnew)=itetclr(jt)
                  itettyp(ntetsnew)=itettyp(jt)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn(1,ntetsnew)=i5
                  itetnn(2,ntetsnew)=i4
                  itetnn(3,ntetsnew)=i3
                  itetnn(4,ntetsnew)=npointsnew
                  itetnn1(1,ntetsnew)=-1
                  itetnn1(2,ntetsnew)=-1
                  itetnn1(3,ntetsnew)=-1
                  itetnn1(4,ntetsnew)=-1
                  itetnn2(1,ntetsnew)=-1
                  itetnn2(2,ntetsnew)=-1
                  itetnn2(3,ntetsnew)=-1
                  itetnn2(4,ntetsnew)=-1
               ntetsnew=ntetsnew+1
                  itetclr(ntetsnew)=itetclr(jt)
                  itettyp(ntetsnew)=itettyp(jt)
                  itetoff(ntetsnew)=nen*(ntetsnew-1)
                  jtetoff(ntetsnew)=nef*(ntetsnew-1)
                  itetnn(1,ntetsnew)=i5
                  itetnn(2,ntetsnew)=i3
                  itetnn(3,ntetsnew)=i2
                  itetnn(4,ntetsnew)=npointsnew
                  itetnn1(1,ntetsnew)=-1
                  itetnn1(2,ntetsnew)=-1
                  itetnn1(3,ntetsnew)=-1
                  itetnn1(4,ntetsnew)=-1
                  itetnn2(1,ntetsnew)=-1
                  itetnn2(2,ntetsnew)=-1
                  itetnn2(3,ntetsnew)=-1
                  itetnn2(4,ntetsnew)=-1
               itflag(jt)=1
               itetclr(jt)=itetclr(it)
               itettyp(jt)=itettyp(it)
               itetoff(jt)=nen*(jt-1)
               jtetoff(jt)=nef*(jt-1)
               itetnn(1,jt)=i1
               itetnn(2,jt)=i3
               itetnn(3,jt)=i4
               itetnn(4,jt)=npointsnew
               itetnn1(1,jt)=-1
               itetnn1(2,jt)=-1
               itetnn1(3,jt)=-1
               itetnn1(4,jt)=-1
               itetnn2(1,jt)=-1
               itetnn2(2,jt)=-1
               itetnn2(3,jt)=-1
               itetnn2(4,jt)=-1
            else
               nface1_save=nface1_save+1
               kfix1(nface1_save)=kfix1(iface)
            endif
   20       continue
         enddo
      endif
      write(logmess,'(a,i10,a,i10)')
     *   "Face-refined tets: old=",ntets," new=",ntetsnew
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
         if(nface1_save.gt.0) then
            nface1=nface1_save
            goto 11
         endif
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
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
C
      call mmrelprt(isubname,icscode)
C
      return
      end
