 
      subroutine rzbrick_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C#######################################################################
C
C     PURPOSE -
C
C
C     Wrapper ROUTINE to rzbrick3
C
C#######################################################################
      implicit none
      include 'chydro.h'
      integer ierr, nwds, imsgin(*),msgtype(*)
      real*8 xmsgin(*)
      character*(*) cmsgin(nwds)
      character*32 cmo,isubname
      integer ierror,npoints,ilen,ityp,ntets,ipointi,ipointj,
     *  icscode,npoints_save
C
C#######################################################################
      isubname='rzbrick3'
      ierr=0
c
      call cmo_get_name(cmo,ierror)
c
      call cmo_get_intinfo('nnodes',cmo,npoints,ilen,ityp,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,ilen,ityp,ierror)
      call cmo_get_intinfo('ipointi',cmo,
     *                      ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_intinfo('ipointj',cmo,
     *                      ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      npoints_save=ipointj
C
C        ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chglocl(1,npoints,1)
C
      call rzbrick3(imsgin(2),xmsgin(2),cmsgin(2),
     *                    msgtype(2),nwds-1,ierr)
C
C        ---------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chgnorm(1,npoints,1)
C
C
C        ******************************************************************
      call cmo_get_name(cmo,ierror)
      call cmo_get_intinfo('nnodes',cmo,npoints,ilen,ityp,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,ilen,ityp,ierror)
C
      if(npoints.gt.npoints_save) then
          ipointi=npoints_save+1
          ipointj=npoints
          call cmo_get_name(cmo,ierror)
C
          call cmo_set_info('ipointi',cmo,
     *                         ipointi,1,1,icscode)
          if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
          call cmo_set_info('ipointj',cmo,
     *                         ipointj,1,1,icscode)
          if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
      endif
      return
      end
C
      subroutine rzbrick3(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C#######################################################################
C
C      PURPOSE -
C
C
C
C      INPUT ARGUMENTS -
C
C
C
C      OUTPUT ARGUMENTS -
C
C
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/rzbrick3.f_a  $
CPVCS    
CPVCS       Rev 1.27   07 May 2003 14:36:40   gable
CPVCS    Added options, fixed bugs, documented functionality in rzinterp
CPVCS    sections of the code. This piece of code is nearly the same as
CPVCS    the point interpolation code used in extrude, but since it required
CPVCS    a lot of changes to get it working, I decided not to touch the
CPVCS    code that extrude uses.
CPVCS    
CPVCS       Rev 1.26   22 Mar 2001 16:15:20   gable
CPVCS    Remove some print statements, modify standard screen
CPVCS    output to include number of nodes and number of elements.
CPVCS    
CPVCS       Rev 1.25   03 Oct 2000 09:43:40   dcg
CPVCS    remove references to ialias
CPVCS
CPVCS       Rev 1.24   08 Sep 2000 09:48:04   dcg
CPVCS    use cmo_get_intinfo for accessing INT attributes
CPVCS    call cmo_get_name before using the cmo name
CPVCS
CPVCS       Rev 1.23   21 Apr 2000 07:08:44   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS
CPVCS       Rev 1.22   Thu Apr 06 14:13:54 2000   dcg
CPVCS    replace get_info_i set_info_i calls
CPVCS
CPVCS       Rev 1.21   Wed Apr 05 13:35:00 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.20   Mon Dec 13 11:41:12 1999   dcg
CPVCS    check nwds before setting cell end and ratio spacing flags
CPVCS
CPVCS       Rev 1.19   Wed Nov 10 16:05:04 1999   dcg
CPVCS    remove unused ipoints
CPVCS
CPVCS       Rev 1.18   Tue Oct 19 11:55:10 1999   dcg
CPVCS    implicit none for interp routine
CPVCS    more cleanup
CPVCS
CPVCS       Rev 1.17   Tue Oct 19 11:37:34 1999   dcg
CPVCS    implicit none for rzbrick3
CPVCS    check for nwds before setting isym,jsym,ksym
CPVCS
CPVCS       Rev 1.16   Fri Jul 23 09:12:20 1999   dcg
CPVCS    replace rz commands with createpts
CPVCS
CPVCS       Rev 1.15   Wed Mar 11 15:58:58 1998   dcg
CPVCS    accept pset or pstatus
CPVCS
CPVCS       Rev 1.14   Tue Sep 23 16:15:12 1997   dcg
CPVCS    don't set itp in this routine - must use setpts
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 17:00:30 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Thu Mar 06 21:52:52 1997   het
CPVCS    Add the rzinterp routine.
CPVCS
CPVCS       Rev 1.11   Mon Nov 11 20:56:32 1996   het
CPVCS    Consider negative volume hexes and switch the node ordering.
CPVCS
CPVCS       Rev 1.10   Wed Jul 24 17:35:16 1996   dcg
CPVCS    use mesh object 'nef' attribute to pack element and
CPVCS    face number into jtet array
CPVCS
CPVCS       Rev 1.9   Fri May 24 13:55:20 1996   het
CPVCS    Fix a problem is counting points created by this routine.
CPVCS
CPVCS       Rev 1.8   Tue Apr 02 02:23:36 1996   het
CPVCS    Add the connection capability for quads.
CPVCS
CPVCS       Rev 1.7   Mon Mar 04 11:14:02 1996   dcg
CPVCS     remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.6   Fri Feb 02 14:24:24 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.5   Tue Jan 23 09:16:42 1996   het
CPVCS    Add the edges_per_element for hexes elements.
CPVCS
CPVCS       Rev 1.4   Fri Dec 22 14:35:50 1995   het
CPVCS    Correct an error
CPVCS    .
CPVCS
CPVCS       Rev 1.3   11/16/95 15:22:40   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.2   11/07/95 17:26:04   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   09/14/95 02:06:50   het
CPVCS    Fix ipcmoprm errors
CPVCS
CPVCS       Rev 1.0   07/17/95 16:23:22   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      integer nwds,ierr2
      real*8 voltot,xtetvol,xmn,ymn,zmn,xmx,ymx,zmx,xrz,yrz,zrz
      integer ipointi,icscode,ipointj,ierror,npoints,
     *  length,icmotype,ntets,nptsc,nptsv,ifpt1,ics,leni,mpno,
     *  npoints_save,ixz,iyz,izz,ixr,iyr,izr,nsdtopo, mbndry,
     *  icount,ntets_save
      integer jt,jf,node1,it,nnegvol,numtet,nelemavs,nv1,nv2,
     *  nv3,nv4,nv5,nv6,nv7,nv8,nw1,nw2,nw3,nw4,nw5,nw6,nw7,ier,
     *  nw8,ix,iy,iz,isym,jsym,ksym,i,j,k,nx,ny,nz,nxm1,nym1,nzm1,
     *  ixm1,iym1,izm1,ixp1,iyp1,izp1,i1,ifelm,nsd,nen,nef,nee
      integer ivc,icharlnf
      character*70 logmess
C
      include 'chydro.h'
      include 'neibor.h'
      include 'local_element.h'
C
C
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
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000),
     *        itet(4,1000000), jtet(4,1000000)
      integer itet1(4*1000000), jtet1(4*1000000)
      real*8   xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
C
C#######################################################################
C
C
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C#######################################################################
C
      pointer (ipjtet2, jtet2)
      integer jtet2(*)
C
      pointer (ipmpary, mpary)
      integer mpary(*)
C
      pointer (ipidone, idone)
      integer idone(*)
C
      character*32 isubname, cmo, cgeom, ioptrz, imeshopt
C
      real*8 xicvol(100), yicvol(100), zicvol(100)
C
C
C#######################################################################
C
C     DEFINE THE STATEMENT FUNCTIONS FOR THIS ROUTINE.
C
      ivc(i,j,k)=i+(j-1+(k-1)*ny)*nx
C
C#######################################################################
C
      isubname='rzbrick3'
 
C.......................................................................
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_intinfo('ipointi',cmo,ipointi,length,icmotype,
     *   icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_intinfo('ipointj',cmo,ipointj,length,icmotype,
     *   icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
C
C.......................................................................
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_intinfo('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,length,icmotype,
     *    ierror)
C
      isym=0
      jsym=0
      ksym=0
C
      cgeom=cmsgin(1)
      if(cgeom(1:icharlnf(cgeom)).eq.'interp') then
         call rzinterp(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
         goto 9999
      endif
      nx=imsgin(2)
      ny=imsgin(3)
      nz=imsgin(4)
      nptsc=max((nx-1)*(ny-1)*(nz-1),
     *          (nx-1)*(ny-1),
     *          (nx-1)*(nz-1),
     *          (ny-1)*(nz-1),
     *          nx-1,
     *          ny-1,
     *          nz-1)
      imeshopt=cmsgin(8)
      ioptrz='notset'
      if(imeshopt(1:7).eq.'connect') then
         nptsv=0
         if(msgtype(5).eq.1) then
            if(imsgin(5).eq.0) then
               ifpt1=ipointi
            else
               ifpt1=imsgin(5)
            endif
         elseif(msgtype(5).eq.3) then
            if(cmsgin(5)(1:7).eq.'pstatus'.or.
     *         cmsgin(5)(1:4).eq.'pset') then
               length=npoints
               call mmgetblk('mpary',isubname,ipmpary,length,2,ics)
               call cmo_get_info('isetwd',cmo,
     *                           ipisetwd,leni,icmotype,ierror)
               call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ics)
               call pntlimc(cmsgin(5),cmsgin(6),cmsgin(7),
     *                      ipmpary,mpno,npoints,isetwd,itp1)
               ifpt1=mpary(1)
            else
               ifpt1=ipointi
            endif
         endif
         if(nwds.ge.9)isym=imsgin(9)
         if(nwds.ge.10)jsym=imsgin(10)
         if(nwds.ge.11)ksym=imsgin(11)
         npoints_save=ifpt1
      else
         nptsv=nx*ny*nz
         ifpt1=npoints+1
         xmn=xmsgin(5)
         ymn=xmsgin(6)
         zmn=xmsgin(7)
         xmx=xmsgin(8)
         ymx=xmsgin(9)
         zmx=xmsgin(10)
C
         ixz=1
         iyz=1
         izz=1
         ixr=0
         iyr=0
         izr=0
         xrz=1.
         yrz=1.
         zrz=1.
         isym=0
         jsym=0
         ksym=0
         if(nwds.ge.11)ixz=imsgin(11)
         if(nwds.ge.12)iyz=imsgin(12)
         if(nwds.ge.13)izz=imsgin(13)
         if(nwds.ge.14)ixr=imsgin(14)
         if(nwds.ge.15)iyr=imsgin(15)
         if(nwds.ge.16)izr=imsgin(16)
         if(nwds.ge.17)xrz=xmsgin(17)
         if(nwds.ge.18)yrz=xmsgin(18)
         if(nwds.ge.19)zrz=xmsgin(19)
         if(nwds.ge.20)isym=imsgin(20)
         if(nwds.ge.21)jsym=imsgin(21)
         if(nwds.ge.22)ksym=imsgin(22)
C
         npoints_save=npoints
         if(nptsv.gt.0) then
            npoints=npoints+nptsv
            call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
            call cmo_newlen(cmo,ierror)
         endif
C
         call cmo_get_intinfo('ndimensions_topo',cmo,
     *                     nsdtopo,length,icmotype,ierror)
         call cmo_get_intinfo('nnodes',cmo,npoints,length,icmotype,
     *       ierror)
         call cmo_get_intinfo('nelements',cmo,ntets,length,
     *       icmotype,ierror)
         call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *       icmotype,ierror)
         call cmo_get_info('isetwd',cmo,
     *                     ipisetwd,leni,icmotype,ierror)
         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierror)
         call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierror)
         call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ierror)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
         call cmo_get_info('itetclr',cmo,
     *                     ipitetclr,leni,icmotype,ier)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
 
C
         icount=ifpt1-1
         if(imeshopt(1:7).ne.'connect') then
            if(ioptrz(1:6).eq.'notset') then
               call rzbrickg(cgeom,icount,
     *              nx , ny , nz,
     *              xmn, ymn, zmn,
     *              xmx, ymx, zmx,
     *              ixz, iyz, izz,
     *              ixr, iyr, izr,
     *              xrz, yrz, zrz ,
     *              ipxic, ipyic, ipzic)
            elseif(ioptrz(1:4).eq.'geom') then
               call rzbrickh(cgeom,icount,
     *              nx , ny , nz,
     *              xmn, ymn, zmn,
     *              xmx, ymx, zmx,
     *              ixz, iyz, izz,
     *              ixr, iyr, izr,
     *              xrz, yrz, zrz,
     *              ipxic, ipyic, ipzic)
            endif
         endif
      endif
C
C
      do 50 i1=1,nptsv
         itp1(i1)=0
         icr1(i1)=0
 50   continue
C
C     ..................................................................
C     CONSTRUCT THE NEIGHBOR CONNECTIVITY MATRIX FOR THE BRICKS.
C
      if(nx.gt.1.and.ny.gt.1.and.nz.gt.1) then
         ifelm=ifelmhex
         nsd=3
      elseif(ny.eq.1.and.nz.eq.1) then
         ifelm=ifelmlin
         nsd=1
      else
         ifelm=ifelmqud
         nsd=2
      endif
      nen=nelmnen(ifelm)
      nef=nelmnef(ifelm)
      nee=nelmnee(ifelm)
      call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
      call cmo_set_info('edges_per_element',cmo,nee,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmo,nsd,1,1,ierror)
C
      ntets_save=ntets
      ntets=ntets+nptsc
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_newlen(cmo,ierror)
C
      call cmo_get_intinfo('nnodes',cmo,npoints,length,
     *  icmotype,ierror)
      call cmo_get_intinfo('nelements',cmo,ntets,length,
     *  icmotype,ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,length,
     *  icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,leni,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,leni,icmotype,ier)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,leni,icmotype,ier)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,leni,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
C
      itetcnt=ntets_save
      do 200 iz=1,max(1,(nz-1))
         do 210 iy=1,max(1,(ny-1))
            do 220 ix=1,max(1,(nx-1))
               nxm1=nx-1
               nym1=ny-1
               nzm1=nz-1
               ixm1=ix-1
               iym1=iy-1
               izm1=iz-1
               ixp1=ix+1
               iyp1=iy+1
               izp1=iz+1
               if(isym.eq.1) then
                  if(ixm1.eq.0 ) ixm1=nx
                  if(ixp1.eq.nx) ixp1=1
               endif
               if(jsym.eq.1) then
                  if(iym1.eq.0 ) iym1=ny
                  if(iyp1.eq.ny) iyp1=1
               endif
               if(ksym.eq.1) then
                  if(izm1.eq.0 ) izm1=nz
                  if(izp1.eq.nz) izp1=1
               endif
               nw1=ivc(ix  ,iy  ,iz  )
               nw2=ivc(ix+1,iy  ,iz  )
               nw3=ivc(ix+1,iy+1,iz  )
               nw4=ivc(ix  ,iy+1,iz  )
               nw5=ivc(ix  ,iy  ,iz+1)
               nw6=ivc(ix+1,iy  ,iz+1)
               nw7=ivc(ix+1,iy+1,iz+1)
               nw8=ivc(ix  ,iy+1,iz+1)
               itetcnt=itetcnt+1
               if(ifelm.eq.ifelmhex) then
                  nv1=ivc(ix  ,iy  ,iz  )
                  nv2=ivc(ixp1,iy  ,iz  )
                  nv3=ivc(ixp1,iyp1,iz  )
                  nv4=ivc(ix  ,iyp1,iz  )
                  nv5=ivc(ix  ,iy  ,izp1)
                  nv6=ivc(ixp1,iy  ,izp1)
                  nv7=ivc(ixp1,iyp1,izp1)
                  nv8=ivc(ix  ,iyp1,izp1)
                  itet1(nen*(itetcnt-1)+1)=nv1+ifpt1-1
                  itet1(nen*(itetcnt-1)+2)=nv2+ifpt1-1
                  itet1(nen*(itetcnt-1)+3)=nv3+ifpt1-1
                  itet1(nen*(itetcnt-1)+4)=nv4+ifpt1-1
                  itet1(nen*(itetcnt-1)+5)=nv5+ifpt1-1
                  itet1(nen*(itetcnt-1)+6)=nv6+ifpt1-1
                  itet1(nen*(itetcnt-1)+7)=nv7+ifpt1-1
                  itet1(nen*(itetcnt-1)+8)=nv8+ifpt1-1
                  itettyp(itetcnt)=ifelm
                  itetoff(itetcnt)=nelmnen(itettyp(itetcnt))*(itetcnt-1)
                  jtetoff(itetcnt)=nelmnef(itettyp(itetcnt))*(itetcnt-1)
                  itetclr(itetcnt)=max(1,
     *                                 imt1(itet1(itetoff(itetcnt)+1)))
                  do i=1,nelmnen(itettyp(itetcnt))
                     i1=itet1(itetoff(itetcnt)+i)
                     itetclr(itetcnt)=max(1,imt1(i1))
                     xicvol(i)=xic(i1)
                     yicvol(i)=yic(i1)
                     zicvol(i)=zic(i1)
                  enddo
                  call volume_element(itettyp(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet1(nen*(itetcnt-1)+1)=nv1+ifpt1-1
                     itet1(nen*(itetcnt-1)+2)=nv4+ifpt1-1
                     itet1(nen*(itetcnt-1)+3)=nv3+ifpt1-1
                     itet1(nen*(itetcnt-1)+4)=nv2+ifpt1-1
                     itet1(nen*(itetcnt-1)+5)=nv5+ifpt1-1
                     itet1(nen*(itetcnt-1)+6)=nv8+ifpt1-1
                     itet1(nen*(itetcnt-1)+7)=nv7+ifpt1-1
                     itet1(nen*(itetcnt-1)+8)=nv6+ifpt1-1
                  endif
               elseif(ifelm.eq.ifelmqud) then
                  if(nz.eq.1) then
                     nv1=ivc(ix  ,iy  ,iz  )
                     nv2=ivc(ixp1,iy  ,iz  )
                     nv3=ivc(ixp1,iyp1,iz  )
                     nv4=ivc(ix  ,iyp1,iz  )
                     itet1(nen*(itetcnt-1)+1)=nv1+ifpt1-1
                     itet1(nen*(itetcnt-1)+2)=nv2+ifpt1-1
                     itet1(nen*(itetcnt-1)+3)=nv3+ifpt1-1
                     itet1(nen*(itetcnt-1)+4)=nv4+ifpt1-1
                  elseif(nx.eq.1) then
                     nv1=ivc(ix  ,iy  ,iz  )
                     nv2=ivc(ix  ,iyp1,iz  )
                     nv3=ivc(ix  ,iyp1,izp1)
                     nv4=ivc(ix  ,iy  ,izp1)
                     itet1(nen*(itetcnt-1)+1)=nv1+ifpt1-1
                     itet1(nen*(itetcnt-1)+2)=nv2+ifpt1-1
                     itet1(nen*(itetcnt-1)+3)=nv3+ifpt1-1
                     itet1(nen*(itetcnt-1)+4)=nv4+ifpt1-1
                  elseif(ny.eq.1) then
                     nv1=ivc(ix  ,iy  ,iz  )
                     nv2=ivc(ixp1,iy  ,iz  )
                     nv3=ivc(ixp1,iy  ,izp1)
                     nv4=ivc(ix  ,iy  ,izp1)
                     itet1(nen*(itetcnt-1)+1)=nv1+ifpt1-1
                     itet1(nen*(itetcnt-1)+2)=nv2+ifpt1-1
                     itet1(nen*(itetcnt-1)+3)=nv3+ifpt1-1
                     itet1(nen*(itetcnt-1)+4)=nv4+ifpt1-1
                  endif
                  itettyp(itetcnt)=ifelm
                  itetoff(itetcnt)=nelmnen(itettyp(itetcnt))*(itetcnt-1)
                  jtetoff(itetcnt)=nelmnef(itettyp(itetcnt))*(itetcnt-1)
                  do i=1,nelmnen(itettyp(itetcnt))
                     i1=itet1(itetoff(itetcnt)+i)
                     itetclr(itetcnt)=max(1,imt1(i1))
                     xicvol(i)=xic(i1)
                     yicvol(i)=yic(i1)
                     zicvol(i)=zic(i1)
                  enddo
                  call volume_element(itettyp(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet1(nen*(itetcnt-1)+2)=nv3+ifpt1-1
                     itet1(nen*(itetcnt-1)+3)=nv2+ifpt1-1
                  endif
               elseif(ifelm.eq.ifelmlin) then
                  nv1=ivc(ix  ,iy  ,iz  )
                  nv2=ivc(ixp1,iy  ,iz  )
                  itet1(nen*(itetcnt-1)+1)=nv1+ifpt1-1
                  itet1(nen*(itetcnt-1)+2)=nv2+ifpt1-1
                  itettyp(itetcnt)=ifelm
                  itetoff(itetcnt)=nelmnen(itettyp(itetcnt))*(itetcnt-1)
                  jtetoff(itetcnt)=nelmnef(itettyp(itetcnt))*(itetcnt-1)
                  itetclr(itetcnt)=max(1,
     *                                 imt1(itet1(itetoff(itetcnt)+1)))
                  do i=1,nelmnen(itettyp(itetcnt))
                     i1=itet1(itetoff(itetcnt)+i)
                     itetclr(itetcnt)=max(1,imt1(i1))
                     xicvol(i)=xic(i1)
                     yicvol(i)=yic(i1)
                     zicvol(i)=zic(i1)
                  enddo
                  call volume_element(itettyp(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet1(nen*(itetcnt-1)+1)=nv2+ifpt1-1
                     itet1(nen*(itetcnt-1)+2)=nv1+ifpt1-1
                  endif
               endif
 220        continue
 210     continue
 200  continue
C
      nelemavs=itetcnt
      length=nef*nelemavs
      call mmgetblk("jtet2",isubname,ipjtet2,length,2,icscode)
      numtet=nelemavs
C
      nnegvol=0
      voltot=0.0
      do it=ntets_save+1,numtet
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            xicvol(i)=xic(i1)
            yicvol(i)=yic(i1)
            zicvol(i)=zic(i1)
         enddo
C
         call volume_element(itettyp(it),
     *                       xicvol,yicvol,zicvol,
     *                       xtetvol)
C
         voltot=voltot+xtetvol
         if(xtetvol.lt.0) then
            nnegvol=nnegvol+1
         endif
      enddo
C
      write(logmess,9026) npoints
 9026 format("Number of nodes: ",i11)
      call writloga('default',0,logmess,0,ier)
      write(logmess,9027) ntets
 9027 format("Number of elements: ",i11)
      call writloga('default',0,logmess,0,ier)
      write(logmess,9028) nnegvol
 9028 format("Number of negative volume elements: ",i11)
      call writloga('default',0,logmess,0,ier)
      write(logmess,9029) voltot
 9029 format("Total volume:  ",e21.12)
      call writloga('default',0,logmess,0,ier)
C
      call geniee_cmo(cmo)
C
C     .................................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
         length=npoints
         call mmgetblk('idone',isubname,ipidone,length,2,icscode)
C
         do i=1,npoints
            idone(i)=0
            itp1(i)=0
         enddo
C
         do it=1,numtet
            do i=1,nelmnef(itettyp(it))
               if (jtet1(jtetoff(it)+i).ge.mbndry) then
                  do j=1,ielmface0(i,itettyp(it))
                     node1 = itet1(itetoff(it)+
     *                              ielmface1(j,i,itettyp(it)))
Cdcg                     itp1(node1)=ifitprfl
                  enddo
               endif
            enddo
         enddo
C
         do it=1,numtet
            do i=1,nelmnef(itettyp(it))
               if (jtet1(jtetoff(it)+i).gt.0.and.
     *             jtet1(jtetoff(it)+i).lt.mbndry) then
                  jt=1+(jtet1(jtetoff(it)+i)-1)/nef
                  jf=jtet1(jtetoff(it)+i)-nef*(it-1)
                  if(itetclr(it).ne.itetclr(jt)) then
                     do j=1,ielmface0(i,itettyp(it))
                        node1=itet1(itetoff(it)+
     *                               ielmface1(j,i,itettyp(it)))
                        if(idone(node1).eq.0) then
                           idone(node1)=1
                           if(itp1(node1).eq.ifitprfl) then
                              itp1(node1)=ifitpinb
                           else
                              itp1(node1)=ifitpini
                           endif
                        endif
                     enddo
                  endif
               endif
            enddo
         enddo
         call mmrelblk('idone',isubname,ipidone,icscode)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,ierror)
      return
      end
*dk,rzinterp
      subroutine rzinterp(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C#######################################################################
C
C   This command adds points to a mesh object. It creates points
C   by linear interpolation of coordinates between two point sets
C   For some special cases, it will also produce element connectivity
C   between the point sets.
C   
C   The combinations of input and output are:
C   
C   Source Element Type | Result Element Type
C   point                 point (no connectivity)
C   line                  quad
C   tri                   prism
C   quad                  hex
C   hybrid (quad,tri)     hybrid (hex, prism)
C   FORMAT:
C   createpts/interp/npoint/i1,i2,i3/j1,j2,j3/cmo_new
C
C
C#######################################################################
      implicit none
C
      include 'local_element.h'
C
      integer nwds
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer i1,i2,i3,i4,i5,i6,i7,i8,ipointi,ipointj,i1temp,
     *  it,iflag,itetcnt,itoff,jtoff,l,nef,nen,nsdtopo,
     *  nsdgeom,leni,i,nptsl,icmotp,nef1,nen1,nsdtopo1,
     *  nsdgeom1,ier,icscode,icmotype,nelements2,npoints_new,
     *  nelements_new,mpno2,mpno1,ics,length,itype,ierr,ilen,
     *  nnodes2,ilayers,nelements,nnodes,ierror,ierr2,mbndry
      real*8 xtetvol,dy1,dz1,dsfac,dx1
      character*70 logmess
      character*32 cmesh_type
C
C     __________________________________________________________________
C     Master Mesh Object.
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer itp1(1000000), imt1(1000000)
      pointer (ipxic1, xic1)
      pointer (ipyic1, yic1)
      pointer (ipzic1, zic1)
      real*8 xic1(10000000), yic1(10000000), zic1(10000000)
      pointer (ipitetclr1, itetclr1(10000000))
      pointer (ipitettyp1, itettyp1(10000000))
      pointer (ipitetoff1, itetoff1(10000000))
      pointer (ipjtetoff1, jtetoff1(10000000))
      pointer (ipitet1, itet1(10000000))
      pointer (ipjtet1, jtet1(10000000))
      integer itet1,jtet1,itetclr1,itettyp1,itetoff1,jtetoff1
C
C     __________________________________________________________________
C     Slave Mesh Object.
C
      pointer (ipimt2, imt2)
      pointer (ipitp2, itp2)
      integer itp2(1000000), imt2(1000000)
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      real*8 xic2(10000000), yic2(10000000), zic2(10000000)
      pointer (ipitetclr2, itetclr2(10000000))
      pointer (ipitettyp2, itettyp2(10000000))
      pointer (ipitetoff2, itetoff2(10000000))
      pointer (ipjtetoff2, jtetoff2(10000000))
      pointer (ipitet2, itet2(10000000))
      pointer (ipjtet2, jtet2(10000000))
      integer itet2,jtet2,itetclr2,itettyp2,itetoff2,jtetoff2
C
      pointer (ipisetwd, isetwd)
      integer isetwd(1000000)
C
      pointer (ipmpary1, mpary1)
      pointer (ipmpary2, mpary2)
      integer mpary1(1000000), mpary2(1000000)
C
      pointer (ipipflag1, ipflag1)
      integer ipflag1(1000000)
      pointer (ipitflag1, itflag1)
      integer itflag1(1000000)
C
      character*32 isubname, cmo, cmonew
      integer ip1,ip2,ip3,ip4,ip5,ip6
C
      real*8 xicvol(100), yicvol(100), zicvol(100)
C
C#######################################################################
C
      isubname='rzinterp'
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_intinfo('nnodes',cmo,nnodes,ilen,itype,ierr)
      call cmo_get_intinfo('nelements',cmo,nelements,ilen,itype,ierr)
C
      ilayers=imsgin(2)
      length=nnodes
      call mmgetblk('mpary1',isubname,ipmpary1,length,1,ics)
      call mmgetblk('mpary2',isubname,ipmpary2,length,1,ics)
      if(msgtype(3).eq.1) then
         ip1=imsgin(3)
         ip2=imsgin(4)
         ip3=imsgin(5)
         if((ip1.eq.1).and.(ip2.eq.0).and.(ip3.eq.0))then
           ip1 = 1
           ip2 = nnodes/2
           ip3 = 1
         endif
         mpno1=0
         do i1=ip1,ip2,ip3
            mpno1=mpno1+1
            mpary1(mpno1)=i1
         enddo
      else
         call cmo_get_info('isetwd',cmo,
     *                     ipisetwd,leni,icmotype,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ics)
         call pntlimc(cmsgin(3),cmsgin(4),cmsgin(5),
     *                ipmpary1,mpno1,nnodes,isetwd,itp1)
      endif
      if(msgtype(6).eq.1) then
         ip4=imsgin(6)
         ip5=imsgin(7)
         ip6=imsgin(8)
         if((ip4.eq.1).and.(ip5.eq.0).and.(ip6.eq.0))then
           ip4 = (nnodes/2) + 1
           ip5 =  nnodes
           ip6 =  1
         endif
         mpno2=0
         do i1=ip4,ip5,ip6
            mpno2=mpno2+1
            mpary2(mpno2)=i1
         enddo
      else
         call cmo_get_info('isetwd',cmo,
     *                     ipisetwd,leni,icmotype,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ics)
         call pntlimc(cmsgin(6),cmsgin(7),cmsgin(8),
     *                ipmpary2,mpno2,nnodes,isetwd,itp1)
      endif
C
      if(mpno1 .ne. mpno2) then
         write(logmess,9025)
 9025    format("ERROR:rzinterp")
         call writloga('default',0,logmess,0,ier)
         write(logmess,9026) mpno1, mpno2
 9026    format("Number of nodes in FROM not equal TO: ",i11,i11)
         call writloga('default',0,logmess,0,ier)
         goto 9999
      endif
      
      cmonew=cmsgin(9)
C
      call cmo_exist(cmonew,icscode)
      if(icscode.eq.0) then
         call cmo_get_intinfo('nnodes',cmonew,nnodes2,ilen,itype,ierr)
         call cmo_get_intinfo('nelements',cmonew,
     *                     nelements2,ilen,itype,ierr)
         call cmo_select(cmonew,icscode)
         npoints_new=nnodes2+(2+ilayers)*mpno1
         nelements_new=nelements2+(2+ilayers-1)*nelements
      else
         nnodes2=0
         nelements2=0
         call cmo_derive(cmonew,cmo,icscode)
         npoints_new=(2+ilayers)*mpno1
         nelements_new=(2+ilayers-1)*nelements
      endif
C
      call cmo_get_intinfo('ndimensions_geom',cmo,nsdgeom1,ilen,
     *  itype,ier)
      call cmo_get_intinfo('ndimensions_topo',cmo,nsdtopo1,ilen,
     *  itype,ier)
      call cmo_get_intinfo('nodes_per_element',cmo,nen1,ilen,
     *  itype,ierr)
      call cmo_get_intinfo('faces_per_element',cmo,nef1,ilen,
     *  itype,ierr)
      if(nsdtopo1.eq.3) then
         write(logmess,9036) nsdtopo1
 9036    format("Grid topology too high: ",i11)
         call writloga('default',0,logmess,0,ier)
         goto 9999
      endif
      if(nen1.eq.nelmnen(ifelmhyb) .and.
     *   nef1.eq.nelmnef(ifelmhyb)) then
         nsdgeom=3
         if(nsdtopo1.eq.1) then
            nsdtopo=2
         else
            nsdtopo=3
         endif
         nen=nelmnen(ifelmhyb)
         nef=nelmnef(ifelmhyb)
         cmesh_type = 'hyb'
      else
         if(nsdtopo1.eq.2) then
            if(nen1.eq.nelmnen(ifelmtri) .and.
     *         nef1.eq.nelmnef(ifelmtri)) then
               nen=nelmnen(ifelmpri)
               nef=nelmnef(ifelmpri)
               nsdgeom = 3
               nsdtopo = 3
               cmesh_type = 'pri'
            elseif(nen1.eq.nelmnen(ifelmqud) .and.
     *             nef1.eq.nelmnef(ifelmqud)) then
               nen=nelmnen(ifelmhex)
               nef=nelmnef(ifelmhex)
               nsdgeom = 3
               nsdtopo = 3
               cmesh_type = 'hex'
            endif
         elseif(nsdtopo1.eq.1) then
            nsdgeom=3
            nsdtopo=2
            if(nen1.eq.nelmnen(ifelmlin) .and.
     *         nef1.eq.nelmnef(ifelmlin)) then
               nen=nelmnen(ifelmqud)
               nef=nelmnef(ifelmqud)
               cmesh_type = 'qud'
            endif
         endif
      endif
C
      call cmo_set_info('nnodes',cmonew,npoints_new,1,1,ierr)
      call cmo_set_info('nelements',cmonew,nelements_new,1,1,ierr)
c
c      call cmo_set_info('ndimensions_geom',cmonew,nsdgeom,1,1,ier)
c      call cmo_set_info('ndimensions_topo',cmonew,nsdtopo,1,1,ier)
c      call cmo_set_info('nodes_per_element',cmonew,nen,1,1,ierr)
c      call cmo_set_info('faces_per_element',cmonew,nef,1,1,ierr)
c
c     The set_info calls above were incomplete. Replace with set_mesh_type
c
      call cmo_set_mesh_type(cmonew, cmesh_type, ierr)
C
      call cmo_newlen(cmonew,ierr)
C
      call cmo_get_intinfo('mbndry',cmonew,mbndry,
     *                  leni,icmotp,ierror)
      call cmo_get_info('imt1',cmonew,ipimt2,leni,icmotp,ierr)
      call cmo_get_info('itp1',cmonew,ipitp2,leni,icmotp,ierr)
      call cmo_get_info('xic',cmonew,ipxic2,leni,icmotp,ierr)
      call cmo_get_info('yic',cmonew,ipyic2,leni,icmotp,ierr)
      call cmo_get_info('zic',cmonew,ipzic2,leni,icmotp,ierr)
      call cmo_get_info('itetclr',cmonew,
     *                  ipitetclr2,leni,icmotp,ier)
      call cmo_get_info('itettyp',cmonew,
     *                  ipitettyp2,leni,icmotp,ier)
      call cmo_get_info('itetoff',cmonew,
     *                  ipitetoff2,leni,icmotp,ier)
      call cmo_get_info('jtetoff',cmonew,
     *                  ipjtetoff2,leni,icmotp,ier)
      call cmo_get_info('itet',cmonew,ipitet2,leni,icmotp,ierr)
      call cmo_get_info('jtet',cmonew,ipjtet2,leni,icmotp,ierr)
C
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotp,ierr)
      call cmo_get_info('xic',cmo,ipxic1,leni,icmotp,ierr)
      call cmo_get_info('yic',cmo,ipyic1,leni,icmotp,ierr)
      call cmo_get_info('zic',cmo,ipzic1,leni,icmotp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr1,leni,icmotp,ier)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp1,leni,icmotp,ier)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff1,leni,icmotp,ier)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff1,leni,icmotp,ier)
      call cmo_get_info('itet',cmo,ipitet1,leni,icmotp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet1,leni,icmotp,ierr)
C
      nptsl=mpno1
      do i=1,mpno1
         i1=mpary1(i)
         imt2(nnodes2+i)=imt1(i1)
         itp2(nnodes2+i)=itp1(i1)
         xic2(nnodes2+i)=xic1(i1)
         yic2(nnodes2+i)=yic1(i1)
         zic2(nnodes2+i)=zic1(i1)
      enddo
      do i=1,mpno2
         i1=mpary2(i)
         imt2(nnodes2+nptsl*(2+ilayers-1)+i)=imt1(i1)
         itp2(nnodes2+nptsl*(2+ilayers-1)+i)=itp1(i1)
         xic2(nnodes2+nptsl*(2+ilayers-1)+i)=xic1(i1)
         yic2(nnodes2+nptsl*(2+ilayers-1)+i)=yic1(i1)
         zic2(nnodes2+nptsl*(2+ilayers-1)+i)=zic1(i1)
      enddo
      if(ilayers.gt.0) then
         do l=2,(2+ilayers)-1
            dsfac=real(l-1)/real(ilayers+1)
            do i=1,mpno1
               i1=mpary1(i)
               dx1=xic2(nnodes2+nptsl*(2+ilayers-1)+i1)-xic2(nnodes2+i1)
               dy1=yic2(nnodes2+nptsl*(2+ilayers-1)+i1)-yic2(nnodes2+i1)
               dz1=zic2(nnodes2+nptsl*(2+ilayers-1)+i1)-zic2(nnodes2+i1)
               imt2(nnodes2+nptsl*(l-1)+i1)=imt2(nnodes2+i1)
               itp2(nnodes2+nptsl*(l-1)+i1)=itp2(nnodes2+i1)
               xic2(nnodes2+nptsl*(l-1)+i1)=xic2(nnodes2+i1)+dx1*dsfac
               yic2(nnodes2+nptsl*(l-1)+i1)=yic2(nnodes2+i1)+dy1*dsfac
               zic2(nnodes2+nptsl*(l-1)+i1)=zic2(nnodes2+i1)+dz1*dsfac
            enddo
         enddo
      endif
C
      length=nnodes
      call mmgetblk('ipflag1',isubname,ipipflag1,length,1,icscode)
      do i=1,nnodes
         ipflag1(i)=0
      enddo
      length=nelements
C
C     To avoid asking for 0 length memory
C
      if (nelements .eq. 0)length = 1
C
      call mmgetblk('itflag1',isubname,ipitflag1,length,1,icscode)
      do it=1,nelements
         itflag1(it)=0
      enddo
      do i=1,mpno1
         i1=mpary1(i)
         ipflag1(i1)=i
      enddo
      do it=1,nelements
         iflag=0
         do i=1,nelmnen(itettyp1(it))
            i1=itet1(itetoff1(it)+i)
            if(ipflag1(i1).gt.0) then
               iflag=iflag+1
            endif
         enddo
         if(iflag.eq.nelmnen(itettyp1(it))) then
            itflag1(it)=1
         endif
      enddo
C
      if(nelements2.le.0) then
         itetcnt=0
         itoff=0
         jtoff=0
      else
         itetcnt=nelements2
         itoff=itetoff2(nelements2)+nelmnen(itettyp2(nelements2))
         jtoff=jtetoff2(nelements2)+nelmnef(itettyp2(nelements2))
      endif
      do l=1,2+ilayers-1
         do it=1,nelements
            if(itflag1(it).eq.1) then
               if(itettyp1(it).eq.ifelmlin) then
                  i1=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+1)
                  i2=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+2)
                  i4=nptsl+i1
                  i3=nptsl+i2
                  itetcnt=itetcnt+1
                  itetclr2(itetcnt)=itetclr1(it)
                  itettyp2(itetcnt)=ifelmqud
                  itetoff2(itetcnt)=itoff
                  jtetoff2(itetcnt)=jtoff
                  itoff=itoff+nelmnen(itettyp2(itetcnt))
                  jtoff=jtoff+nelmnef(itettyp2(itetcnt))
                  itet2(itetoff2(itetcnt)+1)=i1
                  itet2(itetoff2(itetcnt)+2)=i2
                  itet2(itetoff2(itetcnt)+3)=i3
                  itet2(itetoff2(itetcnt)+4)=i4
                  do i=1,nelmnef(itettyp2(it))
                     jtet2(jtetoff2(itetcnt)+i)=-1
                  enddo
                  do i=1,nelmnen(itettyp2(itetcnt))
                     i1temp=itet2(itetoff2(itetcnt)+i)
                     xicvol(i)=xic2(i1temp)
                     yicvol(i)=yic2(i1temp)
                     zicvol(i)=zic2(i1temp)
                  enddo
                  call volume_element(itettyp2(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet2(itetoff2(itetcnt)+1)=i1
                     itet2(itetoff2(itetcnt)+2)=i4
                     itet2(itetoff2(itetcnt)+3)=i3
                     itet2(itetoff2(itetcnt)+4)=i2
                  endif
               elseif(itettyp1(it).eq.ifelmtri) then
                  i1=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+1)
                  i2=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+2)
                  i3=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+3)
                  i4=nptsl+i1
                  i5=nptsl+i2
                  i6=nptsl+i3
                  itetcnt=itetcnt+1
                  itetclr2(itetcnt)=itetclr1(it)
                  itettyp2(itetcnt)=ifelmpri
                  itetoff2(itetcnt)=itoff
                  jtetoff2(itetcnt)=jtoff
                  itoff=itoff+nelmnen(itettyp2(itetcnt))
                  jtoff=jtoff+nelmnef(itettyp2(itetcnt))
                  itet2(itetoff2(itetcnt)+1)=i1
                  itet2(itetoff2(itetcnt)+2)=i2
                  itet2(itetoff2(itetcnt)+3)=i3
                  itet2(itetoff2(itetcnt)+4)=i4
                  itet2(itetoff2(itetcnt)+5)=i5
                  itet2(itetoff2(itetcnt)+6)=i6
                  do i=1,nelmnef(itettyp2(it))
                     jtet2(jtetoff2(itetcnt)+i)=-1
                  enddo
                  do i=1,nelmnen(itettyp2(itetcnt))
                     i1temp=itet2(itetoff2(itetcnt)+i)
                     xicvol(i)=xic2(i1temp)
                     yicvol(i)=yic2(i1temp)
                     zicvol(i)=zic2(i1temp)
                  enddo
                  call volume_element(itettyp2(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
C*****                     itet2(itetoff2(itetcnt)+1)=i1
C*****                     itet2(itetoff2(itetcnt)+2)=i3
C*****                     itet2(itetoff2(itetcnt)+3)=i2
C*****                     itet2(itetoff2(itetcnt)+4)=i4
C*****                     itet2(itetoff2(itetcnt)+5)=i6
C*****                     itet2(itetoff2(itetcnt)+6)=i5
                  endif
               elseif(itettyp1(it).eq.ifelmqud) then
                  i1=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+1)
                  i2=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+2)
                  i3=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+3)
                  i4=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+4)
                  i5=nptsl+i1
                  i6=nptsl+i2
                  i7=nptsl+i3
                  i8=nptsl+i4
                  itetcnt=itetcnt+1
                  itetclr2(itetcnt)=itetclr1(it)
                  itettyp2(itetcnt)=ifelmhex
                  itetoff2(itetcnt)=itoff
                  jtetoff2(itetcnt)=jtoff
                  itoff=itoff+nelmnen(itettyp2(itetcnt))
                  jtoff=jtoff+nelmnef(itettyp2(itetcnt))
                  itet2(itetoff2(itetcnt)+1)=i1
                  itet2(itetoff2(itetcnt)+2)=i2
                  itet2(itetoff2(itetcnt)+3)=i3
                  itet2(itetoff2(itetcnt)+4)=i4
                  itet2(itetoff2(itetcnt)+5)=i5
                  itet2(itetoff2(itetcnt)+6)=i6
                  itet2(itetoff2(itetcnt)+7)=i7
                  itet2(itetoff2(itetcnt)+8)=i8
                  do i=1,nelmnef(itettyp2(it))
                     jtet2(jtetoff2(itetcnt)+i)=-1
                  enddo
                  do i=1,nelmnen(itettyp2(itetcnt))
                     i1temp=itet2(itetoff2(itetcnt)+i)
                     xicvol(i)=xic2(i1temp)
                     yicvol(i)=yic2(i1temp)
                     zicvol(i)=zic2(i1temp)
                  enddo
                  call volume_element(itettyp2(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet2(itetoff2(itetcnt)+1)=i1
                     itet2(itetoff2(itetcnt)+2)=i4
                     itet2(itetoff2(itetcnt)+3)=i3
                     itet2(itetoff2(itetcnt)+4)=i2
                     itet2(itetoff2(itetcnt)+5)=i5
                     itet2(itetoff2(itetcnt)+6)=i8
                     itet2(itetoff2(itetcnt)+7)=i7
                     itet2(itetoff2(itetcnt)+8)=i6
                     do i=1,nelmnen(itettyp2(itetcnt))
                        i1temp=itet2(itetoff2(itetcnt)+i)
                        xicvol(i)=xic2(i1temp)
                        yicvol(i)=yic2(i1temp)
                        zicvol(i)=zic2(i1temp)
                     enddo
                     call volume_element(itettyp2(itetcnt),
     *                                   xicvol,yicvol,zicvol,
     *                                   xtetvol)
                  endif
               else
                  write(logmess,9046) it
 9046             format("Illegal element to rzinterp: ",i11)
                  call writloga('default',0,logmess,0,ier)
               endif
            endif
         enddo
      enddo
C
      call cmo_set_info('nelements',cmonew,itetcnt,1,1,ierr)
C
      ipointi=nnodes2+1
      ipointj=npoints_new
      call cmo_set_info('ipointi',cmonew,
     *                ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_info('ipointj',cmonew,
     *                ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
      call geniee_cmo(cmonew)
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
 
