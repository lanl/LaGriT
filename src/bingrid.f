*dk,bingrid
      subroutine bingrid(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C#######################################################################
C
C     PURPOSE - To indicate which tets should be refined based on
C               conditions placed on the tet.
C
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C          nadd     -
C          ipitadd  -
C
C     CHANGE HISTORY - 11/26/94 algorithm changes by D. Kilcrease.
C
C        $Log: bingrid.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Thu Apr 06 08:12:22 2000   dcg
CPVCS    remove get_info_i calls
CPVCS
CPVCS       Rev 1.2   Wed Apr 05 13:34:04 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:39:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 15:20:52 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      include "local_element.h"
C
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer imt1(1000000), itp1(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ippic, pic)
      real*8 pic(1000000)
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000), jtet1(1000000)
C
      pointer (ipmpary1, mpary1)
      integer mpary1(1000000)
C
      pointer (ipireal1, ireal1)
      integer ireal1(1000000)
C
      pointer (ipxreal, xreal)
      pointer (ipyreal, yreal)
      pointer (ipzreal, zreal)
      real*8 xreal(1000000), yreal(1000000), zreal(1000000)
C
      pointer (ipinodebin, inodebin)
      integer inodebin(1000000)
C
      pointer (ipibincnt, ibincnt)
      pointer (ipibinoff, ibinoff)
      pointer (ipibinnode, ibinnode)
      integer ibincnt(1000000), ibinoff(1000000), ibinnode(1000000)
C
C#######################################################################
C
      character*32 isubname
      character*32 cmo, cmo_bin, cmo_node
      character*32 coption, cgeom
C
      character*32 ich1,ich2,ich3
C
      character*8192 ibuff
C
C#######################################################################
C
C
      isubname='bingrid'
C
      coption=cmsgin(2)
C
      cmo_bin=cmsgin(3)
      cmo_node=cmsgin(4)
C
      call cmo_select(cmo_node,icscode)
C
      cmo=cmo_node
C
      call cmo_get_info('nnodes',cmo,
     *                  npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  ntets,length,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,
     *                  ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
c
c     ******************************************************************
c
c     set the point index boundaries.
c
      ipointi=0
      ipointj=0
      call cmo_get_info('ipointi',cmo,ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      istart=5
      if(msgtype(istart).eq.1.and.imsgin(istart).eq.0) then
         imsgin(istart)=max(1,ipointi)
      endif
      if(msgtype(istart+1).eq.1.and.imsgin(istart+1).eq.0) then
         if(ipointj.le.0.or.imsgin(istart+1).eq.1) then
            imsgin(istart+1)=npoints
         else
            imsgin(istart+1)=ipointj
         endif
      endif
      if(msgtype(istart+2).eq.1.and.imsgin(istart+2).eq.0) then
         imsgin(istart+2)=1
      endif
C
      ich1=' '
      ich2=' '
      ich3=' '
      if(msgtype(istart).eq.1) then
        ipt1=max(1,min(imsgin(istart),npoints))
        ipt2=max(1,min(imsgin(istart+1),npoints))
        ipt3=max(1,min(imsgin(istart+2),npoints))
      else
        ich1=cmsgin(istart)
        ich2=cmsgin(istart+1)
        ich3=cmsgin(istart+2)
      endif
C
C     ******************************************************************
C
C     check point limits and translate to valid limits if necessary.
C
      mpno=npoints
      length=npoints
      call mmgetblk('mpary1',isubname,ipmpary1,length,2,icscode)
      if(msgtype(istart).eq.1) then
         call pntlimn(ipt1,ipt2,ipt3,ipmpary1,mpno,npoints,isetwd,itp1)
      elseif(msgtype(istart).ne.1) then
         call pntlimc(ich1,ich2,ich3,ipmpary1,mpno,npoints,isetwd,itp1)
      endif
C
C
C     ******************************************************************
C     SET THE SEARCH RANGE TO SMALLEST X, Y, OR Z RANGE
C
      call mmgetblk('ireal1',isubname,ipireal1,length,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierrdum)
      if(ierrdum.ne.0) call x3d_error(isubname,'unpacktp')
C
      nreal=0
      do i=1,mpno
         i1=mpary1(i)
         if(ireal1(i1).eq.1) nreal=nreal+1
      enddo
C
      length=nreal
      call mmgetblk('xreal',isubname,ipxreal,length,2,icscode)
      call mmgetblk('yreal',isubname,ipyreal,length,2,icscode)
      call mmgetblk('zreal',isubname,ipzreal,length,2,icscode)
      nreal=0
      do i=1,mpno
         i1=mpary1(i)
         if(ireal1(i1).eq.1) then
            nreal=nreal+1
            xreal(nreal)=xic(i1)
            yreal(nreal)=yic(i1)
            zreal(nreal)=zic(i1)
         endif
      enddo
C
      index=ismin(nreal,xreal,1)
      xmin1=xreal(index)
      index=ismax(nreal,xreal,1)
      xmax1=xreal(index)
      xdiff=xmax1-xmin1
C
      index=ismin(nreal,yreal,1)
      ymin1=yreal(index)
      index=ismax(nreal,yreal,1)
      ymax1=yreal(index)
      ydiff=ymax1-ymin1
C
      index=ismin(nreal,zreal,1)
      zmin1=zreal(index)
      index=ismax(nreal,zreal,1)
      zmax1=zreal(index)
      zdiff=zmax1-zmin1
C
      xmax1=xmax1+0.01*xdiff
      xmin1=xmin1-0.01*xdiff
      ymax1=ymax1+0.01*ydiff
      ymin1=ymin1-0.01*ydiff
      zmax1=zmax1+0.01*zdiff
      zmin1=zmin1-0.01*zdiff
C
C
C     ******************************************************************
C     Delete the bin-grid CMO.
C
      if(coption(1:icharlnf(coption)).eq.'delete') then
C
         call cmo_exist(cmo_bin,ierror)
         if(ierror.eq.0) then
            call cmo_release(cmo_bin,ierror)
         endif
C
C
C     ******************************************************************
C     Create the bin-grid CMO.
C
C
      elseif(coption(1:icharlnf(coption)).eq.'logical') then
C
         call cmo_exist(cmo_bin,ierror)
         if(ierror.eq.0) then
            call cmo_release(cmo_bin,ierror)
         endif
         call cmo_create(cmo_bin,icscode)
C
C           ...............................................................
C        Create the bin-grid for a logical operator
C
         cgeom=cmsgin(8)
         nx=imsgin(9)
         ny=imsgin(10)
         nz=imsgin(11)
         if(nwds.le.11) then
            ibuff=' '
            write(ibuff,9000) 'rz/',
     *            'xyz/',
     *            nx,ny,nz,
     *            xmin1, ymin1, zmin1,
     *            xmax1, ymax1, zmax1,
     *            1,1,1
            call dotaskx3d(ibuff,ierror)
            write(ibuff,9010) 'rzbrick/xyz/',
     *                        nx,ny,nz,1,0,0,
     *                        'connect'
            call dotaskx3d(ibuff,ierror)
         elseif(nwds.le.20) then
            xmin1=xmsgin(12)
            ymin1=xmsgin(13)
            zmin1=xmsgin(14)
            xmax1=xmsgin(15)
            ymax1=xmsgin(16)
            zmax1=xmsgin(17)
            ixz=imsgin(18)
            iyz=imsgin(19)
            izz=imsgin(20)
            ibuff=' '
            if(cgeom(1:icharlnf(cgeom)).eq.'xyz') then
               mx=nx
               my=ny
               mz=nz
            elseif(cgeom(1:icharlnf(cgeom)).eq.'rtz') then
               mx=ny
               my=nx
               mz=nz
            elseif(cgeom(1:icharlnf(cgeom)).eq.'rtp') then
               mx=nz
               my=ny
               mz=nx
            endif
            write(ibuff,9000) 'rz/',
     *            (cgeom(1:icharlnf(cgeom)) // '/'),
     *            nx,ny,nz,
     *            xmin1, ymin1, zmin1,
     *            xmax1, ymax1, zmax1,
     *            ixz,iyz,izz
            call dotaskx3d(ibuff,ierror)
            write(ibuff,9010) 'rzbrick/xyz/',
     *                        mx,my,mz,1,0,0,
     *                        'connect'
            call dotaskx3d(ibuff,ierror)
         endif
C
      elseif(coption(1:icharlnf(coption)).eq.'binmap') then
C
C        ...............................................................
C        Create a CMO attribute to map which bins have nodes. The value
C           of the map indicates how many nodes a bin contains.
C
C
C
         call mmfindbk('ibincnt',cmo_bin,ipibincnt,lenout,icscode)
         if(icscode.eq.0) then
         else
            ibuff='cmo/addatt/' //
     *            cmo_bin(1:icharlnf(cmo_bin)) //
     *            '/' //
     *            'ibincnt' //
     *            '/VINT' //
     *            '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *            ' ; finish '
            call dotaskx3d(ibuff,ierror)
            call mmfindbk('ibincnt',cmo_bin,ipibincnt,lenout,icscode)
         endif
C
         call mmfindbk('ibinoff',cmo_bin,ipibinoff,lenout,icscode)
         if(icscode.eq.0) then
         else
            ibuff='cmo/addatt/' //
     *            cmo_bin(1:icharlnf(cmo_bin)) //
     *            '/' //
     *            'ibinoff' //
     *            '/VINT' //
     *            '/scalar/nnodes/linear/permanent/x/0.0' //
     *            ' ; finish '
            call dotaskx3d(ibuff,ierror)
            call mmfindbk('ibinoff',cmo_bin,ipibinoff,lenout,icscode)
         endif
C
         call mmfindbk('nbinnode',cmo_bin,ipnbinnode,lenout,icscode)
         if(icscode.eq.0) then
         else
            ibuff='cmo/addatt/' //
     *            cmo_bin(1:icharlnf(cmo_bin)) //
     *            '/' //
     *            'nbinnode' //
     *            '/INT' //
     *            '/scalar/scalar/constant/permanent/x/0.0' //
     *            ' ; finish '
            call dotaskx3d(ibuff,ierror)
         endif
C
         call mmfindbk('ibinnode',cmo_bin,ipibinnode,lenout,icscode)
         if(icscode.eq.0) then
         else
            ibuff='cmo/addatt/' //
     *            cmo_bin(1:icharlnf(cmo_bin)) //
     *            '/' //
     *            'ibinnode' //
     *            '/VINT' //
     *            '/scalar/nbinnode/linear/permanent/x/0.0' //
     *            ' ; finish '
            call dotaskx3d(ibuff,ierror)
            call mmfindbk('ibinlst',cmo_bin,ipibinlst,lenout,icscode)
         endif
C
         call mmfindbk('nnodebin',cmo_bin,ipnnodebin,lenout,icscode)
         if(icscode.eq.0) then
         else
            ibuff='cmo/addatt/' //
     *            cmo_bin(1:icharlnf(cmo_bin)) //
     *            '/' //
     *            'nnodebin' //
     *            '/INT' //
     *            '/scalar/scalar/constant/permanent/x/0.0' //
     *            ' ; finish '
            call dotaskx3d(ibuff,ierror)
         endif
C
         call mmfindbk('inodebin',cmo_bin,ipinodebin,lenout,icscode)
         if(icscode.eq.0) then
         else
            ibuff='cmo/addatt/' //
     *            cmo_bin(1:icharlnf(cmo_bin)) //
     *            '/' //
     *            'inodebin' //
     *            '/VINT' //
     *            '/scalar/nnodebin/linear/permanent/x/0.0' //
     *            ' ; finish '
            call dotaskx3d(ibuff,ierror)
            call mmfindbk('inodebin',cmo_bin,ipinodebin,lenout,icscode)
         endif
C
         nnodebin=nreal
         call cmo_set_info('nnodebin',cmo_bin,nnodebin,1,1,ierror)
         call cmo_newlen(cmo_bin,ierror)
         call cmo_get_info('inodebin',cmo_bin,
     *                     ipinodebin,length,icmotype,ierror)
C
C        ...............................................................
C        For each grid point find the bin that contains it.
C
         call table_element(cmo_bin,
     &                      ipxreal,ipyreal,ipzreal,nnodebin,
     &                      ipinodebin,
     &                      ierr2)
C
C        ...............................................................
C        Check to see if each node is contain in a bin. If node is not
C           contained in a bin, then set it to bin=1.
C
         do i=1,nnodebin
            if(inodebin(i).le.0) then
               inodebin(i)=1
            endif
         enddo
C
C        ...............................................................
C        For each grid point find the bin that contains it.
C
C
         cmo=cmo_bin
C
         call cmo_get_info('nelements',cmo_bin,
     *                     nbins,length,icmotype,ierror)
C
         do i=1,nbins
            ibincnt(i)=0
            ibinoff(i)=0
         enddo
         do i=1,nreal
            ibin=inodebin(i)
            ibincnt(ibin)=ibincnt(ibin)+1
         enddo
         ioffset=0
         do ibin=1,nbins
            ibinoff(ibin)=ioffset
            ioffset=ioffset+ibincnt(ibin)
         enddo
C
C        ...............................................................
C        Set the length of the bin-list and resize the CMO.
C
         nbinnode=ioffset
         call cmo_set_info('nbinnode',cmo_bin,nbinnode,1,1,ierror)
         call cmo_newlen(cmo_bin,ierror)
         call cmo_get_info('ibinnode',cmo_bin,
     *                     ipibinnode,length,icmotype,ierror)
C
         do i=1,nbins
            ibincnt(i)=0
         enddo
         do i=1,nreal
            i1=mpary1(i)
            ibin=inodebin(i)
            ibincnt(ibin)=ibincnt(ibin)+1
            ibinnode(ibinoff(ibin)+ibincnt(ibin))=i1
         enddo
C
         call cmo_get_info('pic',cmo_node,
     *                     ippic,length,icmotype,ierror)
C
         do i=1,length
            pic(i)=0.0
         enddo
C
         do i=1,nreal
            i1=mpary1(i)
            pic(i1)=inodebin(i)
         enddo
C
C
C        ...............................................................
C
C
      endif
C
C
C     ******************************************************************
C
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
 9000 format(a3,a,3(i3,'/'),6(1pe15.7,'/'),3(i3,'/'),' ; finish')
 9010 format(a12,6(i3,'/'),a7,' ; finish')
C
      return
      end
