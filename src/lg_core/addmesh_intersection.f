      subroutine addmesh_intersection(cmoa,cmob,pset_name,ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine delete elements from a mesh_object.
C
C     INPUT ARGUMENTS -
C
C        cmoa         - The mesh_object (source1).
C        cmob          - The mesh_object (source1).
C
C     OUTPUT ARGUMENTS -
C
C        cmoa - The mesh_object (sink).
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_intersection.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   21 Apr 2000 08:49:26   gable
CPVCS    Got rid of zq commands and added finish to some of the dotask commands.
CPVCS    
CPVCS       Rev 1.3   Wed Apr 05 13:29:10 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.2   Fri Oct 23 13:02:40 1998   dcg
CPVCS    remove zero length string - DEC compiler complains
CPVCS
CPVCS       Rev 1.1   Fri Oct 17 11:05:46 1997   dcg
CPVCS    remove concatenation // from write statements
CPVCS
CPVCS       Rev 1.0   Wed Oct 08 12:54:00 1997   gable
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
      include "local_element.h"
C
C ######################################################################
C
      character*(*) cmoa, cmob
      integer ierror
C
C ######################################################################
C
C
      pointer (ipimt1, imt1(1000000))
      pointer (ipitp1, itp1(1000000))
      pointer (ipxic1, xic1)
      pointer (ipyic1, yic1)
      pointer (ipzic1, zic1)
      pointer (ipitetclr1, itetclr1(1000000))
      pointer (ipitettyp1, itettyp1(1000000))
      pointer (ipitetoff1, itetoff1(1000000))
      pointer (ipjtetoff1, jtetoff1(1000000))
      pointer (ipitet1, itet1(10000000))
      pointer (ipjtet1, jtet1(10000000))
      dimension xic1(1000000), yic1(1000000), zic1(1000000)
      pointer (ipimt2, imt2(1000000))
      pointer (ipitp2, itp2(1000000))
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      pointer (ipitetclr2, itetclr2(1000000))
      pointer (ipitettyp2, itettyp2(1000000))
      pointer (ipitetoff2, itetoff2(1000000))
      pointer (ipjtetoff2, jtetoff2(1000000))
      pointer (ipitet2, itet2(10000000))
      pointer (ipjtet2, jtet2(10000000))
      dimension xic2(1000000), yic2(1000000), zic2(1000000)
C
C
      pointer (ipipflag, ipflag)
      pointer (ipipflaga, ipflag1)
      pointer (ipipflagb, ipflag2)
      pointer (ipitflag, itflag)
      pointer (ipitflag1, itflag1)
      pointer (ipitflag2, itflag2)
      integer ipflag(1000000), ipflag1(1000000), ipflag2(1000000)
      integer itflag(1000000), itflag1(1000000), itflag2(1000000)
C
C
      pointer (ipitadd, itadd)
      integer itadd(1000000)
C
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(1000000), yadd(1000000), zadd(1000000)
C
C
      character*32 isubname, pset_name
      character*132 logmess, cbuff
C
C ######################################################################
C
      integer itetface0(4), itetface1(4,4)
      integer itet_edge(2,6)
      integer itri_edge(2,3)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 1,
     *                 1, 4, 3, 2,
     *                 1, 2, 4, 3,
     *                 1, 3, 2, 4 /
      data itet_edge / 1, 2,
     *                 1, 3,
     *                 1, 4,
     *                 2, 3,
     *                 2, 4,
     *                 3, 4 /
      data itri_edge / 1, 2,
     *                 1, 3,
     *                 2, 3 /
C
      data iallpoints2 / 0 /
      data istitch / 0 /
C
C
C ######################################################################
C
C
C
      isubname="addmesh_intersection"
C
      call cmo_get_info('nnodes',cmoa,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoa,numtet1,ilen,icmotp,ierror)
      call cmo_get_info('mbndry',cmoa,mbnry,ilen,icmotp,ierror)
      call cmo_get_info('ndimensions_topo',cmoa,
     *                  nsdtopo1,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmoa,
     *                  nsdgeom1,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoa,
     *                  nen1,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmoa,
     *                  nef1,length,icmotype,ierror)
      call cmo_get_info('imt1',cmoa,ipimt1,lenimt1,icmotp,ierror)
      call cmo_get_info('itp1',cmoa,ipitp1,lenitp1,icmotp,ierror)
      call cmo_get_info('xic' ,cmoa,ipxic1,lenxic1,icmotp,ierror)
      call cmo_get_info('yic' ,cmoa,ipyic1,lenyic1,icmotp,ierror)
      call cmo_get_info('zic' ,cmoa,ipzic1,lenzic1,icmotp,ierror)
      call cmo_get_info('itetclr',cmoa,
     *                  ipitetclr1,lenitetclr,icmotp,ier)
      call cmo_get_info('itettyp',cmoa,
     *                  ipitettyp1,lenitettyp,icmotp,ier)
      call cmo_get_info('itetoff',cmoa,
     *                  ipitetoff1,lenitetoff,icmotp,ier)
      call cmo_get_info('jtetoff',cmoa,
     *                  ipjtetoff1,lenjtetoff,icmotp,ier)
      call cmo_get_info('itet',cmoa,ipitet1,lenitet,icmotp,ierror)
      call cmo_get_info('jtet',cmoa,ipjtet1,lenjtet,icmotp,ierror)
C
C
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmob,numtet2,ilen,icmotp,ierror)
      call cmo_get_info('ndimensions_topo',cmob,
     *                  nsdtopo2,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmob,
     *                  nsdgeom2,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmob,
     *                  nen2,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmob,
     *                  nef2,length,icmotype,ierror)
C
      call cmo_get_info('imt1',cmob,ipimt2,lenimt2,icmotp,ierror)
      call cmo_get_info('itp1',cmob,ipitp2,lenitp2,icmotp,ierror)
      call cmo_get_info('xic' ,cmob,ipxic2,lenxic2,icmotp,ierror)
      call cmo_get_info('yic' ,cmob,ipyic2,lenyic2,icmotp,ierror)
      call cmo_get_info('zic' ,cmob,ipzic2,lenzic2,icmotp,ierror)
      call cmo_get_info('itetclr',cmob,
     *                  ipitetclr2,lenitetclr,icmotp,ier)
      call cmo_get_info('itettyp',cmob,
     *                  ipitettyp2,lenitettyp,icmotp,ier)
      call cmo_get_info('itetoff',cmob,
     *                  ipitetoff2,lenitetoff,icmotp,ier)
      call cmo_get_info('jtetoff',cmob,
     *                  ipjtetoff2,lenjtetoff,icmotp,ier)
      call cmo_get_info('itet',cmob,ipitet2,lenitet,icmotp,ierror)
      call cmo_get_info('jtet',cmob,ipjtet2,lenjtet,icmotp,ierror)
C
C
C     ******************************************************************
C     FIND THE MAXIMUM AND MINIMUM EXTENTS OF MESHES 1 AND 2.
C
      xmin1=1.0e+30
      ymin1=1.0e+30
      zmin1=1.0e+30
      xmax1=-xmin1
      ymax1=-ymin1
      zmax1=-zmin1
      do i=1,npoints1
         xmin1=min(xmin1,xic1(i))
         ymin1=min(ymin1,yic1(i))
         zmin1=min(zmin1,zic1(i))
         xmax1=max(xmax1,xic1(i))
         ymax1=max(ymax1,yic1(i))
         zmax1=max(zmax1,zic1(i))
      enddo
      xmin1=1.0*xmin1
      ymin1=1.0*ymin1
      zmin1=1.0*zmin1
      xmax1=1.0*xmax1
      ymax1=1.0*ymax1
      zmax1=1.0*zmax1
      xavg1=0.5*(xmin1+xmax1)
      yavg1=0.5*(ymin1+ymax1)
      zavg1=0.5*(zmin1+zmax1)
      rmin1=sqrt((xmin1-xavg1)**2+(ymin1-yavg1)**2+(zmin1-zavg1)**2)
      rmax1=sqrt((xmax1-xavg1)**2+(ymax1-yavg1)**2+(zmax1-zavg1)**2)
      radius1=2.0*max(rmin1,rmax1)
      write(logmess,9000)xmin1,ymin1,zmin1,xmax1,ymax1,zmax1
 9000 format ('min/max 1: ',6(1pe12.3))
      call writloga('default',0,logmess,0,ierrw)
      dx1=abs(xmax1-xmin1)
      if(dx1.le.1.0d-10) dx1=0.0d+00
      dy1=abs(ymax1-ymin1)
      if(dy1.le.1.0d-10) dy1=0.0d+00
      dz1=abs(zmax1-zmin1)
      if(dz1.le.1.0d-10) dz1=0.0d+00
C
      xmin2=1.0e+30
      ymin2=1.0e+30
      zmin2=1.0e+30
      xmax2=-xmin2
      ymax2=-ymin2
      zmax2=-zmin2
      do i=1,npoints2
         xmin2=min(xmin2,xic2(i))
         ymin2=min(ymin2,yic2(i))
         zmin2=min(zmin2,zic2(i))
         xmax2=max(xmax2,xic2(i))
         ymax2=max(ymax2,yic2(i))
         zmax2=max(zmax2,zic2(i))
      enddo
      xmin2=1.0*xmin2
      ymin2=1.0*ymin2
      zmin2=1.0*zmin2
      xmax2=1.0*xmax2
      ymax2=1.0*ymax2
      zmax2=1.0*zmax2
      xavg2=0.5*(xmin2+xmax2)
      yavg2=0.5*(ymin2+ymax2)
      zavg2=0.5*(zmin2+zmax2)
      rmin2=sqrt((xmin2-xavg2)**2+(ymin2-yavg2)**2+(zmin2-zavg2)**2)
      rmax2=sqrt((xmax2-xavg2)**2+(ymax2-yavg2)**2+(zmax2-zavg2)**2)
      radius2=2.0*max(rmin2,rmax2)
      write(logmess,9001)xmax2,ymax2,zmax2,xmin2,ymin2,zmin2
 9001 format ('Max/min 2: ',6(1pe12.3))
      call writloga('default',0,logmess,0,ierrw)
      dx2=abs(xmax2-xmin2)
      if(dx2.le.1.0d-10) dx2=0.0d+00
      dy2=abs(ymax2-ymin2)
      if(dy2.le.1.0d-10) dy2=0.0d+00
      dz2=abs(zmax2-zmin2)
      if(dz2.le.1.0d-10) dz2=0.0d+00
C
C
C     ******************************************************************
C     ALLOCATE TEMPORARY MEMORY FOR NODE AND ELEMENTS ARRAYS.
C
      length=max(npoints1,npoints2)
      call mmgetblk("ipipflg",isubname,ipipflag,length,2,icscode)
      call mmgetblk("ipipflga",isubname,ipipflaga,length,2,icscode)
      call mmgetblk("ipipflgb",isubname,ipipflagb,length,2,icscode)
      do i=1,length
         ipflag(i)=0
         ipflag1(i)=0
         ipflag2(i)=0
      enddo
      length=max(numtet1,numtet2)
      call mmgetblk("itflag",isubname,ipitflag,length,2,icscode)
      call mmgetblk("itflag1",isubname,ipitflag1,length,2,icscode)
      call mmgetblk("itflag2",isubname,ipitflag2,length,2,icscode)
      do i=1,length
         itflag(i)=0
         itflag1(i)=0
         itflag2(i)=0
      enddo
C
C
C     ******************************************************************
C     DETERMINE WHICH POINTS FROM MESH 1 LIE WITHIN THE BOUNDING BOX
C        FOR MESH 2.
C
      icount1=0
      do i=1,npoints1
         x1=xic1(i)
         y1=yic1(i)
         z1=zic1(i)
         radius=sqrt((x1-xavg2)**2+(y1-yavg2)**2+(z1-zavg2)**2)
         if(radius.lt.radius2) then
            icount1=icount1+1
            ipflag1(i)=icount1
         endif
      enddo
      itcount1=0
      do it=1,numtet1
         iflag=0
         do i=1,nelmnen(itettyp1(it))
            i1=itet1(itetoff1(it)+i)
            if(iflag.eq.0.and.ipflag1(i1).ne.0) then
               iflag=i
               itcount1=itcount1+1
               itflag1(itcount1)=it
            endif
         enddo
      enddo
      icount=0
      do i=1,npoints1
         if(ipflag1(i).ne.0) then
            icount=icount+1
            ipflag1(icount)=i
         endif
      enddo
      if(itcount1.le.0) then
         do it=1,numtet1
            itcount1=itcount1+1
            itflag1(itcount1)=it
         enddo
      endif
C
C
C     ******************************************************************
C     FIND THE NODES FROM MESH1 THAT LIE IN ONE OF THE ELEMENTS FROM
C        MESH2. THIS TEST IS PERFORMED BY CALCULATING THE VOLUME OF
C        FOUR SUB-ELEMENTS TO SEE IF THE MAGNITUDE OF THE SUM OF THE
C        VOLUMES IS EQUAL TO THE VOLUME OF THE ELEMENT. IF THE TWO
C        VOLUMES ARE EQUAL THEN THE NODE MUST BE INSIDE THE ELEMENT.
C
      do i=1,npoints1
         ipflag(i)=0
      enddo
      if(icount1.gt.0) then
         length=icount1
         call mmgetblk('itadd',isubname,ipitadd,length,1,icscode)
         call mmgetblk('xadd',isubname,ipxadd,length,2,icscode)
         call mmgetblk('yadd',isubname,ipyadd,length,2,icscode)
         call mmgetblk('zadd',isubname,ipzadd,length,2,icscode)
         do ip=1,icount1
            ip1=ipflag1(ip)
            xadd(ip)=xic1(ip1)
            yadd(ip)=yic1(ip1)
            zadd(ip)=zic1(ip1)
         enddo
         call table_element(cmob,
     &                      ipxadd,ipyadd,ipzadd,icount1,
     &                      ipitadd,
     &                      ierr2)
 
         do ip=1,icount1
            ip1=ipflag1(ip)
            if(itadd(ip).gt.0) then
               ipflag(ip1)=itadd(ip)
            endif
         enddo
         call mmrelblk('itadd',isubname,ipitadd,icscode)
         call mmrelblk('xadd',isubname,ipxadd,icscode)
         call mmrelblk('yadd',isubname,ipyadd,icscode)
         call mmrelblk('zadd',isubname,ipzadd,icscode)
      endif
C
C
C     .................................................................
C     MARK THE POINTS FROM MESH1 THAT LIE WITHIN MESH2 IN A PSET
C
      cbuff = ' '
      call cmo_select (cmoa,  ierror)
      call dotaskx3d('log/tty/off; finish',ierror)
      write (cbuff, *) 'cmo/addatt/',
     *                  cmoa(1:icharlnf(cmoa)),
     *                 '/overlap/VINT/scalar/nnodes',
     *                 '/linear/temporary/x/0 ; finish'
      call dotaskx3d (cbuff,ierror)
      do i1=1,npoints1
         if(ipflag(i1).ne.0) then
            cbuff = ' '
            write (cbuff, *) 'cmo/setatt/',
     *                        cmoa(1:icharlnf(cmoa)),
     *                       '/overlap/',
     *                       i1, ',', i1,
     *                       ',1/1 ; finish'
            call dotaskx3d (cbuff, ierror)
         endif
      enddo
      cbuff = ' '
      write (cbuff, *) 'pset/',
     *                  pset_name(1:icharlnf(pset_name))
     *                  ,'/attribute/overlap/1 0 0/1/eq ; finish'
      call dotaskx3d (cbuff, ierror)
      write (cbuff, *) 'cmo/delatt/',
     *                  cmoa(1:icharlnf(cmoa)),
     *                 '/overlap ; finish'
      call dotaskx3d (cbuff,ierror)
      call dotaskx3d('log/tty/on; finish',ierror)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
 
