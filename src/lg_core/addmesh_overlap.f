*dk,addmesh_overlap
      subroutine addmesh_overlap(cmoc,cmob,
     *     ipipoint_overlap1,ipitet_overlap1,ipitet_border1,
     *     ipipoint_overlap2,ipitet_overlap2,ipitet_border2,
     *     ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine delete elements from a mesh_object.
C
C     INPUT ARGUMENTS -
C
C        cmoc          - The mesh_object (source1).
C        cmob          - The mesh_object (source1).
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The mesh_object (sink).
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_overlap.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.20   21 Mar 2002 11:18:54   dcg
CPVCS    add subroutine line_plane from file temp.f
CPVCS    
CPVCS       Rev 1.19   Wed Apr 05 13:34:00 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.17   Fri Jan 22 09:40:14 1999   dcg
CPVCS    fix typo ifelmpry to ifelmpyr
CPVCS    remove unused code
CPVCS
CPVCS       Rev 1.16   Mon Apr 14 16:38:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.15   Thu Nov 21 19:08:58 1996   het
CPVCS
CPVCS
CPVCS       Rev 1.14   Mon Nov 11 20:59:26 1996   het
CPVCS    Make changes for adding hybrid grids.
CPVCS
CPVCS       Rev 1.13   Wed Jan 03 10:08:14 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS
CPVCS       Rev 1.12   12/05/95 08:23:04   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.11   11/07/95 17:15:14   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.10   08/10/95 16:19:26   dcg
CPVCS    replace print * with writloga statements
CPVCS
CPVCS       Rev 1.9   06/05/95 10:39:38   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.8   05/30/95 07:52:48   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
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
      character*(*) cmoc, cmob
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
      pointer (ipipoint_overlap1, ipoint_overlap1)
      pointer (ipitet_overlap1, itet_overlap1)
      pointer (ipitet_border1, itet_border1)
      integer ipoint_overlap1(1000000)
      integer itet_overlap1(1000000), itet_border1(1000000)
      pointer (ipipoint_overlap2, ipoint_overlap2)
      pointer (ipitet_overlap2, itet_overlap2)
      pointer (ipitet_border2, itet_border2)
      integer ipoint_overlap2(1000000)
      integer itet_overlap2(1000000), itet_border2(1000000)
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
      pointer (ipitetdel2, itetdel2)
      integer itetdel2(1000000)
C
      pointer (ipitadd, itadd)
      integer itadd(1000000)
C
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(1000000), yadd(1000000), zadd(1000000)
C
      dimension itest0(12), itest1(12), itest2(12), itest3(12)
C
      logical mask_overlap
      character*32 isubname
      character*132 logmess
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
      isubname="addmesh_overlap"
C
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierror)
      call cmo_get_info('mbndry',cmoc,mbndry,ilen,icmotp,ierror)
      call cmo_get_info('ndimensions_topo',cmoc,
     *                  nsdtopo1,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmoc,
     *                  nsdgeom1,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoc,
     *                  nen1,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmoc,
     *                  nef1,length,icmotype,ierror)
      call cmo_get_info('imt1',cmoc,ipimt1,lenimt1,icmotp,ierror)
      call cmo_get_info('itp1',cmoc,ipitp1,lenitp1,icmotp,ierror)
      call cmo_get_info('xic' ,cmoc,ipxic1,lenxic1,icmotp,ierror)
      call cmo_get_info('yic' ,cmoc,ipyic1,lenyic1,icmotp,ierror)
      call cmo_get_info('zic' ,cmoc,ipzic1,lenzic1,icmotp,ierror)
      call cmo_get_info('itetclr',cmoc,
     *                  ipitetclr1,lenitetclr,icmotp,ier)
      call cmo_get_info('itettyp',cmoc,
     *                  ipitettyp1,lenitettyp,icmotp,ier)
      call cmo_get_info('itetoff',cmoc,
     *                  ipitetoff1,lenitetoff,icmotp,ier)
      call cmo_get_info('jtetoff',cmoc,
     *                  ipjtetoff1,lenjtetoff,icmotp,ier)
      call cmo_get_info('itet',cmoc,ipitet1,lenitet,icmotp,ierror)
      call cmo_get_info('jtet',cmoc,ipjtet1,lenjtet,icmotp,ierror)
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
C
C     ******************************************************************
C
      do i=1,npoints1
         ipoint_overlap1(i)=0
      enddo
      do it=1,numtet1
         itet_overlap1(it)=0
         itet_border1(it)=0
      enddo
      do i=1,npoints2
         ipoint_overlap2(i)=0
      enddo
      do it=1,numtet2
         itet_overlap2(it)=0
         itet_border2(it)=0
      enddo
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
      write(logmess,9000) xmax1,ymax1,zmax1,xmin1,ymin1,zmin1
 9000 format ('Max/min 1: ',6(1pe12.3))
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
C     DETERMINE WHICH POINTS FROM MESH 2 LIE WITHIN THE BOUNDING BOX
C        FOR MESH 1.
C
      icount2=0
      do i=1,npoints2
         x2=xic2(i)
         y2=yic2(i)
         z2=zic2(i)
         if(((dx1.eq.0.0d+00).or.(x2.ge.xmin1.and.x2.le.xmax1)) .and.
     *      ((dy1.eq.0.0d+00).or.(y2.ge.ymin1.and.y2.le.ymax1)) .and.
     *      ((dz1.eq.0.0d+00).or.(z2.ge.zmin1.and.z2.le.zmax1))) then
            icount2=icount2+1
            ipflag2(i)=icount2
         endif
      enddo
      itcount2=0
      do it=1,numtet2
         iflag=0
         do i=1,nelmnen(itettyp2(it))
            i1=itet2(itetoff2(it)+i)
            if(iflag.eq.0.and.ipflag2(i1).ne.0) then
               iflag=i
               itcount2=itcount2+1
               itflag2(itcount2)=it
            endif
         enddo
      enddo
      icount=0
      do i=1,npoints2
         if(ipflag2(i).ne.0) then
            icount=icount+1
            ipflag2(icount)=i
         endif
      enddo
      if(itcount2.le.0) then
         do it=1,numtet2
            itcount2=itcount2+1
            itflag2(itcount2)=it
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
C     MARK THE POINTS FROM MESH1 THAT LIE WITHIN MESH2 AS DUDDED POINTS
C
      do i1=1,npoints1
         if(ipflag(i1).ne.0) then
C*****      itp1(i1)=21
         endif
      enddo
C
C
C     .................................................................
C
C
      do i=1,numtet1
         itflag(i)=0
      enddo
      itcount=0
      do it=1,numtet1
         do i=1,nelmnen(itettyp1(it))
            i1=itet1(itetoff1(it)+i)
            if(ipflag(i1).ne.0) itflag(it)=1
         enddo
      enddo
C
C
C     .................................................................
C     FROM THE SET OF POINTS FROM MESH 2 THAT LIE WITHIN THE BOUNDING
C        BOX FOR MESH 1 DETERMINE EXACTLY WHICH POINTS FROM MESH 2
C        LIE WITHIN MESH 1. THIS IS DONE BY CHECKING TO SEE IF THE
C        POINTS FROM MESH 2 LIE IN ANY OF THE TETRAHEDRONS FROM MESH 1.
C
      do i=1,npoints2
         ipflag(i)=0
      enddo
      if(icount2.gt.0) then
         length=icount2
         call mmgetblk('itadd',isubname,ipitadd,length,1,icscode)
         call mmgetblk('xadd',isubname,ipxadd,length,2,icscode)
         call mmgetblk('yadd',isubname,ipyadd,length,2,icscode)
         call mmgetblk('zadd',isubname,ipzadd,length,2,icscode)
         do ip=1,icount2
            ip1=ipflag2(ip)
            xadd(ip)=xic2(ip1)
            yadd(ip)=yic2(ip1)
            zadd(ip)=zic2(ip1)
         enddo
         call table_element(cmoc,
     &                      ipxadd,ipyadd,ipzadd,icount2,
     &                      ipitadd,
     &                      ierr2)
 
         do ip=1,icount2
            ip1=ipflag2(ip)
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
C     MARK THE POINTS FROM MESH2 THAT LIE OUTSIDE MESH1 AS DUDDED
C        POINTS.
C
      do i1=1,npoints2
         if(ipflag(i1).eq.0) then
C*****      itp2(i1)=21
         endif
      enddo
C
C
C     ******************************************************************
C     FLAG THE ELEMENTS FROM MESH2 THAT OVERLAP ELEMENTS FROM MESH1 AND
C        CONSTRUCT A LIST OF THE NODES FROM MESH2 THAT OVERLAP ELEMENTS
C        FROM MESH1.
C
      do i=1,numtet2
         itflag2(i)=0
      enddo
      itcount=0
      do it=1,numtet2
         do i=1,nelmnen(itettyp2(it))
            i1=itet2(itetoff2(it)+i)
            if(ipflag(i1).ne.0) itflag2(it)=1
         enddo
      enddo
      icount2=0
      do i=1,npoints2
         if(ipflag(i).ne.0) then
            itp2(i)=2
            icount2=icount2+1
            ipflag2(icount2)=i
         endif
      enddo
      itcount2=0
      do it=1,numtet2
         if(itflag2(it).ne.0) then
            itcount2=itcount2+1
            itflag2(itcount2)=it
         endif
      enddo
C
C
C     ******************************************************************
C     FOR EACH ELEMENT FROM MESH2 THAT OVERLAPS MESH1 CHECK ALL OF THE
C         CONNECTIONS FOR EACH ELEMENT TO DETERMINE IF THE INTERSECTION
C         POINT INTERSECTS AN ELEMENT FROM MESH1. THIS CATCHES THE CASE
C         WHERE THE END POINTS FROM AN ELEMENT DON'T LIE WITH AN ELEMENT
C         BUT THE LINE CONNECTING TWO NODES WILL INTERSECT A MESH1
C         ELEMENT.
C
      intersect=0
      jcount=0
      do it2=1,numtet2
         ipflagmax=-10*npoints2
         do i=1,nelmnen(itettyp2(it2))
            i1=itet2(itetoff2(it2)+i)
            ipflagmax=max(ipflagmax,ipflag(i1))
         enddo
         if(ipflagmax.le.0) then
            goto 130
         endif
         do k=1,nelmnee(itettyp2(it2))
            ip1=itet2(itetoff2(it2)+ielmedge1(1,k,itettyp2(it2)))
            ip2=itet2(itetoff2(it2)+ielmedge1(2,k,itettyp2(it2)))
            itstart=ipflag(ip1)
            itstop=ipflag(ip2)
            if(itstart.lt.itstop) then
               it=itstop
               itstop=itstart
               itstart=it
               ip=ip2
               ip2=ip1
               ip1=ip
            endif
            xasave=xic2(ip1)
            yasave=yic2(ip1)
            zasave=zic2(ip1)
            xa=0.0
            ya=0.0
            za=0.0
            xb=xic2(ip2)-xasave
            yb=yic2(ip2)-yasave
            zb=zic2(ip2)-zasave
            if(itstart.le.0.and.itstop.le.0) then
               goto 120
            elseif(itstart.eq.itstop) then
               itflag(itstart)=1
               goto 120
            endif
            it=max(itstart,itstop)
            itstop=itstart+itstop-it
 110        continue
               itflag(it)=1
               if(itettyp1(it).eq.ifelmtri) then
                  iface=1
                  itest0(iface)=1
                  itest1(iface)=itet1(itetoff1(it)+1)
                  itest2(iface)=itet1(itetoff1(it)+2)
                  itest3(iface)=itet1(itetoff1(it)+3)
               elseif(itettyp1(it).eq.ifelmqud) then
                  iface=1
                  itest0(iface)=1
                  itest1(iface)=itet1(itetoff1(it)+1)
                  itest2(iface)=itet1(itetoff1(it)+2)
                  itest3(iface)=itet1(itetoff1(it)+3)
                  iface=2
                  itest0(iface)=1
                  itest1(iface)=itet1(itetoff1(it)+1)
                  itest2(iface)=itet1(itetoff1(it)+3)
                  itest3(iface)=itet1(itetoff1(it)+4)
               elseif(itettyp1(it).eq.ifelmtet .or.
     *                itettyp1(it).eq.ifelmpri .or.
     *                itettyp1(it).eq.ifelmpyr .or.
     *                itettyp1(it).eq.ifelmhex) then
                  iface=0
                  do i=1,nelmnef(itettyp1(it))
                     do j=2,ielmface0(i,itettyp1(it))-1
                        jp1=j+1
                        iface=iface+1
                        itest0(iface)=i
                        itest1(iface)=itet1(itetoff1(it)+
     *                                    ielmface1(1,i,itettyp1(it)))
                        itest2(iface)=itet1(itetoff1(it)+
     *                                    ielmface1(j,i,itettyp1(it)))
                        itest3(iface)=itet1(itetoff1(it)+
     *                                    ielmface1(jp1,i,itettyp1(it)))
                     enddo
                  enddo
               elseif(itettyp1(it).eq.ifelmhex) then
                  iface=0
                  do i=1,nelmnef(itettyp1(it))
                     do j=2,ielmface0(i,itettyp1(it))-1
                        jp1=j+1
                        iface=iface+1
                        itest0(iface)=i
                        itest1(iface)=itet1(itetoff1(it)+
     *                                    ielmface1(1,i,itettyp1(it)))
                        itest3(iface)=itet1(itetoff1(it)+
     *                                    ielmface1(j,i,itettyp1(it)))
                        itest2(iface)=itet1(itetoff1(it)+
     *                                    ielmface1(jp1,i,itettyp1(it)))
                     enddo
                  enddo
               else
                  write(logmess,9010) it,itettyp1(it)
                  call writloga('default',0,logmess,0,ierrw)
 9010             format ('Improper element type for addmesh: ',2i10)
               endif
               do j=1,iface
                  jface=itest0(j)
                  i1=itest1(j)
                  i2=itest2(j)
                  i3=itest3(j)
                  x1=xic1(i1)-xasave
                  y1=yic1(i1)-yasave
                  z1=zic1(i1)-zasave
                  x2=xic1(i2)-xasave
                  y2=yic1(i2)-yasave
                  z2=zic1(i2)-zasave
                  x3=xic1(i3)-xasave
                  y3=yic1(i3)-yasave
                  z3=zic1(i3)-zasave
                  xmesh1=0.0
                  ymesh1=0.0
                  zmesh1=0.0
                  iflag=0
                  call line_plane(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                         xa,ya,za,xb,yb,zb,
     *                         xmesh1,ymesh1,zmesh1,iflag)
                  xmesh1=xmesh1+xasave
                  ymesh1=ymesh1+yasave
                  zmesh1=zmesh1+zasave
                  mask_overlap=.false.
                  if(iflag.eq.0) mask_overlap = .true.
                  ax4=  (y3-y1)*(z2-z1)-(z3-z1)*(y2-y1)
                  ay4=-((x3-x1)*(z2-z1)-(z3-z1)*(x2-x1))
                  az4=  (x3-x1)*(y2-y1)-(y3-y1)*(x2-x1)
                  xdot=-(ax4*(xb-xa)+ay4*(yb-ya)+az4*(zb-za))
                  if(xdot.gt.0.0.and.mask_overlap.eqv..true.) then
                     intersect=intersect+1
                     if(jtet1(jtetoff1(it)+jface).le.0 .or.
     *                  jtet1(jtetoff1(it)+jface).eq.mbndry) then
                        goto 120
                     else
                        if(jtet1(jtetoff1(it)+jface).gt.mbndry) then
                           jt=1+
     *                        (jtet1(jtetoff1(it)+jface)-mbndry-1)/nef1
                           jf=jtet1(jtetoff1(it)+jface)-
     *                        mbndry-nef1*(jt-1)
                        else
                           jt=1+(jtet1(jtetoff1(it)+jface)-1)/nef1
                           jf=jtet1(jtetoff1(it)+jface)-nef1*(jt-1)
                        endif
                        it=jt
                        if(jt.eq.itstop) then
                           itflag(jt)=1
                           goto 120
                        else
                           goto 110
                        endif
                     endif
                  endif
               enddo
 120        continue
         enddo
 130     continue
      enddo
C
C
C     ******************************************************************
C     NOW, FLAG THE TETS FROM MESH1 ACCORDING TO WHETHER THEY LIE
C        INSIDE (<0) for OUTSIDE (>0) MESH2. ALSO, FLAG THE ELEMENTS
C        THAT LIE JUST ON THE BORDER OF THE INTERSECTION REGION.
C
      do it=1,numtet1
         if(itflag(it).gt.0) then
            itet_overlap1(it)=-itflag(it)
         else
            itet_overlap1(it)=itet_overlap1(it)+1
         endif
         if(itflag(it).gt.0) then
            iflag1=0
            iflag2=0
            do i=1,4
               if(jtet1(jtetoff1(it)+i).gt.0 .and.
     *            jtet1(jtetoff1(it)+i).lt.mbndry) then
                  jt=1+(jtet1(jtetoff1(it)+i)-1)/nef1
                  jf=jtet1(jtetoff1(it)+i)-nef1*(jt-1)
                  iflag1=iflag1+1
                  if(itflag(jt).gt.0) iflag2=iflag2+1
               endif
            enddo
            if(iflag2.lt.iflag1) then
               itet_border1(it)=-1
            endif
         else
            iflag1=0
            iflag2=0
            do i=1,4
               if(jtet1(jtetoff1(it)+i).gt.0 .and.
     *            jtet1(jtetoff1(it)+i).lt.mbndry) then
                  jt=1+(jtet1(jtetoff1(it)+i)-1)/nef1
                  jf=jtet1(jtetoff1(it)+i)-nef1*(jt-1)
                  iflag1=iflag1+1
                  if(itflag(jt).le.0) iflag2=iflag2+1
               endif
            enddo
            if(iflag2.lt.iflag1) then
               itet_border1(it)=+1
            endif
         endif
      enddo
      goto 9997
C
C
C
 
 9997 continue
C
      if(iallpoints2.eq.1) goto 9998
C
      length=numtet2
      call mmgetblk("itetdel2",isubname,ipitetdel2,length,2,icscode)
      do it=1,numtet2
         itetdel2(it)=0
      enddo
      ntetdel2=0
      do it2=1,numtet2
         do k=1,nelmnee(itettyp2(it2))
            ip1=itet2(itetoff2(it2)+ielmedge1(1,k,itettyp2(it2)))
            ip2=itet2(itetoff2(it2)+ielmedge1(2,k,itettyp2(it2)))
            itstart=ipflag(ip1)
            itstop=ipflag(ip2)
            if(ipflag(ip1).eq.0.or.ipflag(ip2).eq.0) then
               ntetdel2=ntetdel2+1
               itetdel2(it2)=it2
               itet_overlap2(it2)=-it2
            endif
         enddo
      enddo
C
 9998 continue
C
C
C     .................................................................
C
      if(istitch.eq.1) then
         do it2=1,numtet2
            do i=1,nelmnef(itettyp2(it2))
               if(jtet2(jtetoff2(it2)+i).lt.0) then
                  jtet2(jtetoff2(it2)+i)=-1
                  do j=1,ielmface0(i,itettyp2(it2))
                     j1=itet2(itetoff2(it2)+
     *                        ielmface1(j,i,itettyp2(it2)))
                     itp2(j1)=ifitpini
                  enddo
               endif
            enddo
         enddo
      endif
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
c
      subroutine line_plane(x1,y1,z1,x2,y2,z2,x3,y3,z3,
     *                      xa,ya,za,xb,yb,zb,
     *                      x,y,z,iflag)
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
      cx=  (y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      cy=-((x2-x1)*(z3-z1)-(z2-z1)*(x3-x1))
      cz=  (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
      a11=cx
      a12=cy
      a13=cz
      b1=cx*x1+cy*y1+cz*z1
      a21=0.0
      a22=0.0
      a23=0.0
      b2=0.0
      a31=0.0
      a32=0.0
      a33=0.0
      b3=0.0
      dsab=sqrt((xa-xb)**2+(ya-yb)**2+(za-zb)**2)
      if(abs(xa-xb).gt.1.0d-06*dsab) then
         a21=-(ya-yb)
         a22=  xa-xb
         a23=0.0
         b2=-xb*(ya-yb)+yb*(xa-xb)
         a31=-(za-zb)
         a32=0.0
         a33=  xa-xb
         b3=-xb*(za-zb)+zb*(xa-xb)
      elseif(abs(ya-yb).gt.1.0d-06*dsab) then
         a21=-(ya-yb)
         a22=  xa-xb
         a23=0.0
         b2=-xb*(ya-yb)+yb*(xa-xb)
         a31=0.0
         a32=-(za-zb)
         a33=  ya-yb
         b3=-yb*(za-zb)+zb*(ya-yb)
      elseif(abs(za-zb).gt.1.0d-06*dsab) then
         a21=-(za-zb)
         a22=0.0
         a23=  xa-xb
         b2=-xb*(za-zb)+zb*(xa-xb)
         a31=0.0
         a32=-(za-zb)
         a33=  ya-yb
         b3=-yb*(za-zb)+zb*(ya-yb)
      endif
      iflagsav=iflag
      call inv3x3(a11,a12,a13,a21,a22,a23,a31,a32,a33,b1,b2,b3,
     *            x,y,z,iflag)
      if(iflag.eq.iflagsav) then
         dsa=sqrt((x-xa)**2+(y-ya)**2+(z-za)**2)
         dsb=sqrt((x-xb)**2+(y-yb)**2+(z-zb)**2)
         dsum=dsa+dsb
         ax123=  (y2-y1)*(z3-z1)-(y3-y1)*(z2-z1)
         ay123=-((x2-x1)*(z3-z1)-(x3-x1)*(z2-z1))
         az123=  (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)
         area123=0.5*sqrt(ax123**2+ay123**2+az123**2)
         ax12=  (y2-y1)*(z-z1)-(y-y1)*(z2-z1)
         ay12=-((x2-x1)*(z-z1)-(x-x1)*(z2-z1))
         az12=  (x2-x1)*(y-y1)-(x-x1)*(y2-y1)
         area12=0.5*sqrt(ax12**2+ay12**2+az12**2)
         ax23=  (y3-y2)*(z-z2)-(y-y2)*(z3-z2)
         ay23=-((x3-x2)*(z-z2)-(x-x2)*(z3-z2))
         az23=  (x3-x2)*(y-y2)-(y3-y2)*(x-x2)
         area23=0.5*sqrt(ax23**2+ay23**2+az23**2)
         ax31=  (y1-y3)*(z-z3)-(y-y3)*(z1-z3)
         ay31=-((x1-x3)*(z-z3)-(x-x3)*(z1-z3))
         az31=  (x1-x3)*(y-y3)-(x-x3)*(y1-y3)
         area31=0.5*sqrt(ax31**2+ay31**2+az31**2)
         xsum=area12+area23+area31
         if(abs(dsab-dsum).le.1.0d-06*dsab .and.
     *      abs(area123-xsum).le.1.0d-06*area123) then
         else
            iflag=iflag-1
         endif
      endif
      goto 9999
 9999 continue
      return
      end
