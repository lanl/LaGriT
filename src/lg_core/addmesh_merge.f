*dk,addmesh_merge
      subroutine addmesh_merge(cmoc,cmob,ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine merges two mesh objects.
C
C     INPUT ARGUMENTS -
C
C        cmoc - The master mesh_object (source1).
C        cmob - The slave mesh_object (source2).
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The master mesh_object (sink).
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_merge.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   21 Mar 2002 11:19:12   dcg
CPVCS    add addmesh_match and addmesh_match1
CPVCS    
CPVCS       Rev 1.8   Wed Oct 08 12:46:04 1997   gable
CPVCS    Changes from x3d version that allow append vs. merge option
CPVCS    
CPVCS       Rev 1.7   Mon Sep 15 10:13:08 1997   het
CPVCS    Change the definition of _append and _merge
CPVCS    
CPVCS       Rev 1.6   05/30/95 07:52:44   het
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
      imat_offset=0
      call addmesh_append(cmoc,cmob,imat_offset,ierror)
C
 9998 continue
      goto 9999
 9999 continue
      return
      end
c
      subroutine addmesh_match(cmoc,cmob,
     *     ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,
     *     bx1,by1,bz1,bx2,by2,bz2,bx3,by3,bz3,
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
C        $Log: addmesh_merge.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
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
      character*32 isubname
      integer ierror
C
C ######################################################################
C
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      dimension xic2(1000000), yic2(1000000), zic2(1000000)
C
C ######################################################################
C
C
      isubname="addmesh_match"
C
C
      call cmo_get_info('nnodes',cmob,npoints2,ilen,icmotp,ierror)
C
      call cmo_get_info('xic' ,cmob,ipxic2,lenxic2,icmotp,ierror)
      call cmo_get_info('yic' ,cmob,ipyic2,lenyic2,icmotp,ierror)
      call cmo_get_info('zic' ,cmob,ipzic2,lenzic2,icmotp,ierror)
C
      call addmesh_match1(ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,
     *                    bx1,by1,bz1,bx2,by2,bz2,bx3,by3,bz3,
     *                    npoints2,xic2,yic2,zic2)
C
      call addmesh_merge(cmoc,cmob,ierror)
C
      goto 9999
 9999 continue
      return
      end
c
      subroutine addmesh_match1(ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,
     *                         bx1,by1,bz1,bx2,by2,bz2,bx3,by3,bz3,
     *                         npoints2,xic2,yic2,zic2)
      implicit real*8 (a-h,o-z)
      include 'consts.h'
      dimension xic2(npoints2), yic2(npoints2), zic2(npoints2)
      aax1=0.0
      aay1=0.0
      aaz1=0.0
      aax2=ax2-ax1
      aay2=ay2-ay1
      aaz2=az2-az1
      aax3=ax3-ax1
      aay3=ay3-ay1
      aaz3=az3-az1
      bbx1=0.0
      bby1=0.0
      bbz1=0.0
      bbx2=bx2-bx1
      bby2=by2-by1
      bbz2=bz2-bz1
C
      bbx3=bx3-bx1
      bby3=by3-by1
      bbz3=bz3-bz1
      do i1=1,npoints2
         xic2(i1)=xic2(i1)-bx1
         yic2(i1)=yic2(i1)-by1
         zic2(i1)=zic2(i1)-bz1
      enddo
C
C
C
      call angle3v(aax1,aay1,aaz1,
     *             aax2,aay2,aaz2,
     *             phi1,thi1)
C
      call angle3v(bbx1,bby1,bbz1,
     *             bbx2,bby2,bbz2,
     *             phi2,thi2)
C
      phirot=phi1-phi2
      thirot=thi1-thi2
C
      d1=sqrt(aax2**2+aay2**2+aaz2**2)
      d2=sqrt(bbx2**2+bby2**2+bbz2**2)
      rscale=d1/d2
      tscale=1.0
      pscale=1.0
C
C     LOOP OVER ALL POINTS TO ROTATE THEM IN SPHERICAL GEOMETRY.
C
      bbx2=bx2-bx1
      bby2=by2-by1
      bbz2=bz2-bz1
      call angle3v(bbx1,bby1,bbz1,
     *             bbx2,bby2,bbz2,
     *             phi,thi)
      ra=sqrt(bbx2**2+bby2**2+bbz2**2)
      rnew=rscale*ra
      thinew=tscale*(thi+thirot)
      phinew=pscale*(phi+phirot)
      bbx2=rnew*sin(thinew)*cos(phinew)
      bby2=rnew*sin(thinew)*sin(phinew)
      bbz2=rnew*cos(thinew)
C
      call angle3v(bbx1,bby1,bbz1,
     *             bbx3,bby3,bbz3,
     *             phi,thi)
      ra=sqrt(bbx3**2+bby3**2+bbz3**2)
      rnew=rscale*ra
      thinew=tscale*(thi+thirot)
      phinew=pscale*(phi+phirot)
      bbx3=rnew*sin(thinew)*cos(phinew)
      bby3=rnew*sin(thinew)*sin(phinew)
      bbz3=rnew*cos(thinew)
C
      do i1=1,npoints2
         call angle3v(bbx1,bby1,bbz1,
     *                xic2(i1),yic2(i1),zic2(i1),
     *                phi,thi)
         ra=sqrt(xic2(i1)**2+yic2(i1)**2+zic2(i1)**2)
         rnew=rscale*ra
         thinew=tscale*(thi+thirot)
         phinew=pscale*(phi+phirot)
         xic2(i1)=rnew*sin(thinew)*cos(phinew)
         yic2(i1)=rnew*sin(thinew)*sin(phinew)
         zic2(i1)=rnew*cos(thinew)
      enddo
C
C
C     LOOP OVER ALL POINTS TO ROTATE THEM IN CYLINDERICAL GEOMETRY.
C
      ax=  (aay2-aay1)*(aaz3-aaz1)-(aay3-aay1)*(aaz2-aaz1)
      ay=-((aax2-aax1)*(aaz3-aaz1)-(aax3-aax1)*(aaz2-aaz1))
      az=  (aax2-aax1)*(aay3-aay1)-(aax3-aax1)*(aay2-aay1)
      ad=ax*aax1+ay*aay1+az*aaz1
C
      bx=  (bby2-bby1)*(bbz3-bbz1)-(bby3-bby1)*(bbz2-bbz1)
      by=-((bbx2-bbx1)*(bbz3-bbz1)-(bbx3-bbx1)*(bbz2-bbz1))
      bz=  (bbx2-bbx1)*(bby3-bby1)-(bbx3-bbx1)*(bby2-bby1)
      bd=bx*bbx1+by*bby1+bz*bbz1
C
      d1=sqrt(aax3**2+aay3**2+aaz3**2)
      d2=sqrt(bbx3**2+bby3**2+bbz3**2)
      rscale=d1/d2
      tscale=1.0
C
      call angle3v(zero,zero,zero,ax,ay,az,pha,tha)
      call angle3v(zero,zero,zero,bx,by,bz,phb,thb)
      th1=tha-thb
C
      call rotatelo(bbx1,bby1,bbz1,bxnew,bynew,bznew,
     *              aax1,aay1,aaz1,aax2,aay2,aaz2,+th1)
C
      call rotatelo(bbx2,bby2,bbz2,bxnew,bynew,bznew,
     *              aax1,aay1,aaz1,aax2,aay2,aaz2,+th1)
C
      call rotatelo(bbx3,bby3,bbz3,bxnew,bynew,bznew,
     *              aax1,aay1,aaz1,aax2,aay2,aaz2,+th1)
      bbx3=bxnew
      bby3=bynew
      bbz3=bznew
      call eullag3(1,
     *             zero,zero,zero,bbx2,bby2,bbz2,
     *             bbx3,bby3,bbz3,
     *             ap1,bp1,cp1)
      ap1=rscale*ap1
      bp1=rscale*bp1
      call eullag3(2,
     *             zero,zero,zero,bbx2,bby2,bbz2,
     *             ap1,bp1,cp1,
     *             bbx3,bby3,bbz3)
      do i1=1,npoints2
         call rotatelo(xic2(i1),yic2(i1),zic2(i1),
     *                 bxnew,bynew,bznew,
     *                 aax1,aay1,aaz1,aax2,aay2,aaz2,
     *                 +th1)
         call eullag3(1,
     *                zero,zero,zero,bbx2,bby2,bbz2,
     *                bxnew,bynew,bznew,
     *                ap1,bp1,cp1)
         ap1=rscale*ap1
         bp1=rscale*bp1
         call eullag3(2,
     *                zero,zero,zero,bbx2,bby2,bbz2,
     *                ap1,bp1,cp1,
     *                xic2(i1),yic2(i1),zic2(i1))
      enddo
C
      do i1=1,npoints2
         xic2(i1)=xic2(i1)+ax1
         yic2(i1)=yic2(i1)+ay1
         zic2(i1)=zic2(i1)+az1
      enddo
C
C
      goto 9999
 9999 continue
      return
      end
