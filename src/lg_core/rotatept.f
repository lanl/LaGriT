      subroutine rotatept(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS SUBROUTINE IS USED TO ROTATE PART OF THE MESH
C        ABOUT A CENTER POINT THROUGH THE ANGLES THETA AND PHI.
c        only node coordinates are rotated - all other mesh
c        attributes are unchanged
C
C        FORMAT: ROTATE/ipstart/ipend/ipstep/  copy/
C                       xcen/ycen/zcen/theta/phi
C        FORMAT: ROTATE/ipstart/ipend/ipstep/nocopy/
C                       xcen/ycen/zcen/theta/phi
C
C              NOTE: THETA - IS ABOUT THE Z-AXIS
C                    PHI   - IS ABOUT THE X-AXIS IN THE XY-PLANE
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
C      CHANGE HISTORY -
C
C        $Log: rotatept.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   04 Jun 2002 09:32:02   dcg
CPVCS    fix typo
CPVCS    
CPVCS       Rev 1.2   20 Mar 2002 15:13:02   dcg
CPVCS    rewrite of subroutine with simpler algorithm
CPVCS    
CPVCS       Rev 1.1   08 Sep 2000 09:26:32   dcg
CPVCS    check correct input argument for numeric or character type pset designation
CPVCS    
CPVCS       Rev 1.0   26 Jan 2000 15:10:46   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 17:00:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Fri Oct 25 16:57:18 1996   dcg
CPVCS    use multiplier icopy in all places needed
CPVCS
CPVCS       Rev 1.9   Thu Feb 08 14:09:10 1996   dcg
CPVCS    add loop on mesh object attributes
CPVCS
CPVCS       Rev 1.8   Tue Feb 06 12:41:22 1996   dcg
CPVCS    add attribute loop
CPVCS
CPVCS       Rev 1.7   11/16/95 15:22:36   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.6   11/07/95 17:25:40   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.5   09/19/95 13:10:12   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.4   07/13/95 09:03:46   ejl
CPVCS    Cleaned up interface of rotatept, rotateln, copypts.
CPVCS
CPVCS       Rev 1.3   05/26/95 13:13:36   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.2   02/18/95 06:57:14   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:16   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:26   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      pointer (ipisetwd,isetwd)
      pointer (ipitp1,itp1)
      integer isetwd(1000000),itp1(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C#######################################################################
C
      character*32 iword
      character*32 isubname, cmo
      character*32 ich1,ich2,ich3
      integer length,ierrw,
     * i1,icount,ipts,icopy,mpno,ipt1,ipt2,ipt3,ipointi,
     * ipointj,ityp,icscode,npoints,icmotype,ilen,ii
      real*8 xcen,ycen,zcen,theta,phi,xnew,ynew,znew,xold,yold,zold
C
      pointer(ipmpary, mpary )
      integer mpary(1000000)
C
      character*132 logmess
C
C#######################################################################
C
 
      isubname='rotatept'
      call cmo_get_name(cmo,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('ipointi',cmo,ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      ipointi = max(1,ipointi)
      if(ipointj.eq.0) ipointj = max(1,npoints)
C
C     ..................................................................
C     CHECK TO POINT LIMITS AND TRANSLATE THEM TO VALID LIMITS IF
C     NECESSARY.
C
      if(msgtype(2).eq.1.and.imsgin(2).eq.0) then
         imsgin(2)=ipointi
      endif
      if(msgtype(3).eq.1.and.imsgin(3).eq.0) then
         imsgin(3)=ipointj
      endif
      if(msgtype(4).eq.1.and.imsgin(4).eq.0) then
         imsgin(4)=1
      endif
C
      call mmgetblk('mpary',isubname,ipmpary,npoints,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
C
C     Set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      if(msgtype(2).eq.1) then
         ipt1=imsgin(2)
         ipt2=imsgin(3)
         ipt3=imsgin(4)
         if(nwds.eq.2) then
            ipt2=ipt1
            ipt3=1
         elseif(nwds.eq.3) then
            ipt3=1
         endif
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
      else
         ich1=cmsgin(2)
         ich2=cmsgin(3)
         ich3=cmsgin(4)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
      endif
C
      if(msgtype(5).eq.3) then
         iword=cmsgin(5)
      else
         iword=' '
      endif
C
      if(iword(1:4).eq.'copy') then
         icopy=1
      else
         icopy=0
      endif
C
      if (icopy .eq. 1) then
C
C        ...............................................................
C        Before we rotate, first adjust memory if necessary
C
         npoints=npoints+mpno
C
         call cmo_set_info('nnodes',cmo,npoints,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
         call cmo_newlen(cmo,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_newlen')
C
      endif
C
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call test_argument_type(5,2,6,imsgin,xmsgin,cmsgin,msgtype,nwds)
      xcen=xmsgin(6)
      ycen=xmsgin(7)
      zcen=xmsgin(8)
C
      theta=xmsgin(9)
      phi=xmsgin(10)
C
      ipts=0
      icount=0
      do ii=1,mpno
         i1=mpary(ii)
         icount=icount+1
         xold=xic(i1)
         yold=yic(i1)
         zold=zic(i1)
         call rotate_a_point_lg(xold,yold,zold,xcen,ycen,zcen
     *          ,phi,theta,xnew,ynew,znew,ierror)
         if(icopy.eq.0) then
            ipts=i1
         elseif(icopy.eq.1) then
            ipts=ipts+1
         endif
         xic(ipts+icopy*ipointj)=xnew
         yic(ipts+icopy*ipointj)=ynew
         zic(ipts+icopy*ipointj)=znew
      enddo
C
      if(icopy.eq.1) then
C
C        ---------------------------------------------------------------
C        SAVE ipointi AND ipointj.
C
         ipointi=ipointj+1
         ipointj=ipointj+icount
C
         call cmo_set_info('ipointi',cmo,ipointi,length,icmotype,
     &                      icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
         call cmo_set_info('ipointj',cmo,ipointj,length,icmotype,
     &                      icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
C
C        ---------------------------------------------------------------
C        PRINT OUT THE POINT NUMBERS GENERATED
C
         write(logmess,6000) ipointi, ipointj
 6000    format('  ROTATEPT GENERATED POINTS ',i6,' TO ',i6)
         call writloga('default',0,logmess,0,ierrw)
C
      endif
C
      call mmrelblk('mpary',isubname,ipmpary,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelblk')
C
      ierror=0
C
      return
      end
c
c
c
      subroutine rotate_a_point_lg(x1,y1,z1,xcen,ycen,zcen,phi,theta,
     *    xnew,ynew,znew,ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C     rotate a point at x1,y1,y1 with respect to a center at
C     xcen,ycen,zcen according to the angles theta(down from
C     the z axis) and phi (in the xy plane -from x axis towards y)
C
C
C
C      INPUT ARGUMENTS -
C       x1,y1,y1   coordinates of node to be rotated
C       xcen,ycen,zcen  coordinates of center of rotation
C       phi,theta  angles of rotation
C
C     OUTPUT ARGUMENTS -
C        xnew,ynew,znew  coordinates of node after rotation
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C#######################################################################
C
      implicit none
      include 'chydro.h'
      include 'consts.h'
      real*8 x1,y1,z1,xcen,ycen,zcen,phi,theta,xnew,ynew,znew
      integer ierror
      real*8 thetar,phir,radius,radiusq,costheta0,theta0,
     *  phi0,thetatot,phitot
c
C  convert angles to radians
c
      thetar=theta*pie/180.d0
      phir=phi*pie/180.d0
c
c  get coordinates of node to be rotated in rtp space with
c  respect to center of rotation
c
      radiusq = (x1-xcen)**2+(y1-ycen)**2+(z1-zcen)**2
      if(radiusq.le.epsilonr) then
         xnew=x1
         ynew=y1
         znew=z1
         go to 9999
      endif
      radius = sqrt(radiusq)
      costheta0 = (z1-zcen)/radius
      if(abs(costheta0).lt.0.999d0) then
        theta0=acos(costheta0)
      else
        if((z1-zcen).gt.zero) then
          theta0=asin(sqrt((x1-xcen)**2+(y1-ycen)**2)/radius)
        else
          theta0=pie-asin(sqrt((x1-xcen)**2+(y1-ycen)**2)/radius)
        endif
      endif
      phi0=atan2(y1-ycen,x1-xcen)
      thetatot=theta0+thetar
      phitot=phi0+phir
c
c  reconvert cooridnates back to xyz space
c
      znew=radius*cos(thetatot)+zcen
      ynew=radius*sin(thetatot)*sin(phitot)+ycen
      xnew=radius*sin(thetatot)*cos(phitot)+xcen
 
 9999 continue
      return
      end
