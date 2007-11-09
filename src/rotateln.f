      subroutine rotateln(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS SUBROUTINE IS USED TO ROTATE PART OF THE MESH
C        ABOUT A LINE THROUGH AN ANGLE THETA.
C
C        FORMAT: ROTATE/ipstart/ipend/ipstep/  copy/                   &
C                    xstr/ystr/zstr/xend/yend/zend/theta/xcen/ycen/zcen
C        FORMAT: ROTATE/ipstart/ipend/ipstep/nocopy/                   &
C                    xstr/ystr/zstr/xend/yend/zend/theta/xcen/ycen/zcen
C
C           NOTE: THE LINE SEGMENT MUST HAVE LIMITS SUCH THAT IT LIES
C                 BEYOND THE OBJECT BEING ROTATED (I.E. MAKE THE LENGTH
C                 LINE SEGMENT PLENTY LONG).
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
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/rotateln_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.5   29 Sep 2004 16:59:02   dcg
CPVCS    add chydro include file to supply value for pie
CPVCS    
CPVCS       Rev 1.4   29 Sep 2004 16:56:50   dcg
CPVCS     remove line that set value for pie - this is set in initlagrit 
CPVCS    
CPVCS       Rev 1.3   14 Jan 2002 16:23:46   dcg
CPVCS    remove code that attempted to fill all attributes for
CPVCS    new points created with 'copy' option
CPVCS    assumption is now that only coordinates are rotated.
CPVCS    
CPVCS       Rev 1.2   03 Feb 2000 12:36:40   dcg
CPVCS    
CPVCS       Rev 1.1   26 Jan 2000 15:10:10   dcg
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 17:00:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Thu Feb 08 14:09:02 1996   dcg
CPVCS    add loop on mesh object attributes
CPVCS
CPVCS       Rev 1.9   Tue Feb 06 12:41:08 1996   dcg
CPVCS    add attribute loop
CPVCS
CPVCS       Rev 1.8   11/16/95 15:22:34   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.7   11/07/95 17:25:36   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   09/19/95 13:10:16   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.5   07/13/95 09:03:42   ejl
CPVCS    Cleaned up interface of rotatept, rotateln, copypts.
CPVCS
CPVCS       Rev 1.4   05/26/95 13:13:32   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.3   05/11/95 13:15:38   het
CPVCS    Correct errors related to SGI
CPVCS
CPVCS       Rev 1.2   02/18/95 06:57:10   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:08   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:22   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
      include 'chydro.h'
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
      pointer(ipisetwd,isetwd)
      pointer(ipitp1,itp1)
      integer itp1(1000000),isetwd(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C
C#######################################################################
C
      character*32 iword
      integer length,ierrw,
     *lout,itout,i1,icount,ipts,icopy,mpno,ipt1,ipt2,ipt3,ipointi,
     * ipointj,ityp,icscode,npoints,icmotype,ilen,ii
      real*8 xcen,ycen,zcen,theta,xnew,ynew,znew,xold,yold,zold,
     * xstr,ystr,zstr,xend,yend,zend,th1
      character*32 isubname, cmo
      character*32 ich1,ich2,ich3
C
      pointer(ipmpary, mpary )
      integer mpary(1000000)
C
      character*132 logmess
C
C#######################################################################
C
      isubname='rotateln'
C
C
      call cmo_get_name(cmo,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('ipointi',cmo,ipointi,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('ipointj',cmo,ipointj,length,icmotype,icscode)
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
      call test_argument_type(10,2,6,imsgin,xmsgin,cmsgin,msgtype,nwds)
      xstr=xmsgin(6)
      ystr=xmsgin(7)
      zstr=xmsgin(8)
C
      xend=xmsgin(9)
      yend=xmsgin(10)
      zend=xmsgin(11)
C
      theta=xmsgin(12)
C
      xcen=xmsgin(13)
      ycen=xmsgin(14)
      zcen=xmsgin(15)
C
      th1=theta*pie/180.0d0
C
      icount=0
      ipts=0
      do ii=1,mpno
         i1=mpary(ii)
         icount=icount+1
         xold=xic(i1)-xcen
         yold=yic(i1)-ycen
         zold=zic(i1)-zcen
         call rotatelo(xold,yold,zold,xnew,ynew,znew,
     *                 xstr,ystr,zstr,xend,yend,zend,th1)
         if(icopy.eq.0) then
            ipts=i1
         elseif(icopy.eq.1) then
            ipts=ipts+1
         endif
         xic(ipts+icopy*ipointj)=xnew+xcen
         yic(ipts+icopy*ipointj)=ynew+ycen
         zic(ipts+icopy*ipointj)=znew+zcen
      enddo
 
      if(icopy.eq.1) then
C
C        ---------------------------------------------------------------
C        SAVE ipointi AND ipointj.
C
         ipointi=ipointj+1
         ipointj=ipointj+icount
C
         call cmo_set_info('ipointi',cmo,ipointi,
     &                   lout,itout,   icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
         call cmo_set_info('ipointj',cmo,ipointj,
     &                     lout,itout,   icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
C
C        ---------------------------------------------------------------
C        PRINT OUT THE POINT NUMBERS GENERATED
C
         write(logmess,6000) ipointi, ipointj
 6000    format('  ROTATELN GENERATED POINTS ',i6,' TO ',i6)
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
