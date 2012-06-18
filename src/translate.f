
      subroutine translate(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS SUBROUTINE IS USED TO TRANSLATE PART OF THE
C        MESH IN XYZ-SPACE.
C
C       FORMAT: TRANS/ipstart/ipend/ipstep/xold/yold/zold/xnew/ynew/znew
C                or
C               TRANS/ipstart,ipend,ipstep/
C                     center or zero or original/
C                     xyz or rtz or rtp /
C                     x or 0 or 1,
C                     y or 0 or 1,
C                     z or 0 or 1
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
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: translate.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.14   02 Nov 2005 13:43:50   gable
CPVCS    Modified so that if mesh object does not have any nodes then
CPVCS    TRANS returns with no action instead of crashing with mmgetblk error.
CPVCS    
CPVCS       Rev 1.13   Thu Apr 06 14:26:48 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.12   Wed Apr 05 13:35:32 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.11   22 Mar 2000 13:41:14   gable
CPVCS    Added call to setsize.
CPVCS
CPVCS       Rev 1.10   Fri Jan 22 16:52:10 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.9   Tue Aug 19 14:34:48 1997   dcg
CPVCS    add argument type testing
CPVCS
CPVCS       Rev 1.8   Fri Apr 25 15:37:56 1997   tam
CPVCS    added option to translate to origin and back
CPVCS    with att transauto to track xyz translation
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 17:04:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   11/16/95 15:23:02   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.5   11/07/95 17:28:56   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.4   06/13/95 09:04:16   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.3   03/10/95 17:13:48   dcg
CPVCS     put in mesh object calls
CPVCS
CPVCS       Rev 1.2   02/18/95 06:57:34   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.1   01/04/95 22:06:30   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:26   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none

C args translate(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)

      integer nwds,ierr
      integer  imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

C#######################################################################
C
      integer nplen
      parameter (nplen=1000000)

      integer ix,iy,iz,npoints,length,icmotype,ilen,ityp,ipointi,
     * ipointj,ipt1,ipt2,ipt3,ii,i1
      integer iopt_auto, iopt_zero, iopt_center, iopt_original
      integer ierrw, icscode, i, inxt
      integer icharlnf
C
      pointer (ipisetwd, isetwd)
      pointer (ipitp1, itp1)
      integer isetwd(nplen), itp1(nplen)
C
      integer mpno
      pointer(ipmpary, mpary)
      integer mpary(1000000)

C
      real*8 xold,yold,zold,xnew,ynew,znew,xmin,ymin,zmin,
     * xmax,ymax,zmax,xdist,ydist,zdist,xtrans,ytrans,ztrans 

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(nplen), yic(nplen), zic(nplen)
C
      pointer(iptrans, trans)
      real*8 trans(1000000)
C
      character*3  ctype
      character*32 ich1,ich2,ich3
      character*32 isubname, cmo
      character*132 logmess
      character*132 cbuff
C
C
C#######################################################################
C begin
C
C
      isubname='translat'
C
      ierr = 0
      mpno=0
      iopt_auto = 0
      iopt_zero = 0
      iopt_center = 0
      iopt_original = 0
      ix = 1
      iy = 1
      iz = 1
      ctype = 'xyz'
      ich1 = ' '
      ich2 = ' '
      ich3 = ' '
C
C     Get mesh object info
 
      call cmo_get_name(cmo,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('ipointi',cmo,ipointi,length,icmotype,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('ipointj',cmo,ipointj,length,icmotype,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'get_info_i')
C
      if(npoints .eq. 0)then
C
C     Nothing is going to happen if there are no nodes.
C
         write(logmess,'(a,a,a)')'WARNING: trans: ',
     1      cmo(1:icharlnf(cmo)),' has zero nodes. NO ACTION.'
            call writloga('default',0,logmess,0,ierrw)
            return
      else
         call mmgetblk('mpary',isubname,ipmpary,npoints,2,ierr)
      endif
      if (ierr .ne. 0) call x3d_error(isubname,'mmgetblk')
C
C     ..................................................................
C     GET PARSER VALUES FOR POINT SET
C
      if (ipointi.eq.0) ipointi = max(1,ipointi)
      if (ipointj.eq.0) ipointj = max(1,npoints)
 
      if(msgtype(2).eq.1.and.imsgin(2).eq.0) then
         imsgin(2)=ipointi
      endif
      if(msgtype(3).eq.1.and.imsgin(3).eq.0) then
         imsgin(3)=ipointj
      endif
      if(msgtype(4).eq.1.and.imsgin(4).eq.0) then
         imsgin(4)=1
      endif
 
C     Set the point index boundaries defined by integers or pset
C
      if(msgtype(2).eq.1) then
         ipt1 = imsgin(2)
         ipt2 = imsgin(3)
         ipt3 = imsgin(4)
         if(nwds.eq.2) then
            ipt2=ipt1
            ipt3=1
         elseif(nwds.eq.3) then
            ipt3=1
         endif
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
      else
         ich1 = cmsgin(2)
         ich2 = cmsgin(3)
         ich3 = cmsgin(4)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
      endif
 
C
C     ..................................................................
C     GET PARSER VALUES FOR OLD AND NEW COORDINATES
C
      if (msgtype(5).ne.3 ) then
        call test_argument_type(6,2,5,imsgin,xmsgin,cmsgin,msgtype,nwds)
        xold = xmsgin(5)
        yold = xmsgin(6)
        zold = xmsgin(7)
        xnew = xmsgin(8)
        ynew = xmsgin(9)
        znew = xmsgin(10)
 
C     ..................................................................
C     ELSE GET PARSER VALUES FOR AUTO TRANSLATION
C
      else
        if (cmsgin(5)(1:4).eq. 'zero')     iopt_zero = 1
        if (cmsgin(5)(1:6).eq. 'center')   iopt_center = 1
        if (cmsgin(5)(1:8).eq. 'original') iopt_original = 1
        if (iopt_zero.ne.0.or. iopt_center.ne.0 .or.
     *      iopt_original.ne.0 )           iopt_auto = 1
        inxt=6
        if (msgtype(inxt).eq.3 .and. icharlnf(cmsgin(inxt)).eq.3) then
          ctype = cmsgin(inxt)(1:3)
          inxt=inxt+1
          if(ctype(1:3).ne.'xyz') then
            write(logmess,'(a3,a)')ctype,' option not yet implemented.'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
          endif
        endif
 
        if (msgtype(inxt).eq.1) ix=imsgin(inxt)
        if (msgtype(inxt+1).eq.1) iy=imsgin(inxt+1)
        if (msgtype(inxt+2).eq.1) iz=imsgin(inxt+2)
C
        if (nwds.ge.inxt .and. msgtype(inxt).eq.3) then
           ix=0
           iy=0
           iz=0
           do i = inxt,nwds
             if (cmsgin(i)(1:1).eq.'x') ix=1
             if (cmsgin(i)(1:1).eq.'y') iy=1
             if (cmsgin(i)(1:1).eq.'z') iz=1
           enddo
        endif
 
C     END PARSER EVALUATION
      endif
 
C     ..................................................................
C     GET OR SETUP TRANSLATE ATTRIBUTE
C
       call cmo_get_info('transauto',cmo,iptrans,ilen,ityp,icscode)
       if(icscode.ne.0) then
 
         cbuff = 'cmo/addatt/' // cmo(1:icharlnf(cmo)) //
     *          '/transauto/VDOUBLE/scalar/vector' //
     *          '/user/permanent/x/0.0' //
     *          ' ; finish'
         call dotaskx3d(cbuff,ierr)
         if (ierr.ne.0) call x3d_error(isubname,'addatt transauto')
 
         call cmo_newlen(cmo,ierr)
         call cmo_get_info('transauto',cmo,iptrans,ilen,ityp,ierr)
         if (ierr.ne.0) call x3d_error(isubname,'get_info transauto')
 
      endif
 
C     ..................................................................
C     IF AUTO TRANS - CALCULATE OLD AND NEW COORDINATES
C
      if (iopt_auto.eq.1) then
 
      call cmo_get_minmax(cmo,xmin,ymin,zmin,xmax,ymax,zmax,ierr)
      if (ierr.ne.0) call x3d_error(isubname,'get cmo minmax')
 
      xold = xmin
      yold = ymin
      zold = zmin
      xnew = xmin
      ynew = ymin
      znew = zmin
 
C     TRANSLATE MIN TO ZERO
      if (iopt_zero.eq.1) then
 
        if (ix.eq.1) xnew = 0.0
        if (iy.eq.1) ynew = 0.0
        if (iz.eq.1) znew = 0.0
 
C     TRANSLATE CENTER TO ZERO
      elseif (iopt_center.eq.1) then
 
        xdist=xmax-xmin
        ydist=ymax-ymin
        zdist=zmax-zmin
        if (ix.eq.1 .and. xdist.gt.0.0) xnew = 0.0 - (xdist/2.0)
        if (iy.eq.1 .and. ydist.gt.0.0) ynew = 0.0 - (ydist/2.0)
        if (iz.eq.1 .and. zdist.gt.0.0) znew = 0.0 - (zdist/2.0)
 
C     END coordinate destination types
      endif
C     END AUTO TRANS
      endif
C
 
C     Calculate translation values and save
 
      if (iopt_original.eq.1) then
        xtrans = -1.0*trans(1)
        ytrans = -1.0*trans(2)
        ztrans = -1.0*trans(3)
      else
        xtrans = xnew - xold
        ytrans = ynew - yold
        ztrans = znew - zold
      endif
 
c     Check that translation not already done
      if (xtrans.eq.0.0 .and. ytrans.eq.0.0 .and. ztrans.eq.0.0) then
        write(logmess,'(a)')'Trans already complete, nothing done.'
        call writloga('default',0,logmess,0,ierrw)
        goto 9999
      else
        trans(1) = xtrans
        trans(2) = ytrans
        trans(3) = ztrans
      endif
C
C     ..................................................................
C     TRANSLATE TO DESTINATION COORDINATES
C
C
      do ii=1,mpno
         i1=mpary(ii)
         xic(i1) = xic(i1) + xtrans
         yic(i1) = yic(i1) + ytrans
         zic(i1) = zic(i1) + ztrans
      enddo
C
c     ******************************************************************
c     error goes to this statement 9999
      goto 9999
 9999 continue
C
C     Update epsilon, and min/max values
      call setsize()
C
      call mmrelblk('mpary',isubname,ipmpary,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelblk')
C
      return
      end
C#######################################################################
      subroutine cmo_get_minmax(cmonam,
     >            xmin,ymin,zmin,xmax,ymax,zmax,ierr)
C
C      PURPOSE -
C
C         This routine returns the min and max values for x,y,z
C         Should be changed to get min max of any attribute
C
C      INPUT ARGUMENTS -
C
C         cmonam - (character) Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C#######################################################################
C
      implicit none
C
C#######################################################################
C ARGS
      character cmonam*32
      real*8 xmin,ymin,zmin,xmax,ymax,zmax
      integer ierr
 
C LOCAL
      integer i
      integer nnodes,length,icmotype
      integer ilen,ityp
 
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(1000000), yic(1000000), zic(1000000)
 
c BEGIN
 
c  get information from  mesh object
c
      call cmo_get_info('nnodes',cmonam,
     *                  nnodes,length,icmotype,ierr)
      call cmo_get_info('xic',cmonam,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmonam,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmonam,ipzic,ilen,ityp,ierr)
      if (ierr .ne. 0 ) return
 
      xmin = xic(1)
      xmax = xic(1)
      ymin = yic(1)
      ymax = yic(1)
      zmin = zic(1)
      zmax = zic(1)
      do i = 2, nnodes
        if (xic(i) .lt. xmin) xmin = xic(i)
        if (yic(i) .lt. ymin) ymin = yic(i)
        if (zic(i) .lt. zmin) zmin = zic(i)
 
        if (xic(i) .gt. xmax) xmax = xic(i)
        if (yic(i) .gt. ymax) ymax = yic(i)
        if (zic(i) .gt. zmax) zmax = zic(i)
      enddo
 
      return
      end
 
