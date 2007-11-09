*dk,rm
      subroutine rm(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE IS USED TO REMOVE A SPECIFIED LIST OF POINTS.
C
C        FORMAT: RM/igeom/ipstart/ipend/ipstep/xmn,ymn,zmn/xmx,ymx,zmx
C                                      /xcen,ycen,zcen/xscl,yscl,zscl
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
C        $Log: rm.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.13   29 Sep 2004 16:03:44   dcg
CPVCS    remove line that set value for pie - this is set in initlagrit 
CPVCS
CPVCS       Rev 1.12   Thu Apr 06 13:56:54 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 16:59:52 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Mon Mar 04 11:12:24 1996   dcg
CPVCS    remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.9   Fri Feb 02 14:24:16 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.8   11/16/95 15:22:24   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.7   11/07/95 17:25:22   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   09/19/95 13:10:22   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.5   07/17/95 16:02:04   dcg
CPVCS    activate coordsys changes for rm, rz commands
CPVCS
CPVCS       Rev 1.4   06/13/95 09:02:38   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.3   03/10/95 17:13:38   dcg
CPVCS     put in mesh object calls
CPVCS
CPVCS       Rev 1.2   02/18/95 06:57:08   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:04   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:20   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      include 'chydro.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      character*132 logmess
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000)
      dimension xic(1000000), yic(1000000), zic(1000000)
C
C#######################################################################
C
      character*32 isubname
      character*32 ich1,ich2,ich3
C
      character*32  cgeom, cmo
C
C#######################################################################
C
      pointer ( ipmaspnt, maspnt(1) )
      pointer ( ipmpary, mpary(1) )
C
C#######################################################################
C
C
C
      isubname='rm'
C
      ierror = 0
C
C
C  get mesh object
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
C
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,ierr)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,ierr)
C
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
C
      call cmo_get_info('ipointi',cmo,ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      ipointi = max(1,ipointi)
      if(ipointj.eq.0) ipointj = max(1,npoints)
C
      if(msgtype(3).eq.1.and.imsgin(3).eq.0) then
         imsgin(3)=ipointi
      endif
      if(msgtype(4).eq.1.and.imsgin(4).eq.0) then
         imsgin(4)=ipointj
      endif
      if(msgtype(5).eq.1.and.imsgin(5).eq.0) then
         imsgin(5)=1
      endif
C
      call mmgetblk('mpary',isubname,ipmpary,npoints,2,icscode)
C
C
C     ..................................................................
C     CHECK TO POINT LIMITS AND TRANSLATE THEM TO VALID LIMITS IF
C     NECESSARY.
C
C    set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      mpno=0
C
      if(msgtype(3).eq.1) then
         ipt1=imsgin(3)
         ipt2=imsgin(4)
         ipt3=imsgin(5)
         if(nwds.eq.3) then
            ipt2=ipt1
            ipt3=1
         elseif(nwds.eq.4) then
            ipt3=1
         endif
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,ipointj,isetwd,itp1)
      else
         ich1=cmsgin(3)
         ich2=cmsgin(4)
         ich3=cmsgin(5)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,ipointj,isetwd,itp1)
      endif
C
C#######################################################################
C           TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chglocl(1,npoints,1)
C
      icount=ipointj
C
      cgeom=cmsgin(2)
C
      istart=5
      call test_argument_type(9,2,istart+1,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
      xmn=xmsgin(istart+1)
      ymn=xmsgin(istart+2)
      zmn=xmsgin(istart+3)
C
      xmx=xmsgin(istart+4)
      ymx=xmsgin(istart+5)
      zmx=xmsgin(istart+6)
C
      xcn=xmsgin(istart+7)
      ycn=xmsgin(istart+8)
      zcn=xmsgin(istart+9)
C
      if(nwds.ge.istart+10) then
         call test_argument_type(1,2,istart+10,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
         xscl=xmsgin(istart+10)
      else
         xscl=1.0
      endif
      if(nwds.ge.istart+11) then
         call test_argument_type(1,2,istart+11,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
         yscl=xmsgin(istart+11)
      else
         yscl=1.0
      endif
      if(nwds.ge.istart+12) then
         call test_argument_type(1,2,istart+12,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
         zscl=xmsgin(istart+12)
      else
         zscl=1.0
      endif
 
C
C     ******************************************************************
C     GET MEMORY FOR LOCAL VARIABLES.
C
      call mmgetblk('maspnt', isubname, ipmaspnt, npoints,2, icscode)
C
C     CALL THE ROUTINE THAT QUALIFIES THE POINTS THAT WILL BE
C        REMOVED FROM THE MESH BASED ON THE POINT AND GEOMETRIC
C        BOUNDARIES.
C
      nmaspnt=0
      do i1=1,npoints
         maspnt(i1)=0
      enddo
C
      if(cgeom(1:3).eq.'rtz') then
         ymn=ymn*pie/180.0d0
         ymx=ymx*pie/180.0d0
      elseif(cgeom(1:3).eq.'rtp') then
         ymn=ymn*pie/180.0d0
         ymx=ymx*pie/180.0d0
         zmn=zmn*pie/180.0d0
         zmx=zmx*pie/180.0d0
      endif
C
      call qualxmmn(cgeom,mpary,mpno,ipointj,xic,yic,zic,
     *                       xmn,ymn,zmn,xmx,ymx,zmx,
     *                       xcn,ycn,zcn,xscl,yscl,zscl,
     *                       nmaspnt,maspnt,ierr)
C
C
      if(nmaspnt.le.0) goto 9998
C
      do i=1,nmaspnt
         i1=maspnt(i)
         isq=0
C********itp=ifitpdud
         itp=21
         itpcp=itp1(i1)
         if(itpcp.eq.2) then
            itpcp=0
            i1tmp=i1
   50       continue
            isqtmp=isn1(i1tmp)
            isn1(i1tmp)=isq
            itp1(i1tmp)=itpcp
            i1tmp=isqtmp
            if(i1tmp.eq.0) goto 70
            goto 50
   70       continue
         endif
         isn1(i1)=isq
         itp1(i1)=itp
      enddo
C
      ierror=0
C
      if(ierror .eq. 0) then
         call cmo_get_info('nnodes',cmo,npoints,lencmo,itpcmo,ierror)
         ipointi = 1
         ipointj = npoints
         call cmo_set_info('ipointi',cmo,ipointi,
     &                      1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
         call cmo_set_info('ipointj',cmo,ipointj,
     &                      1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
      endif
C
C
C     ******************************************************************
C     RELEASE MEMORY FOR LOCAL VARIABLES IN THE PARTITION
C
 9998 call mmrelprt(isubname, ics)
C
      write(logmess,6000) nmaspnt
 6000 format(' RM DUDDED ',i6,' POINTS')
      call writloga('default',0,logmess,0,ierrw)
C
C           TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chgnorm(1,npoints,1)
C
      goto 9999
 9999 continue
      return
      end
