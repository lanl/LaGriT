      subroutine scale_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE SCALES ANY OF THE COORDINATE DIRECTIONS BY AN
C         ARBITRARY AMOUNT.
C
C         FORMAT: SCALE/ipstart/ipend/ipstep/scaling option/igeom
C                      /xscale,yscale,zscale/xcen,ycen,zcen
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C        $Log: scale_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Thu Apr 06 14:21:42 2000   dcg
CPVCS    replace get_info_i calls
CPVCS
CPVCS       Rev 1.1   22 Mar 2000 13:41:04   gable
CPVCS    Added call to setsize.
CPVCS
CPVCS       Rev 1.0   Tue Apr 13 08:43:04 1999   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 17:00:56 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Fri Feb 16 21:48:52 1996   het
CPVCS    Add the radius option and correct the calles to angle3v.
CPVCS
CPVCS       Rev 1.9   11/16/95 15:22:44   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.8   11/07/95 17:26:12   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.7   09/19/95 13:10:06   dcg
CPVCS    add primative syntax checking
CPVCS
CPVCS       Rev 1.6   07/20/95 08:42:48   dcg
CPVCS    set default center to 0.,0.,0.
CPVCS
CPVCS       Rev 1.5   06/13/95 09:02:42   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.4   05/26/95 13:18:46   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.3   03/10/95 17:13:42   dcg
CPVCS     put in mesh object calls
CPVCS
CPVCS       Rev 1.2   02/18/95 06:57:16   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:24   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:36   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
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
C.... Mesh Object Data.
C
      character*32 cmo
C
      integer npoints
C
      pointer (ipisetwd, isetwd)
      integer isetwd(1000000)
      pointer (ipitp1, itp1)
      integer itp1(1000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(1000000), yic(1000000), zic(1000000)
C
C#######################################################################
C
      integer ii, i1, ipointi, ipointj, ipt1, ipt2, ipt3, mpno
      integer ilen, itype, icscode, ierrw, istart
      real*8 xscale, yscale, zscale, rscale, tscale, pscale
      real*8 xcen, ycen, zcen, dummy
      real*8 xa, ya, za, ra, rnew, thi, thinew, phi, phinew, zero
C
      character*32 isubname
      character*32 ich1, ich2, ich3
C
      character*32 ctype, cgeom
C
      character*132 logmess
C
C#######################################################################
C
      pointer(ipmpary, mpary )
      integer mpary(1000000)
C
C#######################################################################
C
C
C
      isubname='scale'
C
      ierror = 0
C
C     ..................................................................
C     Get mesh object
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
        write(logmess,'(a)') 'SCALE found bad mesh object'
        call writloga('default',0,logmess,0,ierrw)
        go to 9999
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call mmgetblk('mpary',isubname,ipmpary,npoints,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
C
      call cmo_get_info('ipointi',cmo,ipointi,ilen,itype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,itype,icscode)
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
C     Set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      mpno=0
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
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                ipointj,isetwd,itp1)
      else
         ich1=cmsgin(2)
         ich2=cmsgin(3)
         ich3=cmsgin(4)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                ipointj,isetwd,itp1)
      endif
C
C     ..................................................................
C
      ctype=cmsgin(5)
C
      if ((nwds .lt. 6) .or.
     &    (cmsgin(6)(1:5) .eq. '-def-')) then
         cgeom = 'xyz'
      else
         cgeom = cmsgin(6)
      endif
C
      istart=7
C
C  set default center of geometry to the origin
C  use user supplied center if it exists.
      xcen=0.
      ycen=0.
      zcen=0.
      call test_argument_type(3,2,istart+3,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
      if(nwds.ge.10)xcen = xmsgin(istart+3)
      if(nwds.ge.11) ycen = xmsgin(istart+4)
      if(nwds.ge.12) zcen = xmsgin(istart+5)
C
C     Decifer which geometry is being used
C
      if(cgeom(1:3) .eq. 'xyz') then
C
C        ...............................................................
C        SCALE THE MESH COORDINATES IN REFERENCE TO CARTESIAN GEOMETRY.
C
         call test_argument_type(3,2,istart,imsgin,xmsgin,cmsgin,
     *                           msgtype,nwds)
         xscale = xmsgin(istart)
         yscale = xmsgin(istart+1)
         zscale = xmsgin(istart+2)
C
C
         if(ctype(1:8) .eq. 'relative') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xic(i1)=xscale*(xic(i1)-xcen)+xcen
               yic(i1)=yscale*(yic(i1)-ycen)+ycen
               zic(i1)=zscale*(zic(i1)-zcen)+zcen
            enddo
C
         elseif(ctype(1:8) .eq. 'absolute') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xic(i1)=xic(i1)+xscale
               yic(i1)=yic(i1)+yscale
               zic(i1)=zic(i1)+zscale
            enddo
C
         else
C
            write(logmess,'(a,a)')
     &            'SCALE: Invalid Type Option', ctype
            call writloga('default',0,logmess,0,ierrw)
            ierror = -1
C
         endif
C
      elseif(cgeom(1:3) .eq. 'rtz') then
C
C        ...............................................................
C        SCALE THE MESH COORDINATES IN REFERENCE TO CYLINDERICAL GEOMETRY.
C
         call test_argument_type(3,2,istart,imsgin,xmsgin,cmsgin,
     *                           msgtype,nwds)
         rscale = xmsgin(istart)
         tscale = xmsgin(istart+1)
         zscale = xmsgin(istart+2)
C
         if(ctype(1:8) .eq. 'relative') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xa=xic(i1)-xcen
               ya=yic(i1)-ycen
               za=zic(i1)
               ra=sqrt(xa**2+ya**2)
               zero=0.0d+00
               call angle3v(zero,zero,zero,xa,ya,zero,thi,dummy)
               rnew=rscale*ra
               thinew=tscale*thi
               xic(i1)=rnew*cos(thinew)+xcen
               yic(i1)=rnew*sin(thinew)+ycen
               zic(i1)=zscale*(za-zcen)+zcen
            enddo
C
         elseif(ctype(1:8) .eq. 'absolute') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xa=xic(i1)-xcen
               ya=yic(i1)-ycen
               za=zic(i1)
               ra=sqrt(xa**2+ya**2)
               zero=0.0d+00
               call angle3v(zero,zero,zero,xa,ya,zero,thi,dummy)
               rnew  =ra +rscale
               thinew=thi+tscale
               xic(i1)=rnew*cos(thinew)+xcen
               yic(i1)=rnew*sin(thinew)+ycen
               zic(i1)=za+zscale
            enddo
C
         elseif(ctype(1:6) .eq. 'radius') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xa=xic(i1)-xcen
               ya=yic(i1)-ycen
               za=zic(i1)
               ra=sqrt(xa**2+ya**2)
               xic(i1)=xa*rscale/(ra+1.0d-30)+xcen
               yic(i1)=ya*rscale/(ra+1.0d-30)+ycen
               zic(i1)=za
            enddo
C
         else
C
            write(logmess,'(a,a)')
     &            'SCALE: Invalid Type Option', ctype
            call writloga('default',0,logmess,0,ierrw)
            ierror = -1
C
         endif
C
      elseif(cgeom(1:3) .eq. 'rtp') then
C
C        ...............................................................
C        SCALE THE MESH COORDINATES IN REFERENCE TO SPHERICAL GEOMETRY.
C
         call test_argument_type(3,2,istart,imsgin,xmsgin,cmsgin,
     *                           msgtype,nwds)
         rscale = xmsgin(istart)
         tscale = xmsgin(istart+1)
         pscale = xmsgin(istart+2)
C
C
         if(ctype(1:8) .eq. 'relative') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xa=xic(i1)-xcen
               ya=yic(i1)-ycen
               za=zic(i1)-zcen
               ra=sqrt(xa**2+ya**2+za**2)
               zero=0.0d+00
               call angle3v(zero,zero,zero,xa,ya,za,phi,thi)
               rnew=rscale*ra
               thinew=tscale*thi
               phinew=pscale*phi
               xic(i1)=rnew*sin(thinew)*cos(phinew)+xcen
               yic(i1)=rnew*sin(thinew)*sin(phinew)+ycen
               zic(i1)=rnew*cos(thinew)+zcen
            enddo
C
         elseif(ctype(1:8) .eq. 'absolute') then
C
            do ii=1,mpno
               i1=mpary(ii)
               xa=xic(i1)-xcen
               ya=yic(i1)-ycen
               za=zic(i1)-zcen
               ra=sqrt(xa**2+ya**2+za**2)
               zero=0.0d+00
               call angle3v(zero,zero,zero,xa,ya,za,phi,thi)
               rnew  =ra +rscale
               thinew=thi+tscale
               phinew=phi+pscale
               xic(i1)=rnew*sin(thinew)*cos(phinew)+xcen
               yic(i1)=rnew*sin(thinew)*sin(phinew)+ycen
               zic(i1)=rnew*cos(thinew)+zcen
            enddo
C
         elseif(ctype(1:6) .eq. 'radius') then
C
C
            do ii=1,mpno
               i1=mpary(ii)
               xa=xic(i1)-xcen
               ya=yic(i1)-ycen
               za=zic(i1)-zcen
               ra=sqrt(xa**2+ya**2+za**2)
               xic(i1)=xa*rscale/(ra+1.0d-30)+xcen
               yic(i1)=ya*rscale/(ra+1.0d-30)+ycen
               zic(i1)=za*rscale/(ra+1.0d-30)+zcen
            enddo
C
         else
C
            write(logmess,'(a,a)')
     &            'SCALE: Invalid Type Option', ctype
            call writloga('default',0,logmess,0,ierrw)
            ierror = -1
C
         endif
C
      else
C
         write(logmess,'(a,a)')
     &         'SCALE: Invalid Geometry Option', cgeom
         call writloga('default',0,logmess,0,ierrw)
         ierror = -2
C
      endif
C
C     Update epsilon, and min/max values
      call setsize()
C
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
C
      goto 9999
 9999 continue
      return
      end
