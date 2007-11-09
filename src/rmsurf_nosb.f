      subroutine rmsurf(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE REMOVES POINTS IN, ON OR IN AND ON THE REQUESTED
C     SURFACE.
C
C
C     FORMAT: RMSURF/ISURNAM/IOPT
C
C
C     INPUT ARGUMENTS -
C
C        xmsgin - REAL ARRAY OF COMMAND INPUT VALUES
C        msgin - INTEGER ARRAY OF COMMAND INPUT VALUES
C        imsgin - INTEGER ARRAY OF COMMAND INPUT VALUES
C        nwds - NO. OF WORDS OF COMMAND INPUT VALUES
C
C      WHICH ARE CONVERTED TO:
C        isurfnm - SURFACE TO REMOVE POINTS FROM
C        ioper - POINT REMOVAL OPERATOR:
C                lt - REMOVE POINTS IN THE SURFACE ONLY
C                eq - REMOVE POINTS ON THE SURFACE ONLY
C                le - REMOVE ALL POINTS IN AND ON THE SURFACE
C
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr2 - INVALID INPUT ERROR FLAG
C
C
C     CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/rmsurf_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   Wed Feb 02 11:59:00 2000   dcg
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 12:34:00   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.7   Fri Jan 22 16:08:06 1999   dcg
CPVCS    define epsmin to be epsilonl (it was previously undefined)
CPVCS
CPVCS       Rev 1.6   Fri Mar 06 09:02:38 1998   dcg
CPVCS    get correct length for temporary memory
CPVCS
CPVCS       Rev 1.5   Fri Oct 31 10:50:08 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 17:00:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Tue Jun 25 10:41:18 1996   dcg
CPVCS    use icharlnf to limit operation
CPVCS    fix npoints value
CPVCS
CPVCS       Rev 1.2   12/05/95 08:26:08   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.1   11/07/95 17:25:34   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.0   09/20/95 09:46:48   dcg
CPVCS    Initial revision.
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
C#######################################################################
C
      include 'geom_lg.h'
      include 'chydro.h'
C
C
      character*32 cmo, isubname,ioper,isurfnm,geom_name
C
C     *****************************************************************
C
      pointer ( ipimt1 , imt1 )
      integer imt1(*)
      pointer ( ipitp1 , itp1 )
      integer itp1(*)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(*), yic(*), zic(*)
      character*132 logmess
      integer lenc,length,ierror,is,isurfck,ierr,ierr2,icharlnf,
     *  ierrw,nnodes,ilencmo,itypcmo,ilen,ityp,lenmm1,
     *  index,nrm,ip,isq,itp,icscode,nelements,mbndry,ipointf,
     *  ics,ismin,ismax,iout,lout,itype
      real*8 epsmin,xmin1,xmax1,ymin1,ymax1,xdiff,ydiff,zmin1,
     *  zmax1,zdiff,srchval,rout
      pointer(ipout,out)
      real*8 out(*)
      pointer (ipsurft,isurftst)
      integer isurftst(*)
C
C#######################################################################
C
      call cmo_get_name(cmo,icscode)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     ******************************************************************
C     GET INPUT PARAMETERS
C
      isurfnm=cmsgin(1)
      lenc = icharlnf(cmsgin(2))
      ioper='        '
      ioper=cmsgin(2)(1:lenc)
C
C     ******************************************************************
C     CHECK THAT THIS SURFACE EXISTS
C
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      call mmfindbk('istype',geom_name,ipistype,length,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      if (ierror.eq.0) then
            do is=1,nsurf
               if (csall(is).eq.isurfnm) then
                  isurfck=is
                  go to 8
               endif
            enddo
      endif
      ierror=1
      write(logmess,9025) isurfnm
 9025 format('  ERROR - SURFACE ',a8,' DOES NOT EXIST')
      call writloga('default',1,logmess,1,ierr)
      go to 9998
C
C     ******************************************************************
C     CHECK FOR VALID OPERATOR.
C
 8    if (ioper(1:2).ne.'lt' .and. ioper(1:2).ne.'eq'
     *       .and. ioper(1:2).ne.'le') then
         ierr2=1
         write(logmess,9001) ioper
 9001    format('  ERROR - ',a2,' IS INVALID')
         call writloga('default',0,logmess,0,ierrw)
         go to 9998
      endif
C
      call cmo_get_info('nnodes',cmo,nnodes,ilencmo,itypcmo,icscode)
      call cmo_get_info('nelements',cmo,
     *                   nelements,ilencmo,itypcmo,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilencmo,itypcmo,icscode)
C
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      ipointf=nnodes
C
C     ******************************************************************
C     GET TEMPORARY MEMORY
C
      lenmm1=nnodes+100
      isubname='rmsurf'
      call mmgetblk('isurftst',isubname,ipsurft,lenmm1,2,ics)
C
C
C     ******************************************************************
C
C     SET THE MINIMUM SEARCH RANGE TO SMALLEST X, Y, OR Z RANGE
C
      call get_epsilon('epsilonl',epsmin)
      index=ismin(ipointf,xic,1)
      xmin1=xic(index)
      index=ismax(ipointf,xic,1)
      xmax1=xic(index)
      xdiff=xmax1-xmin1
C
      index=ismin(ipointf,yic,1)
      ymin1=yic(index)
      index=ismax(ipointf,yic,1)
      ymax1=yic(index)
      ydiff=ymax1-ymin1
C
      index=ismin(ipointf,zic,1)
      zmin1=zic(index)
      index=ismax(ipointf,zic,1)
      zmax1=zic(index)
      zdiff=zmax1-zmin1
C
      srchval=((xdiff+ydiff+zdiff)/3.)*1.0e-8
      if (srchval .lt. epsmin) srchval=epsmin
C
C     ******************************************************************
C     CALL surftstv TO FIND POINTS THAT SATISFY THE OPERATOR FOR THE
C     SURFACE
C
      call surftstv(xic,yic,zic,nnodes,srchval,cmo,istype(isurfck),
     &     surfparam(offsparam(isurfck)+1),sheetnm(isurfck),
     &     ioper(1:2),isurftst)
C
C     ******************************************************************
C     LOOP THROUGH THE POINTS
C
      nrm=0
      do  ip=1,ipointf
C
C        ---------------------------------------------------------------
C        IF THIS POINT SATISFIES THE OPERATOR FOR THE SURFACE, SET
C        FITWORDS TO DUD THE POINT OUT.
C
         if (isurftst(ip) .eq. 1) then
            nrm=nrm+1
            isq=0
            itp=ifitpdud
            itp1(ip)=ifitpdud
         endif
C
      enddo
C
C     ******************************************************************
C     PRINT OUT THE NO. OF POINTS REMOVED.
C
      write(logmess,6000) nrm
 6000 format(' RMSURF DUDDED ',i6,' POINTS')
      call writloga('default',0,logmess,0,ierrw)
C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY
C
 9998 call mmrelprt(isubname,icscode)
C
 9999 continue
C
      return
      end
