      subroutine rmregion(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE REMOVES POINTS IN THE REQUESTED REGION.
C
C
C     FORMAT: RMREGION/CREGNAME
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
C        cregnam - REGION TO REMOVE POINTS FROM
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
C        $Log: rmregion.f,v $
C        Revision 2.00  2007/11/09 20:04:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   Thu Feb 17 10:54:38 2000   dcg
CPVCS    use geom_name for geometry info arrays
CPVCS    
CPVCS       Rev 1.4   Tue Feb 08 15:33:34 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Tue Feb 08 14:14:12 2000   dcg
CPVCS    
CPVCS       Rev 1.2   Tue Feb 08 13:07:44 2000   dcg
CPVCS
CPVCS       Rev 1.8   Fri Jan 22 16:05:56 1999   dcg
CPVCS    define epsmin to be epsilonl (it was previously undefined)
CPVCS
CPVCS       Rev 1.7   Mon Sep 21 13:33:18 1998   dcg
CPVCS    change call to getregv to check only for requested region
CPVCS
CPVCS       Rev 1.6   Mon Nov 24 16:37:58 1997   dcg
CPVCS     use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.5   Fri Oct 31 10:50:06 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:59:58 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Fri Feb 16 21:47:30 1996   het
CPVCS    Use only the specified region for testing.
CPVCS
CPVCS       Rev 1.2   12/05/95 08:26:06   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.1   11/07/95 17:25:30   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.0   09/20/95 09:46:38   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
      include "machine.h"
      include "chydro.h"
      include "consts.h"
      include 'geom_lg.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer icscode,imat,ierr,len1,ir,len2,len3,
     *  iregck,ilen,ityp,ipointf,nnodes,index,
     *  i,lenmm4,npts,nrm,ip,isq,itp,ics,ierrw,mbndry,
     *  nelements,ierr2,iout
      integer ismin,ismax,icharlnf
      real*8 xmin1,xmax1,ymin1,ymax1,zmin1,zmax1,rout,
     *   xdiff,ydiff,zdiff,srchval,epsmin
      character*32 geom_name
      pointer (ipout,out)
      real*8 out(*)
 
C
C#######################################################################
C
      character*132 logmess
C
C     *****************************************************************
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer imt1(10000000), itp1(10000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C     *****************************************************************
C
C
      character*32  iregnam
C
      pointer(ipregno, iregno(1000000))
      pointer(ipsurfno, isurfno(1000000))
      integer iregno,isurfno
 
 
C
C#######################################################################
      character*32 cmo
C
      character*32 isubname
C
C#######################################################################
C
      isubname='rmregion'
C
C     ******************************************************************
C
      call cmo_get_name(cmo,icscode)
C
C     ******************************************************************
C     GET INPUT PARAMETERS
C
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *    ipout,ilen,ityp,icscode)
      if(msgtype(1).eq.1) then
         call mmfindbk('cmregs',geom_name,ipcmregs,ilen,icscode)
         imat=imsgin(1)
         iregnam=cmregs(imat)
      elseif(msgtype(1).eq.2) then
         call mmfindbk('cmregs',geom_name,ipcmregs,ilen,icscode)
         imat=xmsgin(1)
         iregnam=cmregs(imat)
      elseif(msgtype(1).eq.3) then
         iregnam=cmsgin(1)
      endif
C
C     ******************************************************************
C     CHECK THAT THIS REGION EXISTS
C        ...............................................................
C        SET THE REGION NAMES, POINTERS AND NO. ELEMENTS FOR ALL REGIONS
C
      call mmfindbk('cregs',geom_name,ipcregs,ilen,ierr)
      len1=icharlnf(iregnam)
      do ir=1,nregs
         len2=icharlnf(cregs(ir))
         len3=max(len1,len2)
         if (iregnam(1:len3) .eq.cregs(ir)(1:len3)) iregck=ir
      enddo
C
C        ...............................................................
C        GET THE SEARCH RANGE.
C
      call get_epsilon('epsilonl', srchval)
C
C     ******************************************************************
C
C
      call cmo_get_name(cmo,icscode)
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,icscode)
      call cmo_get_info('nelements',cmo,
     *                   nelements,ilen,ityp,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,ityp,icscode)
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
C     GET MEMORY FOR ARRAYS CONTAINING REGION AND SURFACE POINTERS
C
      lenmm4=ipointf+100
      call mmgetblk('iregno',isubname,ipregno,lenmm4,2,ics)
      call mmgetblk('isurfno',isubname,ipsurfno,lenmm4,2,ics)
C
 
C        ...............................................................
C        CALL getregv TO FIND THE REGIONS THE POINTS LIE IN
C
         npts=nnodes
         do i=1,nnodes
            iregno(i)=-1
            isurfno(i)=-1
         enddo
         call getregv(xic,yic,zic,npts,srchval,'region',iregck,
     &                iregno,isurfno,
     &                ierr)
C
C     ******************************************************************
C     LOOP THROUGH THE POINTS
C
      nrm=0
      do 20 ip=1,nnodes
C
C        ---------------------------------------------------------------
C        IF THIS POINT LIES IN THE REGION, SET FITWORDS TO DUD THE
C        POINT OUT.
C
         if (iregno(ip) .eq. iregck) then
            nrm=nrm+1
            isq=0
            itp=ifitpdud
            itp1(ip)=ifitpdud
         endif
C
   20 continue
C
C     ******************************************************************
C     PRINT OUT THE NO. OF POINTS REMOVED.
C
      write(logmess,6000) nrm
 6000 format(' RMREGION DUDDED ',i6,' POINTS')
      call writloga('default',0,logmess,0,ierrw)
C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY
C
      call mmrelprt(isubname,icscode)
C
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
      return
      end
