      subroutine ifaceregv(x,y,z,npts,epsln,
     &                   imts1,nmts1,
     &                   ierr)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS ALL THE MATERIAL REGIONS THAT TOUCH THE
C     INTERFACE POINT (x,y,z).  THE MATERIAL TYPES MUST BE DEFINED
C     BY A REGION COMMAND.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE OF THE INTERFACE POINT TO CHECK
C        y - Y COORDINATE OF THE INTERFACE POINT TO CHECK
C        z - Z COORDINATE OF THE INTERFACE POINT TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C
C
C     OUTPUT ARGUMENTS -
C
C        imts - ARRAY CONTAINING THE MREGIONS THAT TOUCH THE POINT
C        nimts - NO. OF MREGIONS TOUCHING THE POINT
C        ierr - ERROR FLAG:
C                 1 - A MATERIAL TYPE ERROR
C                 2 - NO REGIONS FOUND
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/ifacereg_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.4   Wed Apr 05 13:34:34 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.3   02 Feb 2000 17:42:54   dcg
CPVCS    
CPVCS       Rev 1.2   13 Jan 2000 14:48:06   dcg
CPVCS    
CPVCS       Rev 1.1   05 Jan 2000 17:32:52   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.8   Wed Nov 26 11:42:24 1997   dcg
CPVCS    move implicit statement for HP
CPVCS
CPVCS       Rev 1.7   Mon Nov 24 16:34:08 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 16:51:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Fri Dec 22 14:12:44 1995   het
CPVCS    Correct a memory management error.
CPVCS
CPVCS       Rev 1.4   12/05/95 08:20:50   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.3   06/20/95 15:43:50   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.2   05/01/95 08:34:36   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:06   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:22   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
c
      include 'geom_lg.h'
C
C#######################################################################
C
      integer npts,ierr,length,icscode,i,ir,ierr2,ierror
      integer iout,lout,itype
      pointer(ipout,out)
      real*8 out(*),rout
      character*32 cmo,geom_name
      real*8 x(npts), y(npts), z(npts),epsln
      integer nmts1(npts), imts1(nmregs,npts)
C
C
C#######################################################################
C
      character*32 isubname
C
      pointer (ipiregloc, iregloc)
      integer iregloc(npts)
C
C#######################################################################
C
C
      isubname='ifaceregv'
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     ******************************************************************
C
C     INITIALIZE THE RETURN VARIABLES.
C
      ierr=0
C
C     ******************************************************************
C
C     CHECK THAT MREGIONS EXIST.
C
      if (nmregs .le. 0) then
         ierr=1
         go to 9999
      else
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
         call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,
     *        ierror)
         call mmfindbk('ndefmregs',geom_name,ipndefmregs,length,ierror)
         call mmfindbk('mregdef',geom_name,ipmregdef,length,ierror)
         call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
      endif
C
      length=npts
      call mmgetblk('iregloc',isubname,ipiregloc,length,2,icscode)
C
C     ******************************************************************
C
C     LOOP THROUGH MREGIONS, SAVE IMT IF THE POINT IS "ON" THE REGION
C     SURFACE.
C
      do i=1,npts
         nmts1(i)=0
      enddo
C
      do 30 ir=1,nmregs
C
C        ---------------------------------------------------------------
C        CALL chkreg TO SEE IF THIS POINT IS ON THE REGION SURFACE.
C
         call chkregv(x,y,z,npts,epsln,'mregion',
     &                  mregdef(offmregdef(ir)+1),ndefmregs(ir),
     &                  iregloc,ierr2)
C
C        ---------------------------------------------------------------
C        IF THE POINT IS "ON" THE SURFACE, GET AND SAVE IMT
C
         do i=1,npts
            if (iregloc(i) .eq. 2) then
               nmts1(i)=nmts1(i)+1
               imts1(nmts1(i),i)=matregs(ir)
            endif
         enddo
C
   30 continue
c
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
*dk,ifacereg
      subroutine ifacereg(x,y,z,epsln,
     &                   imts,nimts,
     &                   ierr)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS ALL THE MATERIAL REGIONS THAT TOUCH THE
C     INTERFACE POINT (x,y,z).  THE MATERIAL TYPES MUST BE DEFINED
C     BY A REGION COMMAND.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE OF THE INTERFACE POINT TO CHECK
C        y - Y COORDINATE OF THE INTERFACE POINT TO CHECK
C        z - Z COORDINATE OF THE INTERFACE POINT TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C
C     OUTPUT ARGUMENTS -
C
C        imts - ARRAY CONTAINING THE MREGIONS THAT TOUCH THE POINT
C        nimts - NO. OF MREGIONS TOUCHING THE POINT
C        ierr - ERROR FLAG:
C                 1 - A MATERIAL TYPE ERROR
C                 2 - NO REGIONS FOUND
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/x3d/src/ifacereg.f_a  $
CPVCS
CPVCS       Rev 1.3   06/20/95 15:43:50   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.2   05/01/95 08:34:36   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:06   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:22   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include 'geom_lg.h'
C
C#######################################################################
C
      integer imts(*)
C
      integer ierr,length,nimts,ierror
      integer iout,lout,itype
      pointer(ipout,out)
      real*8 out(*),rout
C
      character*32 iregloc,geom_name
c
C#######################################################################
C     INITIALIZE THE RETURN VARIABLES.
C
      ierr=0
c
C     CHECK THAT MREGIONS EXIST.
C
      if (nmregs .le. 0) then
         ierr=1
         go to 9999
      else
         call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
         call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,
     *        ierror)
         call mmfindbk('ndefmregs',geom_name,ipndefmregs,length,ierror)
         call mmfindbk('mregdef',geom_name,ipmregdef,length,ierror)
         call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
      endif
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     ******************************************************************
C
C     LOOP THROUGH MREGIONS, SAVE IMT IF THE POINT IS "ON" THE REGION
C     SURFACE.
C
      nimts=0
      do 30 ir=1,nmregs
C
C        ---------------------------------------------------------------
C        CALL chkreg TO SEE IF THIS POINT IS ON THE REGION SURFACE.
C
         call chkreg(x,y,z,epsln,'mregion',
     &                  mregdef(offmregdef(ir)+1),ndefmregs(ir),
     &                  iregloc,ierr2)
C
C        ---------------------------------------------------------------
C        IF THE POINT IS "ON" THE SURFACE, GET AND SAVE IMT
C
         if (iregloc(1:2) .eq. 'on') then
            nimts=nimts+1
            imts(nimts)=matregs(ir)
         endif
C
   30 continue
C
C     ******************************************************************
C     IF NO INTERFACE FOUND, SET ERROR
C
      if (nimts .eq. 0) ierr=2
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
