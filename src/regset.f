      subroutine regset()
C
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE FINDS POINTS THAT ARE WITHIN A MINIMUM SEARCH RANGE
C     FROM MREGION DEFINITIONS AND SETS THE MATERIAL TYPE FOR THE
C     POINTS.  IT ALSO CALCULATES AND STORES THE OUTWARD UNIT NORMAL
C     FOR ANY REFLECTIVE BOUNDARY PLANES.
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/regset_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.8   29 Sep 2004 16:02:06   dcg
CPVCS    remove line that set value for pie - this is set in initlagrit 
CPVCS    
CPVCS       Rev 1.7   28 Mar 2000 14:09:24   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.6   28 Feb 2000 08:32:40   dcg
CPVCS    separate out as separate file getregv1
CPVCS    
CPVCS       Rev 1.5   Thu Feb 03 14:01:38 2000   dcg
CPVCS    quit
CPVCS    
CPVCS       Rev 1.4   Wed Feb 02 12:18:02 2000   dcg
CPVCS    
CPVCS       Rev 1.3   13 Jan 2000 14:48:26   dcg
CPVCS    
CPVCS       Rev 1.2   05 Jan 2000 17:33:20   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.36   Tue Oct 19 15:10:28 1999   dcg
CPVCS    change format statement to allow for more nodes
CPVCS
CPVCS       Rev 1.35   Fri Jun 19 09:40:18 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.34   Fri Dec 19 09:06:58 1997   dcg
CPVCS    if no mregions set imt1 to zero for all points and return
CPVCS
CPVCS       Rev 1.33   Mon Nov 24 16:35:08 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.31   Mon Apr 14 16:59:44 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.30   Thu Jun 20 13:24:50 1996   kuprat
CPVCS    Fixed vectorization bug.
CPVCS
CPVCS       Rev 1.29   Wed Jun 19 13:24:46 1996   kuprat
CPVCS    Corrected vectorized assignment of ISURFNO.
CPVCS
CPVCS       Rev 1.28   Tue Jun 18 19:31:20 1996   kuprat
CPVCS    Avoided 'do 200' loop by assigning 'isurfno' in a previous loop.
CPVCS
CPVCS       Rev 1.27   Mon Jun 03 15:10:40 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.26   Mon Jun 03 14:06:24 1996   dcg
CPVCS    'virtual' interface changes
CPVCS
CPVCS       Rev 1.25   Wed May 15 14:13:28 1996   dcg
CPVCS    change to test for interior interface points
CPVCS
CPVCS       Rev 1.24   Mon May 06 12:27:06 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.23   Tue Jan 23 09:09:10 1996   dcg
CPVCS    add points types to call to getregv1
CPVCS    reset point type to interior if point is on a surface
CPVCS    that is not relevant for the region under consideration
CPVCS
CPVCS       Rev 1.22   12/05/95 08:20:58   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.21   11/16/95 17:06:48   het
CPVCS    Create the getregv1 routine as a special case.
CPVCS
CPVCS       Rev 1.20   11/07/95 17:25:16   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.19   08/29/95 12:03:36   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.18   08/23/95 06:58:56   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.17   08/22/95 06:51:00   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.16   06/20/95 15:41:42   dcg
CPVCS    remove character literals from arguments list to hgetprt
CPVCS
CPVCS       Rev 1.15   06/07/95 15:31:38   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.14   05/26/95 13:16:32   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.13   05/15/95 13:37:02   het
CPVCS    Make changes to the regset and surfset routines
CPVCS
CPVCS       Rev 1.12   05/11/95 13:48:18   ejl
CPVCS    Installed epslion routines
CPVCS
CPVCS       Rev 1.11   05/01/95 08:34:14   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.10   03/31/95 09:09:44   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.9   03/30/95 05:00:48   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.8   03/23/95 15:08:24   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.7   03/17/95 21:11:44   het
CPVCS    Add the model and dictionary calles
CPVCS
CPVCS       Rev 1.6   02/21/95 17:31:26   dcg
CPVCS    compute normals correctly when no interior points exist
CPVCS
CPVCS       Rev 1.5   01/10/95 17:04:50   het
CPVCS    Cleanup operation on pvcs.
CPVCS
CPVCS
CPVCS       Rev 1.3   01/04/95 22:04:52   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.2   12/24/94 10:52:16   het
CPVCS    Add include files for chydro.h and comdict.h.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:26   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:16   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
      character*132 logmess
C
      include "chydro.h"
      include 'geom_lg.h'
      include 'consts.h'
 
C
C#######################################################################
C
      integer ierror,ilen,ityp,length,npoints,
     *  icscode,itype,lout,iout,
     * lenmm0,lenmm1,lenmm2,lenmm3,lenmm4,ics,npmiss,ip,
     * itp,imd,i,nimt,nmat,imtck,imt,npts,ipt,j,ipl
      real*8 srchval,xc,yc,zc,snorm,xn,yn,zn,fac,dot,
     *   dirction,xmin1,xmax1,ymin1,ymax1,zmin1,zmax1
      real*8 a,b,c,d,e,f,crosx,crosy,crosz,delx1,
     *   dely1,delz1,delx2,dely2,delz2,rout
      pointer(ipout,out)
      real*8 out(*)
      integer ismin,ismax
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer imt1(1000000), itp1(1000000)
      real*8 xic(1000000), yic(1000000), zic(1000000)
 
      character*32 iprtnam
      logical itsttp
C
      pointer(ipregno, iregno(1000000))
      pointer(ipsurfno, isurfno(1000000))
      integer iregno,isurfno
      character*32 isubname,  cmo,geom_name
      character*8 cpart
C
C#######################################################################
C
C     MACROS.
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C#######################################################################
C
C
C
C     ******************************************************************
C
      isubname='regset'
      cpart='part'
C
c  get mesh object info
c
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
      call cmo_get_intinfo('nnodes',cmo,
     *                  npoints,length,ityp,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierror)
C  if no mregions set all imts to 1 and return
      if (nmregs.eq.0) then
         do i=1,npoints
            imt1(i)=1
         enddo
         go to 75
      endif
C
C     ******************************************************************
C     GET THE SEARCH RANGE.
C
      call get_epsilon('epsilonl', srchval)
C
C     ******************************************************************
C     SET THE SURFACE NAMES AND POINTERS FOR ALL SURFACES
C
      if(nsurf.ne.0) then
         call mmfindbk('csall',geom_name,ipcsall,length,ierror)
         call mmfindbk('istype',geom_name,ipistype,length,ierror)
         call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
         call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      endif
c
c  get memory for surface and region related temporary
c  arrays
c
      lenmm0=nmregs
      lenmm1=nmregs*3
      lenmm2=nsurf*3
      lenmm3=maxdef*4
      lenmm4=npoints+100
      call mmgetblk('iregno',isubname,ipregno,lenmm4,2,ics)
      call mmgetblk('isurfno',isubname,ipsurfno,lenmm4,2,ics)
C
C     ******************************************************************
C     CALL getregv TO FIND THE MREGION FOR THE POINTS
C
      npts=npoints
      call getregv1(xic,yic,zic,itp1,npts,srchval,'mregion',0,
     &             cmo,
     &             iregno,isurfno,
     &             ierror)
c
c  getregv1 messes with pointers so get mregion info here
c
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      call mmfindbk('offmregdef',geom_name,ipoffmregdef,length,ierror)
      call mmfindbk('ndefmregs',geom_name,ipndefmregs,length,ierror)
      call mmfindbk('mregdef',geom_name,ipmregdef,length,ierror)
      call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
C
C     ******************************************************************
C     LOOP THROUGH POINTS TO FIND THOSE WITHIN srchval FROM
C     REGION DEFINITIONS.
C
      npmiss=0
      do 50 ip=1,npoints
C
C        ---------------------------------------------------------------
C        SKIP DUDDED POINTS.
C
         itp=itp1(ip)
         if (itp .eq. ifitpdud) go to 50
C
C        ---------------------------------------------------------------
C        IF THE POINT LIES IN A MREGION, PLACE THE imt IN FITWORD.
C
         if (iregno(ip) .lt. 0) then
C
C           ............................................................
C           THIS POINT IS IN MULTIPLE REGIONS SO "DUD" OUT THE POINT
C              AND CONTINUE.
C
            itp1(ip)=ifitpdud
C
         elseif (iregno(ip) .gt. 0) then
C
C           ............................................................
C           GET THE imd FOR THE MREGION NAME.
C
            imd=matregs(iregno(ip))
C
C           ............................................................
 
C           SET MATERIAL TYPE IN FITWORD.
C
            imt1(ip)=imd
C
C           ............................................................
C           RESET INTERFACE POINT TYPE.
C
            if (isurfno(ip).ne.0.and.itsttp('virtual',itp1(ip)))
     *                 go to 53
 
            if (isurfno(ip).ne.0.and.itp1(ip) .eq. ifitpini)
     *                itp1(ip)=ifitpint
   53       continue
C
C           ............................................................
C
         else
            if(imt1(ip).eq.0) itp1(ip)=ifitpdud
         endif
C
C        ---------------------------------------------------------------
C
   50 continue
C
C     ******************************************************************
C     FIND THE LARGEST imt TYPE.
C
      nmat=0
      do 55 i=1,npoints
         nmat=max(nmat,imt1(i))
   55 continue
C
C     ******************************************************************
C     COUNT AND PRINT THE NO. OF POINTS PER imt EXCEPT DUDDED POINTS.
C
      do 70 imt=0,nmat
         nimt=0
C
         do 60 i=1,npoints
            imtck=imt1(i)
            itp=itp1(i)
            if (imtck.eq.imt .and. itp.ne.ifitpdud) nimt=nimt+1
   60    continue
C
         if (nimt .gt. 0) then
            iprtnam=' '
            if (imt .gt. 0.and.imt.le.nmregs)
     &         iprtnam=cmregs(imt)
            write(logmess, 9000) imt,iprtnam,nimt
 9000       format(' FOR imt=',i4,' NAME= ',a8,' THERE ARE',i16,
     &             ' POINTS')
            call writloga('default',0,logmess,0,ierror)
         endif
C
   70 continue
C
C     ******************************************************************
C     RELEASE TEMPORARY MEMORY
C
 75   call mmrelprt(isubname,icscode)
C
C
C     ******************************************************************
C     CALCULATE AND STORE THE OUTWARD UNIT NORMAL FOR EACH REFLECTIVE
C     BOUNDARY PLANE.
C
 90   continue
C
      if (nb .ge. 1) then
C
C        ---------------------------------------------------------------
C        FIND AN INTERIOR POINT TO USE AS A TEST POINT
C
         do 100 i=1,npoints
            if (itsttp('interior',itp1(i))) then
               ipt=i
               xc=xic(ipt)
               yc=yic(ipt)
               zc=zic(ipt)
               goto 120
            endif
 100     continue
      i=ismin(npoints,xic,1)
      j=ismax(npoints,xic,1)
      xmin1=xic(i)
      xmax1=xic(j)
      i=ismin(npoints,yic,1)
      j=ismax(npoints,yic,1)
      ymin1=yic(i)
      ymax1=yic(j)
      i=ismin(npoints,zic,1)
      j=ismax(npoints,zic,1)
      zmin1=zic(i)
      zmax1=zic(j)
      xc=(xmin1+xmax1)/2.
      yc=(ymin1+ymax1)/2.
      zc=(zmin1+zmax1)/2.
 120     continue
C
C        ---------------------------------------------------------------
C        DETERMINE ON WHICH SIDE OF EACH REFLECTIVE PLANE THE TEST POINT
C        LIES AND SET THE NORMAL DIRECTIONS ACCORDINGLY.
C
 
         do 150 ipl=1,nb
            delx1=xbb(2,ipl)-xbb(1,ipl)
            dely1=ybb(2,ipl)-ybb(1,ipl)
            delz1=zbb(2,ipl)-zbb(1,ipl)
            delx2=xbb(3,ipl)-xbb(1,ipl)
            dely2=ybb(3,ipl)-ybb(1,ipl)
            delz2=zbb(3,ipl)-zbb(1,ipl)
            xn=crosx(delx1,dely1,delz1,delx2,dely2,delz2)
            yn=crosy(delx1,dely1,delz1,delx2,dely2,delz2)
            zn=crosz(delx1,dely1,delz1,delx2,dely2,delz2)
            snorm=sqrt(xn*xn+yn*yn+zn*zn)
            fac=1.0/(snorm+srchval)
            dot=xn*(xc-xbb(1,ipl))+yn*(yc-ybb(1,ipl))+
     *          zn*(zc-zbb(1,ipl))
            dirction=-sign(one,dot)
            xbbnorm(ipl)=fac*xn*dirction
            ybbnorm(ipl)=fac*yn*dirction
            zbbnorm(ipl)=fac*zn*dirction
 150     continue
      endif
C
C
C     ******************************************************************
C
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      return
      end
C
