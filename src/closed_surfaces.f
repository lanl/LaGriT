      subroutine closed_surfaces(cbndry)
C
C
C#######################################################################
C
C     PURPOSE -
C
c  set point type(itp) and imt for all nodes - if a node sits on
C  exactly one surface, it is a boundary node; if it sits on
C  more that one surface, it is an interface node;  otherwise
C  it is an interior node
C
C
C     INPUT ARGUMENTS -
C
C   cbndry  - boundary type - reflect or free
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log: closed_surfaces.f,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   29 Sep 2004 15:58:48   dcg
CPVCS    remove line that set value for pie - this is set in initlagrit
CPVCS    
CPVCS       Rev 1.6   07 Aug 2001 11:19:54   dcg
CPVCS    remove undefined variable matintrface
CPVCS    
CPVCS       Rev 1.5   28 Mar 2000 14:08:44   dcg
CPVCS    remove include 'machine.h'
CPVCS    
CPVCS       Rev 1.4   Thu Feb 03 12:49:42 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Thu Feb 03 08:46:46 2000   dcg
CPVCS    
CPVCS       Rev 1.2   Wed Feb 02 13:27:56 2000   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:38   dcg
CPVCS    
CPVCS       Rev 1.0   06 Jan 2000 12:54:56   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.3   Wed Apr 07 21:04:52 1999   jtg
CPVCS    call to getregv2 from closed_surfaces had iregck a constant,
CPVCS    then tried to reset it in getregv2. This is fixed.
CPVCS
CPVCS       Rev 1.2   Fri Jan 22 09:53:48 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.1   Fri Jun 19 09:40:08 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.0   Fri May 29 14:55:42 1998   dcg
CPVCS    Initial revision.
 
C#######################################################################
C
      implicit none
C
      include "chydro.h"
      include 'geom_lg.h'
      include 'consts.h'
 
C argument
      character*8 cbndry
C#######################################################################

C variables
C
      pointer(ipregno, iregno(*))
      pointer(ipsurfno, isurfno(*))
      integer iregno,isurfno

      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer imt1(*), itp1(*)

      pointer(ipout,out)
      real*8 out(*)

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)

      real*8 srchval,rout

      integer ierror,ilen,ityp,length,npoints,
     * icscode, nconbnd, lout, itype, 
     * lenmm0,lenmm1,lenmm2,lenmm3,lenmm4,ics,npmiss,ip,
     * itp,imd,i,nimt,imtck,imt,npts,
     * iregck, iout
 
      character*32 isubname,  cmo, geom_name
      character*132 logmess
      character*32 crtype
C
C#######################################################################
C
C     MACROS.
C
      real*8 crosx,crosy,crosz, a,b,c,d,e,f
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C#######################################################################
C BEGIN begin
C
      isubname='close_surfaces'
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) call x3d_error('closed_surfaces','cmo_get_name')
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
      call cmo_get_info('nnodes',cmo,
     *                  npoints,length,ityp,ierror)
      call cmo_get_info('nconbnd',cmo,nconbnd,length,ityp,ierror)
      if(ierror.ne.0) nconbnd=0
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierror)
C
C     ******************************************************************
C     CHECK THAT MREGION DEFINITIONS EXIST AND GET NO. OF MREGION
C     SET THE REGION NAMES, POINTERS AND NO. ELEMENTS FOR ALL REGIONS
C
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
C        SET 'intrface' IN PART NAMES AND GET imd.
C        Refresh the storage block pointer information.
C
      call mmfindbk('cmregs',geom_name,ipcmregs,length,ierror)
      call mmfindbk('matregs',geom_name,ipmatregs,length,ierror)
 
      nmregs=nmregs+1
      cmregs(nmregs)='intrface'
      matregs(nmregs)=nmregs
C
C     CALL conbld TO SET REFLECTIVE BOUNDARY POINTS AND CONSTRAINTS
C
      if (nconbnd .gt. 0) call conbld()
C
C
C     ******************************************************************
C     GET MEMORY FOR ARRAYS CONTAINING REGION AND SURFACE POINTERS
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
      iregck=0
      crtype='mregion'
      call getregv2(xic,yic,zic,itp1,npts,srchval,crtype,iregck,
     &             cmo,iregno,isurfno,
     &             ierror)
C
C     ******************************************************************
C     LOOP THROUGH POINTS TO FIND THOSE WITHIN srchval FROM
C     REGION DEFINITIONS.
C
      npmiss=0
      do ip=1,npoints
C
C        ---------------------------------------------------------------
C        SKIP DUDDED POINTS.
C
        itp=itp1(ip)
        if (itp .ne. ifitpdud)then
C
C        ---------------------------------------------------------------
C        IF THE POINT LIES IN A MREGION, PLACE set imt .
C         if not dud the point out
C
          if (iregno(ip) .lt. 0) then
            itp1(ip)=ifitpdud
          elseif (iregno(ip) .gt. 0) then
C
C           GET THE material number FOR THE MREGION NAME.
C
            imd=matregs(iregno(ip))
C           SET MATERIAL TYPE
C
            imt1(ip)=imd
C           SET POINT TYPE.
C
            if (isurfno(ip).eq.0) then
               itp1(ip)=ifitpint
            elseif (isurfno(ip).gt.1) then
               itp1(ip)=ifitpini
               imt1(ip)=nmregs
            elseif (isurfno(ip).eq.1) then
               if (cbndry(1:7).eq.'reflect') then
                  itp1(ip)=ifitprfl
               elseif (cbndry(1:4).eq.'free') then
                  itp1(ip)=ifitpfre
               else
                  itp1(ip)=ifitpdud
               endif
            else
              itp1(ip)=ifitpdud
            endif
          endif
        endif
      enddo
C     ******************************************************************
C     COUNT AND PRINT THE NO. OF POINTS PER imt EXCEPT DUDDED POINTS.
C
      do 70 imt=0,nmregs
         nimt=0
C
         do 60 i=1,npoints
            imtck=imt1(i)
            itp=itp1(i)
            if (imtck.eq.imt .and. itp.ne.ifitpdud) nimt=nimt+1
   60    continue
C
         if (nimt .gt. 0) then
            write(logmess, 9000) imt,cmregs(imt),nimt
 9000       format(' FOR imt=',i4,' NAME= ',a8,' THERE ARE',i6,
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
 9999 continue
C
C     ******************************************************************
C
      return
      end
C
      subroutine getregv2(x,y,z,itp1,npts,epsln,crtype,iregck,
     &                   cmo,iregno,isurfno,ierr)

C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE RETURNS THE REGION NO. THAT THE POINT (x,y,z) LIES
C     IN.  IF IT DOES NOT LIE IN ANY REGION, iregno AND isurfno
C     ARE SET TO 0.  IF IT LIES ON A SURFACE, iregno IS SET  AND
C     isurfno IS incremented
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE OF THE POINTS TO CHECK
C        y - Y COORDINATE OF THE POINTS TO CHECK
C        z - Z COORDINATE OF THE POINTS TO CHECK
C        npts - NO. OF POINTS TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        crtype - TYPE OF REGION TO CHECK (region or mregion)
C        iregck - PARTICULAR REGION TO CHECK AGAINST (0=ALL REGIONS)
C        iregck - PARTICULAR REGION TO CHECK AGAINST (0=ALL REGIONS)
C        ipiregs - POINTER TO THE ARRAY of region data:
C        ipcregs - POINTER TO THE ARRAY of ALL REGION NAMES
C        ipndefregs- POINTER TO THE ARRAY of NO. OF ELEMENTS PER REGION
C
C        ipisall - POINTER TO THE ARRAY of SURFACE DATA POINTERS
C        ipcsall - POINTER TO THE ARRAY ofALL SURFACE NAMES
C        ipisatt - POINTER TO THE ARRAY of SURFACE attribute POINTERS
C
C     OUTPUT ARGUMENTS -
C
C        iregno - REGION NO. THAT THE POINTS LIE IN
C        isurfno - number of surfaces  THAT THE POINTS LIE ON
C        ierr - ERROR FLAG
C
C#######################################################################
C
      implicit none
C
      include "chydro.h"
      include 'geom_lg.h'
C
C#######################################################################
C arguments getregv2(x,y,z,itp1,npts,epsln,crtype,iregck,
C                        cmo,iregno,isurfno, ierr)

      character*32 crtype,cmo
      integer npts,jp,ierr,iregck
      integer itp1(npts), iregno(npts), isurfno(npts)
      real*8 x(npts),y(npts),z(npts)
      real*8 epsln

C variables
      integer icscode,irstrt,irend,ip,ii,is,i2,
     *   ir,ndef,nltgt,nstr,idefreg,len0, len1,len2,nxsurf,i
      integer ierror,ioff,length,itype,lout,iout
C
      pointer(ipout,out)
      real*8 out(*)
      real*8 rout
C
      integer icharlnf
      integer isurftst(512)
      logical ioper
C
      character*32 isubname, iword, iword1, iword2, itest
      character*32 isurfnm,geom_name
C
C#######################################################################
C BEGIN begin
C
      isubname='getregv2'
      iregck=0
C
C     ******************************************************************
C     INITIALIZE THE RETURN VARIABLES.
C
      do 5 jp=1,npts
         iregno(jp)=0
         isurfno(jp)=0
    5 continue
      ierr=0
C
C     ******************************************************************
C
C     MAKE SURE WE HAVE REGIONS AND SURFACES.
C
      if ( (nregs.le.0.and.crtype(1:6).eq.'region') .or.
     *   (nmregs.le.0.and.crtype(1:7).eq.'mregion')
     *      .or. nsurf.le.0) then
         ierr=1
         go to 9999
      endif
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      call mmfindbk('istype',geom_name,ipistype,length,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      if (crtype(1:6).eq.'region') then
        call mmfindbk('cregs',geom_name,ipcregs,length,ierror)
        call mmfindbk('offregdef',geom_name,ipoffregdef,length,ierror)
        call mmfindbk('ndefregs',geom_name,ipndefregs,length,ierror)
        call mmfindbk('regdef',geom_name,ipregdef,length,ierror)
      else
        call mmfindbk('cmregs',geom_name,ipcregs,length,ierror)
        call mmfindbk('offmregdef',geom_name,ipoffregdef,length,ierror)
        call mmfindbk('ndefmregs',geom_name,ipndefregs,length,ierror)
        call mmfindbk('mregdef',geom_name,ipregdef,length,ierror)
      endif
C
C     ******************************************************************
C
C     LOOP THROUGH THE REGIONS AND SEE IF INSIDE OR ON -
C     ALL REGIONS MUST BE INCLOSED BY A SINGLE SURFACE
C
      irstrt=1
      irend=nregs
      if(crtype(1:7).eq.'mregion') irend=nmregs
C     ******************************************************************
C
C     LOOP THROUGH POINTS IN GROUPS OF 512.
C
      do 150 jp=1,npts,512
C
         i2=512
         if ((jp+i2-1) .gt. npts) i2=npts-jp+1
C
C        ---------------------------------------------------------------
C
C        LOOP THROUGH REGION DEFINITIONS.
C
         do 100 ir=1,nmregs
C
C           ............................................................
C           SET THE REGION DEF. POINTER AND NO. ELEMENTS FROM iregs
C
            ioff=offregdef(ir)
            ndef=ndefregs(ir)
C
C           ............................................................
C           LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION,
C           closed_surface definition must be of the form ge surfnam
C           or le surfnam - anything else is an error
C
            nxsurf=0
            nltgt=0
            nstr=0
            idefreg=1
            ioper=.false.
            do 10 i=1,ndef
               iword=regdef(ioff+i)
               idefreg=idefreg+1
               len0=icharlnf(iword)
               isurfnm=' '
               if (iword(1:len0) .eq. 'lt' .or.
     &             iword(1:len0) .eq. 'le' ) then
                   itest = 'le'
                   ioper=.true.
               elseif (iword(1:len0) .eq. 'ge' .or.
     &             iword(1:len0) .eq. 'gt') then
                   itest = 'ge'
                   ioper=.true.
               elseif(ioper) then
                   isurfnm=iword(1:len0)
 
C                 ......................................................
C                 LOOP THROUGH isall TO FIND MATCHING SURFACE POINTERS.
C                 THEN TEST WITH SURFTST.
C
                  do  ii=1,nsurf
                     iword1=isurfnm
                     iword2=csall(ii)
                     len1=icharlnf(iword1)
                     len2=icharlnf(iword2)
                     len0=max(len1,len2)
                     if (iword1(1:len0) .eq. iword2(1:len0)) is=ii
                  enddo
                  call surftstv(x(jp),y(jp),z(jp),i2,epsln,cmo,
     &                  istype(is),surfparam(offsparam(is)+1),
     &                   sheetnm(is),itest,isurftst)
                  do ip=1,i2
                     if(isurftst(ip).eq.1) then
                        iregno(jp+ip-1)=ir
                     endif
                  enddo
                  itest='eq'
                  call surftstv(x(jp),y(jp),z(jp),i2,epsln,cmo,
     &                  istype(is),surfparam(offsparam(is)+1),
     &                  sheetnm(is),itest,isurftst)
                  do ip=1,i2
                     if(isurftst(ip).eq.1) then
                        isurfno(jp+ip-1)=isurfno(jp-1+ip)+1
                     endif
                  enddo
               else
               endif
C
   10       continue
C
   50       continue
 
C end loop on regions
  100    continue
C
C end loop on blocks of points
  150 continue
C *****************************************************************
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      call mmrelprt(isubname,icscode)
      return
      end
