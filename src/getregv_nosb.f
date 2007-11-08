      subroutine getregv(x,y,z,npts,epsln,irtype,iregck,
     &                   iregno,isurfno,
     &                   ierr)
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE RETURNS THE REGION NO. THAT THE POINT (x,y,z) LIES
C     IN.  IF IT DOES NOT LIE IN ANY REGION, iregno AND isurfno
C     ARE SET TO 0.  IF IT LIES ON A SURFACE, iregno IS SET TO 0 AND
C     isurfno IS SET TO THE SURFACE NUMBER  THE REGION NO. AND SURFACE
C     NO. ARE SET TO THE REGION NAME ORDER AND SURFACE NAME ORDER AS
C     STORED IN THE STORAGE BLOCK.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE OF THE POINTS TO CHECK
C        y - Y COORDINATE OF THE POINTS TO CHECK
C        z - Z COORDINATE OF THE POINTS TO CHECK
C        npts - NO. OF POINTS TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        irtype - TYPE OF REGION TO CHECK (region or mregion)
C        iregck - PARTICULAR REGION TO CHECK AGAINST (0=ALL REGIONS
c
C     OUTPUT ARGUMENTS -
C
C        iregno - REGION NO. THAT THE POINTS LIE IN
C        isurfno - SURFACE NO. THAT THE POINTS LIE ON
C        ierr - ERROR FLAG
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getregv_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.4   Fri Apr 07 10:15:40 2000   dcg
CPVCS    remove machine.h
CPVCS    replace use of KNWPN for length calculation with mmgetblk type 3
CPVCS    
CPVCS       Rev 1.3   Wed Feb 02 13:00:52 2000   dcg
CPVCS    
CPVCS       Rev 1.2   13 Jan 2000 14:48:00   dcg
CPVCS    
CPVCS       Rev 1.1   05 Jan 2000 17:32:42   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.9   Fri Jan 22 12:09:40 1999   dcg
CPVCS    removed  duplicate and unused declarations
CPVCS
CPVCS       Rev 1.8   Mon Nov 24 16:33:44 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:50:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Mon Jul 08 21:36:06 1996   kuprat
CPVCS    Avoided 'do 200' loop by assigning 'isurfno' in a previous loop.
CPVCS
CPVCS       Rev 1.5   11/07/95 17:18:18   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.4   05/11/95 19:53:10   het
CPVCS    Comment out the termgen() call
CPVCS
CPVCS       Rev 1.3   05/01/95 08:34:32   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.2   03/23/95 15:09:04   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.1   01/09/95 17:32:28   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:32   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
      character*132 logmess
C
      include 'geom_lg.h'
C
C#######################################################################
C
      integer npts,jp,ierr,length,icscode,irstrt,irend,iregck,i2,ip,
     *   ir,ndef,nltgt,nstr,idefreg,len0, len1,len2,nxsurf,i,ierror,
     *   npol,ierrp,iopflg,istk1,istk2,ns,ii,nwrite,ierrw,iptno,is,
     *   ioff,itype,iout,lout
      real*8 x(npts),y(npts),z(npts),epsln,rout
      pointer(ipout,out)
      real*8 out(*)
      integer iregno(npts),isurfno(npts)
C
      integer nregdup(512),isurftst(512)
      character*8 iregdup(512,20)
      logical stack(512,20),l1,l2,regck
      integer stkptr,icharlnf
C
      character*32 irtype, isubname, iword, iword1, iword2, itest,
     *             cpolish,cmo,geom_name
C
      pointer(ipiltgt, iltgt)
      pointer(ipsurfnm, isurfnm)
      pointer(ipnewdef, newdef)
      pointer(ippolish, polish)
 
      character*32 iltgt(10000000),isurfnm(10000000),
     *  newdef(10000000),polish(10000000)
 
C
C#######################################################################
C
      isubname='getregv'
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
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
      if ( (nregs.le.0.and.irtype(1:7).eq.'region') .or.
     *   (nmregs.le.0.and.irtype(1:7).eq.'mregion')
     *      .or. nsurf.le.0) then
         ierr=1
         go to 9999
      endif
      call mmfindbk('csall',geom_name,ipcsall,length,ierror)
      call mmfindbk('istype',geom_name,ipistype,length,ierror)
      call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
      call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
      call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
      call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
C
C     ******************************************************************
C
C     SET POINTERS FOR SCRATCH ARRAYS TO ALLOCATED STORAGE.
C
      length=max(maxdef,maxmdef)
      call mmgetblk('iltgt',isubname,ipiltgt,length,3,icscode)
      call mmgetblk('surfnm',isubname,ipsurfnm,length,3,icscode)
      call mmgetblk('newdef',isubname,ipnewdef,length,3,icscode)
      call mmgetblk('polish',isubname,ippolish,length,3,icscode)
C
C     ******************************************************************
C
C     IF CHECKING AGAINST A PARTICULAR REGION, SET irstrt AND irend.
C
      irstrt=1
      irend=nregs
      if(irtype(1:7).eq.'mregion') then
         irend=nmregs
         call mmfindbk('cmregs',geom_name,ipcregs,length,ierror)
         call mmfindbk('offmregdef',geom_name,ipoffregdef,length,ierror)
         call mmfindbk('ndefmregs',geom_name,ipndefregs,length,ierror)
         call mmfindbk('mregdef',geom_name,ipregdef,length,ierror)
      else
         call mmfindbk('cregs',geom_name,ipcregs,length,ierror)
         call mmfindbk('offregdef',geom_name,ipoffregdef,length,ierror)
         call mmfindbk('ndefregs',geom_name,ipndefregs,length,ierror)
         call mmfindbk('regdef',geom_name,ipregdef,length,ierror)
      endif
      if (iregck .gt. 0) then
         irstrt=iregck
         irend=iregck
      endif
C
C     ******************************************************************
C
C     LOOP THROUGH POINTS IN GROUPS OF 512.
C
      do 150 jp=1,npts,512
C
         i2=512
         if ((jp+i2) .gt. npts) i2=npts-jp+1
         do 6 ip=1,i2
            nregdup(ip)=0
    6    continue
C
C        ---------------------------------------------------------------
C
C        LOOP THROUGH REGION DEFINITIONS.
C
         do 100 ir=irstrt,irend
C
C           ............................................................
C           SET THE REGION DEF. POINTER AND NO. ELEMENTS FROM iregs
C
            ndef=ndefregs(ir)
            ioff=offregdef(ir)
C
C           ............................................................
C           LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C           SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C           ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C           AND LOGICAL OPERATORS(and,or,not).
C
            nxsurf=0
            nltgt=0
            nstr=0
            idefreg=1
            do 10 i=1,ndef
               iword=regdef(ioff+i)
               idefreg=idefreg+1
               len0=icharlnf(iword)
C
               if (iword(1:len0) .eq. 'lt' .or.
     &             iword(1:len0) .eq. 'gt' .or.
     &             iword(1:len0) .eq. 'le' .or.
     &             iword(1:len0) .eq. 'ge') then
                  nxsurf=i+1
                  nltgt=nltgt+1
                  iltgt(nltgt)=iword
                  nstr=nstr+1
                  newdef(nstr)=' '
                  write(newdef(nstr),'(i8)') nltgt
C
                else
                  if (i .eq. nxsurf) then
                     isurfnm(nltgt)=iword
                  else
                     nstr=nstr+1
                     newdef(nstr)=iword
                  endif
C
               endif
C
   10       continue
C
C           ............................................................
C           CALL eorpt TO GET A REVERSE POLISH STACK OF THE NEW
C           DEFINITION IN CORRECT PRIORITY ORDER
C
            call eorpt(newdef,nstr,polish,npol,ierrp)
C
C           ............................................................
C           EVALUATE THE REVERSE POLISH STACK
C           LOOP THROUGH THE NO. OF TOKENS IN THE POLISH STACK
C
            stkptr=0
C
            do 50 i=1,npol
               iopflg=0
               cpolish=polish(i)
C
C              .........................................................
C              IF THE TOKEN IS THE and OPERATOR, PERFORM THE OPERATION
C              AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
               if (cpolish(1:3) .eq. 'and') then
                  iopflg=1
                  istk1=stkptr
                  istk2=stkptr-1
                  stkptr=stkptr-1
                  do 20 ip=1,i2
                     l1=stack(ip,istk1)
                     l2=stack(ip,istk2)
                     stack(ip,stkptr)=(l1 .and. l2)
   20             continue
               endif
C
C              .........................................................
C              IF THE TOKEN IS THE or OPERATOR, PERFORM THE OPERATION
C              AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
               if (cpolish(1:2) .eq. 'or') then
                  iopflg=1
                  istk1=stkptr
                  istk2=stkptr-1
                  stkptr=stkptr-1
                  do 25 ip=1,i2
                     l1=stack(ip,istk1)
                     l2=stack(ip,istk2)
                     stack(ip,stkptr)=(l1 .or. l2)
   25             continue
               endif
C
C              .........................................................
C              IF THE TOKEN IS THE not OPERATOR, PERFORM THE OPERATION
C              AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
               if (cpolish(1:3) .eq. 'not') then
                  iopflg=1
                  do 30 ip=1,i2
                     l1=stack(ip,stkptr)
                     stack(ip,stkptr)=(.not. l1)
   30             continue
               endif
C
C              .........................................................
C              IF THE TOKEN IS THE LEFT OR RIGHT PARENTHESIS, SKIP IT.
C
               if (cpolish(1:1) .eq. '(' .or. cpolish(1:1) .eq. ')')
     *            iopflg=1
C
C              .........................................................
C              IF NOT AN OPERATOR, THEN IT MUST BE AN ARRAY ID, EVALUATE
C              AND ADD TO THE EVALUATION STACK.
C
               if (iopflg .eq. 0) then
                  read(cpolish,'(i10)') ns
C******           ns=polish(i)
C
C                 ......................................................
C                 LOOP THROUGH isall TO FIND MATCHING SURFACE POINTERS.
C                 THEN TEST WITH SURFTST.
C
                  do 35 ii=1,nsurf
                     iword1=isurfnm(ns)
                     iword2=csall(ii)
                     len1=icharlnf(iword1)
                     len2=icharlnf(iword2)
                     len0=max(len1,len2)
                     if (iword1(1:len0) .eq. iword2(1:len0)) is=ii
   35             continue
                  write(itest,'(a2)') iltgt(ns)
                  call surftstv(x(jp),y(jp),z(jp),i2,epsln,cmo,
     *               istype(is),surfparam(offsparam(is)+1),
     *               sheetnm(is),itest,isurftst)
                  stkptr=stkptr+1
                  do 40 ip=1,i2
                     stack(ip,stkptr)=.false.
                     if (isurftst(ip) .eq. 1) then
                        stack(ip,stkptr)=.true.
                        isurfno(jp-1+ip)=is
                     endif
c$$$                     if (isurftst(ip) .eq. 1) stack(ip,stkptr)=.true.
   40             continue
               endif
C
   50       continue
C
C           ............................................................
C           SEE IF THE POINTS EXIST IN THIS REGION
C
            do 60 ip=1,i2
               regck=stack(ip,stkptr)
C
C              .........................................................
C              SEE IF THIS POINT EXISTS IN MULTIPLE REGIONS
C
               if (regck) then
                  nregdup(ip)=nregdup(ip)+1
                  if (nregdup(ip) .le. 20)
     &               iregdup(ip,nregdup(ip))=cregs(ir)
               endif
C
C              .........................................................
C              SAVE REGION NUMBER.
C
               if (regck) iregno(jp+ip-1)=ir
C
   60       continue
C
  100    continue
C
C        ---------------------------------------------------------------
C        CHECK FOR MULTIPLE REGIONS IN mregion TYPE.
C
         if (irtype(1:icharlnf(irtype)) .eq. 'mregion') then
            do 120 ip=1,i2
               if (nregdup(ip) .gt. 1) then
                  iptno=jp+ip-1
                  write(logmess,9000) nregdup(ip),iptno
 9000             format('  ERROR, ',i3,
     &                   ' OVERLAPPING MREGIONS AT POINT NO. ',i8)
                  call writloga('default',1,logmess,0,ierrw)
                  write(logmess,9001) x(iptno),y(iptno),z(iptno)
 9001             format(10x,'(x,y,z) = (',1pe14.7,',',1pe14.7,',',
     &                   1pe14.7,')')
                  call writloga('default',0,logmess,0,ierrw)
                  write(logmess,9002)
 9002             format(5x,'THE OVERLAPPING REGIONS ARE:')
                  call writloga('default',0,logmess,0,ierrw)
                  nwrite=min(nregdup(ip),20)
                  do 110 i=1,nwrite
                     write(logmess,9003) iregdup(ip,i)
 9003                format(10x,a8)
                     call writloga('default',0,logmess,0,ierrw)
  110             continue
               endif
  120       continue
         endif
C
  150 continue
c
 9999 continue
C
C     ******************************************************************
C
      call mmrelprt(isubname,icscode)
      return
      end
