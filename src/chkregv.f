      subroutine chkregv(x,y,z,npts,epsln,irtype,
     &                   regdata,ndef,
     &                   iregloc,ierr)
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE CHECK WHETHER THE POINT (x,y,z) LIES INSIDE, ON
C     THE SURFACE OR OUTSIDE THE REGION iregck.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE ARRAY OF THE POINTS TO CHECK
C        y - Y COORDINATE ARRAY OF THE POINTS TO CHECK
C        z - Z COORDINATE ARRAY OF THE POINTS TO CHECK
C        npts - NO. OF POINTS TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        irtype - region or mregion
C        regdata - region definition
C        ndef- number of tokens in region definition
c
C     OUTPUT ARGUMENTS -
C
C        iregloc - RETURNS 1=IN, 2=ON OR 3=OUT
C        ierr - ERROR FLAG
C
C     CHANGE HISTORY -
C
C        $Log: chkregv.f,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Fri Apr 07 10:10:42 2000   dcg
CPVCS    replace use of KNWPN for length calculation with mmgetblk type 3
CPVCS    remove machine.h
CPVCS    
CPVCS       Rev 1.3   Tue Feb 08 16:31:20 2000   dcg
CPVCS    
CPVCS       Rev 1.2   Wed Feb 02 13:06:04 2000   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:38   dcg
CPVCS    
CPVCS       Rev 1.0   04 Jan 2000 16:47:22   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.6   Fri Jan 22 09:52:00 1999   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.5   Mon Nov 24 16:30:44 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:39:44 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   11/07/95 17:15:30   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.2   05/01/95 08:34:28   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.1   01/09/95 17:32:24   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:14   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C
      include 'geom_lg.h'
C
C#######################################################################
C
      integer npts,ierr,length,icscode, nxsurf,npol,
     *  nltgt,nstr,notflg, nparen,idefreg,ndef,jp,ir,ierrp,
     *   i,i2,iopflg,len0,istk1,istk2,ip,ns,ierror,
     *   len1,len2,notck,lschk,ii,is,iout,lout,itype
      real*8 x(npts),y(npts),z(npts),epsln,rout
      pointer(ipout,out)
      real*8 out(*)
C
      integer isurftst(512),ickloc(512,2),iregloc(npts)
      logical stack(512,20),l1,l2,regck
      integer stkptr
      integer icharlnf
C
      character*32 isubname,irtype
      character*32  iword, iword1, iword2, geom_name,
     *             cpolish, ischk,cmo,regdata(*)
      pointer(ipiltgt, iltgt)
      pointer(ipsurfnm, isurfnm)
      pointer(ipnewdef, newdef)
      pointer(ippolish, polish)
      character*32 iltgt(*),isurfnm(*)
     *  ,newdef(*),polish(*)
C
C#######################################################################
C
      isubname='chkregv'
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     GET THE SURFACE NAMES AND OFFSETS FOR ALL SURFACES.
C
      if(nsurf.gt.0) then
         call mmfindbk('csall',geom_name,ipcsall,length,ierror)
         call mmfindbk('istype',geom_name,ipistype,length,ierror)
         call mmfindbk('ibtype',geom_name,ipibtype,length,ierror)
         call mmfindbk('sheetnm',geom_name,ipsheetnm,length,ierror)
         call mmfindbk('surfparam',geom_name,ipsurfparam,length,ierror)
         call mmfindbk('offsparam',geom_name,ipoffsparam,length,ierror)
      endif
C
C     ******************************************************************
C     INITIALIZE THE RETURN VARIABLES.
C
      do  i=1,npts
         iregloc(i)=3
      enddo
      ierr=0
      if(ndef.le.0) go to 9999
C
C     ******************************************************************
C     MAKE SURE WE HAVE REGIONS AND SURFACES.
C
      if ( (nregs.le.0.and.irtype(1:7).eq.'region') .or.
     *   (nmregs.le.0.and.irtype(1:7).eq.'mregion')
     *      .or. nsurf.le.0) then
         ierr=1
         go to 9999
      endif
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
C     LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C     SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C     ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C     AND LOGICAL OPERATORS(and,or,not).
C
      nxsurf=0
      nltgt=0
      nstr=0
      notflg=0
      nparen=0
      idefreg=1
      do 10 i=1,ndef
         iword=regdata(i)
         idefreg=idefreg+1
         len0=icharlnf(iword)
C
C        ---------------------------------------------------------------
C        CHECK FOR not AND ITS EXTENT OF INFLUENCE
C
         if (iword(1:len0) .eq. 'not') notflg=1
         if (iword(1:len0).eq.'(' .and. notflg.gt.0) nparen=nparen+1
         if (iword(1:len0).eq.')' .and. notflg.gt.0) then
            nparen=nparen-1
            if (nparen .eq. 0) notflg=0
         endif
C
C        ---------------------------------------------------------------
C        SAVE OPERATOR AND INDEX, SAVE NEGATIVE INDEX IF UNDER not
C
         if (iword(1:len0) .eq. 'lt' .or. iword(1:len0) .eq. 'gt' .or.
     &       iword(1:len0) .eq. 'le' .or. iword(1:len0) .eq. 'ge') then
            nxsurf=i+1
            nltgt=nltgt+1
            iltgt(nltgt)=iword
            nstr=nstr+1
            newdef(nstr)=' '
            write(newdef(nstr),'(i8)') nltgt
            if (notflg .gt. 0) write(newdef(nstr),'(i8)') -nltgt
            if (notflg.gt.0 .and. nparen.eq.0) notflg=0
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
   10 continue
C
C     ******************************************************************
C     CALL eorpt TO GET A REVERSE POLISH STACK OF THE NEW DEFINITION
C     IN CORRECT PRIORITY ORDER
C
      call eorpt(newdef,nstr,polish,npol,ierrp)
C
C     ******************************************************************
C
C     LOOP THROUGH POINTS IN GROUPS OF 512.
C
      do 150 jp=1,npts,512
C
         i2=512
         if ((jp+i2) .gt. npts) i2=npts-jp+1
C
C        ---------------------------------------------------------------
C        FIRST RESET THE REGION DEFINITION TO SEE IF THE POINT IS INSIDE
C        THE REGION.  IF NOT INSIDE, RESET THE DEFINITION TO INCLUDE THE
C        SURFACES AND CHECK AGAIN TO SEE IF IT IS ON THE BOUNDARY.
C
         do 100 ir=1,2
C
C           ............................................................
C           INITIALIZE CHECK ARRAY
C
            do 15 ip=1,i2
               ickloc(ip,ir)=0
   15       continue
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
               len0=icharlnf(cpolish)
C
C              .........................................................
C              IF THE TOKEN IS THE and OPERATOR, PERFORM THE OPERATION
C              AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
               if (cpolish(1:len0) .eq. 'and') then
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
               if (cpolish(1:len0) .eq. 'or') then
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
               if (cpolish(1:len0) .eq. 'not') then
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
               if (cpolish(1:len0) .eq. '(' .or.
     *             cpolish(1:len0) .eq. ')') then
                  iopflg=1
               endif
C
C              .........................................................
C              IF NOT AN OPERATOR, THEN IT MUST BE AN ARRAY ID, EVALUATE
C              AND ADD TO THE EVALUATION STACK.
C
               if (iopflg .eq. 0) then
                  read(cpolish,'(i10)') ns
                  notck=ns
                  ns=iabs(ns)
                  ischk=iltgt(ns)
                  lschk=icharlnf(ischk)
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
C
C                 ......................................................
C                 FIRST TIME THROUGH, RESET TO EXCLUDE SURFACES.
C
                  if (ir .eq. 1) then
                     if (ischk(1:lschk) .eq. 'le') ischk='lt'
                     if (ischk(1:lschk) .eq. 'ge') ischk='gt'
                     if (notck.lt.0 .and. ischk(1:lschk).eq.'lt') then
                        ischk='le'
                     endif
                     if (notck.lt.0 .and. ischk(1:lschk).eq.'gt') then
                        ischk='ge'
                     endif
                  endif
C
C                 ......................................................
C                 SECOND TIME THROUGH, RESET TO INCLUDE SURFACES.
C
                  if (ir .eq. 2) then
                     if (ischk(1:lschk) .eq. 'lt') ischk='le'
                     if (ischk(1:lschk) .eq. 'gt') ischk='ge'
                     if (notck.lt.0 .and. ischk(1:lschk).eq.'le') then
                        ischk='lt'
                     endif
                     if (notck.lt.0 .and. ischk(1:lschk).eq.'ge') then
                        ischk='gt'
                     endif
                  endif
C
                  call surftstv(x(jp),y(jp),z(jp),i2,epsln,
     *                cmo,istype(is),
     *                surfparam(offsparam(is)+1),
     &                sheetnm(is),ischk,isurftst)
                  stkptr=stkptr+1
                  do 40 ip=1,i2
                     stack(ip,stkptr)=.false.
                     if (isurftst(ip) .eq. 1) stack(ip,stkptr)=.true.
   40             continue
               endif
C
   50       continue
C
C           ............................................................
C           SET CHECK ARRAYS
C
            do 60 ip=1,i2
               regck=stack(ip,stkptr)
               if (regck) ickloc(ip,ir)=1
   60       continue
C
  100    continue
C
C        ---------------------------------------------------------------
C        SET RETURN PARAMETER ARRAY
C        IF FIRST PASS TRUE, POINT IS IN
C        IF SECOND PASS TRUE, POINT IS ON
C
         do 110 ip=1,i2
            if (ickloc(ip,1) .eq. 1) iregloc(jp+ip-1)=1
            if (ickloc(ip,1) .eq. 0 .and.
     &          ickloc(ip,2) .eq. 1) iregloc(jp+ip-1)=2
  110    continue
C
C        ---------------------------------------------------------------
C
  150 continue
c
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
