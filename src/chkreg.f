      subroutine chkreg(x,y,z,epsln,irtype,
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
C        x - X COORDINATE OF THE POINT TO CHECK
C        y - Y COORDINATE OF THE POINT TO CHECK
C        z - Z COORDINATE OF THE POINT TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        irtype - TYPE OF REGION TO CHECK (region or mregion)
C        regdata - region definition
C        ndef- number of tokens in region definition
c
C     OUTPUT ARGUMENTS -
C
C        iregloc - RETURNS 'in', 'on' OR 'out'
C        ierr - ERROR FLAG
C
C     CHANGE HISTORY -
C
C        $Log: chkreg.f,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Fri Apr 07 10:42:02 2000   dcg
CPVCS    remove machine.h
CPVCS    
CPVCS       Rev 1.3   Fri Apr 07 10:07:52 2000   dcg
CPVCS    replace use of KNWPN for length calculation with mmgetblk type 3
CPVCS    
CPVCS       Rev 1.2   Wed Feb 02 13:03:22 2000   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:36   dcg
CPVCS    
CPVCS       Rev 1.0   04 Jan 2000 16:47:20   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.6   Fri Jan 22 09:50:34 1999   dcg
CPVCS    remove duplicate and unused declarations
CPVCS
CPVCS       Rev 1.5   Mon Nov 24 16:37:36 1997   dcg
CPVCS     use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:39:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   11/07/95 17:15:26   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.2   05/01/95 08:34:24   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.1   01/09/95 17:31:36   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:12   pvcs
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
      integer  ierr,length,icscode, nxsurf,npol,iout,lout,itype,
     *  nltgt,nstr,notflg, nparen,idefreg,ndef,ir,ierrp,
     *   i,iopflg,len0,ns,ierror,
     *   len1,len2,notck,lschk,ii,is
      real*8 x,y,z,epsln,rout
      pointer(ipout,out)
      real*8 out(*)
      logical surftst,stack(500),l1,l2,regck
      integer stkptr
C
      character*32 isubname, iregloc, cmo, regdata(*)
      character*32 irtype, iword, iword1, iword2,geom_name,
     *             cpolish, ischk
C
      pointer(ipiltgt, iltgt)
      pointer(ipsurfnm, isurfnm)
      pointer(ipnewdef, newdef)
      pointer(ippolish, polish)
      character*32 iltgt(*),isurfnm(*)
     *  ,newdef(*),polish(*)
      integer icharlnf
C
C#######################################################################
C
      isubname='chkreg'
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
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
      iregloc='out'
      ierr=0
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
      length=maxdef
      if (irtype(1:7).eq.'mregion') length=maxmdef
      call mmgetblk('iltgt',isubname,ipiltgt,length,3,icscode)
      call mmgetblk('surfnm',isubname,ipsurfnm,length,3,icscode)
      call mmgetblk('newdef',isubname,ipnewdef,length,3,icscode)
      call mmgetblk('polish',isubname,ippolish,length,3,icscode)
c
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
         iword=regdef(i)
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
C     FIRST RESET THE REGION DEFINITION TO SEE IF THE POINT IS INSIDE
C     THE REGION.  IF NOT INSIDE, RESET THE DEFINITION TO INCLUDE THE
C     SURFACES AND CHECK AGAIN TO SEE IF IT IS ON THE BOUNDARY.
C
      do 50 ir=1,2
C
         if (iregloc(1:2) .eq. 'in') go to 50
C
C        ---------------------------------------------------------------
C        EVALUATE THE REVERSE POLISH STACK
C        LOOP THROUGH THE NO. OF TOKENS IN THE POLISH STACK
C
         stkptr=0
         do 20 i=1,npol
            iopflg=0
            cpolish=polish(i)
            len0=icharlnf(cpolish)
C
C           ............................................................
C           IF THE TOKEN IS THE and OPERATOR, PERFORM THE OPERATION
C           AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
            if (cpolish(1:len0) .eq. 'and') then
               iopflg=1
               l1=stack(stkptr)
               l2=stack(stkptr-1)
               stkptr=stkptr-1
               stack(stkptr)=(l1 .and. l2)
            endif
C
C           ............................................................
C           IF THE TOKEN IS THE or OPERATOR, PERFORM THE OPERATION
C           AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
            if (cpolish(1:len0) .eq. 'or') then
               iopflg=1
               l1=stack(stkptr)
               l2=stack(stkptr-1)
               stkptr=stkptr-1
               stack(stkptr)=(l1 .or. l2)
            endif
C
C           ............................................................
C           IF THE TOKEN IS THE not OPERATOR, PERFORM THE OPERATION
C           AND REPLACE THE RESULT IN THE EVALUATION STACK.
C
            if (cpolish(1:len0) .eq. 'not') then
               iopflg=1
               l1=stack(stkptr)
               stack(stkptr)=(.not. l1)
            endif
C
C           ............................................................
C           IF THE TOKEN IS THE LEFT OR RIGHT PARENTHESIS, SKIP IT.
C
            if (cpolish(1:len0) .eq. '(' .or. cpolish(1:len0) .eq. ')')
     &         iopflg=1
C
C           ............................................................
C           IF NOT AN OPERATOR, THEN IT MUST BE AN ARRAY ID, EVALUATE
C           AND ADD TO THE EVALUATION STACK.
C
            if (iopflg .eq. 0) then
C
               read(cpolish,'(i10)') ns
               notck=ns
               ns=iabs(ns)
               ischk=iltgt(ns)
               lschk=icharlnf(ischk)
C
C              .........................................................
C              LOOP THROUGH isall TO FIND MATCHING SURFACE POINTERS.
C
               do 15 ii=1,nsurf
                  iword1=isurfnm(ns)
                  iword2=csall(ii)
                  len1=icharlnf(iword1)
                  len2=icharlnf(iword2)
                  len0=max(len1,len2)
                  if (iword1(1:len0) .eq. iword2(1:len0)) is=ii
 15            continue
C
C              .........................................................
C              FIRST TIME THROUGH, RESET TO EXCLUDE SURFACES.
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
C              .........................................................
C              SECOND TIME THROUGH, RESET TO INCLUDE SURFACES.
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
               l1=surftst(x,y,z,epsln,cmo,istype(is),
     *           surfparam(offsparam(is)+1),sheetnm(is),ischk)
               stkptr=stkptr+1
               stack(stkptr)=l1
C
            endif
C
   20    continue
C
         regck=stack(stkptr)
C
C        ---------------------------------------------------------------
C        SET RETURN PARAMETER.
C
         if (ir .eq. 1 .and. regck) iregloc='in'
         if (ir .eq. 2 .and. regck) iregloc='on'
C
C        ---------------------------------------------------------------
C
   50 continue
c
 9999 continue
C
C     ******************************************************************
C
      call mmrelprt(isubname,icscode)
      return
      end
