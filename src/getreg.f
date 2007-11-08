*dk,getreg
      subroutine getreg(x,y,z,epsln,irtype,iregck,
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
C        x - X COORDINATE OF THE POINT TO CHECK
C        y - Y COORDINATE OF THE POINT TO CHECK
C        z - Z COORDINATE OF THE POINT TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        irtype - TYPE OF REGION TO CHECK (region or mregion)
C        iregck - PARTICULAR REGION TO CHECK AGAINST (0=ALL REGIONS)
C
C
C     OUTPUT ARGUMENTS -
C
C        iregno - REGION NO. THAT THE POINT LIES IN
C        isurfno - SURFACE NO. THAT THE POINT LIES ON
C        ierr - ERROR FLAG
C
C
C     CHANGE HISTORY -
C
C        $Log: getreg.f,v $
C        Revision 2.00  2007/11/05 19:45:57  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Fri Apr 07 10:11:46 2000   dcg
CPVCS    replace use of KNWPN for length calculation with mmgetblk type 3
CPVCS    remove machine.h
CPVCS    
CPVCS       Rev 1.2   Wed Feb 02 12:57:50 2000   dcg
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:47:58   dcg
CPVCS    
CPVCS       Rev 1.0   04 Jan 2000 16:47:38   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.7   Fri Jan 22 12:08:18 1999   dcg
CPVCS    remove duplicate and unused declarations
CPVCS
CPVCS       Rev 1.6   Mon Nov 24 16:33:36 1997   dcg
CPVCS    use geom.h and calls to get_regions, get_mregions, get_surfaces
CPVCS    to access geometry data - start to isolate integer*8 dependencies
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:49:58 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   11/07/95 17:18:16   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.3   05/11/95 19:53:08   het
CPVCS    Comment out the termgen() call
CPVCS
CPVCS       Rev 1.2   05/01/95 08:34:30   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.1   01/09/95 17:32:26   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:30   pvcs
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
      character*8 iregdup(50)
      logical surftst,stack(500),l1,l2,regck
      integer stkptr
C
      character*32 irtype, isubname, iword, iword1, iword2,
     *             cpolish, ick, cmo,geom_name
      pointer(ipout,out)
      real*8 out(*)
C
      pointer(ipiltgt, iltgt)
      pointer(ipsurfnm, isurfnm)
      pointer(ipnewdef, newdef)
      pointer(ippolish, polish)
      character*32 iltgt(10000000),isurfnm(10000000),
     *  newdef(10000000),polish(10000000)
      integer icharlnf
      integer ierr,length,icscode,irstrt,irend,iregck,iout,lout,itype,
     *   ir,ndef,nltgt,nstr,idefreg,len0, len1,len2,nxsurf,i,
     *   npol,ierrp,iopflg,ns,ii,iregno,isurfno,ierror,ioff,
     *   nregdup,is
      real*8 x,y,z,epsln,rout
C
C#######################################################################
C
      isubname='getreg'
      call cmo_get_name(cmo,ierror)
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_name,
     *                        ipout,lout,itype,ierror)
C
C     ******************************************************************
C
C     INITIALIZE THE RETURN VARIABLES.
C
      iregno=0
      isurfno=0
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
      nregdup=0
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
      if(irtype(1:7).eq.'mregion') irend=nmregs
      if (iregck .gt. 0) then
         irstrt=iregck
         irend=iregck
      endif
C
C     ******************************************************************
C
C     LOOP THROUGH REGION DEFINITIONS.
C
      do 50 ir=irstrt,irend
C
C        ---------------------------------------------------------------
C        SET THE REGION DEFINITION POINTER AND NO. ELEMENTS FROM iregs
C
         ioff=offregdef(ir)
         ndef=ndefregs(ir)
C
C        ---------------------------------------------------------------
C        LOOP THROUGH THE ELEMENTS OF THE REGION DEFINITION, SAVE
C        SURFACE OPERATOR(lt,gt), AND SURFACE NAME IN CORRESPONDING
C        ARRAYS.  BUILD A NEW DEFINITION LIST WITH THE ARRAY INDEX
C        AND LOGICAL OPERATORS(and,or,not).
C
         nxsurf=0
         nltgt=0
         nstr=0
         idefreg=1
         do 10 i=1,ndef
            iword=regdef(ioff+i)
            idefreg=idefreg+1
            len0=icharlnf(iword)
            if (iword(1:len0) .eq. 'lt' .or.
     &          iword(1:len0) .eq. 'gt' .or.
     &          iword(1:len0) .eq. 'le' .or.
     &          iword(1:len0) .eq. 'ge') then
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
   10    continue
C
C        ---------------------------------------------------------------
C        CALL eorpt TO GET A REVERSE POLISH STACK OF THE NEW DEFINITION
C        IN CORRECT PRIORITY ORDER
C
         call eorpt(newdef,nstr,polish,npol,ierrp)
C
C        ---------------------------------------------------------------
C        EVALUATE THE REVERSE POLISH STACK
C        LOOP THROUGH THE NO. OF TOKENS IN THE POLISH STACK
C
         stkptr=0
C
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
               read(cpolish,'(i10)') ns
C
C              .........................................................
C              LOOP THROUGH isall TO FIND MATCHING SURFACE POINTERS.
C              THEN TEST WITH SURFTST.
C
               do 15 ii=1,nsurf
                  iword1=isurfnm(ns)
                  iword2=csall(ii)
                  len1=icharlnf(iword1)
                  len2=icharlnf(iword2)
                  len0=max(len1,len2)
                  if (iword1(1:len0) .eq. iword2(1:len0)) is=ii
   15          continue
               l1=surftst(x,y,z,epsln,cmo,istype(is),
     *            surfparam(offsparam(is)+1),sheetnm(is),iltgt(ns))
               stkptr=stkptr+1
               stack(stkptr)=l1
            endif
C
   20    continue
C
         regck=stack(stkptr)
C
C        ---------------------------------------------------------------
C        SEE IF THIS POINT EXISTS IN MULTIPLE REGIONS
C
         if (regck) then
            nregdup=nregdup+1
            if (nregdup .le. 50) iregdup(nregdup)=cregs(ir)
         endif
C
C        ---------------------------------------------------------------
C        SAVE REGION NUMBER.
C
         if (regck) iregno=ir
C
C        ---------------------------------------------------------------
C
   50 continue
C
C     ******************************************************************
C     CHECK FOR MULTIPLE REGIONS IN mregion TYPE.
C
      if (irtype .eq. 'mregion' .and. nregdup .gt. 1) then
         write(logmess,9000) nregdup,x,y,z
 9000    format('  ERROR, ',i3,' OVERLAPPING MREGIONS AT POINT (',
     &          e14.7,',',e14.7,',',e14.7,')')
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9001)
 9001    format(5x,'THE OVERLAPPING REGIONS ARE:')
         call writloga('default',0,logmess,0,ierr)
         do 60 i=1,nregdup
            write(logmess,9002) iregdup(i)
 9002       format(10x,a8)
            call writloga('default',0,logmess,0,ierr)
   60    continue
C********call termgen(1)
      endif
C
C     ******************************************************************
C     IF A POINT IS NOT IN A REGION, SEE IF IT IS ON A SURFACE.
C
      if (iregno .eq. 0) then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH THE SURFACES
C
         ick='eq      '
         do 100 is=1,nsurf
            if (surftst(x,y,z,epsln,cmo,istype(is),
     *        surfparam(offsparam(is)+1),sheetnm(is),ick)) isurfno=is
  100    continue
C
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
      call mmrelprt(isubname,icscode)
      return
      end
