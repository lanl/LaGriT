      subroutine mregion(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE STORES THE MATERIAL REGION DEFINITIONS INTO THE
C        MISCELLANEOUS STORAGE BLOCK UNDER MREGION.
C
C        FORMAT: MREGION/IMD/mregion definition data
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: mregion.f,v $
C        Revision 2.00  2007/11/05 19:46:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.12   18 Apr 2000 13:24:44   dcg
CPVCS    implement release option for surfaces, regions and mregions
CPVCS    
CPVCS       Rev 1.11   20 Mar 2000 13:42:32   dcg
CPVCS    check for duplicate name - if so then print warning and skip command
CPVCS    
CPVCS       Rev 1.10   Thu Feb 24 12:13:36 2000   dcg
CPVCS    use character mm arrays for region, mregion definitions
CPVCS    fix tests for incrementing memory
CPVCS    
CPVCS       Rev 1.9   24 Feb 2000 11:17:04   dcg
CPVCS    use type=3 for character arrays
CPVCS    
CPVCS       Rev 1.8   Thu Feb 24 09:55:34 2000   dcg
CPVCS    use correct offset when using region definition as
CPVCS    part of the mregion definition
CPVCS    
CPVCS       Rev 1.7   07 Feb 2000 08:23:36   dcg
CPVCS    
CPVCS       Rev 1.6   Thu Feb 03 13:41:50 2000   dcg
CPVCS    
CPVCS       Rev 1.5   02 Feb 2000 17:37:24   dcg
CPVCS    
CPVCS       Rev 1.4   13 Jan 2000 14:48:12   dcg
CPVCS    
CPVCS       Rev 1.3   06 Jan 2000 12:55:08   dcg
CPVCS
CPVCS       Rev 1.18   Mon Oct 18 16:40:08 1999   dcg
CPVCS    initialize ndefreg, nstbreg
CPVCS
CPVCS       Rev 1.17   Fri Oct 31 10:48:50 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.16   Mon Apr 14 16:54:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.15   11/07/95 17:21:02   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.14   08/29/95 11:51:14   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.13   08/23/95 06:57:02   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.12   08/22/95 06:50:08   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.11   06/20/95 15:44:36   dcg
CPVCS    remove character literal from argument list to savpart
CPVCS
CPVCS       Rev 1.10   06/19/95 16:43:42   dcg
CPVCS    add blank after literal in calling sequence to savpart
CPVCS
CPVCS       Rev 1.9   06/13/95 09:02:02   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.8   06/07/95 15:30:14   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.7   05/01/95 08:33:36   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.6   03/31/95 09:08:24   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.5   03/30/95 05:00:16   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.4   03/23/95 15:07:24   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.3   03/06/95 15:33:54   dcg
CPVCS     Add length field to character comparisons
CPVCS
CPVCS       Rev 1.2   01/09/95 17:31:44   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:12   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:16:20   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include 'geom_lg.h'
C
C ######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
      integer icharlnf
      integer i,iregno,lout,iout,itype,n,j,
     *  nleftp,nrightp,ierrw,ir,irok,len2,is,len1,isok,length,
     *  ier,rdeflen,lentoken
      pointer(ipout,out)
      real*8 out(*),rout
C
C ######################################################################
C
      character*132 logmess
C
      character*32 ctemp,iregnam,geom_nm
      character*32 isubname,cmo
c
C#######################################################################
C
      isubname='mregion'
      ierror = 0
 
C     ******************************************************************
C  get mesh object name
      call cmo_get_name(cmo,ier)
      if(ier.ne.0) call x3d_error('mregion','cmo_get_name')
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geom_nm,
     *                        ipout,lout,itype,ierror)
 
c
c  look for surface data pointers
c
      call mmfindbk('csall',geom_nm,ipcsall,length,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)') 'Mesh has no surfaces '
         call writloga('default',0,logmess,0,ierror)
         go to 9999
      endif
c
c  get mregion data pointers if they exist otherwise make them
c  find region name pointer
c
      if (nregs.ne.0) then
         call mmfindbk('cregs',geom_nm,ipcregs,length,ierror)
         call mmfindbk('offregdef',geom_nm,ipoffregdef,length,ierror)
         call mmfindbk('ndefregs',geom_nm,ipndefregs,length,ierror)
         call mmfindbk('regdef',geom_nm,ipregdef,length,ierror)
      endif
      call mmfindbk('cmregs',geom_nm,ipcmregs,length,ierror)
      if(ierror.ne.0) then
         nmregs=0
         lastmregdef=0
         length=100
         maxmdef=0
         call mmgetblk('cmregs',geom_nm,ipcmregs,length,3,ierror)
         call mmgetblk('offmregdef',geom_nm,ipoffmregdef,length,1,
     *    ierror)
         call mmgetblk('ndefmregs',geom_nm,ipndefmregs,length,1,ierror)
         call mmgetblk('mregdef',geom_nm,ipmregdef,10*length,3,ierror)
         call mmgetblk('matregs',geom_nm,ipmatregs,length,1,ierror)
      elseif ( length.lt.(nmregs+1)*4) then
         call mmincblk('cmregs',geom_nm,ipcmregs,length,ierror)
         call mmincblk('offmregdef',geom_nm,ipoffmregdef,length,ierror)
         call mmincblk('ndefmregs',geom_nm,ipndefmregs,length,ierror)
         call mmincblk('mregdef',geom_nm,ipmregdef,10*length,ierror)
         call mmincblk('matregs',geom_nm,ipmatregs,length,ierror)
      else
         call mmfindbk('cmregs',geom_nm,ipcmregs,length,ierror)
         call mmfindbk('offmregdef',geom_nm,ipoffmregdef,length,ierror)
         call mmfindbk('ndefmregs',geom_nm,ipndefmregs,length,ierror)
         call mmfindbk('mregdef',geom_nm,ipmregdef,length,ierror)
         call mmfindbk('matregs',geom_nm,ipmatregs,length,ierror)
      endif
c
c check if operation is release
c
      if(cmsgin(3).eq.'release'.or.cmsgin(3).eq.'delete'.or.
     *   cmsgin(3).eq.'remove') then
         do i=1,nmregs
            if(cmregs(i).eq.cmsgin(2)) then
               if(i.lt.nmregs) then
                  length=offmregdef(i+1)-offmregdef(i)
                  do j=offmregdef(i),lastmregdef-length
                      mregdef(j+1)=mregdef(j+length+1)
                  enddo
                  do j=i+1,nmregs
                     offmregdef(j)=offmregdef(j)-length
                  enddo
                  do j=i,nmregs-1
                     cmregs(j)=cmregs(j+1)
                     ndefmregs(j)=ndefmregs(j+1)
                     offmregdef(j)=offmregdef(j+1)
                  enddo
                  lastmregdef=lastmregdef-length
               else
                  lastmregdef=0
                  if(nmregs.gt.1)lastmregdef=offmregdef(nmregs)
                  offmregdef(nmregs)=0
                  cmregs(nmregs)=' '
                  ndefmregs(nmregs)=0
               endif
               nmregs=nmregs-1
               go to 9999
            endif
         enddo
      endif
c
c   check for duplicate mregion - if so warn and skip
c
      n=nmregs+1
      do i=1,n
         if(cmsgin(2).eq.cmregs(i)) then
           write(logmess,'(a,a)') 'duplicate mregion - ignored: ',
     *            cmsgin(2)
           call writloga('default',0,logmess,0,ierror)
           go to 9999
        endif
      enddo
 
 
C     ******************************************************************
C     CHECK INPUT DATA, IF ELEMENT IS NOT AN OPERATOR, CHECK SURFACE
C     AND REGION NAMES
C
         nmregs=nmregs+1
      do 5 i=3,nwds
         len1=icharlnf(cmsgin(i))
         ctemp=' '
         ctemp(1:len1)=cmsgin(i)(1:len1)
         if (ctemp(1:len1).ne.'lt' .and. ctemp(1:len1).ne.'gt' .and.
     &       ctemp(1:len1).ne.'le' .and. ctemp(1:len1).ne.'ge' .and.
     &       ctemp(1:len1).ne.'or' .and. ctemp(1:len1).ne.'and' .and.
     &       ctemp(1:len1).ne.'not' .and. ctemp(1:len1).ne.'(' .and.
     &       ctemp(1:len1).ne.')') then
            isok=0
            if (nsurf .gt. 0) then
               do 3 is=1,nsurf
                  len2=max(len1,icharlnf(csall(is)))
                  if (ctemp(1:len2) .eq. csall(is)(1:len2)) isok=1
    3          continue
            endif
            irok=0
            if (nregs .gt. 0) then
               do 4 ir=1,nregs
                  len2=max(len1,icharlnf(cregs(ir)))
                  if (ctemp(1:len2) .eq. cregs(ir)(1:len2)) irok=1
    4          continue
            endif
C
            if (isok.eq.0 .and. irok.eq.0) then
               ierror=1
               write(logmess,9000) ctemp
 9000          format(' MREGION ERROR: ',a8,' IS AN INVALID ',
     &                'OPERATOR, OR A NON-DEFINED SURFACE OR REGION')
               call writloga('default',0,logmess,0,ierrw)
               go to 9999
            endif
C
         endif
C
    5 continue
C
C     ******************************************************************
C     CHECK INPUT DATA FOR UNBALANCED PARENTHESIS
C
      nleftp=0
      nrightp=0
      do 6 i=3,nwds
         len1=icharlnf(cmsgin(i))
         ctemp=' '
         ctemp(1:len1)=cmsgin(i)(1:len1)
         if (ctemp(1:len1) .eq. '(') nleftp=nleftp+1
         if (ctemp(1:len1) .eq. ')') nrightp=nrightp+1
    6 continue
C
      if (nleftp .ne. nrightp) then
         ierror=1
         write(logmess,9001)
 9001    format(' MREGION ERROR: UNBALANCED PARENTHESIS')
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C     ******************************************************************
C     GET MREGION NAME
C
      len1=icharlnf(cmsgin(2))
      iregnam=' '
      iregnam(1:len1)=cmsgin(2)(1:len1)
C
C     ******************************************************************
C     SAVE mregion name and offset and definition
C
      cmregs(nmregs)=iregnam(1:len1)
      offmregdef(nmregs)=lastmregdef
      ndefmregs(nmregs)=0
c
      do i=3,nwds
C
C        ---------------------------------------------------------------
C        SEE IF THIS ELEMENT IS A REGION NAME
C
         iregno=0
         lentoken=icharlnf(cmsgin(i))
         if (nregs .gt. 0) then
            call mmfindbk('cregs',geom_nm,ipcregs,length,ierror)
            do 10 ir=1,nregs
               len2=max(lentoken,icharlnf(cregs(ir)))
               if (cmsgin(i)(1:len2) .eq. cregs(ir)(1:len2)) iregno=ir
   10       continue
         endif
C
C        ---------------------------------------------------------------
C        CHECK THAT THIS IS NOT A SURFACE WITH THE SAME NAME
C
         if (iregno .gt. 0) then
            len1=icharlnf(cmsgin(i-1))
            ctemp=' '
            ctemp(1:len1)=cmsgin(i-1)(1:len1)
            if (ctemp(1:len1).eq.'lt' .or. ctemp(1:len1).eq.'gt' .or.
     &          ctemp(1:len1).eq.'le' .or. ctemp(1:len1).eq.'ge')
     &          iregno=0
         endif
c
c  get length of array of mregion definitions
c
         call mmfindbk('mregdef',geom_nm,ipmregdef,rdeflen,ierror)
C
C        ---------------------------------------------------------------
C        IF THE ELEMENT IS A REGION NAME, CHECK MEMORY AND EXPAND
C
         if (iregno .gt. 0) then
C
C           ............................................................
C           SAVE EXPANDED REGION DEFINITION WITH ( AND ) ADDED
c           check if enough memory
C
            if(rdeflen.lt.ndefmregs(nmregs)+offmregdef(nmregs)
     *         +ndefmregs(iregno)+2) then
               call mmincblk('mregdef',geom_nm,ipmregdef,
     *         5*ndefmregs(iregno)+1000,ierror)
               rdeflen=rdeflen+5*ndefmregs(iregno)+1000
            endif
            ndefmregs(nmregs)=ndefmregs(nmregs)+1
            mregdef(offmregdef(nmregs)+ndefmregs(nmregs))='('
            do ir=1,ndefregs(iregno)
               mregdef(offmregdef(nmregs)+ndefmregs(nmregs)+ir)=
     *             regdef(offregdef(iregno)+ir)
            enddo
            ndefmregs(nmregs)=ndefmregs(nmregs)+ndefregs(iregno)+1
            mregdef(offmregdef(nmregs)+ndefmregs(nmregs))=')'
            lastmregdef=lastmregdef+2+ndefregs(iregno)
C
C        ---------------------------------------------------------------
C        OTHERWISE SAVE INPUT DATA
C
         else
            ndefmregs(nmregs)=ndefmregs(nmregs)+1
            len1=icharlnf(cmsgin(i))
            if(rdeflen.le.ndefmregs(nmregs)+offmregdef(nmregs))
     *        then
               call mmincblk('mregdef',geom_nm,ipmregdef,1000,ierror)
               rdeflen=rdeflen+1000
            endif
            mregdef(offmregdef(nmregs)+ndefmregs(nmregs))=
     *            cmsgin(i)(1:lentoken)
            lastmregdef=lastmregdef+1
C
         endif
C
C     SET MREGION material number in matregs
C
         matregs(nmregs)=nmregs
C
      enddo
C     ------------------------------------------------------------------
C     CALCULATE AND SAVE MAX. NO. OF REGION DEFINITION ELEMENTS
 
      maxmdef=max(maxmdef,ndefmregs(nmregs))
c
 9999 continue
C
      return
      end
