      subroutine region(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      implicit none
C
      character*132 logmess
C
C#######################################################################
C
C     PURPOSE -
C
C
C        THIS ROUTINE STORES THE PHYSICAL REGION DEFINITIONS INTO THE
C        MISCELLANEWOUS STORAGE BLOCK.
C
C        FORMAT: REGION/IREGNAME/region definition data
C        FORMAT: REGION/IREGNAME/RELEASE
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
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/region_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.11   18 Apr 2000 13:24:42   dcg
CPVCS    implement release option for surfaces, regions and mregions
CPVCS    
CPVCS       Rev 1.10   Wed Apr 05 13:35:00 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.9   20 Mar 2000 13:42:28   dcg
CPVCS    check for duplicate name - if so then print warning and skip command
CPVCS    
CPVCS       Rev 1.8   Thu Feb 24 12:13:04 2000   dcg
CPVCS    use character mm arrays for region, mregion definitions
CPVCS    fix tests for incrementing memory
CPVCS    
CPVCS       Rev 1.7   24 Feb 2000 11:17:10   dcg
CPVCS    use type=3 for character arrays
CPVCS    
CPVCS       Rev 1.6   Thu Feb 03 13:39:14 2000   dcg
CPVCS    
CPVCS       Rev 1.5   Wed Feb 02 12:22:46 2000   dcg
CPVCS    
CPVCS       Rev 1.4   13 Jan 2000 14:48:22   dcg
CPVCS    
CPVCS       Rev 1.3   06 Jan 2000 12:55:12   dcg
CPVCS
CPVCS       Rev 1.18   Mon Oct 18 16:39:26 1999   dcg
CPVCS    initialize ndefreg, nstbreg
CPVCS
CPVCS       Rev 1.17   Thu Oct 14 14:10:06 1999   dcg
CPVCS    implicit none
CPVCS    check length of stbout where needed
CPVCS
CPVCS       Rev 1.16   Fri Oct 31 10:49:48 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.15   Mon Apr 14 16:59:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.14   11/07/95 17:24:54   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.13   08/29/95 12:03:16   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.12   08/28/95 11:33:08   ahmed
CPVCS    Adjust the location of mmrelprt in the routine
CPVCS
CPVCS       Rev 1.11   08/23/95 06:58:42   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.10   08/22/95 06:50:40   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.9   06/13/95 09:02:36   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.8   06/07/95 15:31:22   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.7   05/01/95 08:33:58   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.6   03/31/95 09:09:30   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.5   03/30/95 05:00:38   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.4   03/23/95 15:07:54   dcg
CPVCS     Add mesh object name to storage block id for surface,region info.
CPVCS
CPVCS       Rev 1.3   03/06/95 15:33:22   dcg
CPVCS     Add length field to character comparisons
CPVCS
CPVCS       Rev 1.2   01/09/95 17:31:46   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/19/94 08:27:20   het
CPVCS    Add the "comdict.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:10   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include 'geom_lg.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror,iregno,n,j,
     *  nrightp,nleftp,ierrw,ir,irok,len2,is,
     *  isok,len1,length,ier,iout,lout,itype,
     *  i,rdeflen,lentoken
      real*8 rout
      pointer(ipout,out)
      real*8 out(*)
      integer icharlnf
C
C ######################################################################
C
      character*32 ctemp,iregnam,isubname
      character*32 cmo,geomn
C
C#######################################################################
C
      isubname='region'
      ierror = 0
C
C#######################################################################
C
C  get mesh object name
      call cmo_get_name(cmo,ier)
      if(ier.ne.0) call x3d_error('region','cmo_get_name')
      call cmo_get_attinfo('geom_name',cmo,iout,rout,geomn,
     *                        ipout,lout,itype,ierror)
c
c  look for surface data pointers
c
      call mmfindbk('csall',geomn,ipcsall,length,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)') 'Mesh has no surfaces '
         call writloga('default',0,logmess,0,ierror)
         go to 9999
      endif
c
c  get region data pointers if they exist otherwise make them
c
      call mmfindbk('cregs',geomn,ipcregs,length,ierror)
      if(ierror.ne.0) then
         nregs=0
         lastregdef=0
         maxdef=0
         length=100
         call mmgetblk('cregs',geomn,ipcregs,length,3,ierror)
         call mmgetblk('offregdef',geomn,ipoffregdef,length,1,ierror)
         call mmgetblk('ndefregs',geomn,ipndefregs,length,1,ierror)
         call mmgetblk('regdef',geomn,ipregdef,10*length,3,ierror)
      elseif ( length.lt.(nregs+1)*4) then
         call mmincblk('cregs',geomn,ipcregs,length,ierror)
         call mmincblk('offregdef',geomn,ipoffregdef,length,ierror)
         call mmincblk('ndefregs',geomn,ipndefregs,length,ierror)
         call mmincblk('regdef',geomn,ipregdef,10*length,ierror)
      else
         call mmfindbk('cregs',geomn,ipcregs,length,ierror)
         call mmfindbk('offregdef',geomn,ipoffregdef,length,ierror)
         call mmfindbk('ndefregs',geomn,ipndefregs,length,ierror)
         call mmfindbk('regdef',geomn,ipregdef,length,ierror)
 
      endif
      iregnam=cmsgin(2)
c
c check if operation is release
c
      if(cmsgin(3).eq.'release'.or.cmsgin(3).eq.'delete'.or.
     *   cmsgin(3).eq.'remove') then
         do i=1,nregs
            if(cregs(i).eq.iregnam) then
               if(i.lt.nregs) then
                  length=offregdef(i+1)-offregdef(i)
                  do j=offregdef(i),lastregdef-length
                      regdef(j+1)=regdef(j+length+1)
                  enddo
                  do j=i+1,nregs
                     offregdef(j)=offregdef(j)-length
                  enddo
                  do j=i,nregs-1
                     cregs(j)=cregs(j+1)
                     ndefregs(j)=ndefregs(j+1)
                     offregdef(j)=offregdef(j+1)
                  enddo
                  lastregdef=lastregdef-length
               else
                  lastregdef=0
                  if(nregs.gt.1)lastregdef=offregdef(nregs)
                  offregdef(nregs)=0
                  cregs(nregs)=' '
                  ndefregs(nregs)=0
               endif
               nregs=nregs-1
               go to 9999
            endif
         enddo
      endif
c
c  see if duplicate region - if so warn and skip
c
      nregs=nregs+1
      n=nregs
      do i=1,n-1
        if(cregs(i).eq.iregnam) then
           write(logmess,'(a,a)') 'duplicate region - ignored: ',iregnam
           call writloga('default',0,logmess,0,ierror)
            nregs=nregs-1
           go to 9999
        endif
      enddo
C
C     ******************************************************************
C     CHECK INPUT DATA, IF ELEMENT IS NOT AN OPERATOR, CHECK SURFACE
C     AND REGION NAMES
C
 
      do 5 i=3,nwds
         len1=icharlnf(cmsgin(i))
         ctemp=' '
         ctemp(1:len1)=cmsgin(i)(1:len1)
         if (ctemp(1:len1).ne.'lt' .and. ctemp(1:len1).ne.'gt' .and.
     &       ctemp(1:len1).ne.'le' .and. ctemp(1:len1).ne.'ge' .and.
     &       ctemp(1:len1).ne.'or' .and.
     &       ctemp(1:len1).ne.'and' .and. ctemp(1:len1).ne.'not' .and.
     &       ctemp(1:len1).ne.'(' .and. ctemp(1:len1).ne.')') then
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
               write(logmess,9000) cmsgin(i)
 9000          format(' REGION ERROR: ',a8,' IS AN INVALID ',
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
 9001    format(' REGION ERROR: UNBALANCED PARENTHESIS')
         call writloga('default',0,logmess,0,ierrw)
         go to 9999
      endif
C
C     ******************************************************************
C     GET REGION NAME
C
      len1=icharlnf(cmsgin(2))
      iregnam=' '
      iregnam(1:len1)=cmsgin(2)(1:len1)
C
C     ******************************************************************
C     SAVE region name and offset and definition
C
      cregs(nregs)=iregnam(1:len1)
      offregdef(nregs)=lastregdef
      ndefregs(nregs)=0
      do  i=3,nwds
C
C        ---------------------------------------------------------------
C        SEE IF THIS ELEMENT IS A REGION NAME
C
         iregno=0
         lentoken=icharlnf(cmsgin(i))
         if (nregs .gt. 0) then
            do 10 ir=1,nregs-1
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
            if (ctemp(1:2).eq.'lt' .or. ctemp(1:2).eq.'gt' .or.
     &          ctemp(1:2).eq.'le' .or. ctemp(1:2).eq.'ge') iregno=0
         endif
c
c  get length of array of region definitions
c
         call mmfindbk('regdef',geomn,ipregdef,rdeflen,ierror)
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
            if(rdeflen.lt.ndefregs(nregs)+ndefregs(iregno)+2+
     *         offregdef(nregs)) then
               call mmincblk('regdef',geomn,ipregdef,5*ndefregs(iregno)
     *         +1000,ierror)
               rdeflen=rdeflen+5*ndefregs(iregno)+1000
            endif
            ndefregs(nregs)=ndefregs(nregs)+1
            regdef(offregdef(nregs)+ndefregs(nregs))='('
            do ir=1,ndefregs(iregno)
               regdef(offregdef(nregs)+ndefregs(nregs)+ir)=
     *             regdef(offregdef(iregno)+ir)
            enddo
            ndefregs(nregs)=ndefregs(nregs)+ndefregs(iregno)+1
            regdef(offregdef(nregs)+ndefregs(nregs))=')'
            lastregdef=lastregdef+2+ndefregs(iregno)
C
C        ---------------------------------------------------------------
C        OTHERWISE SAVE INPUT DATA
C
         else
            ndefregs(nregs)=ndefregs(nregs)+1
            len1=icharlnf(cmsgin(i))
            if(rdeflen.le.ndefregs(nregs)+offregdef(nregs)) then
               call mmincblk('regdef',geomn,ipregdef,1000,ierror)
               rdeflen=rdeflen+1000
            endif
            regdef(offregdef(nregs)+ndefregs(nregs))=
     *            cmsgin(i)(1:lentoken)
            lastregdef=lastregdef+1
C
         endif
C
      enddo
 
C     ------------------------------------------------------------------
C     CALCULATE AND SAVE MAX. NO. OF REGION DEFINITION ELEMENTS
 
      maxdef=max(maxdef,ndefregs(nregs))
c
 9999 continue
C
      return
      end
