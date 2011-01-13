C
C #####################################################################
C
C     FILE -  subroutines for reading formated files 
C
C
C     CHANGE HISTORY -
C
CPVCS    Original version.
C
C        $Log: readfile.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C #####################################################################
C
C
      subroutine get_filetype(fword,fname,ityp,formtyp,ierr)
C
C######################################################################
C
C      PURPOSE - Check if file is avs, ascii gmv or binary gmv
C
C      RETURNS 
C              - fword = root or name of file
C              - fname = name of file found
C              - ityp =  1 if avs, 2 if gmv, 0 if unknown 
C              - formtyp =  1 if binary, 2 if ascii, 0 if unknown 
C              - err =  0  if found, no errors
C              - err = -1  if file not found
C              - err = -1  if file not found
C              - err = >0  if errors 
C
C######################################################################
C
      implicit NONE
C
      character*32 fword, fname
      integer ityp, formtyp
      integer ierr, ics
C
      integer ilen, iunit 
      integer AVS, GMV, BIN, ASCI
      integer icharlnf
      logical iexist
      
      character*132 logmess
      character*132 cline
      character*32  ftype
C
      data AVS, GMV  /1,2/
      data BIN, ASCI /1,2/
C
C    
      fname=' '
      ierr=1
      ityp=0
      formtyp=0
C
      ilen=icharlnf(fword)

C     Check for .inp
      if (fword(ilen-3:ilen).eq.'.inp') then
         fname=fword
      endif
      inquire(file=fname,exist=iexist,form=ftype,err=9999)
      if (iexist) then
         ityp=AVS
         if (ftype(1:6).eq.'UNFORM') then
           formtyp=BIN
         else
           formtyp=ASCI
         endif
         ierr=0
         return
      endif

C     Check for .gmv
      if (fword(ilen-3:ilen).eq.'.gmv') then
         fname=fword
      endif
      inquire(file=fname,exist=iexist,form=ftype,err=9999)
      if (iexist) then
         ityp=GMV
         if (ftype(1:6).eq.'UNFORM') then
           formtyp=BIN
         else
           formtyp=ASCI
         endif
         ierr=0
         return
      endif

C     Check for .inp by adding .inp
      fname=fword(1:ilen)//'.inp'
      inquire(file=fname,exist=iexist,form=ftype,err=9999)
      if (iexist) then
         ityp=AVS
         if (ftype(1:6).eq.'UNFORM') then
           formtyp=BIN
         else
           formtyp=ASCI
         endif
         ierr=0
         return
      endif

C     Check for .gmv by adding .gmv
      fname=fword(1:ilen)//'.gmv'
      inquire(file=fname,exist=iexist,form=ftype,err=9999)
      if (iexist) then
         ityp=GMV
         if (ftype(1:6).eq.'UNFORM') then
           formtyp=BIN
         else
           formtyp=ASCI
         endif
         ierr=0
         return
      endif


C     Check for file without suffix .inp or .gmv
C     if file is binary, it is gmv binary
      fname=fword
      inquire(file=fname,exist=iexist,form=ftype,err=9999)
      if (.not.iexist) then 
        ierr=-1
        goto 9999
      endif

      if (ftype(1:6).eq.'UNFORM') then
        formtyp=BIN
        ityp=GMV
        ierr=0
        return
      else
        formtyp=ASCI
      endif

C     if file is ascii, it is either avs or gmv 
C     gmv has header = gmvinput ascii

      iunit=-1
      call hassign(iunit,fname,ics)
      if (iunit.lt.0 .or. ics.lt.0) then
       call x3d_error("readfile",'hassign bad file unit')
      endif

      read(iunit,'(a132)') cline
      print*,'cline: ',cline(1:8)
      if (cline(1:8).eq.'gmvinput') then
        ityp=GMV
      else
        ityp=AVS
      endif
      close(iunit)

 9999 if (ityp.ne.0 .and. formtyp.ne.0) ierr=0

      return
      end

C
       subroutine readfile(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C######################################################################
C
C      PURPOSE - 
C         Figure out format and type of file then Read into cmo 
C         Short cut when using avs and gmv with root names
C         Checks for '.inp' and '.gmv' suffix names
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      RETURNS
C                ierr =  0  if found, no errors
C                ierr = -1  if file not found
C                ierr = -1  if file not found
C                ierr = >0  if errors
C
C      ASSUMES
C                fname = name of file found
C                ityp =  1 if avs, 2 if gmv, 0 if unknown
C                formtyp =  1 if binary, 2 if ascii, 0 if unknown
C
C      FORMAT:
C
C      RFILE/file_root | file_name / cmoname
C      (readfile conflicts with LAGriT readdump options)
C
C      EXAMPLES:
C
C      rfile/ 3dmesh.inp / cmo1
C      rfile/ 3dmesh.gmv / cmo1
C      rfile/ 3dmesh / cmo1
C
C######################################################################
C
      implicit NONE
C
C Arguments
      integer nwds, imsgin(nwds), msgtype(nwds), nnwds, icmsg
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierr
C Mesh Object
      character*32 cmo
C Local
      integer ityp, iform
      integer ierr1, ics
      integer ilen, iunit
      integer AVS, GMV, BIN, ASCI
      integer icharlnf
      logical iexist
      character*32 fword, fname
      character*132 logmess
      character*512 cbuff
C
      data AVS, GMV  /1,2/
      data BIN, ASCI /1,2/

C.......................................................................
C BEGIN

      if (nwds.eq.2 .and. msgtype(1).eq.3 .and. msgtype(2).eq.3) then 
        fword = cmsgin(2)
        cmo = '3dmesh' 

      elseif(nwds.eq.3 .and. msgtype(2).eq.3 .and. msgtype(3).eq.3)then
        fword = cmsgin(2)
        cmo = cmsgin(3)

      else
        write(logmess,9000)
        call writloga('default',1,logmess,0,ics)
 9000   format('Bad syntax for readfile/file/cmoname.')
        goto 9999
      endif

      call get_filetype(fword(1:icharlnf(fword)),fname,
     *                  ityp,iform,ierr1)

      if (ierr1 .ne. 0) then
         if(ierr1.eq.-1)then
           write(logmess,9005) fword(1:ilen)
           call writloga('default',1,logmess,0,ics)
         else
           write(logmess,9010) fword(1:ilen)
           call writloga('default',1,logmess,0,ics)
         endif
         goto 9999
 9005    format('The input file, ',a,', does not exist.')
 9010    format('The input file, ',a,', has an unknown format.')
      endif


C.......................................................................
C READ FILE INTO CMO

      ilen=icharlnf(fname)
C     READ AVS
      ierr1=0
      if (ityp.eq.AVS) then
        cbuff = 'read/avs/' //
     *         fname(1:ilen) // '/' //
     *         cmo(1:icharlnf(cmo)) // '/ ; finish'
        call dotaskx3d(cbuff,ierr1)
        if (ierr1.ne.0) call x3d_error('readavs',fname(1:ilen))

C     READ BINARY and ASCI GMV
      elseif (ityp.eq.GMV ) then

        cbuff = 'read/gmv/' //
     *         fname(1:ilen) // '/' //
     *         cmo(1:icharlnf(cmo)) // '/ ; finish'
        call dotaskx3d(cbuff,ierr1)
        if (ierr1.ne.0) call x3d_error('readgmv',fname(1:ilen))
      endif

C END READ INPUT
C.......................................................................

 9999 return
      end

