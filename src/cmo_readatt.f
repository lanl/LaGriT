      subroutine cmo_readatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                    ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C      Reads cmo attributes from a file
C      Assume record style format with the same number of values per line
C      There can be fewer attributes written than number of values
C      The first values are used, the rest are not
C      If there are more attributes listed than there are values
C      then only the first nwords number of attributes are filled
C
C      Note: the parser has a limited length which is near 25 words
C      
C  cmo/readatt/cmo_name/attr1/attr2/.../pset,get,pset_name/file_name
C  cmo/readatt/cmo_name/attr1/attr2/.../add///file_name
C
C     if attr1, attr2 do not exist VDOUBLE attributes are created.
C     if 'add' is specified then new nodes are added to the cmo
C     if a pset is given, the specified nodes are modified
C
C     Lines of an input file that have # in the first column or that do not
C     have a real or integer as their first word are skipped.
C
C     each line is parsed and number of values saved in nwords
C     number of attributes to write to is num_att
C     num_att can be less than nwords
C     the number of attributes written to can not be more than nwords
C     number of valid attributes written to is itotal
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_readatt.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 13:24:46   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.1   Fri Dec 03 16:50:18 1999   dcg
CPVCS    update ipointi, and ipointj if option is 'add'
CPVCS    
CPVCS       Rev 1.0   Tue Sep 29 14:22:26 1998   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C 
C#######################################################################
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
      integer ierror_return

      integer nline, length,l,icscode,lenfile,
     *   i,j,ilen,itype,ierror,nwords,lenout,iunit,npointsnew,
     *   npoints,istart,ityp,ipt1,ipt2,ipt3,mpno, line_index
      integer itotal, num_write 

C     NOTE: 128 length is defined in getcmds.f of lg_util code.
      integer NW_LENGTH
      PARAMETER (NW_LENGTH = 128)
C
      character*32 ich1,ich2,ich3
C
C#######################################################################
C
C.... Subroutine name for memory management and errors.
C
      character*32 isubname
C
C.... Mesh Object Data.
C
      character*32 ifile
      character*32 cmo, cmoatt, cglobal, cdefault
      character*1024 cbuff
      character*1024 fline

C     these message arrays are used to hold values from
C     each line as read from the input data file
      integer lenparse, nwds2, limin, limax

      integer      msgt(NW_LENGTH)
      real*8       xmsg(NW_LENGTH)
      integer      imsg(NW_LENGTH)
      character*32 cmsg(NW_LENGTH)
      character*32 att_list(NW_LENGTH)
C
C#######################################################################
C
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
      integer num_att
      logical if_addpoints
C
      pointer ( ipmpary1 , mpary1 )
      pointer (ipxvalues,xvalues)
      real*8 xvalues(*)
      pointer (ipxarray, iarray)
      pointer (ipxarray, xarray)
      integer iarray(*), mpary1(*)
      real*8 xarray(*)
      pointer (ipitp1,itp1)
      pointer (ipisetwd,isetwd)
      integer itp1(*),isetwd(*)
      integer ipointi,ipointj
C
C#######################################################################
C
C
C     Define the subroutine name for memory management and errors.
C
      ierror_return = 0
      itotal = 0
      num_write = 0
      cglobal='global'
      cdefault='default'
      isubname='cmo_readatt'
      if_addpoints = .false.
      ipt1 = 0
      ipt2 = 0
      ipt3 = 0
C
C     assign parsed values into local variables so later
C     calls to parser to not overwrite needed values
C   1     2      3       4 ....                             nwds
C  cmo/readatt/cmo_name/attr1/attr2/.../pset,get,pset_name/file_name
C  cmo/readatt/cmo_name/attr1/attr2/.../add//             /file_name

      ifile=cmsgin(nwds)
      cmo=cmsgin(3)
      num_att = 0
      do i = 4,nwds-4
         if (msgtype(i) .eq. 3 ) then
             num_att = num_att + 1
             att_list(num_att) = cmsgin(i)
         endif
      enddo

C     get the point information for mpary calls
      if (msgtype(nwds-3).eq.3.and.cmsgin(nwds-3)(1:3).eq.'add')
     &    then
          if_addpoints = .true. 
      elseif(msgtype(nwds-3).eq.1 .and. msgtype(nwds-2).eq.1
     &    .and. msgtype(nwds-1).eq.1) then
          ipt1=imsgin(nwds-3)
          ipt2=imsgin(nwds-2)
          ipt3=imsgin(nwds-1)
      elseif(msgtype(nwds-3).eq.3 .and. msgtype(nwds-2).eq.3 
     &    .and. msgtype(nwds-1).eq.3) then
          ich1=cmsgin(nwds-3)
          ich2=cmsgin(nwds-2)
          ich3=cmsgin(nwds-1)
      endif

C
C
C     ******************************************************************
C
C     CHECK TO SEE IF THE FILE EXISTS, IF NOT THEN BAIL OUT.
C     if there read values into temp array
C
      lenfile=icharlnf(ifile)
      call fexist(ifile(1:lenfile),ierror)
      if(ierror.eq.0) then
         write(logmess,9000) ifile(1:lenfile)
 9000    format('The file, ',a,', does not exist.')
         call writloga('default',0,logmess,0,ierror)
         ierror_return = -1
         goto 9999
      endif
C
C
C     ******************************************************************
C
C     ASSIGN THE FILE TO THE NEXT AVAILABLE LOGICAL UNIT NUMBER.
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (ierror.ne.0 .or. iunit.lt.0) then
         write(logmess,*) 'hassign bad file no for '
     &         //ifile(1:lenfile)
         call writloga('default',0,logmess,0,icscode)
         ierror_return = -1
         goto 9999
      endif

C     TAM - commented out this strange assignment
C     nwords=nwds-7
 
C     temp array to hold values
C
      length=500000
      call mmgetblk('xvalues',isubname,ipxvalues,length,2,icscode)
      if (icscode.ne.0 ) then
         write(logmess,'(a,i5,a,i15)') 'mmgetblk fail: ',icscode 
     &        ,' with length: ',length 
         call writloga('default',0,logmess,0,icscode)
         ierror_return = -2 
         goto 9999
      endif

C     READ FILE LINES
      nwords=0
      nline=0
      limin = 0
      limax = 0
 100  continue
      read(iunit,'(a)',end=110) fline

C     Check for comment lines '#' or lines that do not have either an
C     integer or real as the first word in the line. This will not get
C     everything but at least it will read past character headers such
C     as those found in the header of a TecPlot style file.
 
      if (fline(1:1).ne.'#') then 

        lenparse = len(fline)
        if (nline == 0) then
           limin = lenparse 
           limax = lenparse 
        endif

        if (lenparse .le. 0) then

        write(logmess,'(a,i10)')
     *    "readatt: found 0 line length line number: ",nline+1 
        call writloga('default',0,logmess,0,icscode)
        write(logmess,'(a,i10)') 

        else

        if (lenparse.gt.0 .and. lenparse.lt.limin) limin = lenparse
        if (lenparse.gt.0 .and. lenparse.gt.limax) limax = lenparse

C       parse the line
        call parse_string2(lenparse,fline(1:lenparse),
     &      imsg,msgt,xmsg,cmsg,nwds2)

        if (nline .eq. 0) then
          write(logmess,'(a,i5,a)')'readatt: reading ',nwds2,
     &    ' values from each line.'
          call writloga('default',0,logmess,0,icscode)
          nwords = nwds2

        else
C         return with error 
C         xvalues expects the same number of values per line
          if (nwds2 .ne. nwords) then
            write(logmess,'(a,i5,a,i5,a,i5)') 'ERROR line: ',
     &      nline+1,' found '
     &      ,nwds2,' values but expected ',nwords
            call writloga('default',0,logmess,0,icscode)
            ierror_return = -2
            goto 9999
          endif
        endif

        if (msgt(1).eq.1 .or. msgt(1) .eq.2) then
          nline=nline+1

c         if needed, re-allocate for more values
          if(nline*nwords.gt.length) then
            l=max(int(0.5*length), 500000)

            call mmincblk('xvalues',isubname,ipxvalues,l,icscode)
            if (icscode.ne.0 ) then
             write(logmess,'(a,i5,a,i15)') 'mmincblk fail: ',icscode
     &       ,' with length: ',l
              call writloga('default',0,logmess,0,icscode)
              ierror_return = -2
              goto 9999
            endif
            length=length+l
          endif
          read(fline,*) (xvalues(nwords*(nline-1)+i),i=1,nwords)

        else
           line_index = nline + line_index + 1
           write(logmess,'(a,i10)')
     *       'WARNING: read found character string at line ', 
     *       line_index
           call writloga('default',0,logmess,0,icscode)
           print*, "Skipping this line"
           goto 100

C           write(logmess,'(a,i5)')
C     *       "ERROR: read found unallowed type: ",msgt(1) 
C           call writloga('default',0,logmess,0,icscode)
C           write(logmess,'(a,i10)') 
C     *     'ERROR character string at line ',nline+1
C           call writloga('default',0,logmess,0,icscode)
C           print*,"ERROR: string: [",cmsg(1),"]"
C           print*," "
C           ierror_return = -2
C           goto 9999

         endif
C        end if valid type

       endif
C      end if lenparse gt 0

      else
         line_index = line_index + 1
      endif
      goto 100
 110  continue
C     DONE READ FILE LINES
 
      close(iunit)

C     report inconsistancies that may have caused read problems

      if (num_att .gt. nwords) then
         write(logmess,'(a)') 'Warning readatt:'
         call writloga('default',1,logmess,0,icscode)
         write(logmess,'(a,i5)') 
     &   '  Number of attributes: ',  num_att
         call writloga('default',0,logmess,0,icscode)
         write(logmess,'(a,i5)') 
     &   '  is greater than values read from each line: ',nwords 
         call writloga('default',0,logmess,0,icscode)

         if (limax.ne.limin) then
           write(logmess,'(a,i10,a,i10)')
     &     '  Line lengths differ, min: ',limin,
     &     ' max: ',limax
           call writloga('default',0,logmess,0,icscode)
         endif

         if (limax.gt.750 .or. limin.gt.750) then
           write(logmess,'(a,i10,a)') 
     &     '  Line length: ',limax,' may exceed readable length.'
           call writloga('default',0,logmess,0,icscode)
         endif
      endif
      if (nline.le.0 ) goto 9999

C
C     it is possible to have 0 npoints if cmo is empty so far
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierror)

      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,ierror)
      call cmo_get_info('ipointi',cmo,
     *                ipointi,ilen,ityp,icscode)
      call cmo_get_info('ipointj',cmo,
     *               ipointj,ilen,ityp,icscode) 

      istart = npoints

      if(npoints.eq.0 .or. if_addpoints .eqv. .true.) then
         ilen=nline
      else
         ilen=npoints
      endif

      call mmgetblk('mpary1',isubname,ipmpary1,ilen,1,icscode)
      if (icscode.ne.0 ) then
         write(logmess,'(a,i5,a,i15)') 'mmgetblk/mpary1 fail: ',icscode
     &        ,' with length: ',ilen
         call writloga('default',0,logmess,0,icscode)
         ierror_return = -3
         goto 9999
      endif

      if(npoints.eq.0) then
          npointsnew=nline
          do i=1,nline
             mpary1(i)= i
          enddo
          ipointi=ipointj+1
          ipointj=ipointj+nline
          call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                ipointi,icscode)
          call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                ipointj,icscode)

      elseif( if_addpoints .eqv. .true. ) then
          npointsnew=nline+npoints
          ipointi=ipointj+1
          ipointj=ipointj+nline
          call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                ipointi,icscode)
          call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                ipointj,icscode)

          do i=1,nline
             mpary1(i)= i+npoints
          enddo
      elseif(ipt1 .gt. npoints)  then
          npointsnew=nline+npoints
          do i=1,nline
             mpary1(i)= i+npoints
          enddo
      elseif(ipt2 .gt. npoints)
     *    then
          npointsnew=nline+ipt1-1
          do i=1,nline
             mpary1(i)= i+ipt1-1
          enddo
      else
        if (ipt1 .gt. 0 ) then
          call pntlimn(ipt1,ipt2,ipt3,ipmpary1,mpno,npoints,
     *            isetwd, itp1)
        else
          call pntlimc(ich1,ich2,ich3,ipmpary1,mpno,npoints,
     *            isetwd, itp1)
        endif
        npointsnew=max(npoints,nline,mpary1(mpno))
      endif

c     if this has not been done, do it now 
      if(ipointi.eq.0 .and. ipointj.eq.0 ) then
         ipointi = max(1,ipointi)
         ipointj = max(1,npoints)
         call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                ipointi,icscode)
         call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                ipointj,icscode)
      endif

      call cmo_set_info('nnodes',cmo,npointsnew,1,1,ierror)
      if (ierror .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
      call cmo_newlen(cmo,ierror)
      if (ierror .ne. 0) call x3d_error(isubname,'cmo_newlen')
C
C     ******************************************************************

C     assume order of attribute names are order of values read
C     the number of attributes filled are the number of attributes
C     on the command line and can be less than nwords read from file
      num_write = min(num_att,nwords)

C     process equal number of values and attributes
C     if values read > num_att, write
C        write to first num_att attributes out of total
C     if values read < num_att,
C        write to first nwords attributes out of total

      do i=1,num_write

         cmoatt=att_list(i)
C        to avoid problems adding unecessary extra attribute  
C        which would not be recognized and written to because
C        it is assumed to be one of these cmo reserved words
         if(cmoatt(1:icharlnf(cmoatt)).eq.'imt')cmoatt='imt1'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'itp')cmoatt='itp1'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'icr')cmoatt='icr1'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'isn')cmoatt='isn1'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'ign')cmoatt='ign1'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'xic1')cmoatt='xic'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'yic1')cmoatt='yic'
         if(cmoatt(1:icharlnf(cmoatt)).eq.'zic1')cmoatt='zic'

         call mmfindbk(cmoatt,cmo,ipxarray,lenout,icscode)

         if(icscode.ne.0) then
           write(logmess,'(a,a)') 'readatt adding attribute: ', 
     &        cmoatt(1:icharlnf(cmoatt))
           call writloga('default',0,logmess,0,icscode)

            cbuff='cmo/addatt/' //
     *            cmo(1:icharlnf(cmo)) //
     *            '/' //
     *            cmoatt(1:icharlnf(cmoatt)) //
     *            '/VDOUBLE' //
     *            '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *            ' ; finish '

            call dotask(cbuff,ierror)
            call mmfindbk(cmoatt,cmo,ipxarray,lenout,icscode)

         endif

         write(logmess,'(a,2x,i5,2x,a)') 'readatt filling attribute: ', 
     &        i,cmoatt(1:icharlnf(cmoatt))
         call writloga('default',0,logmess,0,icscode)

         call mmgettyp(ipxarray,itype,icscode)
         do j=1,nline
            if(itype.eq.1) then
               iarray(mpary1(j))=int(xvalues(nwords*(j-1)+i))
             else if (itype.eq.2) then
               xarray(mpary1(j))=xvalues(nwords*(j-1)+i)
             else
               write(logmess,'(a,i5)') 'ERROR: Unsupported type: ',itype 
               call writloga('default',1,logmess,0,icscode)
               ierror_return = -3
               goto 9999
             endif
         enddo
         itotal = itotal + 1
      enddo
C
C     Print status to screen
C
      cbuff='cmo/status/' //
     *       cmo(1:icharlnf(cmo)) //
     *       ' brief ; finish '
      call dotask(cbuff,ierror)

      
      if (istart .ne. npointsnew) then
      write(logmess,9070) ipointi,ipointj
 9070 format('readatt added nodes ',i10,
     *  ' to ',i10)
      call writloga('default',0,logmess,0,icscode)
      endif


C     each line is parsed and number of values saved in nwords
C     number of attributes to write to is num_att
C     num_att can be less than nwords
C     the number of attributes written to can not be more than nwords
C     number of valid attributes written to is itotal


       write(logmess,'(a,i5,a,i5,a)') 
     * 'File values used: ',num_write,' out of ',
     *  nwords,' from each line.'
        call writloga('default',0,logmess,0,icscode)

       write(logmess,'(a,i5,a,i5,a)') 
     * 'File values written to ',itotal,' out of ',
     *  num_att,' attributes.'
        call writloga('default',0,logmess,0,icscode)

 9999 continue

      if (nline.le.0 .or. ierror_return.ne.0) then
         write(logmess,'(a,a,i15)') 'ERROR readatt: read failed: ',
     &      ' number of lines read from file: ',nline
         call writloga('default',0,logmess,0,icscode)
      endif

      if (itotal.eq.0 .or. itotal.lt.num_write ) then
         write(logmess,'(a,a,i15)') 
     &      'ERROR readatt: attributes failed: ',
     &      ' number of attributes written: ',itotal
         call writloga('default',0,logmess,0,icscode)
      endif

      if (ierror_return.ne.0) then
        write(logmess,'(a)') 'ERROR readatt: early exit.'
        call writloga('default',0,logmess,0,icscode)

      else
        write(logmess,'(a,i5,a,i15,a)') 'END readatt: reading ',
     *  nwords,' values from ',nline," lines."
        call writloga('default',0,logmess,0,icscode)
      endif

      close(iunit)
      call mmrelprt(isubname,icscode)
C
      return
      end
