      subroutine cmo_readatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                    ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Reads cmo attributes from a file
C  cmo/readatt/cmo_name/attr1/attr2/.../pset,get,pset_name/file_name
C  cmo/readatt/cmo_name/attr1/attr2/.../add///file_name
c         if attr1, attr2 do not exist VDOUBLE attributes are
C         created.
C         if 'add' is specified then new nodes are added to the cmo
C         if a pset is given, the specified nodes are modified
C
C         Lines of an input file that have # in the first column or that do not
C         have a real or integer as their first word are skipped.
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
      integer ierror_return,icount,length,l,icscode,lenfile,
     *   i,j,ilen,itype,ierror,nvalues,lenout,iunit,npointsnew,
     *   npoints,istart,ityp,ipt1,ipt2,ipt3,mpno
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds),ich1,ich2,ich3
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
      integer lenparse, nwds2
      integer      msgt(128)
      real*8       xmsg(128)
      integer      imsg(128)
      character*32 cmsg(128)
      character*32 att_list(128)
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
         call writloga('default',1,logmess,0,ierror)
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
         call writloga('default',1,logmess,0,icscode)
         ierror_return = -1
         goto 9999
      endif

C
      icount=0
C     TAM - commented out this strange assignment
C     nvalues=nwds-7
      nvalues=0
C
C     temp array to hold values
C
      length=500000
      call mmgetblk('xvalues',isubname,ipxvalues,length,2,icscode)
      if (icscode.ne.0 ) then
         write(logmess,'(a,i5,a,i15)') 'mmgetblk fail: ',icscode 
     &        ,' with length: ',length 
         call writloga('default',1,logmess,1,icscode)
         ierror_return = -2 
         goto 9999
      endif

 100  continue
C     READ FILE LINES
      read(iunit,'(a)',end=110) fline

C     Check for comment lines '#' or lines that do not have either an
C     integer or real as the first word in the line. This will not get
C     everything but at least it will read past character headers such
C     as those found in the header of a TecPlot style file.
 
      if (fline(1:1).ne.'#') then 

        lenparse = len(fline)

        call parse_string2(lenparse,fline(1:lenparse),
     &    imsg,msgt,xmsg,cmsg,nwds2)

        if (icount .eq. 0) then
          write(logmess,'(a,i5,a)')'readatt reading ',nwds2,
     &    ' values from each line.'
          call writloga('default',0,logmess,0,icscode)
        else
          if (nwds2 .ne. nvalues) then
          write(logmess,'(a,i5,a,i5)') 'ERROR: read found '
     &      ,nwds2,' values but expected ',nvalues
          call writloga('default',1,logmess,1,icscode)
          ierror_return = -2
          goto 9999
          endif
        endif
        nvalues = nwds2

        if (msgt(1).eq.1 .or. msgt(1) .eq.2) then
          icount=icount+1

c         if needed, re-allocate for more values
          if(icount*nvalues.gt.length) then
            l=max(int(0.5*length), 500000)

            call mmincblk('xvalues',isubname,ipxvalues,l,icscode)
            if (icscode.ne.0 ) then
             write(logmess,'(a,i5,a,i15)') 'mmincblk fail: ',icscode
     &       ,' with length: ',l
              call writloga('default',1,logmess,1,icscode)
              ierror_return = -2
              goto 9999
            endif
            length=length+l
          endif
          read(fline,*) (xvalues(nvalues*(icount-1)+i),i=1,nvalues)

         else

           write(logmess,'(a,i5)')
     *       "ERROR: read found unallowed type: ",msgt(1) 
           call writloga('default',1,logmess,0,icscode)
           write(logmess,'(a,i10)') 
     *     'ERROR character string at line ',icount+1
           call writloga('default',0,logmess,0,icscode)
           print*,"ERROR: string: [",cmsg(1),"]"
           print*," "
           ierror_return = -2
           goto 9999
         endif

      endif
      goto 100
 110  continue
C     DONE READ FILE LINES
 
      close(iunit)

      if (icount .le. 0) then
         write(logmess,'(a,a,i15)') 'ERROR: read failed: ', 
     &      ' number of lines read from file: ',icount 
         call writloga('default',1,logmess,1,icscode)
         ierror_return = -2
         goto 9999
      endif
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
         ilen=icount
      else
         ilen=npoints
      endif

      call mmgetblk('mpary1',isubname,ipmpary1,ilen,1,icscode)
      if (icscode.ne.0 ) then
         write(logmess,'(a,i5,a,i15)') 'mmgetblk/mpary1 fail: ',icscode
     &        ,' with length: ',ilen
         call writloga('default',1,logmess,1,icscode)
         ierror_return = -3
         goto 9999
      endif

      if(npoints.eq.0) then
          npointsnew=icount
          do i=1,icount
             mpary1(i)= i
          enddo
          ipointi=ipointj+1
          ipointj=ipointj+icount
          call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                ipointi,icscode)
          call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                ipointj,icscode)

      elseif( if_addpoints .eqv. .true. ) then
          npointsnew=icount+npoints
          ipointi=ipointj+1
          ipointj=ipointj+icount
          call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                ipointi,icscode)
          call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                ipointj,icscode)

          do i=1,icount
             mpary1(i)= i+npoints
          enddo
      elseif(ipt1 .gt. npoints)  then
          npointsnew=icount+npoints
          do i=1,icount
             mpary1(i)= i+npoints
          enddo
      elseif(ipt2 .gt. npoints)
     *    then
          npointsnew=icount+ipt1-1
          do i=1,icount
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
        npointsnew=max(npoints,icount,mpary1(mpno))
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
C
C

C     assume order of attribute names are order of values read
C     the number of attributes filled are the number of attributes
C     on the command line and can be less than nvalues read from file
      do i=1,num_att

         cmoatt=att_list(i)
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

         write(logmess,'(a,a)') 'readatt filling attribute: ', 
     &        cmoatt(1:icharlnf(cmoatt))
         call writloga('default',0,logmess,0,icscode)

         call mmgettyp(ipxarray,itype,icscode)
         do j=1,icount
            if(itype.eq.1) then
               iarray(mpary1(j))=int(xvalues(nvalues*(j-1)+i))
             else if (itype.eq.2) then
               xarray(mpary1(j))=xvalues(nvalues*(j-1)+i)
             else
               write(logmess,'(a,i5)') 'ERROR: Unsupported type: ',itype 
               call writloga('default',1,logmess,0,icscode)
               ierror_return = -3
               goto 9999
             endif
         enddo
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

 9999 continue

      write(logmess,'(a,i5,a,i15,a)') 'END readatt: ',
     *      nvalues," attribute values from ",icount," lines."
      call writloga('default',0,logmess,1,icscode)

      if (icount .le. 0 .or. ierror_return.ne.0) then
        write(logmess,'(a,i3)') 'ERROR readatt: early exit.',
     *     ierror_return
        call writloga('default',0,logmess,1,icscode)
      endif

      close(iunit)
      call mmrelprt(isubname,icscode)
C
      return
      end
