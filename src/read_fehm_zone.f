      subroutine read_fehm_zone
     1     (imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr)
C
C#######################################################################
C
C    FORMAT - read / zone|zonn / file_name / [mo_name/-def-] / [att_name/-def-] / 
C
C    INPUT ARGUMENTS - imsgin,xmsgin,cmsgin,msgtyp,nwds
C
C    OUTPUT - None
C
C    PURPOSE - Parse an FEHM format 'zone' or 'zonn' file. It is assumed that a MO
C              already exists and the node numbering of the MO is compatible with
C              the zone/zonn file. The only error checking is that the maximum node
C              number in the input zone/zonn file is less than or equal to the number
C              of nodes in the MO.
C             
C              The second token zone|zonn does not have any effect. The zone/zonn
C              option is controled by the value of the zone/zonn keyword that is
C              read from the input file. If the keyword 'zone' is found in the file
C              file_name,  the attribute att_name is first initialized to zero.
C              If the keyword 'zonn' is found, the attribute att_name is not
C              initialized.
C
C              If att_name is given as -def-, then the imt array is used.
C
C              If att_name exits and is type VINT, it is used. If it does
C              not exist, a VINT array is created.
C
C
C#######################################################################
C
      implicit none
C
C     Input variables
C
      integer nwds, imsgin(nwds), msgtyp(nwds), ierr
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C     MO variables
C
      pointer (ipiatt,iatt)
      integer iatt(*)
      pointer (ipiwork,iwork)
      integer iwork(*)
      integer nnodes, ilen, itp
c      
c local parser variables 
c
      character*1024 input_msg 
      integer lenparse
      integer msgt(128)
      real*8  xmsg(128)
      integer imsg(128)
      character*32 cmsg(128)
C
C     Local variables
C
      integer itype, nnum, iunit, lcmoname, i, 
     1        iwork_min, iwork_max, iflag1, iflag2, id_zone,
     2        icount
      integer icharlnf
      character*132 fname, ftype
      logical iexist
      character*32  cmoname, att_name, cid_zone
      integer itype_bin, itype_asc
      data itype_bin, itype_asc /1,2/
      integer index
      character*32 crank,clen,ctype,cio,cinter,cpers
      character*4   cif_zone_zonn
      character*512 logmess
      character*132 dotask_command
      character*128 isubname
C
      data isubname / 'read_fehm_zone' /
C
C     ******************************************************************
C
      ierr = 0
C
C     Valid input is at least 3 tokens and all must be of type character.
C
      if (nwds .lt. 3) then
        write(logmess,"(a,i3)")
     1     'ERROR: At least 3 input tokens required. Found: ', nwds
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
        return
      endif

      do i = 1, nwds
         if(msgtyp(i) .ne. 3) then
        write(logmess,"(a)")
     1     'ERROR: Expecting all input to be type character '
        call writloga('default',0,logmess,0,ierr)
        write(logmess,"(a,i3,a)")
     1     'ERROR: Token ', i, ' is not type character.'
        call writloga('default',0,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
         endif
      enddo
      if (ierr .ne. 0) return        
C
C     ******************************************************************
C
C     Open the input file.
C
      fname = cmsgin(3)(1:icharlnf(cmsgin(3)))
      inquire(file=fname,exist=iexist,form=ftype,err=9999)
      if (.not.iexist) then 
        ierr = -1
c
c       ERROR: Input file does not exist
c
        write(logmess,"(a,a)")
     1   'ERROR: Input file does not exist: ', fname(1:icharlnf(fname))
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
        return
      endif
      if (ftype(1:6).eq.'UNFORM') then
c
c       ERROR: Binary or Unformatted file types not supported
c
        write(logmess,"(a,a)")
     1     'ERROR: File is of type binary or unformatted: ',
     2      fname(1:icharlnf(fname))
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: Only ascii file type supported'
        call writloga('default',0,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
        return
      endif
c
c     Open the file and assign unit number, iunit
c
      iunit = -1
      call hassign(iunit,fname,ierr)
C
C     ******************************************************************
C
C     Parse MO name
C
      if(nwds .lt. 4)then
         cmoname = '-def-'
      else
         cmoname = cmsgin(4)
      endif
      lcmoname=icharlnf(cmoname)
      if (cmoname(1:lcmoname).eq. '-def-') then
         call cmo_get_name(cmoname,ierr)
         if (ierr.ne.0) then
c
c         error no current MO exists
c
          write(logmess,"(a)")
     1     'ERROR: -def- specified for MO name'
          call writloga('default',1,logmess,0,ierr)
          write(logmess,"(a)")
     1     'ERROR: No MO exists'
          call writloga('default',1,logmess,0,ierr)
          write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
          call writloga('default',0,logmess,1,ierr)
          ierr = -1
          return
         endif
      lcmoname=icharlnf(cmoname)
      endif
      call cmo_exist(cmoname,ierr)
      if (ierr.ne.0) then
c
c           error cmoname does not exist
c
        write(logmess,"(a,a)")
     1     'ERROR: Requested mesh object does not exist: ',
     2      cmoname(1:icharlnf(cmoname))
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
        return
      endif
      call cmo_select(cmoname,ierr)
C
C     ******************************************************************
C
C     Parse the optional attribute name
C
      if ((nwds .lt. 5) .or. (cmsgin(5)(1:5) .eq. '-def-')) then
         att_name = 'imt'
      else
         att_name = cmsgin(5)(1:icharlnf(cmsgin(5)))
      endif
C
C     ******************************************************************
C
C     Finished with command line input tokens
C
C     ******************************************************************
C
      call cmo_get_info('nnodes',cmoname,nnodes,ilen,itp,ierr)
c
      call cmo_get_attparam(att_name,cmoname,
     1           index,ctype,crank,clen,cinter,cpers,cio,ierr)

c
      if(ierr .ne. 0)then
c
c     Need to create the attribute
c
         dotask_command = 'cmo/addatt/' //
     >        cmoname(1:icharlnf(cmoname)) //
     >        '/' //
     >        att_name(1:icharlnf(att_name)) //
     >        '/vint/scalar/nnodes/linear/permanent/agl/0/' //
     >        ' ; finish '
         call dotask(dotask_command,ierr)
c
c     Check that attribute att_name is of type vint
c
      else
      if(ctype(1:4) .ne. 'VINT') then
      print *, 'ctype ', ctype
c
c     error not type integer
c
        write(logmess,"(a,a,a)")
     1     'ERROR: Attribute requested is not of type integer: ',
     2      att_name(1:icharlnf(att_name)), ctype
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
        return
      endif
      endif

      call cmo_get_info(att_name,cmoname,ipiatt,ilen,itype,ierr)
c
      if (ierr .ne. 0)then
c
c        error, att_name does not exist
c
        write(logmess,"(a,a)")
     1     'ERROR: Attribute requested does not exist: ',
     2      att_name(1:icharlnf(att_name))
        call writloga('default',1,logmess,0,ierr)
        write(logmess,"(a)")
     1     'ERROR: No action, RETURN'
        call writloga('default',0,logmess,1,ierr)
        ierr = -1
        return
      endif
C
C     ******************************************************************
C
c
c     Enough has gone right. Allocate a temporary work array.
c
c     On error, no more return statement. Only go to so that clean up
c     and memory release occurs.
c
      call mmgetblk('iwork',isubname,ipiwork,nnodes,1,ierr)
c
C
C     ******************************************************************
C
C     Some screen output.
C
      write(logmess,"(a)")
     1 ' zone/zonn   id_zone       name      '//
     2 '      #nodes  index__min   index_max'
      call writloga('default',1,logmess,0,ierr)
      write(logmess,"(a)")
     1 '-------------------------------------'//
     2 '--------------------------------------'
      call writloga('default',0,logmess,0,ierr)

C     ******************************************************************
C
c     Start reading the input file, keep reading until end of file.
c
c     Start by reading until the keyword 'zone' or 'zonn' is found.
c
      iflag1 = 0
      icount = 0
      dowhile(iflag1.eq.0)
      icount = icount + 1
      read(iunit, '(a)',end=9998, err=9999) input_msg
      lenparse = len(input_msg)
      call parse_string2(lenparse, input_msg,
     1                       imsg, msgt, xmsg, cmsg, nwds)
      if(      (nwds .gt. 0).and.
     1         (msgt(1) .eq. 3).and.
     1         (cmsg(1)(1:3).eq.'zon'))then
         if(cmsg(1)(1:4) .eq. 'zone') then
            cif_zone_zonn = 'zone'
c
c           Initialize array to zero
c
            do i = 1, nnodes
               iatt(i) = 0
            enddo
         elseif (cmsg(1)(1:4) .eq. 'zonn') then
            cif_zone_zonn = 'zonn'
c
c          No action required
c
         else

         endif
c
c     Found zone/zonn keyword. Now read the zone id number etc.
c     until a blank line is found.
c
         iflag2 = 0
         dowhile(iflag2.eq.0)
         read(iunit, '(a)',end=9998, err=9999) input_msg
         call parse_string2(lenparse, input_msg,
     1                       imsg, msgt, xmsg, cmsg, nwds)
         if(nwds .ne. 0)then
           id_zone = imsg(1)
           if((nwds .gt. 1) .and. (msgt(2) .eq. 3))then
              cid_zone = cmsg(2)(1:icharlnf(cmsg(2)))
           else
              cid_zone = 'NO_NAME'
           endif
           read(iunit, '(a)',end=9998, err=9999) input_msg
           call parse_string2(lenparse, input_msg,
     1                       imsg, msgt, xmsg, cmsg, nwds)
           if((msgt(1) .eq. 3) .and. (cmsg(1) .eq. 'nnum'))then
              read(iunit,*) nnum
              if (nnum .gt. nnodes) then
                 write(logmess,"(a)")
     1     'ERROR: Number of entries is greater than mesh object nnodes'
                 call writloga('default',1,logmess,0,ierr)
                 write(logmess,"(a,i10,a,i10)")
     1           'ERROR: nnum = ',nnum, ' nnodes = ',nnodes
                 call writloga('default',0,logmess,0,ierr)
                 go to 9999
              endif
              if(nnum .gt. 0)then
                 read(iunit,*,end=9998, err=9999) (iwork(i), i=1,nnum)
                 iwork_min =  1.e9
                 iwork_max = -1.e9
                 do i = 1, nnum
                    iwork_min = min(iwork(i), iwork_min)
                    iwork_max = max(iwork(i), iwork_max)
                 enddo
                 if((iwork_min .lt. 1) .or. (iwork_max .gt. nnodes))then
                    write(logmess,"(a)")
     1             'ERROR: index values out of range  '
                   call writloga('default',1,logmess,0,ierr)
                   write(logmess,"(a,i10,a,i10)")
     1             'ERROR: index min = ',iwork_min, 
     2             ' index max = ',iwork_max
                 call writloga('default',0,logmess,0,ierr)
                 go to 9999
                 endif
                 do i = 1, nnum
                    iatt(iwork(i)) = id_zone
                 enddo
                 write(logmess,"(a8,2x,i10,4x,a15,3(i10,2x))")
     1        cif_zone_zonn, id_zone,cid_zone,nnum,iwork_min,iwork_max
                 call writloga('default',0,logmess,0,ierr)
              endif
           else
c
c     Error, only support nnum option in zone/zonn macro
c
           write(logmess,"(a,a)")
     1        'ERROR: Found unsupported keyword after zone/zonn keyword:  ',
     2         cmsg(1)(1:icharlnf(cmsg(1)))
           call writloga('default',1,logmess,0,ierr)
           write(logmess,"(a)")
     1     'ERROR: Continue looking for additional zone/zonn keywords'
           call writloga('default',0,logmess,1,ierr)
           iflag2 = 1
           endif
         else
c
c          Found a blank line. Exit from zone/zonn macro but continue
c          parsing the file until end of file is reached.
c
           iflag2 = 1
         endif
         enddo
      endif

 9998 iflag1 = 1
      enddo

 9999 continue

      write(logmess,"(a)")
     1 '-------------------------------------'//
     2 '--------------------------------------'
      call writloga('default',0,logmess,1,ierr)

      call mmrelprt(isubname,ierr)
      if(iunit .gt. 0) close(iunit)
      ierr = 0
      return
      end
      
