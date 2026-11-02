C     File read_line_lg.f: read one LaGriT input line of any length,
C     and check it against the limits in input_lines_lg.h.
C
C     Before these, read_command_lg read each line with '(a80)':
C     everything past column 80 was dropped without a word (a file
C     name, a keyword or a value could be cut and the command still
C     ran). A Fortran '(a)' read into a longer buffer would only move
C     the silent cut to the buffer length, so the line is read here
C     in pieces with non-advancing reads and its true length is
C     always known.
C
C SUBROUTINE BEGIN ####################################################
      subroutine read_line_lg(iunit,buf,lenbuf,ntot,lastnb,ios)
C #####################################################################
C
C     PURPOSE - Read one record (line) from unit iunit.
C
C     INPUT ARGUMENTS -
C        iunit  - the unit to read (5 is standard input)
C        lenbuf - how many characters buf can hold
C
C     OUTPUT ARGUMENTS -
C        buf    - the first lenbuf characters of the line, blank
C                 padded
C        ntot   - the length of the line: the position of its last
C                 character that is not a blank, a tab or a carriage
C                 return (0 for an empty line), however long the line
C        lastnb - that last character (blank if ntot = 0)
C        ios    - 0 a line was read; -1 end of file and no line
C
C #####################################################################
      implicit none
C
      integer iunit,lenbuf,ntot,ios
      character*(*) buf
      character*1 lastnb
C
      integer nr,i,npos,ierr
      character*256 chunk
      character*1 c
      logical anyread
C
      buf = ' '
      ntot = 0
      npos = 0
      lastnb = ' '
      ios = 0
      anyread = .false.
C
 10   continue
      nr = 0
      chunk = ' '
      read(iunit,'(a)',advance='no',size=nr,eor=20,end=30,
     &     iostat=ierr) chunk
      if(ierr .gt. 0) goto 30
      call read_line_keep_lg(chunk,nr,buf,lenbuf,npos,ntot,lastnb)
      anyread = .true.
      goto 10
C
C     end of the record: keep what this last read transferred
 20   continue
      call read_line_keep_lg(chunk,nr,buf,lenbuf,npos,ntot,lastnb)
      return
C
C     end of file: a last line without a newline still counts
 30   continue
      if(.not. anyread .and. nr .le. 0) then
         ios = -1
      else
         call read_line_keep_lg(chunk,nr,buf,lenbuf,npos,ntot,
     &        lastnb)
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine read_line_keep_lg(chunk,nr,buf,lenbuf,npos,ntot,
     &           lastnb)
C #####################################################################
C     PURPOSE - Append chunk(1:nr) to the line: store what fits in buf,
C     count all of it, and track the last character that is not a
C     blank, a tab or a carriage return.
C #####################################################################
      implicit none
      character*(*) chunk,buf
      integer nr,lenbuf,npos,ntot
      character*1 lastnb
C
      integer i
      character*1 c
C
      do i = 1, nr
         npos = npos + 1
         c = chunk(i:i)
         if(npos .le. lenbuf) buf(npos:npos) = c
         if(c.ne.' ' .and. c.ne.char(9) .and. c.ne.char(13)) then
            ntot = npos
            lastnb = c
         endif
      enddo
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine input_line_where_lg(iline,cwhere)
C #####################################################################
C     PURPOSE - "line N of FILE" for the current input level, for
C     messages ("line N of the input" for the terminal or standard
C     input).
C #####################################################################
      implicit none
      include 'commands_lg.h'
      integer iline
      character*(*) cwhere
      integer lenf,icharlnf
C
      if(clevels(nlevels)(1:14).eq.'interactive_lg') then
         write(cwhere,'(a,i0,a)') 'line ',iline,' of the input'
      else
         lenf = icharlnf(clevels(nlevels))
         write(cwhere,'(a,i0,a,a)') 'line ',iline,' of ',
     &        clevels(nlevels)(1:lenf)
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine check_line_len_lg(ntot,buf,iline,ibad)
C #####################################################################
C
C     PURPOSE - Check one command line (not a comment) against the
C     limits in input_lines_lg.h.
C
C        ntot <= LINE_NOTE_LEN                nothing
C        LINE_NOTE_LEN < ntot <= LINE_MAX_LEN a NOTE; the line is used
C        ntot > LINE_MAX_LEN                  an ERROR; ibad = 1 and
C                                             the caller skips the
C                                             command
C
C     INPUT ARGUMENTS -
C        ntot  - the length of the line (read_line_lg)
C        buf   - the line (at least its first 60 characters)
C        iline - its line number at the current input level
C
C     OUTPUT ARGUMENTS -
C        ibad  - 0 the line is used, 1 its command must be skipped
C
C #####################################################################
      implicit none
      include 'commands_lg.h'
      include 'input_lines_lg.h'
      integer ntot,iline,ibad
      character*(*) buf
C
      character*132 cmsg
      character*100 cwhere
      integer lw,ierrw
C
      ibad = 0
      if(ntot .le. LINE_NOTE_LEN) return
      call input_line_where_lg(iline,cwhere)
      lw = len_trim(cwhere)
      if(ntot .le. LINE_MAX_LEN) then
         write(cmsg,'(a,a,a,i0,a)') 'NOTE: ',cwhere(1:lw),' has ',
     &      ntot,' characters;'
         call writloga('default',0,cmsg,0,ierrw)
         write(cmsg,'(a)') '  LaGriT V3.3.4 and earlier used only the'
     &      //' first 80 of them.'
         call writloga('default',0,cmsg,0,ierrw)
         return
      endif
C
      ibad = 1
      write(cmsg,'(a,a,a)') 'ERROR: ',cwhere(1:lw),' is too long:'
      call writloga('default',1,cmsg,0,ierrw)
      write(cmsg,'(a,i0,a,i0,a)') '  ',ntot,' characters, more than ',
     &   LINE_MAX_LEN,'.'
      call writloga('default',0,cmsg,0,ierrw)
      write(cmsg,'(a,a,a)') '  It starts: "',buf(1:min(len(buf),50)),
     &   '..."'
      call writloga('default',0,cmsg,0,ierrw)
      write(cmsg,'(a)') '  Command skipped. Split a long command over'
     &   //' lines ending in &.'
      call writloga('default',0,cmsg,1,ierrw)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine skip_continuations_lg(iunit,line_had_amp)
C #####################################################################
C
C     PURPOSE - After a command is refused, read and drop the rest of
C     its continuation lines (each line holding an & continues it),
C     so they are not run as commands of their own. Counts the lines
C     it reads.
C
C     INPUT ARGUMENTS -
C        iunit        - the unit being read
C        line_had_amp - the refused line itself held an &
C
C #####################################################################
      implicit none
      include 'commands_lg.h'
      include 'input_lines_lg.h'
      integer iunit
      logical line_had_amp
C
      character*1024 cl
      integer ntot,ios
      character*1 lastnb
      logical more
C
      more = line_had_amp
      do while (more)
         call read_line_lg(iunit,cl,len(cl),ntot,lastnb,ios)
         if(ios .ne. 0) return
         nline_lg(nlevels) = nline_lg(nlevels) + 1
         if(ntot .eq. 0) return
         more = index(cl,'&').gt.0 .or. lastnb.eq.'&'
      enddo
      return
      end
