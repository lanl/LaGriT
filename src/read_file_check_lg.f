C SUBROUTINE BEGIN ####################################################
      subroutine read_file_check_lg(cfile,cformat,nwds,ierr)
C #####################################################################
C
C     PURPOSE - Checks made by read (readdump.f) before a file is
C     opened, so that a wrong file name gives an ERROR instead of a
C     crash or a wrong read:
C
C       1 the file is a folder. Fortran opens a folder for reading
C         and the first read of it stops LaGriT (exit status 2, the
C         message only on the terminal). This is what a path gives:
C         read / avs / ../dir/file.inp / mo splits at each / into
C         read, avs, .., dir, file.inp, mo; the file name is '..'.
C       2 the command has more words than the format takes (avs 7,
C         lagrit 5, gmv and gmvfreeformat 5 (a 5th word, ascii, is in
C         old decks and is ignored), gocad 4); also what a path gives
C         when no word of it is a folder. Other formats are not
C         checked.
C
C     A file that does not exist is left to each reader, as before.
C
C     INPUT ARGUMENTS -
C        cfile   - the file name (word 3 of the command)
C        cformat - the format (word 2): avs, gmv, lagrit, ...
C        nwds    - the number of words in the command
C
C     OUTPUT ARGUMENTS -
C        ierr    - 0 read the file, 1 an ERROR was written and nothing
C                  is to be read
C
C #####################################################################
      implicit none
      character*(*) cfile,cformat
      integer nwds,ierr
C
      character*132 cmsg
      character*40 csyn
      integer lf,lfmt,maxw,ierrw
      logical isdir
C
      ierr = 0
      lf = len_trim(cfile)
      lfmt = len_trim(cformat)
      if(lf.eq.0 .or. lfmt.eq.0) return
      if(cfile(1:lf).eq.'-def-') return
C
C     1 a folder: the name followed by /. exists only for a folder
C
      inquire(file=cfile(1:lf)//'/.',exist=isdir)
      if(isdir) then
         ierr = 1
         write(cmsg,'(a,a,a)') 'ERROR read: "',cfile(1:lf),
     &      '" is a folder, not a file.'
         call writloga('default',1,cmsg,0,ierrw)
         call read_file_check_path_lg
         return
      endif
C
C     2 more words than the format takes
C
      maxw = 0
      csyn = ' '
      if(cformat(1:lfmt).eq.'avs') then
         maxw = 7
         csyn = 'file / mo_name / [points/cells/values]'
      elseif(cformat(1:lfmt).eq.'lagrit' .or.
     &       cformat(1:lfmt).eq.'lgt') then
         maxw = 5
         csyn = 'file / [mo_name] / [ascii|binary]'
      elseif(cformat(1:lfmt).eq.'gmv' .or.
     &       cformat(1:lfmt).eq.'gmvfreeformat') then
C        a 5th word (ascii or binary) is used in old decks; ignored
         maxw = 5
         csyn = 'file / [mo_name]'
      elseif(cformat(1:lfmt).eq.'gocad') then
         maxw = 4
         csyn = 'file / [mo_name]'
      endif
      if(maxw.gt.0 .and. nwds.gt.maxw) then
         ierr = 1
         write(cmsg,'(a,a,a,i0,a,i0,a)') 'ERROR read / ',
     &      cformat(1:lfmt),': ',nwds,' words, at most ',maxw,':'
         call writloga('default',1,cmsg,0,ierrw)
         write(cmsg,'(a,a,a,a)') '  read / ',cformat(1:lfmt),' / ',
     &      csyn(1:len_trim(csyn))
         call writloga('default',0,cmsg,0,ierrw)
         call read_file_check_path_lg
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine read_file_check_path_lg
C #####################################################################
C
C     PURPOSE - The rest of the ERROR of read_file_check_lg: why a
C     path does not work, what to do, and that nothing was read.
C
C #####################################################################
      implicit none
      character*80 cmsg
      integer ierrw
C
      write(cmsg,'(a)') '  LaGriT splits a command into words at / , ='
     &   //' and blanks, so a'
      call writloga('default',0,cmsg,0,ierrw)
      write(cmsg,'(a)') '  file name cannot hold a path such as '
     &   //'../dir/file.inp. Copy or'
      call writloga('default',0,cmsg,0,ierrw)
      write(cmsg,'(a)') '  link the file into this folder and give its'
     &   //' name. Nothing read.'
      call writloga('default',0,cmsg,1,ierrw)
      return
      end
