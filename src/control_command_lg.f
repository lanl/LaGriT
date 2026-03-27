      subroutine control_command_lg(ierror)
C#######################################################################
C
C     PURPOSE -
C
C        execute commands
C     INPUT
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: control_command_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.14   11 Apr 2002 15:29:56   gable
CPVCS    Added support for define/VAR/remove.
CPVCS    This allows one to release/remove a string so that
CPVCS    it no longer will be substituted with a definition
CPVCS    defined in a previous define command.
CPVCS    
CPVCS       Rev 1.13   25 Mar 2002 16:55:10   dcg
CPVCS    replace definitions in the case of reusing names
CPVCS    
CPVCS       Rev 1.12   08 Jun 2000 13:42:24   dcg
CPVCS    echo comments to outx3dgen
CPVCS
CPVCS       Rev 1.11   17 Feb 2000 21:37:22   jtg
CPVCS    added variable to count number of times "unexpected eof" hit
CPVCS    and stop code if number too great (currently hardwired at 1000).
CPVCS
CPVCS       Rev 1.10   Tue Aug 31 11:13:14 1999   dcg
CPVCS    check for an empty loop
CPVCS
CPVCS       Rev 1.9   Tue Jul 13 14:58:48 1999   dcg
CPVCS    more changes to handle comments correctly
CPVCS
CPVCS       Rev 1.8   Tue Jul 13 14:25:26 1999   dcg
CPVCS    skip comments that come in as dotasks
CPVCS
CPVCS       Rev 1.7   Wed Jun 23 14:05:58 1999   dcg
CPVCS    fix error in command stack management
CPVCS
CPVCS       Rev 1.6   Tue Jun 22 14:27:28 1999   dcg
CPVCS    allow infile from dotask to work
CPVCS
CPVCS       Rev 1.5   Wed Mar 31 16:45:06 1999   dcg
CPVCS    revert to interactive mode if stack error is encountered
CPVCS
CPVCS       Rev 1.4   Wed Mar 31 14:42:14 1999   dcg
CPVCS    set msgtype to zero before calling parse_string
CPVCS
CPVCS       Rev 1.3   Fri Mar 05 08:39:08 1999   dcg
CPVCS    change pack_command_lg to look for strings of blanks that
CPVCS    may or maynot be preceeded by other delimeters and
CPVCS    delete unnecessary blanks
CPVCS
CPVCS       Rev 1.2   Tue Feb 23 13:16:06 1999   dcg
CPVCS    handle ; correctly on read in lines
CPVCS
CPVCS       Rev 1.1   Mon Feb 22 20:46:18 1999   nnc
CPVCS    Deleted duplicate type specification.
CPVCS
CPVCS       Rev 1.0   Mon Feb 22 16:06:18 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer ierror

      integer icscode,nwds,jname,istrt,iend,i,nloop
      integer imsgout(maxcmd_args),msgtype(maxcmd_args),icharlnf,
     *  len1,len2,j,k,imatch,nremove,ioff,iunit
      real*8 xmsgout(maxcmd_args)
      character*32 cmsgout(maxcmd_args)
      character*32 isubname
c
CCCCCCCCCCCCCC
C begin

      ierror=0
      isubname="control_command_lg"

c  access definitions list
      call mmfindbk('definition',initname,ipdefinition,i,icscode)
c
c  check state and get next command from stack
c
100   if(clevels(nlevels)(1:11).ne.'internal_lg') then
         if(last_char.le.0.or.
     *       clevels(nlevels).ne.'interactive_lg') then
             call read_command_lg(ierror)
             call push_command_lg(ierror)
         endif
      endif
      command=' '
      error_msg=' '
      call pop_command_lg(ierror)
      nloop=(len_cmd+79)/80
      istrt=1
      iend=min(80,len_cmd)
      do i=1,nloop
         write(error_msg,'(a)') command(istrt:iend)
         call writloga('tty',0,error_msg,0,icscode)
         call writloga('bat',0,error_msg,0,icscode)
         if(clevels(nlevels)(1:11).ne.'internal_lg')
     *    call writloga('log',0,error_msg,0,icscode)
         istrt=istrt+80
         iend=min(len_cmd,iend+80)
      enddo
      if (command.eq.' '.or.ierror.ne.0) then
         write(error_msg,9000)
 9000    format ('empty command stack or other stack error',
     *       ' revert to previous mode')
         call writloga('default',0,error_msg,0,icscode)
         if(clevels(nlevels).eq.'internal_lg') then
           nlevels=nlevels-1
         else
           nlevels=1
           clevels(nlevels)='interactive_lg'
         endif
         go to 100
      endif
c process finish command
      if(command(1:6).eq.'finish') then

         if( clevels(nlevels).ne.'interactive_lg'.and.
     *       clevels(nlevels).ne.'internal_lg') then
           close(jlevels(nlevels))
         endif
         nlevels=nlevels-1

         if(nlevels.eq.0) then
           write(error_msg,9001)
 9001      format ('LaGriT successfully completed')
           call writloga('default',0,error_msg,0,icscode)
           go to 9999
         endif
         if(clevels(nlevels+1)(1:11).eq.'internal_lg') go to 9999
         go to 100
      endif

c parse command
      do i=1,maxcmd_args
         msgtype(i)=0
      enddo
      call parse_string(imsgout,msgtype,xmsgout,cmsgout,nwds)
c  handle file change commands
      if(cmsgout(1)(1:6).eq.'infile'.or.
     *   cmsgout(1)(1:5).eq.'input') then
         if (nwds.lt.2) then
            ierror=1
            write(error_msg,9002) cmsgout(1)(1:6)
 9002  format ('Error: file name missing from command : ',a)
            call writloga('default',0,error_msg,0,icscode)
            go to 100
         endif
c
c  expand definitions if found
         call substitute_defs_lg(msgtype,imsgout,xmsgout,cmsgout,
     *        nwds,ierror)
c
         nlevels=nlevels+1
         clevels(nlevels)=cmsgout(2)
         iunit=-1
         call hassign(iunit,clevels(nlevels),icscode)
         if (iunit.lt.0 .or. icscode.lt.0) then
           write(error_msg,'(a,i5)') 
     *     'hassign bad file unit : ',iunit
           call writloga('default',0,error_msg,0,icscode)
           write(error_msg,'(a)') 'Error: infile not used.' 
           call writloga('default',0,error_msg,1,icscode)
         endif
         jlevels(nlevels)=iunit
         go to 100

c  handle define commands
      elseif(cmsgout(1)(1:6).eq.'define') then
         if(ndefinitions+2.ge.maxdefinitions) then
            ierror=1
            write(error_msg,9003) cmsgout(2)(1:32)
 9003       format ('maximum number of definitions exceeded : ',a)
            call writloga('default',0,error_msg,0,icscode)
            go to 100
         endif
c
c  look for existing definition and remove name
c
         len1=icharlnf(cmsgout(2))
         imatch=0
         do j=1,ndefinitions
           len2=icharlnf(definitions_name(j))
           if (len1.eq.len2) then
              if (definitions_name(j)(1:len1).eq.cmsgout(2)(1:len1))
     *           then
                 imatch=1
c
c  compress lists
c 
                 nremove=definitions_off(j+1)-definitions_off(j)
                 ioff=definitions_off(j)
                 do k=1,definitions_off(ndefinitions+1)-ioff
                    definition(ioff+k-1:ioff+k-1)=
     *                 definition(definitions_off(j+1)+k-1:
     *                            definitions_off(j+1)+k-1 )
                 enddo
                 lastchar_def=lastchar_def-(definitions_off(j+1)-
     *              definitions_off(j))
                 do k=j+1,ndefinitions
                    definitions_name(k-1)=definitions_name(k)
                 enddo 
                 do k=j+1,ndefinitions
                    definitions_off(k)=definitions_off(k+1)-nremove
                 enddo       
              endif
           endif
         enddo         
       if( (nwds .gt. 2) .and. 
     *    ((cmsgout(3)(1:7) .eq. 'release') .or.
     *     (cmsgout(3)(1:6) .eq. 'remove' )))then
c
c      Definition removed above, reduce list by one.
c     
             ndefinitions = ndefinitions - 1
       else
c
c  make new definition entry
c
         if(imatch.eq.0) ndefinitions=ndefinitions+1
         definitions_name(ndefinitions)=cmsgout(2)
         len1=icharlnf(cmsgout(3))
         definition(lastchar_def+1:lastchar_def+len1)=cmsgout(3)
         definitions_off(ndefinitions)=lastchar_def+1
         lastchar_def=lastchar_def+len1
         definitions_off(ndefinitions+1)=lastchar_def+1
       endif
c
      else
c
c  expand definitions if found
         call substitute_defs_lg(msgtype,imsgout,xmsgout,cmsgout,
     *        nwds,ierror)
c
c  go to msgtty to process other commands
c
         ierror = 0
         call msgtty(imsgout,msgtype,xmsgout,cmsgout,nwds,ierror)
         if (ierror.ne.0) then
              ierror=1
              write(error_msg,9004) command(1:80)
 9004         format ('error in command : ',a80)
              call writloga('default',0,error_msg,0,icscode)
              go to 100
         endif
      endif
      go to 100
 9999 return
      end
c
      subroutine substitute_defs_lg(msgtype,imsgout,xmsgout,cmsgout,nwds
     *  ,ierror)
C#######################################################################
C
C     PURPOSE -
C
C        substitute definitions into cmsgout if found
C     INPUT
c
c        msgtype  type of input token
c        imsgout  integer input tokens
c        xmsgout  real input tokens
c        cmsgout  character input tokens
c        nwds     number of input tokens
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: control_command_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer ierror,nwds,msgtype(nwds),imsgout(nwds)
      character*32 cmsgout(*)
      integer i,j,len1,len2,icharlnf,icscode,i1
      real*8 xmsgout(nwds),x1
      logical isintgrvar, isrlvar
c  access definitions list
      call mmfindbk('definition',initname,ipdefinition,i,icscode)
c
      ierror=0
      do i=1,nwds
         if(msgtype(i).eq.3) then
            len1=icharlnf(cmsgout(i))
            do j=1,ndefinitions
               len2=icharlnf(definitions_name(j))
               if (len1.eq.len2) then
                 if (definitions_name(j)(1:len1).eq.cmsgout(i)(1:len1))
     *              then
                    cmsgout(i)=definition(definitions_off(j):
     *                                    definitions_off(j+1)-1)
                    len1=icharlnf(cmsgout(i))
C
c  change type if necessary
c
                    call isinteger_lg(cmsgout(i),len1,i1,isintgrvar)
                    if(isintgrvar) then
                       msgtype(i)=1
                       imsgout(i)=i1
                    else
                       call isrl_lg(cmsgout(i),len1,x1,isrlvar)
                       if(isrlvar) then
                           msgtype(i)=2
                           xmsgout(i)=x1
                       endif
                    endif
                 endif
               endif
            enddo
         endif
      enddo

      return
      end
      subroutine get_define_variable_lg
     *           (cstring_in,msgtype,imsgout,xmsgout,cmsgout,ierror)
C#######################################################################
C
C     PURPOSE -
C
C        For internal subroutine calls within LaGriT
C        If a known character string is defined using the 'define' command, 
C        the input is the know string, cstring_in. This subroutine will then
C        return the type (integer=1,real=2,char=3) and value.
C
C        If the input string, cstring_in, does not exist, ierror will be returned
C        as a non-zero value.
C     INPUT
C        cstring_in input string that matches a token defined in the 'define' command
C     OUTPUT
C        msgtype  type of input token
C        imsgout  integer input tokens
C        xmsgout  real input tokens
C        cmsgout  character input tokens
C        ierror -- error flag (ierror=0 means no errors)
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer ierror,msgtype,imsgout
      character*32 cmsgout, cstring_in
      integer i,j,len1,len2,icharlnf,icscode,i1
      real*8 xmsgout,x1
      logical isintgrvar, isrlvar
c  access definitions list
      call mmfindbk('definition',initname,ipdefinition,i,icscode)
c

c      print *, cstring_in
c      print *, msgtype,imsgout,xmsgout,cmsgout

c      do j=1,ndefinitions
c         print *, j, definitions_name(j), definitions_off(j)
c         print *, definition(definitions_off(j):definitions_off(j+1)-1)
c      enddo

      ierror=1
      len1=icharlnf(cstring_in)
        do j=1,ndefinitions
           len2=icharlnf(definitions_name(j))
           if (len1.eq.len2) then
              if (definitions_name(j)(1:len1).eq.cstring_in(1:len1))then
                 ierror=0
                 cmsgout=definition(definitions_off(j):
     *                              definitions_off(j+1)-1)
                 len1=icharlnf(cmsgout)
C
c                change type if necessary
c
                 call isinteger_lg(cmsgout,len1,i1,isintgrvar)
                 if(isintgrvar) then
                    msgtype=1
                    imsgout=i1
                  else
                     call isrl_lg(cmsgout,len1,x1,isrlvar)
                     if(isrlvar) then
                        msgtype=2
                        xmsgout=x1
                     endif
                  endif
               endif
            endif
         enddo

c      print *, cstring_in
c      print *, msgtype,imsgout,xmsgout,cmsgout

      return
      end
c
      subroutine push_command_lg(ierror)
C#######################################################################
C
C     PURPOSE -
C
C        put the command in cmd_buff into cmd_stack
C     INPUT
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: control_command_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer ierror,nloop,n,ib,ie,nmove,icscode
c      pointer (ipcmd_stack,cmd_stack)
c      character*100000 cmd_stack
c
c  access cmd_stack
      call mmfindbk('cmd_stack',initname,ipcmd_stack,n,icscode)
c
c  check that max length is not exceeded
c
      if(last_char+len_buff.gt.maxlen_stack) then
         maxlen_stack=maxlen_stack+4096
         call mmincblk('cmd_stack',initname,ipcmd_stack,1024,
     *     icscode)
      endif
c
c  make room for cmd_buff at front of cmd_stack
C
      nmove=len_buff
      nloop=(nmove+maxlen_buff-1)/maxlen_buff
      ib=1
      ie=last_char
      if(ie.eq.0) nloop=0
      do n=nloop,1,-1
         nmove=ie-ib+1
         cmd_temp(1:nmove)=cmd_stack(ib:ie)
         cmd_stack(ib+len_buff:ie+len_buff)=cmd_temp(1:nmove)
         ib=ib-maxlen_buff
         ie=ie-maxlen_buff
         if(ib.le.0) ib=1
      enddo
      cmd_stack(1:len_buff)=cmd_buff(1:len_buff)
      ierror=0
      last_char=last_char+len_buff
 9999 return
      end
      subroutine pop_command_lg(ierror)
C#######################################################################
C
C     PURPOSE -
C
C        pop the top command from cmd_stack into command
C     INPUT
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: control_command_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer ierror,nloop,n,ib,ie,nmove,icscode,idx,iend
c      pointer (ipcmd_stack,cmd_stack)
c      character*100000 cmd_stack
c
c  access cmd_stack
      call mmfindbk('cmd_stack',initname,ipcmd_stack,n,icscode)
c
c  check that stack is not empty
c
 5    len_cmd=0
      if(last_char.le.0) then
         ierror=1
         write(error_msg,9000)
 9000    format (' command stack empty ',
     *     a)
         call writloga('default',0,error_msg,0,icscode)
         go to 9999
      endif
c
c  get the top command off cmd_stack
C
      idx=0
      ierror=0
      idx=index(cmd_stack(1:maxlen_stack),';')
      if(idx.eq.0) then
         idx=last_char
         len_cmd=idx
      elseif (idx.gt.0) then
         len_cmd=idx-1
      else
         ierror=1
         go to 9999
      endif
      command(1:len_cmd)=cmd_stack(1:len_cmd)
      nmove=last_char-idx
      nloop=(nmove+maxlen_buff-1)/maxlen_buff
      ib=1
      ie=min(nmove,maxlen_buff)
      do n=1,nloop
         cmd_temp(1:ie)=cmd_stack(ib+idx:ie+idx)
         cmd_stack(ib:ie)=cmd_temp(1:ie)
         ib=ib+maxlen_buff
         ie=ie+maxlen_buff
         if(ie.gt.last_char) ie=last_char
      enddo
      ierror=0
      last_char=last_char-idx
      cmd_stack(last_char+1:len_cmd)=' '
cTAM  cmd_stack(last_char+1:maxlen_stack)=' '
c
c  get rid of leading blanks in command
c
      if(command(1:1).ne.' ') go to 9998
      cmd_temp=' '
      cmd_temp=command
      iend=1
      dowhile (command(iend+1:iend+1).eq.' ')
        iend=iend+1
      enddo
      command(1:len_cmd-iend)=cmd_temp(iend+1:len_cmd)
      command(len_cmd-iend+1:len_cmd)=' '
      len_cmd=len_cmd-iend
c
c  skip commands that are comments
c
 9998 if(command(1:1).eq.'*'.and.last_char.ne.0) go to 5
 9999 return
      end
c
      subroutine pack_command_lg(ierror)
C#######################################################################
C
C     PURPOSE -
C
C        get rid of leading, trailing and embedded blanks
C            in cmd_buff
C        make sure it terminates with a ';'
C     INPUT
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: control_command_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer len1,i,istrt,iend,ierror,idelb
c
      ierror=0
C
C  look for trailing blanks
C
      len1=maxlen_buff
      len_buff=len1
      do i=len1,1,-1
        if(cmd_buff(i:i).eq.' ') then
           len_buff=len_buff-1
        else
           go to 100
        endif
      enddo
C
C  look for terminal ';'
C
 100  len1=len_buff
      if (cmd_buff(len1:len1).ne.';') then
         cmd_buff(len1+1:len1+1)=';'
         len_buff=len_buff+1
      endif
c
c  look for leading blanks
c
      len1=len_buff
      istrt=1
      do i=1,len1
         if(cmd_buff(i:i).eq.' ') then
           istrt=istrt+1
         else
           go to 200
         endif
      enddo
 200  if(istrt.gt.1) then
         do i=istrt,len1
            cmd_buff(i-istrt+1:i-istrt+1)=cmd_buff(i:i)
         enddo
         len_buff=len_buff-istrt+1
      endif
C
C  look for embedded blanks
c  look also for delimiter followed or preceeded by a blank
c  and remove these unneeded blanks
c
      len1=len_buff
      istrt=0
 250  i=index(cmd_buff(istrt+1:len1),' ')
      if(i.eq.0) go to 9999
      istrt=i+istrt
      if (istrt.le.0.or.istrt.gt.len1) go to 9999
c
c  get length of blank string
c
      idelb=1
      iend=istrt
      do while (cmd_buff(iend+1:iend+1).eq.' ')
         iend=iend+1
         idelb=idelb+1
      enddo
c
c  if string of blanks is preceeded or followed
c  by a delimeter - get rid of all the blanks
c  otherwise keep one as the blank is the delimeter
c
      if(cmd_buff(istrt-1:istrt-1).ne.','.and.
     *   cmd_buff(istrt-1:istrt-1).ne.'/'.and.
     *   cmd_buff(istrt-1:istrt-1).ne.';'.and.
     *   cmd_buff(istrt-1:istrt-1).ne.'='.and.
     *   cmd_buff(iend+1:iend+1).ne.','.and.
     *   cmd_buff(iend+1:iend+1).ne.'/'.and.
     *   cmd_buff(iend+1:iend+1).ne.'='.and.
     *   cmd_buff(iend+1:iend+1).ne.';') then
         istrt=istrt+1
         idelb=idelb-1
      endif
 300  if(idelb.gt.0) then
         do i=istrt,len1-(iend+1-istrt)
            cmd_buff(i:i)=cmd_buff(i+(iend+1-istrt):i+(iend+1-istrt))
         enddo
         len1=len1-(iend+1-istrt)
         len_buff=len1
      endif
      go to 250
 9999 len_buff=len1
      return
      end
c
      block data data_read_command_lg
C#######################################################################
C
C     PURPOSE -
C
C        initialize n_hits, variable to avoid inf loop in read_command_lg
C
C#######################################################################
C
      implicit none
      integer n_hits
      common /common_read_command_lg/ n_hits
      save /common_read_command_lg/
      data n_hits / 0 /
      end
 
      subroutine read_command_lg(ierror)
C#######################################################################
C
C     PURPOSE -
C
C        read a file for next input lines
C
C     INPUT
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: control_command_lg.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      integer ifile,jstrt,ib,ie,icscode,ierror
      integer n_hits
      common /common_read_command_lg/ n_hits
      save /common_read_command_lg/
c
      ierror=0
c
c  check state - if interactive prompt for command
c
      if(clevels(nlevels)(1:14).eq.'interactive_lg') then
         write(*, 9000)
 9000    format (' Enter a command')
      endif
c
c  get unit number
c
      ifile=jlevels(nlevels)
c
c  read a line and look for continuation
c
      jstrt=0
 5    if(clevels(nlevels)(1:14).eq.'interactive_lg') then
         read(*,'(a80)',end=1000) cmd_buff
      else
         read(ifile,'(a80)',end=1000) cmd_buff
      endif
c
c  check for comment - if so print and go to next command
c
      if((cmd_buff(1:1).eq.'*').or.(cmd_buff(1:1).eq.'#')) then
         write(error_msg,'(a)')cmd_buff(1:80)
         call writloga('default',0,error_msg,0,icscode)
         if (clevels(nlevels)(1:14).eq.'interactive_lg')
     *      write(*,9000)
         go to 5
      endif
c
c  check for blank line - if so print and go to next command
c
      if(cmd_buff(1:80).eq.' ') then
         write(*,'(a)')cmd_buff(1:80)
         go to 5
      endif
c
  10  jstrt=index(cmd_buff,'&')
      if(jstrt.ne.0) then
c
c  found continuation - leave one blank at end as separator
c  tokens cannot be split between lines
c
         cmd_buff(jstrt:jstrt)=' '
         dowhile(cmd_buff(jstrt-2:jstrt-2).eq.' ')
            jstrt=jstrt-1
         enddo
c
c  read continuation lines
c
         if(clevels(nlevels)(1:14).eq.'interactive_lg') then
            read(*,'(a80)',end=1000) cline
         else
            read(ifile,'(a80)',end=1000) cline
         endif
         if(cline.eq.' ') go to 9999
         ib=1
         ie=80
         dowhile (cline(ib:ib).eq.' ')
           ib=ib+1
         enddo
         dowhile (cline(ie:ie).eq.' ')
           ie=ie-1
         enddo
         if(jstrt+ie-ib.gt.maxlen_buff) then
            write(error_msg,9001)
 9001       format('max command buffer length exceeded-command ignored')
            call writloga('default',0,error_msg,0,icscode)
            ierror=1
            cmd_buff=' '
            go to 9999
         endif
         cmd_buff(jstrt:jstrt+ie-ib)=cline(ib:ie)
         go to 10
      endif
c
c  get rid of extra blanks
c
      call pack_command_lg(ierror)
      go to 9999
 1000 write(error_msg,9002)
 9002 format('unexpected eof-command ignored')
      call writloga('default',0,error_msg,0,icscode)
      n_hits=n_hits+1
      if (n_hits.gt.1000) stop 'maximum number hit, code aborting'
      ierror=1
      cmd_buff=' '
      go to 9999
9999  return
      end
