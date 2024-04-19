      subroutine dotask(task_buff,ierror)
C#######################################################################
C
C     PURPOSE -
C        execute commands
C
C        include file defines globals for command parsing and buffer
C        commands_lg.h:C  cmd_buffer is a the command buffer
C        commands_lg.h: common /ccommands_lg/ clevels, cmd_buff, cmd_temp,
C        commands_lg.h: character*16384 cmd_buff, cmd_temp, command
C
C     INPUT
c        task_buff character string of commands to be added
c        to command_stack
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log: dotask.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Mon Feb 22 15:05:20 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'

C     parameters
      character*(*) task_buff
      integer ierror 

C     local variables
      integer ierr
c
      ierror=0
      nlevels=nlevels+1
      jlevels(nlevels)=0
      clevels(nlevels)=' '
      clevels(nlevels)='internal_lg'
      cmd_buff=' '
      cmd_buff=task_buff

      call pack_command_lg(ierr)
      if (ierr .ne. 0)  then
          call x3d_error('dotask:','pack_command_lg')
          ierror = ierror + 1
      endif
      call push_command_lg(ierr)
      if (ierr .ne. 0) then 
          call x3d_error('dotask:','push_command_lg')
          ierror = ierror + 1
      endif
      call control_command_lg(ierr)
      if (ierr .ne. 0) then
          call x3d_error('dotask:','control_command_lg')
          ierror = ierror + 1
      endif


      return
      end
      
