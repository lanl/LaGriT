      subroutine dotask_test(task_buff,ierror)
C#######################################################################
C
C     PURPOSE -
C
C        Test string passing from cpp 
C        Do not execute commands, print info 
C
c        task_buff character string of commands to be added
c        to command_stack
c
C#######################################################################
C
      implicit none
      include 'commands_lg.h'

C     global defines for command parsing and buffer
C     check but do not set in this test routine
C     commands_lg.h:C  cmd_buffer is a the command buffer
C     commands_lg.h: common /ccommands_lg/ clevels, cmd_buff, cmd_temp,
C     commands_lg.h: character*16384 cmd_buff, cmd_temp, command
C     last_char is the location of the last character in cmd_stack
c     len_buff is the length of cmd_buffer
c     len_cmd is the length of a single command

C     parameters
      character*(*) task_buff
      integer ierror

C     local variables
      integer ierr, icharlnf, ilen
      character*132 cbuf
c
      ierror=0
      
      print*,""
      print*,"Begin FORTRAN dotask_test"
      print*,"parameter integer size: ", sizeof(ierr)

      ilen = icharlnf(task_buff)
      print*,"  received string: ",task_buff(1:ilen)
      print*,"  length: ",ilen
      print*,""
      print*,"  check GLOBAL cmd_buff set in dotask calls."
      print*,"  cmd_buff: ",cmd_buff(1:len_buff)
      print*,"  length:",len_buff

      print*,"End FORTRAN dotask_test"
      print*,""

C -------------------------------------------------------
C     do not process the buffer into command options
C
C     call pack_command_lg(ierr)
C     if (ierr .ne. 0)  then
C         call x3d_error('dotask:','pack_command_lg')
C         ierror = ierror + 1
C     endif
C     call push_command_lg(ierr)
C     if (ierr .ne. 0) then
C         call x3d_error('dotask:','push_command_lg')
C         ierror = ierror + 1
C     endif
C     call control_command_lg(ierr)
C     if (ierr .ne. 0) then
C         call x3d_error('dotask:','control_command_lg')
C         ierror = ierror + 1
C     endif
C -------------------------------------------------------

      return
      end
      
