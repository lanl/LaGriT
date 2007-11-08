      subroutine dotask(task_buff,ierror)
C#######################################################################
C
C     PURPOSE -
C
C        execute commands
C     INPUT
c        task_buff character string of commands to be added
c        to command_stack
c
C     OUTPUT
C        ierror -- error flag (ierror=0 means no errors)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/dotask.f_a  $
CPVCS    
CPVCS       Rev 1.0   Mon Feb 22 15:05:20 1999   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      include 'commands_lg.h'
      character*(*) task_buff
      integer ierror
c
      ierror=0
      nlevels=nlevels+1
      jlevels(nlevels)=0
      clevels(nlevels)=' '
      clevels(nlevels)='internal_lg'
      cmd_buff=' '
      cmd_buff=task_buff
      call pack_command_lg(ierror)
      call push_command_lg(ierror)
      call control_command_lg(ierror)
      return
      end
      
