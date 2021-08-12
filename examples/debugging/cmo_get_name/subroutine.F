      subroutine initlagrit(mode,log_file,batch_file)
      implicit none
      character*(*)  mode, log_file, batch_file

      print*,'INITLAGRIT: RECIEVED DATA'
      print*,'==================='
      print*,'mode: >', mode, '<'
      print*,'logfile: >', log_file, '<'
      print*,'batchfile: >', batch_file, '<'

      return
      end

      subroutine cmo_get_name(cmo_name,ierror)
      implicit none

      character*(*) cmo_name
      integer ierror,len,icscode
      character*32 partname

      print*,'IN SUBROUTINE'

      cmo_name = '-notset-'
      ierror = 0

      return
      end



      subroutine dotask(task_buff,ierror)
      implicit none
      character*(*) task_buff
      integer ierror
      character*1233 cmd_buff

      cmd_buff = ' '
      cmd_buff = task_buff

      print*,'DOTASK: ', trim(cmd_buff)

      ierror = 0

      return
      end