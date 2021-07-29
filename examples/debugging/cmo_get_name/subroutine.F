      subroutine cmo_get_name(cmo_name,ierror)
      implicit none

      character*(*) cmo_name
      integer ierror,len,icscode
      character*32 partname

      print*,'IN SUBROUTINE'

      cmo_name = '-notset-'
      ierror = -1

      return
      end


