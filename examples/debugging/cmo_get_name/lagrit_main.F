      program lagrit_main
      implicit none
      character(len=32) :: mode, logfile, outfile
      character(len=80) :: cmo
      integer ierror

      mode = 'noisy'

      call cmo_get_name(cmo, ierror)

      print*,'cmo = ', cmo
      print*,'ierror = ',ierror

      stop
      end program lagrit_main
