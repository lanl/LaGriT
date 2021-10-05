      program lagrit_main
C
C #####################################################################
C
C     PURPOSE -
C
C        start up lagrit
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: lagrit_main.f,v $
C        Revision 2.00  2007/11/05 19:48:30  spchu
C        Import to CVS and change file name from adrivgen.f to lagrit_main.f
C
CPVCS       Rev 1.13   31 Jan 2019 13:29:00   drl
CPVCS    added basic command line parsing
CPVCS    
CPVCS       Rev 1.12   09 Mar 2000 10:07:06   dcg
CPVCS    changes for no storage block version of lagrit
CPVCS
CPVCS       Rev 1.11   Fri Oct 22 11:00:36 1999   dcg
CPVCS    get rid of subroutine fiximts
CPVCS
CPVCS       Rev 1.10   Mon Feb 22 16:16:10 1999   dcg
CPVCS    rewrite of command processing to allow for recursion
C ######################################################################
C
      implicit none
      integer ierror_return, i
      character(len=32) :: arg,arg_value,mode,logfile,outfile
C
      mode = 'noisy'
      logfile = ' '
      outfile = ' '

      i = 0
      do
C        Get command line flag
         call get_command_argument(i,arg)
         if (len_trim(arg) == 0) exit

C        Get flag value - will return empty string if it doesn't exist
         call get_command_argument(i+1,arg_value)

C        Verify that flag value is not empty and is not another flag
         if ((arg_value(1:1).ne.'-').and.(arg_value(1:1).ne.' ')) then
            i = i + 1

            select case (trim(arg))
            case ('-log')
               logfile = trim(arg_value)
            case ('-out')
               outfile = trim(arg_value)
            case ('-mode')
               mode = trim(arg_value)
               if (mode(1:6).ne.'silent') then 
                  if (mode(1:5).ne.'noisy') then
                     print*,'ERROR: Invalid option for -mode: ',mode
                     stop
                  endif
               endif
            end select
         endif

         i = i + 1
      end do

      ierror_return = 0
      call initlagrit(trim(mode),logfile,outfile)
      call control_command_lg(ierror_return)

      stop
      end program lagrit_main
