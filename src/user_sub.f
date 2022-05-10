      subroutine user_sub(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Process user supplied commands
C        user_sub will be passed as a command through msgtty
C        to this routine along with user options
C        parse arguments for your command
C
C        you can copy lagrit_test.f to user_sub.f to help get started
C
C     INPUT ARGUMENTS -
C
C        imsgin - integer array of tokens returned by parser
C        xmsgin - real array of tokens returned by parser
C        cmsgin - character array of tokens returned by parser
C        msgtyp - integer array of token types returned by parser
C
C     OUTPUT ARGUMENTS -
C
C        ierror - 0 for successful completion - -1 otherwise
C
C
C #####################################################################
      implicit none
C
C Define arguments 
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
      integer nwds,ierror

C character buffers and variables
      integer ierr, ierrw

      character*32  cmoname
      character*32  isubname 
      character*132 logmess
      character*8092 cbuf
      integer icharlnf
       
C START CODE ------------------------------------------------

      isubname="user_sub_example"
      ierr= 0

      if (nwds.ne.1) then
          write(logmess,'(a)')
     *   'Syntax: user_sub [empty command]' 
         call writloga('default',1,logmess,0,ierrw)
         go to 9999
      endif 


 9999 continue 

      write(logmess,"(a)")'user_sub exit.'
      if (ierr .ne. 0) then
          write(logmess,"(a,i4)")'user_sub exit early with error: ',ierr 
      endif
      call writloga('default',1,logmess,1,ierrw)

      return
      end

