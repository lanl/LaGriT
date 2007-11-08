      program adrivgen
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
C        $Log:   /pvcs.config/t3d/src/adrivgen.f_a  $
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
      integer ierror_return
C
      call initlagrit('noisy',' ',' ')
C
      call control_command_lg(ierror_return)
C
      stop
      end
      subroutine user_sub(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr1)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Process user supplied commands
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
C        ierr1 - 0 for successful completion - -1 otherwise
C
C
C #####################################################################
C$Log$
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      integer nwds,ierr1,lenc
      real*8 xmsgin(nwds)
C  get command length
      lenc=icharlnf(cmsgin(1))
C  Insert code here to handle user coded subroutines
C  For example
C   if(cmsgin(1)(1:lenc).eq.'my_cmnd') call my_rtn(imsgin,xmsgin
C          cmsgin,msgtyp,nwds,ierr1)
C
      if(cmsgin(1)(1:lenc).eq.'fiximts') then
c        call fiximts()
         ierr1=0
      else
         ierr1=-1
      endif
      return
      end
