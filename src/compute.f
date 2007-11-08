      subroutine compute(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C       The purpose of this subroutine is to parse the second
C       argument and call various compute type modules.
C
C       Valid strings for the second argument are:
C       / distance_field /
C
C
C######################################################################
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/compute.f_a  $
CPVCS    
CPVCS       Rev 1.0   05 Jun 2007 15:33:08   gable
CPVCS    Initial revision.
C
C######################################################################
C
      implicit none
C
C     Subroutine Input Variables
C
      integer       nwds, ierror
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
      character*132 logmess
      character*32  isubname
C
C######################################################################
C
      isubname = 'compute'

      if(msgtype(2) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR compute: Argument 2 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif

      if(cmsgin(2) .eq. 'distance_field')then
      call distance_field(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      return
      else
         write(logmess,'(a)')
     &    'ERROR compute: No valid second argument found.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         goto 9999
      endif
C
C     All done
C
 9999 continue
      return
      end
      
