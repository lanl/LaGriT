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
C       / signed_distance_field /
C
C
C######################################################################
C     CHANGE HISTORY -
C
C        $Log: compute.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
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
      integer       nwds, ierror, ierrw
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

        call distance_field
     &  (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)

        write(logmess,'(a)')'COMPUTE distance_field: Done.'
        call writloga('default',0,logmess,0,ierrw)
        return

      elseif(cmsgin(2) .eq. 'signed_distance_field')then

        call distance_field_signed
     &  (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
        write(logmess,'(a)')'COMPUTE distance_field_signed: Done.'
        call writloga('default',0,logmess,0,ierrw)
        return

      elseif(cmsgin(2) .eq. 'linear_transform' .or.
     &        cmsgin(2) .eq. 'linear_extrapolate' )then

        cmsgin(2) = 'linear_transform'
        call linear_transform 
     &   (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
        write(logmess,'(a)')'COMPUTE linear_transform: Done.'
        call writloga('default',0,logmess,0,ierrw)
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
      
