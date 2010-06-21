      subroutine grid2grid(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C       The purpose of this subroutine is to parse the second
C       argument and call various compute type modules.
C
C       Valid strings for the second argument are:
C       / hextotet2 / 
C       / hextotet2 / 
C       / hextotet2 / 
C       / hextotet2 / 
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
      integer       nwds, ierror
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
      character*256 cmd
      character*132 logmess
      character*32  isubname
C
C######################################################################
C
      isubname = 'grid2grid'

      if(msgtype(2) .ne. 3) then
         write(logmess,'(a)')
     &    'ERROR grid2grid: Argument 2 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif

      if(cmsgin(2) .eq. 'hextotet5')then
          cmd = 'hextotet/5/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'hextotet6')then
          cmd = 'hextotet/6/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'hextotet24')then
          cmd = 'hextotet/24/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'quadtotri2')then
          cmd = 'hextotet/2/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'quadtotri4')then
          cmd = 'hextotet/4/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'pyrtotet4')then
          cmd = 'hextotet/4/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'prismtotet3')then
          cmd = 'hextotet/3/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'prismtotet14')then
          cmd = 'hextotet/14/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'prismtotet18')then
          cmd = 'hextotet/18/'//cmsgin(3)//'/'//cmsgin(4)
     &        //'; finish'
          call dotask(cmd, ierror)
          return
      elseif(cmsgin(2) .eq. 'tree_to_fe')then
          call tree_to_fe(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
          return
      else
         write(logmess,'(a)')
     &    'ERROR grid2grid: No valid second argument found.'
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
      
