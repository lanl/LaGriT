      subroutine do_command(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
       implicit real*8 (a-h,o-z)
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE PROCESSES A CARD IMAGE BY CALLING THE
C        CORRESPONDING ROUTINE.
C
C
C     INPUT ARGUMENTS -
C
C        imsgin     - MODE OF MESSAGE TOKENS
C        xmsgin     - FLOATING-POINT FORM OF MESSAGE TOKENS
C        cmsgin     - CHARACTER FORM OF MESSAGE TOKENS
C        msgtype    - INTEGER FORM OF MESSAGE TOKENS
C        nwds       - NUMBER OF WORDS IN MESSAGE
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr2 - AN ERROR FLAG THAT INDICATES THE COMPLETION STATUS
C                   FOR THIS ROUTINE. (=0 ==> OK, <>0 ==> ERROR)
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/do_command.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:43:18 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Thu Oct 31 14:29:20 1996   dcg
CPVCS    clean up declarations
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:36   pvcs
CPVCS    Original version.
C
C#######################################################################
C
C
C#######################################################################
C
      dimension xmsgin(nwds)
      integer imsgin(nwds), msgtype(nwds)
C
      character*32 cmsgin(nwds)
C
C
      character  icommand*32
C
C#######################################################################
C
      icommand=cmsgin(1)
C
C     ******************************************************************
C
C     wellmesh      : PRINT A TIMESTEP SUMMARY SHEET.
C
      if(icommand(1:8).eq.'wellmesh') then
C
         do i=1,nwds
            itype=msgtype(i)
            if(itype.eq.1) then
               write(*,9000) i,imsgin(i)
 9000          format("Integer parameter: ",2i10)
            elseif(itype.eq.2) then
               write(*,9010) i,xmsgin(i)
 9010          format("Real parameter:    ",i10,1pe15.6)
            elseif(itype.eq.3) then
               write(*,9020) i,cmsgin(i)
 9020          format("Char parameter:    ",i10," ",a8)
            else
               write(*,9030) i,itype
 9030          format("Impossible message type:    ",2i10)
            endif
         enddo
C
         ierr2=0
         goto 9999
      endif
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      return
      end
