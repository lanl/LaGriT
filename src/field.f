*dk,field
      subroutine field(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &                    ierror)
C
C
C #####################################################################
C
C     PURPOSE - FIELD, depending on the specified command option,
C        calls one of several routines for performing operations
C        on cmo field values.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: field.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:47:10 1997   pvcs
CPVCS    No changes.
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:45:22 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   08/24/95 16:36:08   kuprat
CPVCS    Initial revision.
C
C ######################################################################
c*  "The FIELD Command option manipulates one or more specified fields$&
c*   in the Current Mesh Object.                                      $&
c*                                                                    $&
c*   FIELD Command Options:                                           $&
c*                                                                    $&
c*    FORMAT: FIELD/COMPOSE/composition function/ifirst,ilast,istride/$&
c*                          field list                                $&
c*                                                                    $&
c*                  DUMPMFE/root file name/x1,y1,z1/x2,y2,z2/         $&
c*                          field list                                $&
c*                                                                    $&
c*                  SCALE  /scale option/factor/ifirst,ilast,istride/ $&
c*                          field list                                $&
c*                                                                    $&
c*                  VOLAVG /averaging option/iterations/              $&
c*                          ifirst,ilast,istride/field list           $&
c*                                                                    $&
c*  ----------------------------------------------------------------  $&
C
      implicit none
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)

      integer ierror,icharln,ierrw

      character*132 logmess
      character*32 option

      ierror=0

c  Check for correctness of option and call appropriate routine.

      option=cmsgin(2)(1:icharln(cmsgin(2)))

      if (option.eq.'compose') then
         call field_compose(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
      elseif (option.eq.'dumpmfe') then
         call field_dumpmfe(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)  
      elseif (option.eq.'scale') then
         call field_scale(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)  
      elseif (option.eq.'volavg') then
         call field_volavg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)  
      else
         write(logmess,'(a)')
     *      'FIELD: invalid option.'
         call writloga('default',0,logmess,0,ierrw)
         ierror=1
      endif

 9999 continue
      return
      end
