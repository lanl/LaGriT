*dk,writset
      subroutine writset(itype,iopt,ivalue,ierr)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE IS USED TO SET THE FIELDS OF THE LOG FILES.
C
C     INPUT ARGUMENTS -
C
C        itype    - 
C
C        iopt     -
C
C        ivalue   -
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr     - ERROR INDICATOR = 0  ==> O.K.
C
C
C     CHANGE HISTORY -
C
C        $Log: writset.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:06:08 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   01/26/95 08:01:42   ejl
CPVCS    Cleaned up, Fixed holes in the logic, implicit none.
CPVCS    Installed logcom1.h and blockini.h
c
c   Rev 1.0   01/26/95 07:58:20   ejl
cCleaned up, Fixed holes in the logic, implicit none
cInstaled logcom1.h and blockini.h
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:20:36   pvcs
CPVCS    Original version.
C
C
C#######################################################################
C
C
      implicit none
C
C#######################################################################
C
      character*(*) itype
      character*(*) iopt
      character*(*) ivalue
C
      integer ierr
C
C#######################################################################
C
C
      include 'logcom1.h'
C
C#######################################################################
C
      integer i
C
C#######################################################################
C
C
      ierr=1
C
      if(iopt.eq.' ') then
C
         if(numlogs.lt.nlogs) then
            numlogs=numlogs+1
            logtype(numlogs)=ivalue
            ierr=0
         else
            ierr=-1
         endif
C
      else
C
         do i=1,numlogs
            if(logtype(i).eq.iopt) then

               if(itype(1:4).eq.'name') then
                  logname(i)=ivalue
                  ierr=0
               elseif(itype(1:4).eq.'type') then
                  logtype(i)=ivalue
                  ierr=0
               elseif(itype(1:4).eq.'stat') then
                  logstat(i)=ivalue
                  ierr=0
               elseif(itype(1:4).eq.'unit') then
                  read(ivalue,'(i5)') logunit(i)
                  ierr=0
               endif
            endif
         enddo
C
      endif
C
      return
      end
