*dk,writlogb
      subroutine writlogb(logname,isbefore,imessage,isafter,ierr)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        lognam   - UNIT NUMBER WHERE MESSAGE IS SENT
C
C        isbefore - NUMBER OF LINES TO SKIP BEFORE MESSAGE IS PRINTED
C
C        imessage - MESSAGE TO BE PRINTED
C
C        isafter  - NUMBER OF LINES TO SKIP AFTER MESSAGE IS PRINTED
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr     - ERROR INDICATOR
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/writlogb.f_a  $
CPVCS    
CPVCS       Rev 1.5   Mon Apr 14 17:06:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.4   09/20/95 14:08:14   dcg
CPVCS    correct length of print
CPVCS
CPVCS       Rev 1.3   03/23/95 22:59:42   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.2   02/16/95 09:49:10   ejl
CPVCS    Fixed bug with lines longer than 80 characters.
CPVCS
CPVCS       Rev 1.1   01/26/95 08:01:38   ejl
CPVCS    Cleaned up, Fixed holes in the logic, implicit none.
CPVCS    Installed logcom1.h and blockini.h
c
c   Rev 1.0   01/26/95 07:58:18   ejl
cCleaned up, Fixed holes in the logic, implicit none
cInstaled logcom1.h and blockini.h
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:34   pvcs
CPVCS    Original version.
C
C
C ######################################################################
C
C
      implicit none
C
C#######################################################################
C
      integer logname
      integer isbefore
      character*(*) imessage
      integer isafter
      integer istop
C
      integer ierr
C
C#######################################################################
C
      integer i, lenmes, length, is, ie
C
C#######################################################################
C
      integer icharlnb
C
C#######################################################################
C
C
      if(isbefore.gt.0) then
         do i=1,isbefore
            write(logname,9000)
         enddo
      endif
 
      if(imessage.eq.' ') then
C
         write(logname,9000)
 9000    format(' ')
 
      else
C
         lenmes=icharlnb(imessage)
C
C
C*****   istop=0
C
C....    Find the first Null Character.
C
C*****   do while ((istop .lt. lenmes) .and.
C*****             (imessage(istop+1:istop+1) .eq. char(0)))
C
C*****      istop=istop+1
C
C*****   enddo
C*****   if(istop.lt.lenmes) then
C*****      do i=istop+1,lenmes
C*****         imessage(i:i)=' '
C*****      enddo
C*****   endif
C
C*****   lenmes=min(istop,lenmes)
         length=(lenmes-1)/80+1
C
         if(length.le.1) then
C
            write(logname,9010) imessage
 9010       format(a80)
C
         else
C
            do i=1,length
               is=80*(i-1)+1
               ie=80*i
               if(lenmes.lt.ie) ie=lenmes
               write(logname,9010) imessage(is:ie)
            enddo
C
         endif
C
      endif
C
      if(isafter.gt.0) then
         do i=1,isafter
            write(logname,9000)
         enddo
      endif
C
      return
      end
