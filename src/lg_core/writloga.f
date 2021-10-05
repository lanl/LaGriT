*dk,writloga
      subroutine writloga(iopt,isbefore,imess,isafter,ierr)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE DRIVES THE MESSAGE PROCESSING ROUTINES FOR THE
C        CODE.  THE INPUT OPTION iopt CONTROLS WHERE THE MESSAGE IS
C        SENT.
C
C
C     INPUT ARGUMENTS -
C
C        iopt     - OPTION TO DETERMINE WHERE MESSAGE IS SENT
C
C        isbefore - NUMBER OF LINES TO SKIP BEFORE MESSAGE IS PRINTED
C
C        imess    - MESSAGE TO BE PRINTED
C
C        isafter  - NUMBER OF LINES TO SKIP AFTER MESSAGE IS PRINTED
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr     - ERROR INDICATOR = 0  ==> O.K.
C
C
C     CHANGE HISTORY -
C
C        $Log: writloga.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   14 Mar 2000 14:56:46   dcg
CPVCS    set error flag to zero
CPVCS
CPVCS       Rev 1.4   Mon Feb 22 16:08:58 1999   dcg
CPVCS    rewrite of command processing to allow for recursion
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 17:06:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   08/02/95 13:42:48   dcg
CPVCS    fixes for log command and name change with writset
CPVCS
CPVCS       Rev 1.1   01/26/95 08:01:32   ejl
CPVCS    Cleaned up, Fixed holes in the logic, implicit none.
CPVCS    Installed logcom1.h and blockini.h
c
c   Rev 1.0   01/26/95 07:58:14   ejl
cCleaned up, Fixed holes in the logic, implicit none
cInstaled logcom1.h and blockini.h
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:32   pvcs
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
      character*(*) iopt
      integer isbefore
      character*(*) imess
      integer isafter
C
      integer ierr
C
C#######################################################################
C
      include 'logcom1.h'
C
C#######################################################################
C
      integer i, iunit, jname
C
      character*32 iname
C
C#######################################################################
C
      integer*4 lunget, iunit4
      integer l1, l2, icharln
      logical opn
C
C#######################################################################
C
C
      ierr=0
C
C     ******************************************************************
C
C     SEND MESSAGE TO PROPER OUTPUT LOGS.
C
      if(numlogs.le.0) then
C
C        _______________________________________________________________
C
C        SEND MESSAGE ONLY TO tty.
C
         iunit4=lunget('tty')
         iunit = iunit4
C           *** GET THE LOGICAL UNIT NUMBER ASSOCAITED WITH THIS FILE.
         call writlogb(iunit,isbefore,imess,isafter,ierr)
C
C        _______________________________________________________________
C
      else
C
C        _______________________________________________________________
C
C        SEND MESSAGE TO OUTPUT LOGS ACCORDING TO iopt.
C
         if(iopt.eq.'default') then
C
C           ............................................................
C
C           SEND MESSAGE TO tty AND bat LOG FILES IF ON.
C
            do i=1,numlogs
               if(logtype(i)(1:3).eq.'tty'.and.logstat(i)(1:2).eq.'on')
     x                   then
                  iname=logname(i)
                  jname=lunget(iname)
                  if(jname.eq.-1) then
                     jname=logunit(i)
                     l1=icharln(logname(i))
                     inquire(file=logname(i)(1:l1),opened=opn,err=9999)
                     if(opn.eqv..false.) open(file=logname(i)(1:l1),
     x                     unit=logunit(i),status='unknown')
                  endif
C                    *** GET THE LOGICAL UNIT NUMBER ASSOCAITED
C                    ***    WITH THIS FILE.
                  call writlogb(jname,isbefore,imess,isafter,ierr)
               elseif(logtype(i)(1:3).eq.'bat'.and.logstat(i)(1:2).eq.
     x                    'on') then
                  iname=logname(i)
                  jname=lunget(iname)
                  if(jname.eq.-1) then
                     jname=logunit(i)
                     l1=icharln(logname(i))
                     inquire(file=logname(i)(1:l1),opened=opn,err=9999)
                     if(opn.eqv..false.) open(file=logname(i)(1:l1),
     x                     unit=logunit(i),status='unknown')
                  endif
C                    *** GET THE LOGICAL UNIT NUMBER ASSOCAITED
C                    ***    WITH THIS FILE.
                  call writlogb(jname,isbefore,imess,isafter,ierr)
               endif
            enddo
C
C           ............................................................
C
         else
C
C           ............................................................
C
C           SEND MESSAGE TO SPECIFIED LOG FILE IF ON.
C
            do i=1,numlogs
              l1=icharln(logtype(i))
              l2=icharln(iopt)
               if(logtype(i)(1:l1).eq.iopt(1:l2).and.logstat(i)(1:2).eq.
     x                    'on') then
                  iname=logname(i)
                  jname=lunget(iname)
                  if(jname.eq.-1) then
                     jname=logunit(i)
                     l1=icharln(logname(i))
                     inquire(file=logname(i)(1:l1),opened=opn,err=9999)
                     if(opn.eqv..false.) open(file=logname(i)(1:l1),
     x                     unit=logunit(i),status='unknown')
                  endif
C                    *** GET THE LOGICAL UNIT NUMBER ASSOCAITED
C                    ***    WITH THIS FILE.
                  call writlogb(jname,isbefore,imess,isafter,ierr)
               endif
            enddo
C
C           ............................................................
C
         endif
C
C        _______________________________________________________________
C
      endif
C
C     ******************************************************************
C
 9999 continue
      return
      end
