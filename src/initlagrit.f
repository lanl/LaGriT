      subroutine initlagrit(mode,log_file,batch_file)
C
C#######################################################################
C
C     PURPOSE -
C
C        CODE INITIALIZATION ROUTINE.
C
C
C     INPUT ARGUMENTS -
C
C        mode     - set to 'noisy' for tty output
C                 - set to 'silent' for no tty output
C        log_file - name of log file (if ' ' or '-def-' use default name
C      batch_file - name of batch file (if ' ' or '-def-' use default name
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/initlagrit_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.7   03 Oct 2007 08:18:10   tam
CPVCS    removed extra args version and compiled from writinit call
CPVCS    
CPVCS       Rev 1.6   08 Feb 2006 14:38:14   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.5   29 Sep 2004 15:47:36   dcg

CPVCS    change value of pie to double precision

CPVCS    

CPVCS       Rev 1.4   Fri Apr 07 10:40:38 2000   dcg

CPVCS    change reference to blockcom

CPVCS    

CPVCS       Rev 1.3   Wed Apr 05 14:50:24 2000   nnc

CPVCS    Changed the included file to blockcom_nosb.f

CPVCS    

CPVCS       Rev 1.2   Wed Apr 05 14:25:24 2000   nnc

CPVCS    Include blockcom.f at the end of the file.  This will ensure that

CPVCS    this block data procedure gets loaded at link time.

CPVCS    

CPVCS       Rev 1.1   Thu Feb 03 08:52:22 2000   dcg

CPVCS    

CPVCS       Rev 1.0   14 Jan 2000 17:09:08   dcg

CPVCS    Initial revision.

CPVCS
CPVCS       Rev 1.8   Wed Nov 10 15:40:06 1999   dcg
CPVCS    make irwwname,irwrname local variables
CPVCS
CPVCS       Rev 1.7   Wed Nov 10 15:05:00 1999   dcg
CPVCS    remove blockini.h
CPVCS
CPVCS       Rev 1.6   Wed Nov 10 09:21:34 1999   dcg
CPVCS    remove cnames.h
CPVCS
CPVCS       Rev 1.5   Fri Nov 05 13:27:16 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS
CPVCS       Rev 1.4   Wed Sep 01 12:35:10 1999   dcg
CPVCS    remove test on undefined ifsbunit - use if unitcv instead
CPVCS
CPVCS       Rev 1.3   Wed Aug 04 15:17:02 1999   dcg
CPVCS    initial the value of 'pi'
CPVCS
CPVCS       Rev 1.2   Wed Jun 16 10:46:44 1999   nnc
CPVCS    Character arguments declared to have variable length.
CPVCS
CPVCS       Rev 1.1   Fri Feb 26 16:48:26 1999   dcg
CPVCS    use lagrit.h not x3dgen.h
CPVCS
CPVCS       Rev 1.0   Mon Feb 22 16:07:44 1999   dcg
CPVCS    Initial revision.
C#######################################################################
C
      implicit none
C
      character*200 logmess
C
      include "machine.h"
      include "chydro.h"
      include "consts.h"
      include "cmerge.h"
      include "copyrite.h"
      include 'commands_lg.h'
C
C
C#######################################################################
C
      character*(*)  mode, log_file, batch_file
 
      character*32 ixname, isname, irwrnamt, irwwnamt, loglog, logbat
C
      character*32 logtty,isubname
C
      integer iprecision,i,ierrw,ipointi,ipointj,ipointf,ipoints
      real*8 x1,x2
C
C ######################################################################
C
c
      isubname='initlagrit'
c  make sure pie has a value
      pie=3.141592653589793d0
C
C     *** DETERMINE WHETHER THE CODE IS RUNNING IN SINGLE OR
C            DOUBLE PRECISION MODE.
      idouble=iprecision()
C
C     BASED ON THE PRECISION CALCULATE AN EPSILON.
C     EPSILON IS SET TO A SMALL NUMBER
C     EPSILONR IS SET TO REFLECT THE MACHINE EPSILON
C
      epsilon=1.0d+00
      if(idouble.eq.1) then
         epsilon=1.0e-18
      elseif(idouble.eq.2) then
         do i=1,5
            epsilon=1.0d-30*epsilon
         enddo
      else
         do i=1,20
            write(logmess,'(a,i5)') 'Precision is incorrect: ',idouble
           call writloga('default',0,logmess,0,ierrw)
         enddo
         stop
      endif
      epsilonr=1.0/epsilon
      x2=one
      do i=1,1000
         x2=x2/two
         x1=one+x2
         if(x1.le.one) go to 11
       enddo
 11    epsilonr=x2*2.
 
C     TREAT SPECIAL CONTROLLEE-RELATED SITUATIONS.
 
         if(batch_file(1:1).eq.' '.or.batch_file(1:4).eq.'-def') then
            logbat = 'outx3dgen'
         else
            logbat=batch_file
         endif
         if(log_file(1:1).eq.' '.or.log_file(1:4).eq.'-def') then
            loglog = 'logx3dgen'
         else
            loglog=log_file
         endif
C           **BATCH LOG FILE NAME
         ixname = 'lagrit'
C           **CONTROLLEE EXECUTABLE NAME
         isname = 'smpgen '
C           **SAMPLE FILE NAME
         irwrnamt = 'rsgenin'
C           **INPUT RESTART FILE NAME
         irwwnamt = 'rsgenot'
C           **OUTPUT RESTART FILE NAME
         nb = 0
         ipointj = 0
         ipointi = 0
         ipointf = 0
         ipoints = 1
 
C     ASSIGN AND INITIALIZE LOG FILES.
C
      logtty = 'tty'
C        **NAME OF TTY LOG FILE
      call writinit(logbat,loglog,ixname,mode)
C        **ASSIGN LOG FILES AND WRITE CODE VERSION INFORMATION
C
c  inititalize global data and cmo data
c
      call sbinit
 
C     ******************************************************************
c  set up for interactive i/o
c  get memory for the cmd_stack
c
      maxlen_stack=20480
      last_char=0
      initname=isubname
      call mmgetblk('cmd_stack',isubname,ipcmd_stack,
     *  maxlen_stack/NBYTES_INT,1,ierrw)
      nlevels=1
      jlevels(nlevels)=5
      clevels(nlevels)='interactive_lg'
c
c  get memory for the definition_stack
c
      maxlen_def=10240
      ndefinitions=0
      lastchar_def=0
      initname=isubname
      call mmgetblk('definition',isubname,ipdefinition,
     *  maxlen_def/NBYTES_INT,1,ierrw)
c
 9999 continue
C
C     ******************************************************************
C
      return
      end
 
C     ******************************************************************
C      Include this block data procedure here to ensure it gets linked.
C     ******************************************************************
 
      include 'blockcom.h'
 
