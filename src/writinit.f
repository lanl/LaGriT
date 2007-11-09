dk,writinit
      subroutine writinit(logbat,loglog,ixnamxxx,mode)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE CONTROLS THE INITIALIZATION OF THE LOG FILES.
C        THERE ARE CURRENTLY 3 LOG FILES:
C
C           (1) TTY LOG FILE   --- TURNED OFF DURING PRODUCTION RUNS
C
C           (2) BATCH LOG FILE --- ALWAYS TURNED ON TO CAPTURE
C                                  MESSAGES IN A LOG FILE.
C
C           (3) ERROR LOG FILE
C
C
C     INPUT ARGUMENTS -
C
C        logbat   - NAME OF BATCH LOG FILE
C
C        loglog   - NAME OF COMMAND LOG FILE
C
C        ixnamxxx - NAME OF CODE EXECUTABLE
C
C        mode     - set to 'noisy' for tty output, set to 'silent'
C                        for no tty output
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C
C        $Log: writinit.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.19   17 Oct 2007 15:11:44   tam
CPVCS    Version was not being written to SunOS
CPVCS    
CPVCS       Rev 1.18   03 Oct 2007 11:06:54   tam
CPVCS    changed banner and dates so smaller and easier to read
CPVCS    
CPVCS       Rev 1.17   03 Oct 2007 08:16:10   tam
CPVCS    removed args Version and compiled, they are now construed
CPVCS    directly from lagrit.h and allows better control of date
CPVCS    and the look of the banner. Added copyright statement.
CPVCS    
CPVCS       Rev 1.16   08 Jan 2007 07:37:00   tam
CPVCS    added appropriate number of spaces to replace www
CPVCS    
CPVCS       Rev 1.15   08 Jan 2007 07:29:56   tam
CPVCS    took www out of http statement so it works
CPVCS    
CPVCS       Rev 1.14   05 Jan 2007 14:47:58   tam
CPVCS    added Manual web page to header
CPVCS
CPVCS       Rev 1.13   03 Jan 2007 13:49:20   tam
CPVCS    this is the automated and original version
CPVCS    
CPVCS       Rev 1.11   05 Aug 2002 17:08:26   dcg
CPVCS    use 4 digit year
CPVCS
CPVCS       Rev 1.10   Wed Nov 10 15:05:12 1999   dcg
CPVCS    remove blockini.h
CPVCS
CPVCS       Rev 1.9   Tue Nov 09 16:00:34 1999   dcg
CPVCS    remove unused code
CPVCS
CPVCS       Rev 1.8   Mon Feb 22 16:08:20 1999   dcg
CPVCS    rewrite of command processing to allow for recursion
CPVCS
CPVCS       Rev 1.7   Tue Aug 19 16:34:12 1997   dcg
CPVCS    change name of code
CPVCS    check for expiration date
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 17:06:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Tue Sep 24 18:16:00 1996   jan
CPVCS    Write formats added to allow version number and
CPVCS    compile date to appear X3D header.
CPVCS
CPVCS       Rev 1.4   08/23/95 12:27:34   dcg
CPVCS    allow noisy, silent option for tty input
CPVCS
CPVCS       Rev 1.3   05/24/95 15:44:48   het
CPVCS    Change the formats in some write statements
CPVCS
CPVCS       Rev 1.2   01/26/95 08:01:22   ejl
CPVCS    Cleaned up, Fixed holes in the logic, implicit none.
CPVCS    Installed logcom1.h and blockini.h
c
c   Rev 1.0   01/26/95 07:58:08   ejl
cCleaned up, Fixed holes in the logic, implicit none
cInstaled logcom1.h and blockini.h
CPVCS
CPVCS       Rev 1.1   11/22/94 14:33:30   dcg
CPVCS      add call to fdate to get time and date stamp for output
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:28   pvcs
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
      character*(*) loglog
      character*(*) logbat
      character*(*) ixnamxxx
C
C#######################################################################
C
      include 'logcom1.h'
      include 'lagrit.h'
C
C
C
C#######################################################################
C
      integer icharlnf
      integer jlogbat, jloglog, ierr1, ierrass, ierrdum,
     *   nyr,myr
C
      character*132 interfil
      character*8 iunitc,mode
      character*24 now
      character*22 cstring
C
C#######################################################################
C
C
C     *****************************************************************
C
C     INITIALIZE NUMBER OF LOG FILES.
C
      numlogs=0
C
C     *****************************************************************
C
C     ASSIGN BATCH LOG FILE.
C
      if(logbat.ne.' ') then
C*****         call destroy(0,logbat,0,ierr)
         jlogbat=-1
         call hassign(jlogbat,logbat,ierrass)
      endif
C
      if(loglog.ne.' ') then
C*****         call destroy(0,loglog,0,ierr)
         jloglog=-1
         call hassign(jloglog,loglog,ierrass)
      endif
C
C     *****************************************************************
C
C     SET TYPES OF LOG FILES.
C
      call writset('type',' ','tty',ierr1)
      call writset('type',' ','bat',ierr1)
      call writset('type',' ','log',ierr1)
C
C     *****************************************************************
C
C     SET UNIT NUMBERS OF LOG FILES.
C
      write(iunitc,'(i5)') 6
      call writset('unit','tty',iunitc,ierr1)
C
      write(iunitc,'(i5)') jlogbat
      call writset('unit','bat',iunitc,ierr1)
C
      write(iunitc,'(i5)') jloglog
      call writset('unit','log',iunitc,ierr1)
C
C     *****************************************************************
C
C     SET NAMES OF LOG FILES.
C
      call writset('name','tty','tty',ierr1)
      call writset('name','bat',logbat,ierr1)
      call writset('name','log',loglog,ierr1)
C
C     *****************************************************************
C
C     SET ON/OFF STATUS OF LOG FILES.
C
      if(mode(1:3).ne.'sil') then
         call writset('stat','tty','on',ierr1)
      else
         call writset('stat','tty','off',ierr1)
      endif
C
      call writset('stat','bat', 'on',ierr1)
      call writset('stat','log', 'on',ierr1)
C
C     *****************************************************************
C
C     WRITE CODE VERSION INFORMATION TO LOG FILES.
C     strings for Version and Compiled are passed in from
C     lagrit.h and fdate


C-----Banner Top
C
      write(interfil,9001)
 9001 format('*',15x,'* * * * * * * * * * * * * * * * * * * * * * * *')
      call writloga('default',2,interfil,0,ierrdum)
C
      write(interfil,9002)
 9002 format('*',15x,'*                                             *')
      call writloga('default',0,interfil,0,ierrdum)

C-----Banner Program, OS, and Version Number from lagrit.h
C
      if (VMajor .lt. 10) then 
        Version = ' .     '
        write ( Version(1:1), '(i1)' ) VMajor
        write ( Version(3:5), '(i3.3)' ) VMinor
      else
        Version = '  .     '
        write ( Version(1:2), '(i2)' ) VMajor
        write ( Version(4:6), '(i3.3)' ) VMinor
      endif

      if (OSName(1:3) .eq. 'Lin') then
      write(interfil,8089) Version
 8089 format('*',15x,'*    Program:  LaGriT V',a6,'  Linux          *')

      else if (OSName(1:3) .eq. 'Dar') then
      write(interfil,8189) Version
 8189 format('*',15x,'*    Program:  LaGriT V',a6,'  Darwin         *')

      else if (OSName(1:3) .eq. 'Sun') then
      write(interfil,8289) Version
 8289 format('*',15x,'*    Program:  LaGriT V',a6,'  SunOS          *')

      else if (OSName(1:3) .eq. 'IRI') then
      write(interfil,8389) Version
 8389 format('*',15x,'*    Program:  LaGriT V',a6,'  IRIX           *')
      else 

      write(interfil,8389)  Version
 8989 format('*',15x,'*    Program:  LaGriT V',a6,'                 *')
      endif 
      call writloga('default',0,interfil,0,ierrdum)

C-----Banner Compile Time and expiration check 
C     now string is day 6:10   time 12:20   year 21:24
C     the front six is blank or has a year 
C     Compiled string is year 1:4 followed by month and day
c     look for expiration 2 years greater than compile year

      call fdate(now)
      cstring = Compiled
      if (cstring(1:4).eq.'DATE') then
         cstring=now(21:24)//'/00/00 at 00:00:00' 
      endif
      write(interfil,9004) cstring
 9004 format('*',15x,'*    Compiled: ',a22,'         *')
      call writloga('default',0,interfil,0,ierrdum)

      nyr = (ichar(now(23:23))-ichar('0'))*10+
     *     (ichar(now(24:24))-ichar('0'))
      myr = (ichar(cstring(3:3))-ichar('0'))*10+
     *     (ichar(cstring(4:4))-ichar('0'))

      if(myr.ne.nyr.and.mod(myr+1,100).ne.nyr.and.
     *   mod(myr+2,100).ne.nyr) then
          write(interfil,9020)
          call writloga('default',0,interfil,0,ierrdum)
 9020     format(' code expiration date passed ')
          call termcode()
      endif
 
C-----Banner Program run time 
C
      write(interfil,9009) now(21:24),now(6:10),now(12:20)
 9009 format('*',15x,'*    Run Time: ',a4,'/',a5,'  ',a9,'          *')
      call writloga('default',0,interfil,0,ierrdum)

C-----Banner Man pages 
C
      write(interfil,9007)
 9007 format('*',15x,'*    Manual:   http://lagrit.lanl.gov         *')
      call writloga('default',0,interfil,0,ierrdum)

C
C-----Banner Bottom 
C
      write(interfil,9002)
      call writloga('default',0,interfil,0,ierrdum)
      write(interfil,9001)
      call writloga('default',0,interfil,0,ierrdum)

C     *****************************************************************
C     WRITE COPYRITE TEXT 
C

      write(interfil,'(a)')
     * '                               -----oOo-----    '
      call writloga('default',1,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'LaGriT Copyright: This program was prepared by' 
     * //' Los Alamos National Security, LLC'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'at Los Alamos National Laboratory (LANL) under '
     * //'contract No. DE-AC52-06NA25396'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'with the U.S. Department of Energy (DOE). All rights'
     * //' in the program are reserved'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'by the DOE and Los Alamos National Security, LLC.'
     * // ' Permission is granted to the'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'public to copy and use this software without charge,'
     * // ' provided that this Notice' 
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'and any statement of authorship are reproduced'
     * // ' on all copies. Neither the'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * 'U.S. Government nor LANS makes any warranty, '
     * // 'express or implied, or assumes'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     *  'any liability or '
     * // 'responsibility for the use of this software.'
      call writloga('default',0,interfil,0,ierrdum)

      write(interfil,'(a)')
     * '                               -----oOo-----    '
      call writloga('default',0,interfil,1,ierrdum)
 
C
C     *****************************************************************
C
C     WRITE NAME OF BATCH LOG FILE TO LOG FILES.
C
      write(interfil,9100) logbat(1:icharlnf(logbat))
      call writloga('default',1,interfil,0,ierrdum)
 9100 format('Output log file: ',a)
      write(interfil,9110) loglog(1:icharlnf(loglog))
      call writloga('default',0,interfil,1,ierrdum)
 9110 format('Command log file: ',a)
C
C     *****************************************************************
C
      return
      end
