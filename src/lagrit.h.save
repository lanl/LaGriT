c
c----------------------------------------------------------------
c
c     This is a template for the lagrit program banner
c     Variables can be modified at compile time to adjust
c     information written to the LaGriT banner
c     Program banner is written by writinit routine.
C     The TAG keywords can be used to substitute values at
C     compile time for Version, OSName, and version number
c
c----------------------------------------------------------------
C      CHANGE HISTORY -
C
C        $Log: lagrit.h,v $
C        Revision 2.00  2007/11/05 19:46:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.23   03 Oct 2007 11:11:34   tam
CPVCS    cleaned up keywords
CPVCS    
CPVCS       Rev 1.22   03 Oct 2007 08:23:38   tam
CPVCS    added OSName with TAGs as a way to control
CPVCS    variables at compile time
CPVCS    
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   tam
CPVCS    original version
CPVCS
c
      integer         NCall, VMajor, VMinor
      save            NCall
      parameter      (VMajor=2)
      parameter      (VMinor=001)
c
      character*16    MyName
      parameter      (MyName='lagritgen')
c
c     These variables can be set during compile time with by
c     substituting the TAG with editor or compile scripts
c     or if not set, OSNAME will not be used and date will
c     be constructed from fdate()

      character*8    OSName
      parameter      (OSName='OSTAG')
c
      character*22    Compiled
c     parameter      (Compiled='2007/00/00            ')
      parameter      (Compiled='DATETAG               ')
c
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
