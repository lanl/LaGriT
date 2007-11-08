*cd,logcom1
C
C#######################################################################
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/logcom1.h_a  $
CPVCS    
CPVCS       Rev 1.4   Tue Oct 26 14:08:00 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Thu Jan 21 20:58:04 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:37:28 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Tue Nov 05 10:07:40 1996   dcg
CPVCS    separate integer, real and character variables in common blocks
CPVCS
CPVCS       Rev 1.0   01/26/95 08:03:14   ejl
CPVCS    Instaled as part of the writ..... routine cleanup.
C
C#######################################################################
C
C
C#######################################################################
C
      integer nlogs
      parameter ( nlogs=3 )
C
      integer numlogs
      character*32 logname
      character*4 logtype
      character*4 logstat
      integer logunit
C
      common/logcomi/ numlogs,logunit(nlogs)
      common/logcomc/ logtype(nlogs), logstat(nlogs)
      common/logcomc1/ logname(nlogs)
C
      save/logcomi/
      save/logcomc/
      save/logcomc1/
C
C#######################################################################
C
