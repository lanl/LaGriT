*CD,consts
C
C ######################################################################
C
C     PURPOSE -
C
C        COMDECK FOR CONSTANTS.
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: consts.h,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   Tue Oct 26 14:07:48 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.4   Thu Jan 21 20:56:00 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS    
CPVCS       Rev 1.3   Tue Mar 03 09:21:50 1998   dcg
CPVCS    replace x24 by twenty4
CPVCS
CPVCS       Rev 1.3   Tue Mar 03 09:11:00 1998   dcg
CPVCS    replace x24 by twenty4
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:37:22 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   05/15/95 15:02:08   ejl
CPVCS    Typed all variables
CPVCS
CPVCS       Rev 1.0   11/13/94 15:41:24   het
CPVCS    Original Version
C
C ######################################################################
C
      real*8 epsilon, epsilonr
      common /consts/ epsilon, epsilonr
      save /consts/
C
      real*8 c17o48  , c7o48  ,
     &       one3rd  , one6th ,
     &       one12th , one4th ,
     &       one24th , two3rd ,
     &       four3rd ,
     &       zero    , one    ,
     &       half    , two    ,
     &       four    , twenty4
C
      parameter (
     &          c17o48  = 17.0d+00/48.0d+00, c7o48  = 7.0d+00/48.0d+00,
     &          one3rd  = 1.0d+00/3.0d+00  , one6th = 1.0d+00/6.0d+00 ,
     &          one12th = 1.0d+00/12.0d+00 , one4th = 1.0d+00/4.0d+00 ,
     &          one24th = 1.0d+00/24.0d+00 , two3rd = 2.0d+00/3.0d+00 ,
     &          four3rd = 4.0d+00/3.0d+00  ,
     &          zero    = 0.0d+00          , one    = 1.0d+00         ,
     &          half    = 0.5d+00          , two    = 2.0d+00         ,
     &          four    = 4.0d+00          , twenty4= 24.0d+00        )
C
C ######################################################################
