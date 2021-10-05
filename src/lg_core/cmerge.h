*CD,cmerge
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS COMDECK CONTAINS THE COMMON VARIABLES RELATED TO
C           MERGING.
C           CANIDATES FOR MERGING.
C
C
C     INPUT ARGUMENTS -
C
C        NONE.
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE.
C
C
C     CHANGE HISTORY -
C
C        $Log: cmerge.h,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.11   Fri Nov 12 09:06:42 1999   dcg
CPVCS    remove unused variables
CPVCS    
CPVCS       Rev 1.10   Tue Oct 26 14:07:32 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.9   Thu Jan 21 20:54:10 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS    
CPVCS       Rev 1.8   Wed Dec 23 13:07:18 1998   jtg
CPVCS    common blocks moved to after declarations
CPVCS    
CPVCS       Rev 1.7   Fri Jul 11 09:18:40 1997   dcg
CPVCS    add ifvelmrg flag to indicate if velocities exist
CPVCS    this cuts down the number of tests needed
CPVCS    
CPVCS       Rev 1.6   Mon Apr 14 16:37:14 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.5   Tue Nov 05 10:07:28 1996   dcg
CPVCS    separate integer, real and character variables in common blocks
CPVCS
CPVCS       Rev 1.4   Wed Jun 26 08:37:10 1996   dcg
CPVCS    make merge table single dimension so that it can
CPVCS    be set to nconbnd by nconbnd
CPVCS
CPVCS       Rev 1.3   Thu May 16 10:22:32 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.2   10/04/95 07:45:48   het
CPVCS    Change the mrglist array to be pointered
CPVCS
CPVCS       Rev 1.1   01/04/95 22:06:54   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/13/94 15:41:04   het
CPVCS    Orginal Version
C
C#######################################################################
C
      integer lenblk,maxmerg
      parameter ( lenblk = 64 )
      parameter ( maxmerg = 10000 )
C
      pointer (ipmerglst , merglst(2,maxmerg))
      pointer (ipmerglst2, merglst2(2,maxmerg))
C
      pointer ( iptable2 , mtable2(*))
 
      integer  mtable2, merglst, merglst2, 
     *            npairs,   npairs2,mtable
C
      common /cmergei/   npairs ,npairs2,mtable(0:59,0:59)    
      save /cmergei/
C
C#######################################################################
