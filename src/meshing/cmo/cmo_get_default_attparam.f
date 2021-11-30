      subroutine cmo_get_default_attparam(
     *                 ctype,crank,clen,cinter,cpers,cio,
     *                 ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine returns Mesh Object definition.
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C        ctype         - Attribute type.
C        crank         - Attribute rank.
C        clen          - Attribute length.
C        cinter        - Attribute interpolation
C        cpers         - Attribute persistence.
C        cio           - Attribute io file flags
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_get_default_attparam.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Thu Jan 20 14:50:54 2000   dcg
CPVCS    
CPVCS       Rev 1.19   Tue Oct 06 16:48:40 1998   dcg
CPVCS    make equivalent node attributes imt,imt1, itp,itp1,
CPVCS    icr,icr1,...
CPVCS
CPVCS       Rev 1.18   Wed Dec 17 11:25:10 1997   dcg
CPVCS    declare iout as a pointe
CPVCS
CPVCS       Rev 1.17   Mon Apr 14 16:41:12 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.16   Wed Mar 06 16:44:04 1996   dcg
CPVCS    print error messages if idebug=1
CPVCS
CPVCS       Rev 1.15   09/14/95 12:09:18   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.14   09/13/95 14:32:16   het
CPVCS    Correct an error
CPVCS
CPVCS       Rev 1.13   09/11/95 14:44:00   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.12   08/30/95 21:08:46   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.11   05/22/95 15:28:24   ejl
CPVCS    Added nfaces and nedges.
CPVCS
CPVCS       Rev 1.10   03/15/95 15:22:58   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.9   02/16/95 09:56:14   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.8   02/10/95 14:07:24   ejl
CPVCS    Fix bugs left from last update
CPVCS
CPVCS       Rev 1.6   01/30/95 06:22:12   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.5   01/24/95 08:52:42   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.4   01/04/95 22:01:34   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   12/09/94 22:50:58   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:58:44   het
CPVCS    Added a data variable type to the call.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/28/94 14:14:44   het
CPVCS    Add the "mbndry" option.
CPVCS
CPVCS       Rev 1.0   11/14/94 12:04:50   het
CPVCS    Original Version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'cmo_lg.h'
C
C#######################################################################
C
      character*32 partname,ctype,clen,crank,cinter,cpers,cio
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, len, ierr, icscode,length,icmo_index, natts
C
      character*132 logmess

C
C#######################################################################
C
         partname='default_cmo_lg'
         call mmfindbk('cmo_attlist',partname,ipcmo_attlist,
     *                   len,icscode)

 
         ctype =cmo_attlist(2)
         crank =cmo_attlist(3)
         clen  =cmo_attlist(4)
         cinter=cmo_attlist(5)
         cpers =cmo_attlist(6)
         cio   =cmo_attlist(7)


C
 9999 return
      end
