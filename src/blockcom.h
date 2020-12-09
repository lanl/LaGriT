      block data blockcom
       implicit none
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE INITIALIZES DATA IN A NUMBER OF COMMON BLOCKS.
C
C
C     INPUT ARGUMENTS -
C
C          NONE
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C

C        $Log: blockcom.h,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   20 Jun 2001 15:37:04   kuprat
CPVCS    Propagated Tinka's fix from blockcom_nosb.f:
CPVCS    fixed error in ielmnode0 for pri and hex
CPVCS    
CPVCS       Rev 1.0   Fri Apr 07 10:38:58 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 15:47:30 2000   nnc
CPVCS    Removed the stub routine that was added to force linking of the
CPVCS    block data under the Absoft compiler.  Instead we now include
CPVCS    this file in initlagrit_nosb.f.
CPVCS    
CPVCS       Rev 1.1   Wed Apr 05 13:34:06 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 16:20:52   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.58   Fri Nov 12 09:07:24 1999   dcg
CPVCS    remove unused variables
CPVCS    
CPVCS       Rev 1.57   Tue Jun 29 14:25:04 1999   jtg
CPVCS    removed the tabs ...
CPVCS
CPVCS       Rev 1.56   Tue Jun 29 13:53:30 1999   jtg
CPVCS    additional local element data structures added (ielmedge2,
CPVCS    ielmface3,ielmface4,ielmnode0,ielmnode1,ielmnode2,ielmnode3)
CPVCS
CPVCS       Rev 1.55   Wed Sep 30 11:59:22 1998   dcg
CPVCS    fix qmbuff common
CPVCS
CPVCS       Rev 1.54   Fri Jun 19 16:51:22 1998   dcg
CPVCS    allow longer dictionary path
CPVCS
CPVCS       Rev 1.53   Wed Nov 26 11:26:58 1997   dcg
CPVCS    ibm change - remove data statement for variable not in common
CPVCS
CPVCS       Rev 1.52   Wed Nov 19 16:46:44 1997   dcg
CPVCS    add save statement
CPVCS    add geometry common block and include file
CPVCS
CPVCS       Rev 1.50   Fri Oct 03 12:07:34 1997   dcg
CPVCS    fix getcmds common blocks - make lengths match
CPVCS    lengths in getcmds.f
CPVCS
CPVCS       Rev 1.49   Wed May 14 15:19:14 1997   dcg
CPVCS    eliminate double initializing of nwrunpar, irunparm(1)
CPVCS    these were equivalenced in a .h file
CPVCS
CPVCS       Rev 1.48   Mon Apr 14 16:39:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.47   Tue Nov 05 10:05:12 1996   dcg
CPVCS    separate integer,real and charater variable in common
CPVCS
CPVCS       Rev 1.46   Wed May 22 10:13:30 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.45   Thu May 16 10:21:42 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.43   Fri May 03 10:46:58 1996   dcg
CPVCS    changes for new point type --virtual interfaces
CPVCS
CPVCS       Rev 1.42   Fri Mar 08 09:41:56 1996   jxf
CPVCS    Added marching cubes tables for prisms.
CPVCS
CPVCS       Rev 1.41   Wed Jan 24 12:21:48 1996   jxf
CPVCS    Added common block and data for isosurface marching cubes.
CPVCS
CPVCS       Rev 1.40   11/16/95 17:10:16   het
CPVCS    Modify the inside_hex, inside_tet, inside_quad and inside_tri routines
CPVCS
CPVCS       Rev 1.39   09/13/95 14:34:34   het
CPVCS    Correct an error
CPVCS
CPVCS       Rev 1.38   08/30/95 21:07:34   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.37   08/22/95 06:52:06   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.36   07/17/95 16:02:16   dcg
CPVCS    use names for point types
CPVCS
CPVCS       Rev 1.35   06/07/95 16:13:02   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.34   06/05/95 10:37:48   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.33   05/30/95 07:50:30   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.32   05/26/95 13:23:18   het
CPVCS
CPVCS
CPVCS       Rev 1.31   05/23/95 08:55:18   het
CPVCS    Add the include file for blockini.h
CPVCS
CPVCS       Rev 1.30   05/11/95 13:24:18   het
CPVCS    Initialize the nwrunprm variable
CPVCS
CPVCS       Rev 1.29   05/05/95 09:23:56   ejl
CPVCS    Changed name of epsilond to epsilonl.
CPVCS
CPVCS       Rev 1.28   05/04/95 10:16:16   ejl
CPVCS    Added epsilond, epsilona, epsilonv to the dictionary
CPVCS
CPVCS       Rev 1.27   05/02/95 16:46:20   het
CPVCS    Correct an error with nwrunpar in blockcom.f
CPVCS
CPVCS       Rev 1.26   05/01/95 08:35:40   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.25   03/31/95 12:42:44   dcg
CPVCS    fix psetc common block to match character*32 lengths
CPVCS
CPVCS
CPVCS       Rev 1.22   03/28/95 12:35:14   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.21   03/23/95 22:57:14   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.20   03/17/95 21:11:22   het
CPVCS    Add the model and dictionary calles
CPVCS
CPVCS       Rev 1.19   02/16/95 10:37:16   ejl
CPVCS    Changed some of the entries in the Default Mesh Object.
CPVCS
CPVCS       Rev 1.18   02/10/95 09:09:36   ejl
CPVCS    New routines: copy, move, delatt, length, list.
CPVCS    Cleaned up, implicit none.
CPVCS
CPVCS       Rev 1.17   01/31/95 13:20:06   het
CPVCS    Make cmo a common block variable
CPVCS
CPVCS       Rev 1.16   01/31/95 07:02:42   het
CPVCS    Add ign1 to the list of default mesh_object variables
CPVCS
CPVCS       Rev 1.15   01/30/95 06:22:30   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.14   01/24/95 08:54:42   het
CPVCS    Correct an error initializiing the cmo data.
CPVCS
CPVCS
CPVCS       Rev 1.13   01/23/95 12:41:18   het
CPVCS    Define the default cmo_attribute table.
CPVCS
CPVCS
CPVCS       Rev 1.12   01/19/95 09:51:58   het
CPVCS
CPVCS
CPVCS       Rev 1.11   01/13/95 18:20:10   het
CPVCS
CPVCS
CPVCS       Rev 1.10   01/09/95 17:32:14   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.9   01/02/95 15:11:32   het
CPVCS    Correct the common for getcmds.
CPVCS
CPVCS
CPVCS       Rev 1.8   12/24/94 11:10:38   het
CPVCS    Remove the xbb() and coord() arrays to the chydro.h include file.
CPVCS
CPVCS
CPVCS       Rev 1.7   12/11/94 18:35:20   het
CPVCS    Fixed an error for the SGI.
CPVCS
CPVCS
CPVCS       Rev 1.6   12/09/94 22:48:40   het
CPVCS    Make changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.5   12/06/94 19:09:42   het
CPVCS    Add the data definition for the current mesh object definition.
CPVCS
CPVCS
CPVCS       Rev 1.4   12/02/94 16:18:50   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.3   11/23/94 10:15:02   dcg
CPVCS     fixed IBM specific compiler errors
CPVCS
CPVCS       Rev 1.2   11/22/94 14:45:28   dcg
CPVCS      change default code version information
CPVCS
CPVCS       Rev 1.1   11/17/94 22:04:52   het
CPVCS    Added block data for the "neibor.h" include file.
CPVCS
C
C#######################################################################
C
      include "chydro.h"
      include "consts.h"
      include "cmerge.h"
      include "local_element.h"
      save

C
C     ******************************************************************
C
C     LOG MESSAGES.
C
      character logmess*132
      common /logmes1/ logmess
C
C#######################################################################
C
C        THIS COMDECK CONTAINS NEIGHBOR RECONNECTION DATA.

C
      include "neibor.h"
C
C
C
C     THESE ARRAYS CONTAIN THE LAST MESSAGE PROCESSED SO THAT IT CAN BE
C     REPEATED.
      integer nd,id
      parameter ( nd=5  )
      common /getcmd1i/  id(nd)

      integer isotyp,isodat
      integer iiq
      common /mcube/ isotyp(368), isodat(8,368)
C
      common/kent1/igeom_gwt
      integer igeom_gwt
C
C     THIS COMMON PROVIDES SPACE FOR A CFTLIB MESSAGE BUFFER.
C     NOTE: THIS BUFFER IS 64 WORDS LONG FOLLOWED BY 1 WORD THAT
C           INDICATES HOW MANY CHARACTERS ARE IN THE BUFFER.  IF THE
C           FIRST WORD EQUALS -1, THEN NO MESSAGE IS PRESENT.
C
C
C     ******************************************************************
C
C     INITIALIZE MERGE DATA.
      integer i, j
C
      data(mtable( 0,i),i=0,59)/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 1,i),i=0,59)/1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 2,i),i=0,59)/1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 3,i),i=0,59)/1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 4,i),i=0,59)/1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 5,i),i=0,59)/1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 6,i),i=0,59)/1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 7,i),i=0,59)/1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 8,i),i=0,59)/1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable( 9,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(10,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(11,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(12,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(13,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(14,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(15,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(16,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(17,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(18,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(19,i),i=0,59)/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(20,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(21,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(22,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(23,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(24,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(25,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(26,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(27,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(28,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(29,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(30,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(31,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(32,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(33,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(34,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(35,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(36,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(37,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(38,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(39,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(40,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(41,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(42,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(43,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(44,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(45,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(46,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(47,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(48,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(49,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(50,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(51,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(52,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(53,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(54,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(55,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(56,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(57,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(58,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data(mtable(59,i),i=0,59)/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *                          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
C
C     ******************************************************************
C
C
C     TETRAHEDRON DATA
C
c     comment out initialization and let compiler take care of it
c     this causes fits on mac os x compilers
c     because we have 2 instances of pointer sizes
c     see kfix and xfix in neibor.h 
c     data ntetmax/0/
C     ******************************************************************
C
C
C     FITWORD PACKING/UNPACKING DATA FOR TYPE 1 FITWORDS.
C
      data ifitpst1, ifitpen1           /  0,  7     /
      data ifitpint, ifitpini           /  0,  2     /
      data ifitpvrt, ifitpvin           /  3,  4     /
      data ifitpst2, ifitpen2           /  8, 19     /
      data ifitpvif, ifitpalb           /  8,  9     /
      data ifitprfl, ifitpfre, ifitpirb, ifitpifb, ifitprfb, ifitpirf
     *                                  / 10, 11, 12, 13, 14, 15 /
      data ifitpvrb, ifitpvfb, ifitpvrf, ifitpvir
     *                                  / 16, 17, 18, 19         /
      data ifitpinb / 12 /
      data ifitpst3, ifitpen3           / 20, 29     /
      data ifitpmrg, ifitpdud           / 20, 21     /
      data ifitpst4, ifitpen4           / 30, 256    /
      data ifitppin, ifitpcup           / 30, 41     /
C     Point type names
      data inamprfl, inampfre, inampirb, inampifb, inamprfb, inampirf
     *      , inampint, inampdud, inampmrg, inamppar, inampini
     *      /'rfl','fre','irb','ifb','rfb','ifr','int','dud','mrg','par'
     *     ,'ini'/
      data inampvrb, inampvfb, inampvrf, inampvir, inampvif, inampalb
     *      /'vrb','vfb','vrf','vir','vif','alb'/
      data inampvrt, inampvin
     *      /'vrt','vin'/
C
C
C     ******************************************************************
C
C     INITIALIZE NEIGHBOR VARIALBES.
C
c
C     __________________________________________________________________
C

C     SET SOME MERGING COUNTERS.
C
      data npairs, npairs2  / 0, 0 /
C
C     ******************************************************************
C
C     SET COORDINATE SYSTEM ROTATION MATRIX (CURRENT AND SAVED) AND
C     COORDINATE SYSTEM ORIGIN (CURRENT AND SAVED) AND
C     NORMAL COORDINATE SYSTEM FLAGR(0=NORMAL).
C
      data (rotatc(1,i), i=1,3) /1., 0., 0./
      data (rotatc(2,i), i=1,3) /0., 1., 0./
      data (rotatc(3,i), i=1,3) /0., 0., 1./
      data (rotats(1,i), i=1,3) /1., 0., 0./
      data (rotats(2,i), i=1,3) /0., 1., 0./
      data (rotats(3,i), i=1,3) /0., 0., 1./
      data (origc(i), i=1,3) /0., 0., 0./
      data normflgc, normflgs / 0, 0 /
      data (origs(i), i=1,3) /0., 0., 0./

c
C     INITIALIZE BLOCK DATA STATAMENTS IN THE "NEIBOR.H" INCLUDE FILE.
C
C
C
C     ******************************************************************
C
C     "face" DATA
C
      data iflist / 2, 3, 4,
     *              1, 4, 3,
     *              1, 2, 4,
     *              1, 3, 2 /
C
C     ******************************************************************
C
C     "face3ds" DATA
C
      data iflists / 2, 3, 1, 3, 1, 2, 1, 2, 3 /
C
C     ******************************************************************
C
C     "ielist" OR EDGELIST DATA
C
      data ielist  / 1, 2,
     *               3, 4,
     *               1, 3,
     *               4, 2,
     *               1, 4,
     *               2, 3,
     *               2, 3,
     *               1, 4,
     *               2, 4,
     *               3, 1,
     *               3, 4,
     *               1, 2 /
C
C     ******************************************************************
C
C     "ielist2" OR ALTERNATIVE EDGELIST DATA
C
      data ielist2 / 4, 5,
     *               6, 2,
     *               3, 6,
     *               1, 3,
     *               5, 1,
     *               2, 4 /
C
C     ******************************************************************
C
C     "ifliplst" OR FLIP-LIST DATA
C
      data ifliplst / 1, 4,
     *                3, 2,
     *                3, 4,
     *                2, 4,
     *                5, 1,
     *                5, 4,
     *                2, 6,
     *                3, 1,
     *                3, 6,
     *                2, 5,
     *                6, 1,
     *                6, 5 /
C
C     ******************************************************************
C
C     "itpconv" OR POINT TYPE CONVERSION DATA
C
      data itpconv( 0) / 0 /
      data itpconv( 2) / 2 /
      data itpconv(10) / 1 /
      data itpconv(11) / 1 /
      data itpconv(12) / 5 /

C
C     ******************************************************************
C
C     REFLECTED BOUNDARY-POINT INFORMATION.
C
      data nb / 0 /
      data ibbflag / 0 /
      data ib / nbpr*0 /
      data xbb, ybb, zbb / nbpr3*0.0, nbpr3*0.0, nbpr3*0.0 /
      data ubb, vbb, wbb / nbpr3*0.0, nbpr3*0.0, nbpr3*0.0 /
      data xbbnorm, ybbnorm, zbbnorm / nbpr*0, nbpr*0, nbpr*0 /
      data pbb, rbb, ebb,tbb / nbpr*0, nbpr*0, nbpr*0, nbpr*0 /

C
C
C     ******************************************************************
C
C     INITIALIZE THE LOCAL ELEMENT INFO
C
C
      data celmnames / 'pnt',
     *                 'lin',
     *                 'tri',
     *                 'qud',
     *                 'tet',
     *                 'pyr',
     *                 'pri',
     *                 'hex',
     *                 'hyb',
     *                 'ply'
     *               /
C
      data ifelmpnt / 1 /
      data ifelmlin / 2 /
      data ifelmtri / 3 /
      data ifelmqud / 4 /
      data ifelmtet / 5 /
      data ifelmpyr / 6 /
      data ifelmpri / 7 /
      data ifelmhex / 8 /
      data ifelmhyb / 9 /
      data ifelmply / 10 /
C
      data nelmnen / 1, 2, 3, 4, 4, 5, 6,  8, 10, 10 /
      data nelmnef / 0, 2, 3, 4, 4, 5, 5,  6, 10, 10 /
      data nelmnee / 0, 1, 3, 4, 6, 8, 9, 12, 12, 12 /
C
C
C     *****************************************************************
C     Define the number of nodes associated with each element face.
C
C     see also ielmface3
C
      data ielmface0 / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
     *                 2, 2, 2, 2, 0, 0, 0, 0, 0, 0,
     *                 3, 3, 3, 3, 0, 0, 0, 0, 0, 0,
     *                 4, 3, 3, 3, 3, 0, 0, 0, 0, 0,
     *                 3, 3, 4, 4, 4, 0, 0, 0, 0, 0,
     *                 4, 4, 4, 4, 4, 4, 0, 0, 0, 0,
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     *               /
C
C
C
C     *****************************************************************
C     Define the list of local nodes associated with each element face.
C
      data ((ielmface1(i,j,1),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,2),i=1,maxnee1),j=1,maxnef) / 1, 0, 0, 0,
     *                                                   2, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,3),i=1,maxnee1),j=1,maxnef) / 2, 3, 0, 0,
     *                                                   3, 1, 0, 0,
     *                                                   1, 2, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,4),i=1,maxnee1),j=1,maxnef) / 1, 2, 0, 0,
     *                                                   2, 3, 0, 0,
     *                                                   3, 4, 0, 0,
     *                                                   4, 1, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,5),i=1,maxnee1),j=1,maxnef) / 2, 3, 4, 0,
     *                                                   1, 4, 3, 0,
     *                                                   1, 2, 4, 0,
     *                                                   1, 3, 2, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,6),i=1,maxnee1),j=1,maxnef) / 1, 4, 3, 2,
     *                                                   1, 2, 5, 0,
     *                                                   2, 3, 5, 0,
     *                                                   3, 4, 5, 0,
     *                                                   4, 1, 5, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,7),i=1,maxnee1),j=1,maxnef) / 1, 3, 2, 0,
     *                                                   4, 5, 6, 0,
     *                                                   1, 2, 5, 4,
     *                                                   2, 3, 6, 5,
     *                                                   1, 4, 6, 3,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,8),i=1,maxnee1),j=1,maxnef) / 1, 4, 3, 2,
     *                                                   5, 6, 7, 8,
     *                                                   1, 2, 6, 5,
     *                                                   2, 3, 7, 6,
     *                                                   3, 4, 8, 7,
     *                                                   1, 5, 8, 4,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,9),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface1(i,j,10),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0
     *                                                  /
C
C
C     *****************************************************************
C     Define the local edges associated with each face.
C
      data ((ielmface2(i,j,1),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,2),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,3),i=1,maxnee1),j=1,maxnef) / 3, 0, 0, 0,
     *                                                   2, 0, 0, 0,
     *                                                   1, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,4),i=1,maxnee1),j=1,maxnef) / 1, 0, 0, 0,
     *                                                   3, 0, 0, 0,
     *                                                   4, 0, 0, 0,
     *                                                   2, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,5),i=1,maxnee1),j=1,maxnef) / 6, 5, 4, 0,
     *                                                   6, 2, 3, 0,
     *                                                   5, 3, 1, 0,
     *                                                   4, 1, 2, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,6),i=1,maxnee1),j=1,maxnef) / 2, 6, 4, 1,
     *                                                   1, 5, 3, 0,
     *                                                   4, 7, 5, 0,
     *                                                   6, 8, 7, 0,
     *                                                   2, 3, 8, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,7),i=1,maxnee1),j=1,maxnef) / 2, 4, 1, 0,
     *                                                   7, 9, 8, 0,
     *                                                   1, 5, 7, 3,
     *                                                   4, 6, 9, 5,
     *                                                   3, 8, 6, 2,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,8),i=1,maxnee1),j=1,maxnef) / 2, 6, 4, 1,
     *                                                   9,11,12,10,
     *                                                   1, 5, 9, 3,
     *                                                   4, 7,11, 5,
     *                                                   6, 8,12, 7,
     *                                                   3,10, 8, 2,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,9),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0,
     *                                                   0, 0, 0, 0
     *                                                 /
      data ((ielmface2(i,j,10),i=1,maxnee1),j=1,maxnef) / 0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0,
     *                                                    0, 0, 0, 0
     *                                                  /
C
C
C     *****************************************************************
C     Define the list of local nodes associated with each edge.
C
      data ((ielmedge1(i,j,1),i=1,2),j=1,maxnee2) / 0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,2),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,3),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              1, 3,
     *                                              2, 3,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,4),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              1, 4,
     *                                              2, 3,
     *                                              3, 4,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,5),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              1, 3,
     *                                              1, 4,
     *                                              2, 3,
     *                                              2, 4,
     *                                              3, 4,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,6),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              1, 4,
     *                                              1, 5,
     *                                              2, 3,
     *                                              2, 5,
     *                                              3, 4,
     *                                              3, 5,
     *                                              4, 5,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,7),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              1, 3,
     *                                              1, 4,
     *                                              2, 3,
     *                                              2, 5,
     *                                              3, 6,
     *                                              4, 5,
     *                                              4, 6,
     *                                              5, 6,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,8),i=1,2),j=1,maxnee2) / 1, 2,
     *                                              1, 4,
     *                                              1, 5,
     *                                              2, 3,
     *                                              2, 6,
     *                                              3, 4,
     *                                              3, 7,
     *                                              4, 8,
     *                                              5, 6,
     *                                              5, 8,
     *                                              6, 7,
     *                                              7, 8
     *                                            /
      data ((ielmedge1(i,j,9),i=1,2),j=1,maxnee2) / 0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0,
     *                                              0, 0
     *                                            /
      data ((ielmedge1(i,j,10),i=1,2),j=1,maxnee2) / 0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0,
     *                                               0, 0
     *                                             /
C
C
C     *****************************************************************
C
C  Define the list of local faces associated with each edge.
C
C  ielmedge2(1:2,1:nelmnee(itettyp(iel)),1:nelmtypes)
C
C  jface=ielmedge2(i,j,ielmtyp)
C  jface is the i-th face touching the current edge j
C  for the current element type ielmtyp
C
C  order is so that ielmedge2 1->2 consistently ordered
C  with ielmedge1 1->2.
C
C [for tri,qud: it is the i-th face iface
C  (using the face label for the face=edge topological object)
C  around the current node j
C  (the node being the effective "edge" for the 2d object),
C  and is ordered consistently with order of element)
C
      data ((ielmedge2(i,j, 1),i=1,2),j=1,maxnee2)  !pnt
     *        / 0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 2),i=1,2),j=1,maxnee2)  !lin
     *        / 0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 3),i=1,2),j=1,maxnee2)  !tri
     *        / 3, 2,    1, 3,   2, 1,   0, 0,   0, 0,   0, 0,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 4),i=1,2),j=1,maxnee2)  !qud
     *        / 1, 4,    2, 1,   3, 2,   4, 3,   0, 0,   0, 0,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 5),i=1,2),j=1,maxnee2)  !tet
     *        / 4, 3,    2, 4,   3, 2,   4, 1,   1, 3,   2, 1,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 6),i=1,2),j=1,maxnee2)  !pyr
     *        / 1, 2,    5, 1,   2, 5,   1, 3,   3, 2,   1, 4,
     *          4, 3,    5, 4,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 7),i=1,2),j=1,maxnee2)  !pri
     *        / 1, 3,    5, 1,   3, 5,   1, 4,   4, 3,   5, 4,
     *          3, 2,    2, 5,   4, 2,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j, 8),i=1,2),j=1,maxnee2)  !hex
     *        / 1, 3,    6, 1,   3, 6,   1, 4,   4, 3,   1, 5,
     *          5, 4,    6, 5,   3, 2,   2, 6,   4, 2,   5, 2 /
      data ((ielmedge2(i,j, 9),i=1,2),j=1,maxnee2)  !hyb
     *        / 0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /
      data ((ielmedge2(i,j,10),i=1,2),j=1,maxnee2)  !ply
     *        / 0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0,
     *          0, 0,    0, 0,   0, 0,   0, 0,   0, 0,   0, 0 /

C
C     *****************************************************************
C
C Define the local face to face element type relation
C
C ielmface3(1:nelmnef(itettyp(iel)),1:nelmtypes)
C
C jelmtyp=ielmface3(i,ielmtyp)
C jelmtyp if the element type for the i-th face of the current element ielmtyp
C
C same as ielmface0 for all current element types, but keep in
C case this changes in the future (eg, using polygon face type "ply")
C
      data ielmface3 / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
     *                 2, 2, 2, 2, 0, 0, 0, 0, 0, 0,
     *                 3, 3, 3, 3, 0, 0, 0, 0, 0, 0,
     *                 4, 3, 3, 3, 3, 0, 0, 0, 0, 0,
     *                 3, 3, 4, 4, 4, 0, 0, 0, 0, 0,
     *                 4, 4, 4, 4, 4, 4, 0, 0, 0, 0,
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
     *               /
C
C
C
C     *****************************************************************
C
C Define the local face to neighbor local face relation
C
C ielmface4(1:ielmface0(j,itettyp(iel)),1:nelmnef(itettyp(iel)),1:nelmtypes)
C
C jface=ielmface4(i,j,ielmtyp)
C jface is the i-th face across the i-th edge ielmface2(i,j,ielmtyp)
C from the current face j
C
C [for tri,qud: face jface (face=edge topological object using face label)
C   across i-th local node inode=ielmface1(i,j,ielmtyp)]
C
      data ((ielmface4(i,j, 1),i=1,maxnee1),j=1,maxnef)  !pnt
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 2),i=1,maxnee1),j=1,maxnef)  !lin
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 3),i=1,maxnee1),j=1,maxnef)  !tri
     *        / 3, 2, 0, 0,   1, 3, 0, 0,   2, 1, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 4),i=1,maxnee1),j=1,maxnef)  !qud
     *        / 4, 2, 0, 0,   1, 3, 0, 0,   2, 4, 0, 0,
     *          3, 1, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 5),i=1,maxnee1),j=1,maxnef)  !tet
     *        / 2, 3, 4, 0,   1, 4, 3, 0,   1, 2, 4, 0,
     *          1, 3, 2, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 6),i=1,maxnee1),j=1,maxnef)  !pyr
     *        / 5, 4, 3, 2,   1, 3, 5, 0,   1, 4, 2, 0,
     *          1, 5, 3, 0,   1, 2, 4, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 7),i=1,maxnee1),j=1,maxnef)  !pri
     *        / 5, 4, 3, 0,   3, 4, 5, 0,   1, 4, 2, 5,
     *          1, 5, 2, 3,   3, 2, 4, 1,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 8),i=1,maxnee1),j=1,maxnef)  !hex
     *        / 6, 5, 4, 3,   3, 4, 5, 6,   1, 4, 2, 6,
     *          1, 5, 2, 3,   1, 6, 2, 4,   3, 2, 5, 1,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j, 9),i=1,maxnee1),j=1,maxnef)  !hyb
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmface4(i,j,10),i=1,maxnee1),j=1,maxnef)  !ply
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /

C
C     *****************************************************************
C
C  Define the number of ("opposite nodes",edges,faces) associated  w each node
C
C  ielmnode0(1:nelmnen(itettyp(iel)),1:nelmtypes)
C
C  analogous to ielmface0, this is the max values of the
C  "counter" for the ielmnodeX
C
C  for "opposite node" definition: see ielmnode1
C
      data ielmnode0
     *  / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *    1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     *    2, 2, 2, 0, 0, 0, 0, 0, 0, 0,
     *    2, 2, 2, 2, 0, 0, 0, 0, 0, 0,
     *    3, 3, 3, 3, 0, 0, 0, 0, 0, 0,
     *    3, 3, 3, 3, 4, 0, 0, 0, 0, 0,
     *    3, 3, 3, 3, 3, 3, 0, 0, 0, 0,
     *    3, 3, 3, 3, 3, 3, 3, 3, 0, 0,
     *    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
C
C     *****************************************************************
C
C Define the local node to "opposite" local node relation
C
C ielmnode1(1:ielmnode0(j,itettyp(iel)),1:nelmnen(itettyp(iel)),1:nelmtypes)
C
C jnode=ielmnode1(i,j,ielmtyp)
C jnode is node at other end of the i-th edge ielmnode2(i,j,ielmtyp)
C for the current node j of the current element ielmtyp
C
C order is positive interior angle
C
C "opposite node" means node at opposite end of the adjacent edge of the
C ielmnode2 relation
C
C [for tri,qud: uses edge label for face=edge topological object]
C [for lin: this gives the node on the other side of the element]
C
      data ((ielmnode1(i,j, 1),i=1,maxnee1),j=1,maxnen)  !pnt
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 2),i=1,maxnee1),j=1,maxnen)  !lin
     *        / 2, 0, 0, 0,   1, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 3),i=1,maxnee1),j=1,maxnen)  !tri
     *        / 2, 3, 0, 0,   3, 1, 0, 0,   1, 2, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 4),i=1,maxnee1),j=1,maxnen)  !qud
     *        / 2, 4, 0, 0,   3, 1, 0, 0,   4, 2, 0, 0,
     *          1, 3, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 5),i=1,maxnee1),j=1,maxnen)  !tet
     *        / 2, 3, 4, 0,   1, 4, 3, 0,   1, 2, 4, 0,
     *          1, 3, 2, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 6),i=1,maxnee1),j=1,maxnen)  !pyr
     *        / 2, 4, 5, 0,   3, 1, 5, 0,   4, 2, 5, 0,
     *          1, 3, 5, 0,   1, 4, 3, 2,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 7),i=1,maxnee1),j=1,maxnen)  !pri
     *        / 2, 3, 4, 0,   3, 1, 5, 0,   1, 2, 6, 0,
     *          6, 5, 1, 0,   4, 6, 2, 0,   5, 4, 3, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 8),i=1,maxnee1),j=1,maxnen)  !hex
     *        / 2, 4, 5, 0,   3, 1, 6, 0,   4, 2, 7, 0,
     *          1, 3, 8, 0,   8, 6, 1, 0,   5, 7, 2, 0,
     *          6, 8, 3, 0,   7, 5, 4, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j, 9),i=1,maxnee1),j=1,maxnen)  !hyb
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode1(i,j,10),i=1,maxnee1),j=1,maxnen)  !ply
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /

C
C     *****************************************************************
C
C Define the local node to adjacent local edges relation
C
C ielmnode2(1:ielmnode0(j,itettyp(iel)),1:nelmnen(itettyp(iel)),1:nelmtypes)
C
C jedge=ielmnode1(i,j,ielmtyp)
C jedge if the i-th edge connected connected to the current node i
C of the current element type ielmtyp
C
C order is positive interior angle (edge order same order as opp node)
C
C [for tri,qud: this uses the edge label for the face=edge topological object]
C [for lin: this uses the edge label which is topologically the "interior"]
C
      data ((ielmnode2(i,j, 1),i=1,maxnee1),j=1,maxnen)  !pnt
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 2),i=1,maxnee1),j=1,maxnen)  !lin
     *        / 1, 0, 0, 0,   1, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 3),i=1,maxnee1),j=1,maxnen)  !tri
     *        / 1, 2, 0, 0,   3, 1, 0, 0,   2, 3, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 4),i=1,maxnee1),j=1,maxnen)  !qud
     *        / 1, 2, 0, 0,   3, 1, 0, 0,   4, 3, 0, 0,
     *          2, 4, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 5),i=1,maxnee1),j=1,maxnen)  !tet
     *        / 1, 2, 3, 0,   1, 5, 4, 0,   2, 4, 6, 0,
     *          3, 6, 5, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 6),i=1,maxnee1),j=1,maxnen)  !pyr
     *        / 1, 2, 3, 0,   4, 1, 5, 0,   6, 4, 7, 0,
     *          2, 6, 8, 0,   3, 8, 7, 5,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 7),i=1,maxnee1),j=1,maxnen)  !pri
     *        / 1, 2, 3, 0,   4, 1, 5, 0,   2, 4, 6, 0,
     *          8, 7, 3, 0,   7, 9, 5, 0,   9, 8, 6, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 8),i=1,maxnee1),j=1,maxnen)  !hex
     *        / 1, 2, 3, 0,   4, 1, 5, 0,   6, 4, 7, 0,
     *          2, 6, 8, 0,  10, 9, 3, 0,   9,11, 5, 0,
     *         11,12, 7, 0,  12,10, 8, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j, 9),i=1,maxnee1),j=1,maxnen)  !hyb
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode2(i,j,10),i=1,maxnee1),j=1,maxnen)  !ply
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /

C
C     *****************************************************************
C
C Define the local node to adjacent local faces relation
C
C ielmnode3(1:ielmnode0(j,itettyp(iel)),1:nelmnen(itettyp(iel)),1:nelmtypes)
C
C jface=ielmnode3(i,j,ielmtyp)
C jface is the face between the i-th and (i+1)-th edges
C ielmnode2 (i,j,ielmtyp), ielmnode2(i+1,j,ielmtyp)
C of adjacent to the current node j of the current element typ ielmtyp
C
C order is positive interior angle (face 1 between opp edge/node 1,2 etc)
C
C [for tri,qud: this uses the face label for the face=edge topological object]
C [for lin:  this gives the edge label which is topologically the "interior"]
C
      data ((ielmnode3(i,j, 1),i=1,maxnee1),j=1,maxnen)  !pnt
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 2),i=1,maxnee1),j=1,maxnen)  !lin
     *        / 1, 0, 0, 0,   1, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 3),i=1,maxnee1),j=1,maxnen)  !tri
     *        / 3, 2, 0, 0,   1, 3, 0, 0,   2, 1, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 4),i=1,maxnee1),j=1,maxnen)  !qud
     *        / 1, 4, 0, 0,   2, 1, 0, 0,   3, 2, 0, 0,
     *          4, 3, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 5),i=1,maxnee1),j=1,maxnen)  !tet
     *        / 4, 2, 3, 0,   3, 1, 4, 0,   4, 1, 2, 0,
     *          2, 1, 3, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 6),i=1,maxnee1),j=1,maxnen)  !pyr
     *        / 1, 5, 2, 0,   1, 2, 3, 0,   1, 3, 4, 0,
     *          1, 4, 5, 0,   5, 4, 3, 2,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 7),i=1,maxnee1),j=1,maxnen)  !pri
     *        / 1, 5, 3, 0,   1, 3, 4, 0,   1, 4, 5, 0,
     *          2, 3, 5, 0,   2, 4, 3, 0,   2, 5, 4, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 8),i=1,maxnee1),j=1,maxnen)  !hex
     *        / 1, 6, 3, 0,   1, 3, 4, 0,   1, 4, 5, 0,
     *          1, 5, 6, 0,   2, 3, 6, 0,   2, 4, 3, 0,
     *          2, 5, 4, 0,   2, 6, 5, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j, 9),i=1,maxnee1),j=1,maxnen)  !hyb
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
      data ((ielmnode3(i,j,10),i=1,maxnee1),j=1,maxnen)  !ply
     *        / 0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0,   0, 0, 0, 0,   0, 0, 0, 0,
     *          0, 0, 0, 0    /
C     ******************************************************************
C
C     DATA FOR ISOSURFACE.F MARCHING CUBES
C
C
C   First 16 are for tets and only go out to 4 in the first index.
C
      data(isodat(1,iiq),iiq=1,16)/0,1,2,1,3,1,2,4,4,2,1,3,1,2,1,0/
      data(isodat(2,iiq),iiq=1,16)/0,2,1,2,1,3,3,3,3,3,3,1,2,1,2,0/
      data(isodat(3,iiq),iiq=1,16)/0,3,3,3,2,4,4,2,2,4,4,2,3,3,3,0/
      data(isodat(4,iiq),iiq=1,16)/0,4,4,4,4,2,1,1,1,1,2,4,4,4,4,0/
C
C   The next 256 are for hexes.
C
      data(isodat(iiq,17),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,18),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,19),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,20),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,21),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,22),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,23),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,24),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,25),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,26),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,27),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,28),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,29),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,30),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,31),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,32),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,33),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,34),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,35),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,36),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,37),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,38),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,39),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,40),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,41),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,42),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,43),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,44),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,45),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,46),iiq=1,8)/1,4,2,3,5,8,6,7/
      data(isodat(iiq,47),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,48),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,49),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,50),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,51),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,52),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,53),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,54),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,55),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,56),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,57),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,58),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,59),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,60),iiq=1,8)/2,1,3,4,6,5,7,8/
      data(isodat(iiq,61),iiq=1,8)/3,2,7,6,4,1,8,5/
      data(isodat(iiq,62),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,63),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,64),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,65),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,66),iiq=1,8)/5,6,1,2,8,7,4,3/
      data(isodat(iiq,67),iiq=1,8)/6,2,5,1,7,3,8,4/
      data(isodat(iiq,68),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,69),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,70),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,71),iiq=1,8)/6,2,7,3,5,1,8,4/
      data(isodat(iiq,72),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,73),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,74),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,75),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,76),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,77),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,78),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,79),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,80),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,81),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,82),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,83),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,84),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,85),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,86),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,87),iiq=1,8)/3,2,7,6,4,1,8,5/
      data(isodat(iiq,88),iiq=1,8)/3,2,4,1,7,6,8,5/
      data(isodat(iiq,89),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,90),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,91),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,92),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,93),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,94),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,95),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,96),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,97),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,98),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,99),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,100),iiq=1,8)/6,2,5,1,7,3,8,4/
      data(isodat(iiq,101),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,102),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,103),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,104),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,105),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,106),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,107),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,108),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,109),iiq=1,8)/8,4,7,3,5,1,6,2/
      data(isodat(iiq,110),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,111),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,112),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,113),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,114),iiq=1,8)/6,2,5,1,7,3,8,4/
      data(isodat(iiq,115),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,116),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,117),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,118),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,119),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,120),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,121),iiq=1,8)/7,8,3,4,6,5,2,1/
      data(isodat(iiq,122),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,123),iiq=1,8)/3,2,7,6,4,1,8,5/
      data(isodat(iiq,124),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,125),iiq=1,8)/3,7,2,6,4,8,1,5/
      data(isodat(iiq,126),iiq=1,8)/3,2,7,6,4,1,8,5/
      data(isodat(iiq,127),iiq=1,8)/3,2,7,6,4,1,8,5/
      data(isodat(iiq,128),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,129),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,130),iiq=1,8)/5,6,8,7,1,2,4,3/
      data(isodat(iiq,131),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,132),iiq=1,8)/6,2,5,1,7,3,8,4/
      data(isodat(iiq,133),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,134),iiq=1,8)/6,2,5,1,7,3,8,4/
      data(isodat(iiq,135),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,136),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,137),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,138),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,139),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,140),iiq=1,8)/2,1,6,5,3,4,7,8/
      data(isodat(iiq,141),iiq=1,8)/7,8,3,4,6,5,2,1/
      data(isodat(iiq,142),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,143),iiq=1,8)/2,6,3,7,1,5,4,8/
      data(isodat(iiq,144),iiq=1,8)/2,3,1,4,6,7,5,8/
      data(isodat(iiq,145),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,146),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,147),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,148),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,149),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,150),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,151),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,152),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,153),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,154),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,155),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,156),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,157),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,158),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,159),iiq=1,8)/4,3,1,2,8,7,5,6/
      data(isodat(iiq,160),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,161),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,162),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,163),iiq=1,8)/5,6,1,2,8,7,4,3/
      data(isodat(iiq,164),iiq=1,8)/1,5,4,8,2,6,3,7/
      data(isodat(iiq,165),iiq=1,8)/8,4,7,3,5,1,6,2/
      data(isodat(iiq,166),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,167),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,168),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,169),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,170),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,171),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,172),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,173),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,174),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,175),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,176),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,177),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,178),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,179),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,180),iiq=1,8)/5,6,1,2,8,7,4,3/
      data(isodat(iiq,181),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,182),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,183),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,184),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,185),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,186),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,187),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,188),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,189),iiq=1,8)/7,8,3,4,6,5,2,1/
      data(isodat(iiq,190),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,191),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,192),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,193),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,194),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,195),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,196),iiq=1,8)/5,6,1,2,8,7,4,3/
      data(isodat(iiq,197),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,198),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,199),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,200),iiq=1,8)/1,5,2,6,4,8,3,7/
      data(isodat(iiq,201),iiq=1,8)/8,5,7,6,4,1,3,2/
      data(isodat(iiq,202),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,203),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,204),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,205),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,206),iiq=1,8)/1,4,5,8,2,3,6,7/
      data(isodat(iiq,207),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,208),iiq=1,8)/1,2,4,3,5,6,8,7/
      data(isodat(iiq,209),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,210),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,211),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,212),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,213),iiq=1,8)/7,8,3,4,6,5,2,1/
      data(isodat(iiq,214),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,215),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,216),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,217),iiq=1,8)/8,4,7,3,5,1,6,2/
      data(isodat(iiq,218),iiq=1,8)/8,4,5,1,7,3,6,2/
      data(isodat(iiq,219),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,220),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,221),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,222),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,223),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,224),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,225),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,226),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,227),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,228),iiq=1,8)/5,6,1,2,8,7,4,3/
      data(isodat(iiq,229),iiq=1,8)/7,8,6,5,3,4,2,1/
      data(isodat(iiq,230),iiq=1,8)/8,4,7,3,5,1,6,2/
      data(isodat(iiq,231),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,232),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,233),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,234),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,235),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,236),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,237),iiq=1,8)/8,4,7,3,5,1,6,2/
      data(isodat(iiq,238),iiq=1,8)/4,8,1,5,3,7,2,6/
      data(isodat(iiq,239),iiq=1,8)/4,3,8,7,1,2,5,6/
      data(isodat(iiq,240),iiq=1,8)/4,1,3,2,8,5,7,6/
      data(isodat(iiq,241),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,242),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,243),iiq=1,8)/7,6,3,2,8,5,4,1/
      data(isodat(iiq,244),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,245),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,246),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,247),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,248),iiq=1,8)/6,7,2,3,5,8,1,4/
      data(isodat(iiq,249),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,250),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,251),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,252),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,253),iiq=1,8)/7,8,3,4,6,5,2,1/
      data(isodat(iiq,254),iiq=1,8)/3,7,4,8,2,6,1,5/
      data(isodat(iiq,255),iiq=1,8)/7,3,6,2,8,4,5,1/
      data(isodat(iiq,256),iiq=1,8)/3,4,2,1,7,8,6,5/
      data(isodat(iiq,257),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,258),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,259),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,260),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,261),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,262),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,263),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,264),iiq=1,8)/6,5,7,8,2,1,3,4/
      data(isodat(iiq,265),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,266),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,267),iiq=1,8)/5,8,6,7,1,4,2,3/
      data(isodat(iiq,268),iiq=1,8)/5,1,8,4,6,2,7,3/
      data(isodat(iiq,269),iiq=1,8)/8,7,5,6,4,3,1,2/
      data(isodat(iiq,270),iiq=1,8)/8,5,4,1,7,6,3,2/
      data(isodat(iiq,271),iiq=1,8)/7,6,8,5,3,2,4,1/
      data(isodat(iiq,272),iiq=1,8)/1,2,4,3,5,6,8,7/
C
C   The next 32 are for pyramids.
C
      data(isodat(iiq,273),iiq=1,8)/8*0/
      data(isodat(iiq,274),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,275),iiq=1,8)/2,1,3,5,0,0,0,0/
      data(isodat(iiq,276),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,277),iiq=1,8)/3,2,1,4,0,0,0,0/
      data(isodat(iiq,278),iiq=1,8)/1,3,4,5,2,0,0,0/
      data(isodat(iiq,279),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,280),iiq=1,8)/1,5,4,3,2,0,0,0/
      data(isodat(iiq,281),iiq=1,8)/4,3,1,5,0,0,0,0/
      data(isodat(iiq,282),iiq=1,8)/1,4,5,2,3,0,0,0/
      data(isodat(iiq,283),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,284),iiq=1,8)/1,5,2,3,4,0,0,0/
      data(isodat(iiq,285),iiq=1,8)/1,4,3,2,5,0,0,0/
      data(isodat(iiq,286),iiq=1,8)/1,5,2,3,4,0,0,0/
      data(isodat(iiq,287),iiq=1,8)/1,5,2,3,4,0,0,0/
      data(isodat(iiq,288),iiq=1,8)/5,2,1,4,0,0,0,0/
      data(isodat(iiq,289),iiq=1,8)/5,2,1,4,0,0,0,0/
      data(isodat(iiq,290),iiq=1,8)/1,5,2,3,4,0,0,0/
      data(isodat(iiq,291),iiq=1,8)/1,5,2,3,4,0,0,0/
      data(isodat(iiq,292),iiq=1,8)/1,4,3,2,5,0,0,0/
      data(isodat(iiq,293),iiq=1,8)/1,5,2,3,4,0,0,0/
      data(isodat(iiq,294),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,295),iiq=1,8)/1,4,5,2,3,0,0,0/
      data(isodat(iiq,296),iiq=1,8)/4,3,1,5,0,0,0,0/
      data(isodat(iiq,297),iiq=1,8)/1,5,4,3,2,0,0,0/
      data(isodat(iiq,298),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,299),iiq=1,8)/1,3,4,5,2,0,0,0/
      data(isodat(iiq,300),iiq=1,8)/3,2,1,4,0,0,0,0/
      data(isodat(iiq,301),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,302),iiq=1,8)/2,1,3,5,0,0,0,0/
      data(isodat(iiq,303),iiq=1,8)/1,2,3,4,5,0,0,0/
      data(isodat(iiq,304),iiq=1,8)/8*0/
C
C   The next 64 are for prisms.
C
      data(isodat(iiq,305),iiq=1,8)/8*0/
      data(isodat(iiq,306),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,307),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,308),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,309),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,310),iiq=1,8)/1,3,2,4,6,5,0,0/
      data(isodat(iiq,311),iiq=1,8)/3,2,1,6,5,4,0,0/
      data(isodat(iiq,312),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,313),iiq=1,8)/4,1,5,6,2,4,0,0/
      data(isodat(iiq,314),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,315),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,316),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,317),iiq=1,8)/1,3,2,4,6,5,0,0/
      data(isodat(iiq,318),iiq=1,8)/1,3,2,4,6,5,0,0/
      data(isodat(iiq,319),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,320),iiq=1,8)/5,6,4,2,3,1,0,0/
      data(isodat(iiq,321),iiq=1,8)/5,2,4,6,1,3,0,0/
      data(isodat(iiq,322),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,323),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,324),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,325),iiq=1,8)/2,3,1,5,6,4,0,0/
      data(isodat(iiq,326),iiq=1,8)/2,3,1,5,6,4,0,0/
      data(isodat(iiq,327),iiq=1,8)/2,3,1,5,6,4,0,0/
      data(isodat(iiq,328),iiq=1,8)/4,6,5,1,3,2,0,0/
      data(isodat(iiq,329),iiq=1,8)/5,4,6,2,1,3,0,0/
      data(isodat(iiq,330),iiq=1,8)/3,2,1,6,5,4,0,0/
      data(isodat(iiq,331),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,332),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,333),iiq=1,8)/6,5,4,3,2,1,0,0/
      data(isodat(iiq,334),iiq=1,8)/3,2,1,6,5,4,0,0/
      data(isodat(iiq,335),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,336),iiq=1,8)/6,3,4,5,1,2,0,0/
      data(isodat(iiq,337),iiq=1,8)/6,3,4,5,1,2,0,0/
      data(isodat(iiq,338),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,339),iiq=1,8)/3,2,1,6,5,4,0,0/
      data(isodat(iiq,340),iiq=1,8)/6,5,4,3,2,1,0,0/
      data(isodat(iiq,341),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,342),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,343),iiq=1,8)/3,2,1,6,5,4,0,0/
      data(isodat(iiq,344),iiq=1,8)/5,4,6,2,1,3,0,0/
      data(isodat(iiq,345),iiq=1,8)/4,6,5,1,3,2,0,0/
      data(isodat(iiq,346),iiq=1,8)/2,3,1,5,6,4,0,0/
      data(isodat(iiq,347),iiq=1,8)/5,6,4,2,3,1,0,0/
      data(isodat(iiq,348),iiq=1,8)/2,3,1,5,6,4,0,0/
      data(isodat(iiq,349),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,350),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,351),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,352),iiq=1,8)/5,2,4,6,1,3,0,0/
      data(isodat(iiq,353),iiq=1,8)/5,6,4,2,3,1,0,0/
      data(isodat(iiq,354),iiq=1,8)/4,6,5,1,3,2,0,0/
      data(isodat(iiq,355),iiq=1,8)/1,3,2,4,6,5,0,0/
      data(isodat(iiq,356),iiq=1,8)/1,3,2,4,6,5,0,0/
      data(isodat(iiq,357),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,358),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,359),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,360),iiq=1,8)/4,1,5,6,2,3,0,0/
      data(isodat(iiq,361),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,362),iiq=1,8)/2,3,1,5,6,4,0,0/
      data(isodat(iiq,363),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,364),iiq=1,8)/3,1,2,6,4,5,0,0/
      data(isodat(iiq,365),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,366),iiq=1,8)/2,1,3,5,4,6,0,0/
      data(isodat(iiq,367),iiq=1,8)/1,2,3,4,5,6,0,0/
      data(isodat(iiq,368),iiq=1,8)/8*0/
C
C   First 16 are for tets.
C
      data(isotyp(iiq),iiq=1,368)/0,1,1,2,1,2,2,1,1,2,2,1,2,1,1,0,
C
C   The next 256 are for hexes.
C
     * 0,1,1,3,1,6,3,7,1,3,6,7,3,7,7,15,1,3,6,
     * 7,24,25,25,27,6,7,22,23,25,39,30,31,1,6,3,7,6,
     * 22,7,23,24,25,25,39,25,30,27,31,3,7,7,15,25,30,39,
     * 31,25,27,30,31,60,61,61,63,1,24,6,25,3,25,7,39,6,
     * 25,22,30,7,27,23,31,6,25,22,30,25,60,30,61,22,30,0,
     * 107,30,61,107,111,3,25,7,27,7,30,15,31,25,60,30,61,39,
     * 61,31,63,7,39,23,31,27,61,31,63,30,61,107,111,61,126,111,
     * 127,1,6,24,25,6,22,25,30,3,7,25,27,7,23,39,31,3,
     * 7,25,39,25,30,60,61,7,15,30,31,27,31,61,63,6,22,25,
     * 30,22,0,30,107,25,30,60,61,30,107,61,111,7,23,27,31,30,
     * 107,61,111,39,31,61,63,61,111,126,127,3,25,25,60,7,30,27,
     * 61,7,39,30,61,15,31,31,63,7,27,30,61,39,61,61,126,23,
     * 31,107,111,31,63,111,127,7,30,39,61,23,107,31,111,27,61,61,
     * 126,31,111,63,127,15,31,31,63,31,111,63,127,31,63,111,127,
     * 63,127,127,0,
C
C   The next 32 are for pyramids.
C
     * 0,5,1,2,1,2,3,3,1,2,4,4,3,3,2,1,1,2,3,3,4,4,2,1,3,3,2,1,2,
     * 1,5,0,
C
C   The next 64 are for prisms.
C
     * 0,1,1,3,1,3,3,2,1,7,4,5,4,5,6,3,1,4,7,5,4,6,5,3,
     * 3,5,5,7,6,4,4,1,1,4,4,6,7,5,5,3,3,5,6,4,5,7,4,1,
     * 3,6,5,4,5,4,7,1,2,3,3,1,3,1,1,0/
C
C    *******************************************************************

C
C     Add Kent Smith's geometry factor for the GWT algorithm
C
      data igeom_gwt / 0 /
C
C
      data pie/ 3.141592653589793 /

      end
C    *******************************************************************


