c
C
C ######################################################################
C
C     PURPOSE -
C
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
C        $Log: chydro.h,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.30   10 Apr 2001 09:49:14   dcg
CPVCS    remove rinter
CPVCS
CPVCS       Rev 1.29   Fri Nov 12 09:06:20 1999   dcg
CPVCS    remove unused variables
CPVCS
CPVCS       Rev 1.27   Thu Jan 21 20:53:48 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS
CPVCS       Rev 1.26   Wed Nov 04 22:35:20 1998   jtg
CPVCS    moved common blocks to after declarations
CPVCS
CPVCS       Rev 1.25   Wed Nov 19 10:08:08 1997   dcg
CPVCS    remove obsolete code and declarations
CPVCS
CPVCS       Rev 1.24   Mon Apr 14 16:37:12 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.23   Tue Nov 05 10:07:22 1996   dcg
CPVCS    separate integer, real and character variables in common blocks
CPVCS
CPVCS       Rev 1.22   Thu May 23 13:53:06 1996   dcg
CPVCS
CPVCS       Rev 1.21   Thu May 16 10:22:26 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.20   Fri May 03 10:45:42 1996   dcg
CPVCS    add point types for virtual interfaces
CPVCS
CPVCS       Rev 1.19   12/05/95 08:18:56   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.18   07/17/95 15:59:32   dcg
CPVCS    add names for point types
CPVCS
CPVCS       Rev 1.17   06/05/95 10:44:16   het
CPVCS    Add the run-time parameters back into the code
CPVCS
CPVCS       Rev 1.16   05/30/95 07:56:16   het
CPVCS    Cleanup the global parameter list
CPVCS
CPVCS       Rev 1.15   05/19/95 12:47:58   dcg
CPVCS    declare initnpf type character
CPVCS
CPVCS       Rev 1.14   05/19/95 11:31:56   ejl
CPVCS    Fixed error in Typing
CPVCS
CPVCS       Rev 1.13   05/19/95 10:42:08   ejl
CPVCS    Typed all variables.
CPVCS
CPVCS       Rev 1.12   05/17/95 12:54:48   dcg
CPVCS    remove external statement
CPVCS
CPVCS       Rev 1.11   05/11/95 13:25:16   het
CPVCS    Add the new point type variables
CPVCS
CPVCS       Rev 1.10   05/04/95 10:15:36   ejl
CPVCS    Added epsilond, epsilona, epsilonv to the dictionary
CPVCS
CPVCS       Rev 1.9   05/01/95 08:56:32   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.8   03/23/95 22:57:22   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.7   01/19/95 11:53:40   het
CPVCS
CPVCS
CPVCS       Rev 1.6   01/19/95 09:54:18   het
CPVCS
CPVCS
CPVCS       Rev 1.5   01/04/95 22:06:50   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.4   12/26/94 10:36:38   het
CPVCS    Added the rotate coordinates system common blocks.
CPVCS
CPVCS
CPVCS       Rev 1.3   12/09/94 22:45:46   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:52:04   het
CPVCS    Modified the "cmo" support data structures.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/17/94 21:58:48   het
CPVCS    Modified the variable storage common block to contain all X3D variables in
CPVCS    integer, real and character form.
CPVCS
CPVCS       Rev 1.0   11/13/94 15:41:00   het
CPVCS    Orginal Version
C
C ######################################################################
C
C
C***********************************************************************
C***********************************************************************
C     DEFINE THE SCCS INFORMATION BLOCK
C
C        %Z%%M%      %I%      %E%
C
C***********************************************************************
C***********************************************************************
C
C
C
C     idouble = 1 ==> SINGLE PRECESION
C     idouble = 2 ==> DOUBLE PRECESION
C
      integer idouble
      common /idoublea/ idouble
      save /idoublea/
c     external integer iprecision
C
C
C#######################################################################
C
C
C     ******************************************************************
C
C     CONSTANTS FOR PACKING/UNPACKING FIT-WORDS IN VECTOR MODE.
C
C
C
C     ******************************************************************
C     DEFINE THE CONSTANT PARAMETERS NEEDED TO UNPACK AND TEST
C        FITWORD DATA.
C
C     ..................................................................
C
C     ..................................................................
      integer ifwstor
      parameter (ifwstor=20)
C
      integer            ifitpst1, ifitpen1,
     *                      ifitpint, ifitpini,
     *                      ifitpvrt, ifitpvin,
     *                   ifitpst2, ifitpen2,
     *                      ifitprfl, ifitpfre, ifitpirb,
     *                      ifitpifb, ifitprfb, ifitpirf,
     *                      ifitpvrb, ifitpvfb, ifitpvrf,
     *                      ifitpvir, ifitpvif, ifitpalb,
     *                   ifitpst3, ifitpen3,
     *                      ifitpmrg, ifitpdud,
     *                   ifitpst4, ifitpen4,
     *                      ifitppin, ifitpcup
C
      common /ifitwrdc/  ifitpst1, ifitpen1,
     *                      ifitpint, ifitpini,
     *                      ifitpvrt, ifitpvin,
     *                   ifitpst2, ifitpen2,
     *                      ifitprfl, ifitpfre, ifitpirb,
     *                      ifitpifb, ifitprfb, ifitpirf,
     *                      ifitpvrb, ifitpvfb, ifitpvrf,
     *                      ifitpvir, ifitpvif, ifitpalb,
     *                   ifitpst3, ifitpen3,
     *                      ifitpmrg, ifitpdud,
     *                   ifitpst4, ifitpen4,
     *                      ifitppin, ifitpcup
      save /ifitwrdc/
C
      integer            ifitpinb
      common /ifitwrdd/  ifitpinb
      save /ifitwrdd/
      character *3
     *     inamprfl, inampfre, inampirb, inampifb, inamprfb, inampirf
     *      , inampint, inampdud, inampmrg, inamppar, inampini,
     *     inampvrb, inampvfb, inampvrf, inampvir, inampvif, inampalb,
     *     inampvrt, inampvin
      common/ ifitwrde/
     *     inamprfl, inampfre, inampirb, inampifb, inamprfb, inampirf
     *      , inampint, inampdud, inampmrg, inamppar, inampini,
     *     inampvrb, inampvfb, inampvrf, inampvir, inampvif, inampalb,
     *     inampvrt, inampvin
      save/ ifitwrde/
C
C     DEFINE PACKING PARAMETERS FOR INTERIOR POINTS:
C
C      ** IFITPST1 -DEFINES THE STARTING(MINIMUM) INDEX FOR GROUP 1.
C      ** IFITPEN1 -DEFINES THE ENDING(MAXIMUM) INDEX FOR GROUP 1.
C      ** IFITPINT -DEFINES THE VALUE FOR AN INTERIOR POINT.
C      ** IFITPINI -DEFINES THE VALUE FOR AN (INTERIOR) INTERFACE POINT.
C
C     ..................................................................
C     DEFINE PACKING PARAMETERS FOR BOUNDARY POINTS
C
C        (I.E., EXTERIOR BOUNDARIES)
C      ** IFITPST2 -DEFINES THE STARTING(MINIMUM) INDEX FOR GROUP 2.
C      ** IFITPEN2 -DEFINES THE ENDING(MAXIMUM) INDEX FOR GROUP 2.
C      ** IFITPRFL -DEFINES THE VALUE FOR AN (EXTERIOR) REFLECTIVE
C                      BOUNDARY POINTS.
C      ** IFITPFRE -DEFINES THE VALUE FOR A FREE-SURFACE BOUNDARY POINT.
C      ** IFITPIRB -DEFINES THE VALUE FOR A INTERFACE-REFLECTIVE POINT.
C      ** IFITPIFB -DEFINES THE VALUE FOR A INTERFACE-FREE POINT.
C      ** IFITPRFB -DEFINES THE VALUE FOR A REFLECTIVE-FREE POINT.
C      ** IFITPIRF -DEFINES THE VALUE FOR A INTERFACE-REFLECTIVE-FREE PT
C
C     ..................................................................
C     DEFINE PACKING PARAMETERS FOR ACTIVE POINTS THAT ARE REMOVED
C        FROM THE MESH.
C
C      ** IFITPST3 -DEFINES THE STARTING(MINIMUM) INDEX FOR GROUP 3.
C      ** IFITPEN3 -DEFINES THE ENDING(MAXIMUM) INDEX FOR GROUP 3.
C      ** IFITPMRG -DEFINES THE VALUE FOR A MERGED POINT.
C      ** IFITPDUD -DEFINES THE VALUE FOR A DUDDED POINT.
C
C     ..................................................................
C     DEFINE PACKING PARAMETERS FOR MARKER POINTS.
C        FROM THE MESH.
C
C      ** IFITPST4 -DEFINES THE STARTING(MINIMUM) INDEX FOR GROUP 4.
C      ** IFITPEN4 -DEFINES THE ENDING(MAXIMUM) INDEX FOR GROUP 4.
C      ** IFITPPIN -DEFINES THE VALUE FOR A PIN-DATA MARKER POINT.
C      ** IFITPCUP -DEFINES THE VALUE FOR A COUPLED-POINTS.
C
C
C     REFLECTED BOUNDARY-POINT INFORMATION.
C
      integer nbpr, nbpr3
      parameter ( nbpr = 100, nbpr3 = 3*nbpr )
C
      integer          nb           , ibbflag        , ib
      REAL*8           xbb          , ybb            , zbb           ,
     *                 ubb          , vbb            , wbb           ,
     *                 xbbnorm      , ybbnorm        , zbbnorm       ,
     *                 pbb          , rbb            , ebb           ,
     *                 tbb
      common /bcondsi/ nb            , ibbflag       , ib(nbpr)
      common /bcondsr/ xbb(3,nbpr)   , ybb(3,nbpr)   , zbb(3,nbpr)   ,
     *                 ubb(3,nbpr)   , vbb(3,nbpr)   , wbb(3,nbpr)   ,
     *                 xbbnorm(nbpr) , ybbnorm(nbpr) , zbbnorm(nbpr) ,
     *                 pbb(nbpr)     , rbb(nbpr)     , ebb(nbpr)     ,
     *                 tbb(nbpr)
      save /bcondsi/
      save /bcondsr/
C
C
C     ******************************************************************
C
C     MEMORY NODE nodphy.
C
      integer inodphy
C
      pointer (ipinter ,iinter )
      common /phytmp/ ipinter, inodphy
      save /phytmp/
      integer iinter(6,1)
C
C     ******************************************************************
C
C     COORDINATE SYSTEM ROTATION MATRIX (CURRENT AND SAVED) AND
C     COORDINATE SYSTEM ORIGIN (CURRENT AND SAVED) AND
C     NORMAL COORDINATE SYSTEM FLAG (0=NORMAL, 1=LOCAL COORD. SYS.)
C
      REAL*8            origc   , origs
      integer           normflgc, normflgs
C
      real*8 rotatc
      real*8 rotats
      common /cordst0i/  normflgc, normflgs
      common /cordst0r/ origc(3), origs(3)
      common /cordset1/ rotatc(3,3)
      common /cordset2/ rotats(3,3)
      save /cordst0i/
      save /cordst0r/
      save /cordset1/
      save /cordset2/
c
c  global variables
      integer idebug,ivoronoi,nconsurf,ninter
      real*8 xdum101,xdum102,xdum103,pie
      common /global_lg/ xdum101,xdum102,xdum103,pie,
     *   idebug,ivoronoi,nconsurf,ninter
 
