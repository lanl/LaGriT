      subroutine eltlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                   ntets,xtetwd)
      implicit none
C
C
C
C#######################################################################
C
C      PURPOSE -
C
C         CHECK AND INTERPETS THE ESET LIMITS THAT ARE INPUT.
C
C      INPUT ARGUMENTS -
C
C         ich1 - must be 'eset' or 'eltset'
C         ich2 - must be 'get'
C         ich3 - NAME
C         ipmpary - POINTER TO ALLOCATED MASS POINT LENGTH ARRAY
C         mpno - LENGTH OF ARRAY ALLOCATED
C
C
C      OUTPUT ARGUMENTS -
C
C         ipmpary - POINTER TO ARRAY OF ELEMENT INDICES
C         mpno - NO. OF MASS POINT INDICES IN THE ARRAY
C
C
C      CHANGE HISTORY -
C
C$Log: eltlimc.f,v $
CRevision 2.00  2007/11/05 19:45:53  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 13:34:20 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   Thu Feb 03 08:48:54 2000   dcg
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 12:10:18   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.3   Tue Nov 18 12:29:32 1997   dcg
CPVCS    accept eltset or eset as argument
CPVCS
CPVCS       Rev 1.2   Fri Oct 31 10:46:56 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:44:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 14:24:42 1996   dcg
CPVCS    Initial revision.
C#######################################################################
C
C
C
      pointer (ipmpary, mpary(10000000))
      integer mpary,ntets,nptsmax,i,num,ibitpos,ierreset,mpno
      integer xtetwd(ntets)
C
C
      character*(*) ich1,ich2,ich3
      character*32 name
 
C
C     ******************************************************************
C     INITIALIZE
C
      nptsmax=ntets
      do 10 i=1,ntets
         mpary(i)=0
   10 continue
C
C     ******************************************************************
C     HANDLE PSET GROUP
C
      if (ich1(1:4) .eq. 'eset' .or. ich1(1:6).eq. 'eltset' ) then
         num=nptsmax
         name=ich3
         call geteset(name,ipmpary,ibitpos,num,ierreset,
     *                ntets,xtetwd)
         mpno=num
         go to 9999
      endif
9999  return
      end
