      subroutine pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                   npoints,isetwd,itp1)
      implicit none
C
      character*132 logmess
C
      integer isetwd(*), itp1(*),mpno,npoints
C
C#######################################################################
C
C      PURPOSE -
C
C         CHECK AND INTERPETS THE POINT LIMITS THAT ARE INPUT.
C
C      INPUT ARGUMENTS -
C
C         ich1 - FIRST POINT OR PSET OR PSTATUS
C         ich2 - END POINT OR GET
C         ich3 - STRIDE OR NAME
C         ipmpary - POINTER TO ALLOCATED MASS POINT LENGTH ARRAY
C         mpno - LENGTH OF ARRAY ALLOCATED
C
C
C      OUTPUT ARGUMENTS -
C
C         ipmpary - POINTER TO ARRAY OF MASS POINT INDICES
C         mpno - NO. OF MASS POINT INDICES IN THE ARRAY
C
C
C      CHANGE HISTORY -
C
C        $Log: pntlimc.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   30 Aug 2007 10:01:12   tam
CPVCS    format 9100 had a misplaced comma
CPVCS    
CPVCS       Rev 1.1   Thu Feb 03 08:55:18 2000   dcg
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 16:21:04   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.12   Thu Jun 10 11:44:36 1999   dcg
CPVCS    change default behavior to return empty set if can't understand request
CPVCS
CPVCS       Rev 1.11   Thu Jun 10 11:24:44 1999   dcg
CPVCS    fix missing error flag in call to writloga
CPVCS
CPVCS       Rev 1.10   Wed Jan 20 12:09:10 1999   dcg
CPVCS    check for illegal pset designation
CPVCS    check for empty string
CPVCS
CPVCS       Rev 1.9   Tue Jan 19 14:21:44 1999   dcg
CPVCS    check for psetname of -def-
CPVCS
CPVCS       Rev 1.8   Fri Oct 31 10:49:20 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:56:40 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   09/14/95 02:06:52   het
CPVCS    Fix ipcmoprm errors
CPVCS
CPVCS       Rev 1.5   08/29/95 11:52:06   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.4   06/07/95 15:31:02   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.3   05/01/95 08:33:52   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.2   03/31/95 09:09:16   het
CPVCS    Add the buildid calles before all storage block calls
CPVCS
CPVCS       Rev 1.1   03/30/95 04:59:56   het
CPVCS    Change the storage block id packing and preidsb to buildid for long names
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:04   pvcs
CPVCS    Original version.
C
C#######################################################################
C
C
      pointer (ipmpary, mpary(1))
      integer mpary
C
      character*(*) ich1,ich2,ich3
      character*32 name
      integer nptsmax,ifirst,ilast,istride,i,num,ibitpos,ierrpset,
     *  ipt1,ipt2,ipt3,ierrdum
C
C#######################################################################
C     INITIALIZE
C
      mpno=0
      nptsmax=npoints
      ifirst=1
      ilast=nptsmax
      istride=1
      do 10 i=1,npoints
         mpary(i)=0
   10 continue
C
C     ******************************************************************
C     HANDLE PSET GROUP
C
      if (ich1(1:4) .eq. 'pset') then
         num=nptsmax
         name=ich3
         call getpset(name,ipmpary,ibitpos,num,ierrpset,
     *                npoints,isetwd,itp1)
         mpno=num
         go to 9999
C
C     ******************************************************************
C     HANDLE PSTATUS GROUP
C
      elseif(ich1(1:7).eq.'pstatus') then
         write(logmess,9000) ipt1,ipt2,ipt3
         call writloga('default',1,logmess,1,ierrdum)
 9000    format('Pstatus not supported: ',3(1x,a8))
         num=nptsmax
         name=ich3
         call getpset(name,ipmpary,ibitpos,num,ierrpset,
     *                npoints,isetwd,itp1)
         mpno=num
         go to 9999
      elseif(ich1(1:5).eq.'-def-'.or.ich1(1:1).eq.' ') then
         ipt1=1
         ipt2=0
         ipt3=0
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                   npoints,isetwd,itp1)
      else
         write(logmess,9100) ich1
 9100    format('illegal pset designation ',a,' returns empty pset')
         call writloga('default',0,logmess,0,ierrdum)
      endif
c
9999  return
      end
