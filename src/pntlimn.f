      subroutine pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                   npoints,isetwd,itp1)
      implicit none
      integer ipt1,ipt2,ipt3,mpno,npoints
      character*132 logmess
C
C
C#######################################################################
C
C      PURPOSE -
C
C         CHECK AND INTERPETS THE POINT LIMITS THAT ARE INPUT.
C
C      INPUT ARGUMENTS -
C
C         ipt1     - FIRST POINT
C         ipt2     - END POINT
C         ipt3     - STRIDE
C         ipmpary  - POINTER TO ALLOCATED MASS POINT LENGTH ARRAY
C         mpno     - LENGTH OF ARRAY ALLOCATED
C         npoints  - NUMBER OF POINTS TO LOOK AT.
C         isetwd() - THE PSET FITWORD.
C         itp1()   - THE POINT TYPE FLAG ARRAY.
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
C        $Log:   /pvcs.config/t3d/src/pntlimn_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 16:21:06   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:56:42 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   05/01/95 08:37:06   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS    
CPVCS       Rev 1.1   02/18/95 06:56:50   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:06   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include "chydro.h"
C
      pointer (ipmpary, mpary(*))
      integer isetwd(npoints), itp1(npoints)
      integer mpary,id1,id2,id3,ifirst,ilast,istride,i,icount,i1,
     *  nptsmax
C
C#######################################################################
C
C
C     ******************************************************************
C     INITIALIZE
C
      id1=ipt1
      id2=ipt2
      id3=ipt3
      nptsmax=npoints
      ifirst=1
      ilast=nptsmax
      istride=1
      do 10 i=1,npoints
         mpary(i)=0
   10 continue
C
C     ******************************************************************
C     RESET FIRST, LAST AND STRIDE
C
      if(ipt1.lt.0) ipt1=max0(0,ifirst-ipt1)
      if(ipt2.lt.0) ipt2=max0(0,ilast-ipt2)
      if(ipt3.lt.0) ipt3=max0(1,istride-ipt3)
C
      if(ipt1.eq.0) ipt1=ifirst
      if(ipt2.eq.0) ipt2=ilast
      if(ipt3.eq.0) ipt3=istride
C
      if (ipt1 .gt. max0(ilast,nptsmax)) ipt1=max0(ilast,nptsmax)
      if (ipt2 .lt. ipt1) ipt2=ipt1+ipt2
      if (ipt2 .gt. max0(ilast,nptsmax)) ipt2=max0(ilast,nptsmax)
C
C9020*format('PNTLIMIT:  id1',a8,'  id2=',a8,'  id3=',a8)
C9030*format('icount=',i7,'  start=',i7,'  end=',i7,
C
C
C     ******************************************************************
C     FILL MASS POINT INDEX ARRAY
C
      mpno=(ipt2-ipt1)/ipt3 + 1
      do 20 i=1,mpno
         mpary(i)=ipt1 + (i-1)*ipt3
   20 continue
      if (mpary(mpno) .gt. ipt2) mpno=mpno-1
C
C
C     ******************************************************************
C     COMPRESS OUT THE "INACTIVE" POINTS FROM THE LIST.
C
      if(mpno.gt.0) then
         icount=0
         do i=1,mpno
            i1=mpary(i)
            if(itp1(i1).lt.ifitpst3.or.itp1(i1).gt.ifitpen3) then
               icount=icount+1
               mpary(icount)=mpary(i)
            endif
         enddo
         mpno=icount
      endif
c
      return
      end
