      subroutine createpts_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C#######################################################################
C
C     PURPOSE -
C
C     call the appropriate rz routine to create and distribute nodes
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C       ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C   $Log: createpts_lg.f,v $
C   Revision 2.00  2007/11/05 19:45:51  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   07 May 2003 14:36:20   gable
CPVCS    Added option createpts / interp
CPVCS    
CPVCS       Rev 1.6   01 Nov 2002 13:04:38   gable
CPVCS    Added median point option.
CPVCS    
CPVCS       Rev 1.5   04 Dec 2000 17:10:56   dcg
CPVCS    allow calling the diamond routine from createpts
CPVCS
CPVCS       Rev 1.4   02 Nov 2000 10:07:46   dcg
CPVCS    add voronoi points option
CPVCS
CPVCS       Rev 1.3   Tue Mar 21 15:47:42 2000   dcg
CPVCS    add call to setsize
CPVCS
CPVCS       Rev 1.2   Mon Jan 31 11:24:40 2000   dcg
CPVCS    add line option
CPVCS
CPVCS       Rev 1.1   Fri Dec 03 10:29:32 1999   dcg
CPVCS    pass nwds-1 to called routines
CPVCS
CPVCS       Rev 1.0   Fri Jul 23 09:13:12 1999   dcg
CPVCS    replace rz commands with createpts
C
C#######################################################################
      implicit none
      integer nwds,ierr, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer lenopt2, lenopt3, ierror,icharlnf
      character*132 logmess
c
      ierr=0
C
C  test for verb and call appropriate routine
c
      lenopt2=icharlnf(cmsgin(2))
      lenopt2=icharlnf(cmsgin(3))
      if(cmsgin(2)(1:lenopt2).eq.'xyz'.or.
     *   cmsgin(2)(1:lenopt2).eq.'rtz'.or.
     *   cmsgin(2)(1:lenopt2).eq.'line'.or.
     *   cmsgin(2)(1:lenopt2).eq.'rtp') then
         call rz(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'sphere') then
         call rzs_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'diamond') then
         call rzs_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'brick'.or.
     *   cmsgin(2)(1:lenopt2).eq.'hex') then
         call rzbrick_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'amr') then
         call rzamr_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'vector') then
         call rzv_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'random'.or.
     *       cmsgin(2)(1:lenopt2).eq.'ran') then
         call ranpts_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'voronoi'.or.
     *       cmsgin(2)(1:3).eq.'vor') then
         call vorpts_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'median'.or.
     *       cmsgin(2)(1:3).eq.'vor') then
         call medianpts_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'interp') then
         call rzinterp
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'poisson_disk') then
         if(cmsgin(3)(1:lenopt3).eq.'2d_polygon')then
         call poisson_disk_2d
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
         elseif(cmsgin(3)(1:lenopt3).eq.'3d_box')then
         call poisson_disk_3d
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
         endif
      else
         write(logmess,"('Illegal createpts option ',a10)") cmsgin(2)
         call writloga('default',0,logmess,0,ierror)
         ierr=1
      endif
      call setsize()
      return
      end
