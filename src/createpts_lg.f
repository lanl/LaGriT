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
C     Oct 2026  FIX 1: createpts with no current mesh object (no
C       cmo / create before it) wrote points into arrays that were
C       never allocated: segmentation fault. It now stops with an
C       ERROR before calling any option except poisson_disk (which
C       makes its own mesh object). Shown by the test_createpts
C       suite, deck errors_no_mesh_object.
C
C     Oct 2026  FIX 2: input checking (test_createpts decks, syntax
C       and errors groups). createpts read past the end of the
C       command when tokens were missing and silently replaced bad
C       counts. It now stops with an ERROR that says what is wrong:
C       no option; an unknown option; xyz / rtz / rtp / line with
C       fewer than 14 tokens, or with ratio zoning not given as 3
C       switches and 3 ratios; a count that is not a number or is 0
C       or negative (negative counts were documented in rz.f as a
C       staggered pattern, unreachable since Jan 2000); a line of
C       fewer than 2 points; brick with too few tokens, a count of
C       0 or less, a single row not along x (ni,1,1), or a mesh
C       object of another cell type (brick makes hexes, quads or
C       lines: the mesh object must be hex, quad, line or hybrid).
C       Angles that cover the circle (rtz theta over 360 deg) or the
C       sphere (rtp: theta or phi over 360, or both over 180) more than
C       once give a WARNING: the points can repeat. (Oct 15: the first
C       version warned for rtp theta over 180 alone, which also caught
C       the valid theta 0-360 / phi 0-180 sphere of the LANL demo.)
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
      integer lenopt2, ierror,icharlnf
      character*132 logmess
c     FIX 1: the current mesh object
      character*32 cmo
c
      ierr=0
C
C  FIX 2: nothing after createpts
C
      if(nwds.lt.2) then
         write(logmess,'(a)') 'ERROR: createpts: no option given'
         call writloga('default',0,logmess,0,ierror)
         call createpts_options()
         ierr=1
         return
      endif
C
C  FIX 1: every option but poisson_disk writes into the current mesh
C  object; with none (no cmo / create yet) stop here instead of
C  writing into arrays that were never allocated. poisson_disk makes
C  its own output mesh object and needs no current one.
C
      call cmo_get_name(cmo,ierror)
      if(cmsgin(2)(1:12).ne.'poisson_disk' .and.
     *   (ierror.ne.0 .or. cmo(1:8).eq.'-notset-')) then
         write(logmess,'(a)')
     *      'ERROR: createpts: there is no current mesh object'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)')
     *      'ERROR: create one first, e.g. cmo / create / mo / / / tet'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
C
C  test for verb and call appropriate routine
c
      lenopt2=icharlnf(cmsgin(2))
      if(cmsgin(2)(1:lenopt2).eq.'xyz'.or.
     *   cmsgin(2)(1:lenopt2).eq.'rtz'.or.
     *   cmsgin(2)(1:lenopt2).eq.'line'.or.
     *   cmsgin(2)(1:lenopt2).eq.'rtp') then
         call createpts_check_rz(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
         if(ierr.ne.0) return
         call rz(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'sphere') then
         call rzs_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'diamond') then
         call rzs_lg
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      elseif(cmsgin(2)(1:lenopt2).eq.'brick'.or.
     *   cmsgin(2)(1:lenopt2).eq.'hex') then
         call createpts_check_brick(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *        cmo,ierr)
         if(ierr.ne.0) return
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
         call poisson_disk
     *        (imsgin(2),xmsgin(2),cmsgin(2),msgtype(2),nwds-1,ierr)
      else
         write(logmess,'(a,a)') 'ERROR: createpts: unknown option: ',
     *      cmsgin(2)(1:max(1,min(32,lenopt2)))
         call writloga('default',0,logmess,0,ierror)
         call createpts_options()
         ierr=1
         return
      endif
      call setsize()
      return
      end
C
C#######################################################################
C     FIX 2 (Oct 2026): the input checks
C#######################################################################
C
      subroutine createpts_options()
C     list the options (after an ERROR about the option itself)
      implicit none
      character*132 logmess
      integer ierror
      write(logmess,'(a)') 'ERROR: createpts options: xyz, rtz, rtp,'//
     *   ' line, brick, sphere, diamond,'
      call writloga('default',0,logmess,0,ierror)
      write(logmess,'(a)') 'ERROR:   amr, vector, random, voronoi,'//
     *   ' median, interp, poisson_disk'
      call writloga('default',0,logmess,0,ierror)
      return
      end
C
      subroutine createpts_count(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *   i,copt,ival,ierr)
C     the count in token i of createpts / copt: a number of 1 or more
      implicit none
      integer nwds,i,ival,ierr, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds), copt
      character*132 logmess
      integer ierror,icharlnf
      ierr=0
      if(msgtype(i).eq.1) then
         ival=imsgin(i)
      elseif(msgtype(i).eq.2) then
         ival=nint(xmsgin(i))
      else
         write(logmess,'(a,a,a,i0,a,a)') 'ERROR: createpts / ',
     *      copt(1:icharlnf(copt)),': token ',i,
     *      ' must be a count, not ',
     *      cmsgin(i)(1:max(1,min(32,icharlnf(cmsgin(i)))))
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      if(ival.lt.1) then
         write(logmess,'(a,a,a,i0,a,i0,a)') 'ERROR: createpts / ',
     *      copt(1:icharlnf(copt)),': a count is ',ival,' (token ',i,
     *      '): it must be 1 or more'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
      endif
      return
      end
C
      real*8 function createpts_real(imsgin,xmsgin,msgtype,nwds,i)
C     token i as a real number
      implicit none
      integer nwds,i, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      if(msgtype(i).eq.1) then
         createpts_real=dble(imsgin(i))
      else
         createpts_real=xmsgin(i)
      endif
      return
      end
C
      subroutine createpts_check_rz(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *   ierr)
C     createpts / xyz | rtz | rtp | line
      implicit none
      integer nwds,ierr, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*132 logmess
      character*8 copt
      integer ierror,icharlnf,i,ival,ilast
      real*8 createpts_real, range, alim
      ierr=0
      copt=cmsgin(2)(1:min(8,icharlnf(cmsgin(2))))
      if(nwds.lt.14) then
         write(logmess,'(a,a,a,i0,a)') 'ERROR: createpts / ',
     *      copt(1:icharlnf(copt)),': incomplete command, ',nwds,
     *      ' tokens of 14:'
         call writloga('default',0,logmess,0,ierror)
         if(copt(1:4).eq.'line') then
            write(logmess,'(a)') 'ERROR:   createpts / line / n / / '//
     *         '/ x0,y0,z0 / x1,y1,z1 / iiz,ijz,ikz'
         else
            write(logmess,'(a,a,a)') 'ERROR:   createpts / ',
     *         copt(1:icharlnf(copt)),' / ni,nj,nk / min1,min2,min3'//
     *         ' / max1,max2,max3 /'
            call writloga('default',0,logmess,0,ierror)
            write(logmess,'(a)') 'ERROR:     iiz,ijz,ikz'//
     *         ' [/ irat,jrat,krat / rx,ry,rz]'
         endif
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      if(nwds.gt.14 .and. nwds.lt.20) then
         write(logmess,'(a,a,a)') 'ERROR: createpts / ',
     *      copt(1:icharlnf(copt)),': ratio zoning needs 3 switches'//
     *      ' and 3 ratios:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a,i0,a)') 'ERROR:   ... / iiz,ijz,ikz /'//
     *      ' irat,jrat,krat / rx,ry,rz (',nwds-14,' of 6 given)'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      ilast=5
      if(copt(1:4).eq.'line') ilast=3
      do i=3,ilast
         call createpts_count(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *      i,copt,ival,ierr)
         if(ierr.ne.0) return
         if(copt(1:4).eq.'line' .and. ival.lt.2) then
            write(logmess,'(a,i0,a)') 'ERROR: createpts / line: n is ',
     *         ival,': a line needs 2 or more points'
            call writloga('default',0,logmess,0,ierror)
            ierr=1
            return
         endif
      enddo
C     angle ranges (degrees): WARN when the angles cover the circle
C     (rtz) or the sphere (rtp) more than once. rtp covers the sphere
C     once with theta 0-180 and phi 0-360, or theta 0-360 and phi
C     0-180; only both over 180, or either over 360, covers it twice.
      if(copt(1:3).eq.'rtz') then
         range=abs(createpts_real(imsgin,xmsgin,msgtype,nwds,10)-
     *             createpts_real(imsgin,xmsgin,msgtype,nwds,7))
         if(range.gt.360.0d0) then
            write(logmess,'(a,f0.1,a)') 'WARNING: createpts / rtz:'//
     *         ' theta range ',range,' deg is over 360: points can '//
     *         'repeat'
            call writloga('default',0,logmess,0,ierror)
         endif
      endif
      if(copt(1:3).eq.'rtp') then
         range=abs(createpts_real(imsgin,xmsgin,msgtype,nwds,10)-
     *             createpts_real(imsgin,xmsgin,msgtype,nwds,7))
         alim=abs(createpts_real(imsgin,xmsgin,msgtype,nwds,11)-
     *            createpts_real(imsgin,xmsgin,msgtype,nwds,8))
         if(range.gt.360.0d0 .or. alim.gt.360.0d0 .or.
     *      (range.gt.180.0d0 .and. alim.gt.180.0d0)) then
            write(logmess,'(a,f0.1,a,f0.1,a)') 'WARNING: createpts / '//
     *         'rtp: theta range ',range,' and phi range ',alim,' deg'
            call writloga('default',0,logmess,0,ierror)
            write(logmess,'(a)') 'WARNING:   cover the sphere more'//
     *         ' than once: points can repeat'
            call writloga('default',0,logmess,0,ierror)
         endif
      endif
      return
      end
C
      subroutine createpts_check_brick(imsgin,xmsgin,cmsgin,msgtype,
     *   nwds,cmo,ierr)
C     createpts / brick | hex / xyz | rtz | rtp / ni,nj,nk / ...
      implicit none
      integer nwds,ierr, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds), cmo
      character*132 logmess
      character*32 mtype, cneed
      integer ierror,icharlnf,i,n(3),imtype,ln
      logical connect
      ierr=0
      if(nwds.lt.6) then
         write(logmess,'(a,i0,a)') 'ERROR: createpts / brick: '//
     *      'incomplete command, ',nwds,' tokens:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)') 'ERROR:   createpts / brick / xyz /'//
     *      ' ni,nj,nk / min1,min2,min3 / max1,max2,max3'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)') 'ERROR:     [/ iiz,ijz,ikz ...]  or'//
     *      '  ... / ni,nj,nk / i1,i2,i3 / connect'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      if(msgtype(3).ne.3 .or. (cmsgin(3)(1:3).ne.'xyz' .and.
     *   cmsgin(3)(1:3).ne.'rtz' .and. cmsgin(3)(1:3).ne.'rtp')) then
         write(logmess,'(a,a,a)') 'ERROR: createpts / brick: ',
     *      cmsgin(3)(1:max(1,min(32,icharlnf(cmsgin(3))))),
     *      ' is not a geometry: use xyz, rtz or rtp'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      do i=1,3
         call createpts_count(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *      i+3,'brick',n(i),ierr)
         if(ierr.ne.0) return
      enddo
      connect=.false.
      if(nwds.ge.10) connect=cmsgin(10)(1:7).eq.'connect'
      if(.not.connect .and. nwds.lt.12) then
         write(logmess,'(a,i0,a)') 'ERROR: createpts / brick: '//
     *      'incomplete command, ',nwds,' tokens of 12:'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)') 'ERROR:   createpts / brick / xyz /'//
     *      ' ni,nj,nk / min1,min2,min3 / max1,max2,max3'
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a)') 'ERROR:     [/ iiz,ijz,ikz ...]  or'//
     *      '  ... / ni,nj,nk / i1,i2,i3 / connect'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
C     the cells it makes, and the mesh object they must go in
      if(n(1).gt.1 .and. n(2).gt.1 .and. n(3).gt.1) then
         cneed='hex'
      elseif(n(2).eq.1 .and. n(3).eq.1 .and. n(1).gt.1) then
         cneed='line'
      elseif((n(1).eq.1 .and. n(2).gt.1 .and. n(3).gt.1) .or.
     *       (n(2).eq.1 .and. n(1).gt.1 .and. n(3).gt.1) .or.
     *       (n(3).eq.1 .and. n(1).gt.1 .and. n(2).gt.1)) then
         cneed='quad'
      else
         write(logmess,'(a,i0,a,i0,a,i0,a)') 'ERROR: createpts / '//
     *      'brick / ',n(1),',',n(2),',',n(3),
     *      ': one row of cells must run in x'
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      call cmo_get_mesh_type(cmo,mtype,imtype,ierror)
      ln=icharlnf(cneed)
      if(mtype(1:3).ne.'hyb' .and. mtype(1:ln).ne.cneed(1:ln)) then
         write(logmess,'(a,a,a,a)') 'ERROR: createpts / brick makes ',
     *      cneed(1:ln),' cells: the mesh object must be ',cneed(1:ln)
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a,a,a,a)') 'ERROR:   mesh object ',
     *      cmo(1:icharlnf(cmo)),' is ',mtype(1:icharlnf(mtype))
         call writloga('default',0,logmess,0,ierror)
         write(logmess,'(a,a,a,a)') 'ERROR:   create it with cmo / '//
     *      'create / ',cmo(1:icharlnf(cmo)),' / / / ',cneed(1:ln)
         call writloga('default',0,logmess,0,ierror)
         ierr=1
         return
      endif
      return
      end
