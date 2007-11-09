C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
 
C    subroutines setsize, getsize, set_mbndry
 
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
 
      subroutine setsize()
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE SETS XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX
C        DEPENDING ON THE PROBLEM SIZE BY LOOPING THROUGH THE
C        REAL POINTS
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/setsize_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.18   01 Oct 2007 08:18:24   gable
CPVCS    Modified to give warning and continue instead of crash when the MO
CPVCS    does not exist or is empty.
CPVCS    
CPVCS       Rev 1.17   08 Feb 2006 14:38:12   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.16   01 Nov 2005 07:52:20   gable

CPVCS    Fixed syntax of format statement 110.

CPVCS    

CPVCS       Rev 1.15   03 May 2001 15:21:08   kuprat

CPVCS    Changed EPSILONV calculation to consider a point set to 

CPVCS    be '2-D' if one of the dimensions was less than EPSILONL in extent.

CPVCS    

CPVCS       Rev 1.14   28 Jul 2000 08:35:26   dcg

CPVCS    avoid divide by zero

CPVCS    

CPVCS       Rev 1.13   24 Jul 2000 09:52:00   dcg

CPVCS    fix error - test mbndry_old not mbndry

CPVCS    

CPVCS       Rev 1.12   25 May 2000 14:29:12   nnc

CPVCS    Fixed call to INT with real argument.

CPVCS    

CPVCS       Rev 1.11   03 May 2000 12:53:20   dcg

CPVCS    declare jtet to be an integer 

CPVCS    

CPVCS       Rev 1.10   24 Apr 2000 11:29:18   jtg

CPVCS    set_mbndry modified so it never decreases mbndry

CPVCS
CPVCS       Rev 1.9   21 Apr 2000 08:58:26   gable
CPVCS    SGI's problem, not mine.
CPVCS
CPVCS       Rev 1.8   21 Apr 2000 07:34:32   gable
CPVCS    Add description of set_mbndry in header of subroutine.
CPVCS
CPVCS       Rev 1.7   21 Apr 2000 07:10:14   gable
CPVCS    Added set_mbndry routine for dynamic setting and checking of mbndry value.
CPVCS
CPVCS       Rev 1.6   Wed Mar 22 12:49:28 2000   dcg
CPVCS    if epsilonv = 0 use epsilona
CPVCS
CPVCS       Rev 1.5   Wed Mar 22 08:51:42 2000   dcg
CPVCS    move call to set_epsilon to the correct routine
CPVCS
CPVCS       Rev 1.4   Tue Mar 21 15:47:34 2000   dcg
CPVCS    add call to set_epsilon
CPVCS
CPVCS       Rev 1.3   03 Feb 2000 12:40:36   dcg
CPVCS
CPVCS       Rev 1.2   25 Jan 2000 15:42:46   dcg
CPVCS
CPVCS       Rev 1.2   Mon Sep 08 15:33:10 1997   kuprat
CPVCS    Changed epsilona, epsilonv so that they use a safer
CPVCS    multiple (1000 vs 10) of the machine epsilon.
CPVCS
CPVCS       Rev 1.1   Wed Aug 20 12:21:46 1997   dcg
CPVCS    set epsilona and epsilonv
C
C#######################################################################
C
      implicit none
      include 'consts.h'
C#######################################################################
C
      integer npoints
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitp1,itp1)
      integer itp1(1000000)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C#######################################################################
C
      integer ilen, itype, ierr,  length, i,idata
      REAL*8 xmin1, xmax1, ymin1, ymax1, zmin1, zmax1, big,
     *   epsilonv,epsilona,epsilonl
C
      pointer (ipireal1 , ireal1  )
      integer ireal1(1000000)
C
      integer icscode
C
      character*32 isubname, cmonam, cmo,cdata
      character*132 logmess
C
C
C#######################################################################
C
C
C     ******************************************************************
C
      isubname='setsize'
C
C     ******************************************************************
C     Get mesh object name
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      if(npoints .le. 0)then
        write(logmess,'(a)')'WARNING: MO has zero nodes'
        call writloga('default',0,logmess,0,ierr)
        write(logmess,'(a)')'WARNING: NO ACTION'
        call writloga('default',0,logmess,0,ierr)
        write(logmess,'(a)')'setsize: RETURN'
        return
      endif
C      
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
C     GET REAL POINTS
C          IREAL1 = 1  -> Real Node.
C          IREAL1 = 0  -> Not a real node.
      length=npoints
      call mmgetblk('ireal1',isubname,ipireal1,length,2,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'mmgetblk')
C
      call unpacktp('allreal','set',length,ipitp1,ipireal1,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'unpacktp')
C     SET THE SEARCH RANGE TO SMALLEST X, Y, OR Z RANGE
C
      big=1/epsilon
      xmin1=big
      xmax1=-big
      ymin1=big
      ymax1=-big
      zmin1=big
      zmax1=-big
      do i=1,npoints
         if(ireal1(i).eq.1) then
            if (xic(i).lt.xmin1) xmin1=xic(i)
            if (yic(i).lt.ymin1) ymin1=yic(i)
            if (zic(i).lt.zmin1) zmin1=zic(i)
            if (xic(i).gt.xmax1) xmax1=xic(i)
            if (yic(i).gt.ymax1) ymax1=yic(i)
            if (zic(i).gt.zmax1) zmax1=zic(i)
         endif
      enddo
C
C     ******************************************************************
C     *** GET THE NAME OF THE CMO TO BE THE PREFIX OF THE STORAGE BLOCK
C            ID NAME.
      call cmo_get_name(cmonam,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_name')
C
C     PUT THE VALUE OF mins and maxs IN mesh object
C
      call cmo_set_attinfo('xmin',cmonam,idata,xmin1,
     *    cdata,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_attinfo('xmax',cmonam,idata,xmax1,cdata,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_attinfo('ymin',cmonam,idata,ymin1,cdata,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_attinfo('ymax',cmonam,idata,ymax1,cdata,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_attinfo('zmin',cmonam,idata,zmin1,cdata,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_attinfo('zmax',cmonam,idata,zmax1,cdata,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')

c
c  ALSO STORE EPSILONS IN MESH OBJECT
c

c.... set and get epsilonl

      call set_epsilon()
      call get_epsilon('epsilonl', epsilonl)

c.... Calculate EPSILONA.

      epsilona=((xmax1-xmin1)**2+(ymax1-ymin1)**2+
     *   (zmax1-zmin1)**2)*epsilonr*1000.
      call cmo_set_attinfo('epsilona',cmonam,idata,epsilona,cdata,2,
     *   icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')

c.... Calculate EPSILONV.  Use expression appropriate for 3D node
c.... distribution, unless one of the linear dimensions is smaller
c.... than EPSILONL.  In this case assume a 2D node distribution
c.... and use EPSILONA.

      if ((abs(xmax1-xmin1).le.epsilonl).or.(abs(ymax1-ymin1).le
     &   .epsilonl).or.(abs(zmax1-zmin1).le.epsilonl)) then
         epsilonv=epsilona
      else
         epsilonv=abs(xmax1-xmin1)*abs(ymax1-ymin1)*abs(zmax1-zmin1)
     &      *epsilonr*1000.
      endif

      call cmo_set_attinfo('epsilonv',cmonam,idata,epsilonv,cdata,2,
     *  icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')

      call mmrelprt(isubname,ierr)
      return
      end
 
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
 
      subroutine getsize(xn,xx,yn,yx,zn,zx,epa,epv)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE GETS THE SEARCH RANGE TO BE USED IN DETERMINING
C        POINT TYPES, MATERIAL TYPES, AND INTERFACE LOCATIONS.
C
C     INPUT ARGUMENTS -
C
C        none
C
C     OUTPUT ARGUMENTS -
C
C        xn,xx,yn,yx,zn,zx mins and maxs of problem size
C        epd, epa, epv length, area and volume epsilons
C
C     CHANGE HISTORY -
C
C        $Log $
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      real*8 xn,xx,yn,yx,zn,zx,epa,epv
C#######################################################################
C
      integer icscode,ilen,itype,iout
      pointer(ipout,out)
      real*8 out(*)
C
      character*32 isubname, cmonam, cout
C
      isubname='getsize'
C
C     ******************************************************************
C     GET THE VALUES FROM THE mesh object
C     *** GET THE NAME OF THE CMO TO BE THE PREFIX OF THE STORAGE BLOCK
C            ID NAME.
      call cmo_get_name(cmonam,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_name')
C
 
      call cmo_get_attinfo('xmin',cmonam,iout,xn,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('xmax',cmonam,iout,xx,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('ymin',cmonam,iout,yn,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('ymax',cmonam,iout,yx,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('zmin',cmonam,iout,zn,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('zmax',cmonam,iout,zx,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('epsilona',cmonam,iout,epa,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_attinfo('epsilonv',cmonam,iout,epv,cout,
     *                        ipout,ilen,itype,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'cmo_get_info')
      return
      end
 
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
 
      subroutine set_mbndry()
C
C
C#######################################################################
C
C     PURPOSE -
C
C        SET THE MBNDRY VALUE DEPENDING ON NUMBER OF ELEMENTS IN CMO
C        To insure that the old value of 16000000 convention stays
C        around, mbndry will be set to 16000000 when nelements*nef
C        is less than 16000000. For larger problems mbndry will be
C        set to a round multiple of 1000000 which is big enough for
C        the problem and leaves some room to get bigger. If the CMO
C        does not have elements, the test value is 30*nnodes instead
C        of nelements*nef.
C
C        There has been some discussion of changing mbndry to zero.
C        and some of the lower_d routines already use that. If mbndry
C        is zero upon entering this routine it will not be modified.
C
C        Also, since mbndry impacts the values in the jtet array, if
C        mbndry is changed within a call to this routine, the jtet
C        array will be updated.
C
C        From here on, standard usage should be to call set_mbndry()
C        to set mbndry within the active CMO. When mbndry is needed,
C        it should be obtained via a cmo_get_info('mbndry'... call.
C
C     INPUT ARGUMENTS -
C
C        none
C
C     OUTPUT ARGUMENTS -
C
C        none
C
C#######################################################################
C
      implicit none
      include 'machine.h'
C#######################################################################
C
      integer ilen, itype, ierr
      integer nnodes, nelements, nef, mbndry
C
      integer i, ifac, int_one
      integer mbndry_convention
      integer mbndry_default, mbndry_old, mbndry_test, isize_test
      integer int_big,icompare
      real*8  extra
C
      character*32 isubname, cmo
      character*80 logmess
C
      pointer(ipjtet , jtet)
      integer  jtet(10000000)
C
C#######################################################################
C
C
C     ******************************************************************
C
      isubname='set_mbndry'
C
C     ******************************************************************
      ifac = 1000000
      extra = 1.2
      mbndry_default = 16000000
      int_one = 1
C
C     Get mesh object name and some values associated with mesh object
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_intinfo('nelements',cmo,nelements,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_intinfo')
C
      call cmo_get_intinfo('nnodes',cmo,nnodes,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_intinfo')
C
      call cmo_get_intinfo('faces_per_element',cmo,nef,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_intinfo')
C
      call cmo_get_intinfo('mbndry',cmo,mbndry_old,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_intinfo')
 
      if((mbndry_old .eq. 0) .or. (ierr .ne. 0))then
C
C     If mbndry_convention = 0, this is done. Do no more. RETURN
C
         goto 9999
      else
         mbndry_convention = 1
      endif
C
C     Set mbndry to either mbndry_default or if nelements*nef is greater
C     than mbndry_default than set it to a multiple of one million that
C     is larger. Also leave some headroom (factor of 1.2) for the cmo
C     to grow without having to change mbndry.
C     However, do not DECREASE mbndry if input value
C     larger than required
C
C     If there are only nodes, use the 30*nnodes guess/rule.
C
      if(nelements .ne. 0)then
         mbndry_test = ifac*int(1.2*float(nelements*nef)/ifac)
      else
         mbndry_test = ifac*int(1.2*float(30*nnodes)/ifac)
      endif
      mbndry = max(mbndry_default, mbndry_test)
      mbndry = max(mbndry, mbndry_old)
 
C
C     Check if this is a 32 bit integer or 64 bit integer version of the
C     code. If it is a 32 bit version, the max value of mbndry is half 2**31
C     2**31 = 2,147,483,647      0.5*(2**31) = 1,073,741,823
 
      int_big = 1000000000
 
      if(nef .ne. 0)then
         isize_test = nelements/nef
         icompare=int_big/nef
      else
         isize_test = nnodes
         icompare=int_big/8
      endif
 
      if((isize_test .gt. icompare) .and. (NBYTES_INT .eq. 4))then
         write(logmess,100)
  100    format('ERROR: Subroutine set_mbndry')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,110) nelements, mbndry, NBYTES_INT
  110    format('nelements = ',i20,' mbndry = ',i20,' nbytes_int = ',i3)
         call writloga('default',0,logmess,0,ierr)
         write(logmess,120)
  120    format('Error: cannot have mbndry .gt. 0.5*(2**31)')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,130)
  130    format('ERROR: Problem size too big for 32 bit version')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,140)
  140    format('ERROR: Problem size requires 64 bit version')
         call writloga('default',0,logmess,0,ierr)
         write(logmess,150)
  150    format('ERROR: STOP')
         call writloga('default',0,logmess,0,ierr)
         stop
      endif
 
C
C     Do nothing if mbndry=mbndry_old
C
      if (mbndry_old.ge.mbndry) goto 9999
 
C
C     Stick the new mbndry value into the mesh object
C
      call cmo_set_info('mbndry',cmo,mbndry,int_one,int_one,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_set_info')
 
C
C     Since this may change the value of mbndry, we need to be sure that
C     if there are jtet array values set to mbndry_old, they are changed
C     to the new value of mbndry.
C
      if(mbndry_old .ne. mbndry)then
 
        call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ierr)
        if(ilen .ge. 1)then
          do i = 1,ilen
            if(jtet(i) .ge. mbndry_old)
     *         jtet(i) = jtet(i) - mbndry_old + mbndry
          enddo
        endif
 
      endif
 
 9999 continue
      return
      end
 
C#######################################################################
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
C#######################################################################
