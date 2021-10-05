*dk,sethessian
      subroutine sethessian(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        SETHESSIAN creates an estimate of the Hessian (second
C           -derivative) matrix for each element and stores it in 
C           six NELEMENTS long arrays called HXX, HXY, HXZ, HYY, HYZ,
C           and HZZ.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: sethessian.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   07 Jan 2002 20:29:24   kuprat
CPVCS    Corrected bug where MMRELPRT called after no memory management.
CPVCS    
CPVCS       Rev 1.1   02 Jan 2002 17:52:42   kuprat
CPVCS    Commented out call to hess2d.
CPVCS    
CPVCS       Rev 1.0   21 Dec 2001 18:06:20   kuprat
CPVCS    Initial revision.
C
c#######################################################################
c
c   SETHESSIAN can be performed using a CMO field, function evaluations, or
c   edge errors:
c 
c     FORMAT: 
c 
c        SETHESSIAN / { field, USER, ERRADPT }
c           / ifirst,ilast,istride 
c
c     In the "field" form, the user has
c     specified a valid field from the current mesh 
c     object, and the second derivative matrix is to be based upon this field.
c
c     If USER is specified, the Hessian will be computed using 
c     calls to the subroutine FADPT which must be supplied by the user:
c
c     SUBROUTINE FADPT(X,Y,Z,MAT,NVEC,TIME,F)
c 
c     In this required format, the subroutine is supplied data for NVEC
c     nodes.  (The user may or may not use all of this information.) 
c     The information is the position and material type for each node,
c     supplied in the form of the arrays X, Y, Z, and MAT.
c     The simulation time TIME, is also supplied.  Given this data, 
c     what is required of the subroutine is that it 
c     return NVEC function values in the output array F. 
c
c     If ERRADPT is specified, the Hessian will be computed using calls
c     to the subroutine ERRADPT which must be supplied by the user:
c
c     SUBROUTINE ERRADPT(IELTARY,IELTNO,RANGE,ERRVEC)
c
c     In this required format, the subroutine is supplying errors 
c     associated with the 6 edges of each of the IELTNO tetrahedral elements
c     in the tet array IELTARY.  That is, the user returns an
c     array ERRVEC(6,IELTNO) of edge errors.  The ordering of the
c     six edges is such that the local endpoint nodes of the I'th
c     edge are IELMEDGE1(1,I,IFELMTET) and IELMEDGE1(2,I,IFELMTET).
c     [In other words, the order is (1,2), (1,3), (1,4), (2,3),
c     (2,4), (3,4).]  The user must also supply the scalar 'RANGE'
c     which gives an estimate of the maximum value minus
c     the minimum value of the function being represented on the grid.
c     RANGE may be required for some algorithms that use the Hessian.
c
c     ifirst,ilast,istride is currently ignored and need not be
c     specified.
c
c#######################################################################
C
      implicit none
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror,ierrw
      
      character*132 logmess
      pointer (ipisetwd,isetwd)
      pointer (ipitp1, itp1)
      pointer (ipreffield,reffield)
      integer isetwd(10000000)
      integer itp1(10000000)
      real*8 reffield(10000000)
C
      pointer (ipmpary,mpary)
      integer mpary(10000000)
C
      character*32 ich1,ich2,ich3,cmo,action,isubname,blkname
      character*32 algchoice,movechoice,climit1,climit2,climit3,
     &   cfield
      integer nsdtopo,length,icmotype,icscode,mpno,ipt1,ipt2,
     &   ipt3,nnodes
      real*8 ctrl
      integer icharlnf,lenaction
C
      isubname = 'sethessian'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'SETHESSIAN: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
      
C  Find out if it's a 2D or 3D mesh.
      call cmo_get_info('ndimensions_topo',cmo,
     *   nsdtopo,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')

c... If user specifies 'USER', Hessian is derived from FADPT.
c... If user specifies 'ERRADPT', Hessian is derived from ERRADPT.
c... Otherwise, the user must specify the name of 
c... a valid field to adapt to.  In this case, we
c... also check if the 'refresh' option is specified.

      if (msgtype(2).eq.3 .and. cmsgin(2)(1:5).ne.'-def-') then
         if (cmsgin(2)(1:4).eq.'user') then
            action='user'
         elseif (cmsgin(2)(1:7).eq.'erradpt') then
            action='erradpt'
         else
            cfield=cmsgin(2)(1:icharlnf(cmsgin(2)))
            call mmgetpr(cfield,cmo,ipreffield,icscode)
            if(icscode.ne.0) then
               ierror=icscode
               write(logmess,'(a)')
     *            'SETHESSIAN: bad reference field'
               call writloga('default',0,logmess,0,ierrw)
               goto 9999
            endif
            action='field'
         endif
      else
         ierror=icscode
         write(logmess,'(a)')
     *      'SETHESSIAN: bad reference field'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif

c... Determine which algorithm is to be used.

      if (nsdtopo.eq.2) then
         if (action.eq.'erradpt') then
            write(logmess,'(a)')
     *         'SETHESSIAN: can''t use ERRADPT in 2-D'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
      else
         if (msgtype(3).ne.3 .or. cmsgin(3)(1:5).eq.'-def-') then
            algchoice = 'mega'
         else
            algchoice = cmsgin(3)(1:icharlnf(cmsgin(3)))
         endif
      endif

c Print correct message and call correct routine, based on whether
c mesh is 2- or 3-dimensional.

      if (nsdtopo.eq.2) then
         
         if (action.eq.'erradpt') then
            write(logmess,'(a)')
     *         'SETHESSIAN: can''t use ERRADPT in 2-D'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif

c$$$$         call hess2d(cmo,action,cfield,ierror)
            write(logmess,'(a)')
     *         'SETHESSIAN: not set up for 2-D yet'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999

      else

         call hess3d(cmo,action,cfield,ierror)

      endif

 9999 continue
C
      return
      end
