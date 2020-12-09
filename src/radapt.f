*dk,radapt
      subroutine radapt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        RADAPT performs r-adaption (adaptive node movement) on
C        2D and 3D mesh objects.
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
C        $Log: radapt.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   04 Jun 2003 15:27:26   dcg
CPVCS    initialized ctrl to zero
CPVCS
CPVCS       Rev 1.3   30 Jul 2001 15:48:20   kuprat
CPVCS    Put in mega_exact option.
CPVCS
CPVCS       Rev 1.2   23 Dec 2000 17:27:36   kuprat
CPVCS    Fixed documentation.
CPVCS
CPVCS       Rev 1.1   21 Dec 2000 23:20:02   kuprat
CPVCS    Added ERRADPT option.
CPVCS
CPVCS       Rev 1.0   Tue Sep 02 23:15:34 1997   kuprat
CPVCS    Initial revision.
C
c#######################################################################
c
c   RADAPT performs "r-adaption" on 2-D or 3-D mesh objects.
c   r-adaption is the movement of nodes (without changing connectivity)
c   for a particular numerical computation.
c
c     FORMAT:
c
c        RADAPT / { POSITION | SET | ADD | SUB } /
c           { ESUG | MEGA } / ifirst,ilast,istride / field /
c           { STALE | REFRESH }
c
c        RADAPT / { POSITION | SET | ADD | SUB } /
c           { ESUG | MEGA } / ifirst,ilast,istride / USER
c
c        RADAPT / { POSITION | SET | ADD | SUB } /
c           MEGA / ifirst,ilast,istride / ERRADPT
c
c     Specifying "position" signifies that the x-y-z values of the
c     nodes in the current mesh object will be altered.
c     (The "set", "add", and "sub" options pertain to a node
c     velocity modification option that is not yet implemented.)
c
c     Specifying "esug" or "mega" chooses which algorithm will be
c     used for r-adaption: (Adaptive) Elliptic Smoothing for
c     Unstructured Grids or Minimum Error Gradient Adaption.
c     "esug" is the default in 2D.  "mega" is the default in 3D.
c
c     References:  ESUG: Ahmed Khamayseh and Andrew Kuprat, "Anisotropic
c     Smoothing and Solution Adaption for Unstructured Grids",
c     Int. J. Num. Meth. Eng., Vol. 39, pp. 3163-3174 (1996).
c     MEGA: Randolph E. Bank and R. Kent Smith, "Mesh Smoothing Using A
c     Posteriori Error Estimates", SIAM J. Num. Anal. Vol. 34, Issue 3,
c     pp. 979-997 (1997).
c
c     ifirst,ilast,istride define the set of nodes in the cmo which
c     will be affected.
c
c     In the first form above, "field" denotes that the user has
c     specified a valid field from the current mesh
c     object, and  r-adaption is to be based upon this field.
c     Typically, if the field has large gradients or curvature in
c     a particular region, r-adaption using this field will cause
c     nodes to be attracted to the region of interest.  (ESUG adapts
c     especially to large gradients.  MEGA adapts especially to
c     large second derivatives---"curvature".)  The final argument of
c     the first form---"stale" or "refresh"---signifies if the user
c     desires the cmo field to be refreshed through interpolation during
c     the course of r-adaption.  If "stale" is specified (the default),
c     the field values will not be altered even though the node positions are
c     moved, and hence the field values become "stale".  In this case,
c     the user should reduce the number of adaption iterations to
c     e.g. 1 to 4, since r-adaption with stale data becomes meaningless
c     after a while.  (See MAXITER_SM variable description below.)  In
c     this case the user takes on the task of refreshing the field
c     values by e.g. re-solving a PDE for the new field values on the
c     new mesh.  If "refresh" is specified, the r-adaption routine will
c     automatically interpolate the new field values every iteration,
c     using a call to the DOPING command.  In this case, the number
c     of adaption iterations need not be reduced from the default value
c     of 25.
c
c     If the second form above, with the final argument being "user",
c     the mesh will r-adapt to the function returned by the subroutine
c     FADPT which must be supplied by the user:
c
c     SUBROUTINE FADPT(X,Y,Z,MAT,NVEC,TIME,F)
c
c     In this required format, the subroutine is supplied data for NVEC
c     nodes.  (The user may or may not use all of this information.)
c     The information is the position and material type for each node,
c     supplied in the form of the arrays X, Y, Z, and MAT.
c     The simulation time TIME, is also supplied.  Given this data,
c     what is required of the subroutine is that it
c     return NVEC function values in the output array F.  The NVEC
c     pieces of information returned to RADAPT will be used to adapt the
c     mesh.  Again, those regions where the function defined by
c     FADPT has large gradients or curvature will be where the
c     mesh will accumulate.  For this "user" option, function values
c     do not become "stale" because we have the ability to evaluate function
c     values at the new node positions.
c
c     If the third form above is specified, with the final argument
c     being 'ERRADPT', the mesh will r-adapt to the edge errors returned
c     by the subroutine ERRADPT which must be supplied by the user:
c
c     SUBROUTINE ERRADPT(IELTARY,IELTNO,RANGE,ERRVEC)
c
c     In this required format, the subroutine is supplying errors
c     associated with the 6 edges of each the IELTNO tetrahedral elements
c     in the tet array IELTARY.  That is, the user returns an
c     array ERRVEC(6,IELTNO) of edge errors.  The ordering of the
c     six edges is such that the local endpoint nodes of the I'th
c     edge are IELMEDGE1(1,I,IFELMTET) and IELMEDGE1(2,I,IFELMTET).
c     [In other words, the order is (1,2), (1,3), (1,4), (2,3),
c     (2,4), (3,4).]  The user must also supply the scalar 'RANGE'
c     which gives an estimate of the maximum value minus
c     the minimum value of the function being represented on the grid.
c     RANGE need only be a rough estimate, and it's used for setting
c     the magnitude of the regularization term.
 
c     If ESUG is used (currently available in 2D only), the degree of
c     of node adaption will depend on the scale of the specified field.
c     In this case, the results of adaption of the grid to the field can be
c     altered by using one or more FIELD commands beforehand to modify
c     the field.  For example, by increasing the
c     scale of a field using FIELD/SCALE, the ESUG algorithm
c     will produce grids with increased numbers of nodes in the
c     regions where the field experiences relatively large gradients.
c     By volume averaging a field using FIELD/VOLAVG, ESUG will
c     cause a more gentle form of adaption with a better grading of
c     elements.  By composing the values of the field with LOG or
c     ASINH using FIELD/COMPOSE, one can cause ESUG to shift nodes
c     to where the logarithm (or hyperbolic arcsine) of the field has
c     interesting features, rather than where the field itself has
c     interesting features.
c
c        The code variable MAXITER_SM (Default: 25)  can be set
c     using the ASSIGN command before calling RADAPT.  This controls
c     the maximum number of adaption iterations to be performed by RADAPT.
c     If convergence is detected beforehand, less iterations will be
c     performed.  If field data is allowed to become "stale" during the
c     course of r-adaption, MAXITER_SM should be reduced (e.g. less than 5).
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
      isubname = 'radapt'
C
      ierror=0
      ctrl=0.d0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'RADAPT: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
C  Find out if it's a 2D or 3D mesh.
      call cmo_get_info('ndimensions_topo',cmo,
     *   nsdtopo,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
 
c.....position/velocity option
 
      movechoice=cmsgin(2)(1:icharlnf(cmsgin(2)))
      if (msgtype(2).le.0.or.movechoice.eq.'-def-'
     &   .or.movechoice.eq.'position') then
         write(logmess,'(a)') 'RADAPT: position option'
         call writloga('default',0,logmess,0,ierrw)
      else
         if (movechoice.eq.'set'.or.movechoice.eq.'add'.or.
     &      movechoice.eq.'sub') then
            write(logmess,'(a)') 'RADAPT: velocity option unimplemented'
            call writloga('default',0,logmess,0,ierrw)
         else
            write(logmess,'(a)') 'RADAPT: incorrect format'
            call writloga('default',0,logmess,0,ierrw)
         endif
         goto 9999
      endif
 
C.... set the point index boundaries
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call mmgetblk('mpary',isubname,ipmpary,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
C
      call cmo_get_info('isetwd',cmo,
     *   ipisetwd,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *   ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      climit1=cmsgin(4)
      climit2=cmsgin(5)
      climit3=cmsgin(6)
 
      mpno=0
C
      if (msgtype(4).le.0 .or. cmsgin(4)(1:5).eq.'-def-') then
         ipt1=1
         ipt2=0
         ipt3=0
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *      nnodes,isetwd,itp1)
      elseif (msgtype(4) .eq. 1) then
         ipt1=imsgin(4)
         ipt2=imsgin(5)
         ipt3=imsgin(6)
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *      nnodes,isetwd,itp1)
      else
         ich1=cmsgin(4)
         ich2=cmsgin(5)
         ich3=cmsgin(6)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *      nnodes,isetwd,itp1)
      endif
C
      if (mpno.gt.0) then
         write(logmess,'(a,i10)')
     *      'nodes in point set  = ',mpno
         call writloga('default',0,logmess,0,ierrw)
      else
         write(logmess,'(a)') 'No points to r-adapt!'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
c... If user specifies 'user', adaption is to FADPT.
c... If user specifies 'erradpt', adaption is to ERRADPT.
c... Otherwise, the user must specify the name of
c... a valid field to adapt to.  In this case, we
c... also check if the 'refresh' option is specified.
 
      if (msgtype(7).eq.3 .and. cmsgin(7)(1:5).ne.'-def-') then
         if (cmsgin(7)(1:4).eq.'user') then
            action='user'
         elseif (cmsgin(7)(1:7).eq.'erradpt') then
            action='erradpt'
         else
            cfield=cmsgin(7)(1:icharlnf(cmsgin(7)))
            call mmgetpr(cfield,cmo,ipreffield,icscode)
            if(icscode.ne.0) then
               ierror=icscode
               write(logmess,'(a)')
     *            'RADAPT: bad reference field'
               call writloga('default',0,logmess,0,ierrw)
               goto 9999
            endif
            if (msgtype(8).eq.3.and.
     &         cmsgin(8)(1:7).eq.'refresh') then
               action="field:refresh"
            else
               action="field"
            endif
         endif
      else
         ierror=icscode
         write(logmess,'(a)')
     *      'RADAPT: bad reference field'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
c... Determine which algorithm is to be used.
 
      if (nsdtopo.eq.2) then
         if (action.eq.'erradpt') then
            write(logmess,'(a)')
     *         'RADAPT: can''t use ERRADPT in 2-D'
            call writloga('default',0,logmess,0,ierrw)
            goto 9999
         endif
         if (msgtype(3).ne.3 .or. cmsgin(3)(1:5).eq.'-def-') then
            algchoice = 'esug'
         else
            algchoice = cmsgin(3)(1:icharlnf(cmsgin(3)))
         endif
      else
         if (msgtype(3).ne.3 .or. cmsgin(3)(1:5).eq.'-def-') then
            algchoice = 'mega'
         else
            algchoice = cmsgin(3)(1:icharlnf(cmsgin(3)))
         endif
      endif
 
c Print correct message and call correct routine, based on whether
c r-adaption is (1) 2- or 3-dimensional, (2) to be done by ESUG or MEGA.
 
      if (nsdtopo.eq.2) then
 
         if (algchoice .eq. 'esug') then
 
            write(logmess,'(a)')
     *         'RADAPT: Perform r-adaption on 2D triangular mesh'//
     *         ' using ESUG'
            call writloga('default',0,logmess,0,ierrw)
            call esug2d(cmo,mpary,mpno,ctrl,
     *         action,cfield,climit1,climit2,climit3,ierror)
 
         elseif (algchoice.eq. 'mega') then
 
            write(logmess,'(a)')
     *         'RADAPT: Perform r-adaption on 2D triangular mesh'//
     *         ' using MEGA'
            call writloga('default',0,logmess,0,ierrw)
 
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
 
c           call mega2d(cmo,mpary,mpno,ctrl,
c     *        action,reffield,ierror)
c
         endif
 
      else
 
         if (algchoice.eq. 'esug') then
 
            write(logmess,'(a)')
     *         'RADAPT: Perform r-adaption on 3D mesh using ESUG'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
c           call esug3d(cmo,mpary,mpno,ctrl,
c     *        action,reffield,ierror)
 
         elseif (algchoice.eq. 'mega') then
 
            write(logmess,'(a)')
     *         'RADAPT: Perform r-adaption on 3D mesh using MEGA'
            call writloga('default',0,logmess,0,ierrw)
 
            call mega3d(cmo,mpary,mpno,ctrl,
     *         action,cfield,climit1,climit2,climit3,ierror)
C
         elseif (algchoice.eq. 'mega_exact') then
 
            write(logmess,'(a)')
     *        'RADAPT: Perform r-adaption on 3D mesh using MEGA (exact)'
            call writloga('default',0,logmess,0,ierrw)
 
            lenaction=icharlnf(action)
            action(lenaction+1:lenaction+6)=':exact'
            call mega3d(cmo,mpary,mpno,ctrl,
     *         action,cfield,climit1,climit2,climit3,ierror)
C
         endif
      endif
 9999 continue
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
C
      return
      end
