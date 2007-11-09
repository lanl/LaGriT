*dk,smooth
      subroutine smooth(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        SMOOTH smooths 2D and 3D mesh objects.
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
C        $Log: smooth.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.37   05 Sep 2000 12:44:46   dcg
CPVCS    comment out error message if mmrelprt fails (some options do not 
CPVCS    get any temp arrays)
CPVCS
CPVCS       Rev 1.36   25 Aug 2000 16:28:48   dcg
CPVCS    add check on nwds
CPVCS
CPVCS       Rev 1.35   16 Aug 2000 08:47:52   dcg
CPVCS    fix call to cycle_lg
CPVCS
CPVCS       Rev 1.34   15 Aug 2000 17:09:32   dcg
CPVCS    add smooth/position/network option
CPVCS
CPVCS       Rev 1.33   Mon Nov 29 15:24:06 1999   kuprat
CPVCS    Removed 'aspectlaplace' option; added 'inradius' option.
CPVCS
CPVCS       Rev 1.32   Fri Feb 19 13:25:44 1999   jtg
CPVCS    minor error setting parameters for laplace smooth fixed
CPVCS
CPVCS       Rev 1.31   Tue Dec 01 15:59:22 1998   nnc
CPVCS    Added documentation for LPFILTER option.  Reorganized the
CPVCS    algorithm selection logic.  Added informational output
CPVCS    for unimplemented commands.
CPVCS
CPVCS       Rev 1.30   Thu Nov 05 13:15:00 1998   dcg
CPVCS    add lpfilter option
CPVCS
CPVCS       Rev 1.29   Wed Nov 04 02:44:12 1998   kuprat
CPVCS    We now have ASPECT and ASPECTLAPLACE smoothing.  Default
CPVCS    TOLDAMAGE for both varieties is infinity.
CPVCS
CPVCS       Rev 1.28   Tue Oct 13 11:27:06 1998   dcg
CPVCS    fix comment for LAPLACE smoothing
CPVCS
CPVCS       Rev 1.27   Fri Oct 09 11:32:38 1998   dcg
CPVCS    allow 3d and 2d laplace smoothing
CPVCS
CPVCS       Rev 1.26   Fri Oct 09 11:25:42 1998   dcg
CPVCS    use 'inclusive' and 'exclusive' to set neighborhood
CPVCS    for laplace 3d smoothing
CPVCS
CPVCS       Rev 1.25   Thu Oct 08 16:54:56 1998   dcg
CPVCS    add laplace 3d smoothing
CPVCS
CPVCS       Rev 1.24   Fri Jan 30 15:36:02 1998   kuprat
CPVCS    Corrected from '2' to '1' parameter in call to mmgetblk
CPVCS    for integer array mpary.
CPVCS
CPVCS       Rev 1.23   Wed Oct 29 16:55:56 1997   kuprat
CPVCS    Corrected call parameter lists for calls to mega3d and esug2d.
CPVCS
CPVCS       Rev 1.22   Tue Sep 02 23:16:32 1997   kuprat
CPVCS    Made changes in keeping with new SMOOTH/RADAPT bifurcation.
CPVCS
CPVCS       Rev 1.21   Mon Jul 28 10:38:34 1997   kuprat
CPVCS    Put in random option for 2d grids
CPVCS
CPVCS       Rev 1.20   Tue Jun 03 13:11:02 1997   kuprat
CPVCS    Enabled 3-D MEGA smoothing using CMO field.
CPVCS
CPVCS       Rev 1.19   Mon Apr 14 17:01:50 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.18   Wed Sep 18 11:33:10 1996   kuprat
CPVCS    Made 'geometry' the default type of smoothing in 3-D.
CPVCS
CPVCS       Rev 1.17   Mon Sep 16 21:15:38 1996   kuprat
CPVCS    Cleaned up 'ES   UG' message.
CPVCS
CPVCS       Rev 1.16   Wed Jul 31 19:45:42 1996   kuprat
CPVCS    Added 'geometry' option for 3d.
CPVCS
CPVCS       Rev 1.15   Sun Jul 28 21:03:58 1996   kuprat
CPVCS    Added 'geometry' option.
CPVCS
CPVCS       Rev 1.14   Sun Jul 28 18:28:08 1996   kuprat
CPVCS    Put in call to ELLIPTIC2d.
CPVCS
CPVCS       Rev 1.13   Thu Jul 25 21:31:52 1996   kuprat
CPVCS    Added calls to mega2d and laplace2d.
CPVCS
CPVCS       Rev 1.12   Wed May 29 20:28:50 1996   kuprat
CPVCS    REALLY fixed the bug.
CPVCS
CPVCS       Rev 1.11   Wed May 29 20:03:04 1996   kuprat
CPVCS    Corrected bug.
CPVCS
CPVCS       Rev 1.10   Tue May 28 21:31:06 1996   kuprat
CPVCS    Put in correct handling of defaulted options.
CPVCS
CPVCS       Rev 1.9   Sun Apr 28 22:52:54 1996   kuprat
CPVCS    Fixed bug.
CPVCS
CPVCS       Rev 1.8   Sun Apr 28 20:29:18 1996   kuprat
CPVCS    Added new USER option for smoothing using FADPT.
CPVCS
CPVCS       Rev 1.7   12/14/95 21:29:18   kuprat
CPVCS    Made subroutine conform to new `position|set|add|sub' argument.
CPVCS
CPVCS
CPVCS       Rev 1.6   12/05/95 08:25:56   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.5   11/07/95 17:26:40   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.4   11/07/95 14:54:54   kuprat
CPVCS    Version used for Tempe conference results.
CPVCS
CPVCS       Rev 1.2   10/13/95 01:00:10   kuprat
CPVCS    Added mega3d hook.
CPVCS
CPVCS       Rev 1.1   08/29/95 11:24:00   dcg
CPVCS    fix write(logmess) instructions
CPVCS
CPVCS       Rev 1.0   08/24/95 16:39:00   kuprat
CPVCS    Initial revision.
C
c#######################################################################
c     The SMOOTH Command smooths 2- or 3-D mesh objects.
c
c     FIRST FORMAT:
c
c        SMOOTH / { POSITION | SET | ADD | SUB } /
c           { ESUG | ELLIPTIC | LAPLACE | RANDOM | MEGA | GEOMETRY } /
c           ifirst,ilast,istride / control
c
c     Specifying "position" signifies that the x-y-z values of the
c     nodes in the current mesh object will be altered.
c     (The "set", "add", and "sub" options pertain to a node
c     velocity modification option that is not yet implemented.)
c
c     The next field specifies which type of algorithm is to be
c     used for smoothing.  Available for 2D meshes is
c     ESUG, LAPLACE, ELLIPTIC, and RANDOM.  ESUG is Elliptic
c     Smoothing for Unstructured grids, and is the default.
c     (Ref.: Ahmed Khamayseh and Andrew Kuprat, "Anisotropic
c     Smoothing and Solution Adaption for Unstructured Grids",
c     Int. J. Num. Meth. Eng., Vol. 39, pp. 3163-3174 (1996).)
c     ELLIPTIC is the same as ESUG, but here we turn off 'guards'
c     which prevent the grid from folding.  (Thus ESUG is
c     preferred.)  LAPLACE is the standard scheme of changing a
c     node's position to be the average of that of its neighbours.
c     RANDOM causes each node's position to be set to a
c     randomly weighted average position of its neighbours.
c     However guards keep the elements from inverting.
c
c     Available for 3D meshes is MEGA and GEOMETRY.
c     MEGA is Minimum Error Gradient Adaption.
c     Although MEGA is designed to adapt a mesh to a data field,
c     we use it for smoothing by calling it to adapt the mesh to
c     a function with constant Hessian.  (I.e., a multiple of
c     f(x,y,z)=x**2+y**2+z**2.)  Adaption to this function
c     creates a uniform isotropic mesh.
c     (Ref.:  Randolph E. Bank and R. Kent Smith, "Mesh Smoothing Using A
c     Posteriori Error Estimates", SIAM J. Num. Anal. Vol. 34, Issue 3,
c     pp. 979-997 (1997).)
c
c     GEOMETRY smooths the grid by use of the MEGA algorithm,
c     but here we only retain the leading geometry term.  Thus
c     we don't assume we are adapting to a function with unit
c     Hessian, because the term in which the Hessian appears
c     has been dropped.
c
c     Both MEGA and GEOMETRY options have guards against
c     mesh folding.
c
c     ifirst,ilast,istride define the set of nodes in the cmo which
c     will be affected.
c
c     Finally, you can specify an optional CONTROL value between
c     zero and one.  The default (CONTROL=0.) results in the standard
c     smoothing scheme.  Increasing CONTROL towards 1. causes the
c     scheme to be progressively more controlled (moving the mesh
c     less), until at control=1, there is no mesh movement whatsoever.
c
c        The code variable MAXITER_SM (Default: 25)  can be set
c     using the ASSIGN command before calling RADAPT.  This controls
c     the maximum number of adaption iterations to be performed by RADAPT.
c     If convergence is detected beforehand, less iterations will be
c     performed.
c
c     SECOND FORMAT:
c
c        SMOOTH/POSITION/LPFILTER/
c           ifirst,ilast,istride/filtdeg/k_pb [/MATERIAL]
c
c     This command smooths curves in 2D and 3D, and surfaces in 3D,
c     using a low-pass filtering technique of Taubin [1,2].  The
c     mesh object may be a simple curve or surface, or it may be a
c     curve or surface network, which is characterized by a mesh
c     with some cell faces shared by three or more cells.
c
c     In a surface network, the boundary (edges common to a single
c     triangle) and singularity set (edges common to three or more
c     triangles) form a curve network.  This curve network itself
c     has a singularity set (points common to three or more lines).
c     The smoothing respects this hierarchy of topological features:
c     the singularity points of the curve network are kept fixed,
c     the curve network is smoothed as a 1D object without regard
c     to the attached surfaces, and the surfaces are smoothed as
c     2D objects.  The smoothing also respects the hierarchy of
c     surface constraints.  Provided that these are all planar
c     constraints, the smoothed mesh will continue to satisfy them.
c     This command takes two parameters.  The filtering procedure
c     uses a polynomial Hamming filter where FILTDEG is the degree
c     of the polynomial (default is 30).  K_PB is the pass-band
c     value (default is 0.1), which should lie in the interval (0,2).
c
c     WARNING!  The following mode of LPFILTER is not robust, usually
c     resulting in some inverted cells.  USE WITH EXTREME CAUTION.
c
c     If the final flag /MATERIAL is present then the mesh object is
c     regarded as a multi-material mesh rather than just a complex
c     of cells.  In this mode the material interface network is
c     identified as a topological feature, and smoothing applied to
c     this resulting extended topological hierarchy as above, with
c     surfaces smoothed as 2D objects without regard to the attached
c     material volumes, and the material volumes smoothed as 3D objects.
c     This mode supports multi-material meshes in 2D and 3D, and
c     multi-material surfaces in 3D.
c
c     [1] Gabriel Taubin, "A signal processing approach to fair surface
c         design".  Computer Graphics, p351-358, August 1995
c         (Proceedings SIGGRAPH'95).
c     [2] Taubin, Zhang, and Golub, "Optimal surface smoothing as filter
c         design".  IBM research report, RC-20404(#90237), Computer
c         Sciences, 3/12/96.
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
      integer isetwd(10000000)
      integer itp1(10000000)
      character*32 cdum
C
      pointer (ipmpary,mpary)
      integer mpary(10000000)
C
      character*32 ich1,ich2,ich3,cmo,isubname
      character*32 algchoice,movechoice,action
      integer nsdtopo,length,icmotype,icscode,mpno,ipt1,ipt2,
     &   ipt3,nnodes,ntimes,nwttry,useisn,extnbr,ilaplace,
     &   filtdeg,lpf_mode,nsdgeom
      real*8 ctrl,rlxwt,toldamage,k_pb
      integer icharlnf
C
      isubname = 'smooth'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'SMOOTH: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
C  Find out if it's a 1D, 2D or 3D mesh.
      call cmo_get_info('ndimensions_topo',cmo,
     *   nsdtopo,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ndimensions_geom',cmo,
     *   nsdgeom,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
c
c  check for network smoothing option
c
      if(cmsgin(3).eq.'network'.and.nwds.ge.3) then
         call cycle_lg(nwds,msgtype,cmsgin,imsgin,xmsgin,
     &   ierror)
         go to 9999
      endif
 
c.....position/velocity option
 
      movechoice=cmsgin(2)(1:icharlnf(cmsgin(2)))
      if (msgtype(2).le.0.or.movechoice.eq.'-def-'
     &   .or.movechoice.eq.'position') then
         write(logmess,'(a)') 'SMOOTH: position option'
         call writloga('default',0,logmess,0,ierrw)
      else
         if (movechoice.eq.'set'.or.movechoice.eq.'add'.or.
     &      movechoice.eq.'sub') then
            write(logmess,'(a)') 'SMOOTH: velocity option unimplemented'
            call writloga('default',0,logmess,0,ierrw)
         else
            write(logmess,'(a)') 'SMOOTH: old format will not run:'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)') 'insert "position/" after "smooth/"'
            call writloga('default',0,logmess,0,ierrw)
         endif
         goto 9999
      endif
 
C.... set the point index boundaries
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call mmgetblk('mpary',isubname,ipmpary,nnodes,1,icscode)
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
         write(logmess,'(a)') 'No points to smooth!'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
c  If ctrl is not specified, it
c  defaults to zero (uncontrolled smoothing).
c
      if (msgtype(7).eq.2) then
         ctrl=xmsgin(7)
      else
         ctrl=0.
      endif
 
c.... Select smoothing algorithm.
 
      if (nsdtopo.eq.2) then
         if (msgtype(3).ne.3 .or. cmsgin(3)(1:5).eq.'-def-') then
            algchoice = 'esug'
         else
            algchoice = cmsgin(3)(1:icharlnf(cmsgin(3)))
         endif
      else
         if (msgtype(3).ne.3 .or. cmsgin(3)(1:5).eq.'-def-') then
            algchoice = 'geometry'
         else
            algchoice = cmsgin(3)(1:icharlnf(cmsgin(3)))
         endif
      endif
 
c.... We handle 7th argument differently for 'aspect',
c.... 'inradius' smoothing.
 
      if ((algchoice.eq.'aspect').or.(algchoice.eq.'inradius')) then
         if (msgtype(7).eq.2) then
            toldamage=xmsgin(7)
         else
            toldamage=1.d99
         endif
      endif
 
C
C     Call the appropriate smoothing routine and print a message.
C
 
      if (algchoice .eq. 'esug') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth 2D triangular mesh using ESUG'
          call writloga('default',0,logmess,0,ierrw)
          action='smooth'
          call esug2d(cmo,mpary,mpno,ctrl,
     &       action,cdum,cdum,cdum,cdum,ierror)
 
        else if (nsdtopo .eq. 3) then
          logmess = 'SMOOTH: Smooth 3D tet mesh using ESUG'
          call writloga('default',0,logmess,0,ierrw)
          write(logmess,'(a)') 'Sorry.  Not implemented yet.'
          call writloga('default',0,logmess,0,ierrw)
c          call esug3d(cmo,mpary,mpno,ctrl,
c     &       cmoref,cdum,cdum,cdum,cdum,ierror)
 
        else
          logmess = 'SMOOTH: ESUG is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'laplace') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth 2D mesh using LAPLACE'
          call writloga('default',0,logmess,0,ierrw)
          call laplace2d(cmo,mpary,mpno,ctrl,ierror)
 
        else if (nsdtopo .eq. 3) then
 
          !! get parameters for 3d laplace smoothing
 
          if (nwds.ge.7.and.msgtype(7).eq.2) then
             rlxwt=xmsgin(7)
          else if (nwds.ge.7.and.msgtype(7).eq.1) then
             rlxwt=imsgin(7)
          else
             rlxwt=0.5d0
          endif
          if (nwds.ge.8.and.msgtype(8).eq.1) then
             ntimes=imsgin(8)
          else if (nwds.ge.8.and.msgtype(8).eq.2) then
             ntimes=nint(xmsgin(8))
          else
             ntimes=5
          endif
          if (nwds.ge.9.and.msgtype(9).eq.1) then
             nwttry=imsgin(9)
          else if (nwds.ge.9.and.msgtype(9).eq.2) then
             nwttry=nint(xmsgin(9))
          else
             nwttry=3
          endif
          if (nwds.ge.10.and.msgtype(10).eq.1) then
             useisn=imsgin(10)
          else if (nwds.ge.10.and.msgtype(10).eq.2) then
             useisn=nint(xmsgin(10))
          else
             useisn=1
          endif
          extnbr=1
          if (nwds.ge.11.and.msgtype(11).eq.3) then
             if(cmsgin(11)(1:9).eq.'exclusive') extnbr=0
          endif
 
          action='laplace'
 
          logmess = 'SMOOTH: Smooth 3D mesh using LAPLACE'
          call writloga('default',0,logmess,0,ierrw)
 
          call nbrsmooth(cmo,mpary,mpno,rlxwt,ntimes,nwttry,
     &        useisn,extnbr)
 
        else
          logmess = 'SMOOTH: LAPLACE is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'elliptic') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth mesh using ELLIPTIC2D'
          call writloga('default',0,logmess,0,ierrw)
          call elliptic2d(cmo,mpary,mpno,ctrl,ierror)
 
        else
          logmess = 'SMOOTH: ELLIPTIC is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'geometry') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth mesh using MEGA geometry term'
          call writloga('default',0,logmess,0,ierrw)
          call mega2d(cmo,mpary,mpno,ctrl,1,ierror)
 
        else if (nsdtopo .eq. 3) then
          logmess = 'SMOOTH: Smooth 3D mesh using geometric functional'
          call writloga('default',0,logmess,0,ierrw)
          action='geometry'
          call mega3d(cmo,mpary,mpno,ctrl,
     &       action,cdum,cdum,cdum,cdum,ierror)
 
        else
          logmess = 'SMOOTH: GEOMETRY is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'random') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth mesh using random weights'
          call writloga('default',0,logmess,0,ierrw)
          action='random'
          call esug2d(cmo,mpary,mpno,ctrl,
     &       action,cdum,cdum,cdum,cdum,ierror)
 
        else
          logmess = 'SMOOTH: RANDOM is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'mega') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth 2D mesh using MEGA'
          call writloga('default',0,logmess,0,ierrw)
          call mega2d(cmo,mpary,mpno,ctrl,0,ierror)
 
        else if (nsdtopo .eq. 3) then
          logmess = 'SMOOTH: Smooth 3D mesh using MEGA'
          call writloga('default',0,logmess,0,ierrw)
          action='idhess'
          call mega3d(cmo,mpary,mpno,ctrl,
     &       action,cdum,cdum,cdum,cdum,ierror)
 
        else
          logmess = 'SMOOTH: MEGA is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'aspect') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth 2D mesh improving inscribed radii'
          call writloga('default',0,logmess,0,ierrw)
          ilaplace=0
          call sgd(cmo,toldamage,ilaplace,mpary,mpno,ierror)
 
        else if (nsdtopo .eq. 3) then
          logmess = 'SMOOTH: Smooth 3D mesh improving inscribed radii'
          call writloga('default',0,logmess,0,ierrw)
          ilaplace=0
          call sgd(cmo,toldamage,ilaplace,mpary,mpno,ierror)
 
        else
          logmess = 'SMOOTH: ASPECT is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'inradius') then
 
        if (nsdtopo .eq. 2) then
          logmess = 'SMOOTH: Smooth 2D mesh improving inscribed radii'
          call writloga('default',0,logmess,0,ierrw)
          ilaplace=0
          call sgd(cmo,toldamage,ilaplace,mpary,mpno,ierror)
 
        else if (nsdtopo .eq. 3) then
          logmess = 'SMOOTH: Smooth 3D mesh improving inscribed radii'
          call writloga('default',0,logmess,0,ierrw)
          ilaplace=0
          call sgd(cmo,toldamage,ilaplace,mpary,mpno,ierror)
 
        else
          logmess = 'SMOOTH: INRADIUS is not supported on this mesh'
          call writloga('default',0,logmess,0,ierrw)
        end if
 
      else if (algchoice .eq. 'lpfilter') then
 
        !! Get parameters for lpfilter smoothing
        if ((nwds .ge. 7) .and. (msgtype(7) .eq. 1)) then
          filtdeg = imsgin(7)
        else if ((nwds .ge. 7) .and. (msgtype(7) .eq. 2)) then
          filtdeg = nint(xmsgin(7))
        else
          filtdeg = 30
        endif
        if ((nwds .ge. 8) .and. (msgtype(8) .eq. 2)) then
          k_pb = xmsgin(8)
        else if ((nwds .ge. 8) .and. (msgtype(8) .eq. 1)) then
          k_pb = imsgin(8)
        else
          k_pb = 0.1d0
        endif
        if ((nwds .ge. 9) .and. (msgtype(9) .eq. 3)) then
          if (cmsgin(9)(1:8) .eq. 'material') then
            lpf_mode = 1
          else
            lpf_mode = 0
          endif
        else
          lpf_mode = 0
        endif
 
        if (lpf_mode .eq. 1) then  !! Material smoothing mode.
 
          if (nsdtopo .eq. 2 .and. nsdgeom .eq. 2) then
            logmess='SMOOTH: Smooth 2D multimaterial mesh with LPFILTER'
          else if (nsdtopo .eq. 2 .and. nsdgeom .eq. 3) then
            logmess='SMOOTH: Smooth multimaterial surface with LPFILTER'
          else if (nsdtopo .eq. 3) then
            logmess='SMOOTH: Smooth 3D multimaterial mesh with LPFILTER'
          else
            logmess = 'SMOOTH: LPFILTER does not support material' //
     &                ' smoothing on this mesh'
            call writloga('default',0,logmess,0,ierrw)
            go to 9999
          end if
 
        else  !! Network smoothing mode.
 
          if (nsdtopo .eq. 1 .and. nsdgeom .ge. 2) then
            logmess = 'SMOOTH: Smooth curve network using LPFILTER'
          else if (nsdtopo .eq. 2 .and. nsdgeom .eq. 3) then
            logmess = 'SMOOTH: Smooth surface network using LPFILTER'
          else
            logmess = 'SMOOTH: LPFILTER does not support network' //
     &                ' smoothing on this mesh'
            call writloga('default',0,logmess,0,ierrw)
            go to 9999
          end if
 
        end if
 
        call writloga('default',0,logmess,0,ierrw)
        call lpfilter(cmo,mpary,mpno,filtdeg,k_pb,lpf_mode,ierror)
 
      else
 
        logmess = "SMOOTH: Unknown smoothing algorithm " // algchoice
        call writloga('default',0,logmess,0,ierrw)
 
      end if
 
 9999 continue
      call mmrelprt(isubname,icscode)
c     if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
C
      return
      end
