*dk,field_volavg
      subroutine field_volavg(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &                    ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        FIELD_VOLAVG calls the appropriate routine to replace the node
C        values of a 2D or 3D field with volume averages of 
C        associated VORONOI or MEDIAN mesh control volumes.  (This
C        has the effect of smoothing out the field values.)
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
C        $Log: field_volavg.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:47:48 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   12/05/95 08:25:52   het
CPVCS    Make changes for UNICOS
CPVCS    
CPVCS       Rev 1.1   11/07/95 17:17:26   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.0   08/24/95 16:38:40   kuprat
CPVCS    Initial revision.
C
c*  ----------------------------------------------------------------  $&
c*                                                                    $&
c*    FIELD/VOLAVG/averaging option/iterations/ifirst,ilast,istride/  $&
c*      field list                                                    $&
c*                                                                    $&
c*        For all the members of the point set, and for all specified $&
c*      fields, we replace the point field values with values that    $&
c*      represent the average of the field(s) over control volumes    $&
c*      associated with the points.  The AVERAGING OPTION specifies   $&
c*      what kind of control volume is to be used; the choices are    $&
c*      VORONOI and MEDIAN.  ITERATIONS is an integer that specifies  $&
c*      a repeat count for how many times this procedure is to be     $&
c*      performed on the field(s).  The affect of this process is     $&
c*      to broaden and smooth the features of the field(s), similar   $&
c*      to the effect of a diffusion process.                         $&
c*                                                                    $&
c*        The VORONOI choice, unlike the MEDIAN choice, produces a    $&
c*      diffusive effect independent of mesh connectivity.  However,  $&
c*      again unlike the MEDIAN choice, it REQUIRES that the mesh     $&
c*      be Delaunay, or incorrect results will occur.                 $&
c*                                                                    $&
c*                                                                     "
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
      pointer (ipfield,field)
      integer isetwd(10000000)
      integer itp1(10000000)
      real*8 field(10000000)
C
      pointer (ipmpary,mpary)
      integer mpary(10000000)
C
      character*32 ich1,ich2,ich3,cmo,isubname,blkname
      character*32 option
      integer nsdtopo,length,icmotype,icscode,mpno,ipt1,ipt2,
     &   ipt3,nnodes,niters
      integer icharln
C
      isubname = 'field_volavg'
C
      ierror=0
C
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *         'FIELD_VOLAVG: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif

C  Find out if it's a 2D or 3D mesh.
      call cmo_get_info('ndimensions_topo',cmo,
     *                        nsdtopo,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
C    set the point index boundaries
C
      call cmo_get_info('nnodes',cmo,
     *                        nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call mmgetblk('mpary',isubname,ipmpary,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
C
      call cmo_get_info('isetwd',cmo,
     *                        ipisetwd,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *                        ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      mpno=0
C
      if (msgtype(5) .eq. 1) then
         ipt1=imsgin(5)
         ipt2=imsgin(6)
         ipt3=imsgin(7)
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      else
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif
C
      if (mpno.gt.0) then
         write(logmess,'(a,i10)')
     *         'nodes in point set  = ',mpno
         call writloga('default',0,logmess,0,ierrw)
      else
         write(logmess,'(a)') 'No points to volume average!'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif

      blkname=cmsgin(8)(1:icharln(cmsgin(8)))
      call mmgetpr(blkname,cmo,ipfield,icscode)
      if(icscode.ne.0) then
         ierror=icscode
         write(logmess,'(a)')
     *      'FIELD_VOLAVG: bad field for averaging'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif

      option = cmsgin(3)(1:icharln(cmsgin(3)))
      niters =  imsgin(4)

      if (nsdtopo.eq.2) then     

         if (option .eq. 'median') then

            write(logmess,'(a)')
     *         'FIELD_VOLAVG: Smooth 2D field using median mesh'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
c            call medavg2d(cmo,mpary,mpno,niters,
c     *         field,ierror)

         elseif (option.eq. 'voronoi') then
            write(logmess,'(a)')
     *         'FIELD_VOLAVG: Smooth 2D field using voronoi mesh'
            call writloga('default',0,logmess,0,ierrw)
            call voravg2d(cmo,mpary,mpno,niters,
     *         field,ierror)

         endif
      else
         if (option .eq. 'median') then

            write(logmess,'(a)')
     *         'FIELD_VOLAVG: Smooth 3D field using median mesh'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
c            call medavg3d(cmo,mpary,mpno,niters,
c     *         field,ierror)

         elseif (option.eq. 'voronoi') then
            write(logmess,'(a)')
     *         'FIELD_VOLAVG: Smooth 3D field using voronoi mesh'
            call writloga('default',0,logmess,0,ierrw)
            write(logmess,'(a)')
     *         'Sorry.  Not implemented yet.'
            call writloga('default',0,logmess,0,ierrw)
c            call voravg3d(cmo,mpary,mpno,niters,
c     *         field,ierror)

         endif
     
      endif
 9999 continue
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')

      return
      end
