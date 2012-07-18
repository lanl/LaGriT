      subroutine reconloop2d(iobtuse,toldamage,lcheckaxy,epsilona)
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: reconloop2d.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.17   15 May 2001 14:44:14   kuprat
CPVCS    We now pass the parameters TOLDAMAGE, LCHECKAXY, EPSILONA
CPVCS    to the call to RECON2D.
CPVCS    
CPVCS       Rev 1.16   Thu Oct 09 10:11:36 1997   gable
CPVCS    Change loop max from a hardwired number to a
CPVCS    variable in a data statement so that it is easier
CPVCS    to change in the debugger.
CPVCS    
CPVCS       Rev 1.15   Wed Oct 08 14:37:32 1997   dcg
CPVCS    fix typo on format statement
CPVCS
CPVCS       Rev 1.14   Wed Oct 08 13:55:28 1997   dcg
CPVCS    replace print statements
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 16:58:38 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Thu Mar 06 21:50:26 1997   het
CPVCS    Fix an error with mmrelblk of ineg.
CPVCS
CPVCS       Rev 1.11   Sun Feb 23 10:33:50 1997   het
CPVCS    Don't calculate coefficients if no points are to be added.
CPVCS
CPVCS       Rev 1.10   Wed Oct 30 21:28:44 1996   het
CPVCS    Add an error flag to the refine_edge_2d call to
CPVCS       indicate that no edges where refined. This will
CPVCS       terminate the refinement loop.
CPVCS
CPVCS       Rev 1.9   11/07/95 17:23:56   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.8   09/29/95 09:12:34   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.7   09/14/95 02:07:52   het
CPVCS    Fix ipcmoprm errors
CPVCS
CPVCS       Rev 1.6   08/15/95 18:24:00   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.5   05/30/95 07:51:00   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
CPVCS
CPVCS       Rev 1.4   05/26/95 13:18:30   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.3   04/01/95 11:02:46   dcg
CPVCS    fix call to refine_edge_2d
CPVCS
CPVCS       Rev 1.2   03/07/95 16:53:06   dcg
CPVCS     fixed mesh object length after reconnection
CPVCS
CPVCS       Rev 1.1   03/03/95 11:07:38   dcg
CPVCS     Remove hardwired 2dmesh object
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:38   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'

C arguments (iobtuse,toldamage,lcheckaxy,epsilona)
      integer iobtuse
      real*8 toldamage,epsilona
      logical lcheckaxy
C
      integer iloop_max
      parameter (iloop_max=50)

      pointer (ipineg, ineg)
      integer nneg, ineg(2,*)

      integer ierror,nsdtopo,length,icmotype,i,nelements,icscode
     &       ,ierrmat,lenout


      character*132 logmess
      character*32 cmo, isubname
C
C ######################################################################
C BEGIN begin
C
      isubname='reconloop2d'
C
C  get info from mesh object and make sure it is a 2d mesh
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')'RECON2D: no 3d mesh object'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
      call cmo_get_info('ndimensions_topo',cmo,
     *                  nsdtopo,length,icmotype,ierror)
      if(nsdtopo.ne.2) then
         write(logmess,'(a)')'RECON2D: bad 2d mesh object'
         call writloga('default',0,logmess,0,ierror)
         goto 9999
      endif
C
C
      do i= 1, iloop_max
         write(logmess,'(a,i5)') 'RECON2D reconnection loop2d: ',i
         call writloga('default',0,logmess,0,ierror)
         call recon2d(cmo,toldamage,lcheckaxy,epsilona)
         call cmo_get_info('nelements',cmo,
     *                     nelements,length,icmotype,ierror)
         length=nelmnee(ifelmtri)*nelements

         if(iobtuse.eq.1) then
            call mmgetblk('ineg',isubname,ipineg,length,1,icscode)
            call matbld0tri(nneg,ipineg,ierrmat)

            if(ierrmat.gt.0) then
               call refine_edge_2d(cmo,ierror)

               if(ierror.gt.0) goto 100
               call mmrelblk('ineg',isubname,ipineg,icscode)
               call cmo_get_info('nelements',cmo,
     *                            nelements,length,icmotype,ierror)
               length=nelmnee(ifelmtri)*nelements

               call mmgetblk('ineg',isubname,ipineg,length,2,icscode)
               call matbld0tri(nneg,ipineg,ierrmat)
               call mmrelblk('ineg',isubname,ipineg,icscode)
               if(ierrmat.lt.0) goto 110
            else

               call mmrelblk('ineg',isubname,ipineg,icscode)
               goto 110
            endif
         else
            goto 100
         endif
      enddo
 100  continue

      call mmfindbk('ineg',isubname,ipineg,lenout,icscode)
      if(icscode.eq.0) then
         call mmrelblk('ineg',isubname,ipineg,icscode)
      endif

 110  continue
C
      goto 9999
 9999 continue
C
      return
      end
