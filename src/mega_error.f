*dk,mega_error
      subroutine mega_error()
C
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine builds the 2D or 3D Hessian matrix and places
C           it in the CMO.
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/mega_error.f_a  $
CPVCS    
CPVCS       Rev 1.5   22 Jan 2001 14:49:48   dcg
CPVCS    clean-up make implicit none
CPVCS
CPVCS       Rev 1.4   22 Jan 2001 14:32:42   dcg
CPVCS    remove all references to unused memory managed array 'tmp'
CPVCS
CPVCS       Rev 1.3   Mon Feb 01 12:18:08 1999   nnc
CPVCS    Drop GMV output of megadet and megaerr.
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:53:22 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   Fri Nov 08 12:25:38 1996   dcg
CPVCS    kent smith changes
CPVCS
CPVCS       Rev 1.0   Mon Jul 29 15:17:48 1996   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      character*132 logmess
C
      integer ierror
C
      character*40 isubname, cmo
      character*8092 cbuff
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
C
      pointer (ipitet, itet1)
      integer itet1(*)
C
      pointer (iplelements, lelements)
      pointer (ipxmegah, xmegah)
      pointer (ipxmegadet, xmegadet)
      pointer (ipxmegaerr, xmegaerr)
      integer lelements(*)
      integer ierrwrt,nen,ilen,itype,icscode,nef,nsd,mbndry,nnodes,
     *  nelements,length,icmotype,lenout,ier,ityp,ierr,it,kdim,kpe
      real*8 xmegah(*), xmegadet(*),
     *                 xmegaerr(*)
      external fcn
C
      isubname='mega_error'
C
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,9000)
 9000    format('No CMOs defined')
         call writloga('default',1,logmess,1,ierrwrt)
         goto 9999
      endif
C
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('ndimensions_topo',cmo,nsd,ilen,itype,icscode)
C
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,icmotype,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
 
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
C
      call mmfindbk('megah',cmo,ipxmegah,lenout,icscode)
      if(icscode.ne.0) then
         call mega_hessian()
      endif
      call mmfindbk('megadet',cmo,ipxmegadet,lenout,icscode)
      if(icscode.ne.0) then
         cbuff = 'cmo/addatt/-def-/megadet/VDOUBLE/scalar/' //
     *           'nelements/linear/permanent/x/0.0 ;  ' //
     *           'finish'
         call dotaskx3d(cbuff,ier)
         call cmo_get_info('megadet',cmo,ipxmegadet,ilen,ityp,ierr)
      endif
      call mmfindbk('megaerr',cmo,ipxmegaerr,lenout,icscode)
      if(icscode.ne.0) then
         cbuff = 'cmo/addatt/-def-/megaerr/VDOUBLE/scalar/' //
     *           'nelements/linear/permanent/x/0.0 ;  ' //
     *           'finish'
         call dotaskx3d(cbuff,ier)
         call cmo_get_info('megaerr',cmo,ipxmegaerr,ilen,ityp,ierr)
      endif
C
      length=nelements
      call mmgetblk('lelements',isubname,iplelements,length,2,icscode)
CX3D
      do it=1,nelements
         lelements(it)=it
      enddo
CX3D
      kdim=nsd
      kpe=kdim+1
C
      if(nen.eq.nelmnen(ifelmtri).and.nef.eq.nelmnef(ifelmtri)) then
         kdim=3
         call errb2d (nelements,lelements,nnodes,kdim,xic,yic,zic,
     *                xmegah,nelements,kpe,itet1,
     *                xmegadet,xmegaerr)
      elseif(nen.eq.nelmnen(ifelmtet).and.nef.eq.nelmnef(ifelmtet).and.
     *       nsd.eq.3) then
         call errb3d (nelements,lelements,nnodes,kdim,xic,yic,zic,
     *                xmegah,nelements,kpe,itet1,
     *                xmegadet,xmegaerr)
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
